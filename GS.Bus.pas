///-------------------------------------------------------------------------------
/// Title      : GS.Bus
/// Short Desc : "central" threaded mini bus.
/// Source     : https://github.com/VincentGsell
/// Aim        : - This Bus implementation provide classic Subscription
///                model to a "channel" (Queue or Topic).
///              - It permit Inter thread communication safely.
///              - It work in its own thread for distribution.
///              - One of the basic usage should to separate GUI and
///                Model/Controler, and let a chance to swap easely from
///                a standalone app to a middleware (network) one.
///             : FPC Dependancy : github.com/dathox/generics.collections
///-------------------------------------------------------------------------------
unit GS.Bus;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  Generics.Collections,
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Threads;

Const
  CST_BUSTIMER = 250; //MilliSec.

Type
TBus = Class;

//Write in it whatever you want.
TBusMessage = Packed Record
  Buffer : TBytes;

  Procedure FromString(aText : String);
  Function AsString : String;
  Procedure FromStream(aStream : TMemoryStream);
  Function AsStream : TMemoryStream;
  Procedure FromByte(aByte : Byte);
  Function AsByte : Byte;
End;
pTBusMessage = ^TBusMessage;

//This is the envelop for a message to the bus
TBusEnvelop = packed Record
  EnvelopId : UINT64;
  TargetChannel : String;
  ResponseChannel : String;
  ContentMessage : TBusMessage;
  AdditionalData : String;
  Persistent : Boolean;
End;
PTBusEnvelop = ^TBusEnvelop;

TBusEnvelopList = Class
Private
  FList : TList<PTBusEnvelop>;
  FLock : TCriticalSection;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function Lock : Tlist<PTBusEnvelop>;
  Procedure Unlock;
end;

TBusMessageNotify = Procedure(Sender : TBus;Var Packet : TBusEnvelop) Of Object;
TBusChannel = Class;
TBusClient = Class;
TBusClientReader = Class;
TBusChannelList = Class;
TBusClientReaderList = Class;
TBusClientReaderListShortcut = Class;


TBusChannelAdditionalInformation = class
private
  FMasterChannel: TBusChannel;
Public
  Constructor Create(aMasterChannel : TBusChannel); reintroduce; Virtual;
  Property MasterChannel : TBusChannel read FMasterChannel;
end;

TBusChannelPrivacy = Class(TBusChannelAdditionalInformation)
Public
{ TODO 1 -oVGS -cNiceToHave : Add Restricted : Manual mode where user give what session can read or write. }
End;

TBusChannelBehaviourInformation = Class(TBusChannelAdditionalInformation)
Public
End;

TBusChannelBehaviourTopic = Class(TBusChannelAdditionalInformation)
Public
End;

TBusChannelBehaviourQueueSpecific = (cbqQueueFaultTolerant, cbqQueueDistributed);
///cbqQueueFaultTolerant :
///   - Serve first client, as long as it is connected, if this client disconnect, server the second one and so on.
///cbqQueueDistributed :
///   - Serve each client one after the other.
TBusChannelBehaviourQueue = Class(TBusChannelAdditionalInformation)
private
  FQueueBehaviour: TBusChannelBehaviourQueueSpecific;
  FLastClientIndexServed: UInt32;
Public
  Constructor Create(aMasterChannel : TBusChannel); Override;
  Property QueueBehaviour : TBusChannelBehaviourQueueSpecific read FQueueBehaviour;
  Property LastClientIndexServed : UInt32 read FLastClientIndexServed write FLastClientIndexServed;
End;

TBusChannelBehaviour = (bcbTopic, bcbQueue);

//WARNING : All TBUSChannel Event (included event) is processed within a thread.
TOnBusChannelBeforeDeliverMessage = Procedure(Var aMessage : TBusEnvelop) of Object;
TBusChannel = Class
private
  function GetConsumedMessageCount: Int64;
  function GetReceivedMessageCount: Int64;
  procedure SetChannelBehaviour(const Value: TBusChannelBehaviour);
  function GetPersistantMessageCount: Int64;
  function GetMessageInThisChannelWillBeSetAsPersistant: Boolean;
  procedure SettMessageInThisChannelWillBeSetAsPersistant(
      const Value: Boolean);
  function GetChannel: string;
Protected
  FOnBeforeDeliverMessage: TOnBusChannelBeforeDeliverMessage;
  FChannel: TProtectedString;
  FReceivedMessageCount: TProtectedInt64;
  FConsumedMessageCount : TProtectedInt64;
  FSubscibters : TBusClientReaderListShortcut;
  FBehaviour: TBusChannelBehaviour;
  FBehaviourInfo: TBusChannelAdditionalInformation;

  FPersistantMessage : TBusEnvelopList;
  FMessageInThisChannelWillBeSetAsPersistant: TProtectedBoolean;

  function GetCurrentSubscribterCount: Int64;
Public
  Constructor Create(aChannelName : string); Reintroduce;
  Destructor Destroy; Override;

  Function PersistantMessageLock : Tlist<PTBusEnvelop>;
  Procedure PersistantMessageUnlock;

  Procedure IncReceivedMessageCount;
  Procedure IncConsumedMessageCount;

  Property ChannelName : string read GetChannel;
  Property ReceivedMessageCount : Int64 read GetReceivedMessageCount;
  Property ConsumedMessageCount : Int64 read GetConsumedMessageCount;
  Property CurrentSubscribterCount : Int64 read GetCurrentSubscribterCount;
  Property PersistantMessageCount : Int64 read GetPersistantMessageCount;

  Property Subscribters : TBusClientReaderListShortcut read FSubscibters;

  Property ChannelBehaviour : TBusChannelBehaviour read FBehaviour write SetChannelBehaviour;
  Property ChannelBehaviourInfo : TBusChannelAdditionalInformation read FBehaviourInfo write FBehaviourInfo;

  Property MessageInThisChannelWillBeSetAsPersistant : Boolean read GetMessageInThisChannelWillBeSetAsPersistant
                                                               write SettMessageInThisChannelWillBeSetAsPersistant;

  Property OnBeforeDeliverMessage : TOnBusChannelBeforeDeliverMessage read FOnBeforeDeliverMessage Write FOnBeforeDeliverMessage;
End;

//CLIENT SUBSCRIBTION OBJECT
TBusClient = Class abstract
private
Protected
  function GetClientPendingMessageCount: Int64;
  function GetClientProcessMessageCount: Int64; Virtual; Abstract;
Public
  ClientMessageStack : TBusEnvelopList;
  Event : TEvent; //Pointer ! Use it for thread use only, manage by bus.

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Property PendingMessageCount : Int64 read GetClientPendingMessageCount;
  Property ProcessMessageCount : Int64 read GetClientProcessMessageCount;
End;

TBusClientReader = Class(TBusClient)
private
  FProcessMessageCount : TProtectedInt64;
  FCallBack: TBusMessageNotify;
  FChannel: String;
Protected
  function GetClientProcessMessageCount: Int64; Override;
Public
  Procedure IncProcessMessageCount;

  //Not aimed to be call directly : Call Bus.Subscribt.
  Constructor Create(aChannelName : String; aCallBack : TBusMessageNotify); Reintroduce;

  Destructor Destroy; Override;

  Property ChannelListening : String read FChannel;
  Property CallBack : TBusMessageNotify read FCallBack;
End;
PTBusClientReader = ^TBusClientReader;


TBusChannelList = Class
Private
  FList : TObjectlist<TBusChannel>;
  FLock : TCriticalSection;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  //Flush all message in waiting, delete the channel.
  Procedure DeleteChannel(aChannelName : string);
  //Create or Set channel to achieve advanced behaviour (Memory Persitance, Queue)
  Procedure CreateOrSetChannel( aChannelName : String;
                                aChannelBehaviourType : TBusChannelBehaviour;
                                Const aMessageWillBePersistant : Boolean = False);
  //Set an event, which will trig when a message is delivered on this channel.
  //Warning : It it the thread of the bus whitch will process the event ! Keep it thread safe and beware
  //          to not take too many time in this event : Other message will not be dispached during this time.
  Procedure SetChannelOnBeforeDeliverEvent(aChannelName : String; aChannelProc : TOnBusChannelBeforeDeliverMessage);


  Function Lock : TObjectlist<TBusChannel>;
  Procedure Unlock;
end;

TBusClientReaderList = Class
Private
  FList : Tlist<TBusClientReader>;
  FLock : TCriticalSection;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function Lock : TList<TBusClientReader>;
  Procedure Unlock;
end;

//Same specialized list than above, but temporary (not owned object).
TBusClientReaderListShortcut = Class
Private
  FList : Tlist<TBusClientReader>; //Pointer.
  FLock : TCriticalSection;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function Lock : TList<TBusClientReader>;
  Procedure Unlock;
end;


TBus = Class(TThread)
Private
  FTotalMessageSend : TProtectedInt64;
  FTotalMessagePending : TProtectedInt64;
  FTotalMessageProcessed : TProtectedInt64;
  FTotalMessagePersistant : TProtectedInt64;

  FInternalMessageIdGenerator : TProtectedInt64;     //Message generator ID. Not used today (Just a incremental), certainly better system when needed.
  FWaitMessageList : TBusEnvelopList;                //Message pending. (Where clients post)
  FMessageList : TBusEnvelopList;                    //Message list (TBus use this list to distribute effectively).
  FChannels : TBusChannelList;                       //List of all channels.
  FDoWork : TEvent;                                  //Event signaled when "Send" is called : It start message processing (Execute).
  FSubscribters : TBusClientReaderList;              //Raw Subscripter list. Reference. : WARNING : In channel object, there is shortcut to content object. Keep it synchro.

  FEventShadowList : TObjectList<TEvent>;   //List of Event created for Client, and just here for clean on destroy.

  //Work variables.
  FpChannel : TBusChannel;
  FMes : PTBusEnvelop;

  Procedure DoSubscribterWork;
  Procedure DoNotifyClients;
  function GetStats: String;            //Distibute a message to a subscribter.
Protected
  Procedure BusExecute; Virtual;
Public
  constructor Create; overload; Virtual;
  Destructor Destroy; Override;

  Procedure Execute; Override;

  //TBusClientReader : WARNING : in current design, the object returned owned by the bus.
  Function Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
  //TBusClientReader : WARNING : "aclient" instance will be freed if function success.
  Function UnSubscribe(aClient : TBusClientReader; aChannelName : String): Boolean;


  Function Send( var aMessage : TBusMessage;
                 aTargetChannel : String;
                 const aSomeAdditionalData : String = '';
                 const aResponseChannel : string = '';
                 const IsPersistant : Boolean = False) : Int64;

  Procedure GetChannelsConfigurationAsCSV(var aStr : TStringList);
  Procedure GetSubscribtersConfigurationAsCSV(var aStr : TStringList);

  Procedure ProcessMessages(aClientReaders : Array of TBusClientReader);

  Function GetNewEvent : TEvent;

  Procedure ChannelDelete(aChannelName : string);
  Procedure ChannelSet( aChannelName : String;
                        aChannelBehaviourType : TBusChannelBehaviour;
                        Const aMessageWillBePersistant : Boolean = False);
  Procedure ChannelSetOnBeforeDeliverMessageEvent( aChannelName: String;
                                                   aChannelProc: TOnBusChannelBeforeDeliverMessage);


  Property Stats : String read GetStats;
End;

var Bus : TBus;

Procedure StartStandartBus;
Procedure ReleaseStandartBus;

implementation

{ TBus }
Procedure StartStandartBus;
begin
  if Not(assigned(Bus)) then
  begin
    Bus := TBus.Create;
    Bus.Start;
  end;
end;
Procedure ReleaseStandartBus;
begin
  if Assigned(bus) then
  begin
    Bus.Terminate;
    Bus.WaitFor;
    FreeAndNil(Bus);
  end;
end;

procedure TBus.ChannelSet(aChannelName: String;
  aChannelBehaviourType: TBusChannelBehaviour;
  const aMessageWillBePersistant: Boolean);
begin
  FChannels.CreateOrSetChannel( aChannelName,
                                aChannelBehaviourType,
                                aMessageWillBePersistant);
end;

procedure TBus.ChannelSetOnBeforeDeliverMessageEvent(aChannelName: String;
  aChannelProc: TOnBusChannelBeforeDeliverMessage);
begin
  FChannels.SetChannelOnBeforeDeliverEvent(aChannelName,aChannelProc);
end;

constructor TBus.Create;
begin
  inherited Create(true);
  {$IFDEF DELPHI}
  NameThreadForDebugging(ClassName);
  {$ENDIF}
  FWaitMessageList := TBusEnvelopList.Create;
  FMessageList := TBusEnvelopList.Create;
  FChannels := TBusChannelList.Create;
  FSubscribters := TBusClientReaderList.Create;
  FDoWork := TEvent.Create(nil,False,False,EmptyStr);
  FInternalMessageIdGenerator := TProtectedInt64.Create(0);
  FTotalMessageSend := TProtectedInt64.Create(0);
  FTotalMessagePending := TProtectedInt64.Create(0);
  FTotalMessageProcessed := TProtectedInt64.Create(0);
  FTotalMessagePersistant := TProtectedInt64.Create(0);
  FEventShadowList := TObjectList<TEvent>.Create;
end;

procedure TBus.ChannelDelete(aChannelName: string);
begin
  FChannels.DeleteChannel(aChannelName);
end;

destructor TBus.Destroy;
begin
  if not(Terminated) then
  begin
    Terminate;
    FDoWork.ResetEvent;
    FDoWork.SetEvent;
    if not(Terminated) then
      Waitfor; //Terminate main bus loop.
  end;
  FreeAndNil(FDoWork);
  FreeAndNil(FMessageList);
  FreeAndNil(FWaitMessageList);
  FreeAndNil(FSubscribters);
  FreeAndNil(FChannels);
  FreeAndNil(FEventShadowList);
  FreeAndNil(FInternalMessageIdGenerator);
  FreeAndNil(FTotalMessageSend);
  FreeAndNil(FTotalMessagePending);
  FreeAndNil(FTotalMessageProcessed);
  FreeAndNil(FTotalMessagePersistant);
  inherited;
end;

procedure TBus.DoNotifyClients;
var ClientList : TList<TBusClientReader>;
    L : TList<PTBusEnvelop>;
    i : integer;
begin
  ClientList := FpChannel.Subscribters.Lock;
  try
    for I := 0 to ClientList.Count-1 do
    begin
      L := ClientList[i].ClientMessageStack.Lock;
      try
        if L.Count>0 then
        begin
           if assigned(ClientList[i].Event) then
          begin
            ClientList[i].Event.SetEvent; //If this reader is waiting somewhere in a thread, it will be trig.
          end;
        end
        else
        begin
          //TODO : We can here evaluate if there are to many message in waiting for a client...
          // Or start a procedure of notification ?
        end;
      finally
        ClientList[i].ClientMessageStack.Unlock;
      end;
    end;
  finally
    FpChannel.Subscribters.Unlock;
  end;
end;

procedure TBus.DoSubscribterWork;
var aPacket : PTBusEnvelop;
    ClientList : TList<TBusClientReader>;
    L : TList<PTBusEnvelop>;
    i : Integer;
    ClientIndex : Integer;


    Procedure PrepareAndSavePersistantMessage;
    var ll : TList<PTBusEnvelop>;
    begin
      ll := FpChannel.PersistantMessageLock;
      try
        New(aPacket);
        aPacket^.EnvelopId := FMes^.EnvelopId;
        aPacket^.AdditionalData := FMes^.AdditionalData;
        aPacket^.TargetChannel := FpChannel.ChannelName;
        aPacket^.ResponseChannel := FMes^.ResponseChannel;
        aPacket^.ContentMessage := FMes^.ContentMessage; //Deep copy;
        aPacket^.Persistent := True;
        ll.Add(aPacket);
        FTotalMessagePersistant.Inc;
      finally
        FpChannel.PersistantMessageUnlock;
      end;
    end;

    Procedure PrepareAndDeliverMessage;
    begin
      New(aPacket);
      aPacket^.EnvelopId := FMes^.EnvelopId;
      aPacket^.AdditionalData := FMes^.AdditionalData;
      aPacket^.TargetChannel := FpChannel.ChannelName;
      aPacket^.ResponseChannel := FMes^.ResponseChannel;
      aPacket^.Persistent := FMes^.Persistent;
      aPacket^.ContentMessage := FMes^.ContentMessage; //Deep copy;

      if FpChannel.MessageInThisChannelWillBeSetAsPersistant then
      begin
        //Even if the message is not persitant, the channel is set as auto persistant.
        //We turn the message as persitant one. It will be copied in private list just after.
        aPacket^.Persistent := True;
      end;

      L.Add(aPacket);
      FpChannel.IncConsumedMessageCount;
      FTotalMessageProcessed.Inc;

      if FMes.Persistent then
      begin
        PrepareAndSavePersistantMessage;
      end;
    end;
begin
  ClientList := FpChannel.Subscribters.Lock;
  try
    if ClientList.Count=0 then
    begin
      //No client : Only process persistant message if needed.
      if (FpChannel.MessageInThisChannelWillBeSetAsPersistant) Or
         (FMes.Persistent) then
      begin
        PrepareAndSavePersistantMessage;
      end;

      Exit; //Stop !
    end;

    case FpChannel.ChannelBehaviour of
      bcbTopic:
      begin
        for I := 0 to ClientList.Count-1 do
        begin
          L := ClientList[i].ClientMessageStack.Lock;
          try
            PrepareAndDeliverMessage;
          finally
            ClientList[i].ClientMessageStack.Unlock;
          end;
        end;


      end;
      bcbQueue:
      begin
        case TBusChannelBehaviourQueue(FpChannel.ChannelBehaviourInfo).QueueBehaviour of
          ///cbqQueueFaultTolerant :
          ///   - Serve first client, as long as it is connected, if this client disconnect, server the second one and so on.
          cbqQueueFaultTolerant :
          begin
            L := ClientList[0].ClientMessageStack.Lock;
            try
              PrepareAndDeliverMessage;
            finally
              ClientList[0].ClientMessageStack.Unlock;
            end;
          end;
          ///cbqQueueDistributed :
          ///   - Serve each client one after the other.
          cbqQueueDistributed :
          begin
            ClientIndex := TBusChannelBehaviourQueue(FpChannel.ChannelBehaviourInfo).LastClientIndexServed;
            Inc(ClientIndex);
            if ClientIndex > ClientList.Count-1 then
              ClientIndex := 0;
            TBusChannelBehaviourQueue(FpChannel.ChannelBehaviourInfo).LastClientIndexServed := ClientIndex;

            L := ClientList[ClientIndex].ClientMessageStack.Lock;
            try
              PrepareAndDeliverMessage;
            finally
              ClientList[ClientIndex].ClientMessageStack.Unlock;
            end;
          end;
        end;
      end;
    end;


  finally
    FpChannel.Subscribters.Unlock;
  end;
end;

procedure TBus.Execute;
begin
  while not(Terminated) do
  begin
    BusExecute;
  end;
end;

procedure TBus.BusExecute;
var
    aMes : PTBusEnvelop;
    lMasterMessageList,lTempML : TList<PTBusEnvelop>;
    CL : TObjectList<TBusChannel>;

    Procedure LocalInternalTransfertMessage;
    var i : Integer;
    begin
      //STEP ONE : ALL message in the pending list are transfered for processing (= liberate pending list for new reception)
      lMasterMessageList := FWaitMessageList.Lock;
      lTempML := FMessageList.Lock;
      try
        //transfert message pending.
        for I := 0 to lMasterMessageList.Count-1 do
        begin
          lTempML.Add(lMasterMessageList[i]);
          FTotalMessagePending.Inc;
        end;
        lMasterMessageList.Clear; //do not release message, there are transferered in MessageList.
      finally
        FWaitMessageList.Unlock;
        FMessageList.Unlock;
      end;
    end;

    Procedure LocalInternalDispatchToSubscribterAndNotify;
    var i,j : Integer;
    begin
      //STEP TWO : All message in processing list are now dispached to the subscibter.
      CL := FChannels.Lock;
      try

        lMasterMessageList := FMessageList.Lock;
        try
          //Optimizing. MasterMessageList only change in this thread. :--<
        finally
          FMessageList.Unlock;
        end;

        For i := 0 to lMasterMessageList.Count-1 do
        begin

          FpChannel := Nil;
          for j := 0 to CL.Count-1 do
          begin
            if lMasterMessageList[i].TargetChannel = CL[j].ChannelName then
            begin
              FpChannel := CL[j];
              break;
            end;
          end;

          if Not(Assigned(FpChannel)) then //Channel not exists : Create it. Todo : Parameter for Send without channel open ?
          begin
            FpChannel := TBusChannel.Create(lMasterMessageList[i].TargetChannel);
            CL.Add(FpChannel);
          end;

          If (lMasterMessageList[i].TargetChannel = FpChannel.ChannelName) then
          begin
            FpChannel.IncReceivedMessageCount;
            FMes := lMasterMessageList[i];

            if Assigned(FpChannel.OnBeforeDeliverMessage) then
            begin
              try
                fpChannel.OnBeforeDeliverMessage(FMes^);
              Except
                //ToDo : Send Exception Message ?
                //raise Exception.Create('Error Message'); TODO Exception on processing...
              end;
            end;

            DoSubscribterWork;
            DoNotifyClients;
          end;
        end;

        //finally
        //  FMessageList.Unlock;
        //end;
      finally
        FChannels.Unlock;
      End;
    end;

    Procedure LocalInternalCleaning;
    var i : integer;
    begin
      //Cleaning.
      lMasterMessageList := FMessageList.Lock;
      try
        For i := 0 to lMasterMessageList.Count-1 do
        begin
          aMes := lMasterMessageList[i];
          Dispose(aMes);
          FTotalMessagePending.Dec;
        end;
        lMasterMessageList.Clear;
      finally
        FMessageList.Unlock;
      end;
    end;

begin
  case FDoWork.WaitFor(CST_BUSTIMER) of
    wrSignaled :
    begin
      if Terminated then Exit;
      LocalInternalTransfertMessage;
      LocalInternalDispatchToSubscribterAndNotify;
      LocalInternalCleaning;
    end;
    wrTimeout :
    begin
      if Terminated then Exit;
      //Todo : Generate Idle message;
    end;
    wrAbandoned, wrError {$IFNDEF FPC}, wrIOCompletion {$ENDIF} :
    begin
      if Terminated then Exit;
      //Todo : Exception message. Stop ?
    end;
  end;
end;


procedure TBus.GetChannelsConfigurationAsCSV(var aStr: TStringList);
var i : Integer;
    s2 : TStringList;
    c : TBusChannel;
    CL : TObjectList<TBusChannel>;

    Function GetChannelType : String;
    begin
      case c.ChannelBehaviour of
        bcbTopic: result := 'Topic';
        bcbQueue: result := 'Queue';
      end;
    end;

    Function GetIsChannelPersistant : String;
    begin
      result := 'No';
      if c.MessageInThisChannelWillBeSetAsPersistant then
        result := 'Yes';
    end;
begin
  if Terminated then
    Exit;
  Assert(Assigned(aStr));
  aStr.Clear;
  s2 := TStringList.Create;
  CL := FChannels.Lock;
  try
    s2.Add('ChannelName');
    s2.Add('ChannelType');
    s2.Add('IsChannelPersistant');
    s2.Add('ReceivedMessageCount');
    s2.Add('ConsumedMessageCount');
    s2.Add('PersistantMessageCount');
    s2.Add('SubscribterCount');
    aStr.Add(s2.DelimitedText);
    for I := 0 to CL.Count-1 do
    begin
      c := CL[i];
      s2.Clear;
      s2.Add(c.ChannelName);
      s2.Add(GetChannelType);
      s2.Add(GetIsChannelPersistant);
      s2.Add(IntToStr(c.ReceivedMessageCount));
      s2.Add(IntToStr(c.ConsumedMessageCount));
      s2.Add(IntToStr(c.PersistantMessageCount));
      s2.Add(IntToStr(c.CurrentSubscribterCount));
      aStr.Add(s2.DelimitedText);
    end;
  finally
    FreeAndNil(s2);
    FChannels.Unlock;
  end;
end;

function TBus.GetNewEvent: TEvent;
begin
  Result := TEvent.Create(nil,False,False,EmptyStr);
  FEventShadowList.Add(Result);
end;

function TBus.GetStats: String;
begin
  //TODO : Jsonify ?
  Result := 'Messages - Send : ' +IntTostr(FTotalMessageSend.Value) +
            ' Pending : ' +IntTostr(FTotalMessagePending.Value) +
            ' Processed : ' +IntTostr(FTotalMessageProcessed.Value);
end;

procedure TBus.GetSubscribtersConfigurationAsCSV(var aStr: TStringList);
var i : Integer;
    s2 : TStringList;
    sub : TList<TBusClientReader>;

    Function AssignedOrNot(b : boolean) : String;
    begin
      Result := 'Assigned';
      if Not(b) then
        Result := 'Unassigned';
    end;

begin
  if Terminated then
    Exit;
  Assert(Assigned(aStr));
  aStr.Clear;
  s2 := TStringList.Create;
  s2.Add('SubscribterID');
  s2.Add('SubscribtedToChannel');
  s2.Add('PendingMessageCount');
  s2.Add('ProcessMessageCount');
  aStr.Add(s2.DelimitedText);
  sub := FSubscribters.Lock;
  try
    for I := 0 to sub.Count-1 do
    begin
      s2.Clear;
      s2.Add(IntToStr(i));
      s2.Add(sub[i].ChannelListening);
      s2.Add(IntToStr(sub[i].PendingMessageCount));
      s2.Add(IntToStr(sub[i].ProcessMessageCount));
      aStr.Add(s2.DelimitedText);
    end;
  finally
    FreeAndNil(s2);
    FSubscribters.Unlock;
  end;
end;

{ TBusClientReaderListShortcut }

constructor TBusClientReaderListShortcut.Create;
begin
  FList := TList<TBusClientReader>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TBusClientReaderListShortcut.Destroy;
begin
  FreeAndNil(FList);
  FreeAndNil(FLock);
  inherited;
end;

function TBusClientReaderListShortcut.Lock: Tlist<TBusClientReader>;
begin
  FLock.Enter;
  Result := FList;
end;

procedure TBusClientReaderListShortcut.Unlock;
begin
  FLock.Leave;
end;

//------------------------------------------------------------------------------
//W A R N I N G
// TBus.Process([aobj...]) will be called by different thread :
// It build a "execution list" of the clientreaders (for speed) and execute it
// for the caller thread context.
//------------------------------------------------------------------------------
Procedure TBus.ProcessMessages(aClientReaders : Array of TBusClientReader);
var CL : TList<TBusClientReader>;
    mCL : TList<PTBusEnvelop>;
    mCL2 : TList<PTBusEnvelop>;
    i,k,q : integer;
    aReader : TBusClientReader;
begin
  //VGS : Above assert : Removed once more... Let this comment to ***not turn it back !!!***.
  //      Bus.processMessages is attended to be called by everywhere, in all thread we want.
  //  Assert(aClientReader.CreatorThreadId = GetThreadID);

  for k := Low(aClientReaders) to High(aClientReaders) do
  begin
    if not(assigned(aClientReaders[k])) then
      continue;

    aReader := aClientReaders[k];

    CL := FSubscribters.Lock;
    try
      { TODO -oVGS -cNice to have : Above assert : How to retrieve it wihout assert : a user can lauch hundread of reader in a useless way. Use an internal log ? to see. }
      //Assert(CL.IndexOf(aReader)>-1,ClassName+'.ProcessMessages : reader "'+IntToStr(k)+'" is not subscript to this Bus');
      if CL.IndexOf(aReader) = -1 then
        Continue;
    finally
      FSubscribters.Unlock;
    end;

    //aReader.ClientMessageStack *must* be stoped shorter than possible !
    //--> We make a quick transfert of message's pointer form the locked list to another new one.
    mcl2 := TList<PTBusEnvelop>.Create;
    try
      mcl := aReader.ClientMessageStack.Lock;
      try
        for i := 0 to mcl.Count-1 do
        begin
          if Assigned(aReader.CallBack) then
          begin
            mCL2.Add(mcl[i]); // Pointer copy only !
          end;
        end;
        mcl.Clear;
      finally
        aReader.ClientMessageStack.Unlock;
      end;

      q := 0;
      try
        for i := 0 to mcl2.Count-1 do
        begin
          q := i; //For Exception.
          aReader.IncProcessMessageCount;
          aReader.CallBack(Self,mcl2[i]^);
        end;
      Except
        On E : Exception do
        begin
          //TODO : Remove Reader when exception (Avoid Exception stack) ?
          //for instance  --> Exception.
          raise Exception.Create( 'Client CallBack from Bus : Exception in Callback for channel "'+
                                  aReader.ChannelListening+'" Message '+IntToStr(q)+' of '+IntToStr(mcl2.Count)+
                                  ' : ['+E.Message+']');
        end;
      end;
    finally
      FreeAndNil(mcl2);
    end;
  end;

end;

Function TBus.Send( var aMessage: TBusMessage;
                    aTargetChannel : String;
                    const aSomeAdditionalData : String;
                    const aResponseChannel : String;
                    const IsPersistant : Boolean) : Int64;
var aPacket : PTBusEnvelop;
    L : TList<PTBusEnvelop>;
begin
  Result := 0;
  if Terminated then
    Exit;

  New(aPacket);

  aPacket^.EnvelopId := FInternalMessageIdGenerator.Value; //For later use (ack ?)
  FInternalMessageIdGenerator.Inc;

  aPacket^.TargetChannel := aTargetChannel;
  aPacket^.AdditionalData := aSomeAdditionalData;
  aPacket^.ResponseChannel := aResponseChannel;
  aPacket^.ContentMessage := aMessage; //Deep copy;
  aPacket^.Persistent := IsPersistant;
  Result := aPacket^.EnvelopId;

  L := FWaitMessageList.Lock;
  try
    L.Add(aPacket);
  finally
    FWaitMessageList.Unlock;
    FDoWork.SetEvent;
    FTotalMessageSend.Inc;
  end;
end;


Function TBus.Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
var aNewChannel : TBusChannel;
var i : integer;
    CL : TObjectList<TBusChannel>;
    C : TList<TBusClientReader>;
    CD : TList<TBusClientReader>;

    ll,llo : TList<PTBusEnvelop>;
    aPacket : PTBusEnvelop;
begin
  Assert(Assigned(CallBack));
  Assert(aChannelName <> EmptyStr);
  Result := TBusClientReader.Create(aChannelName, CallBack);
  //Channel : Find or create it.
  aNewChannel := Nil;
  CL := FChannels.Lock;
  try
    for I := 0 to CL.Count-1 do
    begin
      //Channel case sensitive ? Option ? todo...
      if CL[i].ChannelName = aChannelName then
      begin
        aNewChannel := CL[i];
        Break;
      end;
    end;

    if not(Assigned(aNewChannel)) then
    begin
      aNewChannel := TBusChannel.Create(aChannelName);
      CL.Add(aNewChannel);
    end;

    //Add the subscribter to the channel (ShortCut).
    C := aNewChannel.Subscribters.Lock;
    try
      C.Add(Result);
    finally
      aNewChannel.Subscribters.Unlock;
    end;

    //Add new client to the reference subscibter list (Reference).
    CD := FSubscribters.Lock;
    try
      CD.Add(Result);
    finally
      FSubscribters.Unlock;
    end;

    //Finally, deliver all potentiel persistant message of the channel to the new subscribter
    if FTotalMessagePersistant.Value>0 then
    begin
      ll := Result.ClientMessageStack.Lock;
      llo := aNewChannel.PersistantMessageLock;
      try
        for I := 0 to llo.Count-1 do
        begin
          New(aPacket);
          aPacket^.EnvelopId := llo[i]^.EnvelopId;
          aPacket^.AdditionalData := llo[i]^.AdditionalData;
          aPacket^.TargetChannel := aNewChannel.ChannelName;
          aPacket^.ResponseChannel := llo[i]^.ResponseChannel;
          aPacket^.ContentMessage := llo[i]^.ContentMessage; //Deep copy;
          aPacket^.Persistent := llo[i]^.Persistent;
          ll.Add(aPacket);
        end;
      finally
        Result.ClientMessageStack.Unlock;
        aNewChannel.PersistantMessageUnlock;
      end;
    end;
  finally
    FChannels.Unlock;
  end;
end;

function TBus.Unsubscribe(aClient: TBusClientReader;
  aChannelName: String): Boolean;
var i : integer;
    lChannel : TBusChannel;
    CL : TObjectList<TBusChannel>;

    C : TList<TBusClientReader>;
    CD : TList<TBusClientReader>;
begin
  Result := false;
  Assert(Assigned(aClient));
  Assert(aChannelName <> EmptyStr);
  //Channel : Find it.
  lChannel := Nil;
  CL := FChannels.Lock;
  try
    for I := 0 to CL.Count-1 do
    begin
      //Channel case sensitive ? Option ? todo...
      if CL[i].ChannelName = aChannelName then
      begin
        lChannel := CL[i];
        Break;
      end;
    end;

    if not(Assigned(lChannel)) then
    begin  //Channel does not exists : Exit ;)
      Exit;
    end;

    //we do not forget to remove that from Channel.Subscripters list, which is a mirror of FSubscripter reference list.
    C := lChannel.Subscribters.Lock;
    try
      i := c.IndexOf(aClient);
      if i>-1 then
        c.Delete(i); //It a shortcut list : not the real object owned.
    finally
      lChannel.Subscribters.Unlock;
    end;

    //find the client to subscribter reference list, and remove it.
    CD := FSubscribters.Lock;
    try
      i := CD.IndexOf(aClient);
      if i>-1 then
      begin
        CD.Delete(i); //aClient is NOT destroy : As documented, it is CLIENT responsability to free its client access.
        Result := True;
      end;
    finally
      FSubscribters.Unlock;
    end;

    //TODO : Historize
    //TODO : What todo with empty channel ?

  finally
    FChannels.Unlock;
  end;
end;

{ TBusMessage }

{ TBusEnvelopList }

constructor TBusEnvelopList.Create;
begin
  FList := TList<PTBusEnvelop>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TBusEnvelopList.Destroy;
var i : integer;
    ap : pTBusEnvelop;
begin
  FLock.Acquire;
  try
    for I := 0 to FList.Count-1 do
    begin
      ap := FList[i];
      Dispose(ap);
    end;
  finally
    FLock.Release;
  end;
  FreeAndNil(flist);
  FreeAndNil(FLock);
  inherited;
end;

function TBusEnvelopList.Lock: Tlist<PTBusEnvelop>;
begin
  FLock.Enter;
  Result := FList;
end;

procedure TBusEnvelopList.Unlock;
begin
  FLock.Release;
end;

{ TBusChannel }

constructor TBusChannel.Create(aChannelName : String);
begin
  Inherited Create;
  Assert(aChannelName<>EmptyStr);
  FChannel := TProtectedString.Create(aChannelName);
  FSubscibters := TBusClientReaderListShortcut.Create;
  FReceivedMessageCount := TProtectedInt64.Create(0);
  FConsumedMessageCount := TProtectedInt64.Create(0);
  FBehaviour := bcbTopic;
  FBehaviourInfo := TBusChannelBehaviourTopic.Create(Self);
  FPersistantMessage := TBusEnvelopList.Create;
  FMessageInThisChannelWillBeSetAsPersistant:= TProtectedBoolean.Create(False);
end;

destructor TBusChannel.Destroy;
begin
  FreeAndNil(FChannel);
  FreeAndNil(FSubscibters);
  FreeAndNil(FReceivedMessageCount);
  FreeAndNil(FConsumedMessageCount);
  FreeAndNil(FBehaviourInfo);
  FreeAndNil(FPersistantMessage);
  FreeAndNil(FMessageInThisChannelWillBeSetAsPersistant);
  inherited;
end;

function TBusChannel.GetChannel: string;
begin
  result := FChannel.Value;
end;

function TBusChannel.GetConsumedMessageCount: Int64;
begin
  Result := FConsumedMessageCount.Value;
end;

function TBusChannel.GetCurrentSubscribterCount: Int64;
var l : Tlist<TBusClientReader>;
begin
  l := Subscribters.Lock;
  try
    Result := l.Count;
  finally
    Subscribters.Unlock;
  end;
end;

function TBusChannel.GetMessageInThisChannelWillBeSetAsPersistant: Boolean;
begin
  result := FMessageInThisChannelWillBeSetAsPersistant.Value;
end;

function TBusChannel.GetPersistantMessageCount: Int64;
var ll : Tlist<PTBusEnvelop>;
begin
  ll := FPersistantMessage.Lock;
  try
    Result := ll.Count;
  finally
    FPersistantMessage.Unlock;
  end;
end;

function TBusChannel.GetReceivedMessageCount: Int64;
begin
  Result := FReceivedMessageCount.Value;
end;

procedure TBusChannel.IncConsumedMessageCount;
begin
  FConsumedMessageCount.Inc;
end;

procedure TBusChannel.IncReceivedMessageCount;
begin
  FReceivedMessageCount.Inc;
end;

function TBusChannel.PersistantMessageLock: Tlist<PTBusEnvelop>;
begin
  Result := FPersistantMessage.Lock;
end;

procedure TBusChannel.PersistantMessageUnlock;
begin
  FPersistantMessage.Unlock;
end;

procedure TBusChannel.SetChannelBehaviour(const Value: TBusChannelBehaviour);
begin
  if FBehaviour<>Value then
  begin
    FreeAndNil(FBehaviourInfo);
    FBehaviour := Value;
    case FBehaviour of
      bcbTopic: FBehaviourInfo := TBusChannelBehaviourTopic.Create(Self);
      bcbQueue: FBehaviourInfo := TBusChannelBehaviourQueue.Create(Self);
    end;
  end;
end;

procedure TBusChannel.SettMessageInThisChannelWillBeSetAsPersistant(
  const Value: Boolean);
begin
  FMessageInThisChannelWillBeSetAsPersistant.Value := Value;
end;

{ TBusChannelList }

constructor TBusChannelList.Create;
begin
  Inherited;
  FList := TObjectlist<TBusChannel>.Create;
  FLock := TCriticalSection.Create;
end;

procedure TBusChannelList.CreateOrSetChannel(aChannelName: String;
  aChannelBehaviourType: TBusChannelBehaviour;
  Const aMessageWillBePersistant : Boolean);
var CL : TObjectList<TBusChannel>;
    i : integer;
    C : TBusChannel;
Begin
  CL := Lock;
  try
    C := Nil;
    for I := CL.Count-1 downto 0 do
    begin
      if CL[i].ChannelName = aChannelName then
      begin
        C := CL[i];
      end;
    end;

    if Not(assigned(C)) then
    begin
      C := TBusChannel.Create(aChannelName);
      CL.Add(C);
    end;

    C.ChannelBehaviour := aChannelBehaviourType;
    C.MessageInThisChannelWillBeSetAsPersistant := aMessageWillBePersistant;

  finally
    Unlock;
  end;
end;

procedure TBusChannelList.DeleteChannel(aChannelName: string);
var CL : TObjectList<TBusChannel>;
    i : integer;
Begin
  CL := Lock;
  try
    for I := CL.Count-1 downto 0 do
    begin
      if CL[i].ChannelName = aChannelName then
      begin
        CL.Delete(i); //Own object.
      end;
    end;
  finally
    Unlock;
  end;
end;

destructor TBusChannelList.Destroy;
begin
  FreeAndNil(FList); //ObjectList owned items.
  FreeAndNil(FLock);
  inherited;
end;

function TBusChannelList.Lock: TObjectlist<TBusChannel>;
begin
  FLock.Enter;
  Result := FList;
end;

procedure TBusChannelList.SetChannelOnBeforeDeliverEvent(
  aChannelName: String; aChannelProc: TOnBusChannelBeforeDeliverMessage);
var CL : TObjectList<TBusChannel>;
    i : integer;
    C : TBusChannel;
Begin
  CL := Lock;
  try
    C := Nil;
    for I := CL.Count-1 downto 0 do
    begin
      if CL[i].ChannelName = aChannelName then
      begin
        C := CL[i];
      end;
    end;

    if Not(assigned(C)) then
    begin
      C := TBusChannel.Create(aChannelName);
      CL.Add(C);
    end;

    C.OnBeforeDeliverMessage := aChannelProc;
  finally
    Unlock;
  end;
end;

procedure TBusChannelList.Unlock;
begin
  FLock.Leave;
end;

{ TBusClient }

constructor TBusClient.Create;
begin
  Inherited Create;
//  FCreatorThreadID := GetThreadID;
  ClientMessageStack := TBusEnvelopList.Create;
  Event := Nil;
end;

destructor TBusClient.Destroy;
begin
  FreeAndNil(ClientMessageStack); //ClientMessageStack freed its messages.
  //FreeAndNil(Event)....NO ! Managed by the bus, because could be shared between client.
  inherited;
end;

function TBusClient.GetClientPendingMessageCount: Int64;
var L : TList<PTBusEnvelop>;
begin
  L := ClientMessageStack.Lock;
  try
    Result := L.Count;
  finally
    ClientMessageStack.Unlock;
  end;
end;


{ TBusClientReader }

constructor TBusClientReader.Create(aChannelName : String; aCallBack: TBusMessageNotify);
begin
  Assert(aChannelName <> EmptyStr);
  Inherited Create;
  FCallBack := aCallBack;
  FChannel := aChannelName;
  FProcessMessageCount := TProtectedInt64.Create(0);
end;

destructor TBusClientReader.Destroy;
begin
  FreeAndNil(FProcessMessageCount);
  inherited;
end;

function TBusClientReader.GetClientProcessMessageCount: Int64;
begin
  result := FProcessMessageCount.Value;
end;

procedure TBusClientReader.IncProcessMessageCount;
begin
  FProcessMessageCount.Inc;
end;

{ TBusClientReaderList }

constructor TBusClientReaderList.Create;
begin
  FList := Tlist<TBusClientReader>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TBusClientReaderList.Destroy;
begin
  FreeAndNil(FList); //Client resp. to clean all.
  FreeAndNil(FLock);
  inherited;
end;

function TBusClientReaderList.Lock: TList<TBusClientReader>;
begin
  FLock.Enter;
  Result := FList;
end;

procedure TBusClientReaderList.Unlock;
begin
  FLock.Leave;
end;

{ TBusMessage }

function TBusMessage.AsByte: Byte;
begin
  if length(Buffer) >0 then
    Result := Buffer[0]
  else
    raise Exception.Create('TBusMessage.asByte : Read error');
end;

function TBusMessage.AsStream: TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(Buffer[0],Length(Buffer));
  Result.Position := 0;
end;

function TBusMessage.AsString: String;
begin
  Result := TEncoding.UTF8.GetString(Buffer);
end;

procedure TBusMessage.FromByte(aByte: Byte);
begin
  SetLength(Buffer,1);
  Buffer[0] := aByte;
end;

procedure TBusMessage.FromStream(aStream: TMemoryStream);
begin
  aStream.Position := 0;
  SetLength(Buffer,aStream.Size);
  aStream.Read(Buffer[0],aStream.Size);
end;

procedure TBusMessage.FromString(aText: String);
begin
  Buffer := TEncoding.UTF8.GetBytes(aText);
end;

{ TBusChannelAdditionalInformation }

constructor TBusChannelAdditionalInformation.Create(
  aMasterChannel: TBusChannel);
begin
  inherited Create;
  Assert(Assigned(AMasterChannel));
  FMasterChannel := aMasterChannel;
end;

{ TBusChannelBehaviourQueue }

constructor TBusChannelBehaviourQueue.Create(aMasterChannel: TBusChannel);
begin
  inherited;
  FLastClientIndexServed := 0;
end;

Initialization

Bus := Nil;

Finalization

//ReleaseStandartBus;

end.
