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
///
/// History
/// 20180524 - VGS - Added Wilcard "message dispatch" capabilities
///                  (i.e a message launch on "ABC\DEF\GHI" channel, will be
///                  delivered on "ABC\DEF\GHI", "ABC\DEF" and "ABC".
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
  SyncObjs;
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  System.Threading;
{$ENDIF}

Const
  CST_BUSTIMER = 250; //MilliSec.
  CST_DEFAULT_WILDCARD = '\';
Type
TBusSystem = Class;

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

TBusMessageNotify = Procedure(Sender : TObject;Var Packet : TBusEnvelop) Of Object;
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

TBusChannelData = class
protected
  FLock : TCriticalSection;
  FChannel: String;
  FReceivedMessageCount : Int64;
  FConsumedMessageCount : Int64;
  FMessageInThisChannelWillBeSetAsPersistent : Boolean;
public
  Procedure Lock;
  Procedure Unlock;

  Constructor Create; Virtual;
  Destructor Destroy; Override;

end;

//WARNING : All TBUSChannel Event (included event) is processed within a thread.
TOnBusChannelBeforeDeliverMessage = Procedure(Var aMessage : TBusEnvelop) of Object;
TBusChannel = Class
private
  function GetConsumedMessageCount: Int64;
  function GetReceivedMessageCount: Int64;
  procedure SetChannelBehaviour(const Value: TBusChannelBehaviour);
  function GetPersistentMessageCount: Int64;
  function GetMessageInThisChannelWillBeSetAsPersistent: Boolean;
  procedure SettMessageInThisChannelWillBeSetAsPersistent(const Value: Boolean);
  function GetChannel: string;
Protected
  FBusChannelData : TBusChannelData;
  FOnBeforeDeliverMessage: TOnBusChannelBeforeDeliverMessage;
  FSubscibters : TBusClientReaderListShortcut;
  FBehaviour: TBusChannelBehaviour;
  FBehaviourInfo: TBusChannelAdditionalInformation;

  FPersistentMessage : TBusEnvelopList;

  function GetCurrentSubscribterCount: Int64;
Public
  Constructor Create(aChannelName : string); Reintroduce;
  Destructor Destroy; Override;

  Function PersistentMessageLock : Tlist<PTBusEnvelop>;
  Procedure PersistentMessageUnlock;

  Procedure IncReceivedMessageCount;
  Procedure IncConsumedMessageCount;

  Property ChannelName : string read GetChannel;
  Property ReceivedMessageCount : Int64 read GetReceivedMessageCount;
  Property ConsumedMessageCount : Int64 read GetConsumedMessageCount;
  Property CurrentSubscribterCount : Int64 read GetCurrentSubscribterCount;
  Property PersistentMessageCount : Int64 read GetPersistentMessageCount;

  Property Subscribters : TBusClientReaderListShortcut read FSubscibters;

  Property ChannelBehaviour : TBusChannelBehaviour read FBehaviour write SetChannelBehaviour;
  Property ChannelBehaviourInfo : TBusChannelAdditionalInformation read FBehaviourInfo write FBehaviourInfo;

  Property MessageInThisChannelWillBeSetAsPersistent : Boolean read GetMessageInThisChannelWillBeSetAsPersistent
                                                               write SettMessageInThisChannelWillBeSetAsPersistent;

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
  Event : TEvent; //Pointer ! Use it for thread use only, *NOT* managed by bus.

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Property PendingMessageCount : Int64 read GetClientPendingMessageCount;
  Property ProcessMessageCount : Int64 read GetClientProcessMessageCount;
End;

TBusClientReader = Class(TBusClient)
private
  FProcessMessageCount : Int64;
  FCallBack: TBusMessageNotify;
  FChannel: String;
Protected
  function GetClientProcessMessageCount: Int64; Override;
Public
  Procedure IncProcessMessageCount;

  //Not aimed to be call directly : Call Bus.Subscribt.
  Constructor Create(aChannelName : String; aCallBack : TBusMessageNotify); Reintroduce;

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
                                Const aMessageWillBePersistent : Boolean = False);
  //Set an event, which will trig when a message is delivered on this channel.
  //Warning : It it the thread of the bus whitch will process the event ! Keep it thread safe and beware
  //          to not take too many time in this event : Other message will not be dispached during this time.
  Procedure SetChannelOnBeforeDeliverEvent(aChannelName : String; aChannelProc : TOnBusChannelBeforeDeliverMessage);


  Function Lock : TObjectlist<TBusChannel>;
  Procedure Unlock;
end;

TBusClientReaderArray = Array of TBusClientReader;
TBusClientReaderList = Class
Private
  FList : Tlist<TBusClientReader>;
  FLock : TCriticalSection;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function ToArray : TBusClientReaderArray;

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

TBusSystem = Class
Private
  FLockStat : TCriticalSection;
  FLockPropertyBasic : TCriticalSection;

  FTotalMessageSend : Int64;
  FTotalMessagePending : Int64;
  FTotalMessageProcessed : Int64;
  FTotalMessagePersistent : Int64;
  FWildcardChar : Char;
  FWildcardEnabled : Boolean;

  FInternalMessageIdGenerator : Int64;               //Message generator ID. Not used today (Just a incremental), certainly better system when needed.
  FWaitMessageList : TBusEnvelopList;                //Message pending. (Where clients post)
  FMessageList : TBusEnvelopList;                    //Message list (TBusSystem use this list to distribute effectively).
  FChannels : TBusChannelList;                       //List of all channels.
  FDoWork : TEvent;                                  //Event signaled when "Send" is called : It start message processing (Execute).
  FSubscribters : TBusClientReaderList;              //Raw Subscripter list. Reference. : WARNING : In channel object, there is shortcut to content object. Keep it synchro.

  //Work variables.
  FpChannel : TBusChannel;
  FMes : PTBusEnvelop;

  //Distibute a message to a subscribter.
  Procedure DoSubscribterWork;
  Procedure DoNotifyClients;


  function GetStats: String;
  function GetWildCardEnabled: Boolean;
  function GetWildCardSeparator: Char;
  procedure SetWildCardEnabled(const Value: Boolean);
  procedure SetWildCardSeparator(const Value: Char);
Protected
  Procedure BusShutDown; Virtual;
Public
  constructor Create; overload; Virtual;
  Destructor Destroy; Override;

  //Call this only if you have to pulse inner logic of Bus : in a timer, in OnIdle
  //(For those 2, modify CST_TIMER Value to a very low value), or in a thread Execute (And here CST_TIMER is ok).
  //this will : consume awaiting message, dispatch messages folowing channel rules.
  //You have than to call ProcessMessages to consume dispatched message.
  Procedure BusExecute; Virtual;

  //Key methods : Call that in your coin of world (in another thread or not) to check your message box ;)
  Procedure ProcessMessages(Const aClientReaders : Array of TBusClientReader);


  //WARNING : In current design, client is not owned by BusSystem : client responsability to free it.
  Function Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
  Function UnSubscribe(aClient : TBusClientReader) : Boolean;

  Function Send( var aMessage : TBusMessage;
                 const aTargetChannel : String;
                 const aSomeAdditionalData : String = '';
                 const aResponseChannel : string = '';
                 const IsPersistent : Boolean = False) : Int64;

  Procedure GetChannelsConfigurationAsCSV(var aStr : TStringList);
  Procedure GetSubscribtersConfigurationAsCSV(var aStr : TStringList);


  Function GetNewEvent : TEvent;

  Procedure ChannelDelete(aChannelName : string);
  Procedure ChannelSet( aChannelName : String;
                        aChannelBehaviourType : TBusChannelBehaviour;
                        Const aMessageWillBePersistent : Boolean = False);
  Procedure ChannelSetOnBeforeDeliverMessageEvent( aChannelName: String;
                                                   aChannelProc: TOnBusChannelBeforeDeliverMessage);


  Property Stats : String read GetStats;
  property WildcardEnabled : Boolean read GetWildCardEnabled Write SetWildCardEnabled;
  property WildcardSeparator : Char read GetWildCardSeparator Write SetWildCardSeparator;
End;


//Thread bus wrapper.
TBus = class(TThread)
  private
    function GetStats: String;
    function GetWildCardEnabled: Boolean;
    function GetWildCardSeparator: Char;
    procedure SetWildCardEnabled(const Value: Boolean);
    procedure SetWildCardSeparator(const Value: Char);
protected
  Sys : TBusSystem;
  Procedure BusShutDown; Virtual;
Public
  constructor Create; Reintroduce; virtual;
  Destructor Destroy; Override;
  Procedure Execute; Override;

  ///All the bellow method is only "shortcut" method to the BusSystem.
  ///
  ///  TBusSystem object is fully Multi-threaded friendly.
  ///  you can share it between many thread. It is done for that.
  ///
  Function Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
  Function UnSubscribe(aClient : TBusClientReader) : Boolean;

  Function Send( var aMessage : TBusMessage;
                 aTargetChannel : String;
                 const aSomeAdditionalData : String = '';
                 const aResponseChannel : string = '';
                 const IsPersistent : Boolean = False) : Int64;

  Procedure GetChannelsConfigurationAsCSV(var aStr : TStringList);
  Procedure GetSubscribtersConfigurationAsCSV(var aStr : TStringList);

  Procedure ProcessMessages(Const aClientReaders : Array of TBusClientReader);

  Function GetNewEvent : TEvent;

  Procedure ChannelDelete(aChannelName : string);
  Procedure ChannelSet( aChannelName : String;
                        aChannelBehaviourType : TBusChannelBehaviour;
                        Const aMessageWillBePersistent : Boolean = False);
  Procedure ChannelSetOnBeforeDeliverMessageEvent( aChannelName: String;
                                                   aChannelProc: TOnBusChannelBeforeDeliverMessage);

  Property Stats : String read GetStats;
  property WildcardEnabled : Boolean read GetWildCardEnabled Write SetWildCardEnabled;
  property WildcardSeparator : Char read GetWildCardSeparator Write SetWildCardSeparator;
end;

var Bus : TBus;

Procedure StartStandartBus;
Procedure ReleaseStandartBus;

Function AtomicIncrement64(var a : Int64) : Int64;
Function AtomicDecrement64(var a : Int64) : Int64;


implementation

{ TBusSystem }
Function AtomicIncrement64(var a : Int64) : Int64;
begin
  {$IFDEF FPC}
  result := InterLockedIncrement64(a);
  {$END}
  {$ELSE}
  result := AtomicIncrement(a);
  {$ENDIF}
end;

Function AtomicDecrement64(var a : Int64) : Int64;
begin
  {$IFDEF FPC}
  result := InterLockedDecrement64(a);
  {$END}
  {$ELSE}
  result := AtomicDecrement(a);
  {$ENDIF}
end;


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
    FreeAndNil(Bus);
  end;
end;

procedure TBusSystem.ChannelSet(aChannelName: String;
  aChannelBehaviourType: TBusChannelBehaviour;
  const aMessageWillBePersistent: Boolean);
begin
  FChannels.CreateOrSetChannel( aChannelName,
                                aChannelBehaviourType,
                                aMessageWillBePersistent);
end;

procedure TBusSystem.ChannelSetOnBeforeDeliverMessageEvent(aChannelName: String;
  aChannelProc: TOnBusChannelBeforeDeliverMessage);
begin
  FChannels.SetChannelOnBeforeDeliverEvent(aChannelName,aChannelProc);
end;

constructor TBusSystem.Create;
begin
  Inherited Create;
  FLockStat := TCriticalSection.Create;
  FLockPropertyBasic := TCriticalSection.Create;
  FWaitMessageList := TBusEnvelopList.Create;
  FMessageList := TBusEnvelopList.Create;
  FChannels := TBusChannelList.Create;
  FSubscribters := TBusClientReaderList.Create;
  FDoWork := TEvent.Create(nil,False,False,EmptyStr);
  FInternalMessageIdGenerator := 0;
  FTotalMessageSend := 0;
  FTotalMessagePending := 0;
  FTotalMessageProcessed := 0;
  FTotalMessagePersistent := 0;
  FWildcardChar := CST_DEFAULT_WILDCARD;
  FWildcardEnabled := False;
end;

procedure TBusSystem.BusShutDown;
begin
  FDoWork.ResetEvent;
  FDoWork.SetEvent;
end;

procedure TBusSystem.ChannelDelete(aChannelName: string);
begin
  FChannels.DeleteChannel(aChannelName);
end;

destructor TBusSystem.Destroy;
begin
  BusShutdown;
  FreeAndNil(FDoWork);
  FreeAndNil(FMessageList);
  FreeAndNil(FWaitMessageList);
  FreeAndNil(FSubscribters);
  FreeAndNil(FChannels);
  FreeAndNil(FLockStat);
  FreeAndNil(FLockPropertyBasic);
  inherited;
end;

procedure TBusSystem.DoNotifyClients;
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

procedure TBusSystem.DoSubscribterWork;
var aPacket : PTBusEnvelop;
    ClientList : TList<TBusClientReader>;
    L : TList<PTBusEnvelop>;
    i : Integer;
    ClientIndex : Integer;


    Procedure PrepareAndSavePersistentMessage;
    var ll : TList<PTBusEnvelop>;
    begin
      ll := FpChannel.PersistentMessageLock;
      try
        New(aPacket);
        aPacket^.EnvelopId := FMes^.EnvelopId;
        aPacket^.AdditionalData := FMes^.AdditionalData;
        aPacket^.TargetChannel := FpChannel.ChannelName;
        aPacket^.ResponseChannel := FMes^.ResponseChannel;
        aPacket^.ContentMessage := FMes^.ContentMessage; //Deep copy;
        aPacket^.Persistent := True;
        ll.Add(aPacket);
        AtomicIncrement64(FTotalMessagePersistent);
      finally
        FpChannel.PersistentMessageUnlock;
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

      if FpChannel.MessageInThisChannelWillBeSetAsPersistent then
      begin
        //Even if the message is not persitant, the channel is set as auto Persistent.
        //We turn the message as persitant one. It will be copied in private list just after.
        aPacket^.Persistent := True;
      end;

      L.Add(aPacket);
      FpChannel.IncConsumedMessageCount;
      AtomicIncrement64(FTotalMessageProcessed);

      if FMes.Persistent then
      begin
        PrepareAndSavePersistentMessage;
      end;
    end;
begin
  ClientList := FpChannel.Subscribters.Lock;
  try
    if ClientList.Count=0 then
    begin
      //No client : Only process Persistent message if needed.
      if (FpChannel.MessageInThisChannelWillBeSetAsPersistent) Or
         (FMes.Persistent) then
      begin
        PrepareAndSavePersistentMessage;
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


procedure TBusSystem.BusExecute;
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
          AtomicIncrement64(FTotalMessagePending);
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
//                On E : Exception do
//                begin
//                  FpChannel.OnBeforeDeliverMessage := Nil; //Descativated ?
//                  raise Exception.Create('Error processed onBeforeDelivered : "'+e.Message+'". Event Handler desactivated.');
//                end;
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
          AtomicDecrement64(FTotalMessagePending);
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
      LocalInternalTransfertMessage;
      LocalInternalDispatchToSubscribterAndNotify;
      LocalInternalCleaning;
    end;
    wrTimeout :
    begin
      //Todo : Generate Idle message ?;
    end;
    wrAbandoned, wrError {$IFNDEF FPC}, wrIOCompletion {$ENDIF} :
    begin
      //Todo : Exception message. Stop ?
    end;
  end;
end;


procedure TBusSystem.GetChannelsConfigurationAsCSV(var aStr: TStringList);
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

    Function GetIsChannelPersistent : String;
    begin
      result := 'No';
      if c.MessageInThisChannelWillBeSetAsPersistent then
        result := 'Yes';
    end;
begin
//  if Terminated then
//    Exit;
  Assert(Assigned(aStr));
  aStr.Clear;
  s2 := TStringList.Create;
  CL := FChannels.Lock;
  try
    s2.Add('ChannelName');
    s2.Add('ChannelType');
    s2.Add('IsChannelPersistent');
    s2.Add('ReceivedMessageCount');
    s2.Add('ConsumedMessageCount');
    s2.Add('PersistentMessageCount');
    s2.Add('SubscribterCount');
    aStr.Add(s2.DelimitedText);
    for I := 0 to CL.Count-1 do
    begin
      c := CL[i];
      s2.Clear;
      s2.Add(c.ChannelName);
      s2.Add(GetChannelType);
      s2.Add(GetIsChannelPersistent);
      s2.Add(IntToStr(c.ReceivedMessageCount));
      s2.Add(IntToStr(c.ConsumedMessageCount));
      s2.Add(IntToStr(c.PersistentMessageCount));
      s2.Add(IntToStr(c.CurrentSubscribterCount));
      aStr.Add(s2.DelimitedText);
    end;
  finally
    FreeAndNil(s2);
    FChannels.Unlock;
  end;
end;

function TBusSystem.GetNewEvent: TEvent;
begin
  Result := TEvent.Create(nil,False,False,EmptyStr);
end;

function TBusSystem.GetStats: String;
begin
//---CRITICAL
  //TODO : Jsonify ?
  Result := 'Messages - Send : ' +IntTostr(FTotalMessageSend) +
            ' Pending : ' +IntTostr(FTotalMessagePending) +
            ' Processed : ' +IntTostr(FTotalMessageProcessed);
end;

procedure TBusSystem.GetSubscribtersConfigurationAsCSV(var aStr: TStringList);
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
//  if Terminated then
//    Exit;
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

function TBusSystem.GetWildCardEnabled: Boolean;
begin
  FLockPropertyBasic.Enter;
  try
    Result := FWildcardEnabled;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

function TBusSystem.GetWildCardSeparator: Char;
begin
  FLockPropertyBasic.Enter;
  try
    result := FWildcardChar;
  finally
    FLockPropertyBasic.Leave;
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
// TBusSystem.Process([aobj...]) will be called by different thread :
// It build a "execution list" of the clientreaders (for speed) and execute it
// for the caller thread context.
//------------------------------------------------------------------------------
Procedure TBusSystem.ProcessMessages(Const aClientReaders : Array of TBusClientReader);
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
          dispose(mCL2[i]);
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

Function TBusSystem.Send( var aMessage: TBusMessage;
                    const aTargetChannel : String;
                    const aSomeAdditionalData : String;
                    const aResponseChannel : String;
                    const IsPersistent : Boolean) : Int64;
var aPacket : PTBusEnvelop;
    L : TList<PTBusEnvelop>;
    lTempChannel : String;
    ltempstr : TStringList;
    i : integer;
    FWEnabled : Boolean;
    FWchr : char;

    Procedure InternalSendMessage(aChannel : String);
    begin
      New(aPacket);
      FLockStat.Enter;
      try
        aPacket^.EnvelopId := FInternalMessageIdGenerator; //For later use (ack ?)
        FInternalMessageIdGenerator := FInternalMessageIdGenerator + 1;
      finally
        FLockStat.Leave;
      end;
      aPacket^.TargetChannel := aChannel;
      aPacket^.AdditionalData := aSomeAdditionalData;
      aPacket^.ResponseChannel := aResponseChannel;
      aPacket^.ContentMessage := aMessage; //Deep copy;
      aPacket^.Persistent := IsPersistent;
      Result := aPacket^.EnvelopId;
      L := FWaitMessageList.Lock;
      try
        L.Add(aPacket);
      finally
        FWaitMessageList.Unlock;
        FDoWork.SetEvent;
        AtomicIncrement64(FTotalMessageSend);
      end;
    end;
begin
//  if Terminated then
//    Exit;
  Result := 0;

  //Send message with full targetname.
  InternalSendMessage(aTargetChannel);


  //Decode wildcard to dispatch to other path.
  ///(i.e a message launch on "ABC\123\456" channel, will be
  ///delivered on "ABC\123\456", "ABC\123" and "ABC".
  ///
  FLockPropertyBasic.Enter;
  try
    FWEnabled := FWildcardEnabled;
    FWchr := FWildcardChar;
  finally
    FLockPropertyBasic.Leave;
  end;

  if FWEnabled then
  begin
    if Pos(WildcardSeparator,aTargetChannel)>0 then
    begin
      ltempStr := TStringList.Create;
      try
        lTempChannel := aTargetChannel;
        ltempstr.Delimiter := FWchr;
        ltempstr.DelimitedText := lTempChannel;
        ltempstr.Delete(ltempstr.Count-1);
        while ltempstr.Count>0 do
        begin
          lTempChannel := ltempstr[0];
          if ltempstr.Count>1 then
          for I := 1 to ltempstr.Count-1 do
          begin
            lTempChannel := lTempChannel + ltempstr.Delimiter + ltempstr[i];
          end;
          InternalSendMessage(lTempChannel);
        end;
      finally
        FreeAndNil(ltempstr);
      end;
    end;
  end;

end;


procedure TBusSystem.SetWildCardEnabled(const Value: Boolean);
begin
  FLockPropertyBasic.Enter;
  try
    FWildcardEnabled := Value;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

procedure TBusSystem.SetWildCardSeparator(const Value: char);
begin
  FLockPropertyBasic.Enter;
  try
    if length(value)>0 then
      FWildcardChar := Value;
  finally
    FLockPropertyBasic.Leave;
  end;
end;

Function TBusSystem.Subscribe(aChannelName : String; CallBack : TBusMessageNotify) : TBusClientReader;
var aNewChannel : TBusChannel;
var i : integer;
    CL : TObjectList<TBusChannel>;
    C : TList<TBusClientReader>;
    CD : TList<TBusClientReader>;

    ll,llo : TList<PTBusEnvelop>;
    aPacket : PTBusEnvelop;
    tm : Int64;
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

    //Finally, deliver all potentiel Persistent message of the channel to the new subscribter
    FLockPropertyBasic.Enter;
    try
      tm := FTotalMessagePersistent;
    finally
      FLockPropertyBasic.Leave;
    end;

    if tm>0 then
    begin
      ll := Result.ClientMessageStack.Lock;
      llo := aNewChannel.PersistentMessageLock;
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
        aNewChannel.PersistentMessageUnlock;
      end;
    end;
  finally
    FChannels.Unlock;
  end;
end;

function TBusSystem.Unsubscribe(aClient: TBusClientReader): Boolean;
var i : integer;
    lChannel : TBusChannel;
    lChannelName : String;
    CL : TObjectList<TBusChannel>;

    C : TList<TBusClientReader>;
    CD : TList<TBusClientReader>;
begin
  Result := false;
  Assert(Assigned(aClient));
  lChannelName := aClient.ChannelListening;
  Assert(lChannelName <> EmptyStr);
  //Channel : Find it.
  lChannel := Nil;
  CL := FChannels.Lock;
  try
    for I := 0 to CL.Count-1 do
    begin
      //Channel case sensitive ? Option ? todo...
      if CL[i].ChannelName = lChannelName then
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
  FBusChannelData := TBusChannelData.Create;
  FBusChannelData.FChannel := aChannelName;
  FBusChannelData.FReceivedMessageCount := 0;
  FBusChannelData.FConsumedMessageCount := 0;
  FBusChannelData.FMessageInThisChannelWillBeSetAsPersistent:= false;

  FSubscibters := TBusClientReaderListShortcut.Create;
  FBehaviour := bcbTopic;
  FBehaviourInfo := TBusChannelBehaviourTopic.Create(Self);
  FPersistentMessage := TBusEnvelopList.Create;
end;

destructor TBusChannel.Destroy;
begin
  FreeAndNil(FBusChannelData);
  FreeAndNil(FSubscibters);
  FreeAndNil(FBehaviourInfo);
  FreeAndNil(FPersistentMessage);
  inherited;
end;

function TBusChannel.GetChannel: string;
begin
  FBusChannelData.Lock;
  try
    result := FBusChannelData.FChannel;
  finally
    FBusChannelData.Unlock;
  end;
end;

function TBusChannel.GetConsumedMessageCount: Int64;
begin
  FBusChannelData.Lock;
  try
    Result := FBusChannelData.FConsumedMessageCount;
  finally
    FBusChannelData.Unlock;
  end;
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

function TBusChannel.GetMessageInThisChannelWillBeSetAsPersistent: Boolean;
begin
  FBusChannelData.Lock;
  try
    result := FBusChannelData.FMessageInThisChannelWillBeSetAsPersistent;
  finally
    FBusChannelData.Unlock
  end;
end;

function TBusChannel.GetPersistentMessageCount: Int64;
var ll : Tlist<PTBusEnvelop>;
begin
  ll := FPersistentMessage.Lock;
  try
    Result := ll.Count;
  finally
    FPersistentMessage.Unlock;
  end;
end;

function TBusChannel.GetReceivedMessageCount: Int64;
begin
  FBusChannelData.Lock;
  try
  Result := FBusChannelData.FReceivedMessageCount;
  finally
    FBusChannelData.Unlock;
  end;
end;

procedure TBusChannel.IncConsumedMessageCount;
begin
  AtomicIncrement64(FBusChannelData.FConsumedMessageCount);
end;

procedure TBusChannel.IncReceivedMessageCount;
begin
  AtomicIncrement64(FBusChannelData.FReceivedMessageCount);
end;

function TBusChannel.PersistentMessageLock: Tlist<PTBusEnvelop>;
begin
  Result := FPersistentMessage.Lock;
end;

procedure TBusChannel.PersistentMessageUnlock;
begin
  FPersistentMessage.Unlock;
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

procedure TBusChannel.SettMessageInThisChannelWillBeSetAsPersistent(
  const Value: Boolean);
begin
  FBusChannelData.Lock;
  try
    FBusChannelData.FMessageInThisChannelWillBeSetAsPersistent := Value;
  finally
    FBusChannelData.Unlock;
  end;
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
  Const aMessageWillBePersistent : Boolean);
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
    C.MessageInThisChannelWillBeSetAsPersistent := aMessageWillBePersistent;

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
  //FreeAndNil(Event)....NO ! Managed by user, because could be shared between client.
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
  FProcessMessageCount := 0;
end;


function TBusClientReader.GetClientProcessMessageCount: Int64;
begin
  result := FProcessMessageCount;
end;

procedure TBusClientReader.IncProcessMessageCount;
begin
  AtomicIncrement64(FProcessMessageCount);
end;

{ TBusClientReaderList }

constructor TBusClientReaderList.Create;
begin
  FList := Tlist<TBusClientReader>.Create;
  FLock := TCriticalSection.Create;
end;

destructor TBusClientReaderList.Destroy;
begin
  FreeAndNil(FList); //User *must* delete client themself, because of share object (event)
  FreeAndNil(FLock);
  inherited;
end;

function TBusClientReaderList.Lock: TList<TBusClientReader>;
begin
  FLock.Enter;
  Result := FList;
end;

function TBusClientReaderList.ToArray: TBusClientReaderArray;
var i : integer;
begin
  FLock.Acquire;
  try
    SetLength(Result,FList.Count);
    for i := 0 to FList.Count-1 do
    begin
      Result[i] := FList[i];
    end;
  finally
    FLock.Release;
  end;
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
  Result := String(TEncoding.UTF8.GetString(Buffer));
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
  Buffer := TEncoding.UTF8.GetBytes(UnicodeString(aText));
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

{ TBus }

procedure TBus.BusShutDown;
begin
  if not(suspended) and not(Terminated) then
  begin
    Terminate;
    Sys.BusShutDown;
    Waitfor; //Terminate main bus loop.
  end;
end;

procedure TBus.ChannelDelete(aChannelName: string);
begin
  sys.ChannelDelete(aChannelName);
end;

procedure TBus.ChannelSet(aChannelName: String;
  aChannelBehaviourType: TBusChannelBehaviour;
  const aMessageWillBePersistent: Boolean);
begin
  sys.ChannelSet(aChannelName,aChannelBehaviourType,aMessageWillBePersistent);
end;

procedure TBus.ChannelSetOnBeforeDeliverMessageEvent(
  aChannelName: String; aChannelProc: TOnBusChannelBeforeDeliverMessage);
begin
  sys.ChannelSetOnBeforeDeliverMessageEvent(aChannelName,aChannelProc);
end;

constructor TBus.Create;
begin
  inherited Create(true);
  {$IFDEF DELPHI}
  NameThreadForDebugging(ClassName);
  {$ENDIF}
  Sys := TBusSystem.Create;
end;

destructor TBus.Destroy;
begin
  BusShutDown;
  FreeAndNil(Sys);
  inherited;
end;

procedure TBus.Execute;
begin
  while not(Terminated) do
    Sys.BusExecute;
end;

procedure TBus.GetChannelsConfigurationAsCSV(var aStr: TStringList);
begin
  sys.GetChannelsConfigurationAsCSV(aStr);
end;

function TBus.GetNewEvent: TEvent;
begin
  Result := sys.GetNewEvent;
end;

function TBus.GetStats: String;
begin
  result := sys.GetStats;
end;

procedure TBus.GetSubscribtersConfigurationAsCSV(var aStr: TStringList);
begin
  sys.GetSubscribtersConfigurationAsCSV(aStr);
end;

function TBus.GetWildCardEnabled: Boolean;
begin
  result :=  GetWildCardEnabled;
end;

function TBus.GetWildCardSeparator: Char;
begin
  result := GetWildCardSeparator;
end;

procedure TBus.ProcessMessages(
  Const aClientReaders: array of TBusClientReader);
begin
  sys.ProcessMessages(aClientReaders);
end;

function TBus.Send(var aMessage: TBusMessage; aTargetChannel: String;
  const aSomeAdditionalData, aResponseChannel: string;
  const IsPersistent: Boolean): Int64;
begin
  result := sys.Send(aMessage, aTargetChannel,aSomeAdditionalData,aResponseChannel,IsPersistent);
end;

procedure TBus.SetWildCardEnabled(const Value: Boolean);
begin
  Sys.WildcardEnabled := value;
end;

procedure TBus.SetWildCardSeparator(const Value: Char);
begin
  sys.WildcardSeparator := value;
end;

function TBus.Subscribe(aChannelName: String;
  CallBack: TBusMessageNotify): TBusClientReader;
begin
  result := sys.Subscribe(aChannelName,CallBack);
end;

function TBus.UnSubscribe(aClient: TBusClientReader): Boolean;
begin
  result := sys.UnSubscribe(aClient);
end;

{ TBusChannelData }

constructor TBusChannelData.Create;
begin
  Inherited;
  FLock := TCriticalSection.Create;
end;

destructor TBusChannelData.Destroy;
begin
  FreeAndNil(Flock);
  inherited;
end;

procedure TBusChannelData.Lock;
begin
  FLock.Acquire;
end;

procedure TBusChannelData.Unlock;
begin
  FLock.Release;
end;

Initialization

Bus := Nil;


end.
