///-------------------------------------------------------------------------------
/// Title      : GS.LocalMemCached
/// Short Desc : Mimic MemCached server locally.
/// Source     : https://github.com/VincentGsell
/// Aim        : MemCched helper in a task : Ressouce shared in all thread easely.
/// Notes      :
/// Did you know memcached ?  https://memcached.org/
///
/// MemCached is a client server program : You have to get the memcache server to get it work.
/// This one (GS.localmemcached) is the same stuff but inside a process.
/// you can share easely string or stream oriented data between all your thread ! It is a reasonably fast and light solution
/// to achieve an simple ressource sharing.
///
/// Design Note : We decided to inherited from TBus for performance reason : The Bus thread dispatch the message AND, in this version, process
/// the memcached duty (all in the same thread) : It could take a certain amount of time, during it, none other message is dispatched !
/// --> In a performance way, It is not a realy good idea to use this TLocalMemcached as a local memcached AND as a Bus.
/// Take care to use a pure TBus for communication, and when wou specilize a TBus,
/// such here, keep in mind to take care about the process cost by the inside processing event.
unit GS.LocalMemcached;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Bus,
  GS.Threads,
  GS.Stream,
  GS.Reference;


Const
  CST_HOLY_CHANNEL = 'MemcachedProcess';
  CST_HOLY_CHANNELOAD = 'MemcachedProcessLoad';

Type

TLocalMCOrder = (lmoUnknown, lmoGET, lmoSET);
TLocalMCtype = (lmtInknown, lmtString, lmtStream);

TOnWarmEvent = Procedure(Sender : TObject; PercentProgress : Double) of Object;
TLocalMemcached = Class(GS.Bus.TBus)
private
  FInternalDataCS : TCriticalSection;
  FInternalData : TofReference;
  FFileName: TProtectedString;
  FOnWarm: TOnWarmEvent;
  FOnWarmTrigEvery : Integer;
  FOnReady: TNotifyEvent;
  function GetItemCount: Int64;
  function GetFileName: String;
  procedure SetFileName(const Value: String);
  function GetInMemory: Boolean;
  procedure SetInMemory(const Value: Boolean);
Protected
 //This event will be trig when channel will receive a message. (See constructor)
 Procedure InternalEventOnChannelBeforeDeliverMessage(Var aMessage : TBusEnvelop); Virtual;
 Procedure InternalEventLoadData(Var aMessage : TBusEnvelop); Virtual;

 Procedure InternalOnWarmLoad(Sender : TObject; PercentProgress : Double);
 Procedure InternalOnReady;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function GetCVSSnapShot : String; Overload;
  Function GetCVSSnapShot(aFrom, aTo : Int64) : String; Overload;
  Function GetCVSSnapShotAsString(aFrom, aTo : Int64) : String;

  Procedure Open;

  Property ItemCount : Int64 read GetItemCount;
  Property FileName : String read GetFileName Write SetFileName;

  //All fellowing event are Threaded event : Use syncho or queue in other thread such as GUI.
  Property OnWarm : TOnWarmEvent read FOnWarm Write FonWarm;
  Property OnReady : TNotifyEvent read FOnReady Write FOnReady;

  //To make memcached Memory only (no write on disk) pleaser set file name to empty string BEFORE open ;)
  //Property InMemory : Boolean read GetInMemory Write SetInMemory;
End;

TLocalMemcachedClient = Class
Private
  FMaster : TLocalMemcached; //pointer.
  FClient : TBusClientReader;
  FInternalResponseChannel : String;
  FResponseString : string;
  FLastCallForType : TLocalMCtype;
  FResponseStream : TMemoryStream;
  //This object in not really itended to be share by different thread.
  //But it can theorically. Twpycal use is to build a client for each thread, or x client in each thread.
  FResponseStringProtection : TCriticalSection;

 Procedure InternalBusMemCachedProcessResponse(Sender : TBus;Var Packet : TBusEnvelop);

 Function BuildCmdSet(aKey : String; aValue : string) : TMemoryStream; Overload;
 Function BuildCmdSet(aKey : String; aValue : TmemoryStream) : TMemoryStream; Overload;
 Function BuildCmdGetString(aKey : String) : TMemoryStream; Virtual;
 Function BuildCmdGetStream(aKey : String) : TMemoryStream; Virtual;

Public
  Constructor Create(aMemcached : TLocalMemcached); Reintroduce;
  Destructor Destroy; Override;

  Procedure SetValue(Key : String; Value : String); Overload;
  Function GetValue(Key : String) : String; Overload;

  Procedure SetValue(Key : String; Value : TMemoryStream); Overload;
  Procedure GetValue(Key : String; Var aStream : TMemoryStream); Overload;
End;


implementation

{ TLocalMemcached }

constructor TLocalMemcached.create;
begin
  Inherited Create;
  FOnWarm := nil;
  FOnWarmTrigEvery := 0;
  FFileName := TProtectedString.Create('LocalMemCached.data');
  FInternalDataCS := TCriticalSection.Create;
  //Set event on our channel : This event let us a chance to modify the message.
  //Step 1 :  Client --- Send message --> Bus [Event executed IN Bus thread.];
  ChannelSetOnBeforeDeliverMessageEvent(CST_HOLY_CHANNEL, InternalEventOnChannelBeforeDeliverMessage);
  ChannelSetOnBeforeDeliverMessageEvent(CST_HOLY_CHANNELOAD, InternalEventLoadData);
  //Step 2 :  Client ---> Call Bus.ProcessMessage(...) : We deliver the new message throught InternalBusMemCachedProcessResponse
end;

destructor TLocalMemcached.Destroy;
begin
  FreeAndNil(FInternalData);
  FreeAndNil(FInternalDataCS);
  FreeAndNil(FFileName);
  inherited;
end;

function TLocalMemcached.GetCVSSnapShot: String;
var i : integer;
begin
  FInternalDataCS.Acquire;
  Result := 'number;key;type';
  try
    if Not(Assigned(FInternalData)) then
    begin
      Exit;
    end;
    for I := 0 to FInternalData.Allocation.Count-1 do
    begin
      result := result +
                sLineBreak + IntToStr(i) +
                ';' + FInternalData.Allocation.ByIndex[i].Key+';'+Cst_ContentTypeStr[Ord(FInternalData.Allocation.ByIndex[i].BufferContentType)];
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

function TLocalMemcached.GetCVSSnapShot(aFrom, aTo: Int64): String;
var i : integer;
begin
  FInternalDataCS.Acquire;
  Result := 'number;key;type';
  try
    if Not(Assigned(FInternalData)) then
    begin
      Exit;
    end;
    for I := aFrom to aTo do
    begin
      if (i < FInternalData.Allocation.Count-1) then
      begin
        result := result +
                  sLineBreak + IntToStr(i) +
                  ';' + FInternalData.Allocation.ByIndex[i].Key+';'+Cst_ContentTypeStr[Ord(FInternalData.Allocation.ByIndex[i].BufferContentType)];
      end;
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

function TLocalMemcached.GetFileName: String;
begin
  Result := FFileName.Value;
end;

function TLocalMemcached.GetInMemory: Boolean;
begin
  FInternalDataCS.Acquire;
  try
    if Not(Assigned(FInternalData)) then
    begin
      Result := False;
      Exit;
    end;
    Result := FInternalData.InMemoryOnly;
  finally
    FInternalDataCS.Release;
  end;
end;

function TLocalMemcached.GetItemCount: Int64;
begin
  FInternalDataCS.Acquire;
  try
    if Not(Assigned(FInternalData)) then
    begin
      Result := 0;
      Exit;
    end;
    Result := FInternalData.Allocation.Count;
  finally
    FInternalDataCS.Release;
  end;
end;

procedure TLocalMemcached.InternalEventLoadData(var aMessage: TBusEnvelop);
begin
  ChannelSetOnBeforeDeliverMessageEvent(CST_HOLY_CHANNELOAD,nil);
  FInternalDataCS.Acquire;
  try
    FInternalData := TofReference.Create(FFileName.Value, InternalOnWarmLoad);
    FInternalData.TypeConversionAllowedWriteTime := True; //Case by default, but specified in order to be explicit.
  Finally
    FInternalDataCS.Release;
  end;
  InternalOnReady;
end;

procedure TLocalMemcached.InternalEventOnChannelBeforeDeliverMessage(
  var aMessage: TBusEnvelop);
  var lMsg : TMemoryStream;
      lcmd : TLocalMCOrder;
      lkey : string;
      ltype : TLocalMCtype;
      lstream : TMemoryStream;
      ltemp : TofAllocationTableStruc;
begin
  lmsg := aMessage.ContentMessage.AsStream;
  SEtLength(aMessage.ContentMessage.Buffer,0);
  lmsg.Position := 0;
  lcmd := TLocalMCOrder(ReadByte(lmsg));
  ltype := TLocalMCtype(ReadByte(lMsg));
  lkey := ReadString(lMsg);

  FInternalDataCS.Acquire;
  try
    case lcmd of
      lmoUnknown: ;
      lmoGET:
      begin
        if FInternalData.Allocation.ContainsKey(lkey) then
        begin
          //Value exists.
          ltemp := FInternalData.Allocation.ByValue[lkey];

          case ltemp.BufferContentType of
            ctUnknow: ;
            ctString:
            begin
              case ltype of
                lmtInknown: ;
                lmtString:
                begin
                  aMessage.ContentMessage.FromString(FInternalData.GetEntryAsString(lkey));
                end;
                lmtStream:
                begin
                  //Convert : Send the string in a stream.
                  lstream := TMemoryStream.Create;
                  try
                    WriteString(lStream,FInternalData.GetEntryAsString(lkey));
                    lstream.Position := 0;
                    aMessage.ContentMessage.FromStream(lstream);
                  finally
                    FreeAndNil(lstream);
                  end;
                end
              end;
            end;
            ctStream:
            begin
              case ltype of
                lmtInknown: ;
                lmtString:
                begin
                  //Convert : Stream and string are not compatible : Send string with explicit content !
                  lstream := FInternalData.GetEntryAsStream(lkey);
                  try
                    aMessage.ContentMessage.FromString('DataStreamLength='+IntToStr(lstream.Size));
                  finally
                    FreeAndNil(lstream)
                  end;
                end;
                lmtStream:
                begin
                  lstream := FInternalData.GetEntryAsStream(lkey);
                  try
                    lStream.Position := 0;
                    aMessage.ContentMessage.FromStream(lStream);
                  finally
                    FreeAndNil(lStream);
                  end;
                end
              end;
            end;
            ctInteger: ;
            ctUINT32: ;
            ctDouble: ;
          end;
        end
        else
        begin
          //Value not exists : In a get case, we create empty shell.
          case ltype of
            lmtInknown: ;
            lmtString:
            begin
              aMessage.ContentMessage.FromString(EmptyStr);
            end;
            lmtStream:
            begin
              lstream := TMemoryStream.Create;
              try
                aMessage.ContentMessage.FromStream(lStream); //Empty : normal.
              finally
                FreeAndNil(lStream);
              end;
            end
          end;
        end;
      end;
      lmoSET:
      begin;
        if FInternalData.Allocation.ContainsKey(lkey) then
        begin
          //Value exists.
          ltemp := FInternalData.Allocation.ByValue[lkey];
          case ltemp.BufferContentType of
            ctUnknow: ;
            ctString:
            begin
              case ltype of
                lmtInknown: ;
                lmtString:
                begin
                  FInternalData.WriteEntryString(lkey,ReadString(lMsg));
                end;
                lmtStream:
                begin
                  //Conversion
                  lstream := TMemoryStream.Create;
                  try
                    WriteStream(lstream,lMsg);
                    lstream.Position := 0;
                    FInternalData.WriteEntryStream(lkey,lstream);
                  finally
                    FreeAndNil(lstream);
                  end;
                end
              end;
            end;
            ctStream:
            begin
              case ltype of
                lmtInknown: ;
                lmtString:
                begin
                  //Conversion
                  FInternalData.WriteEntryString(lkey,ReadString(lMsg));
                end;
                lmtStream:
                begin
                  lstream := TMemoryStream.Create;
                  try
                    WriteStream(lstream,lMsg);
                    lstream.Position := 0;
                    FInternalData.WriteEntryStream(lkey,lStream);
                  finally
                    FreeAndNil(lStream);
                  end;
                end;
              end;
            end;
            ctInteger: ;
            ctUINT32: ;
            ctDouble: ;
          end;
        end
        else
        begin
          //Value not exists : Create.
          case ltype of
            lmtInknown: ;
            lmtString:
            begin
              FInternalData.WriteEntryString(lkey,ReadString(lMsg));
            end;
            lmtStream:
            begin
              lstream := TMemoryStream.Create;
              try
                WriteStream(lstream,lMsg);
                lstream.Position := 0;
                FInternalData.WriteEntryStream(lkey,lstream)
              finally
                FreeAndNil(lstream);
              end;;
            end;
          end;
        end;
      end;
    end;
  finally
    FreeAndNil(lMsg);
    FInternalDataCS.Release;
    //Now, we send this content on the asked Response channel.
    Send(aMessage.ContentMessage,aMessage.ResponseChannel);
  end;
end;

procedure TLocalMemcached.InternalOnReady;
begin
  if Assigned(FOnReady) then
  begin
    FOnReady(Self);
  end;
end;

procedure TLocalMemcached.InternalOnWarmLoad(Sender: TObject;
  PercentProgress: Double);
begin
  if Assigned(FOnWarm) then
  begin
    if (FOnWarmTrigEvery = 0) or (PercentProgress = 100.00) then
    begin
      FOnWarm(Self,PercentProgress);
      FOnWarmTrigEvery := 10000;
    end;
    Dec(FOnWarmTrigEvery);
  end;
end;

procedure TLocalMemcached.Open;
var am : TBusMessage;
begin
  FInternalDataCS.Acquire;
  try
    if not(Assigned(FInternalData)) then
    begin
      Start;
      Send(am,CST_HOLY_CHANNELOAD);
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

procedure TLocalMemcached.SetFileName(const Value: String);
begin
  if not(Assigned(FInternalData)) then
  begin
    FFileName.Value := Value;
  end
  else
  begin
    raise Exception.Create(ClassName + ' FileName cannot be set once "Open" has been called.');
  end;
end;

procedure TLocalMemcached.SetInMemory(const Value: Boolean);
begin
end;

function TLocalMemcached.GetCVSSnapShotAsString(aFrom, aTo: Int64): String;
var i : integer;
    m : TMemoryStream;
begin
  FInternalDataCS.Acquire;
  Result := 'number;key;type;Value';
  try
    if Not(Assigned(FInternalData)) then
    begin
      Exit;
    end;
    for I := aFrom to aTo do
    begin
      if (i < FInternalData.Allocation.Count-1) then
      begin

        case FInternalData.Allocation.ByIndex[i].BufferContentType of
          ctStream :
          begin
            m := FInternalData.GetEntryAsStream(FInternalData.Allocation.ByIndex[i].Key);
            try
              result := result +
                        sLineBreak + IntToStr(i) +
                        ';' + FInternalData.Allocation.ByIndex[i].Key+
                        ';'+Cst_ContentTypeStr[Ord(FInternalData.Allocation.ByIndex[i].BufferContentType)]+
                        ';Stream size : '+IntTostr(m.Size);
            finally
              FreeAndNil(m);
            end;
          end;
          ctString :
          begin
            result := result +
                      sLineBreak + IntToStr(i) +
                      ';' + FInternalData.Allocation.ByIndex[i].Key+
                      ';'+Cst_ContentTypeStr[Ord(FInternalData.Allocation.ByIndex[i].BufferContentType)]+
                      ';'+FInternalData.GetEntryAsString(FInternalData.Allocation.ByIndex[i].Key);
          end;
          else
          begin
            raise Exception.Create(ClassName + '.GetCVSSnapShotAsString : Type not supported.');
          end;
        end;
      end;
    end;
  finally
    FInternalDataCS.Release;
  end;
end;

{ TLocalMemcachedClient }

function TLocalMemcachedClient.BuildCmdGetStream(aKey: String): TMemoryStream;
begin
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(lmoGET));
  WriteByte(Result,Byte(lmtStream));
  WriteString(Result,aKey);
  Result.Position :=0;
end;

function TLocalMemcachedClient.BuildCmdGetString(aKey: String): TMemoryStream;
begin
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(lmoGET));
  WriteByte(Result,Byte(lmtString));
  WriteString(Result,aKey);
  Result.Position :=0;
end;

function TLocalMemcachedClient.BuildCmdSet(aKey: String;
  aValue: TmemoryStream): TMemoryStream;
begin
  Assert(Assigned(aValue));
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(lmoSET));
  WriteByte(Result,Byte(lmtStream));
  WriteString(Result,aKey);
  aValue.Position := 0;
  WriteStream(Result,aValue);
  Result.Position :=0;
end;

function TLocalMemcachedClient.BuildCmdSet(aKey, aValue: string): TMemoryStream;
begin
  Assert(length(aKey)>0);
  Result := TMemoryStream.Create;
  WriteByte(Result,Byte(lmoSET));
  WriteByte(Result,Byte(lmtString));
  WriteString(Result,aKey);
  WriteString(Result,aValue);
  Result.Position :=0;
end;

constructor TLocalMemcachedClient.Create(aMemcached : TLocalMemcached);
begin
  Inherited Create;
  Assert(Assigned(aMemcached));
  FMaster := aMemCached;
  FResponseStringProtection := TCriticalSection.Create;
  FResponseStream := TMemoryStream.Create;
  FInternalResponseChannel := CST_HOLY_CHANNEL+IntToStr(UINT64(Self));

  FClient := aMemcached.Subscribe( FInternalResponseChannel,
                                   InternalBusMemCachedProcessResponse);
  FClient.Event := aMemcached.GetNewEvent;
end;

destructor TLocalMemcachedClient.Destroy;
begin
  FreeAndNil(FResponseStringProtection);
  FreeAndNil(FResponseStream);
  inherited;
end;

function TLocalMemcachedClient.GetValue(Key: String): String;
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  FResponseStringProtection.Acquire;
  try
    FLastCallForType := TLocalMCtype.lmtString;
    lss := BuildCmdGetString(Key);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_HOLY_CHANNEL,EmptyStr,FInternalResponseChannel);
    FClient.Event.WaitFor(INFINITE);
    FMaster.ProcessMessages(FClient);
    Result := FResponseString;
  finally
    FResponseStringProtection.Leave;
  end;
end;


Procedure TLocalMemcachedClient.GetValue(Key: String;
  var aStream: TMemoryStream);
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  Assert(Assigned(aStream));

  FResponseStringProtection.Acquire;
  try
    FLastCallForType := TLocalMCtype.lmtStream;
    lss := BuildCmdGetStream(Key);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_HOLY_CHANNEL,EmptyStr,FInternalResponseChannel);
    FClient.Event.WaitFor(INFINITE);
    FMaster.ProcessMessages(FClient);
    aStream.LoadFromStream(FResponseStream);
    aStream.Position := 0;
  finally
    FResponseStringProtection.Leave;
  end;
end;

procedure TLocalMemcachedClient.InternalBusMemCachedProcessResponse(Sender: TBus;
  var Packet: TBusEnvelop);
var lStream : TMemoryStream;
begin
  //Here, FresponseString is protected.
  if FLastCallForType = lmtString then
    FResponseString := Packet.ContentMessage.AsString
  else
  if FLastCallForType = lmtStream then
  begin
    lStream := Packet.ContentMessage.AsStream; //Copy
    try
      lStream.Position := 0;
      FResponseStream.Clear;
      FResponseStream.LoadFromStream(lStream);
      FResponseStream.Position := 0;
    finally
      FreeAndNil(lStream);
    end;
  end
  else
  begin
    raise Exception.Create('Error Message');
  end;
end;

Procedure TLocalMemcachedClient.SetValue(Key, Value: String);
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  FResponseStringProtection.Acquire;
  try
    FLastCallForType := TLocalMCtype.lmtString;
    lss := BuildCmdSet(Key,Value);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_HOLY_CHANNEL,EmptyStr,FInternalResponseChannel);
    FClient.Event.WaitFor(INFINITE);
    FMaster.ProcessMessages(FClient);
  finally
    FResponseStringProtection.Leave;
  end;
end;

Procedure TLocalMemcachedClient.SetValue(Key: String; Value: TMemoryStream);
var aM : TBusMessage;
    lss : TMemoryStream;
begin
  Assert(assigned(Value));
  FResponseStringProtection.Acquire;
  try
    FLastCallForType := TLocalMCtype.lmtStream;
    Value.Position := 0;
    lss := BuildCmdSet(Key,Value);
    try
      am.FromStream(lss);
    finally
      FreeAndNil(lss);
    end;
    FMaster.Send(aM,CST_HOLY_CHANNEL,EmptyStr,FInternalResponseChannel);
    FClient.Event.WaitFor(INFINITE);
    FMaster.ProcessMessages(FClient);
  finally
    FResponseStringProtection.Leave;
  end;
end;


end.
