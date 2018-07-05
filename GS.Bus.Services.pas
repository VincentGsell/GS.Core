unit GS.Bus.Services;
///-------------------------------------------------------------------------------
/// Title      : GS.Bus.Services
/// Short Desc : Service definition : Bus supervising Bus, which could be task process.
/// Source     : https://github.com/VincentGsell
/// Aim        : ServiceManager will be used as base for GridServer.
///              Basically, Services is a thread worker / survey implementation.
/// Notes      : Written to replace GS.Tasks, which is not usefull in GridServer
///              current implementation
///              See GS.LocalMemCached for "from ground" thread task dedicated bus.
///              Services is just a more abstract implementation.
///              Never forget that  a bus is firstly a thread,
///              An high commmunication skill thread. And when you work,
///              as a thread, more important thing in not what you done,
///              it is the way of you communicate between thread. ;)

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
{$ENDIF}
  GS.Bus,
  GS.Threads,
  GS.Stream,
  GS.CPUUsage;


const
  CST_CUSTOMSERVICEAWAITINGTIMEOUT = 250; //ms
  CST_THREAD_SERVICE_NOT_RIGHT = 'Service has not a TThreadService class Thread or Thread not assigned.';
  cst_ThreadServiceStatus_string : Array[0..4] of String = ('None', 'Exception', 'Waiting for data', 'Processing', 'Finished');


  //Thread's Sending channel. (Service receive)
  cThreadHeardBeatChannel        = 'HBC';
  cThreadProgressChannel         = 'PRG';
  cThreadChangeStatus            = 'CHS';
  cThreadResultChannel           = 'RES';
  cThreadLogChannel              = 'LOG';
  cThreadReceiveChannel          = 'RCV';
  cThreadProcessDuration         = 'PDU';

Type
TCustomServiceManager = Class;   //Management level. container. Bus.
TCustomService = Class;    //Single Service level : Container.
TServiceClass = class of TCustomService;
TThreadServiceStatus = (None, ExceptionInvoke, AWaitingForData, Processing, Terminated);

TThreadServiceStat = Record
  StartDateTime : TDateTime;
  FinishDateTime : TDateTime;
  DurationInMs : NativeUInt;
  FinishedNormaly : Boolean;
  FinishedAnormalyInfo : String;
  InternalStatus : TThreadServiceStatus;
  SystemEventCount : NativeUInt;
  LogProcessed : NativeUInt;

  Function AsString : String;
  Function Header : String;
End;
pTThreadServiceStat = ^TThreadServiceStat;


//Message def.
TThreadServiceMessage_changeStatus = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  NewStatus : TThreadServiceStatus;
  AdditionalData : String;

  Function GetAsStream : TMemoryStream; //Cannot use delphi serialization : Multiplatform set.
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_Progress = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  Progress : Double;
  AdditionalData : String;

  Function GetAsStream : TMemoryStream; //Idem as previous...
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_HeartBeat = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;

  Function GetAsStream : TMemoryStream; //And idem...
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_Log = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  LogText : string;

  Function GetAsStream : TMemoryStream; //...
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_Result = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  PartialResult : Boolean;
  ResultAsStream : TMemoryStream;

  Function GetAsStream : TMemoryStream;
  Procedure LoadFromStream(aStream : TMemoryStream);
end;

TThreadServiceMessage_ProcessDuration = Packed Record
  ServiceContext : UInt64;
  ThreadID : UInt64;
  Duration : UInt64;

  Function GetAsStream : TMemoryStream;
  Procedure LoadFromStream(aStream : TMemoryStream);
end;



TCustomServiceThread = Class;
TServiceTask = Class
  MasterThread : TCustomServiceThread;
  Procedure Initialize; Virtual; //Use this as a constructor, in this method, you had acces to MasterThread. Not in the constructor.
  Procedure Execute; Virtual; Abstract;
  Procedure Finalize; Virtual;
End;

//Internal resident Thread of a service.
//This thread take task as an entry and execute it, as an engine.
//This thread's aim is to supervise "Run" the task, while TServiceManager supervise the run.
TServiceStatus = (NotActive, Active);
TCustomServiceThread = Class(TThread)
Protected
  FStatus : TProtectedValue<TServiceStatus>;

  //Thread signal runner.
  FGo : TEvent;
  //Communication Bus Instance.
  FBus : TBus; //Pointer.
  //Master service
  FService : TCustomService; //Pointer.
  //Current Task
  FTask : TServiceTask;

  Procedure DoProcessTask;
  Procedure DoChangeStatus(aNewStatus : TThreadServiceStatus; Const AdditionalInfo : String = ''); Virtual;
  Procedure DoReportDuration(aDurationInMs : UInt64); Virtual;

Public
  Constructor Create; reintroduce;
  Destructor Destroy; Override;

  Procedure Go;
  Procedure Execute; Override;

  //Should not be called. Used by manager to setup internal tools.
  Procedure SetUpThread(Const aService : TCustomService; aBus : GS.Bus.TBus);

  Procedure SubmitTask(aTask : TServiceTask);

  //This method is available to be used inside the "run" Procedure, in order to
  //notify information to master ServiceManager.
  Procedure DoHeartBeat; Virtual;
  Procedure DoProgress(aPercent : Double); Virtual;
  Procedure DoResult(aStream : TMemoryStream; Const IsAFinalResult : Boolean = False); Virtual;
  Procedure DoLog(aLogText : String); Virtual;

  Property Bus : TBus read FBus;
  Property Terminated; //Prop. Visibility.
End;

TCustomService = Class
private
protected
  FWaitForCompletion : TEvent;
  FThread :  TCustomServiceThread;
  FBus : Gs.Bus.TBus;
  FInternalStats : TProtectedValue<pTThreadServiceStat>;
  FTask : TServiceTask;
  function GetServiceStats: TThreadServiceStat;
  Function GetServiceName : String; Virtual;
  function GetServiceStatus: TServiceStatus; Virtual;

  function GetTask: TServiceTask;
  procedure SetTask(const Value: TServiceTask);

public
  constructor Create; Virtual;
  Destructor Destroy; Override;
  Property ServiceName : String read GetServiceName;

  Procedure StartService;
  Procedure StopService;
  Procedure WaitFor;

  //Should Not called : Should be used only by underlying thread.
  //When user call a a WaitFor from outside, thread must call SetTaskFinished for
  //liberate the user call.
  Procedure SetTaskFinished;

  Property ServiceStatus : TServiceStatus read GetServiceStatus;
  Property ServiceStats :  TThreadServiceStat read GetServiceStats;
  Property Task : TServiceTask read GetTask Write SetTask;
End;

TService = Class(TCustomService)
End;

TServiceList = TObjectList<TCustomService>;
TCustomServiceManager = Class(GS.Bus.TBus)
private
  function GetServiceCount: Uint32;
protected
  FServices : TProtectedObject<TObjectList<TCustomService>>;

  function GetServices(Index: UInt32): TCustomService; Virtual;
  Procedure EnsureAllThreadStoped; Virtual;

  //Bus processing
  Procedure OnServiceChangeStatus(Var aMessage : TBusEnvelop);
  Procedure OnServiceLog(Var aMessage : TBusEnvelop);
  Procedure OnServiceHearBeat(Var aMessage : TBusEnvelop);
  Procedure OnServiceResult(Var aMessage : TBusEnvelop);
  Procedure OnServiceProgress(Var aMessage : TBusEnvelop);
  Procedure OnServiceDuration(Var aMessage : TBusEnvelop);

  //Tools
  Function ServiceFromContext(aContext : UInt64) : TCustomService;
Public
  Constructor Create; Override;
  Destructor Destroy; Override;

  Procedure StartAllServices; Virtual;
  Procedure StopAllServices; Virtual;
  Procedure StopService(aService : TCustomService); Virtual;

  Procedure RegisterService(aService : TCustomService); Virtual;
  procedure UnregisterService(aService : TCustomService); Virtual;
  Procedure UnregisterAllServices(Const aFreeTask : Boolean = False); Virtual;

  Function ServiceIndex(aService : TCustomService) : Integer; Virtual;

  Function StatsTask : String;
  Function StatsServices : String;

  Function ServicesLock : TServiceList; //Call with try..
  Procedure ServicesUnlock;             //...finally.

  Property Services[Index : UInt32] : TCustomService read getServices;
  Property ServiceCount : Uint32 read GetServiceCount;
End;

TServiceManager = Class(TCustomServiceManager)
End;



implementation


{ TThreadService }

Function NullThreadStat : pTThreadServiceStat;
var la : pTThreadServiceStat;
begin
  new(la);
  la.StartDateTime :=  0;
  la.FinishDateTime :=  0;
  la.DurationInMs := 0;
  la.FinishedNormaly := True; //We are proudly Optimistic.
  la.FinishedAnormalyInfo := EmptyStr;
  la.InternalStatus := TThreadServiceStatus.None;
  la.SystemEventCount := 0;
  la.LogProcessed := 0;
  Result := la;
end;


procedure TCustomServiceThread.DoChangeStatus( aNewStatus: TThreadServiceStatus;
                                         Const AdditionalInfo : String = '');
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_changeStatus;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.NewStatus := aNewStatus;
    lFormatedMessage.AdditionalData := AdditionalInfo;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadChangeStatus,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoResult(aStream: TMemoryStream; Const IsAFinalResult : Boolean = False);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_Result;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.PartialResult := Not(IsAFinalResult);
    lFormatedMessage.ResultAsStream := aStream;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadResultChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoHeartBeat;
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_HeartBeat;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadHeardBeatChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoLog(aLogText: String);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_Log;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.LogText := aLogText;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadLogChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.DoProcessTask;
begin
  Assert(Assigned(FService));
  if Assigned(FTask) then
  begin
    FTask.Initialize;
    FTask.Execute;
    FTask.Finalize;
  end;
end;

procedure TCustomServiceThread.DoProgress(aPercent: Double);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_Progress;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.Progress := aPercent;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadProgressChannel,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;


procedure TCustomServiceThread.DoReportDuration(aDurationInMs: UInt64);
var lMes : TBusMessage;
    lMem : TMemoryStream;
    lFormatedMessage : TThreadServiceMessage_ProcessDuration;
begin
  if Assigned(FBus) then
  Begin
    lFormatedMessage.ServiceContext := UInt64(FService);
    lFormatedMessage.ThreadID := ThreadID;
    lFormatedMessage.Duration := aDurationInMs;
    lMem := lFormatedMessage.GetAsStream;
    try
      lMes.FromStream(lMem);
      FBus.Send(lMes,cThreadProcessDuration,IntToStr(ThreadID));
    finally
      FreeAndNil(lMem);
    end;
  End;
end;

procedure TCustomServiceThread.SetUpThread(Const aService : TCustomService; aBus : GS.Bus.TBus);
begin
  Assert(Assigned(aService));
  Assert(Assigned(aBus));
  FService := AService;
  FBus := aBus;
  if Suspended then
    Start;
end;


procedure TCustomServiceThread.SubmitTask(aTask: TServiceTask);
begin
  if FStatus.Value = NotActive then
  begin
    FTask := aTask;
    FTask.MasterThread := Self;
  end;
end;

{ TCustomServiceManager }

constructor TCustomServiceManager.Create;
begin
  inherited Create;
  FServices := TProtectedObject<TObjectList<TCustomService>>.Create(TObjectList<TCustomService>.Create);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadChangeStatus,OnServiceChangeStatus);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadResultChannel,OnServiceResult);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadProgressChannel,OnServiceProgress);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadLogChannel,OnServiceLog);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadHeardBeatChannel,OnServiceHearBeat);
  ChannelSetOnBeforeDeliverMessageEvent(cThreadProcessDuration,OnServiceDuration);
end;

destructor TCustomServiceManager.Destroy;
begin
  BusShutDown;
  EnsureAllThreadStoped;
  UnregisterAllServices;
  FreeAndNil(FServices);
  inherited;
end;

procedure TCustomServiceManager.EnsureAllThreadStoped;
var a : TCustomService;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    for a in lServices do
    begin
      a.StopService;
    end;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.GetServiceCount: Uint32;
var lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    result := lServices.Count;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.getServices(Index: UInt32): TCustomService;
var lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    result := lServices[Index];
  finally
    FServices.Unlock;
  end;
end;

procedure TCustomServiceManager.OnServiceChangeStatus(
  var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_changeStatus;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.InternalStatus := lFormatedMessage.NewStatus;
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    case lFormatedMessage.NewStatus of
      None: ;
      ExceptionInvoke :
      begin
        lcc.FInternalStats.Value^.FinishedNormaly := False;
        lcc.FInternalStats.Value^.FinishedAnormalyInfo := lFormatedMessage.AdditionalData;
      end;
      AWaitingForData:
      begin
      end;
      Processing:
      begin
        lcc.FInternalStats.Value^.StartDateTime := Now;
      end;
      TThreadServiceStatus.Terminated:
      begin
        lcc.FInternalStats.Value^.FinishDateTime := Now;
      end;
    end;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceDuration(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_ProcessDuration;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    lcc.FInternalStats.Value^.DurationInMs := lFormatedMessage.Duration;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceHearBeat(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_HeartBeat;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceLog(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_Log;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    lcc.FInternalStats.Value^.LogProcessed := lcc.FInternalStats.Value^.LogProcessed+1;
    //Event ?
  end;

end;

procedure TCustomServiceManager.OnServiceProgress(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_Progress;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    //Event ?
  end;
end;

procedure TCustomServiceManager.OnServiceResult(var aMessage: TBusEnvelop);
var lcc : TCustomService;
    lFormatedMessage : TThreadServiceMessage_Result;
    lMem : TMemoryStream;
begin
  lMem := aMessage.ContentMessage.AsStream;
  try
    lFormatedMessage.LoadFromStream(lMem);
  finally
    FreeAndNil(lMem);
  end;
  lcc := ServiceFromContext(lFormatedMessage.ServiceContext);

  if assigned(lcc) then
  begin
    lcc.FInternalStats.Value^.SystemEventCount := lcc.FInternalStats.Value^.SystemEventCount+1;
    //Event ?
  end;
end;

procedure TCustomServiceManager.RegisterService(aService: TCustomService);
var lServices : TServiceList;
begin
  Assert(Assigned(aService));
  lServices := FServices.Lock;
  try
  if lServices.IndexOf(aService) = -1 then
  begin
    lServices.Add(aService);
    aService.FBus := TBus(Self); //Only bus part is visible. But it is a TCustomServiceManager.
  end;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.ServiceFromContext(
  aContext: UInt64): TCustomService;
var lc :  TCustomService;
    lServices : TServiceList;

begin
  Result := nil;
  lServices := FServices.Lock;
  try
    for lc in lServices do
    begin
      if Uint64(lc) = aContext  then
      begin
        Result := lc;
        Exit;
      end;
    end;

    if Not(Assigned(Result)) then
    begin
      //Thread could respond after service removing.
      //raise Exception.Create(ClassName+'.ServiceFromContext : Not found');
    end;
  finally
    Fservices.Unlock;
  end;
end;

function TCustomServiceManager.ServiceIndex(aService: TCustomService): Integer;
var lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    Result := lServices.IndexOf(aService);
  finally
    FServices.Lock;
  end;
end;

function TCustomServiceManager.ServicesLock: TServiceList;
begin
  Result := FServices.Lock;
end;

procedure TCustomServiceManager.ServicesUnlock;
begin
  FServices.Unlock;
end;

procedure TCustomServiceManager.StartAllServices;
var a : TCustomService;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    for a in lServices do
    begin
      a.StartService;
    end;
  finally
    FServices.Unlock;
  end;
end;

function TCustomServiceManager.StatsServices: String;
var ls : TStringList;
    lService : TCustomService;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    ls := TStringList.Create;
    try
      if lServices.Count>0 then
      begin
        ls.Add('ServiceName,'+lServices[0].ServiceStats.Header);
        for lService in lServices do
        begin
          ls.Add(lService.ServiceName+','+lService.ServiceStats.AsString);
        end;
      end;
      Result := ls.Text;
    finally
      FreeAndNil(ls);
    end;
  finally
    FServices.UnLock;
  end;
end;

function TCustomServiceManager.StatsTask: String;
var ls : TStringList;
    lService : TCustomService;
    lServices : TServiceList;
begin
  lservices := FServices.Lock;
  try
    ls := TStringList.Create;
    try
      if lServices.Count>0 then
      begin
        for lService in lServices do
        begin
          if Assigned(lService.Task) then
          begin
            ls.Add(lService.ServiceName+' - '+lService.Task.ClassName);
          end
          else
          begin
            ls.Add(lService.ServiceName+' - No task.' );
          end;
        end;
      end;
      Result := ls.Text;
    finally
      FreeAndNil(ls);
    end;
  finally
    FServices.Unlock;
  end;
end;

procedure TCustomServiceManager.StopAllServices;
begin
  EnsureAllThreadStoped;
end;

procedure TCustomServiceManager.StopService(aService: TCustomService);
begin
  if Assigned(aService) then
  begin
    aService.StopService;
  end;
end;

procedure TCustomServiceManager.UnregisterAllServices(Const aFreeTask : Boolean = False);
var la : TCustomService;
    lServices : TServiceList;
begin
  lServices := FServices.Lock;
  try
    for la in lServices do
    begin
      if Assigned(la.Task) And aFreeTask then
      begin
        la.Task.Free;
      end;
    end;
  finally
    FServices.Unlock;
  end;
end;

procedure TCustomServiceManager.UnregisterService(aService: TCustomService);
var lServices : TServiceList;
begin
  if Assigned(aService) then
  begin
    StopService(aService);
    lServices := FServices.Lock;
    try
      lServices.Remove(aService); //Service freed.
    Finally
      FServices.Unlock;
    end;
  end;
end;


{ TCustomService }

constructor TCustomService.Create;
begin
  Inherited Create;
  FInternalStats := TProtectedValue<pTThreadServiceStat>.Create(NullThreadStat);
  FThread := TCustomServiceThread.Create;
  FWaitForCompletion :=  TEvent.Create(nil,False,False,EmptyStr);
end;

destructor TCustomService.Destroy;
begin
  if Not(FThread.Terminated) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;
  FreeAndNil(FThread);
  Dispose(FInternalStats.Value);
  FreeAndNil(FInternalStats);
  FreeAndNil(FWaitForCompletion);
  inherited;
end;

function TCustomService.GetServiceName: String;
begin
  if Assigned(FTask) then
  begin
    Result :=  FTask.ClassName;
  end
  else
  if Assigned(FThread) then
  begin
    Result := '"'+FThread.ClassName+'" based Service';
  end
  else
  begin
    Result := 'Noname Service';
  end;
end;

function TCustomService.GetServiceStats: TThreadServiceStat;
begin
  Result := FInternalStats.Value^;
end;

function TCustomService.GetServiceStatus: TServiceStatus;
begin
  Result := TServiceStatus.NotActive;
  if Not(FThread.Suspended) and Not(FThread.Finished) then
    Result := TServiceStatus.Active;
end;


function TCustomService.GetTask: TServiceTask;
begin
//  Result := nil;
//  if FThread.FStatus.Value = NotActive then
//  begin
    Result := FTask;
//  end
//  else
//  begin
//    raise Exception.Create(ClassName+'.GetTask : Cannot access Task during Task processing.');
//  end;
end;

procedure TCustomService.SetTask(const Value: TServiceTask);
begin
  if FThread.FStatus.Value = NotActive then
  begin
    FTask := Value;
    FThread.SubmitTask(Ftask);
  end
  else
  begin
    raise Exception.Create(ClassName+'.SetTask : Cannot apply a Task during Task processing.');
  end;
end;

procedure TCustomService.SetTaskFinished;
begin
  FWaitForCompletion.SetEvent;
end;

procedure TCustomService.StartService;
begin
  Fthread.SetUpThread(Self, FBus); //Will thread.Start.
  FThread.Go;                      //Will Run the Task process.
end;

procedure TCustomService.StopService;
begin
  if Not FThread.Terminated then
  begin
    if Not(FThread.Started) then
      FThread.Go;
    FThread.Terminate;
    FThread.WaitFor;
  end;
end;

procedure TCustomService.WaitFor;
begin
  if TThread.CurrentThread.ThreadID <> FThread.ThreadID then
    FWaitForCompletion.WaitFor(INFINITE);
end;

{ TThreadServiceStat }

function TThreadServiceStat.AsString: String;
var ls : TStringList;
begin
  ls := TStringList.Create;
  try
    ls.Delimiter := ',';
    ls.Add(IntToStr(DurationInMs));
    ls.Add(DateTimeToStr(StartDateTime));
    ls.Add(DateTimeToStr(FinishDateTime));
    ls.Add(IntToStr(Byte(FinishedNormaly)));
    ls.Add(FinishedAnormalyInfo);
    ls.Add(cst_ThreadServiceStatus_string[Byte(InternalStatus)]);
    ls.Add(IntToStr(SystemEventCount));
    ls.Add(IntToStr(LogProcessed));
    Result := ls.DelimitedText;
  finally
    FreeAndNil(ls);
  end;
end;

function TThreadServiceStat.Header: String;
var ls : TStringList;
begin
  ls := TStringList.Create;
  try
    ls.Delimiter := ',';
    ls.Add('DurationInMs');
    ls.Add('StartDateTime');
    ls.Add('FinishDateTime');
    ls.Add('FinishedNormaly');
    ls.Add('FinishedAnormalyInfo');
    ls.Add('Status');
    ls.Add('SystemEventCount');
    ls.Add('LogProcessed');
    Result := ls.DelimitedText;
  finally
    FreeAndNil(ls);
  end;
end;

{ TThreadServiceMessage_changeStatus }

function TThreadServiceMessage_changeStatus.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteByte(Result, Byte(NewStatus));
  WriteString(Result, AdditionalData);
end;

procedure TThreadServiceMessage_changeStatus.LoadFromStream(
  aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  NewStatus := TThreadServiceStatus(ReadByte(aStream));
  AdditionalData := ReadString(aStream);
end;

{ TThreadServiceMessage_Progress }

function TThreadServiceMessage_Progress.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteDouble(Result, Progress);
  WriteString(Result, AdditionalData);
end;

procedure TThreadServiceMessage_Progress.LoadFromStream(aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  Progress := ReadDouble(aStream);
  AdditionalData := ReadString(aStream);
end;

{ TThreadServiceMessage_HeartBeat }

function TThreadServiceMessage_HeartBeat.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
end;

procedure TThreadServiceMessage_HeartBeat.LoadFromStream(
  aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
end;

{ TThreadServiceMessage_Log }

function TThreadServiceMessage_Log.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteString(Result, LogText);
end;

procedure TThreadServiceMessage_Log.LoadFromStream(aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  LogText := ReadString(aStream)
end;

{ TThreadServiceMessage_Result }

function TThreadServiceMessage_Result.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteBoolean(Result, PartialResult);
  ResultAsStream.Position := 0;
  WriteStream(Result, ResultAsStream);
end;

procedure TThreadServiceMessage_Result.LoadFromStream(aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  PartialResult := ReadBoolean(aStream);
  ResultAsStream := TMemoryStream.Create;
  ReadStream(aStream,ResultAsStream);
end;

{ TCustomServiceThread }

constructor TCustomServiceThread.Create;
begin
  Inherited Create(True); //run immediately.
  FreeOnTerminate := false;
  FStatus := TProtectedValue<TServiceStatus>.Create(NotActive);
  FGo := TEvent.Create(nil,False,False,EmptyStr);
end;

destructor TCustomServiceThread.Destroy;
begin
  Terminate;
  if Not(Terminated) then
  begin
    //FGo.SetEvent;
    WaitFor;
  end;
  FreeAndNil(FGo);
  FreeAndNil(FStatus);
  inherited;
end;

procedure TCustomServiceThread.Execute;
var lt : UInt64;
begin
  while not(terminated) do
  begin
    DoChangeStatus(TThreadServiceStatus.AWaitingForData);
    if FStatus.Value<>NotActive then
      FStatus.Value := NotActive;
    case FGo.WaitFor(CST_CUSTOMSERVICEAWAITINGTIMEOUT) of
      wrSignaled :
      begin
        try
          if Terminated then
            Exit;
          FStatus.Value := Active;
          DoChangeStatus(TThreadServiceStatus.Processing);
          FTask.MasterThread := self;
          lt :=  gsGetTickCount;
          DoProcessTask;
          if Terminated then
            Exit;
          lt := gsGetTickCount - lt;
          DoReportDuration(lt);
          DoChangeStatus(TThreadServiceStatus.Terminated);
          FService.SetTaskFinished;
        Except
          On E : Exception do
          begin
            DoChangeStatus(TThreadServiceStatus.ExceptionInvoke, E.Message);
          end;
        end;
      end;
      wrTimeout:
      begin
        if Terminated then
          Exit;
        DoHeartBeat; //TODO : Heard beath only on a givent HeatBeat frequency time (Usualy, 1 sec.)
      end;
      wrAbandoned, wrError{$IFDEF DELPHI}, wrIOCompletion {$ENDIF} :
      begin
        if Terminated then
          Break;
        DoTerminate;
      end;
    end;
  end;
  //DOterminated will be called.
end;

procedure TCustomServiceThread.Go;
begin
  FGo.SetEvent;
end;

{ TThreadServiceMessage_ProcessDuration }

function TThreadServiceMessage_ProcessDuration.GetAsStream: TMemoryStream;
begin
  Result :=  TMemoryStream.Create;
  WriteUInt64(Result, ServiceContext);
  WriteUInt64(Result, ThreadID);
  WriteUInt64(Result, Duration);
end;

procedure TThreadServiceMessage_ProcessDuration.LoadFromStream(
  aStream: TMemoryStream);
begin
  Assert(assigned(aStream));
  ServiceContext := ReadUInt64(aStream);
  ThreadID := ReadUInt64(aStream);
  Duration := ReadUInt64(aStream);
end;

{ TServiceTask }

procedure TServiceTask.Finalize;
begin
  //None here. Can be override if needed.
end;

procedure TServiceTask.Initialize;
begin
  //None here. Can be override if needed.
end;

end.
