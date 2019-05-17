///-------------------------------------------------------------------------------
/// Title      : GS.Timer
/// Short Desc : Simple threaded timer.
/// Source     : https://github.com/VincentGsell
/// Aim        : - threaded timer (If you plan to use in a gui, please see TGSTimerExecMode)
///              - Use inside a thread.
///              - Not aimed to be precise, but lighter than classic TTimer
///-------------------------------------------------------------------------------
unit GS.Timer;

{$I GSCore.inc}

interface


uses
  Classes,
  SysUtils,
  SyncObjs,
  GS.Threads;


Const
  CST_TIMER_DEFAULT_VALUE = 1000; //In ms : 1 Seconde.

type
TGSTimerThreadContainer = class;
//Mode Thread : OnTimer is executed in FTimerThread context.
//Mode Synchro : OnTimer is executed with Synchronize (Expl : For Usage GUI)
//Mode Queued : OnTimer is executed with Queued (Expl : For Usage GUI, with no emergency. Delphi only)
TGSTimerExecMode = (Thread, Synchro, Queued);

TGSTimerThread = class(TThread)
private
protected
  FTrigger : TEvent;
  FTimerExecMode :TGSTimerExecMode;
  FLastError: String;
  FOnTimer: TNotifyEvent;
  Finterval : TGSProtectedUInt32;
  FExecCounter : TGSProtectedUInt32;
  function GetInterval: Cardinal;
  procedure SetInterval(const Value: Cardinal);
  function GetLoopCounter: Cardinal;

  Procedure DoTimerProcess; Virtual;
  procedure DoTimerEvent; Virtual;
public
  constructor Create(Const aTimerExecMode : TGSTimerExecMode = TGSTimerExecMode.Thread); Reintroduce; Virtual;
  destructor Destroy; Override;

  procedure Execute; Override;

  Property LastError : String read FLastError;
  property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
  Property Interval : Cardinal read GetInterval Write SetInterval;
  Property LoopCounter : Cardinal read GetLoopCounter;
end;

//Why a container : Better to destroy and recreate thread when changing deep
//parameter : this container do this job.

TGSTimerThreadContainer = class
private
protected
  FTimerThread : TGSTimerThread;
  FEnabled: Boolean;
  FInterval: Cardinal;
  FOnTimer: TNotifyEvent;
  FOnTimerExecMode: TGSTimerExecMode;

  procedure SetEnabled(const Value: Boolean);
  procedure SetInterval(const Value: Cardinal);
  procedure SetOnTimerExecMode(const Value: TGSTimerExecMode);
  function GetLoopCounter: Cardinal;
  function GetTimerThreadID: NativeInt;
  procedure UpdateTimer; Virtual;
public
  constructor Create; Virtual;
  destructor Destroy; Override;

published
  property Enabled : Boolean read FEnabled write SetEnabled;
  property Interval: Cardinal read FInterval write SetInterval;

  Property LoopCounter : Cardinal read GetLoopCounter;
  Property TimerThreadID : NativeInt read GetTimerThreadID;

  property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;

  Property TimerExecMode : TGSTimerExecMode read FOnTimerExecMode Write SetOnTimerExecMode;
end;



implementation

{ TGSTimerThread }

constructor TGSTimerThread.Create(Const aTimerExecMode : TGSTimerExecMode);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  Finterval := TGSProtectedUInt32.Create(CST_TIMER_DEFAULT_VALUE);
  FExecCounter := TGSProtectedUInt32.Create(0);
  FTrigger := TEvent.Create(Nil,False,False,EmptyStr);
  FTrigger.ResetEvent;
  FTimerExecMode := aTimerExecMode;
  FLastError := EmptyStr;
end;

destructor TGSTimerThread.Destroy;
begin
  If Not(Suspended) then
  begin
    Terminate;
    FTrigger.SetEvent;
    WaitFor;
  end;
  FreeAndNil(Finterval);
  FreeAndNil(FExecCounter);
  FreeAndNil(FTrigger);
  inherited;
end;

procedure TGSTimerThread.DoTimerEvent;
begin
  if Terminated then
    Exit;
  FExecCounter.Inc;
  if Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TGSTimerThread.DoTimerProcess;
begin
  if FTrigger.WaitFor(Finterval.Value) = TWaitResult.wrTimeout then
  begin
    if Terminated then
      Exit;
    try
      case FTimerExecMode of
        TGSTimerExecMode.Thread: DoTimerEvent;
        TGSTimerExecMode.Synchro: Synchronize(DoTimerEvent);
        TGSTimerExecMode.Queued: Queue(DoTimerEvent);
      end;
    Except
      On E : Exception do
      begin
        FLastError := '['+ClassName+'] - '+E.Message;
      end;
    end;
  end;
  //In All oher case (wrEvent, erError) we exit : It used to finish th thread.
end;

procedure TGSTimerThread.Execute;
begin
  while (not Terminated) do
  begin
    DoTimerProcess;
  end;
end;

function TGSTimerThread.GetInterval: Cardinal;
begin
  Result := Finterval.Value
end;

function TGSTimerThread.GetLoopCounter: Cardinal;
begin
  Result := FExecCounter.Value;
end;

procedure TGSTimerThread.SetInterval(const Value: Cardinal);
begin
  Finterval.Value := Value;
  FTrigger.SetEvent; //The triggers's time out could be high : we have to wait uselessy until the setevent.
end;

{ TGSTimerThreadContainer }


constructor TGSTimerThreadContainer.Create;
begin
  inherited Create;
  FOnTimerExecMode := TGSTimerExecMode.Thread;
  FEnabled := False;
  Interval := 1000;
end;

destructor TGSTimerThreadContainer.Destroy;
begin
  Enabled := False;
  inherited;
end;

function TGSTimerThreadContainer.GetLoopCounter: Cardinal;
begin
  Result := 0;
  if Assigned(FTimerThread) then
  begin
    Result := FTimerThread.LoopCounter;
  end;
end;

function TGSTimerThreadContainer.GetTimerThreadID: NativeInt;
begin
  Result := 0;
  if Assigned(FTimerThread) then
  begin
    Result := FTimerThread.ThreadID;
  end;
end;

procedure TGSTimerThreadContainer.UpdateTimer;
begin
  if Assigned(FTimerThread) then
  begin
    FreeAndNil(FTimerThread);
  end;

  if FEnabled then
  begin
    if FInterval > 0 then
    begin
      FTimerThread := TGSTimerThread.Create(FOnTimerExecMode);
      FTimerThread.Interval := FInterval;
      FTimerThread.OnTimer := FOnTimer;
      FTimerThread.Start;
    end
    else
    begin
      Enabled := False;
    end;
  end;
end;


//Getters - Setters\
procedure TGSTimerThreadContainer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateTimer;
end;

procedure TGSTimerThreadContainer.SetInterval(const Value: Cardinal);
begin
  FInterval := Value;
  if Assigned(FTimerThread) then
    FTimerThread.Interval := Value;
end;

procedure TGSTimerThreadContainer.SetOnTimerExecMode(const Value: TGSTimerExecMode);
begin
  FOnTimerExecMode := Value;
  if FEnabled then
    UpdateTimer;
end;

end.
