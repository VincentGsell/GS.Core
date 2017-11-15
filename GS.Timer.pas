///-------------------------------------------------------------------------------
/// Title      : GS.Timer
/// Short Desc : Simple threaded timer.
/// Source     : https://github.com/VincentGsell
/// Aim        : - threaded timer (Not intended to be used in GUI as is.)
///              - Use inside a thread.
///-------------------------------------------------------------------------------
unit GS.Timer;

interface
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}


uses
  Classes, SysUtils, SyncObjs;

type
TofTimer = class;

TofTimerThread = class(TThread)
private
  FTimer : TofTimer;
  FTrigger : TEvent;
protected
  procedure DoTimer;
public
  constructor Create(ATimer : TofTimer);
  destructor Destroy; Override;
  procedure Execute; Override;
end;

TofTimer = class(TComponent)
private
  FEnabled: Boolean;
  FInterval: Cardinal;
  FOnTimer: TNotifyEvent;

  procedure SetEnabled(const Value: Boolean);
  procedure SetInterval(const Value: Cardinal);

protected
  FTimerThread : TofTimerThread;
  procedure UpdateTimer;

public
  constructor Create(AOwner:TComponent); Override;
  destructor Destroy; Override;

published
  property Enabled : Boolean read FEnabled write SetEnabled;
  property Interval: Cardinal read FInterval write SetInterval;

  property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
end;


implementation

{ TofTimerThread }

constructor TofTimerThread.Create(ATimer: TofTimer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FTimer := ATimer;
  FTrigger := TEvent.Create(Nil,False,False,EmptyStr);
end;

destructor TofTimerThread.Destroy;
begin
  inherited;
  FTrigger.SetEvent;
  FreeAndNil(FTrigger);
end;

procedure TofTimerThread.DoTimer;
begin
  if Assigned(FTimer.OnTimer) then
    FTimer.OnTimer(FTimer);
end;

procedure TofTimerThread.Execute;
begin
  while (not Self.Terminated) and (FTimer.Enabled) do
  begin
    FTrigger.WaitFor(FTimer.Interval);
    DoTimer;
  end;
end;


{ TofTimer }

constructor TofTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  Interval := 1000;
end;

destructor TofTimer.Destroy;
begin
  inherited;
end;
procedure TofTimer.UpdateTimer;
begin
  if Assigned(FTimerThread) then
  begin
    FTimerThread.Terminate;
    FTimerThread := nil;
  end;

  if Enabled then
  begin
    if FInterval > 0 then
    begin
      FTimerThread := TofTimerThread.Create(Self);
      FTimerThread.Start;
    end
    else
    begin
      Enabled := False;
    end;
  end;
end;


//Getters - Setters\
procedure TofTimer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateTimer;
end;

procedure TofTimer.SetInterval(const Value: Cardinal);
begin
  if Value <> FInterval then
  begin
    FInterval := Value;
    UpdateTimer;
  end;
end;

end.
