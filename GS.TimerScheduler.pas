unit GS.TimerScheduler;

{$I GSCore.inc}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  {$IFDEF USE_GENERIC}
  Generics.Collections, //Starting 3.1.1 only.
  {$ENDIF}
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  {$IFDEF USE_GENERIC}
  System.Generics.Collections,
  {$ENDIF}
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Common,
  GS.Timer;

Type

  TGSEventItem = Class;
  TGSEventFiring = Procedure(Sender : TObject; aEvent : TGSEventItem) of Object;
  TGSEventItemPeriodicity = (none, EveryMilliSec, EveryDay, EveryWeek, EveryMonth, EveryYear, EveryDecade, EveryCentury);
  TGSEventItemPeriodicityTrigged = (Once, Ntime, Loop);
  TGSEventItem = Class
  private
    FEventFiredAndAckInfoString: String;
    FEventFiredAndAckDateTime: Boolean;
    procedure SetEventFinished(const Value: Boolean);
    procedure SetEventFiredAndAckDateTime(const Value: Boolean);
    procedure SetEventFiredAndAckInfoString(const Value: String);
    procedure SetEventFiredAndAckReceived(const Value: Boolean);
  protected
    FPeriodicity: TGSEventItemPeriodicity;
    FTriggedPeriodicity: TGSEventItemPeriodicityTrigged;

    FEventFired : Boolean;
    FEventFiredAndAckReceived : Boolean;
    FEventFinished : Boolean;
    FEventDateTime : TDateTime;
    FEventStringMessage : String;
    FEventCodeMessage : Int64;

    function GetIsPeriodic: Boolean; Virtual;

  Public

    Procedure PeriodicProcess(Out aNextEvent : TGSEventItem); Virtual;

    Procedure Process; virtual;

    Property EventDateTime : TDateTime read FEventDateTime Write FEventDateTime;
    Property EventStringMessage : String read FEventStringMessage Write FEventStringMessage;
    Property EventCodeMessage : Int64 read FEventCodeMessage Write FEventCodeMessage;

    Property EventFired : Boolean read FEventFired Write FEventFired;
    Property EventFiredAndAckReceived : Boolean read FEventFiredAndAckReceived write SetEventFiredAndAckReceived;
    Property EventFiredAndAckDateTime : Boolean read FEventFiredAndAckDateTime write SetEventFiredAndAckDateTime;
    Property EventFiredAndAckInfoString : String read FEventFiredAndAckInfoString write SetEventFiredAndAckInfoString;
    Property EventFinished : Boolean read FEventFinished write SetEventFinished;

    Property IsPeriodic : Boolean read GetIsPeriodic;
    Property Periodicity : TGSEventItemPeriodicity read  FPeriodicity;
    Property TriggedPeriodicity : TGSEventItemPeriodicityTrigged read FTriggedPeriodicity;
  End;

  {$IFDEF USE_GENERIC}
  TObjectList_TGSEventItem = TObjectList<TGSEventItem>;
  {$ELSE}
  TObjectList_TGSEventItem = Class(TList_ObjectArray)
  private
    function GetGSEventItem(Index: Uint32): TGSEventItem;
    procedure SetGSEventItem(Index: Uint32; const Value: TGSEventItem);
  public
    Constructor Create; Reintroduce;
    Procedure Add(aGSEventItem : TGSEventItem);
    Property Items[Index : Uint32] : TGSEventItem read GetGSEventItem Write SetGSEventItem; Default;
  End;

  {$ENDIF}

  TGSTimerEventList = Class(TObjectList_TGSEventItem)
  Public
  End;

  TGSTimerSchedulerThread = Class(TGSTimerThread)
  private
  protected
    FCurrentEvent : TGSEventItem;
    FOnEventFiring: TGSEventFiring;
    FEventList : TGSTimerEventList;
    FEventLocker : TCriticalSection;
    procedure DoTimerEvent; Override;
    procedure DoTimerFiring; Virtual;

    Procedure InternalReportEvent(aEvent : TGSEventItem); Virtual;

    Function GetReferenceDateTime : TDateTime; Virtual; //Override this to change reference time;
  public
    constructor Create(Const aTimerExecMode : TGSTimerExecMode = TGSTimerExecMode.Thread); Override;
    destructor Destroy; Override;


    Function AddEvent(aEvent : TGSEventItem) : TGSEventItem; Overload; Virtual;
    Function AddEvent(aDateTimeToOccured : TDateTime; aStringMessage : String; aCodeMessage : Int64) : TGSEventItem; Overload; Virtual;
    Function AddEventFromMilliSec(FromXMilliSecFromDate : Int64; StartFrom : TDateTime; aStringMessage : String; aCodeMessage : Int64) : TGSEventItem; Virtual;

    Property ReferenceDateTime : TDateTime read GetReferenceDateTime;
    Property OnEventFiring : TGSEventFiring read FOnEventFiring Write FOnEventFiring;
  end;

  TGSTimerSchedulerThreadClass = Class of TGSTimerSchedulerThread;
  //Why a container : It is good to destroy and recreate thread when changing deep
  //parameter : this container do this job.
  TGSTimerSchedulerThreadContainer = Class
  private
  protected
    FSchedClass: TGSTimerSchedulerThreadClass;
    FSched : TGSTimerSchedulerThread;
    FEnabled: Boolean;
    FOnTimerExecMode: TGSTimerExecMode;
    FOnTimer: TNotifyEvent;
    FInterval: Cardinal;
    FOnEventFiring: TGSEventFiring;
    procedure SetSchedClass(const Value: TGSTimerSchedulerThreadClass);
    function GetLoopCounter: Cardinal;
    function GetTimerThreadID: NativeInt;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimerExecMode(const Value: TGSTimerExecMode);
    procedure updatetimer; Virtual;
  public
    constructor Create; Virtual;
    Destructor Destroy; Override;

    { TODO -oVGS : Add here the thread method "addEvent" }

    property Enabled : Boolean read FEnabled write SetEnabled;
    property Interval: Cardinal read FInterval write SetInterval;

    Property LoopCounter : Cardinal read GetLoopCounter;
    Property TimerThreadID : NativeInt read GetTimerThreadID;

    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
    Property OnEventFiring : TGSEventFiring read FOnEventFiring Write FOnEventFiring;

    Property TimerExecMode : TGSTimerExecMode read FOnTimerExecMode Write SetOnTimerExecMode;

    Property SchedulerClass : TGSTimerSchedulerThreadClass read FSchedClass Write SetSchedClass;

  End;


implementation

Uses DateUtils;

{ TGSEventItem }


function TGSEventItem.GetIsPeriodic: Boolean;
begin
  result := FPeriodicity > TGSEventItemPeriodicity.none;
end;

procedure TGSEventItem.PeriodicProcess(out aNextEvent: TGSEventItem);
begin

end;

procedure TGSEventItem.Process;
begin
  FEventFired := True;
  //Send message here !
  FEventFinished := True;
  //...
  //...//
  //...//...//
  //...//...//...//
  //...//...//...//...//
  //...//...//...//...//...//
  //...//...//...//...//...//...//
  //...//...//...//...//...//
  //...//...//...//...//
  //...//...//...//
  //...//...//
  //...//
  //...

end;


procedure TGSEventItem.SetEventFinished(const Value: Boolean);
begin
  FEventFinished := Value;
end;

procedure TGSEventItem.SetEventFiredAndAckDateTime(const Value: Boolean);
begin
  FEventFiredAndAckDateTime := Value;
end;

procedure TGSEventItem.SetEventFiredAndAckInfoString(const Value: String);
begin
  FEventFiredAndAckInfoString := Value;
end;

procedure TGSEventItem.SetEventFiredAndAckReceived(const Value: Boolean);
begin
  FEventFiredAndAckReceived := Value;
end;

{ TGSTimerSchedulerThread }

function TGSTimerSchedulerThread.AddEvent(aDateTimeToOccured: TDateTime;
  aStringMessage: String; aCodeMessage: Int64): TGSEventItem;
begin
  Result := TGSEventItem.Create;
  Result.EventDateTime := aDateTimeToOccured;
  Result.EventStringMessage := aStringMessage;
  Result.EventCodeMessage := aCodeMessage;
  FEventLocker.Acquire;
  try
    FEventList.Add(Result);
  finally
    FEventLocker.Release;
  end;
end;

function TGSTimerSchedulerThread.AddEvent(aEvent: TGSEventItem): TGSEventItem;
begin
  Assert(Assigned(aEvent));
  result := aEvent;
  FEventLocker.Acquire;
  try
    FEventList.Add(aEvent);
  finally
    FEventLocker.Release;
  end;
end;

function TGSTimerSchedulerThread.AddEventFromMilliSec(FromXMilliSecFromDate: Int64;
  StartFrom: TDateTime; aStringMessage: String;
  aCodeMessage: Int64): TGSEventItem;
begin
  Result := TGSEventItem.Create;
  Result.EventDateTime := StartFrom + (86400 / (FromXMilliSecFromDate / 1000));
  Result.EventStringMessage := aStringMessage;
  Result.EventCodeMessage := aCodeMessage;
  FEventLocker.Acquire;
  try
    FEventList.Add(Result);
  finally
    FEventLocker.Release;
  end;
end;

constructor TGSTimerSchedulerThread.Create(
  const aTimerExecMode: TGSTimerExecMode);
begin
  inherited Create(aTimerExecMode);
  FEventLocker := TCriticalSection.Create;
  FEventList :=TGSTimerEventList.Create;
end;

destructor TGSTimerSchedulerThread.Destroy;
begin
  FEventLocker.Acquire;
  inherited;
  FreeAndNil(FEventLocker);
  FreeAndNil(FEventList);
end;

procedure TGSTimerSchedulerThread.DoTimerEvent;
var lEvent, lNextEvent : TGSEventItem;
    i : integer;
begin
  inherited DoTimerEvent; //Manage exec count and event.

  //Processing of schedule ...
  FEventLocker.Acquire;
  try
    for i := 0 to FEventList.Count-1 do
    begin
      lEvent := FEventList[i];
      if (lEvent.EventDateTime>GetReferenceDateTime) And
         (lEvent.EventFired) then
        Continue;

      if (lEvent.EventDateTime>GetReferenceDateTime) And
         (Not (lEvent.EventFired)) then
      begin
        lEvent.Process;
        InternalReportEvent(lEvent);
        lEvent.PeriodicProcess(lNextEvent);
        if Assigned(lNextEvent) then
        begin
          AddEvent(lNextEvent);
        end;
      end;
    end;
  finally
    FEventLocker.Release;
  end;
end;

procedure TGSTimerSchedulerThread.DoTimerFiring;
begin
  if Assigned(FCurrentEvent) and Assigned(FOnEventFiring) then
  begin
    FOnEventFiring(Self,FCurrentEvent);
  end;
end;

function TGSTimerSchedulerThread.GetReferenceDateTime: TDateTime;
begin
  Result := Now;
end;

procedure TGSTimerSchedulerThread.InternalReportEvent(aEvent : TGSEventItem);
begin
  if Not(Assigned(aEvent)) then
    Exit;
  if Not(Assigned(FOnEventFiring)) then
    Exit;

  FCurrentEvent := aEvent;
  case FTimerExecMode of
    TGSTimerExecMode.Thread : DoTimerFiring;
    TGSTimerExecMode.Synchro : Synchronize(Self,DoTimerEvent);
    TGSTimerExecMode.Queued : Queue(Self,DoTimerFiring);
  end;
end;


{ TGSTimerSchedulerThreadContainer }

constructor TGSTimerSchedulerThreadContainer.Create;
begin
  Inherited;
  FSchedClass := TGSTimerSchedulerThread;
  FOnTimerExecMode := TGSTimerExecMode.Thread;
  FEnabled := False;
  Interval := 1000;
end;

destructor TGSTimerSchedulerThreadContainer.Destroy;
begin
  Enabled := False;
  inherited;
end;

function TGSTimerSchedulerThreadContainer.GetLoopCounter: Cardinal;
begin
  Result := 0;
  if Assigned(FSched) then
  begin
    Result := FSched.LoopCounter;
  end;
end;

function TGSTimerSchedulerThreadContainer.GetTimerThreadID: NativeInt;
begin
  Result := 0;
  if Assigned(FSched) then
  begin
    Result := FSched.ThreadID;
  end;
end;

procedure TGSTimerSchedulerThreadContainer.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  UpdateTimer;
end;

procedure TGSTimerSchedulerThreadContainer.SetInterval(const Value: Cardinal);
begin
  FInterval := Value;
  if Assigned(FSched) then
    FSched.Interval := Value;
end;

procedure TGSTimerSchedulerThreadContainer.SetOnTimerExecMode(
  const Value: TGSTimerExecMode);
begin
  FOnTimerExecMode := Value;
end;

procedure TGSTimerSchedulerThreadContainer.SetSchedClass(
  const Value: TGSTimerSchedulerThreadClass);
begin
  FSchedClass := Value;
  updatetimer;
end;

procedure TGSTimerSchedulerThreadContainer.updatetimer;
begin
  if Assigned(FSched) then
  begin
    FreeAndNil(FSched);
  end;

  if FEnabled then
  begin
    if FInterval > 0 then
    begin
      FSched := FSchedClass.Create(FOnTimerExecMode);
      FSched.Interval := FInterval;
      FSched.OnTimer := FOnTimer;
      FSched.OnEventFiring := FOnEventFiring;
      FSched.Start;
    end
    else
    begin
      Enabled := False;
    end;
  end;
end;

{$IFNDEF USE_GENERIC}
{ TObjectList_TGSEventItem }

procedure TObjectList_TGSEventItem.Add(aGSEventItem: TGSEventItem);
begin
  ManagedAdd(aGSEventItem);
end;

constructor TObjectList_TGSEventItem.Create;
begin
  Inherited Create(true);
end;

function TObjectList_TGSEventItem.GetGSEventItem(Index: Uint32): TGSEventItem;
begin
  result := TGSEventItem(FArray[Index]);
end;

procedure TObjectList_TGSEventItem.SetGSEventItem(Index: Uint32;
  const Value: TGSEventItem);
begin
  ManagedSet(Index,Value);
end;

{$ENDIF}

end.
