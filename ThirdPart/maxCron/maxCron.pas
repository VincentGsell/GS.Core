unit maxCron;
{

  Version: 2.2
 NOTE:
 202109 VGS : Modify for crossplatform use, without TApplication (console server app).
              Remove useless units.
}

interface

uses
  SysUtils,
  classes,
  Generics.Collections,
  GS.Timer;

Type
  // forward declarations
  TmaxCron = class;
  TmaxCronEvent = class;
  TCronSchedulePlan = class;

  TmaxCronNotifyEvent = procedure(Sender: TmaxCronEvent) of object;
  TmaxCronNotifyProc = reference to procedure(Sender: TmaxCronEvent);

  TmaxCron = class(TObject)
  private
//    fTimer: TGSTimerThreadContainer;
    fItems: TObjectList<TmaxCronEvent>;
    function GetCount: integer;
    function GetEvents(index: integer): TmaxCronEvent;
    procedure TimerTimer(Sender: TObject);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    procedure Update;

    Function Add(const aName: string): TmaxCronEvent; overload;
    Function Add(const aName, aEventPlan: string; const aOnScheduleEvent: TmaxCronNotifyEvent): TmaxCronEvent; overload;
    Function Add(const aName, aEventPlan: string; const aOnScheduleEvent: TmaxCronNotifyProc): TmaxCronEvent; overload;

    function Delete(index: integer): boolean; overload;
    function Delete(event: TmaxCronEvent): boolean; overload;
    function IndexOf(event: TmaxCronEvent): integer;

    property Count: integer read GetCount;
    property Events[index: integer]: TmaxCronEvent read GetEvents;
  end;

  TmaxCronEvent = class(TObject)
  private
    fScheduler: TCronSchedulePlan;
    FEventPlan: string;

    FName: string;
    FOnScheduleEvent: TmaxCronNotifyEvent;

    FTag: integer;
    FUserData: Pointer;
    FUserDataInterface: iInterface;

    FEnabled: boolean;
    fNextSchedule: TDateTime;
    FValidFrom: TDateTime;
    FValidTo: TDateTime;
    FOnScheduleProc: TmaxCronNotifyProc;
    fNumOfExecutions: uint64;
    fLastExecutionTime: TDateTime;
    procedure SetName(const Value: string);
    procedure SetOnScheduleEvent(const Value: TmaxCronNotifyEvent);
    procedure SetTag(const Value: integer);
    procedure SetUserData(const Value: Pointer);
    procedure SetEventPlan(const Value: string);
    procedure SetEnabled(const Value: boolean);
    procedure SetNumOfExecutions(const Value: uint64);
    procedure SetValidFrom(const Value: TDateTime);
    procedure SetValidTo(const Value: TDateTime);
    procedure SetUserDataInterface(const Value: iInterface);
    procedure SetOnScheduleProc(const Value: TmaxCronNotifyProc);

    // this is the main function that will be called by the TmaxCron in a timer
    procedure checkTimer;
    procedure ResetSchedule;
  public
    constructor Create;
    destructor Destroy; override;

    function Run: TmaxCronEvent;
    procedure Stop;

    property EventPlan: string read FEventPlan write SetEventPlan;
    property NextSchedule: TDateTime read fNextSchedule;
    property Name: string read FName write SetName;
    property LastExecution: TDateTime read fLastExecutionTime;

    // User data
    property Tag: integer read FTag write SetTag;
    property UserData: Pointer read FUserData write SetUserData;
    Property UserDataInterface: iInterface read FUserDataInterface write SetUserDataInterface;

    property OnScheduleEvent: TmaxCronNotifyEvent read FOnScheduleEvent write SetOnScheduleEvent;
    // you can use an anonymous method as well
    property OnScheduleProc: TmaxCronNotifyProc read FOnScheduleProc write SetOnScheduleProc;

    property Enabled: boolean read FEnabled write SetEnabled;

    // tels how many times this event was executed
    property NumOfExecutionsPerformed: uint64 read fNumOfExecutions;

    Property ValidFrom: TDateTime read FValidFrom write SetValidFrom;
    property ValidTo: TDateTime read FValidTo write SetValidTo;
  end;

  // those are the parts as they appear in that order
  TPartKind = (
    ckMinute = 0,
    ckHour,
    ckDayOfTheMonth,
    ckMonth,
    ckDayOfTheWeek,
    ckYear,
    ckSecond
    // ckExecutionLimit  - this is not a part kind but it is a part of the schedule string
    );

  TPlan = record
  private
    function asString: String;
    procedure setText(const Value: string);
  public
    parts: array [0 .. 7] of string;

    property Minute: string read parts[0] write parts[0];
    property Hour: string read parts[1] write parts[1];
    property DayOfTheMonth: string read parts[2] write parts[2];
    property Month: string read parts[3] write parts[3];
    property DayOfTheWeek: string read parts[4] write parts[4];
    property Year: string read parts[5] write parts[5];
    property Second: string read parts[6] write parts[6];
    property ExecutionLimit: string read parts[7] write parts[7];

    property text: string read asString write setText;

    // resets all values to their defaults
    procedure reset;
  end;

  TCronPart = class(TObject)
  private
    FData: string;
    FValidFrom, FValidTo: word;
    fPartKind: TPartKind;
    fFullrange: boolean;
    fRange: array of word;
    FCount: integer;

    procedure Parse;
    procedure SetData(const Value: string);
    procedure ParsePart(const Value: string);
    function ReplaceMonthNames(const Value: string): string;
    function ReplaceDaynames(const Value: string): string;
    Procedure Add2Range(Value: word);
    function FindInRange(Value: word; out index: integer): boolean;

    function PushYear(var NextDate: TDateTime): boolean;
    function PushMonth(var NextDate: TDateTime): boolean;
    function PushDayOfMonth(var NextDate: TDateTime): boolean;
    function PushDayOfWeek(var NextDate: TDateTime): boolean;
    function PushHour(var NextDate: TDateTime): boolean;
    function PushMinute(var NextDate: TDateTime): boolean;
    function PushSecond(var NextDate: TDateTime): boolean;
    function GetFullrange: boolean;
  public
    constructor Create(aPartKind: TPartKind);
    destructor Destroy; override;

    procedure Clear;
    function NextVal(v: word): word;

    property Data: string read FData write SetData;
    property PartKind: TPartKind read fPartKind;
    property Fullrange: boolean read GetFullrange;
  end;

  // this is a helper class to calculate and parse the schedule plan
  TCronSchedulePlan = class
  private
    FSecond: TCronPart;
    FMinute: TCronPart;
    FHour: TCronPart;
    FMonth: TCronPart;
    FDayOfTheMonth: TCronPart;
    FDayOfTheWeek: TCronPart;
    FYear: TCronPart;
    fExecutionLimit: LongWord;

    function GetParts(PartKind: TPartKind): TCronPart;
    procedure SetDayOfTheMonth(const Value: TCronPart);
    procedure SetDayOfTheWeek(const Value: TCronPart);
    procedure SetHour(const Value: TCronPart);
    procedure SetMinute(const Value: TCronPart);
    procedure SetMonth(const Value: TCronPart);
    procedure SetParts(PartKind: TPartKind; const Value: TCronPart);
    procedure SetSecond(const Value: TCronPart);
    procedure SetYear(const Value: TCronPart);
  public
    Constructor Create;
    Destructor Destroy; override;

    procedure Parse(const CronPlan: string);
    procedure Clear;

    function FindNextScheduleDate(const aBaseDate: TDateTime;
      out aNextDateTime: TDateTime;
      const aValidFrom: TDateTime = 0;
      const aValidTo: TDateTime = 0): boolean;

    property Second: TCronPart read FSecond write SetSecond;
    property Minute: TCronPart read FMinute write SetMinute;
    property Hour: TCronPart read FHour write SetHour;
    property Day_of_the_Month: TCronPart read FDayOfTheMonth write SetDayOfTheMonth;
    property Month: TCronPart read FMonth write SetMonth;
    property Day_of_the_Week: TCronPart read FDayOfTheWeek write SetDayOfTheWeek;
    property Year: TCronPart read FYear write SetYear;
    property parts[PartKind: TPartKind]: TCronPart read GetParts write SetParts;
    property ExecutionLimit: LongWord read fExecutionLimit;
  end;

  TDates = array of TDateTime;

  // you can use this to show the user a preview ow what his schedule will look like.
function MakePreview(const SchedulePlan: string; out Dates: TDates; Limit: integer = 100): boolean;

implementation

uses
  dateUtils,
  math;

const
  OneMinute = 1 / 24 / 60;
  OneSecond = OneMinute / 60;
  OneHour = 1 / 24;

const
  DayNames: array [1 .. 7] of string = (
    'Mon',
    'Tue',
    'Wed',
    'Thu',
    'Fri',
    'Sat',
    'Sun');

const
  MonthNames: array [1 .. 12] of string = (
    'Jan',
    'Feb',
    'Mar',
    'Apr',
    'May',
    'Jun',
    'Jul',
    'Aug',
    'Sep',
    'Oct',
    'Nov',
    'Dec');

procedure Log(const msg: string);
begin
  // pawel1.AddToLogFile(msg, 's:\tmp\te.log');
end;

procedure SplitString(const s: string; delim: char; ts: TStrings);
var
  i: integer;
  t: string;
begin
  t := '';
  for i := 1 to length(s) do
  begin
    if (s[i] <> delim) then
      t := t + s[i]
    else
    begin
      ts.Add(t);
      t := '';
    end;
  end;

  if t <> '' then
    ts.Add(t);
end;

{ TCronSchedulePlan }

procedure TCronSchedulePlan.Clear;
var
  pk: TPartKind;
begin
  fExecutionLimit := 0;
  for pk := Low(TPartKind) to High(TPartKind) do
    parts[pk].Clear;
end;

constructor TCronSchedulePlan.Create;
var
  pk: TPartKind;
begin
  inherited;

  for pk := low(pk) to High(pk) do
    parts[pk] := TCronPart.Create(pk);
end;

destructor TCronSchedulePlan.Destroy;
var
  pk: TPartKind;
begin
  for pk := low(pk) to High(pk) do
    parts[pk].Free;
  inherited;
end;

function TCronSchedulePlan.FindNextScheduleDate(const aBaseDate: TDateTime;
  out aNextDateTime: TDateTime;
  const aValidFrom,
  aValidTo: TDateTime): boolean;
var
  dc: word;
  v: word;
  StartDate: TDateTime;
  pk: TPartKind;
  h, m, s, ms: word;
begin
  Result := false;

  if (aBaseDate > aValidFrom) then
    StartDate := aBaseDate
  else
    StartDate := aValidFrom;

  // clear out milliseconds
  DecodeTime(StartDate, h, m, s, ms);
  StartDate := trunc(StartDate) +
    encodeTime(h, m, s, 0);
  StartDate := StartDate + OneSecond;

  aNextDateTime := StartDate;

  while True do
  begin
    if (aValidTo > 0) and (aNextDateTime > aValidTo) then
      Exit(false);



    // year
    if not FYear.PushYear(aNextDateTime) then
      Exit;

    if FMonth.PushMonth(aNextDateTime) then
      if FDayOfTheMonth.PushDayOfMonth(aNextDateTime) then
        if FDayOfTheWeek.PushDayOfWeek(aNextDateTime) then
          if FHour.PushHour(aNextDateTime) then
            if FMinute.PushMinute(aNextDateTime) then
              if FSecond.PushSecond(aNextDateTime) then
              begin
                Exit(True);
              end;
  end;
end;

function TCronSchedulePlan.GetParts(PartKind: TPartKind): TCronPart;
begin
  case PartKind of
    ckSecond:
      Result := FSecond;
    ckMinute:
      Result := FMinute;
    ckHour:
      Result := FHour;
    ckDayOfTheMonth:
      Result := FDayOfTheMonth;
    ckMonth:
      Result := FMonth;
    ckDayOfTheWeek:
      Result := FDayOfTheWeek;
  else
    // ckyear:
    Result := FYear;
  end;
end;

procedure TCronSchedulePlan.Parse(const CronPlan: string);
var
  plan: TPlan;
  s: string;
  pk: TPartKind;
begin
  Clear;
  plan.text := CronPlan;

  for pk := Low(TPartKind) to High(TPartKind) do
    parts[pk].Data := plan.parts[integer(pk)];

  fExecutionLimit := 0;
  if plan.ExecutionLimit <> '*' then
    fExecutionLimit := StrToIntDef(plan.ExecutionLimit, 0);
end;

procedure TCronSchedulePlan.SetDayOfTheMonth(const Value: TCronPart);
begin
  FDayOfTheMonth := Value;
end;

procedure TCronSchedulePlan.SetDayOfTheWeek(const Value: TCronPart);
begin
  FDayOfTheWeek := Value;
end;

procedure TCronSchedulePlan.SetHour(const Value: TCronPart);
begin
  FHour := Value;
end;

procedure TCronSchedulePlan.SetMinute(const Value: TCronPart);
begin
  FMinute := Value;
end;

procedure TCronSchedulePlan.SetMonth(const Value: TCronPart);
begin
  FMonth := Value;
end;

procedure TCronSchedulePlan.SetParts(PartKind: TPartKind; const Value: TCronPart);
begin
  case PartKind of
    ckSecond:
      FSecond := Value;
    ckMinute:
      FMinute := Value;
    ckHour:
      FHour := Value;
    ckDayOfTheMonth:
      FDayOfTheMonth := Value;
    ckMonth:
      FMonth := Value;
    ckDayOfTheWeek:
      FDayOfTheWeek := Value;
    ckYear:
      FYear := Value;
  end;
end;

procedure TCronSchedulePlan.SetSecond(const Value: TCronPart);
begin
  FSecond := Value;
end;

procedure TCronSchedulePlan.SetYear(const Value: TCronPart);
begin
  FYear := Value;
end;

{ TCronPart }

procedure TCronPart.Add2Range(Value: word);
var
  i: integer;
begin
  if Value >= FValidFrom then
    if Value <= FValidTo then
      if not self.FindInRange(Value, i) then
      begin
        SetLength(fRange, FCount + 1);
        if i < FCount then
          Move(fRange[i], fRange[i + 1], Sizeof(fRange[0]) * (FCount - i));
        fRange[i] := Value;
        inc(FCount);
      end;
end;

procedure TCronPart.Clear;
begin
  fFullrange := false;
  FCount := 0;
  fRange := NIL;
end;

constructor TCronPart.Create;
begin
  inherited Create;
  FCount := 0;
  fRange := NIL;
  fPartKind := aPartKind;

  Data := '*';

  case fPartKind of

    ckSecond,
      ckMinute:
      begin
        FValidFrom := 0;
        FValidTo := 59;
      end;
    ckHour:
      begin
        FValidFrom := 0;
        FValidTo := 23;
      end;
    ckDayOfTheMonth:
      begin
        FValidFrom := 1;
        FValidTo := 31;
      end;
    ckMonth:
      begin
        FValidFrom := 1;
        FValidTo := 12;
      end;
    ckDayOfTheWeek:
      begin
        FValidFrom := 1; // Monday
        FValidTo := 7;
      end;
    ckYear:
      begin
        FValidFrom := 1900;
        FValidTo := 3000;
      end;

  end;
end;

destructor TCronPart.Destroy;
begin
  Clear;
  inherited;
end;

function TCronPart.FindInRange(Value: word; out index: integer): boolean;
var
  C, l, h, i: integer;
begin
  Result := false;
  l := 0;
  h := FCount - 1;
  while l <= h do
  begin
    i := (l + h) shr 1;

    IF fRange[i] > Value then
      C := 1
    else if fRange[i] < Value then
      C := -1
    else
      C := 0;

    if C < 0 then
      l := i + 1
    else
    begin
      h := i - 1;
      if C = 0 then
      Begin
        Result := True;
        Index := i;
        Exit;
      end;
    end;
  end;
  Index := l;
end;

function TCronPart.GetFullrange: boolean;
begin
  Result := fFullrange OR (FCount = 0)
end;

function TCronPart.NextVal(v: word): word;
var
  i: integer;
begin
  if Fullrange then
    Result := v
  else
  begin
    if FindInRange(v, i) then
      Result := v
    else
    begin
      if i < FCount then
        Result := fRange[i]
      else
        Result := fRange[0];
    end;
  end;

end;

procedure TCronPart.Parse;
var
  x: integer;
  l: TStringList;
begin
  Clear;

  if FData = '*' then
  begin
    fFullrange := True;
    fRange := NIL;
    FCount := 0;
  end else begin

    l := TStringList.Create;
    SplitString(FData, ',', l);
    for x := 0 to l.Count - 1 do
    begin
      if l[x] = '*' then
      begin
        fFullrange := True;
        fRange := NIL;
        FCount := 0;
      end
      else
        ParsePart(l[x]);
    end;

    l.Free;
  end;
end;

procedure TCronPart.ParsePart(const Value: string);
var
  iR: integer;
  RangeTo: word;
  RangeFrom: word;
  i: integer;
  s: string;
  Repeater: integer;
begin

  s := Value;
  case fPartKind of
    ckMonth:
      s := ReplaceMonthNames(s);
    ckDayOfTheWeek:
      s := ReplaceDaynames(s);
  end;

  iR := Pos('-', s);
  if (iR > 1) or (s[1] = '*') then
  begin
    i := Pos('/', s);
    if i > 1 then
    begin
      Repeater := StrToInt(copy(s, i + 1, length(s)));
      s := copy(s, 1, i - 1);
    end
    else
      Repeater := 1;

    if iR > 0 then
    begin
      RangeFrom := StrToInt(copy(s, 1, iR - 1));
      RangeTo := StrToInt(copy(s, iR + 1, length(s)));
    end else begin
      RangeFrom := FValidFrom;
      RangeTo := FValidTo;
    end;

    i := RangeFrom;
    repeat
      self.Add2Range(i);
      inc(i, Repeater);
    until i > RangeTo;

  end
  else
    Add2Range(StrToInt(s));
end;

function TCronPart.PushDayOfMonth(var NextDate: TDateTime): boolean;
var
  dc, v, i: word;
begin
  Result := True;
  if not Fullrange then
  begin
    v := DayOf(NextDate);
    i := NextVal(v);
    if i < v then
    begin
      NextDate := EncodeDateTime(YearOf(NextDate), MonthOf(NextDate), i, 0, 0, 0, 0);
      NextDate := IncMonth(NextDate);
      Result := false;
    end
    else if i > v then
    begin
      dc := dateUtils.DaysInMonth(NextDate);
      if i <= dc then
        NextDate := EncodeDateTime(YearOf(NextDate), MonthOf(NextDate), i, 0, 0, 0, 0)
      else
      begin
        NextDate := EncodeDateTime(YearOf(NextDate), MonthOf(NextDate), 1, 0, 0, 0, 0);
        NextDate := IncMonth(NextDate);
        Result := false;
      end;
    end;
  end;
end;

function TCronPart.PushDayOfWeek(var NextDate: TDateTime): boolean;
var
  v, i: word;
begin
  Result := True;
  if not Fullrange then
  begin
    v := DayOfTheWeek(NextDate);
    i := NextVal(v);
    if i <> v then
    begin
      Result := false;
      NextDate := trunc(NextDate); // reset hh:nn:ss to 00:00:00
      if i > v then
        NextDate := NextDate + (i - v)
      else
        NextDate := NextDate + (7 - v) + i;

    end;

  end;
end;

function TCronPart.PushHour(var NextDate: TDateTime): boolean;
var
  v, i: word;
begin
  Result := True;
  if not Fullrange then
  begin
    v := HourOf(NextDate);
    i := NextVal(v);
    if i < v then
    begin
      Result := false;
      NextDate := trunc(NextDate) + 1 + i * OneHour;
    end
    else if i > v then
      NextDate := trunc(NextDate) + i * OneHour;
  end;
end;

function TCronPart.PushMinute(var NextDate: TDateTime): boolean;
var
  h, v, i: word;
begin
  Result := True;
  if not Fullrange then
  begin
    v := MinuteOf(NextDate);
    i := NextVal(v);
    if i < v then
    begin
      Result := false;
      h := HourOf(NextDate);
      NextDate := trunc(NextDate) + (h + 1) * OneHour + i * OneMinute;
    end
    else if i > v then
    begin
      h := HourOf(NextDate);
      NextDate := trunc(NextDate) + h * OneHour + i * OneMinute;
    end;
  end;
end;

function TCronPart.PushMonth(var NextDate: TDateTime): boolean;
var
  v, i: word;
begin
  Result := True;
  if not Fullrange then
  begin
    v := MonthOf(NextDate);
    i := NextVal(v);
    if i < v then
    begin
      NextDate := EncodeDateTime(YearOf(NextDate) + 1, i, 1, 0, 0, 0, 0);
      Result := false;
    end
    else if i > v then
    begin
      NextDate := EncodeDateTime(YearOf(NextDate), i, 1, 0, 0, 0, 0)
    end;
  end;

end;

function TCronPart.PushSecond(var NextDate: TDateTime): boolean;
var
  h, m, v, i: word;
begin
  Result := True;
  if not Fullrange then
  begin
    v := SecondOf(NextDate);
    i := NextVal(v);
    if i < v then
    begin
      Result := false;
      h := HourOf(NextDate);
      m := MinuteOf(NextDate);
      NextDate := trunc(NextDate) + h * OneHour + (m + 1) * OneMinute + i * OneSecond;
    end
    else if i > v then
    begin
      h := HourOf(NextDate);
      m := MinuteOf(NextDate);
      NextDate := trunc(NextDate) + h * OneHour + m * OneMinute + i * OneSecond;
    end;
  end;

end;

function TCronPart.PushYear(var NextDate: TDateTime): boolean;
var
  v, i: word;
begin
  Result := True;
  v := YearOf(NextDate);
  if v > FValidTo then
  begin
    Result := false;
    Exit;
  end;

  if not Fullrange then
  begin
    i := self.NextVal(v);
    if i < v then
      Result := false
    else if i > v then
    begin
      NextDate := EncodeDateTime(i, 1, 1, 0, 0, 0, 0)
    end;
  end;
end;

function TCronPart.ReplaceDaynames(const Value: string): string;
var
  x: integer;
  s: string;
begin
  s := Value;
  for x := 1 to 7 do
    s := StringReplace(s, DayNames[x], IntToStr(x), [rfReplaceAll, rfIgnorecase]);
  Result := s;
end;

function TCronPart.ReplaceMonthNames(const Value: string): string;
var
  x: integer;
  s: string;
begin
  s := Value;
  for x := 1 to 12 do
    s := StringReplace(s, MonthNames[x], IntToStr(x), [rfReplaceAll, rfIgnorecase]);
  Result := s;
end;

procedure TCronPart.SetData(const Value: string);
begin
  if FData <> Value then
  begin
    FData := Value;
    Parse;
  end;
end;

{ TScheduledEvent }

constructor TmaxCronEvent.Create;
begin
  inherited;
  FValidFrom := 0;
  FValidTo := 0;
  fScheduler := TCronSchedulePlan.Create;
  FEnabled := false;
end;

destructor TmaxCronEvent.Destroy;
begin
  fScheduler.Free;

  inherited
end;

procedure TmaxCronEvent.ResetSchedule;
var
  dt: TDateTime;
begin
  if fLastExecutionTime = 0 then
    dt := now + OneSecond
  else
    dt := fLastExecutionTime + OneSecond;

  if not fScheduler.FindNextScheduleDate(dt,
    fNextSchedule,
    FValidFrom, FValidTo) then
    Stop;

end;

function TmaxCronEvent.Run: TmaxCronEvent;
begin
  Result := self;

  FEnabled := True;
  fNumOfExecutions := 0;
  fLastExecutionTime := 0;

  self.ResetSchedule;
end;

procedure TmaxCronEvent.SetEnabled(const Value: boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    case FEnabled of
      True:
        Run;
      false:
        Stop;
    end;
  end;
end;

procedure TmaxCronEvent.SetEventPlan(const Value: string);
begin
  if FEventPlan <> Value then
  begin
    FEventPlan := Value;
    fScheduler.Parse(Value);
    ResetSchedule;
  end;
end;

procedure TmaxCronEvent.SetName(const Value: string);
begin
  FName := Value
end;

procedure TmaxCronEvent.SetOnScheduleEvent(const Value: TmaxCronNotifyEvent);
begin
  self.FOnScheduleEvent := Value;
end;

procedure TmaxCronEvent.SetNumOfExecutions(const Value: uint64);
begin
  fNumOfExecutions := Value;
  if fScheduler.ExecutionLimit <> 0 then
    if fNumOfExecutions >= fScheduler.ExecutionLimit then
      Stop;

end;

procedure TmaxCronEvent.SetTag(const Value: integer);
begin
  FTag := Value
end;

procedure TmaxCronEvent.SetUserData(const Value: Pointer);
begin
  FUserData := Value
end;

procedure TmaxCronEvent.SetUserDataInterface(const Value: iInterface);
begin
  FUserDataInterface := Value;
end;

procedure TmaxCronEvent.SetValidFrom(const Value: TDateTime);
begin
  FValidFrom := Value;
end;

procedure TmaxCronEvent.SetValidTo(const Value: TDateTime);
begin
  FValidTo := Value;
end;

procedure TmaxCronEvent.Stop;
begin
  FEnabled := false;
end;

{ TSchEventList }

function TmaxCron.Add(const aName: string): TmaxCronEvent;
var
  event: TmaxCronEvent;
begin
  event := TmaxCronEvent.Create;
  event.Name := aName;
  Result := event;
  fItems.Add(event);
end;

procedure TmaxCron.Clear;
var
  x: integer;
begin
  fItems.Clear;
end;

constructor TmaxCron.Create;
begin
  inherited;
  fItems := TObjectList<TmaxCronEvent>.Create;
//  fTimer := TGSTimerThreadContainer.Create;
//  fTimer.onTimer := TimerTimer;
//  fTimer.Interval := 1000;
//  fTimer.Enabled := True;
end;

function TmaxCron.Delete(index: integer): boolean;
begin

  if (index >= 0) and (index < self.Count) then
  begin
    Result := True;
    fItems.Delete(index);
  end
  else
    Result := false;
end;

function TmaxCron.Delete(event: TmaxCronEvent): boolean;
var
  i: integer;
begin
  Result := false;
  i := IndexOf(event);
  if i <> -1 then
    Result := Delete(i);
end;

destructor TmaxCron.Destroy;
begin
  Clear;

  fItems.Free;
//  fTimer.Free;
  inherited;
end;

function TmaxCron.GetCount: integer;
begin
  Result := fItems.Count
end;

function TmaxCron.GetEvents(index: integer): TmaxCronEvent;
begin
  Result := fItems[index]
end;

function TmaxCron.IndexOf(event: TmaxCronEvent): integer;
var
  x: integer;
begin
  Result := fItems.IndexOf(event);
end;

procedure TmaxCron.TimerTimer(Sender: TObject);
var
  x: integer;
begin
  for x := 0 to fItems.Count - 1 do
    if fItems[x].Enabled then
      fItems[x].checkTimer;
end;

procedure TmaxCron.Update;
begin
  TimerTimer(self);
end;

function MakePreview(const SchedulePlan: string; out Dates: TDates; Limit: integer = 100): boolean;
var
  C, x: integer;
  d: TDateTime;
  scheduler: TCronSchedulePlan;
begin
  scheduler := TCronSchedulePlan.Create;
  scheduler.Parse(SchedulePlan);
  SetLength(Dates, Limit);
  C := 0;
  d := now;
  for x := 0 to Limit - 1 do
  begin
    if not scheduler.FindNextScheduleDate(d, d) then
      break;

    Dates[x] := d;
    inc(C);

  end;

  SetLength(Dates, C);
end;

procedure TmaxCronEvent.SetOnScheduleProc(const Value: TmaxCronNotifyProc);
begin
  FOnScheduleProc := Value;
end;

function TmaxCron.Add(const aName, aEventPlan: string;
  const aOnScheduleEvent: TmaxCronNotifyEvent): TmaxCronEvent;
begin
  Result := Add(aName);
  Result.EventPlan := aEventPlan;
  Result.OnScheduleEvent := aOnScheduleEvent;
end;

function TmaxCron.Add(const aName, aEventPlan: string;
  const aOnScheduleEvent: TmaxCronNotifyProc): TmaxCronEvent;
begin
  Result := Add(aName);
  Result.EventPlan := aEventPlan;
  Result.OnScheduleProc := aOnScheduleEvent;
end;

procedure TmaxCronEvent.checkTimer;
var
  dt: TDateTime;
begin
  dt := now;
  if dt >= fNextSchedule then
  begin
    fNumOfExecutions := fNumOfExecutions + 1;
    fLastExecutionTime := dt;

    if Assigned(OnScheduleEvent) then
      OnScheduleEvent(self);

    if Assigned(FOnScheduleProc) then
      FOnScheduleProc(self);

    if fScheduler.ExecutionLimit <> 0 then
      if fNumOfExecutions >= fScheduler.ExecutionLimit then
        Stop;

    if FEnabled then // one of the above callbacks might set it to false, so we need to check it here
      ResetSchedule;
  end;
end;

{ TPlan }

function TPlan.asString: String;
const
  sep = ' ';

  function process(const s: string; aDefault: char = '*'): string;
  begin
    Result := Trim(s);
    if Result = '' then
      Result := aDefault;
  end;

begin
  Result :=
    process(Minute) + sep +
    process(Hour) + sep +
    process(DayOfTheMonth) + sep +
    process(Month) + sep +
    process(DayOfTheWeek) + sep +
    process(Year) + sep +
    process(Second, '0') + sep +
    process(ExecutionLimit, '0');
end;

procedure TPlan.reset;
begin
  Minute := '';
  Hour := '';
  DayOfTheMonth := '';
  Month := '';
  DayOfTheWeek := '';
  Year := '';
  Second := '0';
  ExecutionLimit := '0';
end;

procedure TPlan.setText(const Value: string);
var
  l: TStringList;
  s: string;
  x: integer;
begin
  reset;

  s := Value;
  // preprocess the string
  s := StringReplace(s, '*', ' *', [rfReplaceAll]);
  s := StringReplace(s, ', ', ',', [rfReplaceAll]);
  s := StringReplace(s, '  ', ' ', [rfReplaceAll]);
  s := Trim(s);

  l := TStringList.Create;
  l.Delimiter := ' ';
  l.StrictDelimiter := True;
  l.DelimitedText := s;

  for x := 0 to min(length(parts), l.Count) - 1 do
    parts[x] := l[x];

  l.Free;

end;

end.
