unit GS.Common.Monitoring.Default;

interface

uses SysUtils,
     Classes,
     Generics.Collections,
     GS.Common.Monitoring;

Type

//Abstract

TMonitoringItem = class
private
  flastticks : Int64;
  fOneSec : Uint32;
  fCall : Uint32;
public
  Ticks : Int64;
  TicksBetweenCall : Int64;
  DateTime : TDateTime;
  CallCount : Uint32;
  CallBySecond : Uint32;
  Constructor Create; virtual;
  procedure UpdateMetrics;
  function Category : TMonCat; virtual; abstract;
  function GetValueAsString : String; virtual; abstract;
end;

TMonitoringItemString = class(TMonitoringItem)
public
  Value : String;
  function GetValueAsString : String; override;
end;
TMonitoringItemNativeInt = class(TMonitoringItem)
public
  Value : NativeInt;
  function GetValueAsString : String; override;
end;

//To use.

  TMonitoringEnter = class(TMonitoringItemString)
  protected
    FCorrespondingExitEncountered : Boolean;
    FCorrespondingExit_Ticks : NativeInt;
    FCorrespondingExit_DateTime : TDateTime;
  public
    Constructor Create; Override;
    function Category : TMonCat; override;

    procedure SetExitEncountered;

    property CorrespondingExitEncountered : Boolean read FCorrespondingExitEncountered;
    property CorrespondingExit_Ticks : NativeInt read FCorrespondingExit_Ticks;
    property CorrespondingExit_DateTime : TDateTime read FCorrespondingExit_DateTime;
  end;
  TMonitoringExit = class(TMonitoringItemString)
    function Category : TMonCat; override;
  end;
  TMonitoringWatch = class(TMonitoringItemString)
    function Category : TMonCat; override;
  end;
  TMonitoringWatchNativeInt = class(TMonitoringItemNativeInt)
    function Category : TMonCat; override;
  end;

TMonitoringManager = Class(TObjectDictionary<string,TMonitoringItem>)
End;


TMonitoringImplementationDefault = Class(TMonitoringImplementation)
protected
  FActive : Boolean;
  FList : TMonitoringManager;
  FCurrentTickValue : int64;
public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Procedure watch(const code : String); Overload; Override;
  Procedure enter(const code : String);  Override;
  Procedure exit(const code : String);  Override;
  procedure watch(value : NativeInt; const code : String); Overload;  Override;

  function GetIsActive : boolean;  Override;
  procedure SetIsActive(Value : boolean);  Override;

  function ReportLogAsString : String; override;

  //Called automaticaly by Monitor manager.
  procedure SetSystemTick(value : Int64); Override;

  Property Active : boolean read GetIsActive Write SetIsActive;
End;

implementation

{ TMonitoringImplementationDefault }

constructor TMonitoringImplementationDefault.Create;
begin
  Inherited;
  FList := TMonitoringManager.Create([doOwnsValues]);
  FActive := true;
end;

destructor TMonitoringImplementationDefault.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TMonitoringImplementationDefault.GetIsActive: boolean;
begin
  result := Factive;
end;

function TMonitoringImplementationDefault.ReportLogAsString: String;
var v : TStringList;
    l : TPair<string,TMonitoringItem>;
    i : integer;
begin
  Result := '';
  v :=TStringList.Create;
  try
    i := 1;
    v.Add(Format('"%s","%s","%s","%s","%s","%s","%s","%s"',['n#','Ticks','CallCount','TicksBetweenCall','CallbySecond','Key','Class','ValueAsString']));
    for l in FList do
    begin
      v.Add(Format('%d,%d,%d,%d,%d,"%s",%s,%s',[i,l.Value.Ticks,l.Value.CallCount,l.Value.TicksBetweenCall,l.Value.CallBySecond, l.Key,l.Value.ClassName,l.Value.GetValueAsString]));
      inc(i);
    end;
    Result := Trim(v.Text);
  finally
    FreeAndNil(v);
  end;
end;

procedure TMonitoringImplementationDefault.SetIsActive(Value: boolean);
begin
  FActive := Value;
end;


procedure TMonitoringImplementationDefault.SetSystemTick(value: Int64);
begin
  FCurrentTickValue := value;
end;

procedure TMonitoringImplementationDefault.enter(const code: String);
var l : TMonitoringItem;
begin
  if Not FList.TryGetValue(code+'_enter',l) then
  begin
    l := TMonitoringEnter.Create;
    FList.Add(code+'_enter',l);
  end;

  l.Ticks := FCurrentTickValue;
  l.UpdateMetrics;
  if l is TMonitoringEnter then
    TMonitoringEnter(l).Value := code
  else
    raise Exception.Create('Monitoring Enter : found '+l.ClassName+' expected "TMonitoringEnter"');
end;

procedure TMonitoringImplementationDefault.exit(const code: String);
var l : TMonitoringItem;
begin
  if Not FList.TryGetValue(code+'_exit',l) then
  begin
    l := TMonitoringExit.Create;
    FList.Add(code+'_exit',l);
  end;

  l.Ticks := FCurrentTickValue;
  l.UpdateMetrics;
  if l is TMonitoringExit then
    TMonitoringExit(l).Value := code
  else
  if l is TMonitoringEnter then
    TMonitoringEnter(l).SetExitEncountered
  else
    raise Exception.Create('Monitoring Exit : found '+l.ClassName+' expected "TmonitoingEnter or TMonitoringExit"');
end;

procedure TMonitoringImplementationDefault.watch(const code: String);
var l : TMonitoringItem;
begin
  if Not FList.TryGetValue(code+'_watch',l) then
  begin
    l := TMonitoringWatch.Create;
    FList.Add(code+'_watch',l);
  end;

  l.Ticks := FCurrentTickValue;
  l.UpdateMetrics;
  if l is TMonitoringWatch then
    TMonitoringWatch(l).Value := code
  else
    raise Exception.Create('Monitoring Watch : found '+l.ClassName+' expected "TMonitoringWatch"');
end;

procedure TMonitoringImplementationDefault.watch(value: NativeInt;
  const code: String);
var l : TMonitoringItem;
begin
  if Not FList.TryGetValue(code+'_watchval',l) then
  begin
    l := TMonitoringWatchNativeInt.Create;
    FList.Add(code+'_watchval',l);
  end;

  l.Ticks := FCurrentTickValue;
  l.UpdateMetrics;
  if l is TMonitoringWatchNativeInt then
    TMonitoringWatchNativeInt(l).Value := Value
  else
    raise Exception.Create('Monitoring WatchNativeInt : found '+l.ClassName+' expected "TMonitoringWatchNativeInt"');
end;

{ TMonitoringWatchNativeInt }

function TMonitoringWatchNativeInt.Category: TMonCat;
begin
  result := TMonCat.mcWatchNativeInt;
end;

{ TMonitoringWatch }

function TMonitoringWatch.Category: TMonCat;
begin
  result := TMonCat.mcWatch;
end;

{ TMonitoringEnter }

function TMonitoringEnter.Category: TMonCat;
begin
  result := TMonCat.mcEnter;
end;

constructor TMonitoringEnter.Create;
begin
  Inherited;
  FCorrespondingExitEncountered:= false;;
  FCorrespondingExit_Ticks := 0;
  FCorrespondingExit_DateTime := 0;
end;

procedure TMonitoringEnter.SetExitEncountered;
begin
  FCorrespondingExitEncountered := true;
  FCorrespondingExit_Ticks := Ticks;
  FCorrespondingExit_DateTime := Now;
end;

{ TMonitoringExit }

function TMonitoringExit.Category: TMonCat;
begin
  result := TMonCat.mcExit;
end;

{ TMonitoringItemNativeInt }

function TMonitoringItemNativeInt.GetValueAsString: String;
begin
  result := IntToStr(Value);
end;

{ TMonitoringItemString }

function TMonitoringItemString.GetValueAsString: String;
begin
  result := Value;
end;

{ TMonitoringItem }

constructor TMonitoringItem.Create;
begin
  Inherited;
  Ticks := 0;
  flastticks := Ticks;
  fOneSec := 0;
  fCall := 0;
  TicksBetweenCall := 0;
  DateTime := Now;
  CallCount := 0;
  CallBySecond := 0;
end;

procedure TMonitoringItem.UpdateMetrics;
begin
  DateTime := Now;
  TicksBetweenCall := (Ticks-flastticks);
  flastticks := Ticks;
  fOneSec := fOneSec + TicksBetweenCall;
  Inc(CallCount);
  inc(fCall);
  if fOneSec>=1000 then
  begin
    CallBySecond := fCall;
    fOneSec := 0;
    fCall := 0;
  end;
end;

end.
