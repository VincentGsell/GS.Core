//GS Monitoring Facade.
//Implement whatever monitoring system behing this facade, by inherited TMonitoringImplementation object.
//This facade can manage several monittorr on the same time : Just add it and so on.
unit GS.Common.Monitoring;

interface

uses SysUtils,
     Classes,
     GS.System.CPU,
     Generics.Collections;

Type
TMonCat = (mcWatch,mcEnter,mcExit,mcWatchNativeInt);


TMonitoring = Class
  Class Procedure watch(const code : String); Overload;
  Class Procedure enter(const code : String);
  Class Procedure exit(const code : String);

  Class procedure watch(value : NativeInt; const code : String); Overload;

  Class Function Reports : String;
End;

//Implementation.

TMonitoringImplementation = Class
public
  Procedure watch(const code : String); Overload; Virtual; abstract;
  Procedure enter(const code : String); Virtual; abstract;
  Procedure exit(const code : String); Virtual; abstract;
  procedure watch(value : NativeInt; const code : String); Overload; Virtual; abstract;

  function GetIsActive : boolean; virtual; abstract;
  procedure SetIsActive(Value : boolean); virtual; abstract;

  procedure SetSystemTick(value : Int64); virtual; abstract;

  function ReportLogAsString : String; virtual; abstract;

  Property Active : boolean read GetIsActive Write SetIsActive;
End;

TMonitoringManager = Class
private
protected
  fMonImpl : TObjectList<TMonitoringImplementation>;
  FAutomaticAddDefaulMonitoringIfNoOtherImplementation: boolean;

  function GetMoncount: UInt32;
  function GetMonitoringInstance(Index: Uint32): TMonitoringImplementation;

  procedure MonDispatch(const _Cat : TMonCat; const _MonCode : String);
  procedure MonDispatchWatchNativeIntValue(const Value : NativeInt; const _MonCodeText : String);
Public
  Constructor Create; virtual;
  Destructor Destroy; Override;

  procedure addImplementation(_MonImpl : TMonitoringImplementation);

  Procedure watch(const code : String); Overload;
  Procedure enter(const code : String);
  Procedure exit(const code : String);
  procedure watch(value : NativeInt; const code : String); Overload;

  Property AutomaticAddDefaulMonitoringIfNoOtherImplementation : boolean read FAutomaticAddDefaulMonitoringIfNoOtherImplementation Write FAutomaticAddDefaulMonitoringIfNoOtherImplementation;

  Property MonitoringCount : UInt32 read GetMoncount;
  Property Montorings[Index : Uint32] : TMonitoringImplementation read GetMonitoringInstance;
End;

var GS_Global_Mon : TMonitoringManager;

implementation

Uses GS.Common.Monitoring.Default;

{ TMonitoring }

class procedure TMonitoring.enter(const code: String);
begin
  GS_Global_Mon.enter(code);
end;

class procedure TMonitoring.exit(const code: String);
begin
  GS_Global_Mon.exit(code);
end;

class function TMonitoring.Reports: String;
var l : TStringList;
    i : integer;
begin
  Result := '';
  l := TStringList.Create;
  try
    l.Add(Format('Report Monitoring (%d)',[GS_Global_Mon.MonitoringCount]));
    for I := 0 to GS_Global_Mon.MonitoringCount-1 do
    begin
      l.Add(Format(' Monitoring (%s)',[GS_Global_Mon.Montorings[i].ClassName]));
      l.Add(GS_Global_Mon.Montorings[i].ReportLogAsString);
    end;
    Result := trim(l.Text);
  finally
    FreeAndNil(l);
  end;
end;

class procedure TMonitoring.watch(value: NativeInt; const code: String);
begin
  GS_Global_Mon.watch(value,code);
end;

class procedure TMonitoring.watch(const code: String);
begin
  GS_Global_Mon.watch(code);
end;


{ TMonitoringManager }

procedure TMonitoringManager.addImplementation(
  _MonImpl: TMonitoringImplementation);
begin
  Assert(assigned(_MonImpl));
  if fMonImpl.IndexOf(_MonImpl)=-1 then
    fMonImpl.Add(_MonImpl);
end;

constructor TMonitoringManager.Create;
begin
  inherited;
  fMonImpl := TObjectList<TMonitoringImplementation>.Create;
  FAutomaticAddDefaulMonitoringIfNoOtherImplementation := True;
end;

destructor TMonitoringManager.Destroy;
begin
  FreeAndNil(fMonImpl);
  inherited;
end;

procedure TMonitoringManager.MonDispatch(const _Cat : TMonCat; const _MonCode: String);
var l : TMonitoringImplementation;
begin
  Assert(_Cat < TMonCat.mcWatchNativeInt,'Error : use MonDispatchWatchNativeIntValue');
  if fMonImpl.Count=0 then
    if FAutomaticAddDefaulMonitoringIfNoOtherImplementation then
      fMonImpl.Add(TMonitoringImplementationDefault.Create);

  for l in fMonImpl do
    if l.Active then
    begin
      l.SetSystemTick(gsGetTickCount);
      case _Cat of
        mcWatch: l.watch(_MonCode);
        mcEnter: l.enter(_MonCode);
        mcExit: l.exit(_MonCode);
      end;
    end;
end;


procedure TMonitoringManager.MonDispatchWatchNativeIntValue(const Value: NativeInt;
  const _MonCodeText: String);
var l : TMonitoringImplementation;
begin
  if fMonImpl.Count=0 then
    if FAutomaticAddDefaulMonitoringIfNoOtherImplementation then
      fMonImpl.Add(TMonitoringImplementationDefault.Create);
  for l in fMonImpl do
    if l.Active then
    begin
      l.SetSystemTick(gsGetTickCount);
      l.watch(Value,_MonCodeText);
    end;
end;

procedure TMonitoringManager.watch(const code: String);
begin
  MonDispatch(TMonCat.mcWatch,code);
end;

procedure TMonitoringManager.enter(const code: String);
begin
  MonDispatch(TMonCat.mcEnter,code);
end;

procedure TMonitoringManager.exit(const code: String);
begin
  MonDispatch(TMonCat.mcExit,code);
end;

function TMonitoringManager.GetMoncount: UInt32;
begin
  result := fMonImpl.Count;
end;

function TMonitoringManager.GetMonitoringInstance(
  Index: Uint32): TMonitoringImplementation;
begin
  Result := fMonImpl[Index];
end;

procedure TMonitoringManager.watch(value: NativeInt; const code: String);
begin
  MonDispatchWatchNativeIntValue(value,code);
end;

Initialization

GS_Global_Mon := TMonitoringManager.Create;

Finalization

FreeAndNil(GS_Global_Mon);

end.
