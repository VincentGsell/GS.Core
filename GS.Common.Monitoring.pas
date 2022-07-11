//GS Monitoring Facade.
//Implement whatever monitoring system behing this facade, by inherited TMonitoringImplementation object.
//This facade can manage several monittorr on the same time : Just add it and so on.
unit GS.Common.Monitoring;

interface

uses SysUtils,
     Classes,
     syncObjs,
     GS.Common,
     GS.JSON,
     Generics.Collections;

Type
TMonCat = (mcWatch,mcEnter,mcLeave);
Const
CstMonCat : Array[TMonCat] of string = ('watch','enter','leave');
Type

TMonitoringExtDataItem = record
D : TDateTime;
V : Double;
end;

IGSMonitoringExtensionData = interface
  function getCount : Uint32;
  function getData : TArray<TMonitoringExtDataItem>;
end;

TMonitoringItem = Record
private
  flastticks : Int64;
  fOneSec : Uint32;
  fCall : Uint32;
public
  id : String;
  monCat : TMonCat;
  StringValue : string;
  doubleValue : double;
  intValue : int64;
  uIntValue : uInt64;
  Ticks : Int64;
  TicksBetweenCall : Int64;
  CreateDateTime : TDateTime;
  DateTime : TDateTime;
  CallCount : Uint32;
  CallBySecond : Uint32;
  ExtensionData : IGSMonitoringExtensionData;
  procedure init;
  procedure UpdateMetrics;
  function AsString : String;
end;
PMonitoringItem = ^TMonitoringItem;

IGSMonitoring = interface
  procedure writeMonStr(monCat : TMonCat; id : String; value : String);
  procedure writeMonUInt(monCat : TMonCat; id : String; value : UInt64);
  procedure writeMonInt(monCat : TMonCat; id : String; value : Int64);
  procedure writeMonDouble(monCat : TMonCat; id : String; value : Double);
end;

TGSCustomMonitoringImplementation = Class(TInterfacedObject, IGSMonitoring)
protected
  FActive : Boolean;
public
  //IGSMonitoring
  procedure writeMonStr(monCat : TMonCat; id : String; value : String); virtual; abstract;
  procedure writeMonUInt(monCat : TMonCat; id : String; value : UInt64); virtual; abstract;
  procedure writeMonInt(monCat : TMonCat; id : String; value : Int64); virtual; abstract;
  procedure writeMonDouble(monCat : TMonCat; id : String; value : Double); virtual; abstract;

  //Enhancement
  function lookForId(id : string; out _monitoredItem : PMonitoringItem) : Boolean; virtual; abstract;
  function Count : integer; virtual; abstract;
  function QueryIds(Const FromDateTime : TDateTime = 0) : TArray<String>; virtual; abstract;

  //..
  function GetIsActive : boolean; virtual;
  procedure SetIsActive(Value : boolean); virtual;
  Property Active : boolean read GetIsActive Write SetIsActive;
End;

IGSMonitoringManager = Interface
  procedure addImplementation(_MonImpl : TGSCustomMonitoringImplementation);
  procedure watch(id : string; value : string);
  procedure enter(id : string);
  procedure leave(id : string);
  function GetImpl(index : integer) : TGSCustomMonitoringImplementation;
  function GetImplcount : integer;
End;

TGSMonManager = Class(TInterfacedObject,IGSMonitoringManager)
private
protected
  fMonImpl : TObjectList<TGSCustomMonitoringImplementation>;
  FCS : TCriticalSection;
  procedure MonDispatch(Const _MonCat : TMonCat; const _Id : String; const _value : string); overload;
  procedure MonDispatch(Const _MonCat : TMonCat; const _Id : String; const _value : double); overload;
  //TODO : int64
Public
  Constructor Create; virtual;
  Destructor Destroy; Override;

  procedure addImplementation(_MonImpl : TGSCustomMonitoringImplementation);

  procedure watch(id : string; value : string);
  procedure enter(id : string);
  procedure leave(id : string);

  function GetImpl(index : integer) : TGSCustomMonitoringImplementation;
  function GetImplcount : integer;
End;

//singleton. If needed. Not a mandatory.
function MonManager : IGSMonitoringManager;

implementation

var GS_Global_Mon : IGSMonitoringManager;

function MonManager : IGSMonitoringManager;
begin
  if Not Assigned(GS_Global_Mon) then
    GS_Global_Mon := TGSMonManager.Create;
  result := GS_Global_Mon;
end;

{ TMonitoringItem }

function TMonitoringItem.AsString: String;
var l : IGSJsonObject;
begin
  l := TGSJsonImpl.Create;
  l.Put('id',id);
  l.Put('category',CstMonCat[monCat]);
  l.Put('doubleValue',doubleValue);
  l.Put('intValue',intValue);
  l.Put('uIntValue',uIntValue);
  l.Put('StringValue',StringValue);
  l.Put('Ticks',Ticks);
  l.Put('TicksBetweenCall',TicksBetweenCall);
  l.Put('CreateDateTime',TGSJSonTool.DataTimeToJSONStr(CreateDateTime));
  l.Put('DateTime',TGSJSonTool.DataTimeToJSONStr(DateTime));
  l.Put('CallCount',CallCount);
  l.Put('CallBySecond',CallBySecond);
  l.Put('ExtendionDataAssigned',Assigned(ExtensionData));
  if Assigned(ExtensionData) then
    l.Put('ExtensionDataClassName',(ExtensionData as TObject).ClassName);
  result := l.Stringify;
end;

procedure TMonitoringItem.init;
begin
  flastticks :=0;
  fOneSec :=0;
  fCall :=0;
  id:='';
  monCat:=TMonCat.mcWatch;
  StringValue :='';
  doubleValue :=0;
  intValue :=0;
  uIntValue :=0;
  Ticks :=0;
  TicksBetweenCall :=0;
  CreateDateTime :=0;
  DateTime :=0;
  CallCount :=0;
  CallBySecond :=0;
end;

procedure TMonitoringItem.UpdateMetrics;
begin
  if CreateDateTime=0 then
    CreateDateTime := Now;
  DateTime := Now;
  TicksBetweenCall := (Ticks-flastticks);
  flastticks := Ticks;
  Ticks := TThread.GetTickCount;
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


{ TGSMonManager }

procedure TGSMonManager.addImplementation(
  _MonImpl: TGSCustomMonitoringImplementation);
begin
  Assert(assigned(_MonImpl));
  FCS.Enter;
  try
    if fMonImpl.IndexOf(_MonImpl)=-1 then begin
      fMonImpl.Add(_MonImpl);
    end;
  finally
    FCS.Leave;
  end;
end;

constructor TGSMonManager.Create;
begin
  inherited;
  FCS := TCriticalSection.Create;
  fMonImpl := TObjectList<TGSCustomMonitoringImplementation>.Create(True);
end;

destructor TGSMonManager.Destroy;
begin
  FreeAndNil(fMonImpl);
  FreeAndNil(FCS);
  inherited;
end;

function TGSMonManager.GetImpl(
  index: integer): TGSCustomMonitoringImplementation;
begin
  result := fMonImpl[index];
end;

function TGSMonManager.GetImplcount: integer;
begin
  result:= fMonImpl.Count;
end;

procedure TGSMonManager.enter(id: string);
begin
  MonDispatch(TMonCat.mcEnter,id,'');
end;

procedure TGSMonManager.leave(id: string);
begin
  MonDispatch(TMonCat.mcLeave,id,'');
end;

procedure TGSMonManager.watch(id, value: string);
begin
  MonDispatch(TMonCat.mcWatch,id,value);
end;


procedure TGSMonManager.MonDispatch(const _MonCat: TMonCat; const _Id,
  _value: string);
var l : TGSCustomMonitoringImplementation;
    h : IGSStringList;
    i : integer;
begin
  h := TGSStringList.Create;
  if _value.Trim = '' then
    h.setText(' ')
  else
    h.setText(_value);

  for i:= 0 to h.count-1 do
    for l in fMonImpl do
      if l.Active then
        l.writeMonStr(_MonCat,_id,h.lines(i));
end;

procedure TGSMonManager.MonDispatch(const _MonCat: TMonCat; const _Id: String;
  const _value: double);
var l : TGSCustomMonitoringImplementation;
    i : integer;
begin
  for l in fMonImpl do
    if l.Active then
      l.writeMonDouble(_MonCat,_id,_value);
end;


{ TGSCustomMonitoringImplementation }

function TGSCustomMonitoringImplementation.GetIsActive: boolean;
begin
  result := FActive;
end;

procedure TGSCustomMonitoringImplementation.SetIsActive(Value: boolean);
begin
  FActive := Value;
end;

end.
