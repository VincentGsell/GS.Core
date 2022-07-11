///
///
/// Native/Naive in mem only monitoring.
/// Warning : Mechanism only. Not threadsafe.
///
///

unit GS.Common.Monitoring.Default;

interface

uses SysUtils,
     Classes,
     Generics.Collections,
     GS.Common.Monitoring;
Type


TMonItems = class
private
  Const
    CST_GROW = 1000; //Array grow size when no more room.
  Var
  Findex : Uint32;
  Fbuffer : Array of TMonitoringItem;
public
  procedure Add(aMonItem : TMonitoringItem);
  function Get(index : integer) : PMonitoringItem;
  function count : Integer;

  Constructor Create; virtual;
  Destructor Destroy; Override;
end;


///
/// Keep Mon in memory (one by id).
///
TGSMonImplementationInMemory = class(TGSCustomMonitoringImplementation)
protected
  FMonItems : TMonItems;
public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  procedure writeMonStr(monCat : TMonCat; id : String; value : String); Override;
  procedure writeMonUInt(monCat : TMonCat; id : String; value : UInt64); Override;
  procedure writeMonInt(monCat : TMonCat; id : String; value : Int64); Override;
  procedure writeMonDouble(monCat : TMonCat; id : String; value : Double); Override;

  function lookForId(id : string; out _monitoredItem : PMonitoringItem) : Boolean; Override;
  function Count : integer; Override;
  function QueryIds(Const FromDateTime : TDateTime = 0) : TArray<String>; Override;
end;

///
///   Idea : Keep Mon in memory : Log (expl : Keep proc value in each sec. , temp value etc etc.
///  TODO : Specialiued datat entry such as : One value table behinf a TMonItem. (1 TmonItem, and a table of value behind. [[TIMESTAMP,VALUE][TIMESTAMP,VALUE]...]
///

///
/// other impl : Put the same stuff in a file.
///

implementation


{ TMonItems }

procedure TMonItems.Add(aMonItem : TMonitoringItem);
var lb,li : Int64;
begin
  lb := Length(Fbuffer);
  if Not(Findex < lb-1) then
    SetLength(FBuffer,length(Fbuffer)+CST_GROW);
  li := Findex;
  Inc(FIndex);
  Fbuffer[li] := aMonItem;
end;

function TMonItems.count: Integer;
begin
  result := length(Fbuffer);
end;

constructor TMonItems.Create;
begin
  Findex := 0;
end;

destructor TMonItems.Destroy;
begin
  Fbuffer := nil;
  inherited;
end;



function TMonItems.Get(index: integer): PMonitoringItem;
begin
  result := @Fbuffer[index];
end;

{ TGSMonImplementationInMemory }

function TGSMonImplementationInMemory.Count: integer;
begin
  result := FMonItems.count;
end;

constructor TGSMonImplementationInMemory.Create;
begin
  inherited;
  fActive := true;
  FMonItems := TMonItems.Create;
end;

destructor TGSMonImplementationInMemory.Destroy;
begin
  FreeAndNil(FMonItems);
  inherited;
end;

function TGSMonImplementationInMemory.lookForId(id : string; out _monitoredItem : PMonitoringItem) : Boolean;
var i : integer;
begin
  result := false;
  for i := FMonItems.count-1 downto 0 do
    if FMonItems.Get(i).id = id then begin
      _monitoredItem := FMonItems.Get(i);
      result := true;
      break
    end;
end;

function TGSMonImplementationInMemory.QueryIds(
  const FromDateTime: TDateTime): TArray<String>;
var i : integer;
begin
  setLength(Result,FMonItems.count);
  for i := 0 to FMonItems.count-1 do
    result[i] := FMonItems.Get(i).id;
end;

procedure TGSMonImplementationInMemory.writeMonDouble(monCat: TMonCat; id: String;
  value: Double);
var l : PMonitoringItem;
    lp : TMonitoringItem;
begin
  if lookForId(id,l) then begin
    l.doubleValue := value;
    l.UpdateMetrics;
    exit;
  end;
  lp.init;
  lp.id := id;
  lp.monCat := monCat;
  lp.doubleValue := value;
  lp.UpdateMetrics;
  FMonItems.Add(lp);
end;

procedure TGSMonImplementationInMemory.writeMonInt(monCat: TMonCat; id: String;
  value: Int64);
var l : PMonitoringItem;
    lp : TMonitoringItem;
begin
  if lookForId(id,l) then begin
    l.intValue := value;
    l.UpdateMetrics;
    exit;
  end;
  lp.init;
  lp.id := id;
  lp.monCat := monCat;
  lp.intValue := value;
  lp.UpdateMetrics;
  FMonItems.Add(lp);
end;

procedure TGSMonImplementationInMemory.writeMonStr(monCat: TMonCat; id,
  value: String);
var l : PMonitoringItem;
    lp : TMonitoringItem;
begin
  if lookForId(id,l) then begin
    l.StringValue := value;
    l.UpdateMetrics;
    exit;
  end;
  lp.init;
  lp.id := id;
  lp.monCat := monCat;
  lp.StringValue := value;
  lp.UpdateMetrics;
  FMonItems.Add(lp);
end;

procedure TGSMonImplementationInMemory.writeMonUInt(monCat: TMonCat; id: String;
  value: UInt64);
var l : PMonitoringItem;
    lp : TMonitoringItem;
begin
  if lookForId(id,l) then begin
    l.uIntValue := value;
    l.UpdateMetrics;
    exit;
  end;
  lp.init;
  lp.id := id;
  lp.monCat := monCat;
  lp.uIntValue := value;
  lp.UpdateMetrics;
  FMonItems.Add(lp);
end;

end.
