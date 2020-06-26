//In memory simple log.
unit GS.Common.Log.Default;

interface

uses Classes,
     SysUtils,
     Gs.Common.Log;

Type
TLogItem = record
  ThreadID : NativeInt;
  Datetime : TDateTime;
  Tick : Uint64;
  cat : TLogCat;
  logStr : String;
end;

TLogItems = class
private
  Const
    CST_GROW = 1000; //Array grow size when no more room.
  Var
  Findex : Uint32;
public
  buffer : Array of TLogItem;
  procedure Add(Const _logCat : TLogCat; const _logStr : String);

  Constructor Create; virtual;
  Destructor Destroy; Override;
end;

TLogImplementationDefault = class(TLogImplementation)
protected
  fLogItems : TLogItems;
  fActive : boolean;
public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  procedure warning(const text : string); Override;
  procedure error(const text : string); Override;
  procedure info(const text : string); Override;
  function GetIsActive : boolean; Override;
  procedure SetIsActive(Value : boolean); Override;
end;

implementation

{ TLogItems }

procedure TLogItems.Add(Const _logCat : TLogCat; const _logStr : String);
var lb : Int64;
begin
  lb := Length(buffer);
  if Not(Findex < lb-1) then
    SetLength(Buffer,length(buffer)+CST_GROW);
  Add(_logCat,_logStr);
end;

constructor TLogItems.Create;
begin
  Findex := 0;
  Add(TLogCat.lcInfo,'Log start...');
end;

destructor TLogItems.Destroy;
begin
  Add(TLogCat.lcInfo,'Log end...');
  buffer := nil;
  inherited;
end;


{ TLogImplementationDefault }

function TLogImplementationDefault.GetIsActive: boolean;
begin
  result := FActive;
end;

procedure TLogImplementationDefault.SetIsActive(Value: boolean);
begin
  fActive := Value;
end;

constructor TLogImplementationDefault.Create;
begin
  inherited;
  fLogItems := TLogItems.Create;
  fActive := true;
end;

destructor TLogImplementationDefault.Destroy;
begin
  FreeAndNil(fLogItems);
  inherited;
end;

procedure TLogImplementationDefault.error(const text: string);
begin
  fLogItems.Add(TLogCat.lcError,Text);
end;

procedure TLogImplementationDefault.info(const text: string);
begin
  fLogItems.Add(TLogCat.lcInfo,Text);
end;

procedure TLogImplementationDefault.warning(const text: string);
begin
  fLogItems.Add(TLogCat.lcWarning,Text);
end;

initialization

///
///  Normaly, you have here the fellowing code.
///  -----------------------
///  GS_Global_Log.addImplementation(TLogImplementationDefault.Create);
///  -----------------------
///
///  But the manager auto create this instance, if there are no other log implementation.
/// (see GS_Global_Log.AutomaticAddDefaultLoggerIfNoOtherImplementation property)
///  If you do your own TLogImplementation descandant, do not forget to add it in
///  initialization part ! (In this case, the only loggers will be yours, by default.)
///
///
end.
