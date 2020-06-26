//GS Log Facade.
//Implement whatever log system behing this facade, by inherited TLogImplementation object.
//This facade can manage several logger on the same time : Just add it and so on.
unit GS.Common.Log;

interface

uses SysUtils,
     Classes,
     Generics.Collections;

Type
TLogCat = (lcInfo,lcWarning,lcError);

//Facade. Use this one.
TLog = class
Public
  Class procedure warning(const text : string);
  Class procedure error(const text : string);
  Class procedure info(const text : string);
end;

//Implementation.

TLogImplementation = Class
public
  procedure warning(const text : string); virtual; abstract;
  procedure error(const text : string); virtual; abstract;
  procedure info(const text : string); virtual; abstract;
  function GetIsActive : boolean; virtual; abstract;
  procedure SetIsActive(Value : boolean); virtual; abstract;

  Property Active : boolean read GetIsActive Write SetIsActive;
End;

TLogManager = Class
private
protected
  flogImpl : TObjectList<TLogImplementation>;
  FAutomaticAddDefaultLoggerIfNoOtherImplementation: boolean;

  procedure LogDispatch(Const _logCat : TLogCat; const _LogText : String);
Public
  Constructor Create; virtual;
  Destructor Destroy; Override;

  procedure addImplementation(_logImpl : TLogImplementation);

  procedure warning(const text : string);
  procedure error(const text : string);
  procedure info(const text : string);

  Property AutomaticAddDefaultLoggerIfNoOtherImplementation : boolean read FAutomaticAddDefaultLoggerIfNoOtherImplementation Write FAutomaticAddDefaultLoggerIfNoOtherImplementation;
End;

var GS_Global_Log : TLogManager;

implementation

uses GS.Common.Log.Default;

{ TLog }

class procedure TLog.error(const text: string);
begin
  GS_Global_Log.error(text);
end;

class procedure TLog.info(const text: string);
begin
  GS_Global_Log.error(text);
end;

class procedure TLog.warning(const text: string);
begin
  GS_Global_Log.error(text);
end;

{ TLogManager }

procedure TLogManager.addImplementation(_logImpl: TLogImplementation);
begin
  Assert(assigned(_logImpl));
  if flogImpl.IndexOf(_logImpl)=-1 then
    flogImpl.Add(_logImpl);
end;

constructor TLogManager.Create;
begin
  inherited;
  flogImpl := TObjectList<TLogImplementation>.Create;
  FAutomaticAddDefaultLoggerIfNoOtherImplementation := true;
end;

destructor TLogManager.Destroy;
begin
  FreeAndNil(flogImpl);
  inherited;
end;

procedure TLogManager.error(const text: string);
begin
  LogDispatch(TlogCat.lcError,Text);
end;

procedure TLogManager.info(const text: string);
begin
  LogDispatch(TlogCat.lcInfo,Text);
end;

procedure TLogManager.LogDispatch(const _logCat: TLogCat;
  const _LogText: String);
var l : TLogImplementation;
begin
  if flogImpl.Count=0 then
    if FAutomaticAddDefaultLoggerIfNoOtherImplementation then
      flogImpl.Add(TLogImplementationDefault.Create);

  for l in flogImpl do
    if l.Active then
      case _logCat of
        lcInfo: l.info(_LogText);
        lcWarning: l.warning(_LogText);
        lcError: l.error(_LogText);
      end;
end;

procedure TLogManager.warning(const text: string);
begin
  LogDispatch(TlogCat.lcWarning,Text);
end;


initialization

GS_Global_Log := TLogManager.Create;

finalization

FreeAndNil(GS_Global_Log);


end.
