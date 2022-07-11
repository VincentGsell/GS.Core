//GS Log Facade.
//Implement whatever log system behing this facade, by inherited TLogImplementation object.
//This facade can manage several logger on the same time : Just add it and so on.
unit GS.Common.Log;

interface

uses SysUtils,
     Classes,
     syncObjs,
     Generics.Collections,
     GS.Common;

Type
TLogCat = (lcInfo, lcWarning,lcError, lcEnter, lcLeave, lcPerfStart, lcPerfStop, lcDev, lcDebug);
TLogCats = set of TLogCat;
Const
TLogCatStr : Array[TLogCat] of string = ('Info','Warning','Error','Enter','Leave','PerfStart','PerfStop','Dev','Debug');

Type
TLogItem = record
  ThreadID : NativeInt;
  Datetime : TDateTime;
  Tick : Uint64;
  cat : TLogCat;
  logNameSpace : string;
  logStr : String;

  function GetAsStringStdFormat : String;
end;

//Facade. Use this one.
TLog = class
Public
  Class procedure warning(const text : string; const NameSpace : string = '');
  Class procedure error(const text : string; const NameSpace : string = '');
  Class procedure info(const text : string; const NameSpace : string = '');
  Class procedure debug(const text : string; const NameSpace : string = '');
  Class procedure dev(const text : string; const NameSpace : string = '');

  class procedure enter(const text : string; const NameSpace : string = '');
  class procedure leave(const text : string; const NameSpace : string = '');

  class procedure perfStart(const id : string);
  class procedure perfStop(const id : string);
end;

//Implementation.

TLogImplementation = Class
private
protected
  const
  cstStandartFormat : String = '%s|Thread %d| [%s]%s';
  var
  FFormat : string;
  FActive : Boolean;
  FNameSpaces : TArray<String>;
  FFilter: TLogCats;
  function GetIsActive : boolean; virtual;
  procedure SetIsActive(Value : boolean); virtual;
public
  Constructor Create(const aLogFormatString : string = ''); Reintroduce; virtual;
  procedure writeLog(aLogItem : TLogItem); virtual; abstract;
  function matchNameSpace(aNameSpace : string) : Boolean;

  Property Active : boolean read GetIsActive Write SetIsActive;
  property NameSpaces : TArray<string> read FNameSpaces Write FNameSpaces;
  property LogCatFilters : TLogCats read FFilter write FFilter;
  property LogFormat : string read FFormat write FFormat;
End;

ILogManager = interface
end;

TLogManager = Class(TInterfacedObject,ILogManager)
private
protected
  FPreventLogRepeat: boolean;
  flogImpl : TObjectList<TLogImplementation>;
  FCS : TCriticalSection;
  FLastLog : string;
  FLastLogTime : TDateTime;
  FSameLogCount : integer;
  procedure LogDispatch(Const _logCat : TLogCat; const _LogText : String; const _LogNameSpace : string = '');
Public
  Constructor Create; virtual;
  Destructor Destroy; Override;

  procedure addImplementation(_logImpl : TLogImplementation; const LogNameSpaces : TArray<String> = []; const Filters : TLogCats = []);

  procedure warning(const text : string; const NameSpace : string = '');
  procedure error(const text : string; const NameSpace : string = '');
  procedure info(const text : string; const NameSpace : string = '');
  procedure debug(const text : string; const NameSpace : string = '');
  procedure dev(const text : string; const NameSpace : string = '');

  procedure enter(const text : string; const NameSpace : string = '');
  procedure leave(const text : string; const NameSpace : string = '');

  procedure perfStart(const id : string);
  procedure perfStop(const id : string);

  function GetImpl(index : integer) : TLogImplementation;
  function GetImplcount : integer;

  Property PreventLogRepeat : boolean read FPreventLogRepeat write FPreventLogRepeat;
End;

function LogManager : TLogManager;

implementation

var GS_Global_Log : ILogManager;

function LogManager : TLogManager;
begin
  if not assigned(GS_Global_Log) then
    GS_Global_Log := TLogManager.Create;
  result := GS_Global_Log as TLogManager;
end;

{ TLog }

class procedure TLog.debug(const text, NameSpace: string);
begin
  LogManager.debug(text,NameSpace);
end;

class procedure TLog.dev(const text, NameSpace: string);
begin
  LogManager.dev(text,NameSpace);
end;

class procedure TLog.enter(const text, NameSpace: string);
begin
  LogManager.enter(text,NameSpace);
end;

class procedure TLog.error(const text: string; const NameSpace : string);
begin
  LogManager.error(text,NameSpace);
end;

class procedure TLog.info(const text: string; const NameSpace : string);
begin
  LogManager.info(text,NameSpace);
end;

class procedure TLog.leave(const text, NameSpace: string);
begin
  LogManager.leave(text,NameSpace);
end;

class procedure TLog.perfStart(const id: string);
begin
  LogManager.perfStart(id);
end;

class procedure TLog.perfStop(const id: string);
begin
  LogManager.perfStop(id);
end;

class procedure TLog.warning(const text: string; const NameSpace : string);
begin
  LogManager.warning(text,NameSpace);
end;

{ TLogManager }

procedure TLogManager.addImplementation(_logImpl: TLogImplementation; const LogNameSpaces : TArray<String>; const Filters : TLogCats);
begin
  Assert(assigned(_logImpl));
  FCS.Enter;
  try
    if flogImpl.IndexOf(_logImpl)=-1 then begin
      _logImpl.NameSpaces := LogNameSpaces;
      _logImpl.LogCatFilters := Filters;
      flogImpl.Add(_logImpl);
    end;
  finally
    FCS.Leave;
  end;
end;

constructor TLogManager.Create;
begin
  inherited;
  FPreventLogRepeat := true;
  FCS := TCriticalSection.Create;
  flogImpl := TObjectList<TLogImplementation>.Create(True);
end;

procedure TLogManager.debug(const text, NameSpace: string);
begin
  LogDispatch(TLogCat.lcDebug,text,NameSpace);
end;

destructor TLogManager.Destroy;
begin
  FreeAndNil(flogImpl);
  FreeAndNil(FCS);
  inherited;
end;

procedure TLogManager.dev(const text, NameSpace: string);
begin
  LogDispatch(TLogCat.lcDev,text,NameSpace);
end;

procedure TLogManager.enter(const text, NameSpace: string);
begin
  LogDispatch(TLogCat.lcEnter,text,NameSpace);
end;

procedure TLogManager.error(const text: string; const NameSpace : string);
begin
  LogDispatch(TlogCat.lcError,Text,NameSpace);
end;

function TLogManager.GetImpl(index: integer): TLogImplementation;
begin
  assert(Index<flogImpl.Count);
  result := flogImpl[Index];
end;

function TLogManager.GetImplcount: integer;
begin
  result := flogImpl.Count;
end;

procedure TLogManager.info(const text: string; const NameSpace : string);
begin
  LogDispatch(TlogCat.lcInfo,Text,NameSpace);
end;

procedure TLogManager.leave(const text, NameSpace: string);
begin
  LogDispatch(TLogCat.lcLeave,text,NameSpace);
end;

procedure TLogManager.warning(const text: string; const NameSpace : string);
begin
  LogDispatch(TlogCat.lcWarning,Text,NameSpace);
end;

procedure TLogManager.LogDispatch(const _logCat: TLogCat;
  const _LogText: String; const _LogNameSpace : string);
var h : IGSStringList;
    i : integer;
    si : string;

    procedure internalLog;
    var l : TLogImplementation;
        lt : TLogItem;
    begin
      FCS.Enter; { TODO : do better : Bus or another lock free situation. }
      try
        for l in flogImpl do
          if l.Active then
            if (_LogNameSpace.Length=0) or (length(l.NameSpaces)=0) or (l.matchNameSpace(_LogNameSpace)) then
              if (l.LogCatFilters = []) or (_logCat in l.LogCatFilters) then begin //Soft filter.
                lt.ThreadID := TThread.CurrentThread.ThreadID;
                lt.Datetime := Now;
                lt.Tick := TThread.GetTickCount;
                lt.cat := _logCat;
                lt.logStr := si;
                lt.logNameSpace := _LogNameSpace;
                l.writeLog(lt);
              end;
      finally
        FCS.Leave;
      end;
    end;

begin
  //Avoid same log line in burst mode.
  if FPreventLogRepeat then begin
      if (SameStr(_LogText,FLastLog)) And (((Now - FLastLogTime)*24*3600)<5) then begin
        Inc(FSameLogcount);
        exit;
      end
      else begin
        if FSameLogCount>0 then begin
          si := 'WARNING : Log repeated same on '+IntToStr(FSameLogCount)+' line(s) has been ignored.';
          internalLog;
        end;
        FSameLogCount := 0;
      end;
  end;

  if pos(#13,_LogText)>0 then begin
    h := TGSStringList.Create;
    h.setText(_LogText);
    for i:= 0 to h.count-1 do begin
      si := h.lines(i);
      internallog;
    end;
  end
  else begin
    si := _LogText;
    internallog;
  end;

  //avoid burst.
  if FPreventLogRepeat then begin
    FLastLog := si;
    FLastLogTime := Now;
  end;
end;

procedure TLogManager.perfStart(const id: string);
begin
  LogDispatch(TLogCat.lcPerfStart,id);
end;

procedure TLogManager.perfStop(const id: string);
begin
  LogDispatch(TLogCat.lcPerfStop,id);
end;



{ TLogImplementation }


{ TLogImplementation }

constructor TLogImplementation.Create(const aLogFormatString: string);
begin
  Inherited Create;
  FActive := true;
  FFormat := cstStandartFormat;
  if aLogFormatString<>'' then
    FFormat := aLogFormatString;
end;

function TLogImplementation.GetIsActive: boolean;
begin
  result := FActive;
end;

function TLogImplementation.matchNameSpace(aNameSpace: string): Boolean;
var l : string;
begin
  result := false;
  for l in FNameSpaces do begin
    result := SameStr(l,aNameSpace);
    if result then
      break;
  end;
end;

procedure TLogImplementation.SetIsActive(Value: boolean);
begin
  FActive := Value;
end;


{ TLogItem }

function TLogItem.GetAsStringStdFormat: String;
begin
  //'%s|Thread %d| [%s]%s';
  result := Format(TLogImplementation.cstStandartFormat,[DateTimeToStr(Datetime),ThreadID,logNameSpace,logStr]);
end;

end.
