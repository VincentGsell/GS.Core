//In memory simple log.
unit GS.Common.Log.Default;

interface

uses Classes,
     SysUtils,
     SyncObjs,
     DateUtils,
     {$IFDEF MSWINDOWS}
     {$IFDEF DCC}
     Winapi.Windows, //OutputDebugString.
     {$ENDIF}
     {$ENDIF}
     Gs.System.Files,
     Gs.Common,
     Gs.Common.Log;

Type
TLogItems = class
private
  Const
    CST_GROW = 1000; //Array grow size when no more room.
  Var
  Findex : Uint32;
  Fbuffer : Array of TLogItem;
  FCriticalSect : TCriticalSection; //for a full blown parallele log capable system, please see bus or network oriented implementation.
public
  procedure Add(aLogItem : TLogItem);

  Constructor Create; virtual;
  Destructor Destroy; Override;
end;

///
/// Keep log in memory.
///
TLogImplementationInMemory = class(TLogImplementation)
protected
  FLogItems : TLogItems;
public
  Constructor Create(const aLogFormatString : string = ''); Override;
  Destructor Destroy; Override;
  procedure writeLog(aLogItem : TLogItem); Override;
end;

TOnLogEventHook = procedure(Sender : TObject; Item :TLogItem) Of Object;
///
/// For Gui app, to get log visual.
///
TLogImplementationEventHook = class(TLogImplementation)
protected
  FOnLogHook : TOnLogEventHook;
public
  procedure writeLog(aLogItem : TLogItem); Override;
  property OnLog : TOnLogEventHook read FOnLogHook Write FOnLogHook;

  Constructor Create(aHook : TOnLogEventHook); Reintroduce;
end;

///
/// writeln output.
///
TLogImplementationConsole = class(TLogImplementation)
protected
public
  procedure writeLog(aLogItem : TLogItem); Override;
end;

///
///  Standart file.
///
TLogImplementationSingleFile = class(TLogImplementationConsole)
protected
  FOriginalFileName, FOriginalFileDir : String;
  FCurrentFileName : string;
  FOnlyOneLogFile : Boolean;
  FSW : TStreamWriter;
  FCS : TCriticalSection;

  function InternalGetFileName : string;
  procedure InternalFileAccessUpdate;
public
  Constructor Create(const aDir : string=''; const afileName : string =''; const aLogFormatString : string = ''; const OnlyOneLogFile : boolean = false); Reintroduce;
  Destructor Destroy; Override;
  procedure writeLog(aLogItem : TLogItem); Override;
end;

{$IFDEF MSWINDOWS}
{$IFDEF DCC}
//Delphi IDE
TlogImplementationDelphiDebugString = class(TlogImplementation)
public
  procedure writeLog(aLogItem : TLogItem); Override;
end;
{$ENDIF}
{$ENDIF}

implementation

{ TLogItems }

procedure TLogItems.Add(aLogItem : TLogItem);
var lb,li : Int64;
begin
  FCriticalSect.Enter;
  try
    lb := Length(Fbuffer);
    if Not(Findex < lb-1) then
      SetLength(FBuffer,length(Fbuffer)+CST_GROW);
    li := Findex;
    Inc(FIndex);
  finally
    FCriticalSect.Leave;
  end;
  Fbuffer[li] := aLogItem;
end;

constructor TLogItems.Create;
begin
  FCriticalSect := TCriticalSection.Create;
  Findex := 0;
end;

destructor TLogItems.Destroy;
begin
  Fbuffer := nil;
  FreeAndNil(FCriticalSect);
  inherited;
end;


{ TLogImplementationDefault }


procedure TLogImplementationInMemory.writeLog(aLogItem: TLogItem);
begin
  FLogItems.Add(aLogItem);
end;

constructor TLogImplementationInMemory.Create(const aLogFormatString : string);
begin
  inherited;
  fActive := true;
  FLogItems := TLogItems.Create;
end;

destructor TLogImplementationInMemory.Destroy;
begin
  FreeAndNil(FLogItems);
  inherited;
end;


{ TLogImplementationEventHook }

constructor TLogImplementationEventHook.Create(aHook: TOnLogEventHook);
begin
  inherited Create;
  if Assigned(aHook) then
    FOnLogHook := aHook;
end;

procedure TLogImplementationEventHook.writeLog(aLogItem: TLogItem);
begin
  if Assigned(FOnLogHook) then
    FOnLogHook(Self,alogItem);
end;

{ TLogImplementationConsole }

procedure TLogImplementationConsole.writeLog(aLogItem: TLogItem);
var l : String;
begin
  l := format(FFormat,[DateTimeToStr(aLogItem.Datetime),aLogItem.ThreadID,TLogCatStr[aLogItem.cat],aLogItem.logStr]);
  writeln(l);
end;

{ TLogImplementationSingleFile }

constructor TLogImplementationSingleFile.Create(const aDir : string; const afileName: string; const aLogFormatString: string; const OnlyOneLogFile : boolean);
begin
  inherited Create(aLogFormatString);
  FOnlyOneLogFile := OnlyOneLogFile;
  FCS := TCriticalSection.Create;
  if (adir.Trim.Length>0) and not TGSFileSys.DirectoryExists(aDir) then
    TGSFileSys.DirectoryCreate(aDir);
  FOriginalFileName := afileName;
  FOriginalFileDir := aDir;
  FCurrentFileName := '';
end;

destructor TLogImplementationSingleFile.Destroy;
begin
  if Assigned(FSW) then begin
    FSW.Flush;
    FreeAndNil(FSW);
    FreeAndNil(FCS);
  end;
  inherited;
end;

procedure TLogImplementationSingleFile.InternalFileAccessUpdate;
var
  lFileAccessMode: Word;
  lFileStream : TFileStream;
begin
  if FCurrentFileName.Trim = '' then
    FCurrentFileName := InternalGetFileName;

  lFileAccessMode := fmOpenWrite or fmShareDenyNone;
  if Assigned(FSW) then begin
    FSW.Close;
    FreeAndNil(FSW);
  end;
  if not TGSFileSys.FileExists(FCurrentFileName) then
    lFileAccessMode := lFileAccessMode or fmCreate;
  lFileStream := TFileStream.Create(FCurrentFileName, lFileAccessMode);
  lFileStream.Seek(0, TSeekOrigin.soEnd);
  FSW := TStreamWriter.Create(lFileStream, TEncoding.UTF8);
  FSW.AutoFlush := true;
  FSW.OwnStream;
end;

function TLogImplementationSingleFile.InternalGetFileName: string;
begin
  if FOnlyOneLogFile then
    result := TGSFileSys.PathCombine(FOriginalFileDir,FOriginalFileName)
  else
    result := TGSFileSys.PathCombine(FOriginalFileDir,FormatDateTime('YYYYMMDD_',Now)+FOriginalFileName);
end;

procedure TLogImplementationSingleFile.writeLog(aLogItem: TLogItem);
var l : String;
begin
  FCS.Enter;
  try
    if FCurrentFileName <> InternalGetFileName then begin
      FCurrentFileName := InternalGetFileName;
      if Assigned(FSW) then begin
        l := format(FFormat,[DateTimeToStr(aLogItem.Datetime),aLogItem.ThreadID,TLogCatStr[TLogCat.lcInfo],'log will continue in '+FCurrentFileName]);
        FSW.WriteLine(l);
        FSW.Flush;
      end;
      InternalFileAccessUpdate;
    end;
    l := format(FFormat,[DateTimeToStr(aLogItem.Datetime),aLogItem.ThreadID,TLogCatStr[aLogItem.cat],aLogItem.logStr]);
    FSW.WriteLine(l);
    FSW.Flush;
  finally
    FCS.Leave;
  end;
end;

{$IFDEF MSWINDOWS}
{$IFDEF DCC}

{ TlogImplementationDelphiDebugString }

procedure TlogImplementationDelphiDebugString.writeLog(aLogItem: TLogItem);
var l : String;
begin
  inherited;
  l := aLogItem.GetAsStringStdFormat;
  OutputDebugString(PChar(l));
end;

{$ENDIF}
{$ENDIF}

end.

