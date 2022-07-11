unit GS.System.Processes.CommandLine;

interface

uses
  System.SysUtils
  ,System.Classes
  ,GS.Common
  ,GS.System.Processes
{$IFDEF LINUX}
  ,Posix.Base
  ,Posix.Fcntl
{$ELSE}
{$IFDEF MSWINDOWS}
  ,Windows
{$ENDIF}
{$ELSE}
{$MESSAGE 'Command line Need platform impl.'}
{$ENDIF}
  ;

type

TGSCommandLine = class(TInterfacedObject, IGSProcessLauncher)
  function processLaunch(aCommand : string;out aReturnCode : integer) : string;
end;

{$IFDEF LINUX}
  TStreamHandle = pointer;

  TLinuxUtils = class
  public
    class function RunCommandLine(ACommand : string) : TStringList;overload;
    class function RunCommandLine(Acommand : string; Return : TProc<String>) : boolean; overload;
    class function findParameter(AParameter : string) : boolean;
  end;

  function popen(const command: MarshaledAString; const _type: MarshaledAString): TStreamHandle; cdecl; external libc name _PU + 'popen';
  function pclose(filehandle: TStreamHandle): int32; cdecl; external libc name _PU + 'pclose';
  function fgets(buffer: pointer; size: int32; Stream: TStreamHAndle): pointer; cdecl; external libc name _PU + 'fgets';
{$ENDIF}

{$IFDEF MSWINDOWS}

 TProcessWindowsUtils = class
 public
   class function RunCommandLine(ACommand : string) : TStringList;overload;
 end;

{$ENDIF}



implementation

{$IFDEF LINUX}

class function TLinuxUtils.RunCommandLine(ACommand : string) : TStringList;
var
  Handle: TStreamHandle;
  Data: array[0..511] of uint8;
  M : TMarshaller;

begin
  Result := TStringList.Create;
  try
    Handle := popen(M.AsAnsi(PWideChar(ACommand)).ToPointer,'r');
    try
      while fgets(@data[0],Sizeof(Data),Handle)<>nil do begin
        Result.Add(Copy(UTF8ToString(@Data[0]),1,UTF8ToString(@Data[0]).Length -1));//,sizeof(Data)));
      end;
    finally
      pclose(Handle);
    end;
  except
    on E: Exception do
      Result.Add(E.ClassName + ': ' + E.Message);
  end;
end;

class function TLinuxUtils.RunCommandLine(Acommand : string; Return : TProc<string>) : boolean;
var
  Handle: TStreamHandle;
  Data: array[0..511] of uint8;
  M : TMarshaller;

begin
  Result := false;
  try
    Handle := popen(M.AsAnsi(PWideChar(ACommand)).ToPointer,'r');
    try
      while fgets(@data[0],Sizeof(Data),Handle)<>nil do begin
        Return(Copy(UTF8ToString(@Data[0]),1,UTF8ToString(@Data[0]).Length -1));//,sizeof(Data)));
      end;
    finally
      pclose(Handle);
    end;
  except
    on E: Exception do
      Return(E.ClassName + ': ' + E.Message);
  end;
end;

class function TLinuxUtils.findParameter(AParameter : string) : boolean;
var
  I : Integer;
begin
  Result := false;
  for I := 0 to Pred(ParamCount) do
  begin
    Result := AParameter.ToUpper = ParamStr(i).ToUpper;
    if Result then
      Break;
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}

procedure CaptureConsoleOutput(const ACommand, AParameters: String; AStringList: TStrings);
 const
   CReadBuffer = 2400;
 var
   saSecurity: TSecurityAttributes;
   hRead: THandle;
   hWrite: THandle;
   suiStartup: TStartupInfo;
   piProcess: TProcessInformation;
   pBuffer: array[0..CReadBuffer] of AnsiChar;
   dRead: DWord;
   dRunning: DWord;
 begin
   saSecurity.nLength := SizeOf(TSecurityAttributes);
   saSecurity.bInheritHandle := True;
   saSecurity.lpSecurityDescriptor := nil;

   if CreatePipe(hRead, hWrite, @saSecurity, 0) then
   begin
     FillChar(suiStartup, SizeOf(TStartupInfo), #0);
     suiStartup.cb := SizeOf(TStartupInfo);
     suiStartup.hStdInput := hRead;
     suiStartup.hStdOutput := hWrite;
     suiStartup.hStdError := hWrite;
     suiStartup.dwFlags := STARTF_USESTDHANDLES or STARTF_USESHOWWINDOW;
     suiStartup.wShowWindow := SW_HIDE;

     if CreateProcess(nil, PChar(ACommand + ' ' + AParameters), @saSecurity,
       @saSecurity, True, NORMAL_PRIORITY_CLASS, nil, nil, suiStartup, piProcess)
       then
     begin
       repeat
         dRunning  := WaitForSingleObject(piProcess.hProcess, 100);
         repeat
           dRead := 0;
           ReadFile(hRead, pBuffer[0], CReadBuffer, dRead, nil);
           pBuffer[dRead] := #0;
           OemToAnsi(pBuffer, pBuffer);
           AStringList.Add(String(pBuffer));
         until (dRead < CReadBuffer);
       until (dRunning <> WAIT_TIMEOUT);
       CloseHandle(piProcess.hProcess);
       CloseHandle(piProcess.hThread);
     end;

     CloseHandle(hRead);
     CloseHandle(hWrite);
   end;
end;

 class function  TProcessWindowsUtils.RunCommandLine(ACommand : string) : TStringList;
 begin
   result := TStringList.Create;
   CaptureConsoleOutput(ACommand,'',Result);
 end;

{$ENDIF}

{ TGSCommandLine }

function TGSCommandLine.processLaunch(aCommand: string; out aReturnCode : integer): string;
var l : TStringList;
begin
{$IFDEF LINUX}
  l := TLinuxUtils.RunCommandLine(acommand);
  try
    result := l.Text;
    aReturnCode := 0; // :-/
  finally
    FreeAndNil(l);
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
  l := TProcessWindowsUtils.RunCommandLine(acommand);
  try
    result := l.Text;
    aReturnCode := 0; // :-/
  finally
    FreeAndNil(l);
  end;
{$ENDIF}
end;

end.
