unit GS.System.WindowList.Win;

interface

uses
  System.SysUtils,
  Winapi.Windows,
  Winapi.PsAPI,
  GS.System.WindowList;


  procedure DoEnumWindows;

Implementation

function GetPathFromPID( const PID : cardinal ) : string;
var
  hProcess : THandle;
  path :     array [0 .. MAX_PATH - 1] of char;
begin
  hProcess := OpenProcess( PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, false, PID );
  if hProcess <> 0
  then
    try
      if GetModuleFileNameEx( hProcess, 0, path, MAX_PATH ) = 0
      then
        RaiseLastOSError;
      Result := path;
    finally
      CloseHandle( hProcess )
    end
  else
    RaiseLastOSError;
end;

function EnumWinProc( wHandle : hWnd; lparam : integer ) : Bool; stdcall;
Const
  MAX_TEXT = MAX_PATH;
var
  strText, strClass : array [0 .. MAX_TEXT] of char;
  strPath :           string;
  IsAppMainWin :      Boolean;
  ProcId :            cardinal;

  gs_wcount : integer;
  gs_lpr : TRect;
begin
  // Check if the window is a visible application main window.
  IsAppMainWin := IsWindowVisible( wHandle ) AND // Visible
    ( GetWindow( wHandle, GW_OWNER ) = 0 ) AND   // Not owned by other windows
    ( GetParent( wHandle ) = 0 ) AND             // Does not have any parent
    ( GetWindowLong( wHandle, GWL_EXSTYLE ) AND WS_EX_TOOLWINDOW = 0 ); // Not a tool window

  if IsAppMainWin
  then
    begin

      GetWindowText( wHandle, strText, MAX_TEXT );
      GetClassName( wHandle, strClass, MAX_TEXT );

      GetWindowThreadProcessID( wHandle, ProcId );
      GetWindowRect( wHandle, gs_lpr);

      try
        strPath := GetPathFromPID( ProcId );
      except
        strPath := '???';
      end;


      //WriteLn( ProcId, ' - ', strClass, ' - ', strText, ' - ', strPath );
      gs_wcount := length(GSGlobalOSVisibleWindows);
      SetLength(GSGlobalOSVisibleWindows,gs_wcount+1);
      GSGlobalOSVisibleWindows[gs_wcount].Name := strText;
      GSGlobalOSVisibleWindows[gs_wcount]._PID := ProcId;
      GSGlobalOSVisibleWindows[gs_wcount]._Path := strPath;
      GSGlobalOSVisibleWindows[gs_wcount]._Class := strClass;
      GSGlobalOSVisibleWindows[gs_wcount].X := gs_lpr.Left;
      GSGlobalOSVisibleWindows[gs_wcount].Y := gs_lpr.Top;
      GSGlobalOSVisibleWindows[gs_wcount].width := gs_lpr.Width;
      GSGlobalOSVisibleWindows[gs_wcount].height := gs_lpr.Height;
    end;

  Result := True;
end;

procedure DoEnumWindows;
var
  FirstWnd : cardinal;
begin
  EnumWindows( @EnumWinProc, cardinal( @FirstWnd ) );
end;


end.
