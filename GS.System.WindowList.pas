unit GS.System.WindowList;

interface

uses SysUtils;

Type

TGSWindow = Packed Record
  X,Y : integer;
  width, height : Uint32;
  Name : String;
  _PID : NativeInt;
  _Path : string;
  _Class : string;

  function ToString : String;
End;
TGSWindowArray =Array of TGSWindow;


procedure GetOSVisibleWindow; //Feed GSGlobalOSVisibleWindows

Var
  GSGlobalOSVisibleWindows : TGSWindowArray;

implementation

{$IFDEF WIN32}
uses GS.System.WindowList.Win;
{$ENDIF}

procedure GetOSVisibleWindow;
begin
  GSGlobalOSVisibleWindows := nil;
{$IFDEF WIN32}
  DoEnumWindows;
{$ELSE}
  {$MESSAGE 'GS.System.WindowList not implememnted for this OS !'}
{$ENDIF}
end;

{ TGSWindow }

function TGSWindow.ToString: String;
begin
  result := format('%s (%d,%d - %d,%d) - PID %d class %s Path %s',[Name,X,Y,width,height,_Pid, _Class,_Path]);
end;

Initialization

GSGlobalOSVisibleWindows := nil;

end.
