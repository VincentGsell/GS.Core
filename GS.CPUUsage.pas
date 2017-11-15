///-------------------------------------------------------------------------------
/// Title      : GS.CPUUsage
/// Short Desc : Got CPU Usage cros platform
/// Source     : https://github.com/VincentGsell
/// Aim        : - Win and Linux Delphi/FPC harmonized CPU access.
///-------------------------------------------------------------------------------
unit GS.CPUUsage;

interface

{$IFDEF FPC}
  {$mode Delphi}
Uses SysUtils, Classes, SyncObjs

  {$IFDEF WINDOWS}
    ,Windows,
    Messages
  {$ENDIF}
  ;

{$ELSE}
Uses System.SysUtils, System.Classes, System.SyncObjs;
{$ENDIF}
Type

//TODO : Dig into GetSystemTimes to separate Kernel time for Linux, android and MacOSX.
TopCPUUsage = Class
Private
  fvPreviousST : TThread.TSystemTimes;
  fPreviousST : TThread.TSystemTimes;

{$IFDEF FPC}
  {$IFDEF LINUX}
  //For decoding /prog/cpuinfo
  Lines, LineSegments: TStringList;
  {$ENDIF}
{$ENDIF}

Protected
  CurrentSystemTime,DeltaSystemTime : TThread.TSystemTimes;

  //Based on delta.
  DeltaTotalSystemTime, DeltaUsageSystemTime, DeltaKernelUsageSystemTime : UINT64;

  Function InternalGetSystemTime(var aSystemTimes: TThread.TSystemTimes) : Boolean;
  Function InternalGetCPUUsage(var PrevSystemTimes: TThread.TSystemTimes) : Integer;

Public

  PureIdleTimePercent : Single;
  PureUserTimePercent : Single;
  PureKernelTimePercent : Single;
  PureNiceTimePercent : Single;

  UsageCPUPercentInt : Integer;
  UsageCPUPercent,
  KernelUsageCPUPercent : Single;

  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Procedure Update;

End;


{$IFDEF FPC}
  {$IFDEF WINDOWS}
function GetSystemTimes(var lpIdleTime, lpKernelTime, lpUserTime: TFileTime): BOOL; stdcall; external 'Kernel32.dll' name 'GetSystemTimes';
  {$ENDIF}
{$ENDIF}
implementation


{ TopCPUUsage }

constructor TopCPUUsage.Create;
begin
  Inherited;
  TThread.GetSystemTimes(fvPreviousST);
  TThread.GetSystemTimes(fPreviousST);
{$IFDEF FPC}
  {$IFDEF LINUX}
  //For decoding /prog/cpuinfo
  Lines := TStringList.Create;
  LineSegments := TStringList.Create;
  {$ENDIF}
{$ENDIF}
end;

Destructor TopCPUUsage.Destroy;
begin
{$IFDEF FPC}
  {$IFDEF LINUX}
  //For decoding /prog/cpuinfo
  FreeAndNil(Lines);
  FreeAndNil(LineSegments);
  {$ENDIF}
{$ENDIF}
 Inherited;
end;

Function TopCPUUsage.InternalGetCPUUsage(
  var PrevSystemTimes: TThread.TSystemTimes) : Integer;
var
  CurSystemTimes: TThread.TSystemTimes;
  Usage, Idle: UInt64;
begin
  Result := 0;
  if InternalGetSystemTime(CurSystemTimes) then
  begin
    Usage :=
      (CurSystemTimes.UserTime - PrevSystemTimes.UserTime) +
      (CurSystemTimes.KernelTime - PrevSystemTimes.KernelTime) +
      (CurSystemTimes.NiceTime - PrevSystemTimes.NiceTime);
    Idle := CurSystemTimes.IdleTime - PrevSystemTimes.IdleTime;
    if Usage > Idle then
      Result := (Usage - Idle) * 100 div Usage;
    PrevSystemTimes := CurSystemTimes;
  end;
end;

Function TopCPUUsage.InternalGetSystemTime(
  var aSystemTimes: TThread.TSystemTimes) : Boolean;

{$IFDEF FPC}
  {$IFDEF WINDOWS}
var
  Idle, User, Kernel: TFileTime;
begin
  Result := GetSystemTimes(Idle, Kernel, User);
  if Result then
  begin
    aSystemTimes.IdleTime := UInt64(Idle.dwHighDateTime) shl 32 or Idle.dwLowDateTime;
    aSystemTimes.Usertime := UInt64(User.dwHighDateTime) shl 32 or User.dwLowDateTime;
    aSystemTimes.KernelTime := UInt64(Kernel.dwHighDateTime) shl 32 or Kernel.dwLowDateTime;
    aSystemTimes.NiceTime := 0;
  end;
end;
  {$ELSE}
  {$IFDEF LINUX}
var Line: string;
begin
  Result := False;
  Lines.loadfromFile('/proc/stat');
  LineSegments.Delimiter := ' ';
  aSystemTimes.UserTime := 0;
  aSystemTimes.NiceTime := 0;
  aSystemTimes.KernelTime := 0;
  aSystemTimes.IdleTime := 0;

  for Line in Lines do
  begin
    if copy(Line,1,4) = 'cpu ' then
    begin
      LineSegments.DelimitedText := line;
      Inc(aSystemTimes.UserTime, StrToInt64(LineSegments[1]));
      Inc(aSystemTimes.NiceTime, StrToInt64(LineSegments[2]));
      // include idle time in kernel time in order to be consistent with Windows.
      Inc(aSystemTimes.KernelTime, StrToInt64(LineSegments[3]) + StrToInt64(LineSegments[4]));
      Inc(aSystemTimes.IdleTime, StrToInt64(LineSegments[4]));
      Result := True
    end
    else
      Break;
  end;
end;
  {$ELSE}
    {$MESSAGE Fatal 'Method not implemented for FPC on platform'}
  {$ENDIF}
{$ENDIF}
{$ELSE} //DELPHI
  {$IF Defined(MSWINDOWS) or Defined(MACOS) or Defined(ANDROID)}
  begin
    Result := TThread.GetSystemTimes(aSystemTimes);
  end;
  {$ELSE OTHERPLATFORM}
    {$MESSAGE Fatal 'Method not implemented for DELPHI on Platform'}
  {$ENDIF OTHERPLATFORM}
{$ENDIF}


procedure TopCPUUsage.Update;
Begin

  UsageCPUPercentInt := InternalGetCPUUsage(fvPreviousST);

  InternalGetSystemTime(CurrentSystemTime);

  DeltaSystemTime.IdleTime   := CurrentSystemTime.IdleTime - fPreviousST.IdleTime;
  DeltaSystemTime.UserTime   := CurrentSystemTime.UserTime - fPreviousST.UserTime;
  DeltaSystemTime.KernelTime := CurrentSystemTime.KernelTime - fPreviousST.KernelTime;
  DeltaSystemTime.NiceTime   := CurrentSystemTime.NiceTime - fPreviousST.NiceTime;


  DeltaTotalSystemTime := DeltaSystemTime.IdleTime +
                          DeltaSystemTime.UserTime +
                          DeltaSystemTime.KernelTime +
                          DeltaSystemTime.NiceTime;

  if DeltaTotalSystemTime>0 then
  begin

    PureIdleTimePercent   := ((DeltaSystemTime.IdleTime*100/DeltaTotalSystemTime));
    PureUserTimePercent   := ((DeltaSystemTime.UserTime*100/DeltaTotalSystemTime));
    PureKernelTimePercent := ((DeltaSystemTime.KernelTime*100/DeltaTotalSystemTime));
    PureNiceTimePercent   := ((DeltaSystemTime.NiceTime*100/DeltaTotalSystemTime));


    //Theoritical Usage.
    DeltaUsageSystemTime := DeltaSystemTime.UserTime +
                            DeltaSystemTime.KernelTime +
                            DeltaSystemTime.NiceTime;
  end;

  UsageCPUPercent := 0.00;
  if DeltaUsageSystemTime > DeltaSystemTime.IdleTime then
      UsageCPUPercent := ((DeltaUsageSystemTime - DeltaSystemTime.IdleTime) * 100 / DeltaUsageSystemTime);

  DeltaKernelUsageSystemTime := DeltaSystemTime.UserTime +
                                DeltaSystemTime.IdleTime +
                                DeltaSystemTime.NiceTime;


  //Windows have not Kernel CPU time : Try to simulate ?
  KernelUsageCPUPercent := 0.00;
  if  DeltaKernelUsageSystemTime > DeltaSystemTime.KernelTime  then
      KernelUsageCPUPercent := ((DeltaKernelUsageSystemTime - DeltaSystemTime.KernelTime) * 100 / DeltaKernelUsageSystemTime);

  InternalGetSystemTime(fPreviousST);
end;

end.
