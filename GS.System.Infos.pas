///-------------------------------------------------------------------------------
/// Title      : agr.System.Infos
/// Original   : GS.System.Infos
/// Short Desc : GS System info Delphi/FPC
/// Source     : https://github.com/VincentGsell
/// Aim        : - Got max system info system wide.
/// Licence    : GPL - Accorded without restriction at Agroplus SAS, switzerland
///                a standalone app to a middleware (network) one.
unit GS.System.Infos;

interface

uses
 {$IFDEF FPC}
 Classes,
 SysUtils,
 SyncObjs;
 {$ELSE}
 System.Classes,
 System.SysUtils,
 System.SyncObjs;
 {$ENDIF}

const
{$ifdef fpc}
  CST_Platform : Array[0..5] of string = ('Windows','Mac OS','iOS','Android','WinRT','Linux');
  CST_Archi : Array[0..2] of string = ('X86','X64','Arm32',,'Arm64');
{$else}
  CST_Platform : Array of string = ['Windows','Mac OS','iOS','Android','WinRT','Linux'];
  CST_Archi : Array of string = ['X86','X64','Arm32','Arm64'];
{$endif}


Type
TSystemInformation = class
protected
  class procedure deviceSignature(var dev,os,ver : String);
public
  Class Function GetOSAndArchAsString : String;
  Class Function GetOSMajorMinorBuild : String;
  Class Function GetOSArchitecture : String;
  Class Function GetOSName : String;
  Class Function GetOSGenuineName : String;
  Class function GetOSSignature : String;

  class function getBinaryVersionInfo : string;
end;


implementation


{$IFNDEF FPC}
  {$IFDEF IOS}
uses
   iOSapi.UIKit, iOSapi.Foundation, Macapi.Helpers;
  {$ENDIF}

  {$IFDEF ANDROID}
uses
    androidapi.JNI.JavaTypes,
    Androidapi.Helpers,
    androidapi.JNI.Os;
  {$ENDIF}
{$ENDIF}

{$IFDEF MSWINDOW}
function GetAppVersionStr: string;
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  Result := Format('%d.%d.%d.%d',
    [LongRec(FixedPtr.dwFileVersionMS).Hi,  //major
     LongRec(FixedPtr.dwFileVersionMS).Lo,  //minor
     LongRec(FixedPtr.dwFileVersionLS).Hi,  //release
     LongRec(FixedPtr.dwFileVersionLS).Lo]) //build
end;

//Should perhaps better but not tested : This version allow to use it even if
//binary is hardlocked by windows.
//WARNING : PARIALY TESTED ONLY. We use "GetAppVersionStr" actualy without hassle.
function GetVersionInfo(AIdent: String): String;
type
  TLang = packed record
    Lng, Page: WORD;
  end;
  TLangs = array [0 .. 10000] of TLang;
  PLangs = ^TLangs;
var
  BLngs: PLangs;
  BLngsCnt: Cardinal;
  BLangId: String;
  RM: TMemoryStream;
  RS: TResourceStream;
  BP: PChar;
  BL: Cardinal;
  BId: String;

begin
  // Assume error
  Result := '';

  RM := TMemoryStream.Create;
  try
    // Load the version resource into memory
    RS := TResourceStream.CreateFromID(HInstance, 1, RT_VERSION);
    try
      RM.CopyFrom(RS, RS.Size);
    finally
      FreeAndNil(RS);
    end;

    // Extract the translations list
    if not VerQueryValue(RM.Memory, '\\VarFileInfo\\Translation', Pointer(BLngs), BL) then
      Exit; // Failed to parse the translations table
    BLngsCnt := BL div sizeof(TLang);
    if BLngsCnt <= 0 then
      Exit; // No translations available

    // Use the first translation from the table (in most cases will be OK)
    with BLngs[0] do
      BLangId := IntToHex(Lng, 4) + IntToHex(Page, 4);

    // Extract field by parameter
    BId := '\\StringFileInfo\\' + BLangId + '\\' + AIdent;
    if not VerQueryValue(RM.Memory, PChar(BId), Pointer(BP), BL) then
      Exit; // No such field

    // Prepare result
    Result := BP;
  finally
    FreeAndNil(RM);
  end;
end;

function FileDescription: String;
begin
  Result := GetVersionInfo('FileDescription');
end;

function LegalCopyright: String;
begin
  Result := GetVersionInfo('LegalCopyright');
end;

function DateOfRelease: String;
begin
  Result := GetVersionInfo('DateOfRelease');
end;

function ProductVersion: String;
begin
  Result := GetVersionInfo('ProductVersion');
end;

function FileVersion: String;
begin
  Result := GetVersionInfo('FileVersion');
end;
{$ENDIF}


{ TSystemInformation }

class function TSystemInformation.getBinaryVersionInfo: string;
{$IFDEF ANDROID}
var
  PackageManager: JPackageManager;
  PackageInfo: JPackageInfo;
{$ENDIF}
begin
{$IFDEF ANDROID}
  PackageManager := SharedActivityContext.getPackageManager;
  PackageInfo := PackageManager.getPackageInfo(SharedActivityContext.getPackageName, 0);
  Result := JStringToString(PackageInfo.versionName);
{$ELSE}
{$IFDEF IOS}
  Result :=  TNSString.Wrap(CFBundleGetValueForInfoDictionaryKey(CFBundleGetMainBundle, kCFBundleVersionKey)).UTF8String;
{$ELSE}
{$IFDEF MSWINDOWS}
  result := GetAppVersionStr;
{$ELSE}
{$IFDEF LINUX}
{$ELSE}
  todo.
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
end;

Class function TSystemInformation.GetOSAndArchAsString: String;
begin
  Result :=  GetOSGenuineName + ' ('+GetOSMajorMinorBuild+') - '+ GetOSName +' - '+ GetOSArchitecture;
end;

Class function TSystemInformation.GetOSArchitecture: String;
begin
{$IFDEF FPC}
  {$IFDEF CPUARM}
  Result := CST_Archi[2];
  {$ELSE}
  Result := CST_Archi[0];
  {$ENDIF}
{$ELSE}
    Result := CST_Archi[integer(TOSVersion.Architecture)]
{$ENDIF}
end;

Class function TSystemInformation.GetOSGenuineName: String;
begin
{$IFDEF FPC}
  {$IFDEF LINUX}
  Result := CST_Platform[5];
  {$ELSE}
  Result := CST_Platform[0];
  {$ENDIF}
{$ELSE}
  Result := TOSVersion.Name;
{$ENDIF}
end;

Class function TSystemInformation.GetOSMajorMinorBuild: String;
begin
{$IFDEF FPC}
  Result := '0'; //?
{$ELSE}
  Result := IntToStr(TOSVersion.Major)+'.'+IntToStr(TOSVersion.Minor)+'.'+IntToStr(TOSVersion.Build);
{$ENDIF}
end;

Class function TSystemInformation.GetOSName: String;
begin
{$IFDEF FPC}
  {$IFDEF LINUX}
  Result := CST_Platform[5];
  {$ELSE}
  Result := CST_Platform[0];
  {$ENDIF}
{$ELSE}
  Result := CST_Platform[Integer(TOSVersion.Platform)]
{$ENDIF}
end;


class function TSystemInformation.GetOSSignature: String;
var a,b,c : String;
begin
  deviceSignature(a,b,c);
  result := format('%s on %s %s',[a,b,c]);
end;

{$IFNDEF FPC}
  {$IFDEF ANDROID}
  function GetCodename(VerString: string): string;
  begin
    if VerString = '1.0' then
      Result := 'BASE'
    else if VerString = '1.1' then
      Result := 'BASE_1_1'
    else if VerString = '1.5' then
      Result := 'CUPCAKE'
    else if VerString = '1.6' then
      Result := 'DONUT'
    else if VerString = '2.0' then
      Result := 'ECLAIR'
    else if VerString = '2.0.1' then
      Result := 'ECLAIR_0_1'
    else if VerString = '2.1' then
      Result := 'ECLAIR_MR1'
    else if VerString = '2.2' then
      Result := 'FROYO'
    else if VerString = '2.3' then
      Result := 'GINGERBREAD'
    else if VerString = '2.3.3' then
      Result := 'GINGERBREAD_MR1'
    else if VerString = '3.0' then
      Result := 'HONEYCOMB'
    else if VerString = '3.1' then
      Result := 'HONEYCOMB_MR1'
    else if VerString = '3.2' then
      Result := 'HONEYCOMB_MR2'
    else if VerString = '4.0' then
      Result := 'ICE_CREAM_SANDWICH'
    else if VerString = '4.0.3' then
      Result := 'ICE_CREAM_SANDWICH_MR1'
    else if VerString = '4.1' then
      Result := 'JELLY_BEAN'
    else if VerString = '4.2' then
      Result := 'JELLY_BEAN_MR1'
    else if VerString = '4.3' then
      Result := 'JELLY_BEAN_MR2'
    else if Pos('4.4',VerString) = 1 then
      Result := 'KITKAT'
    else if Pos('5',VerString) = 1 then
      Result := 'LOLLIPOP'
    else if Pos('6',VerString) = 1 then
      Result := 'MARSHMALLOW'
    else if Pos('7',VerString) = 1 then
      Result := 'NOUGAT'
    else if Pos('8',VerString) = 1 then
      Result := 'OREO'
    else if Pos('9',VerString) = 1 then
      Result := 'PIE'
    else if Pos('10',VerString) = 1 then
      Result := 'Quince Tart'
    else if Pos('11',VerString) = 1 then
      Result := '	Red Velvet Cake'
    else if Pos('12',VerString) = 1 then
      Result := 'Snow Cone'
    else if Pos('13',VerString) = 1 then
      Result := 'Tiramisu'
    else Result := 'UNKNOWN';
  end;

  class procedure TSystemInformation.deviceSignature(var dev,os,ver : String);
  begin
    dev := Format('%s', [JStringToString(TJBuild.JavaClass.MODEL)]);
    os  := Format('%s', [GetCodename(JStringToString(TJBuild_VERSION.JavaClass.RELEASE))]);
    ver := Format('%s', [JStringToString(TJBuild_VERSION.JavaClass.RELEASE)]);
  end;
  {$ELSEIF IOS}
  class procedure TSystemInformation.deviceSignature(var dev,os,ver : String);
  var
    Device : UIDevice;
  begin
    Device := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice);
    os := Format('%s', [NSStrToStr(Device.systemName)]);
    ver := Format('%s', [NSStrToStr(Device.systemVersion)]);
    dev := Format('%s', [NSStrToStr(Device.model)]);
  end;
  {$ENDIF}
{$ENDIF}

{$IFNDEF ANDROID AND IOS}
  //Delphi desktop, FPC all case.
  class procedure TSystemInformation.deviceSignature(var dev, os, ver: String);
  begin
    dev := GetOSArchitecture;
    os := GetOSName;
    ver := GetOSMajorMinorBuild;
  end;
{$ENDIF}



end.
