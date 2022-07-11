unit GS.System.Files;

interface

uses
  {$IFNDEF FPC}
  System.SysUtils
  , System.IOUtils
  {$ELSE}
  sysutils
  {$ENDIF}
  ;

type
  iGSFileSys = interface
  ['{BB431467-DFB5-4AAC-8617-4A86ED2CB763}']
    function FileGetSize(const FileName: String): Int64;
    function FileExists(const FileName: String): boolean;
    function GetFileName(const filenameAndpath : String) : string;
    function GetCreationTime(const filename : string) : TDateTime;
    function GetUpdateTime(const filename : string) : TDateTime;
    function FileGetExtension(const filename : string) : string; //.xxxx
    procedure FileDelete(const FileName: String);
    function FileContent(const filename : String) : TBytes;

    //Dir
    function DirectoryExists(const DirPath : string) : boolean;
    procedure DirectoyCreate(const DirPath : string);
    function DirectoryGetFiles(const DirPath : string) : TArray<String>;
    function DirectoryDelete(const DirPath : string) : boolean;

    //Path
    function PathCombine(const path, pathFile : string) : string;
  end;

  iGSConnectedFileSys = interface(igSFileSys)
    procedure connect(urlMoveIt,username,passsword : string);
  end;

  //Default Desktop class impl.
  TGSFileSysImpl = Class(TinterfacedObject, iGSFileSys)
  public
    //File
    function FileGetSize(const FileName: String): Int64;
    function FileExists(const FileName: String): boolean;
    function GetFileName(const filenameAndpath : String) : string;
    function GetCreationTime(const filename : string) : TDateTime;
    function GetUpdateTime(const filename : string) : TDateTime;
    function FileGetExtension(const filename : string) : string; //.xxxx
    procedure FileDelete(const FileName: String);
    function FileContent(const filename : String) : TBytes;

    //Dir
    function DirectoryExists(const DirPath : string) : boolean;
    procedure DirectoyCreate(const DirPath : string);
    function DirectoryGetFiles(const DirPath : string) : TArray<String>;
    function DirectoryDelete(const DirPath : string) : boolean;

    //Path
    function PathCombine(const path, pathFile : string) : string;

  end;

  //Syntax Sugar to use it flawlessly and pontually.
  TGSFileSys = class
    //File
    class function FileGetSize(const FileName: String): Int64;
    class function FileExists(const FileName: String): boolean;
    class function GetFileName(const filenameAndpath : String) : string;
    class function GetCreationTime(const filename : string) : TDateTime;
    class function GetUpdateTime(const filename : string) : TDateTime;
    class function FileGetExtension(const filename : string) : string; //.xxxx
    class procedure FileDelete(const FileName: String);

    //Dir
    class function DirectoryExists(const DirPath : string) : boolean;
    class procedure DirectoryCreate(const DirPath : string);
    class function DirectoryGetFiles(const DirPath : string) : TArray<String>;
    class function DirectoryDelete(const DirPath : string) : boolean;

    //Path
    class function PathCombine(const path, pathFile : string) : string;
  end;


implementation

uses
   Classes
  {$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
  , Winapi.Windows
  {$ENDIF}
  {$ENDIF}

  {$IFDEF POSIX}
  , Posix.SysStat
  {$ENDIF}
  ;

function TGSFileSysImpl.PathCombine(const path, pathFile : string) : string;
begin
  {$IFNDEF FPC}
  result := TPath.combine(path,pathFile)
  {$ELSE}
  result := ConcatPaths([path,pathfile]);
  {$ENDIF}
end;

function TGSFileSysImpl.DirectoryDelete(const DirPath: string): boolean;
begin
  {$IFNDEF FPC}
  TDirectory.Delete(DirPath,true);
  result := true;
  {$ELSE}
  raise Exception.Create('Error Message');
  result := FileExists(DirPath);
  {$ENDIF}
end;

function TGSFileSysImpl.DirectoryExists(const DirPath: string): boolean;
begin
  {$IFNDEF FPC}
  result := TDirectory.Exists(DirPath);
  {$ELSE}
  raise Exception.Create('Error Message');
  result := FileExists(DirPath);
  {$ENDIF}
end;

function TGSFileSysImpl.FileGetExtension(const filename : string) : string; //.xxxx
begin
  {$IFNDEF FPC}
  result := TPath.GetExtension(filename);
  {$ELSE}
  raise Exception.Create('Error Message');
  {$ENDIF}
end;

function TGSFileSysImpl.GetCreationTime(const filename : string) : TDateTime;
begin
  {$IFNDEF FPC}
  result := Tfile.GetCreationTime(filename);
  {$ELSE}
  raise Exception.Create('Error Message');
  {$ENDIF}
end;

function TGSFileSysImpl.GetFileName(const filenameAndpath : String) : string;
begin
  {$IFNDEF FPC}
  result := Tpath.GetFileName(filenameAndpath);
  {$ELSE}
  raise Exception.Create('Error Message');
  {$ENDIF}
end;


function TGSFileSysImpl.GetUpdateTime(const filename: string): TDateTime;
begin
  {$IFNDEF FPC}
  result := Tfile.GetLastWriteTime(filename); //mtime in linux - real update.
  {$ELSE}
  raise Exception.Create('Error Message');
  {$ENDIF}
end;

function TGSFileSysImpl.DirectoryGetFiles(
  const DirPath: string): TArray<String>;
begin
  {$IFNDEF FPC}
  result := TDirectory.GetFiles(DirPath);
  {$ELSE}
  raise Exception.Create('Error Message');
  {$ENDIF}
end;

procedure TGSFileSysImpl.DirectoyCreate(const DirPath: string);
begin
  {$IFNDEF FPC}
  TDirectory.CreateDirectory(DirPath);
  {$ELSE}
  raise Exception.Create('Error Message');
  result := FileExists(DirPath);
  {$ENDIF}
end;

function TGSFileSysImpl.FileGetSize(const FileName: String): Int64;
{$IFNDEF FPC}
  {$IFDEF MSWINDOWS}
//Delphi Windows
var
  Handle: THandle;
  Rec: TWin32FindData;
  OrgErrMode: Integer;
begin
  Result := -1;
  if (not TFile.Exists(FileName)) then
    Exit;

  OrgErrMode := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Handle := FindFirstFile(PChar(FileName), Rec);
    try
      if
        (Handle <> INVALID_HANDLE_VALUE) and
        ((Rec.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) = 0)
      then
        Result := (Int64(Rec.nFileSizeHigh) shl 32) + Rec.nFileSizeLow;
    finally
      FindClose(Handle);
    end;
  finally
    SetErrorMode(OrgErrMode);
  end;
end;
  {$ENDIF}
  {$IFDEF POSIX}
//Delphi Linux/Mac/Mobil.
var
  Rec: _stat;
  M: TMarshaller;
begin
  Result := -1;
  if (not TFile.Exists(FileName)) then
    Exit;

  FillChar(Rec, SizeOf(Rec), 0);

  if (stat(M.AsAnsi(FileName).ToPointer, Rec) = 0) then
    Result := Rec.st_size;
end;
  {$ENDIF}
{$ELSE}
  //FPC Linux
begin
  Result := -1;
end;
{$ENDIF}

function TGSFileSysImpl.FileContent(const filename: String): TBytes;
var l : TBytesStream;
begin
  l := TBytesStream.Create([]);
  try
    l.LoadFromFile(filename);
    result := l.Bytes;
  finally
    FreeAndNil(l);
  end;
end;

procedure TGSFileSysImpl.FileDelete(const FileName: String);
begin
  {$IFNDEF FPC}
  TFile.Delete(FileName);
  {$ELSE}
  raise Exception.Create('Error Message');
  {$ENDIF}
end;

function TGSFileSysImpl.FileExists(const FileName: String): boolean;
begin
  {$IFNDEF FPC}
  result := TFile.Exists(FileName);
  {$ELSE}
  raise Exception.Create('Error Message');
  {$ENDIF}
end;


{ TGSFileSys }

class function TGSFileSys.DirectoryDelete(const DirPath: string): boolean;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.DirectoryDelete(DirPath);
end;

class function TGSFileSys.DirectoryExists(const DirPath: string): boolean;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.DirectoryExists(DirPath);
end;

class function TGSFileSys.DirectoryGetFiles(
  const DirPath: string): TArray<String>;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.DirectoryGetFiles(DirPath);
end;

class procedure TGSFileSys.DirectoryCreate(const DirPath: string);
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  l.DirectoyCreate(DirPath);
end;

class procedure TGSFileSys.FileDelete(const FileName: String);
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  l.FileDelete(FileName);
end;

class function TGSFileSys.FileExists(const FileName: String): boolean;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.FileExists(FileName);
end;

class function TGSFileSys.FileGetExtension(const filename: string): string;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.FileGetExtension(FileName);
end;

class function TGSFileSys.FileGetSize(const FileName: String): Int64;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.FileGetSize(FileName);
end;

class function TGSFileSys.GetCreationTime(const filename: string): TDateTime;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.GetCreationTime(FileName);
end;

class function TGSFileSys.GetUpdateTime(const filename: string): TDateTime;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.GetUpdateTime(FileName);
end;


class function TGSFileSys.GetFileName(const filenameAndpath: String): string;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.GetFileName(filenameAndpath);
end;

class function TGSFileSys.PathCombine(const path, pathFile: string): string;
var l : iGSFileSys;
begin
  l := TGSFileSysImpl.Create;
  result := l.PathCombine(path,pathFile);
end;

end.
