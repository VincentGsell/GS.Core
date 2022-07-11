//Delphi specific progress/ipswitch/MoveIt servoie implementation - TRestXXX based
unit GS.System.Files.MoveItWebService;

interface

uses Classes
     ,SysUtils
     ,Rest.Types
     ,Rest.Client
     ,GS.Common
     ,GS.System.Files;

Type
  TGSMoveItFileSysImpl = Class(TinterfacedObject, iGSConnectedFileSys)
  protected
    FClient : TRestClient;
    FResponse : TRestResponse;
    FRequest : TRestRequest;
    FToken : String;

    FFolders : String; //Shadow Json string of last FFolders query.
    FFiles : String; //Shadow Json string of last FFiles query.
    FLastDirId, FLastFileId : String;

    procedure Check;

    function internalGetDirId(aDirNameAndPath : String) : string;
    function internalGetFiles : TArray<string>;
    function internalgetFileId(aFileName : String) : string;
    function internalDownloadFile(aDirId, aFileId : string) : TBytes;
  public
    Constructor Create; virtual;
    Destructor Destroy; Override;

    procedure connect(urlMoveIt,username,passsword : string);

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


implementation

uses GS.JSON;


{ TGSMoveItFileSysImpl }

procedure TGSMoveItFileSysImpl.Check;
begin
  if FToken.Length=0 then
    raise Exception.Create(ClassName+' - not ready (not connected) - Call Connect(...) before use.');
end;

procedure TGSMoveItFileSysImpl.connect(urlMoveIt, username, passsword: string);
var lj : TGSJson;
    lsFolderId : string;
    lsParam : TRESTREquestParameter;
begin
  lj := TGSJson.Create;
  try
    FClient.BaseURL := 'https://ft.wbf.admin.ch/api/v1/';

    FClient.Accept := 'application/json';
    FClient.Params.AddItem('Content-Type','application/x-www-form-urlencoded',TRESTRequestParameterkind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
    FClient.Params.AddItem('Accept','application/json',TRESTRequestParameterkind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

    FClient.Params.AddItem('grant_type', 'password', TRESTRequestParameterKind.pkGETorPOST);
    FClient.Params.AddItem('username', 'maf-agroplus', TRESTRequestParameterKind.pkGETorPOST);
    FClient.Params.AddItem('password', 'hdfgol%%34DKDsSAgr', TRESTRequestParameterKind.pkGETorPOST);

    FRequest.Resource := 'token';

    FRequest.Method := TRESTRequestMethod.rmPOST;
    FRequest.Execute;

    lj.Parse(FResponse.Content);
    FToken := lj.Get('access_token').AsString;

    //Get user.
    FRequest.Resource := 'users/self';
    FRequest.AddAuthParameter('Authorization','Bearer ' + FToken, TRESTRequestParameterKind.pkHTTPHEADER, [poDoNotEncode]);

    FRequest.Method := TRESTRequestMethod.rmGET;
    FRequest.Execute;
    lj.Parse(FResponse.Content);
    lsFolderId := lj.Get('homeFolderID').AsString;

    //Directory.
    FRequest.Resource := 'folders';
    FRequest.Execute;
    FFolders := FResponse.Content;
  finally
    FreeAndNil(lj);
  end;
end;


constructor TGSMoveItFileSysImpl.Create;
begin
  inherited;
  FToken := '';
  FFolders := '';
  FFiles := '';
  FClient := TRESTClient.Create(nil);
  FResponse := TRESTResponse.Create(nil);
  FRequest := TRESTRequest.Create(nil);
  FRequest.Client := FClient;
  FRequest.Response := FResponse;
end;

destructor TGSMoveItFileSysImpl.Destroy;
begin
  FreeAndNil(FClient);
  FreeAndNil(FResponse);
  FreeAndNil(FRequest);
  inherited;
end;

function TGSMoveItFileSysImpl.DirectoryDelete(const DirPath: string): boolean;
begin
  check;
  raise Exception.Create('todo');
end;

function TGSMoveItFileSysImpl.DirectoryExists(const DirPath: string): boolean;
begin
  check;
  raise Exception.Create('todo');
end;

function TGSMoveItFileSysImpl.DirectoryGetFiles(
  const DirPath: string): TArray<String>;
var lid : string;
begin
  Check;
  lid := InternalGetDirId(dirPath);
  if lid.Length=0 then
    exit;

  FRequest.Resource := 'folders/'+lid+'/files';
  FRequest.Execute;
  FFiles := FResponse.Content;
  result := internalGetFiles;
end;

procedure TGSMoveItFileSysImpl.DirectoyCreate(const DirPath: string);
begin
  Check;
  raise Exception.Create('todo');
end;

function TGSMoveItFileSysImpl.FileContent(const filename: String): TBytes;
begin
  check;
  internalgetFileId(filename);
  if FLastDirId.IsEmpty then
    raise Exception.Create(ClassName+' - Unknown directory');
  if FLastFileId.IsEmpty then
    raise Exception.Create(ClassName+' - No file selected');
  result := internalDownloadFile(FLastDirId,FLastFileId);
end;

procedure TGSMoveItFileSysImpl.FileDelete(const FileName: String);
begin
  Check;
  //...
end;

function TGSMoveItFileSysImpl.FileExists(const FileName: String): boolean;
begin
  Check;
  //...
end;

function TGSMoveItFileSysImpl.FileGetExtension(const filename: string): string;
begin
  Check;
  //...
end;

function TGSMoveItFileSysImpl.FileGetSize(const FileName: String): Int64;
var lj : TGSJson;
    lja : TGSJsonArray;
    ljo : TGSJsonObject;
    i : integer;
begin
  result := 0;
  if FFiles.Length=0 then
    Exit;
  lj := TGSJSon.Create;
  try
    lj.Parse(FFiles);
    lja := lj.Get('items').AsArray;
    for I := 0 to lja.Count-1 do begin
      ljo := lja.Items[i].AsObject;
      if ljo.Values['name'].AsString.ToLower = FileName.ToLower then begin
        result := ljo.Values['size'].AsInteger;
        Exit;
      end;
    end;
  finally
    FreeAndNil(lj);
  end;
end;

function TGSMoveItFileSysImpl.GetCreationTime(
  const filename: string): TDateTime;
begin
  Check;
  //...
end;

function TGSMoveItFileSysImpl.GetFileName(
  const filenameAndpath: String): string;
begin
  Check;
  //...
end;

function TGSMoveItFileSysImpl.GetUpdateTime(const filename: string): TDateTime;
begin
  Check;
  //...
end;

function TGSMoveItFileSysImpl.internalDownloadFile(aDirId, aFileId: string): TBytes;
begin
  FRequest.Resource := 'folders/'+aDirId+'/files/'+aFileId+'/download';
  FRequest.Execute;
  result := FResponse.RawBytes;
end;

function TGSMoveItFileSysImpl.internalGetDirId(aDirNameAndPath: String): string;
var lj : TGSJson;
    lja : TGSJsonArray;
    ljo : TGSJsonObject;
    i : integer;
begin
  result := '';
  lj := TGSJSon.Create;
  try
    lj.Parse(FFolders);
    lja := lj.Get('items').AsArray;
    for I := 0 to lja.Count-1 do begin
      ljo := lja.Items[i].AsObject;
      if aDirNameAndPath.ToLower = ljo.Values['name'].AsString.ToLower then begin
        result := ljo.Values['id'].AsString;
        exit;
      end;
    end;
  finally
    FLastDirId := result;
    FreeAndNil(lj);
  end;
end;

function TGSMoveItFileSysImpl.internalgetFileId(aFileName: String): string;
var lj : TGSJson;
    lja : TGSJsonArray;
    ljo : TGSJsonObject;
    i : integer;
begin
  result := '';
  if FFiles.Length=0 then
    Exit;
  lj := TGSJSon.Create;
  try
    lj.Parse(FFiles);
    lja := lj.Get('items').AsArray;
    SetLength(result,lja.Count);
    for I := 0 to lja.Count-1 do begin
      ljo := lja.Items[i].AsObject;
      if ljo.Values['name'].AsString.ToLower = aFileName.ToLower then begin
        result := ljo.Values['id'].AsString;
        Exit;
      end;
    end;
  finally
    FLastFileId := result;
    FreeAndNil(lj);
  end;
end;

function TGSMoveItFileSysImpl.internalGetFiles: TArray<string>;
var lj : TGSJson;
    lja : TGSJsonArray;
    ljo : TGSJsonObject;
    i : integer;
begin
  result := [];
  if FFiles.Length=0 then
    Exit;
  lj := TGSJSon.Create;
  try
    lj.Parse(FFiles);
    lja := lj.Get('items').AsArray;
    SetLength(result,lja.Count);
    for I := 0 to lja.Count-1 do begin
      ljo := lja.Items[i].AsObject;
      result[i] := ljo.Values['name'].AsString;
    end;
  finally
    FreeAndNil(lj);
  end;
end;

function TGSMoveItFileSysImpl.PathCombine(const path, pathFile: string): string;
begin
  Check;
  //...
end;

end.
