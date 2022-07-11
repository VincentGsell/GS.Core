unit GS.System.Infos.Extended;

interface

Uses Classes, SysUtils,
     System.Generics.Collections;

Type
IgsSysInfo = interface
  function getPlatform: String;
  function getPlatformVer: String;
  function getArchitecture: String;
  function getDevice: String;

  function getExtFieldCount : integer;
  function getExtFieldName(index : integer) : string;
  function getFieldNames : string;
  function getExtFieldValue(fieldName : string) : string;
end;

IgsSysInfoFactory = interface
  function GetImplementation : IgsSysInfo;
end;

TGSInfoFactory = Class(TInterfacedObject,IGSSysInfoFactory)
  function GetImplementation : IgsSysInfo; virtual; abstract;
End;

//Impl
TgsSysInfoBase = class(TInterfacedObject, IgsSysInfo)
protected
  FValues : TStringList;
public
  Constructor Create; virtual;
  Destructor Destroy; override;

  function getPlatform: String; virtual; abstract;
  function getPlatformVer: String; virtual; abstract;
  function getArchitecture: String; virtual; abstract;
  function getDevice: String; virtual; abstract;

  function getExtFieldCount : integer;
  function getExtFieldName(index : integer) : string;
  function getFieldNames : string;
  function getExtFieldValue(fieldName : string) : string;
end;


TgsSysInfoImplManager = class
private
  class var flist : TDictionary<string,IgsSysInfoFactory>;
  class procedure internalCheck;
public
  class procedure registerSysInfo(name : String; factory : IGSSysInfoFactory);
  class function GetImpl(name : String) : IgsSysInfo;
  class function ImplList : string;
  class procedure releaseRes;
end;

implementation

{ TgsSysInfoImplManager }

class function TgsSysInfoImplManager.GetImpl(name: String): IgsSysInfo;
var l : IgsSysInfoFactory;
begin
  internalCheck;
  if flist.TryGetValue(name,l) then
    result := l.GetImplementation;
end;

class function TgsSysInfoImplManager.ImplList: string;
var l : TPair<string,IgsSysInfoFactory>;
begin
  for l in flist do
    result := result + l.Key + sLineBreak;
  result := result.Trim;
end;

class procedure TgsSysInfoImplManager.internalCheck;
begin
  if Not(assigned(flist)) then
    flist := TDictionary<string,IgsSysInfoFactory>.Create;
end;

class procedure TgsSysInfoImplManager.registerSysInfo(name: String;
  factory: IGSSysInfoFactory);
begin
  internalCheck;
  Assert(Assigned(factory));
  Assert(name.Trim.Length>0);
  flist.Add(name,TGSInfoFactory(factory));
end;

class procedure TgsSysInfoImplManager.releaseRes;
begin
  if assigned(flist) then
    FreeAndNil(flist);
end;

{ TgsSysInfoBase }

constructor TgsSysInfoBase.Create;
begin
  Inherited;
  FValues := TStringList.Create;
end;

destructor TgsSysInfoBase.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TgsSysInfoBase.getExtFieldCount: integer;
begin
  result := FValues.Count;
end;

function TgsSysInfoBase.getExtFieldName(index: integer): string;
begin
  result := FValues.Names[index];
end;

function TgsSysInfoBase.getExtFieldValue(fieldName: string): string;
begin
  result := FValues.Values[fieldName];
end;

function TgsSysInfoBase.getFieldNames: string;
begin
  result := FValues.Text;
end;

Initialization

Finalization

TgsSysInfoImplManager.releaseRes;

end.
