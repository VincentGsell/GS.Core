unit GS.System.Infos.Extended.Default;

interface

uses Classes,
     SysUtils,
     GS.System.Infos.Extended;

Type

TgsSysInfoDefault = class(TgsSysInfoBase)
public
  function getPlatform: String; override;
  function getPlatformVer: String; override;
  function getArchitecture: String; override;
  function getDevice: String; override;
end;

TgsSysInfoFactoryDefault = Class(TGSInfoFactory)
  function GetImplementation : IGSSysInfo; Override;
End;

implementation

Uses GS.System.Infos;


{ TgsSysInfoDefault }

function TgsSysInfoDefault.getArchitecture: String;
begin
  result := TSystemInformation.GetOSArchitecture;
end;

function TgsSysInfoDefault.getDevice: String;
begin
  result := TSystemInformation.GetOSGenuineName
end;

function TgsSysInfoDefault.getPlatform: String;
begin
  result := TSystemInformation.GetOSName;
end;

function TgsSysInfoDefault.getPlatformVer: String;
begin
  result := TSystemInformation.GetOSMajorMinorBuild;
end;


{ TgsSysInfoFactoryDefault }

function TgsSysInfoFactoryDefault.GetImplementation: IGSSysInfo;
begin
  result := TgsSysInfoDefault.Create;
end;

Initialization

TgsSysInfoImplManager.registerSysInfo('gsSys-default',TgsSysInfoFactoryDefault.Create);

Finalization

end.
