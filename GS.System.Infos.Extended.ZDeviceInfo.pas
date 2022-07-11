unit GS.System.Infos.Extended.ZDeviceInfo;

///
/// https://github.com/rzaripov1990/FMX.DeviceInfo.git

interface

uses classes,
     sysutils,
     GS.System.Infos.Extended,
     FMX.ZDeviceInfo;

Type

TgsSysInfoZDevice = class(TgsSysInfoBase)
private
  FImpl : TZDeviceInfo;
public
  Constructor Create; Override;
  Destructor Destroy; override;

  function getPlatform: String; override;
  function getPlatformVer: String; override;
  function getArchitecture: String; override;
  function getDevice: String; override;
end;

TgsSysInfoFactoryZDevice = Class(TGSInfoFactory)
  function GetImplementation : IGSSysInfo; Override;
End;


implementation


{ TgsSysInfoFactoryZDevice }

function TgsSysInfoFactoryZDevice.GetImplementation: IGSSysInfo;
begin
  Result := TgsSysInfoZDevice.Create;
end;

{ TgsSysInfoZDevice }

constructor TgsSysInfoZDevice.Create;
begin
  Inherited Create;
  FImpl := TZDeviceInfo.Create;
  FValues.Add('deviceid='+FImpl.DeviceID);
  FValues.Add('architecture='+FImpl.Architecture);
  FValues.Add('architecture2='+FImpl.Architecture2);
  FValues.Add('macaddress='+FImpl.MacAddress);
  FValues.Add('ipaddress='+FImpl.IPAddress);
  FValues.Add('deviceid='+FImpl.LangID);
  FValues.Add('screenphys='+FImpl.ScreenPhis);
  FValues.Add('screenlogic='+FImpl.ScreenLogic);
  FValues.Add('screenwidth='+FImpl.ScreenWidth.ToString);
  FValues.Add('screenheight='+FImpl.ScreenHeight.ToString);
  FValues.Add('scale='+FImpl.Scale.ToString);
  FValues.Add('mobileoperator='+FImpl.MobileOperator);

  case FImpl.networkconnectiontype of
    TZNetworkConnectionType.None : FValues.Add('networkconnectiontype=none');
    TZNetworkConnectionType.Unknown : FValues.Add('networkconnectiontype=unknown');
    TZNetworkConnectionType.WIFI : FValues.Add('networkconnectiontype=wifi');
    TZNetworkConnectionType.Mobile : FValues.Add('networkconnectiontype=mobile');
    TZNetworkConnectionType.Ethernet : FValues.Add('networkconnectiontype=ethernet');
  end;

  case FImpl.MobileDataType of
    TZMobileDataType.None : FValues.Add('mobiledatatype=none');
    TZMobileDataType.Unknown : FValues.Add('mobiledatatype=unknown');
    TZMobileDataType.n2G : FValues.Add('mobiledatatype=2G');
    TZMobileDataType.n3G : FValues.Add('mobiledatatype=3G');
    TZMobileDataType.n4G : FValues.Add('mobiledatatype=4G/LTE');
    TZMobileDataType.n5G : FValues.Add('mobiledatatype=5G');
    TZMobileDataType.n6G : FValues.Add('mobiledatatype=6G');
  end;
  FValues.Add('timezone='+FImpl.TimeZone.ToString);
  FValues.Add('isintel='+intTostr(byte(FImpl.IsIntel)));
  FValues.Add('isnetconnected='+intTostr(byte(FImpl.IsNetConnected)));
  FValues.Add('isportraitorientation='+intTostr(byte(FImpl.IsPortraitOrientation)));
  FValues.Add('isgpsactive='+intTostr(byte(FImpl.IsGPSActive)));
end;

destructor TgsSysInfoZDevice.Destroy;
begin
  FreeAndNil(FImpl);
  inherited;
end;

function TgsSysInfoZDevice.getArchitecture: String;
begin
  result := FImpl.Architecture;
end;

function TgsSysInfoZDevice.getDevice: String;
begin
  result := FImpl.Device;
end;


function TgsSysInfoZDevice.getPlatform: String;
begin
  result := FImpl.Platform;
end;

function TgsSysInfoZDevice.getPlatformVer: String;
begin
  Result := FImpl.PlatformVer;
end;


Initialization

TgsSysInfoImplManager.registerSysInfo('zDeviceInfo',TgsSysInfoFactoryZDevice.Create);


finalization

end.
