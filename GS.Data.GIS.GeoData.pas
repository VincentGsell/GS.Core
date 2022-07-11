unit GS.Data.GIS.GeoData;

interface

uses sysutils
    ,classes
    ,system.Types
    ,GS.Common
    ,GS.Common.Log
    ,GS.Data.GIS
    ,Rest.Types
    ,Rest.Client
    ,GS.JSON
    ;


const
  cst_LocationBaseQuery = 'https://api3.geo.admin.ch/1909260825/rest/services/ech/SearchServer?sr=3857'
  + '&searchText=%s&lang=fr&type=locations';
const
  cst_GeometryBaseQuery = 'https://api3.geo.admin.ch/rest/services/all/MapServer/identify?'
                + 'geometry=%s,%s&geometryFormat=geojson&geometryType=esriGeometryPoint'
                + '&lang=fr&layers=all:ch.kantone.cadastralwebmap-farbe&limit=10'
                + '&returnGeometry=true&sr=3857&tolerance=0';

Type
  TGSDataGISGeoData = class(TInterfacedObject, IGSDataGIS)
  protected
    function GetParcelleLocation(params: String): Tpointf;
    function GetParcelleGeometry(x,y:string) : TArray<TArray<TPointf>>;
  public
    function getAreaGeometryByStringQueryAsJson(aQuery :string) : string;
  end;

implementation

function TGSDataGISGeoData.GetParcelleLocation(params: String): Tpointf;
var
  lHttp: TRESTClient;
  lHttpQuery : TRESTRequest;
  lHttpResponse : TRESTResponse;

  Response: string;
  ParcelleLocation: TPointf;
  lj,lo : TGSJsonObject;
  la : TGSJsonArray;
begin
  lHttp := TRESTClient.Create(nil);
  lHttpQuery := TRESTRequest.Create(nil);
  lHttpResponse := TRESTResponse.Create(nil);

  lHttpQuery.Client := lHttp;
  lHttpQuery.Response := lHttpResponse;

  lHttp.BaseURL := sysutils.Format(cst_LocationBaseQuery, [params]);

  lHttp.Accept := 'application/json';
  lHttp.Params.AddItem('Content-Type','application/x-www-form-urlencoded',TRESTRequestParameterkind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
  lHttp.Params.AddItem('Accept','application/json',TRESTRequestParameterkind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

  lHttpQuery.Method := TRESTRequestMethod.rmGet;
  lHttpQuery.Execute;

  try
    lHttpQuery.Execute;
    Response := lHttpResponse.Content;
  finally
    FreeAndNil(lHttp);
    FreeAndNil(lHttpResponse);
    FreeAndNil(lHttpQuery);
  end;

  if Response.IsEmpty then
    exit;

  lj := TGSJsonObject.Create(nil);
  try
    lj.Parse(Response);
    la := lj.Values['results'].AsArray;
    if la.Count>0 then begin //Take first.
      lo := la[0].AsObject.Values['attrs'].AsObject;
      ParcelleLocation.X := lo.Values['x'].AsNumber;
      ParcelleLocation.Y := lo.Values['y'].AsNumber;
      result := ParcelleLocation;
    end;
  finally
    freeAndNil(lj);
  end;
end;

function TGSDataGISGeoData.getAreaGeometryByStringQueryAsJson(aQuery: string): string;
var
  ParcelleLocation: Tpointf;
  ParcelleGeometry: String;
  r : TGSJson;
  ro : TGSJsonObject;
  rt,rt2 : TGSJsonArray;
  l : TArray<TArray<TPointf>>;
  i,j : integer;
begin
  ParcelleLocation := GetParcelleLocation(aQuery);
  l := GetParcelleGeometry(StringReplace(ParcelleLocation.x.ToString,',','.',[]), StringReplace(ParcelleLocation.y.ToString,',','.',[]));
  r := TGSJson.Create;
  try
    rt := TGSJsonArray.Create(nil);
    r.Put('format','agpgeom');
    r.Put('orgineQuery',aQuery);
    r.put('polycount',Length(l));
    for i := 0 to length(l)-1 do begin
      Tlog.info(format('%s.parcelleGeometry array index %d -> %d value(s)',[className,i,length(l[i])]));
      rt2 := rt.add.AsArray;
      for j := 0 to length(l[i])-1 do begin
        rt2.Add.AsNumber := l[i][j].X;
        rt2.Add.AsNumber := l[i][j].Y;
      end;
    end;
    r.Put('results',rt);
  finally
    result := r.Stringify;
    freeandNil(r);
  end;
end;

function TGSDataGISGeoData.GetParcelleGeometry(x,y : string): TArray<TArray<TPointf>>;
var
  lHttp: TRESTClient;
  lHttpQuery : TRESTRequest;
  lHttpResponse : TRESTResponse;

  Response: string;
  lj,lo : TGSJsonObject;
  la,la2,la3,la4 : TGSJsonArray;
  i,j : integer;
begin
  Result := [[]];
  lHttp := TRESTClient.Create(nil);
  lHttpQuery := TRESTRequest.Create(nil);
  lHttpResponse := TRESTResponse.Create(nil);

  lHttpQuery.Client := lHttp;
  lHttpQuery.Response := lHttpResponse;

  lHttp.BaseURL := sysutils.Format(cst_GeometryBaseQuery, [x,y]);

  lHttp.Accept := 'application/json';
  lHttp.Params.AddItem('Content-Type','application/x-www-form-urlencoded',TRESTRequestParameterkind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);
  lHttp.Params.AddItem('Accept','application/json',TRESTRequestParameterkind.pkHTTPHEADER, [TRESTRequestParameterOption.poDoNotEncode]);

  lHttpQuery.Method := TRESTRequestMethod.rmGet;
  lHttpQuery.Execute;

  try
    lHttpQuery.Execute;
    Response := lHttpResponse.Content;
  finally
    FreeAndNil(lHttp);
    FreeAndNil(lHttpResponse);
    FreeAndNil(lHttpQuery);
  end;

  if Response.IsEmpty then
    exit;

  lj := TGSJsonObject.Create(nil);
  try
    lj.Parse(Response);
    la := lj.Values['results'].AsArray;
    if la.Count=0 then
      exit;
    //List of "solution" given by geoData.
    la2 := la[0].AsObject.Values['geometry'].AsObject.Values['coordinates'].AsArray;  //Array of array of coord.
    SetLength(result,la2.Count);
    for j := 0 to la2.Count-1 do begin
      la3 := la2[j].AsArray;  //Array of coord.
      SetLength(result[j],la3.Count);
      for i := 0 to la3.Count-1 do begin
        la4 := la3[i].AsArray;
        Result[j][i].X := la4[0].AsNumber;
        Result[j][i].Y := la4[1].AsNumber;
      end;
    end;
  finally
    freeAndNil(lj);
  end;
end;

end.
