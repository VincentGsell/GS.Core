unit GS.Assets;

interface

uses Classes, SysUtils,
     GS.Common,
     GS.Stream,
     GS.Geometry,
     GS.Geometry.Mesh2D,
     GS.Geometry.Mesh2D.Tools;


Type

TGSAssetType = (gsatText, gsatImage, gsatAtlas, gsatSound, gsatMesh2d, gsaComposed);
TGSAssetString = String;
TGSAssetInt = NativeInt;

IGSAsset = interface
  function GetAssetType: TGSAssetType;
  function GetAssetHumanName: TGSAssetString;
  function GetAssetID: TGSAssetString;
  function GetAssetMemorySize: TGSAssetInt;

  procedure SaveToStream(aStream : TStream);
  procedure LoadFromStream(aStream : TStream);
end;


TGSAssetBase = class(TInterfacedObject, IGSAsset)
protected
  FID : TGUID;
  function GetAssetType: TGSAssetType; virtual; abstract;
  function GetAssetHumanName: TGSAssetString; virtual; abstract;
  function GetAssetID: TGSAssetString; virtual;
  function GetAssetMemorySize: TGSAssetInt; virtual; abstract;
public
  Constructor Create; virtual;
  procedure SaveToStream(aStream : TStream); virtual; abstract;
  procedure LoadFromStream(aStream : TStream); virtual; abstract;

  property AssetType : TGSAssetType read GetAssetType;
  property AssetName : TGSAssetString read GetAssetHumanName;
  property AssetId : TGSAssetString read GetAssetID;
  property AssetMemorySize : TGSAssetInt read GetAssetMemorySize;
end;


TGSAssetObject = class(TGSAssetBase)
end;

TGSAssetComposedObject = Class(TGSAssetBase)
End;


TGSAssetText = class(TGSAssetObject)
private
protected
  FText: String;
  function GetAssetType: TGSAssetType; override;
  function GetAssetHumanName: TGSAssetString; override;
  function GetAssetMemorySize: TGSAssetInt; override;
public
  procedure SaveToStream(aStream : TStream); override;
  procedure LoadFromStream(aStream : TStream); override;

  property Text : String read FText Write FText;
end;

TGSAsset2DMeshedObject = class(TGSAssetObject)
private
protected
  const cstHeaderMagicNumber = -42;
        cstHeaderSign = 'GSMSH';
        cstHeaderV1dot0 = 10;
  var
    fMesh : TGSRawMesh2D;
    fBorders: Vec2sArray; //Raw poly.
    FVersion: Integer;

  function GetAssetType: TGSAssetType; override;
  function GetAssetHumanName: TGSAssetString; override;
  function GetAssetMemorySize: TGSAssetInt; override;

  //Load.
  procedure internalReadAdvMesh(aStream : TStream); //Border and mesh.
  procedure internalWriteAdvMesh(aStream : TStream); //Border and mesh.

  procedure internalReadRawMesh(aStream : TStream); //Mesh only.
  procedure internalReadRawPolys(aStream : TStream); //Borders only.

  procedure internalWriteRawMesh(aStream : TStream); //Mesh only
  procedure internalWriteRawPolys(aStream : TStream); //Borders only.
public
  Constructor Create; override;
  Destructor Destroy; override;
  procedure SaveToStream(aStream : TStream); override;
  procedure LoadFromStream(aStream : TStream); override;

  procedure SaveToStreamMeshPart(aStream : TStream);
  procedure LoadFromStreamMeshPart(aStream : TStream);

  procedure BordersCopyFrom(var aSource : Vec2sArray);

  property MeshData : TGSRawMesh2D read fMesh;
  property Borders : Vec2sArray read fBorders write fBorders;
  property Version : Integer read FVersion;
end;


TGSAssetShapeMesh = class(TGSAsset2DMeshedObject)
end;


TGSAssetSquareMesh = Class(TGSAssetShapeMesh)
private
  FSide: TVecType;
  procedure SetSide(const Value: TVecType);
protected
public
  Constructor Create; override;
  property Side : TVecType read FSide write SetSide;
end;

TGSAssetRoundedShapeMesh = class(TGSAssetShapeMesh)
private
  FRadius: TVecType;
  FSubdi: Uint32;
  FPreShapeModelEnabled: Boolean;
  FPreShapedModel: TGSShape2dType;
  procedure SetRadius(const Value: TVecType);
  procedure SetSubdi(const Value: Uint32);
  procedure InternalBuild;
  procedure SetPreShapeModelEnabled(const Value: Boolean);
  procedure SetPreShapedModel(const Value: TGSShape2dType);
protected
public
  Constructor Create; override;

  property PreShapedModelEnabled : Boolean read FPreShapeModelEnabled write SetPreShapeModelEnabled;
  property PreShapedModel : TGSShape2dType read FPreShapedModel write SetPreShapedModel;
  property Radius : TVecType read FRadius write SetRadius;
  property Subdivision : Uint32 read FSubdi Write SetSubdi;
end;


TGSAssetAtlas = Class(TGSAssetObject)
protected
  names : TList_UTF8String;
  zones : array of vec4;

  function GetAssetType: TGSAssetType; override;
  function GetAssetHumanName: TGSAssetString; override;
  function GetAssetMemorySize: TGSAssetInt; override;
public
  procedure SaveToStream(aStream : TStream); override;
  procedure LoadFromStream(aStream : TStream); override;

  Constructor Create; override;
  Destructor Destroy; override;

  procedure clear;
  procedure addZone(x,y,w,h : TVecType; name : TGSAssetString);
End;

TGSAssetImageFormat = (aifRawARGB32BitFormat);
TGSAssetImageContainer = class(TGSAssetObject)
private
protected
  FImageDesc: TGSAssetString;
  FImageSource: TGSAssetString;
  FBinaryData: TStream;
  FImageFormat: TGSAssetImageFormat;
  FWidth: Uint32;
  FHeight: Uint32;
  function GetAssetType: TGSAssetType; override;
  function GetAssetHumanName: TGSAssetString; override;
  function GetAssetMemorySize: TGSAssetInt; override;
public
  procedure SaveToStream(aStream : TStream); override;
  procedure LoadFromStream(aStream : TStream); override;

  Constructor Create; override;
  Destructor Destroy; override;
  property ImageFormat : TGSAssetImageFormat read FImageFormat write FImageFormat;
  property BinaryData : TStream read FBinaryData;
  property ImageDescription : TGSAssetString read FImageDesc write FImageDesc;
  property AssetImageSource : TGSAssetString read FImageSource write FImageSource;
  property Width : Uint32 read FWidth write FWidth;
  property Height : Uint32 read FHeight write FHeight;
end;

//Composed based.
TGSAssetComposed = class(TGSAssetComposedObject)
  private
    function GetAsset(Index: UInt32): TGSAssetObject;
    function GetAssetCount: UInt32;
    function GetAssetTotalMemory: TGSAssetInt;
protected
  FComposition : TList_ObjectArray;

  function GetAssetType: TGSAssetType; override;
  function GetAssetMemorySize: TGSAssetInt; override;
public
  procedure SaveToStream(aStream : TStream); override;
  procedure LoadFromStream(aStream : TStream); override;
  Constructor Create; override;
  Destructor Destroy; override;

  procedure addAsset(asset : TGSAssetObject);
  property Assets[Index : UInt32] : TGSAssetObject read GetAsset;
  property AssetsCount : UInt32 read GetAssetCount;
  property AssetsTotalMemory : TGSAssetInt read GetAssetTotalMemory;
end;

///
///
///  COMPOSED OBJECT
///
///
///


///
///  Atlas Image source.
///
TGSAssetAtlasImage = class(TGSAssetComposed)
protected
  FImage: TGSAssetImageContainer;
  FAtlas: TGSAssetAtlas;
  function GetAssetHumanName: TGSAssetString; override;
public
  Constructor Create; override;
  property Image : TGSAssetImageContainer read FImage; //Original image.
  property Atlas :TGSAssetAtlas read FAtlas; //Atlas
end;

///
///  Image source.
///

TGSAssetImageSource = Class(TGSAssetAtlasImage)
protected
  FShape: TGSAssetSquareMesh;
  function GetAssetHumanName: TGSAssetString; override;
public
  Constructor Create; override;
  property Shape : TGSAssetSquareMesh read FShape write FShape;
End;


var
  GSAssetObjectClass : array of TClass;
implementation

procedure RegisterGSAssetObject(objectClass : TClass);
var i : Integer;
begin
  i := length(GSAssetObjectClass);
  setlength(GSAssetObjectClass,i+1);
  GSAssetObjectClass[i] := objectClass;
end;

function getGSAssetObjectImplementationFromName(_Classname : String) : TGSAssetObject;
var i : integer;
    c : String;
begin
  result := nil;
  c := UpperCase(_Classname);
  for i:= 0 to Length(GSAssetObjectClass)-1 do
    if UpperCase(GSAssetObjectClass[i].ClassName) = c then
    begin
      result := TGSAssetObject(GSAssetObjectClass[i].Create);
      break
    end;
  assert(assigned(result));
end;

{ TGSAsset }


{ TGSAssetBase }

constructor TGSAssetBase.Create;
begin
  inherited Create;
  CreateGUID(FID);
end;

function TGSAssetBase.GetAssetID: TGSAssetString;
begin
  result := FID.ToString;
end;


{ TGSAssetAtlas }

procedure TGSAssetAtlas.addZone(x, y, w, h: TVecType; name: TGSAssetString);
var l : int32;
begin
  l := length(zones);
  SetLength(zones,l+1);
  zones[l].Left := x;
  zones[l].top := y;
  zones[l].right := x+w;
  zones[l].bottom := y+h;
  names.Add(UTF8String(name));
end;

procedure TGSAssetAtlas.clear;
begin
  SetLength(zones,0);
  names.Clear;
end;

constructor TGSAssetAtlas.Create;
begin
  inherited create;
  names :=TList_UTF8String.Create(false);
  clear;
end;

destructor TGSAssetAtlas.Destroy;
begin
  clear;
  freeAndNil(names);
  inherited;
end;

function TGSAssetAtlas.GetAssetHumanName: TGSAssetString;
begin
  result := 'Atlas - rect geo location (zone)';
end;

function TGSAssetAtlas.GetAssetMemorySize: TGSAssetInt;
begin
  result := length(zones) * SizeOf(vec4);
end;

function TGSAssetAtlas.GetAssetType: TGSAssetType;
begin
  result := TGSAssetType.gsatAtlas;
end;

procedure TGSAssetAtlas.LoadFromStream(aStream: TStream);
var i, fz : Int32;
begin
  fz := ReadInt32(aStream);
  for i := 0 to fz-1 do
    names.Add(UTF8String(ReadString(aStream)));
  fz := ReadInt32(aStream);
  SetLength(zones,fz);
  for i := 0 to Length(zones)-1 do
  begin
    zones[i].x := ReadDouble(aStream);
    zones[i].y := ReadDouble(aStream);
    zones[i].z := ReadDouble(aStream);
    zones[i].w := ReadDouble(aStream);
  end;
end;

procedure TGSAssetAtlas.SaveToStream(aStream: TStream);
var i : integer;
begin
  WriteInt32(aStream,names.Count);
  for I := 0 to names.Count-1 do
    WriteString(aStream,String(names[i]));
  WriteInt32(aStream,Length(zones));
  for i := 0 to length(zones)-1 do
  begin
    WriteDouble(aStream,zones[i].x);
    WriteDouble(aStream,zones[i].y);
    WriteDouble(aStream,zones[i].z);
    WriteDouble(aStream,zones[i].w);
  end;
end;

{ TGSAssetComposed }

procedure TGSAssetComposed.addAsset(asset: TGSAssetObject);
begin

end;

constructor TGSAssetComposed.Create;
begin
  inherited create;
  FComposition := TList_ObjectArray.Create(True);
end;

destructor TGSAssetComposed.Destroy;
begin
  FreeAndNil(FComposition);
  inherited;
end;

function TGSAssetComposed.GetAsset(Index: UInt32): TGSAssetObject;
begin
  Assert(Index<FComposition.Count);
  result := TGSAssetObject(FComposition[Index]);
end;

function TGSAssetComposed.GetAssetCount: UInt32;
begin
  result := FComposition.Count;
end;


function TGSAssetComposed.GetAssetMemorySize: TGSAssetInt;
begin
  result := GetAssetTotalMemory;
end;

function TGSAssetComposed.GetAssetTotalMemory: TGSAssetInt;
var i : integer;
begin
  result := 0;
  for i := 0 to FComposition.Count-1 do
    inc(result,TGSAssetObject(FComposition[i]).AssetMemorySize);
end;

function TGSAssetComposed.GetAssetType: TGSAssetType;
begin
  result := TGSAssetType.gsaComposed;
end;

procedure TGSAssetComposed.LoadFromStream(aStream: TStream);
var i,l : Int32;
    s : string;
    o : TGSAssetObject;
begin
  l := ReadInt32(aStream);
  for i := 0 to l-1 do
  begin
    s := ReadString(aStream);
    o := getGSAssetObjectImplementationFromName(s);
    o.LoadFromStream(aStream);
  end;
end;

procedure TGSAssetComposed.SaveToStream(aStream: TStream);
var i : integer;
begin
  WriteInt32(aStream,FComposition.Count);
  for i := 0 to FComposition.Count-1 do
  begin
    WriteString(aStream,TGSAssetObject(FComposition[i]).ClassName);
    TGSAssetObject(FComposition[i]).SaveToStream(aStream);
  end;
end;

{ TGSAssetSquareMesh }

constructor TGSAssetSquareMesh.Create;
begin
  inherited create;
  Side := 2.0;
end;

procedure TGSAssetSquareMesh.SetSide(const Value: TVecType);
begin
  FSide := Value;
  FMesh.SetUpQuad(FSide);
end;

{ TGSAssetText }

function TGSAssetText.GetAssetHumanName: TGSAssetString;
begin
  result := 'Text (UTF8 text data)'
end;

function TGSAssetText.GetAssetMemorySize: TGSAssetInt;
begin
  result := SizeOf(FText);
end;

function TGSAssetText.GetAssetType: TGSAssetType;
begin
  result := TGSAssetType.gsatText;
end;


procedure TGSAssetText.LoadFromStream(aStream: TStream);
begin
  FText := ReadString(aStream);
end;

procedure TGSAssetText.SaveToStream(aStream: TStream);
begin
  WriteString(aStream, FText);
end;

{ TGSAsset2DMeshedObject }

procedure TGSAsset2DMeshedObject.BordersCopyFrom(var aSource: Vec2sArray);
begin
  _vect2sArrayCopy(aSource,fBorders);
end;

constructor TGSAsset2DMeshedObject.Create;
begin
  inherited;
  fMesh := TGSRawMesh2D.Create;
end;

destructor TGSAsset2DMeshedObject.Destroy;
begin
  FreeAndNil(fmesh);
  inherited;
end;

function TGSAsset2DMeshedObject.GetAssetHumanName: TGSAssetString;
begin
  result := '2D Mesh - Raw data (vertices, UVs, indexes)';
end;

function TGSAsset2DMeshedObject.GetAssetMemorySize: TGSAssetInt;
begin
  result := length(fmesh.vertices) * sizeof(vec2) +
            SizeOf(fmesh.indexes) * sizeof(uint32) +
            SizeOf(fmesh.uvs) * sizeof(vec2);
end;

function TGSAsset2DMeshedObject.GetAssetType: TGSAssetType;
begin
  result := TGSAssetType.gsatMesh2d;
end;

procedure TGSAsset2DMeshedObject.internalReadRawPolys(aStream : TStream); //Borders.
var i,j : integer;
    lpc,luc : Int32;
    lindex : Integer;
begin
  lpc := ReadInt32(aStream); //Polys count.
  lindex := length(fBorders); //Merge compatible.
  SetLength(fBorders,lindex+lpc);
  for i := 0 to lpc-1 do begin
    luc := ReadInt32(aStream); //units count.
    SetLength(fBorders[lindex+i],luc);
    for j := 0 to luc-1 do begin
      fBorders[lindex+i][j].x := ReadDouble(aStream);
      fBorders[lindex+i][j].y := ReadDouble(aStream);
    end;
  end;
end;

procedure TGSAsset2DMeshedObject.internalWriteRawPolys(aStream : TStream); //Borders.
var i,j : integer;
begin
  WriteInt32(aStream,length(fBorders));
  for i := 0 to length(fBorders)-1 do begin
    WriteInt32(aStream,Length(fBorders[i]));
    for j := 0 to length(fBorders[i])-1 do begin
       WriteDouble(aStream,fBorders[i][j].x);
       WriteDouble(aStream,fBorders[i][j].y);
    end;
  end;
end;



procedure TGSAsset2DMeshedObject.internalWriteRawMesh(aStream: TStream);
var i : integer;
begin
  WriteInt32(aStream,Length(fMesh.vertices));
  WriteInt32(aStream,Length(fMesh.uvs));
  WriteInt32(aStream,Length(fMesh.indexes));
  for i := 0 to length(FMesh.vertices)-1 do
  begin
    WriteDouble(aStream,FMesh.vertices[i].x);
    WriteDouble(aStream,FMesh.vertices[i].y);
  end;
  for i := 0 to length(FMesh.uvs)-1 do
  begin
    WriteDouble(aStream,FMesh.uvs[i].x);
    WriteDouble(aStream,FMesh.uvs[i].y);
  end;
  for i := 0 to length(FMesh.indexes)-1 do
  begin
    WriteInt32(aStream,FMesh.indexes[i]);
    WriteInt32(aStream,FMesh.indexes[i]);
  end;
end;

procedure TGSAsset2DMeshedObject.internalReadRawMesh(aStream: TStream);
var i : integer;
    fv,fu,fi : Int32;
    lvi, lii : Int32;
begin
  fv := ReadInt32(aStream);
  fu := ReadInt32(aStream);
  fi := ReadInt32(aStream);

  lvi := length(fMesh.vertices);
  lii := length(fMesh.indexes);

  SetLength(fMesh.vertices,lvi+fv);
  SetLength(fMesh.uvs,fu);
//  SetLength(fMesh.cols,fu);
  SetLength(fMesh.indexes,lii+fi);
  fBorders := Nil;

  for i := 0 to Length(fMesh.vertices)-1 do
  begin
    fmesh.vertices[lvi+i].x := ReadDouble(aStream);
    fmesh.vertices[lvi+i].y := ReadDouble(aStream);
  end;
  for i := 0 to length(FMesh.uvs)-1 do
  begin
    fmesh.uvs[i].x := ReadDouble(aStream);
    fmesh.uvs[i].y := ReadDouble(aStream);
  end;
  for i := 0 to length(FMesh.indexes)-1 do
  begin
    fmesh.indexes[lii+i] := ReadInt32(aStream);
    fmesh.indexes[lii+i] := ReadInt32(aStream);
  end;
end;


procedure TGSAsset2DMeshedObject.internalReadAdvMesh(aStream: TStream);
begin
  //header.
  ReadInt32(aStream);
  ReadString(aStream);
  FVersion := ReadInt32(aStream);
  //Payload.
  internalReadRawPolys(aStream);
  internalReadRawMesh(aStream);
end;

procedure TGSAsset2DMeshedObject.internalWriteAdvMesh(aStream: TStream);
begin
  Writeint32(aStream,cstHeaderMagicNumber);
  WriteString(aStream,cstHeaderSign);
  Writeint32(aStream,cstHeaderV1dot0);
  internalWriteRawPolys(aStream);
  internalWriteRawMesh(aStream);
end;

procedure TGSAsset2DMeshedObject.LoadFromStream(aStream: TStream);
  function IsAssetMeshFileHeader : boolean;
  var l : NativeInt;
  begin
    l := aStream.Position;
    FVersion := 10; //Simplest file : Mesh only.
    result := (ReadInt32(aStream)= cstHeaderMagicNumber) And (ReadString(aStream) = cstHeaderSign);
    aStream.Position := l;
  end;
begin
  Assert(aStream.size>0);
  if IsAssetMeshFileHeader then
    internalReadAdvMesh(aStream)
  else
    raise Exception.Create('TGSAsset2DMeshedObject.LoadFromStream Error : Wrong file type');
end;

procedure TGSAsset2DMeshedObject.SaveToStream(aStream: TStream);
begin
  internalWriteAdvMesh(aStream);
end;

procedure TGSAsset2DMeshedObject.LoadFromStreamMeshPart(aStream: TStream);
begin
  internalReadRawMesh(aStream);
end;

procedure TGSAsset2DMeshedObject.SaveToStreamMeshPart(aStream: TStream);
begin
  internalWriteRawMesh(aStream);
end;

{ TGSAssetImageContainer }

constructor TGSAssetImageContainer.Create;
begin
  inherited;
  FBinaryData := TMemoryStream.Create;
end;

destructor TGSAssetImageContainer.Destroy;
begin
  freeAndNil(FBinaryData);
  inherited;
end;

function TGSAssetImageContainer.GetAssetHumanName: TGSAssetString;
begin
  result := 'Image - Raw binary data of a image format';
end;

function TGSAssetImageContainer.GetAssetMemorySize: TGSAssetInt;
begin
  result := FBinaryData.Size;
end;

function TGSAssetImageContainer.GetAssetType: TGSAssetType;
begin
  result := TGSAssetType.gsatImage;
end;

procedure TGSAssetImageContainer.LoadFromStream(aStream: TStream);
begin
  FImageDesc := ReadString(aStream);
  FImageSource := ReadString(aStream);
  FImageFormat := TGSAssetImageFormat(Readbyte(aStream));
  FWidth := ReadUint32(aStream);
  FHeight := ReadUint32(aStream);
  ReadStream(aStream,FBinaryData);
end;

procedure TGSAssetImageContainer.SaveToStream(aStream: TStream);
begin
  WriteString(aStream,FImageDesc);
  WriteString(aStream,FImageSource);
  Writebyte(aStream,Byte(FImageFormat));
  WriteUInt32(aStream,FWidth);
  WriteUInt32(aStream,FHeight);
  WriteStream(aStream,FBinaryData);
end;

{ TGSAssetAtlasImage }

constructor TGSAssetAtlasImage.Create;
begin
  inherited;
  FImage:= TGSAssetImageContainer.Create;
  FAtlas:= TGSAssetAtlas.Create;
  FComposition.Add(FImage);
  FComposition.Add(FAtlas);
end;

function TGSAssetAtlasImage.GetAssetHumanName: TGSAssetString;
begin
  result := 'Image (Image binary source, atlas mapping';
end;


{ TGSAssetImageSource }

constructor TGSAssetImageSource.Create;
begin
  inherited Create;
  FShape := TGSAssetSquareMesh.Create;
  FComposition.Add(FShape);
end;

function TGSAssetImageSource.GetAssetHumanName: TGSAssetString;
begin
  result := 'Image (Image binary source, atlas mapping, uv';
end;

{ TGSAssetRoundedShapeMesh }

constructor TGSAssetRoundedShapeMesh.Create;
begin
  inherited;
  FPreShapedModel := TGSShape2dType.hexa;
  FPreShapeModelEnabled := true;
  FSubdi := 1;
  FRadius := 1;
  InternalBuild;
end;

procedure TGSAssetRoundedShapeMesh.InternalBuild;
begin
  if FPreShapeModelEnabled then
    fMesh.SetShapeType(FPreShapedModel)
  else
  begin
    fmesh.Build_SetRoundShape(FSubdi,FRadius);
  end;
end;

procedure TGSAssetRoundedShapeMesh.SetPreShapedModel(const Value: TGSShape2dType);
begin
  FPreShapedModel := Value;
  InternalBuild;
end;

procedure TGSAssetRoundedShapeMesh.SetPreShapeModelEnabled(const Value: Boolean);
begin
  FPreShapeModelEnabled := Value;
  InternalBuild;
end;

procedure TGSAssetRoundedShapeMesh.SetRadius(const Value: TVecType);
begin
  FRadius := Value;
  InternalBuild;
end;

procedure TGSAssetRoundedShapeMesh.SetSubdi(const Value: Uint32);
begin
  FSubdi := Value;
  if  FSubdi<3 then
    FSubdi := 3;
  InternalBuild;
end;

end.
