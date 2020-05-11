unit GS.Soft3D.Types;

interface

uses Classes,
     SysUtils,
     GS.Geometry;

Type
  TS3DProjectionType = (Perspective, OrthographicIsometric);

  TCoord = record u,v : single; end;
  TPoint2D = record x, y : single; end;
  TPoint2Di = record
    x, y : Integer;
    property width : Integer read x write x;
    property height : Integer read y write y;
  end;

  TMeshData3D = Record
    vertices : array of vec3; //...
    UVMap : array of TCoord;  //...Synchro.
    indices : array of Uint32;

    procedure clear;

    function verticesArraySize : Uint32;
    procedure setVerticesArraySize(newSize : Uint32);
    function indicesArraySize : Uint32;
    procedure setIndicesArraySize(newSize : Uint32);

    procedure copy(var destination : TMeshData3D);
  private
  End;

  TBase3D = class
  private
  public
    x,y,z : single;    //position.
    rx,ry,rz : single; //Rotation (self)
    Enabled : boolean;
    constructor Create; virtual;
  end;

  TBaseVertexArray = array of vec3;
  TBaseTriangle = record VertexIndiceA,VertexIndiceB,VertexIndiceC : integer; uvA, uvB, uvC : TCoord; end;
  TBaseTriangle3D = record VertexA,VertexB,VertexC : vec3; uvA, uvB, uvC : TCoord; end;

  TMesh3D = class(TBase3D)
  protected
  public
    meshData : TMeshData3D;

    constructor Create; override;
    destructor Destroy; override;

    procedure addVertex(x,y,z : single); overload;  //todo Normal
    procedure addVertex(const map : TBaseVertexArray); overload; //todo Normal
    procedure addTriangle(i1,i2,i3 : uint32; u,v,u1,v1,u2,v2 : integer); overload;
    procedure addQuad(i1,i2,i3,i4 : uint32);

    procedure meshScale(factor : single); //?
  end;


implementation

{ TBase3D }

constructor TBase3D.create;
begin
  x := 0;
  y := x;
  z := y;
  rx := 0;
  ry := 0;
  rz := 0;
  Enabled := true;
end;

{ TMeshData3D }

procedure TMeshData3D.clear;
begin
//  FillChar(vertices,(length(vertices)-1)*SizeOf(TVertice3D),0);
//  FillChar(Indices,length(Indices)-1,0);
end;

procedure TMeshData3D.copy(var destination: TMeshData3D);
begin
  SetLength(destination.vertices,length(vertices));
  SetLength(destination.UVMap,length(vertices));
  SetLength(destination.indices,length(indices));

  Move(vertices[0],destination.vertices[0],length(vertices)*sizeOf(vec3));
  Move(UVMap[0],destination.UVMap[0],length(UVMap)*sizeOf(TCoord));
  Move(indices[0],destination.indices[0],length(indices)*SizeOf(Uint32));
end;

function TMeshData3D.indicesArraySize: Uint32;
begin
  result := Length(Indices);
end;

procedure TMeshData3D.setIndicesArraySize(newSize: Uint32);
begin
  SetLength(Indices,newSize);
end;

procedure TMeshData3D.setVerticesArraySize(newSize: Uint32);
begin
  SetLength(vertices,newSize);
  SetLength(UVMap,newSize);
end;

function TMeshData3D.verticesArraySize: Uint32;
begin
  result := Length(vertices);
end;


{ TMesh3D }

procedure TMesh3D.addQuad(i1, i2, i3, i4: uint32);
begin
  addTriangle(i1,i2,i3,0,0,1,0,1,1);
  addTriangle(i4,i1,i3,0,1,0,0,1,1);
end;

procedure TMesh3D.addTriangle(i1, i2, i3: uint32; u,v,u1,v1,u2,v2 : integer);
var l : uint32;
begin
  assert(i1<meshData.verticesArraySize);
  assert(i2<meshData.verticesArraySize);
  assert(i3<meshData.verticesArraySize);
  l := meshData.indicesArraySize;
  meshData.setIndicesArraySize(l+3);
  meshData.Indices[l] := i1;
  meshData.Indices[l+1] := i2;
  meshData.Indices[l+2] := i3;
  meshData.UVMap[l].u := u;
  meshData.UVMap[l].v := v;
  meshData.UVMap[l+1].u := u1;
  meshData.UVMap[l+1].v := v1;
  meshData.UVMap[l+2].u := u2;
  meshData.UVMap[l+2].v := v2;
end;

procedure TMesh3D.addVertex(x, y, z: single);
var l : integeR;
begin
  l := meshData.verticesArraySize;
  meshData.setVerticesArraySize(l+1);
  meshData.vertices[l] := vec3.create(x,y,z);
end;

procedure TMesh3D.addVertex(const map: TBaseVertexArray);
var i,l : integer;
begin
  assert(length(map)>0);
  l := meshData.verticesArraySize-1;
  meshData.setVerticesArraySize(meshData.verticesArraySize + length(map));
  for i := l to meshData.verticesArraySize-1 do
    meshData.Vertices[i] := vec3.create(map[i-l].x,map[i-l].y,map[i-l].z);
end;

constructor TMesh3D.create;
begin
  inherited;
  meshData.Clear;
end;

destructor TMesh3D.destroy;
begin
  inherited;
end;

procedure TMesh3D.meshScale(factor: single);
var i : integer;
begin
  for I := 0 to meshData.verticesArraySize-1 do
    meshData.Vertices[i] := meshData.Vertices[i] * factor;
end;

end.
