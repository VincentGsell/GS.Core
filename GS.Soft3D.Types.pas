unit GS.Soft3D.Types;

interface

uses Classes,
     SysUtils,
     contnrs,
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
    normals : array of vec3;

    procedure clear;

    function verticesArraySize : Uint32;
    procedure setVerticesArraySize(newSize : Uint32);
    function indicesArraySize : Uint32;
    procedure setIndicesArraySize(newSize : Uint32);
    function NormalArraySize : Uint32;
    procedure setNormalsArraySize(newSize : Uint32);

    procedure copy(var destination : TMeshData3D);
  private
  End;

  TBaseVertexArray = array of vec3;
//  TBaseTriangle = record VertexIndiceA,VertexIndiceB,VertexIndiceC : integer; uvA, uvB, uvC : TCoord; end;
//  TBaseTriangle3D = record VertexA,VertexB,VertexC : vec3; uvA, uvB, uvC : TCoord; end;

  TBase3D = class
  private
  protected
  public
    x,y,z : single;    //position.
    rx,ry,rz : single; //Rotation (self)
    DefaultColor : Vec4;
    Enabled : boolean;
    Constructor Create; virtual;

    function TransformationMatrix : Mat4;
  end;

  TMesh3D = class
  protected
  public
    meshData : TMeshData3D;
    constructor Create; virtual;
    destructor Destroy; override;

    procedure addVertex(x,y,z : single); overload;  //todo Normal
    procedure addVertex(const map : TBaseVertexArray); overload; //todo Normal
    procedure addTriangle(i1,i2,i3 : uint32; u,v,u1,v1,u2,v2 : integer); overload;
    procedure addQuad(i1,i2,i3,i4 : uint32);
    procedure meshScale(factor : single); //?
  end;

  TS3DMeshList = Class(TObjectList)
  private
    function GetMesh(Index: Uint32): TMesh3D;
    procedure SetMesh(Index: Uint32; const Value: TMesh3D);
  public
    procedure AddMesh(mesh : TMesh3D);
    property Mesh[Index : Uint32] : TMesh3D read GetMesh write SetMesh; default;
  End;

  TS3DObject = Class(TBase3D)
    MeshAsset : TMesh3D; //Pointer.
  End;

  TS3DObjectList = Class(TObjectList)
  private
    function GetObj(Index: UInt32): TS3DObject;
    procedure SetObj(Index: UInt32; const Value: TS3DObject);
  public
    procedure AddObject(_obj : TS3DObject);
    property  Objects3D[Index : UInt32] : TS3DObject read GetObj Write SetObj; default;
  End;


implementation

{ TBase3D }

constructor TBase3D.create;
begin
  inherited Create;
  x := 0;
  y := x;
  z := y;
  rx := 0;
  ry := 0;
  rz := 0;
  DefaultColor.create(1,0,0,1);
  Enabled := true;
end;

function TBase3D.TransformationMatrix: Mat4;
var ObjTransMatrix, ObjRotX,ObjRotY,OBjRotZ : Mat4;
begin
  objTransMatrix := mat4CreateTranslation(vec3.create(x,y,z));
  objRotX := mat4CreateRotationX(_radians(rx));
  objRotY := mat4CreateRotationY(_radians(ry));
  objRotZ := mat4CreateRotationZ(_radians(rz));

  Result := objRotX;
  Result := Result * mat4CreateTranslation(vec3.create(0,0,0));
  Result := Result * objRotY;
  Result := Result * mat4CreateTranslation(vec3.create(0,0,0));
  Result := Result * objRotZ;
  Result := Result * mat4CreateTranslation(vec3.create(0,0,0));
  Result := Result * objTransMatrix;
end;

{ TMeshData3D }

procedure TMeshData3D.clear;
begin
  vertices := nil;
  indices := nil;
  normals := nil;
  UVMap := nil;
//  FillChar(vertices,(length(vertices)-1)*SizeOf(TVertice3D),0);
//  FillChar(Indices,length(Indices)-1,0);
end;

procedure TMeshData3D.copy(var destination: TMeshData3D);
begin
  SetLength(destination.vertices,length(vertices));
  SetLength(destination.normals,length(normals));
  SetLength(destination.UVMap,length(vertices));
  SetLength(destination.indices,length(indices));

  Move(vertices[0],destination.vertices[0],length(vertices)*sizeOf(vec3));
  Move(normals[0],destination.normals[0],length(normals)*sizeOf(vec3));
  Move(UVMap[0],destination.UVMap[0],length(UVMap)*sizeOf(TCoord));
  Move(indices[0],destination.indices[0],length(indices)*SizeOf(Uint32));
end;

function TMeshData3D.indicesArraySize: Uint32;
begin
  result := Length(Indices);
end;

function TMeshData3D.NormalArraySize: Uint32;
begin
  result:=Length(Normals);
end;

procedure TMeshData3D.setIndicesArraySize(newSize: Uint32);
begin
  SetLength(Indices,newSize);
end;

procedure TMeshData3D.setNormalsArraySize(newSize: Uint32);
begin
  SetLength(normals,newSize);
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

{ TS3DMeshList }

procedure TS3DMeshList.AddMesh(mesh: TMesh3D);
begin
  if IndexOf(mesh)=-1 then
    Add(mesh);
end;

function TS3DMeshList.GetMesh(Index: Uint32): TMesh3D;
begin
  result := TMesh3D(Items[Index]);
end;

procedure TS3DMeshList.SetMesh(Index: Uint32; const Value: TMesh3D);
begin
  Items[Index] := Value;
end;



{ TS3DObjectList }

procedure TS3DObjectList.AddObject(_obj: TS3DObject);
begin
  add(_Obj);
end;

function TS3DObjectList.GetObj(Index: UInt32): TS3DObject;
begin
  result := TS3DObject(Items[Index]);
end;

procedure TS3DObjectList.SetObj(Index: UInt32; const Value: TS3DObject);
begin
  Items[Index] := Value;
end;

end.
