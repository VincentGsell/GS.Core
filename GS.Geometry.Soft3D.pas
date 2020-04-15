{ -----------------------------------------------------------------------------
    This program is free software: Under statement of join file README - LGPL.txt
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-----------------------------------------------------------------------------
 Unit Name : GS.Pixel
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic Pixel class introduced by interface.
 Date:     : 2020031
 History   :
 20200301 - Creating unit.

Credits :
https://learnopengl.com/Getting-started/Coordinate-Systems
https://www.scratchapixel.com/lessons/3d-basic-rendering/computing-pixel-coordinates-of-3d-point/mathematics-computing-2d-coordinates-of-3d-points

Description :
3D standalone software 3D system. Mainly for learning purpose, this unit
is volountary kept simple.

-----------------------------------------------------------------------------}
{$I GSCore.Inc}

unit GS.Geometry.Soft3D;
interface

uses SysUtils,
     classes,
     contNrs,
     System.Math,
     System.Math.Vectors,
     GS.Pixel;

Type
  TCoord = record u,v : single; end;
  TPoint2D = record x, y : single; end;
  TMeshData3D = Record
    vertices : array of TPoint3D;
    indices : array of Uint32;
    UVMap : array of TCoord;

    procedure clear;

    function verticesArraySize : Uint32;
    procedure setVerticesArraySize(newSize : Uint32);
    function indicesArraySize : Uint32;
    procedure setIndicesArraySize(newSize : Uint32);
  private
  End;

  TBase3D = class
  private
    fx,fy,fz : single;
    fEnabled : boolean;
  public
    constructor Create; virtual;
  end;

  TBaseVertexArray = array of TPoint3D;
  TBaseTriangle = record VertexIndiceA,VertexIndiceB,VertexIndiceC : integer; uvA, uvB, uvC : TCoord; end;
  TBaseTriangle3D = record VertexA,VertexB,VertexC : TPoint3D; uvA, uvB, uvC : TCoord; end;

  TMesh3D = class(TBase3D)
  private
    fmeshData : TMeshData3D;
    ftransformed : TMeshData3D;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure addVertex(x,y,z : single); overload;  //todo Normal
    procedure addVertex(const map : TBaseVertexArray); overload; //todo Normal
    procedure addTriangle(i1,i2,i3 : uint32; u,v,u1,v1,u2,v2 : integer);
    procedure addQuad(i1,i2,i3,i4 : uint32);

    procedure meshScale(factor : single);

    function triangleCount : Integer;
    function triangles(TriIndice : integer) : TBaseTriangle;
    function triangles3D(TriIndice : integer) : TBaseTriangle3D;

    procedure transform(_matrix : TMatrix3D);
  end;

  TPlane = class(TMesh3d)
  public
    constructor create; override;
  end;

  TCube = class(TMesh3d)
  public
    constructor create; override;
  end;

  TCamera = class(TBase3D)
  public
  end;

  TViewportProjectionType = (Perspective, PerspectiveIsometric, OrthographicIsometric);

  TViewport = class
  private
    FList : TObjectList; //To do : Separate list form viewport.
    GTest : single;
    FCamZ: single;
    FWireframe: boolean;
    Frasterframe: boolean;
    FProjection: TViewportProjectionType;

  public
    TargetCanvas : iPixSurface;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure addMesh(_mesh : TMesh3d);
    function addCube(x,y,z : single) : TCube;
    function addPlane(x,y,z : single) : TPlane;

    procedure Execute;

    Property CameraZ : single read FCamZ Write FCamZ;

    property rasterFrame : boolean read Frasterframe write fRasterframe;
    property wireFrame : boolean read FWireframe write FWireframe;

    property Projection : TViewportProjectionType read FProjection write FProjection;

  end;




implementation


{ TBase3D }

constructor TBase3D.create;
begin
  fx := 0;
  fy := fx;
  fz := fy;
  fEnabled := true;
end;

{ TMesh3D }

procedure TMesh3D.addQuad(i1, i2, i3, i4: uint32);
begin
  addTriangle(i1,i2,i3,0,0,1,0,1,1);
  addTriangle(i3,i4,i1,1,1,0,1,0,0);
end;

procedure TMesh3D.addTriangle(i1, i2, i3: uint32; u,v,u1,v1,u2,v2 : integer);
var l : uint32;
begin
  assert(i1<fmeshData.verticesArraySize);
  assert(i2<fmeshData.verticesArraySize);
  assert(i3<fmeshData.verticesArraySize);
  l := fmeshData.indicesArraySize;
  fmeshData.setIndicesArraySize(l+3);
  fmeshData.Indices[l] := i1;
  fmeshData.Indices[l+1] := i2;
  fmeshData.Indices[l+2] := i3;
  fmeshData.UVMap[l].u := u;
  fmeshData.UVMap[l].v := v;
  fmeshData.UVMap[l+1].u := u1;
  fmeshData.UVMap[l+1].v := v1;
  fmeshData.UVMap[l+2].u := u2;
  fmeshData.UVMap[l+2].v := v2;
end;

procedure TMesh3D.addVertex(x, y, z: single);
var l : integeR;
begin
  l := fmeshData.verticesArraySize;
  fmeshData.setVerticesArraySize(l+1);
  fmeshData.vertices[l] := Point3d(x,y,z);
end;

procedure TMesh3D.addVertex(const map: TBaseVertexArray);
var i,l : integer;
begin
  assert(length(map)>0);
  l := fmeshData.verticesArraySize-1;
  fmeshData.setVerticesArraySize(fmeshData.verticesArraySize + length(map));
  for i := l to fmeshData.verticesArraySize-1 do
    fmeshData.Vertices[i] := Point3d(map[i-l].x,map[i-l].y,map[i-l].z);
end;

constructor TMesh3D.create;
begin
  inherited;
  fmeshData.Clear;
end;

destructor TMesh3D.destroy;
begin
  inherited;
end;

procedure TMesh3D.meshScale(factor: single);
var i : integer;
begin
  for I := 0 to fmeshData.verticesArraySize-1 do
    fmeshData.Vertices[i] := fmeshData.Vertices[i] * factor;
end;


procedure TMesh3D.transform(_matrix: TMatrix3D);
var i : integer;
begin
  ftransformed.setVerticesArraySize(fmeshData.verticesArraySize);
  ftransformed.setIndicesArraySize(fmeshData.indicesArraySize);
  for i := 0 to fmeshData.verticesArraySize-1 do
  begin
    ftransformed.Vertices[i] := fmeshData.Vertices[i] * _matrix;

    //normalized device coordinates
    ftransformed.Vertices[i].x := ftransformed.Vertices[i].x / _matrix.m43;
    ftransformed.Vertices[i].y := ftransformed.Vertices[i].y / _matrix.m43;
    ftransformed.Vertices[i].z := ftransformed.Vertices[i].z / _matrix.m43;

    //ftransformed.IndexBuffer.Indices[i] := fmeshData.IndexBuffer.Indices[i]; //need for calcNormal.
  end;
//  ftransformed.CalcFaceNormals;
end;

function TMesh3D.triangleCount: Integer;
begin
  result := fmeshData.indicesArraySize div 3;
end;

function TMesh3D.triangles(TriIndice: integer): TBaseTriangle;
var l : integer;
begin
  l := TriIndice * 3;
  Assert(l<fmeshData.indicesArraySize-2,'Triangles indice overflow');
  result.VertexIndiceA := fmeshData.indices[l];
  result.VertexIndiceB := fmeshData.indices[l+1];
  result.VertexIndiceC := fmeshData.indices[l+2];
  Result.uvA := fmeshData.UVMap[l];
  Result.uvB := fmeshData.UVMap[l+1];
  Result.uvC := fmeshData.UVMap[l+2];
end;

function TMesh3D.triangles3D(TriIndice: integer): TBaseTriangle3D;
var l : TBaseTriangle;
begin
  l := triangles(TriIndice);

  result.VertexA.x := ftransformed.vertices[l.VertexIndiceA].X;
  result.VertexA.y := ftransformed.vertices[l.VertexIndiceA].Y;
  result.VertexA.z := ftransformed.vertices[l.VertexIndiceA].Z;

  result.VertexB.x := ftransformed.vertices[l.VertexIndiceB].X;
  result.VertexB.y := ftransformed.vertices[l.VertexIndiceB].Y;
  result.VertexB.z := ftransformed.vertices[l.VertexIndiceB].Z;

  result.VertexC.x := ftransformed.vertices[l.VertexIndiceC].X;
  result.VertexC.y := ftransformed.vertices[l.VertexIndiceC].Y;
  result.VertexC.z := ftransformed.vertices[l.VertexIndiceC].Z;

  Result.uvA := fmeshData.UVMap[l.VertexIndiceA];
  Result.uvB := fmeshData.UVMap[l.VertexIndiceB];
  Result.uvC := fmeshData.UVMap[l.VertexIndiceC];

end;

{ TViewport }

function TViewport.addCube(x, y, z :  single) : TCube;
begin
  Result :=  TCube.Create;
  Result.fx := x;
  Result.fy := y;
  Result.fz := z;
  addMesh(Result);
end;

procedure TViewport.addMesh(_mesh: TMesh3d);
begin
  assert(assigned(_mesh));
  FList.Add(_mesh)
end;

function TViewport.addPlane(x, y, z: single): TPlane;
begin
  Result :=  TPlane.Create;
  Result.fx := x;
  Result.fy := y;
  Result.fz := z;
  addMesh(Result);
end;

constructor TViewport.Create;
begin
  FList := TObjectList.Create;
  GTest := 0;
  FCamZ := -15;
  FWireframe := false;
  Frasterframe := true;
  FProjection := TViewportProjectionType.Perspective;
end;

destructor TViewport.Destroy;
begin
  freeAndNil(FList);
  inherited;
end;



function CalculateSurfaceNormal(AVecA, AVecB,
  AVecC: TVector3D) : TVector3D;
var
  LVecU, LVecV: TPoint3d;
begin
  LVecU := AVecB;
  LVecU := LVecU - AVecA;

  LVecV := AVecC;
  LVecV := LVecV - AVecA;

  Result.X := (LVecU.Y*LVecV.Z) - (LVecU.Z*LVecV.Y);
  Result.Y := (LVecU.Z*LVecV.X) - (LVecU.X*LVecV.Z);
  Result.Z := (LVecU.X*LVecV.Y) - (LVecU.Y*LVecV.X);
end;

procedure TViewport.Execute;
var l : TMesh3D;
    i,j : integer;
    t : TBaseTriangle3D;

    FTranslationMatrix : TMatrix3D;
    FRotationX : TMatrix3D;
    FRotationY: TMatrix3D;
    FRotationZ : TMatrix3D;
    FObjectMatrix : TMatrix3D;
    FViewMatrix : TMatrix3D;
    FCameraMatrix : TMatrix3D;

    FProjectionMatrix : TMatrix3D;
    FFinalProjection : TMatrix3D;

    AspectRatio : Single;
    ZFar, ZNear : single;
    FOV : single;

    Width : Integer;
    Height : Integer;


    tw,th : Integer; //Texture size.

begin
  Assert(Assigned(TargetCanvas));
  Width := TargetCanvas.Width;
  Height := TargetCanvas.Height;

  FCameraMatrix := TMatrix3d.Identity;
  FProjectionMatrix := TMatrix3d.Identity;

  FCameraMatrix := TMatrix3D.CreateLookAtDirLH(point3d(0,0,FCamZ),point3d(0,0,1),point3d(0,1,0));

  Case FProjection of
    TViewportProjectionType.Perspective,
    TViewportProjectionType.PerspectiveIsometric :
    begin
      AspectRatio := Width/Height;
      ZFar := 100;
      ZNear := 1;
      FOV := DegToRad(45.0);

      FProjectionMatrix :=
        TMatrix3D.CreatePerspectiveFovLH(
            FOV,
            AspectRatio,ZNear,ZFar);
    end;
    TViewportProjectionType.OrthographicIsometric :
    begin
      //ORTHOGRAPHIC PROJECTION.
      // 45° rotation on Y
      FProjectionMatrix :=
          TMatrix3D.CreateRotationY(-45*PI/180)
      // 20° rotation on X
        * TMatrix3D.CreateRotationX(-20*PI/180)
      // move the scene to the back to avoid Z Clipping
        * TMatrix3D.CreateTranslation(TPoint3D.Create(0, 0, -500))
      // create an iometric projection
        * TMatrix3D.CreateOrthoOffCenterRH(
            -Width/50, -Height/50,
            +Width/50, +Height/50,
            1, 1000
          );
    end;
  End;




//  TargetCanvas.SetLine(0,0,30,30); //,$FFAABBFF);
//  TargetCanvas.SetLine(30,0,0,30); //,$FFAABBFF);
  for i := 0 to FList.Count-1 do
  begin
    l := TMesh3D(FList[i]);

    //Object.
    FTranslationMatrix := TMatrix3D.CreateTranslation(point3d(l.fx,l.fy,l.fz));
    FRotationX := TMatrix3D.CreateRotationX(DegToRad(GTest*i));
    FRotationY := TMatrix3D.CreateRotationY(DegToRad(GTest));
    FRotationZ := TMatrix3D.CreateRotationZ(DegToRad(GTest/3));

    FObjectMatrix := FRotationX;
    FObjectMatrix := FObjectMatrix * TMatrix3d.CreateTranslation(Point3D(0,0,0));
    FObjectMatrix := FObjectMatrix * FRotationY;
    FObjectMatrix := FObjectMatrix * TMatrix3d.CreateTranslation(Point3D(0,0,0));
    FObjectMatrix := FObjectMatrix * FRotationZ;
    FObjectMatrix := FObjectMatrix * TMatrix3d.CreateTranslation(Point3D(0,0,0));
    FObjectMatrix := FObjectMatrix * FTranslationMatrix;

    //View
    FViewMatrix := TMatrix3D.Identity * FCameraMatrix.Inverse;

    //Projection.
    FFinalProjection := FObjectMatrix * FViewMatrix * FProjectionMatrix;

    l.transform(FFinalProjection);
    for j := 0 to l.triangleCount-1 do
    begin
      t := l.triangles3D(j);

      //Perspective : Applying Z.

      if FProjection = TViewportProjectionType.Perspective then
      begin
        t.VertexA.X := t.VertexA.X * t.VertexA.Z;
        t.VertexA.Y := t.VertexA.Y * t.VertexA.Z;
        t.VertexB.X := t.VertexB.X * t.VertexB.Z;
        t.VertexB.Y := t.VertexB.Y * t.VertexB.Z;
        t.VertexC.X := t.VertexC.X * t.VertexC.Z;
        t.VertexC.Y := t.VertexC.Y * t.VertexC.Z;
      end;

      //https://www.khronos.org/opengl/wiki/Vertex_Transformation
      //viewport = (0,0,Width, Height) | Formula = windowCoordinate[0] = (x * 0.5 + 0.5) * viewport[2] + viewport[0];

      //Denormalization.
      t.VertexA.X := (t.VertexA.X * 0.5 + 0.5) * Width + 0;
      t.VertexA.Y := (t.VertexA.Y * 0.5 + 0.5) * Height + 0;
      t.VertexA.Z := (1 + t.VertexA.Z) * 0.5;
      t.VertexB.X := (t.VertexB.X * 0.5 + 0.5) * Width + 0;
      t.VertexB.Y := (t.VertexB.Y * 0.5 + 0.5) * Height + 0;
      t.VertexB.Z := (1 + t.VertexB.Z) * 0.5;
      t.VertexC.X := (t.VertexC.X * 0.5 + 0.5) * Width + 0;
      t.VertexC.Y := (t.VertexC.Y * 0.5 + 0.5) * Height + 0;
      t.VertexC.Z := (1 + t.VertexC.Z) * 0.5;

{     Denormalization Equivalence : (for info)
      t.VertexA.x := (1-t.VertexA.x) * TargetCanvas.Surface.Width/2;
      t.VertexA.y := (1-t.VertexA.y) * TargetCanvas.Surface.Height/2;
      t.VertexB.x := (1-t.VertexB.x) * TargetCanvas.Surface.Width/2;
      t.VertexB.y := (1-t.VertexB.y) * TargetCanvas.Surface.Height/2;
      t.VertexC.x := (1-t.VertexC.x) * TargetCanvas.Surface.Width/2;
      t.VertexC.y := (1-t.VertexC.y) * TargetCanvas.Surface.Height/2;
}


      TargetCanvas.beginDraw;
      if Frasterframe then
      begin
        tw := 320;
        th := 240;
        TargetCanvas.setVertex(0,round(t.VertexA.X),round(t.VertexA.Y),round(t.VertexA.Z),trunc(t.uvA.u*tw),trunc(t.uvA.v*th));
        TargetCanvas.setVertex(1,round(t.VertexB.X),round(t.VertexB.Y),round(t.VertexB.Z),trunc(t.uvB.u*tw),trunc(t.uvB.v*th));
        TargetCanvas.setVertex(2,round(t.VertexC.X),round(t.VertexC.Y),round(t.VertexC.Z),trunc(t.uvC.u*tw),trunc(t.uvC.v*th));
        TargetCanvas.rasterize;
      end;

      if wireFrame then
      begin
        TargetCanvas.moveTo(round(t.VertexA.x),round(t.VertexA.y));
        TargetCanvas.LineTo(round(t.VertexB.x),round(t.VertexB.y));
        TargetCanvas.lineTo(round(t.VertexC.x),round(t.VertexC.y));
        TargetCanvas.lineTo(round(t.VertexA.x),round(t.VertexA.y));
      end;
      TargetCanvas.endDraw;

    end;

  end;

  GTest := GTest + 1.5;
end;

{ TCube }

constructor TCube.create;
begin
  inherited;
  addVertex(-0.5,-0.5,-0.5); //0
  addVertex(0.5,-0.5,-0.5);  //1
  addVertex(0.5,0.5,-0.5);   //2
  addVertex(-0.5,0.5,-0.5);  //3

  addVertex(-0.5,-0.5,0.5);  //4
  addVertex(0.5,-0.5,0.5);   //5
  addVertex(0.5,0.5,0.5);    //6
  addVertex(-0.5,0.5,0.5);   //7

  addQuad(0,1,2,3);
  addQuad(0,3,7,4);
  addQuad(3,2,6,7);
  addQuad(2,1,5,6);
  addQuad(1,0,4,5);
  addQuad(4,5,6,7);
end;


{ TPlane }

constructor TPlane.create;
begin
  inherited;
  addVertex(-0.5,-0.5,0);
  addVertex(0.5,-0.5,0);
  addVertex(0.5,0.5,0);
  addVertex(-0.5,0.5,0);
  addQuad(0,1,2,3);
end;

{ TMeshData3D }

procedure TMeshData3D.clear;
begin
//  FillChar(vertices,(length(vertices)-1)*SizeOf(TVertice3D),0);
//  FillChar(Indices,length(Indices)-1,0);
end;

function TMeshData3D.indicesArraySize: Uint32;
begin
  result := Length(Indices);
end;

procedure TMeshData3D.setIndicesArraySize(newSize: Uint32);
begin
  SetLength(Indices,newSize);
  SetLength(UVMap,newSize);
end;

procedure TMeshData3D.setVerticesArraySize(newSize: Uint32);
begin
  SetLength(vertices,newSize);
end;

function TMeshData3D.verticesArraySize: Uint32;
begin
  result := Length(vertices);
end;

end.
