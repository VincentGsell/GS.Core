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
 Unit Name : GS.SoftwareRaster3D.ExempleStandAlone
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : 3D software raster unit.
 Date:     : 2020031
 History   :
 20200301 - Creating unit.
 20200424 - Change Math vector library (vendor-specific math lib free)

Credits :
https://learnopengl.com/Getting-started/Coordinate-Systems
https://www.scratchapixel.com/lessons/3d-basic-rendering/computing-pixel-coordinates-of-3d-point/mathematics-computing-2d-coordinates-of-3d-points

Description :
- All-In-One unit 3d Software raster app. (appart 2d canvas)
  - All matric calculus, and major "pipeline" step is present in only one unit (vertexshader, tesselator, raster preparation, fragment)
- Mainly for learning purpose.
- This unit is volountary kept simple.
- Large optimization is always possible, but I do not want to break the understandability.
- Must be cross platform. No Asm.
- This is a "gentle" ref, just for documentation, for  more strength, see "pipeline" series.
-----------------------------------------------------------------------------}
{$I GSCore.Inc}

unit GS.SoftwareRaster3D.ExempleStandAlone;
interface

uses SysUtils,
     classes,
     contNrs,
     Math,
     GS.Geometry,
     GS.Pixel32,
     GS.Pixel32.Rasterize,
     GS.Pixel32.Types;

Type
  TCoord = record u,v : single; end;
  TPoint2D = record x, y : single; end;

  TVertice = record
    x,y,z : TVecType; //Coord.
    rgba : vec4;      //vertice color.
    u,v : TVecType;   //UV Map (texture, ref., etc.)
  end;
  TTriVertices = array of TVertice;

  TMeshData3D = Record
    vertices : array of vec3;
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

  TBaseVertexArray = array of vec3;
  TBaseTriangle = record VertexIndiceA,VertexIndiceB,VertexIndiceC : integer; uvA, uvB, uvC : TCoord; end;
  TBaseTriangle3D = record VertexA,VertexB,VertexC : vec3; uvA, uvB, uvC : TCoord; end;

  TMesh3D = class(TBase3D)
  private
    fmeshData : TMeshData3D;
    ftransformed : TMeshData3D;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure addVertex(x,y,z : single); overload;  //todo Normal
    procedure addVertex(const map : TBaseVertexArray); overload; //todo Normal
    procedure addTriangle(i1,i2,i3 : uint32; u,v,u1,v1,u2,v2 : integer); overload;
    procedure addQuad(i1,i2,i3,i4 : uint32);

    procedure meshScale(factor : single);

    //Tesselation stage.
    function triangleCount : Integer;
    function triangles(TriIndice : integer) : TBaseTriangle;
    function triangles3D(TriIndice : integer) : TBaseTriangle3D;

    //VertexShader stage
    procedure transform(_matrix : mat4);
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

  TView3dProjectionType = (Perspective, OrthographicIsometric);

  TView3d = class
  private
    FList : TObjectList; //Normsaly, better to Separate list form viewport ;)
    GTest : single;
    FCamZ: single;
    FWireframe: boolean;
    Frasterframe: boolean;
    FProjection: TView3dProjectionType;

    procedure InternalDrawTri(const v : TTriVertices);
  public
    TargetCanvas : TPixel32;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure addMesh(_mesh : TMesh3d);
    function addCube(x,y,z : single) : TCube;
    function addPlane(x,y,z : single) : TPlane;

    procedure Execute;

    Property CameraZ : single read FCamZ Write FCamZ;

    property rasterFrame : boolean read Frasterframe write fRasterframe;
    property wireFrame : boolean read FWireframe write FWireframe;

    property Projection : TView3dProjectionType read FProjection write FProjection;
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
  addTriangle(i4,i1,i3,0,1,0,0,1,1);
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
  fmeshData.vertices[l] := vec3.create(x,y,z);
end;

procedure TMesh3D.addVertex(const map: TBaseVertexArray);
var i,l : integer;
begin
  assert(length(map)>0);
  l := fmeshData.verticesArraySize-1;
  fmeshData.setVerticesArraySize(fmeshData.verticesArraySize + length(map));
  for i := l to fmeshData.verticesArraySize-1 do
    fmeshData.Vertices[i] := vec3.create(map[i-l].x,map[i-l].y,map[i-l].z);
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


procedure TMesh3D.transform(_matrix: mat4);
var i : integer;
begin
  ftransformed.setVerticesArraySize(fmeshData.verticesArraySize);
  ftransformed.setIndicesArraySize(fmeshData.indicesArraySize);
  for i := 0 to fmeshData.verticesArraySize-1 do
    ftransformed.Vertices[i] := fmeshData.Vertices[i] * _matrix;
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

  Result.uvA := l.uvA; //  fmeshData.UVMap[l.VertexIndiceA];
  Result.uvB := l.uvB; //  fmeshData.UVMap[l.VertexIndiceB];
  Result.uvC := l.uvC; //  fmeshData.UVMap[l.VertexIndiceC];

end;

{ TView3d }

function TView3d.addCube(x, y, z :  single) : TCube;
begin
  Result :=  TCube.Create;
  Result.fx := x;
  Result.fy := y;
  Result.fz := z;
  addMesh(Result);
end;

procedure TView3d.addMesh(_mesh: TMesh3d);
begin
  assert(assigned(_mesh));
  FList.Add(_mesh)
end;

function TView3d.addPlane(x, y, z: single): TPlane;
begin
  Result :=  TPlane.Create;
  Result.fx := x;
  Result.fy := y;
  Result.fz := z;
  addMesh(Result);
end;

constructor TView3d.Create;
begin
  FList := TObjectList.Create;
  GTest := 0;
  FCamZ := -15;
  FWireframe := false;
  Frasterframe := true;
  FProjection := TView3dProjectionType.Perspective;
end;

destructor TView3d.Destroy;
begin
  freeAndNil(FList);
  inherited;
end;



function CalculateSurfaceNormal(AVecA, AVecB,
  AVecC: vec3) : vec3;
var
  LVecU, LVecV: vec3;
begin
  LVecU := AVecB;
  LVecU := LVecU - AVecA;

  LVecV := AVecC;
  LVecV := LVecV - AVecA;

  Result.X := (LVecU.Y*LVecV.Z) - (LVecU.Z*LVecV.Y);
  Result.Y := (LVecU.Z*LVecV.X) - (LVecU.X*LVecV.Z);
  Result.Z := (LVecU.X*LVecV.Y) - (LVecU.Y*LVecV.X);
end;

procedure TView3d.Execute;
var l : TMesh3D;
    i,j : integer;
    t : TBaseTriangle3D;
    ca,cb,cc : vec4; //Colors.

    FTranslationMatrix : mat4;
    FRotationX : mat4;
    FRotationY: mat4;
    FRotationZ : mat4;
    FObjectMatrix : mat4;
    FViewMatrix : mat4;
    FCameraMatrix : mat4;

    FProjectionMatrix : mat4;
    FFinalProjection : mat4;

    AspectRatio : Single;
    ZFar, ZNear : single;
    FOV : single;

    Width : Integer;
    Height : Integer;

    tri : TTriVertices;
begin
  SetLength(tri,2);
  Assert(Assigned(TargetCanvas));
  Width := TargetCanvas.Width;
  Height := TargetCanvas.Height;

  FCameraMatrix := mat4Identity;
  FProjectionMatrix := mat4Identity;

  FCameraMatrix := mat4CreateLookAtDirLH(vec3.create(0,0,FCamZ),vec3.create(0,0,1),vec3.create(0,1,0));

  Case FProjection of
    TView3dProjectionType.Perspective :
    begin
      AspectRatio := Width/Height;
      ZFar := 1000;
      ZNear := 0;
      FOV := DegToRad(90.0);

      FProjectionMatrix :=
        mat4CreatePerspectiveFovLH(
            FOV,
            AspectRatio,ZNear,ZFar);
    end;
    TView3dProjectionType.OrthographicIsometric :
    begin
      //ORTHOGRAPHIC PROJECTION.
      // 45° rotation on Y
      FProjectionMatrix :=
//          Mat4CreateRotationY(-45*PI/180)
      // 20° rotation on X
//        * Mat4CreateRotationX(-20*PI/180)
      // move the scene to the back to avoid Z Clipping
//        Mat4CreateTranslation(vec3.Create(0, 0, -500))
      // create an iometric projection
          Mat4CreateOrthoOffCenterRH(
            -Width, -Height,
            +Width, +Height,
            0, 1000
          );
    end;
  End;



  //View
  FViewMatrix := FCameraMatrix * FProjectionMatrix;

// :(
  FViewMatrix := mat4CreateRotationY(DegToRad(GTest)) * FViewMatrix;


  TargetCanvas.beginDraw;
  for i := 0 to FList.Count-1 do
  begin
    l := TMesh3D(FList[i]);

    //Object.
    FTranslationMatrix := mat4CreateTranslation(vec3.create(l.fx,l.fy,l.fz));
    FRotationX := Mat4Identity;
    FRotationY := Mat4Identity;
    FRotationZ := Mat4Identity;
    FRotationX := mat4CreateRotationX(DegToRad(GTest*(i+1)));
    FRotationY := mat4CreateRotationY(DegToRad(GTest*(i+1)));
    FRotationZ := mat4CreateRotationZ(DegToRad(GTest*(i+1)));

    FObjectMatrix := FRotationX;
    FObjectMatrix := FObjectMatrix * mat4CreateTranslation(vec3.create(0,0,0));
    FObjectMatrix := FObjectMatrix * FRotationY;
    FObjectMatrix := FObjectMatrix * mat4CreateTranslation(vec3.create(0,0,0));
    FObjectMatrix := FObjectMatrix * FRotationZ;
    FObjectMatrix := FObjectMatrix * mat4CreateTranslation(vec3.create(0,0,0));
    FObjectMatrix := FObjectMatrix * FTranslationMatrix;


    //Projection.
    FFinalProjection := FObjectMatrix * FViewMatrix;

    l.transform(FFinalProjection);
//    TargetCanvas.beginDraw;
    for j := 0 to l.triangleCount-1 do
    begin
      t := l.triangles3D(j);

      ///
      ///
      ///  RASTERING PHASE
      ///
      ///
      ///

      //Applying Z.
      t.VertexA.X := t.VertexA.X / t.VertexA.Z;
      t.VertexA.Y := t.VertexA.Y / t.VertexA.Z;
      t.VertexB.X := t.VertexB.X / t.VertexB.Z;
      t.VertexB.Y := t.VertexB.Y / t.VertexB.Z;
      t.VertexC.X := t.VertexC.X / t.VertexC.Z;
      t.VertexC.Y := t.VertexC.Y / t.VertexC.Z;

      //https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/perspective-correct-interpolation-vertex-attributes
      //https://www.khronos.org/opengl/wiki/Vertex_Transformation
      //viewport = (0,0,Width, Height) | Formula = windowCoordinate[0] = (x * 0.5 + 0.5) * viewport[2] + viewport[0];

      //Denormalization.
      t.VertexA.X := (1 + t.VertexA.X) * 0.5 * Width;
      t.VertexA.Y := (1 + t.VertexA.Y) * 0.5 * Height;
      t.VertexB.X := (1 + t.VertexB.X) * 0.5 * Width;
      t.VertexB.Y := (1 + t.VertexB.Y) * 0.5 * Height;
      t.VertexC.X := (1 + t.VertexC.X) * 0.5 * Width;
      t.VertexC.Y := (1 + t.VertexC.Y) * 0.5 * Height;

//     Denormalization Equivalence : (for info - vector dir display inverse.)
{
      t.VertexA.x := (1-t.VertexA.x) * Width/2;
      t.VertexA.y := (1-t.VertexA.y) * Height/2;
      t.VertexB.x := (1-t.VertexB.x) * Width/2;
      t.VertexB.y := (1-t.VertexB.y) * Height/2;
      t.VertexC.x := (1-t.VertexC.x) * Width/2;
      t.VertexC.y := (1-t.VertexC.y) * Height/2;
}
//


//      TargetCanvas.beginDraw;
      if Frasterframe then
      begin
        tri[0].x := t.VertexA.x;
        tri[0].y := t.VertexA.y;
        tri[0].z := t.VertexA.z;
        tri[0].u := t.uvA.u;
        tri[0].v := t.uvA.v;

        tri[1].x := t.VertexB.x;
        tri[1].y := t.VertexB.y;
        tri[1].z := t.VertexB.z;
        tri[1].u := t.uvB.u;
        tri[1].v := t.uvB.v;

        tri[2].x := t.VertexC.x;
        tri[2].y := t.VertexC.y;
        tri[2].z := t.VertexC.z;
        tri[2].u := t.uvC.u;
        tri[2].v := t.uvC.v;

        InternalDrawTri(tri);
      end;

      if wireFrame then
      begin
        TargetCanvas.moveTo(round(t.VertexA.x),round(t.VertexA.y));
        TargetCanvas.LineTo(round(t.VertexB.x),round(t.VertexB.y));
        TargetCanvas.lineTo(round(t.VertexC.x),round(t.VertexC.y));
        TargetCanvas.lineTo(round(t.VertexA.x),round(t.VertexA.y));
      end;
//      TargetCanvas.endDraw;
    end;
//    TargetCanvas.endDraw;

  end;
  TargetCanvas.endDraw;

//  GTest := GTest + 0.2;
  GTest := GTest + 0.35;
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
  addQuad(4,5,6,7);
  addQuad(0,4,7,3);
  addQuad(5,1,2,6);
  addQuad(0,4,5,1);
  addQuad(3,7,6,2);
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

function edgeFunctionLocal(var a,b,c : TVertice) : TVecType; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := (a.x - c.x) * (b.y - a.y) - (a.y - c.y) * (b.x - a.x);
end;

procedure TView3d.InternalDrawTri(const v : TTriVertices);
var w,h : Uint32;

    sv : TVertice;

    area,w0,w1,w2 : TVecType;
    p : TVertice;
    minx, maxx, miny, maxy : integer;
    vi0x,vi0y,vi1x,vi1y,vi2x,vi2y : integer;

    bits : pTP32; //frontbuffer.
    bbBits : pTP32; //backbuffer
    zbits : pTP32; //ZBuffer.

    //ZBuffer gray level display.
    procedure ZBufferDisplayRaster;
    var i,j : integer;
        holyZ : TVecType;
        ZCol,ZDef : single;
    begin
      for j := miny to maxy do
      begin
        bits := TargetCanvas.getSurfaceScanLinePtr(j);
        inc(bits,minx);

        bbBits := BackBuffer.getSurfaceScanLinePtr(j);
        inc(bbBits,minx);

        zBits := DepthBuffer.getSurfaceScanLinePtr(j);
        inc(zbits,minx);

        for i := minx to maxx do
        begin
          p.x := i; p.y := j;
          w0 := edgeFunctionLocal(v[1], v[2], p);
          w1 := edgeFunctionLocal(v[2], v[0], p);
          w2 := edgeFunctionLocal(v[0], v[1], p);
          if (w0>=0) and (w1>=0) and (w2>=0) then
          begin
            w0 := w0 / area;
            w1 := w1 / area;
            w2 := w2 / area;

            holyZ := (w0*v[0].z + w1*v[1].z + w2*v[2].z);
            ZDef := 0;
            if holyZ<>0 then
              ZDef := abs(1 / holyZ);
            ZCol := _clamp(ZDef,0,1);

            if bbBits^ <> BackBuffercol then
            begin
              bits^ := TargetCanvas.colorP32Rec(trunc(ZCol*255),trunc(ZCol*255),trunc(ZCol*255),255).Color;
              bbBits^ := BackBuffercol;
              TP32rec(zBits^).Valuef := ZDef;
            end
            else
            begin
              if ZDef > TP32rec(zBits^).Valuef  then
              begin
                bits^ := TargetCanvas.colorP32Rec(trunc(ZCol*255),trunc(ZCol*255),trunc(ZCol*255),255).Color;
                TP32rec(zBits^).Valuef := ZDef;
              end;
            end;

          end;
          inc(bits);
          inc(bbBits);
          inc(zbits);
        end;
      end;
    end;




begin
  Assert(BackBuffer.width = TargetCanvas.width);
  Assert(BackBuffer.height = TargetCanvas.height);

  area := edgeFunctionLocal(v[0],v[1],v[2]);
  if area<=0 then
  begin
    //swap (rev. triangle.)
    sv := v[0];
    v[0] := v[2];
    v[2] := sv;
    area := edgeFunctionLocal(v[0],v[1],v[2]);
    if area<=0 then
      Exit;
  end;

  w := TargetCanvas.width;
  h := TargetCanvas.height;

  vi0x := round(v[0].x);
  vi0y := round(v[0].y);
  vi1x := round(v[1].x);
  vi1y := round(v[1].y);
  vi2x := round(v[2].x);
  vi2y := round(v[2].y);

  minx:=max(min(min(vi0x,vi1x),vi2x),0);
  miny:=max(min(min(vi0y,vi1y),vi2y),0);

  maxx:=min(max(max(vi0x,vi1x),vi2x),w-1);
  maxy:=min(max(max(vi0y,vi1y),vi2y),h-1);

  if maxx-minx=0 then exit;
  if maxy-miny=0 then exit;

  ZBufferDisplayRaster;
end;



end.

