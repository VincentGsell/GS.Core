unit GS.Geometry.Triangulation;

interface

uses Classes, SysUtils,
     Math.Vectors,
     GS.Geometry,
     GS.Geometry.Mesh2D,
     GS.Geometry.Mesh2D.Tools,
     Delaunay,
     BeRoTriangulation;

Type
TGSCustomTriangulation = Class
public
  Mesh : TGSRawMesh2D;

  procedure AddPoint(x,y : single); virtual; abstract;
  procedure Process; virtual; abstract;

  Constructor Create; Virtual;
  Destructor Destroy; override;
End;

//Used that for repetitive usage (See demo MiniDelau)
TGSTriangulationDelaunay = class(TGSCustomTriangulation)
protected
  Engine : TDelaunay;
public

  Constructor Create; override;
  Destructor Destroy; override;

  procedure AddPoint(x,y : single); override;
  procedure Process; override;
end;

Type TTiangulationMethod = (tmDelaunay, tmSeibel, tmBeRo);

//Use that for "One time call".
TGSTriangulationPortal = class
  class function Delaunay(var cloudPoint : array of vec2; var resultMesh : TGSRawMesh2D) : UInt32;
//  class function PolygoneTriangulation(var orderedCloudPoint : array of vec2; resultMesh : TGSRawMesh2D;Const TriangulationMethod : TTiangulationMethod = tmBeRo) : UInt32; Overload;
  class function PolygoneTriangulation(var orderedCloudPoint : array of vec2s; resultMesh : TGSRawMesh2D;Const TriangulationMethod : TTiangulationMethod = tmBeRo) : UInt32; Overload;
  //class function Delaunay(cloudPoint : array of vec2; var resultMesh : TGSRawMesh2D) : boolean;
end;


implementation

{ TGSTriangulationPortal }

class function TGSTriangulationPortal.Delaunay(var cloudPoint: array of vec2;
  var resultMesh: TGSRawMesh2D): Uint32;
var l : TDelaunay; //Direct by delaunay, to avoid uselsee copy.
    i : integer;
    a,b,c : vec2;
begin
  result := 0;
  l := TDelaunay.Create;
  try
    if Not Assigned(resultMesh) then
    begin
      resultMesh := TGSRawMesh2D.Create;
    end;

    for I := 0 to Length(cloudPoint) do
      l.AddPoint(cloudPoint[i].x,cloudPoint[i].y);

    l.Mesh;
    for i:= 1 to l.HowMany do
    begin
      a.x := Round(l.Vertex^[l.Triangle^[i].vv0].x);
      a.y := Round(l.Vertex^[l.Triangle^[i].vv0].y);
      b.x := Round(l.Vertex^[l.Triangle^[i].vv1].x);
      b.y := Round(l.Vertex^[l.Triangle^[i].vv1].y);
      c.x := Round(l.Vertex^[l.Triangle^[i].vv2].x);
      c.y := Round(l.Vertex^[l.Triangle^[i].vv2].y);

      resultMesh.addTriangle(a,b,c);
    end;
    result := resultMesh.getTriangleCount;

  finally
    FreeandNil(l);
  end;
end;

{

class function TGSTriangulationPortal.PolygoneTriangulation(var
  orderedCloudPoint: array of vec2; resultMesh: TGSRawMesh2D; Const TriangulationMethod : TTiangulationMethod): UInt32;
var
  pb : TBeRoTriangulationPolygons;
  outpb : TBeRoTriangulationTriangles;
  res : boolean;
  a,b,c : vec2;
  i : integer;
begin
  assert(assigned(resultMesh));
  resultMesh.reset;
  //Only one poly here.
  SetLength(pb,1);
  SetLength(pb[0],length(orderedCloudPoint));
  for i := 0 to length(orderedCloudPoint)-1 do
  begin
    pb[0][i].X := Trunc(orderedCloudPoint[i].x);
    pb[0][i].Y := Trunc(orderedCloudPoint[i].y);
  end;

  case TriangulationMethod of
    tmDelaunay: res := TriangulateDelaunayClipping(pb,outpb);
    tmSeibel: res := TriangulateSeidel(pb,outpb);
    tmBeRo: res := TriangulateBeRo(pb,outpb);
  end;

  if res then
  begin
    for i := 0 to length(outpb)-1 do
    begin
      a.x := outpb[i][0].x;
      a.y := outpb[i][0].y;
      b.x := outpb[i][1].x;
      b.y := outpb[i][1].y;
      c.x := outpb[i][2].x;
      c.y := outpb[i][2].y;
      resultMesh.addTriangle(a,b,c);
    end;
  end;
  result := resultMesh.getTriangleCount;
end;
}

class function TGSTriangulationPortal.PolygoneTriangulation(
  var orderedCloudPoint: array of vec2s; resultMesh: TGSRawMesh2D;
  const TriangulationMethod: TTiangulationMethod): UInt32;
var
  pb : TBeRoTriangulationPolygons;
  outpb : TBeRoTriangulationTriangles;
  res : boolean;
  a,b,c : vec2;
  i,j : integer;
begin
  assert(assigned(resultMesh));
  resultMesh.reset;
  SetLength(pb,Length(orderedCloudPoint));
  for j := 0 to length(orderedCloudPoint)-1 do begin
    SetLength(pb[j],length(orderedCloudPoint[j]));
    for i := 0 to length(orderedCloudPoint[j])-1 do
    begin
      pb[j][i].X := Trunc(orderedCloudPoint[j][i].x);
      pb[j][i].Y := Trunc(orderedCloudPoint[j][i].y);
    end;
  end;

  case TriangulationMethod of
    tmDelaunay: res := TriangulateDelaunayClipping(pb,outpb);
    tmSeibel: res := TriangulateSeidel(pb,outpb);
    tmBeRo: res := TriangulateBeRo(pb,outpb);
  end;

  if res then
  begin
    for i := 0 to length(outpb)-1 do
    begin
      a.x := outpb[i][0].x;
      a.y := outpb[i][0].y;
      b.x := outpb[i][1].x;
      b.y := outpb[i][1].y;
      c.x := outpb[i][2].x;
      c.y := outpb[i][2].y;
      resultMesh.addTriangle(a,b,c);
    end;
  end;
  result := resultMesh.getTriangleCount;
end;

{ TGSCustomTriangulation }

constructor TGSCustomTriangulation.Create;
begin
  Inherited;
  Mesh := TGSRawMesh2D.Create;
  Mesh.reset;
end;

destructor TGSCustomTriangulation.Destroy;
begin
  FreeAndNil(Mesh);
  inherited;
end;

{ TGSTriangulationDelaunay }

procedure TGSTriangulationDelaunay.AddPoint(x, y: single);
begin
  Engine.AddPoint(x,y);
end;

constructor TGSTriangulationDelaunay.Create;
begin
  inherited Create;
  Engine := TDelaunay.Create;
end;

destructor TGSTriangulationDelaunay.Destroy;
begin
  FreeAndNil(Engine);
  inherited;
end;

procedure TGSTriangulationDelaunay.Process;
var i : integer;
    a,b,c : vec2;
begin
  Engine.Mesh;
  Mesh.reset;
  for i:= 1 to Engine.HowMany do
  begin
    a.x := Round(Engine.Vertex^[Engine.Triangle^[i].vv0].x);
    a.y := Round(Engine.Vertex^[Engine.Triangle^[i].vv0].y);
    b.x := Round(Engine.Vertex^[Engine.Triangle^[i].vv1].x);
    b.y := Round(Engine.Vertex^[Engine.Triangle^[i].vv1].y);
    c.x := Round(Engine.Vertex^[Engine.Triangle^[i].vv2].x);
    c.y := Round(Engine.Vertex^[Engine.Triangle^[i].vv2].y);

    Mesh.addTriangle(a,b,c);
  end;
end;

end.

