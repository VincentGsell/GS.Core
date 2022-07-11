unit GS.Geometry.Mesh2D.Tools;
{$I GSCore.Inc}

interface


uses Types,
     Classes,
     SysUtils,
     Math,
     FastGeo,
     Clipper,
     ClipperCore,
     GS.Geometry.Direction,
     GS.Geometry,
     Gs.Geometry.Mesh2D;
type

TGSShape2dType = (triangleOneOneOne,
                      quadOneOne,
                      rectOneTwo,
                      rectTwoOne,
                      penta,
                      hexa,
                      hepta,
                      octo,
                      circle20,
                      circle50,
                      circle100,
                      halfmoon,
                      gear,
                      star5,
                      sun);

TGSMesh2dHelper = class helper for TGSRawMesh2d
private
public
  procedure Build_SetRoundShape(subdivision : UInt32; len : single);
  procedure SetShapeType(shapetype : TGSShape2dType);
  function isPointInside(x, y: single; var TriangleIndice : Uint32): boolean;
  procedure addTriangle(a,b,c : vec2);
//  procedure triangulate(poly : array of vec2); ?
end;

TGSMeshMatrixProcessing = class
  class procedure Transform(var target : TGSRawMesh2d; const transformMatrix : mat3);
end;

TGSMeshTurtle = class
private
  FMesh : TGSRawMesh2d;
  FDown: boolean;
  FAngle : single;
  FTool : TDirectionalObject;
  FStepValue: TVecType;
public
  constructor create(rawMesh : TGSRawMesh2d); virtual;
  Destructor Destroy; override;

  function up : TGSMeshTurtle;
  function down : TGSMeshTurtle;
  function turnRight : TGSMeshTurtle;
  function turnLeft : TGSMeshTurtle;
  function turnHalf : TGSMeshTurtle;
  function turnBy(angleInDegreeClocked : single)  : TGSMeshTurtle;
  function step : TGSMeshTurtle;

  property isDown : boolean read FDown;
  property stepValue : TVecType read FStepValue write FStepValue;

  property Mesh : TGSRawMesh2D read FMesh;
end;

iGSMeshGrapher = interface
  function rect(x,y,width,height : single) : iGSMeshGrapher;
  function moveTo(x,y : single) : iGSMeshGrapher;
  function lineTo(x,y : single; const thickness : single = 0) : iGSMeshGrapher;
end;

TGSMeshGrapher = Class(TInterfacedObject, iGSMeshGrapher)
private
  FTool : TDirectionalObject;
  FX,FY : Single;
public
  Mesh : TGSRawMesh2D;

  Constructor Create; virtual;
  Destructor Destroy; override;
  function rect(x,y,width,height : single) : iGSMeshGrapher;
  function moveTo(x,y : single) : iGSMeshGrapher;
  function lineTo(x,y : single; const thickness : single = 0) : iGSMeshGrapher;
End;

IGSGeometryData = interface
  procedure clear;
  Procedure SetCapacity(aCapa : Integer);
  function getBorder : Vec2sp;
  function GetMesh : TGSRawMesh2D;
  Procedure Scale(xScale, yScale : Single);
  Procedure Translate(xAmount, yAmount : Single);
end;

TGSGeometry2DData = Class(TInterfacedObject, IGSGeometryData)
Private
  FBorder : Vec2s;
  Procedure ClearArray;
Public
  function getBorder : Vec2sp;
  Procedure Clear;
  Procedure SetCapacity(aCapa : Integer);
  Procedure Scale(xScale, yScale : Single);
  Procedure Translate(xAmount, yAmount : Single);
  function GetMesh : TGSRawMesh2D;
End;

///
///
///  GENERATOR
///
///

IGSGeometryGenerator = interface
 Procedure Generate(var aData : IGSGeometryData);
end;

TGSGeometryGenerator = Class(TInterfacedObject, IGSGeometryGenerator)
Public
  Procedure Generate(var aData : IGSGeometryData); Virtual; Abstract;
End;

TGS2DGeometryGenerator = Class(TGSGeometryGenerator)
Private
Protected
  LocalGeometryTool : TDirectionalObject;
Public
  Constructor Create; Virtual;
  Destructor Destroy; Override;
End;

//--------------------------------------------------- Disk.
TGSDiskGenerator = Class(TGS2DGeometryGenerator)
Private
  FRadius: Single;
  FSubdivision: Integer;
Public
  Procedure Generate(var aData : IGSGeometryData); Override;
  Constructor Create; Override;

  Property Radius : Single read FRadius WRite FRadius;
  Property Subdivision : Integer Read FSubdivision Write FSubdivision;
End;

//-------------------------------------------------- Donut (from Disk).
TGSDiskDonutGenerator = Class(TGSDiskGenerator)
Private
  FInnerRadius: Single;
  FSecondaryGeometryTool : TDirectionalObject;

  procedure SetInnerRadius(const Value: Single);
Public
  Procedure Generate(var aData : IGSGeometryData); Override;
  Constructor Create; override;
  Destructor Destroy; Override;

  Property InnerRadius : Single read FInnerRadius WRite SetInnerRadius;
End;

//------------------------------------------------- Sub Operation.
TGS2DSubOpp = Class(TGSGeometryGenerator)
private
  FSubject: IGSGeometryData;
  FSubOp: IGSGeometryData;
Public
  Procedure Generate(var aData : IGSGeometryData); Override;

  Property Subject : IGSGeometryData Read FSubject Write FSubject;
  Property SubOp : IGSGeometryData read FSubOp Write FSubOp;
End;

TGSMesh2dToolsFactory = class
  class function CreateDiskGenerator(radius : single) : IGSGeometryGenerator;
  class function CreateDiskGeneratorAndGetResult(radius : single) : IGSGeometryData;
  class function CreateBooleanSubOpGenerator(subject, clipped : IGSGeometryData) : IGSGeometryGenerator;
  class function CreateBooleanSubOpGeneratorAndGetResult(subject, clipped : IGSGeometryData) : IGSGeometryData;
end;

implementation

{$IFDEF FPC}
function Pointf(x,y : single) : TPointf;
begin
  result.X := x;
  result.Y := y;
end;

{$ENDIF}

{ TGSMeshTurtle }

constructor TGSMeshTurtle.create(rawMesh: TGSRawMesh2d);
begin
  inherited create;
  Assert(assigned(rawMesh));
  FMesh := rawMesh;
  FDown := False;
  FAngle := 0;
  FStepValue := 10.0;
end;

destructor TGSMeshTurtle.Destroy;
begin
  FreeAndNil(FTool);
  FreeAndNil(fmesh);
  inherited;
end;

function TGSMeshTurtle.down: TGSMeshTurtle;
begin
  FDown := true;
  result := self;
end;

function TGSMeshTurtle.step: TGSMeshTurtle;
var l : Uint32;
begin
  FTool.MoveAhead;
  l := length(FMesh.vertices);
  SetLength(FMesh.vertices,l+1);
  FMesh.vertices[l] := vec2.create(FTool.Origin.X,FTool.Origin.Y);
  result := self;
end;

function TGSMeshTurtle.turnBy(angleInDegreeClocked: single): TGSMeshTurtle;
begin
  FTool.TurnBy(angleInDegreeClocked);
  result := self;
end;

function TGSMeshTurtle.turnHalf: TGSMeshTurtle;
begin
  fTool.TurnHalf;
  result := self;
end;

function TGSMeshTurtle.turnLeft: TGSMeshTurtle;
begin
  FTool.TurnLeft;
  result := self;
end;

function TGSMeshTurtle.turnRight: TGSMeshTurtle;
begin
  FTool.TurnRight;
  result := self;
end;

function TGSMeshTurtle.up: TGSMeshTurtle;
begin
  FDown := false;
  result := self;
end;

{ TGSMesh2dHelper }

procedure TGSMesh2dHelper.addTriangle(a, b, c: vec2);
var l,i : Uint32;
begin
  l := Length(vertices);
  i := Length(indexes);

  SetLength(vertices,l+3);
  SetLength(uvs,l+3);
  SetLength(indexes,i+3);
//  SetLength(cols,l+3);

  vertices[l] := a;
  indexes[i] := l;
  uvs[l].create(0,0);
//  cols[l].create(1,0,0,1);
  inc(l);
  inc(i);
  vertices[l] := b;
  indexes[i] := l;
  uvs[l].create(1,0);
//  cols[l].create(0,1,0,1);
  inc(l);
  inc(i);
  vertices[l] := c;
  indexes[i] := l;
  uvs[l].create(1,1);
//  cols[l].create(0,0,1,1);
  ProgressiveRefreshBounding(a);
  ProgressiveRefreshBounding(b);
  ProgressiveRefreshBounding(c);
end;

procedure TGSMesh2dHelper.SetShapeType(shapetype: TGSShape2dType);
begin
  vertices := nil;
  indexes := nil;
  case shapetype of
    TGSShape2dType.triangleOneOneOne: SetUpQuad(1);
    TGSShape2dType.quadOneOne: SetUpQuad(1);
    TGSShape2dType.rectOneTwo: SetUpRect(1,2);
    TGSShape2dType.rectTwoOne: SetUpRect(2,1);
    TGSShape2dType.penta: Build_SetRoundShape(5,1);
    TGSShape2dType.hexa: Build_SetRoundShape(6,1);
    TGSShape2dType.hepta: Build_SetRoundShape(7,1);
    TGSShape2dType.octo: Build_SetRoundShape(8,1);
    TGSShape2dType.circle20: Build_SetRoundShape(20,1);
    TGSShape2dType.circle50: Build_SetRoundShape(50,1);
    TGSShape2dType.circle100: Build_SetRoundShape(100,1);
    TGSShape2dType.halfmoon: ;
    TGSShape2dType.gear: ;
    TGSShape2dType.star5: ;
    TGSShape2dType.sun: ;
  end;
end;

procedure TGSMesh2dHelper.Build_SetRoundShape(subdivision : UInt32; len : single);
var generator : TGSDiskGenerator;
    dta : IGSGeometryData;
    m : TGSRawMesh2D;
begin
  reset;
  dta := TGSGeometry2DData.Create;
  generator := TGSDiskGenerator.Create;
  try
    generator.Subdivision := subdivision;
    generator.Radius := len;
    generator.Generate(dta);
  finally
    freeAndNil(generator);
  end;

  m := dta.GetMesh;

  try
    m.copy(Self);
  finally
    FreeAndNil(m);
  end;
end;


//------------------------------------------------------------------------------
function Interpolate(position, iTargetStart,
  iTargetEnd, iSourceStart, iSourceEnd: single): single;

var
  iTargetWidth, iSourceWidth: single;
  rPercent: single;
  rMoveMent: single;
begin
  iSourceWidth := iSourceEnd-(iSourceStart);
  iTargetWidth := iTargetEnd-(iTargetStart);

  if iSourceWidth = 0 then
    result := 0
  else begin
    rPercent := (position - iSourceStart)/(iSourceEnd-iSourceStart);
    rMovement := rPercent*iTargetWidth;
    result := rMoveMent + iTargetStart;

//    result := (((position-iSourceStart))/iSourceWidth)*((iTargetWidth))+iTargetStart;
  end;

end;


procedure GetCenterOfTriangle(x1,y1,x2,y2,x3,y3: single; out rx,ry: single);
begin
  rx := ((((x1+x2)/2)+((x1+x3)/2)+((x2+x3)/2))/3);
  ry := ((((y1+y2)/2)+((y1+y3)/2)+((y2+y3)/2))/3);
end;



function TGSMesh2dHelper.isPointInside(x, y: single; var TriangleIndice : UInt32): boolean;
var i,t : integer;
    a,b,c,au,bu,cu : vec2;
    ca : vec4;

Begin
  result := false;
  if (X >= BoundingBox.Left) and (X < BoundingBox.Right) and (Y >= BoundingBox.Top) and (Y < BoundingBox.Bottom) then
  begin
    for i := 0 to getTriangleCount-1 do
    begin
      Triangle(i,a,b,c,au,bu,cu,ca,ca,ca);
      if PointInTriangle(x,y,a.x,a.y,b.x,b.y,c.x,c.y) then
      begin
        result := true;
        TriangleIndice := i;
        break;
      end;
    end;
  end;
end;

{ TeMeshMatrixProcessing }

class procedure TGSMeshMatrixProcessing.Transform(
  var target: TGSRawMesh2d;
  const transformMatrix: Mat3);
begin
  Assert(assigned(target));
  target.UpdateVertices(transformMatrix);
end;


{ TGSGeometry2DData }

procedure TGSGeometry2DData.Clear;
begin
  ClearArray;
end;


procedure TGSGeometry2DData.ClearArray;
var i : Integer;
begin
{
  for i := Low(Mesh) to High(Mesh) do
  begin
    Mesh[i].P1.X:= 0;
    Mesh[i].P1.y:= 0;
    Mesh[i].P2.X:= 0;
    Mesh[i].P2.y:= 0;
    Mesh[i].P3.X:= 0;
    Mesh[i].P3.y:= 0;
  end;
}
  for i := Low(FBorder) to High(FBorder) do
  begin
    FBorder[i].X:= 0;
    FBorder[i].y:= 0;
  end;
end;

function TGSGeometry2DData.getBorder: Vec2sp;
begin
  result := @FBorder;
end;

function TGSGeometry2DData.GetMesh: TGSRawMesh2D;
var i : Integer;
    pa,pb,pc : vec2;
begin
  result := TGSRawMesh2D.Create;
  Result.reset;
  SetLength(result.vertices,length(FBorder));
  for i := Low(FBorder) to High(FBorder) do begin
    result.vertices[i].x := FBorder[i].X;
    result.vertices[i].y := FBorder[i].Y;
  end;
end;

procedure TGSGeometry2DData.Scale(xScale, yScale: Single);
var i : Integer;
begin
  for i := Low(FBorder) to High(FBorder) do
  begin
    FBorder[i] := vec2.create(FBorder[i].X * xScale, FBorder[i].Y * yScale);
  end;
end;

procedure TGSGeometry2DData.SetCapacity(aCapa: Integer);
begin
  SetLength(FBorder,aCapa);
  ClearArray;
end;

procedure TGSGeometry2DData.Translate(xAmount, yAmount: Single);
var i : Integer;
begin
  for i := Low(FBorder) to High(FBorder) do
  begin
    FBorder[i]:= vec2.create(FBorder[i].X + xAmount, FBorder[i].Y + yAmount);
  end;
end;

{ TGS2DGeometryGenerator }

constructor TGS2DGeometryGenerator.Create;
begin
  Inherited;
  LocalGeometryTool := TDirectionalObject.Create(0,0,1);
end;

destructor TGS2DGeometryGenerator.Destroy;
begin
  FreeAndNil(LocalGeometryTool);
  inherited;
end;


{ TGSDiskGenerator }

constructor TGSDiskGenerator.Create;
begin
  inherited;
  Subdivision := 30;
  Radius := 1;
end;

procedure TGSDiskGenerator.Generate(var aData : IGSGeometryData);
var i : integer;
    FStepAngle : Single;
begin
  aData.SetCapacity(FSubdivision);

  LocalGeometryTool.SetOrigin(0,0);
  LocalGeometryTool.Norm := FRadius;
  LocalGeometryTool.Angle := 0;

  FStepAngle := 360/FSubdivision;

  for i := 0 to FSubdivision-1 do
  begin
    LocalGeometryTool.TurnBy(FStepAngle);
    aData.getBorder^[i].X := LocalGeometryTool.GetPointedCoord.X;
    aData.getBorder^[i].Y := LocalGeometryTool.GetPointedCoord.Y;
  end;
  aData.getBorder^[FSubdivision-1] := aData.getBorder^[0];
end;

{ TGSDiskDonutGenerator }

constructor TGSDiskDonutGenerator.Create;
begin
  Inherited;
  FSecondaryGeometryTool := TDirectionalObject.Create(0,0,1);
  InnerRadius := 0.5;
end;

destructor TGSDiskDonutGenerator.Destroy;
begin
  FreeAndNil(FSecondaryGeometryTool);
  inherited;
end;

procedure TGSDiskDonutGenerator.Generate(var aData: IGSGeometryData);
var i,j : integer;
    FStepAngle : Single;
begin
  aData.SetCapacity(FSubdivision*2); //2 Circles. = twice triangle quantity, and twice border long.

  LocalGeometryTool.SetOrigin(0,0);
  LocalGeometryTool.Norm := FRadius;
  LocalGeometryTool.Angle := 0;

  FSecondaryGeometryTool.SetOrigin(0,0);
  FSecondaryGeometryTool.Norm := FInnerRadius;
  FSecondaryGeometryTool.Angle := 0;


  FStepAngle := 360/FSubdivision;

  //Outer circle.
  j := 0;
  for i := 0 to FSubdivision-1 do
  begin
//    aData.Border[j].Code := TGSEdgeCode.fecDraw;
    aData.getBorder^[j] := Vec2.create(LocalGeometryTool.GetPointedCoord.X,LocalGeometryTool.GetPointedCoord.Y);
    LocalGeometryTool.TurnBy(FStepAngle);
    inc(j);
  end;

  //Jump !
  aData.getBorder^[j] := vec2.create(FSecondaryGeometryTool.GetPointedCoord.X,FSecondaryGeometryTool.GetPointedCoord.Y);
//  aData.Border[j].Code := TGSEdgeCode.fecIgnore;
  FSecondaryGeometryTool.TurnBy(FStepAngle);
  inc(j);

  //Inner circle.
  for i := 0 to FSubdivision-1 do
  begin
//    aData.Border[j].Code := TGSEdgeCode.fecDraw;
    aData.getBorder^[j] := vec2.create(FSecondaryGeometryTool.GetPointedCoord.X,FSecondaryGeometryTool.GetPointedCoord.Y);
    FSecondaryGeometryTool.TurnBy(FStepAngle);
    inc(j);
  end;
end;

procedure TGSDiskDonutGenerator.SetInnerRadius(const Value: Single);
begin
  FInnerRadius := Value;
  if FInnerRadius>(Radius - 0.001*Radius) then
    FInnerRadius := Radius - 0.001*Radius;
end;


{ TGS2DSubOpp }

procedure TGS2DSubOpp.Generate(var aData: IGSGeometryData);
var c : TClipper;
    a,b : TPath;
    r : TPaths;

    i : integer;
begin
  Assert(Assigned(Subject));
  Assert(Assigned(SubOp));

  //Todo : Paths with other dim (Jump sequence in border)
  SetLength(a, Length(Subject.getBorder^));
  SetLength(b, Length(SubOp.getBorder^));

  for I := Low(Subject.getBorder^) to High(Subject.getBorder^) do
  begin
    a[i].X := Round(Subject.getBorder^[i].X*1000);
    a[i].Y := Round(Subject.getBorder^[i].Y*1000);
  end;

  for I := Low(SubOp.getBorder^) to High(SubOp.getBorder^) do
  begin
    b[i].X := Round(SubOp.getBorder^[i].X*1000);
    b[i].Y := Round(SubOp.getBorder^[i].Y*1000);
  end;

  //Use Clipper lib to generate aData starting from Subject and SubOp.
  c := TClipper.Create;
  c.AddPath(a,ptSubject);
  c.AddPath(b,ptClip);
  c.Execute(ctDifference,frEvenOdd,r);

  aData.SetCapacity(LEngth(r[0]));
  for I := Low(r[0]) to High(r[0]) do
  begin
    aData.getBorder^[i].X := r[0][i].X/1000;
    aData.getBorder^[i].Y := r[0][i].Y/1000;
  end;

  c.Free;
end;


{ TGSMesh2dToolsFactory }

class function TGSMesh2dToolsFactory.CreateBooleanSubOpGenerator(subject,
  clipped: IGSGeometryData): IGSGeometryGenerator;

begin
  result := TGS2DSubOpp.Create;
  TGS2DSubOpp(result).Subject := subject;
  TGS2DSubOpp(result).SubOp := clipped;
end;

class function TGSMesh2dToolsFactory.CreateBooleanSubOpGeneratorAndGetResult(
  subject, clipped: IGSGeometryData): IGSGeometryData;
begin
  result := TGSGeometry2DData.Create;
  CreateBooleanSubOpGenerator(subject,clipped).Generate(result);
end;

class function TGSMesh2dToolsFactory.CreateDiskGenerator(
  radius: single): IGSGeometryGenerator;
begin
  result := TGSDiskGenerator.Create;
  TGSDiskGenerator(result).Radius := radius;
end;

class function TGSMesh2dToolsFactory.CreateDiskGeneratorAndGetResult(
  radius: single): IGSGeometryData;
var lgenerator : IGSGeometryGenerator;
begin
  result := TGSGeometry2DData.Create;
  lgenerator := CreateDiskGenerator(radius);
  lgenerator.Generate(result);
end;

{ TGSMeshGrapher }

constructor TGSMeshGrapher.Create;
begin
  Mesh := TGSRawMesh2D.Create;
  FTool := TDirectionalObject.Create(0,0,1);
  FX := 0;
  FY := FX;
end;

destructor TGSMeshGrapher.Destroy;
begin
  FreeAndNil(Mesh);
  FreeAndNil(FTool);
  inherited;
end;

function TGSMeshGrapher.lineTo(x, y: single;
  const thickness: single): iGSMeshGrapher;
var v1,v2,v3,v4 : Vec2;
    lpt : TPt;
begin
  if thickness>0 then
  begin
    FTool.SetOrigin(FX,FY);
    FTool.SetPointedCoord(point(x,y));
    FTool.Norm := thickness;
    FTool.TurnLeft;
    lpt := FTool.GetPointedCoord;
    v1.x := lpt.X; v1.y := lpt.Y;
    FTool.TurnHalf;
    lpt := FTool.GetPointedCoord;
    v2.x := lpt.X; v2.y := lpt.Y;
    FTool.SetPointedCoord(point(x,y));
    FTool.MoveAhead;
    FTool.Norm := thickness;
    FTool.TurnLeft;
    lpt := FTool.GetPointedCoord;
    v3.x := lpt.X; v3.y := lpt.Y;
    FTool.TurnHalf;
    lpt := FTool.GetPointedCoord;
    v4.x := lpt.X; v4.y := lpt.Y;

    Mesh.addTriangle(v1,v2,v3);
    Mesh.addTriangle(v2,v4,v3);
    //2 sides ?
    //Mesh.addTriangle(v3,v2,v1);
    //Mesh.addTriangle(v3,v4,v2);
  end;
  result := self;
end;

function TGSMeshGrapher.moveTo(x, y: single): iGSMeshGrapher;
begin
  FX := x;
  FY := y;
  result := self;
end;

function TGSMeshGrapher.rect(x, y, width, height: single): iGSMeshGrapher;
var l : IGSRawMesh;
begin
  l := TGSRawMesh2D.Create;
  l.SetUpRect(width,height);
  l.Pan(x,y);
  Mesh.Merge(l as TGSRawMesh2D);
  result := self;
end;

end.
