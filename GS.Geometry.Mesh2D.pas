unit GS.Geometry.Mesh2D;

interface

uses SysUtils, Classes,
     GS.Geometry;

type
TGSRawMesh2D = class;
IGSRawMesh = interface
  procedure reset;
  procedure SetUpQuad(side : single);
  procedure SetUpRect(width,height : single);
  procedure Rotate(angle : single); //Todo : Specify a center ??
  procedure Scale(x,y : single);
  procedure Pan(x,y : single);
  procedure Modify(xpos,ypos,angle : single); overload;
  procedure Modify(xpos,ypos,angle,xscale,yscale : single); overload;
  procedure UpdateVertices(const matrix : Mat3);
  procedure reCenter; //Back all coordinate surround (0,0)
  procedure Merge(from : TGSRawMesh2D);
end;

TGSRawMesh2D = class(TInterfacedObject, IGSRawMesh)
private
protected
public
  BoundingBox : Vec4;
  vertices : array of vec2;
  uvs : array of vec2;       //...
//  cols : array of vec4;      //... Synchro with vertices.
  indexes : array of Uint32;

  Constructor Create; virtual;

  procedure reset;
  procedure SetUpQuad(side : single);
  procedure SetUpRect(width,height : single);

  procedure Merge(from : TGSRawMesh2D);

  //Setup....

  //This modify the vertices ! No way to return exactly due to processing approx..
  //to reverse a rotate exactly, we have to reset (setupxxx) the vertices before.
  procedure Rotate(angle : single); //Todo : Specify a center ??
  procedure Scale(x,y : single);
  procedure Pan(x,y : single);
  procedure Modify(xpos,ypos,angle : single); overload;
  procedure Modify(xpos,ypos,angle,xscale,yscale : single); overload;

  procedure UpdateVertices(const matrix : Mat3);

  function fullRefreshBounding : vec4;
  procedure progressiveRefreshBounding(a : vec2); //to Call when you add mesh.
  function BoudingCenter : vec2;
  procedure resetBoundingBox;

  procedure reCenter; //Back all coordinate surround (0,0)

  function getVerticeCount : Uint32;
  function getIndicesCount : Uint32;

  function getTriangleCount : Uint32;
  procedure Triangle(const index : Uint32; out a,b,c, uva,uvb,uvc : vec2; out ca,cb,cc : vec4);

  procedure copy(var destination : TGSRawMesh2D);
end;


implementation

{ TGSRawMesh2D }

procedure TGSRawMesh2D.Modify(xpos, ypos, angle: single);
var lLocalMatrix : Mat3;
begin
  lLocalMatrix := Mat3Identity;
  lLocalMatrix := lLocalMatrix * Mat3CreateRotation(angle);
  lLocalMatrix := lLocalMatrix * Mat3CreateTranslation(xpos,ypos);
  UpdateVertices(lLocalMatrix);
end;

function TGSRawMesh2D.BoudingCenter: vec2;
begin
  result.create(BoundingBox.left + abs(BoundingBox.right - BoundingBox.Left)/2,
                BoundingBox.top + abs(BoundingBox.top - BoundingBox.bottom)/2);
end;

procedure TGSRawMesh2D.copy(var destination: TGSRawMesh2D);
begin
  SetLength(destination.vertices,length(vertices));
  SetLength(destination.indexes,length(indexes));
  SetLength(destination.uvs,length(uvs));
//  SetLength(destination.cols,length(cols));

  Move(vertices[0],destination.vertices[0],length(vertices)*sizeOf(vec2));
  Move(uvs[0],destination.uvs[0],length(uvs)*sizeOf(vec2));
//  Move(cols[0],destination.cols[0],length(cols)*sizeOf(vec2));
  Move(indexes[0],destination.indexes[0],length(indexes)*SizeOf(Uint32));

  destination.BoundingBox := BoundingBox;
end;

constructor TGSRawMesh2D.create;
begin
  Inherited;
//  SetUpQuad(1);
end;


function TGSRawMesh2D.FullRefreshBounding: vec4;
var i : integer;
    p : vec2;
begin
  result.create(999999,99999,-999999,-999999);
  for i := 0 to length(vertices)-1 do
  begin
    p.create(vertices[i].x,vertices[i].y);
    if result.left>p.X then
      result.left := p.X;
    if result.top>p.Y then
      result.top := p.Y;
    if result.right<p.X then
      result.right := p.X;
    if result.bottom<p.Y then
      result.bottom := p.Y;
  end;
  BoundingBox := result;
end;


function TGSRawMesh2D.getIndicesCount: Uint32;
begin
  result := System.Length(indexes);
end;

function TGSRawMesh2D.getTriangleCount: Uint32;
begin
  result := Length(indexes) div 3;
end;

function TGSRawMesh2D.getVerticeCount: Uint32;
begin
  result := System.Length(vertices);
end;


procedure TGSRawMesh2D.ProgressiveRefreshBounding(a: vec2);
begin
  if a.x < BoundingBox.Left then
    BoundingBox.Left := a.x;
  if a.x > BoundingBox.right then
    BoundingBox.right := a.x;
  if a.y < BoundingBox.top then
    BoundingBox.top := a.y;
  if a.y > BoundingBox.bottom then
    BoundingBox.bottom := a.y;
end;

procedure TGSRawMesh2D.Merge(from: TGSRawMesh2D);
var cp : integer;
  I: Integer;
  vold, iold : integer;
  iv : integer;
begin
{  if (from.getVerticeCount=0) or (from.getIndicesCount=0) then
    exit;
  //vetices copy.
  cp := System.Length(vertices);
  SetLength(vertices,Length(vertices)+length(from.vertices));
  move(from.vertices[0],vertices[cp],length(from.vertices)*Sizeof(from.vertices[0]));

  //uvs
  cp := System.Length(uvs);
  SetLength(uvs,Length(uvs)+length(from.uvs));
  move(from.uvs[0],uvs[cp],length(from.uvs)*Sizeof(from.uvs[0]));

  //cols
//  cp := System.Length(cols);
//  SetLength(cols,Length(cols)+length(from.cols));
//  move(from.cols[0],cols[cp],length(from.cols)*Sizeof(from.cols[0]));

  cp := System.Length(indexes);
  SetLength(indexes,System.Length(indexes)+System.length(from.indexes));
  move(from.indexes[0],indexes[cp],System.length(from.indexes)*Sizeof(from.indexes[0]));
  if cp>0 then //index corrector.
    for i := cp-1 to System.length(indexes)-1 do
      indexes[i] := indexes[i] + cp;
  FullRefreshBounding;
}

    vold := getVerticeCount;
    iv := 0;

    SetLength(vertices,vold+from.getVerticeCount);

    for I := vold to getVerticeCount- 1 do
    begin
      Vertices[I].x :=  from.vertices[iv].X;
      Vertices[I].y :=  from.vertices[iv].Y;
      iv := iv + 1;
    end;

    iold := getIndicesCount;
    iv := 0;

    setLength(indexes,iold + length(from.indexes));
    for I := iold to getIndicesCount - 1 do
    begin
      indexes[I] := vold + from.indexes[iv];
      iv := iv+1;
    end;
end;

procedure TGSRawMesh2D.Modify(xpos, ypos, angle, xscale, yscale: single);
var fLocalMatrix : Mat3;
begin
  FLocalMatrix := Mat3Identity;
  FLocalMatrix := FLocalMatrix * Mat3CreateRotation(angle);
  FLocalMatrix := FLocalMatrix * Mat3CreateScaling(xscale,yscale);
  FLocalMatrix := FLocalMatrix * Mat3CreateTranslation(xpos,ypos);
  UpdateVertices(fLocalMatrix);
end;

procedure TGSRawMesh2D.Pan(x, y: single);
var fLocalMatrix : Mat3;
begin
  FLocalMatrix := Mat3Identity;
  FLocalMatrix := FLocalMatrix * Mat3CreateTranslation(x,y);
  UpdateVertices(fLocalMatrix);
end;

procedure TGSRawMesh2D.reCenter;
begin
  fullRefreshBounding;
  Pan(-BoudingCenter.x,-BoudingCenter.y);
end;

procedure TGSRawMesh2D.reset;
begin
  vertices := nil;
  indexes := nil;
  uvs := nil;
//  cols := nil;
  resetBoundingBox;
end;

procedure TGSRawMesh2D.resetBoundingBox;
begin
  BoundingBox.create(M_MXVT,M_MXVT,-M_MXVT,-M_MXVT);
end;

procedure TGSRawMesh2D.Rotate(angle: single);
var fLocalMatrix : Mat3;
begin
  FLocalMatrix := Mat3Identity;
  FLocalMatrix := FLocalMatrix * Mat3CreateRotation(angle*Pi/180);
  UpdateVertices(fLocalMatrix);
end;

procedure TGSRawMesh2D.Scale(x, y: single);
var fLocalMatrix : Mat3;
begin
  FLocalMatrix := Mat3Identity;
  FLocalMatrix := FLocalMatrix * Mat3CreateScaling(x,y);
  UpdateVertices(fLocalMatrix);
end;

procedure TGSRawMesh2D.UpdateVertices(const matrix: Mat3);
var i : integer;
    p : vec2;
begin
  resetBoundingBox;
  for i := 0 to length(vertices)-1 do
  begin
    p := vec2.create(vertices[i].x,vertices[i].y) * matrix;
    vertices[i].x := p.x;
    vertices[i].y := p.y;
    ProgressiveRefreshBounding(p);
  end;
end;


procedure TGSRawMesh2D.SetUpQuad(side: single);
begin
  SetUpRect(side,side);
end;

procedure TGSRawMesh2D.SetUpRect(width, height: single);
begin
  setLength(vertices,4);
  vertices[0].create(-width/2,-height/2);
  vertices[1].create(width/2,-height/2);
  vertices[2].create(width/2,height/2);
  vertices[3].create(-width/2,height/2);

  setLength(indexes,6);
  setLength(uvs,4);
//  setLength(cols,4);

  indexes[0] := 0;
  indexes[1] := 1;
  indexes[2] := 2;
  indexes[3] := 2;
  indexes[4] := 3;
  indexes[5] := 0;

  uvs[0] := vec2.create(0,0);
  uvs[1] := vec2.create(1,0);
  uvs[2] := vec2.create(1,1);
  uvs[3] := vec2.create(0,1);

//  cols[0] := vec4.create(1,0,0,1);
//  cols[1] := vec4.create(0,1,0,1);
//  cols[2] := vec4.create(0,0,1,1);
//  cols[3] := vec4.create(1,1,1,1);

  BoundingBox.create(-width/2,-height/2,width/2,height/2);
end;


procedure TGSRawMesh2D.Triangle(const index: Uint32; out a, b, c, uva,uvb,uvc : vec2; out ca,cb,cc : vec4);
var baseIndex : Uint32;
    ai,bi,ci : Uint32;
begin
  assert(index<=getTriangleCount);
  baseIndex := index*3;
  ai := indexes[baseIndex];
  bi := indexes[baseIndex+1];
  ci := indexes[baseIndex+2];
  a := vertices[ai];
  b := vertices[bi];
  c := vertices[ci];
  uva := uvs[ai];
  uvb := uvs[bi];
  uvc := uvs[ci];
//  ca := cols[ai];
//  cb := cols[bi];
//  cc := cols[ci];
end;

end.
