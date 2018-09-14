unit raytrace;

interface

uses Classes,
     Math,
     Generics.Collections,
     SysUtils;

type

TRawBitmap = Class
Private
  FWidth: Integer;
  FHeight: Integer;

Public
  Buffer : Array of Byte;

  function ScanLine(LineIndex : Integer) : Pointer;

  Procedure SetSize(aWidth, aHeight : integer);

  Property Width : Integer read FWidth;
  Property Height : Integer read FHeight;
end;

Surface = class;
Light = class;

Vector = record
    x,y,z:Double ;
    class operator Add(const a, b: Vector): Vector;
    class operator Subtract(const a, b: Vector): Vector;
    class operator Multiply( k:Double ; const b: Vector): Vector;

    constructor Create(x,y,z:Double );

    function dot(const v:Vector):Double;
    function norm():Vector;
    function cross(const v:Vector):Vector;
    function mag():Double ;
end;

RGBColor = record
  a, b, g, r: Byte
end;

Color = record
  r,g,b:Double;

  constructor Create(r,g,b:Double );

  class operator Add(const a, b: Color): Color;
  class operator Multiply(k:Double; const b: Color): Color;
  class operator Multiply(const a, b: Color): Color;

  class function Clamp(v:Double):Byte; static;
  function ToDrawingColor:RGBColor;
end;

Camera = class(TObject)
public
  forwardDir: Vector;
  right: Vector;
  up: Vector;
  pos: Vector;
  constructor Create(pos:Vector; lookAt:Vector);
end;

Thing = class;
Ray = class;

Intersection = record
  thing: Thing;
  ray:  Ray;
  dist: Double;
  constructor Create(thing:Thing; ray:Ray; dist:Double);
  class function Invalid : Intersection; static;

  function IsValid : boolean;
 end;

Thing = class(TObject)
  public
  function intersect(ray: Ray) : Intersection; virtual; abstract;
  function normal (pos: Vector): Vector; virtual; abstract;
  function surface:Surface;virtual;  abstract;
end;

Ray = class(TObject)
   start:Vector;
   dir:Vector;
   public
   constructor Create();overload;
   constructor Create(start,dir:Vector);overload;
   function ToString:String;override;
end;

Surface = class abstract
public
  function diffuse(const pos:Vector):Color; virtual;  abstract;
  function specular(const pos:Vector):Color; virtual;  abstract;
  function reflect(const pos:Vector):Double ; virtual;  abstract;
  function roughness:Double ;virtual;abstract;
end;

ShinySurface = class(Surface)
  public
    function diffuse(const pos: Vector): Color; override;
    function specular(const pos: Vector): Color; override;
    function reflect(const pos: Vector): Double ; override;
    function roughness: Double ; override;
end;

CheckerboardSurface = class(Surface)
  public
    function diffuse(const pos: Vector): Color; override;
    function specular(const pos: Vector): Color; override;
    function reflect(const pos: Vector): Double ; override;
    function roughness: Double ; override;
end;

Scene = class(TObject)
public
  shiny:ShinySurface;
  checkerboard:CheckerboardSurface;

  things:TObjectList<Thing>;
  lights:TObjectList<Light>;
  xcamera:Camera;
  constructor Create(varPart : Double); Reintroduce;
  destructor Destroy; Override;
end;

RayTracerEngine = class
  private
    maxDepth:Integer;

  public
    constructor Create;

    function intersections(ray: Ray; scene: Scene):Intersection;
    function testRay(ray: Ray; scene: Scene):Double ;
    function traceRay(ray: Ray; scene: Scene; depth: integer): Color;
    function shade(isect: Intersection; scene: Scene; depth: integer):Color;

    function getReflectionColor(thing: Thing; const pos, normal, rd: Vector; scene: Scene; depth: integer):Color;
    function getNaturalColor(thing: Thing; const pos, norm, rd: Vector; scene: Scene):Color;

    Function Render(  scene:Scene;
                      ImgWidth, ImgHeight, //Image total size.
                      X1,X2 : integer;     //X interval of rendering.
                      Image : TRawBitmap) : Boolean;
end;


Light = class
public
  pos:Vector;
  color:Color;
  constructor Create(pos:Vector; color:Color);
end;

Sphere = class(Thing)
  private
    radius2:Double ;
    center:Vector;
    _surface:Surface;
  public
    function intersect(ray: Ray): Intersection; override;
    function normal(pos: Vector): Vector; override;
    function surface: Surface; override;

    constructor Create(center:Vector; radius:Double ; surface:Surface);
end;

Plane = class(Thing)
  private
    norm:Vector;
    offset:Double ;
    _surface:Surface;
  public
    function intersect(ray: Ray): Intersection; override;
    function normal(pos: Vector): Vector; override;
    function surface: Surface; override;

    constructor Create(norm:Vector; offset:Double ; surface:Surface);
end;


Var
  Colorwhite : Color;
  Colorgrey  : Color;
  Colorblack : Color;

  ColordefaultColor : Color;
  Colorbackground   : Color;


implementation

{ Vector }
constructor Vector.Create(x,y,z:Double );
begin
   self.x := x;
   self.y := y;
   self.z := z;
end;

function Vector.cross(const v: Vector): Vector;
begin
    Result.x := y * v.z - z * v.y;
    Result.y := z * v.x - x * v.z;
    Result.z := x * v.y - y * v.x;
end;

function Vector.dot(const v: Vector): Double ;
begin
    Result := x * v.x +
              y * v.y +
              z * v.z;
end;

function Vector.mag: Double ;
begin
    Result := Sqrt(x * x + y * y + z * z);
end;

class operator Vector.Multiply(k: Double ;const b: Vector): Vector;
begin
    Result.x := k * b.x;
    Result.y := k * b.y;
    Result.z := k * b.z;
end;

function Vector.norm: Vector;
var
    divBy, m:Double ;
begin
    m := self.mag();

    if (m = 0) then
      divBy := MaxDouble
    else
      divBy := 1.0 / m;

    Result := divBy * self;
end;

class operator Vector.Subtract(const a, b: Vector): Vector;
begin
    Result.x := a.x - b.x;
    Result.y := a.y - b.y;
    Result.z := a.z - b.z;
end;

class operator Vector.Add(const a, b: Vector): Vector;
begin
    Result.x := a.x + b.x;
    Result.y := a.y + b.y;
    Result.z := a.z + b.z;
end;

{ Color }

class operator Color.Add(const a, b: Color): Color;
begin
    Result.r := a.r + b.r;
    Result.g := a.g + b.g;
    Result.b := a.b + b.b;
end;

class operator Color.Multiply(k: Double ; const b: Color): Color;
begin
  Result.r := k*b.r;
  Result.g := k*b.g;
  Result.b := k*b.b;
end;

class function Color.Clamp(v: Double ): Byte;
begin
  if v > 255 then
    Result := 255
  else
    Result := Byte(Floor(v));
end;

constructor Color.Create(r, g, b: Double );
begin
  self.r := r;
  self.g := g;
  self.b := b;
end;

class operator Color.Multiply(const a, b: Color): Color;
begin
  Result.r := a.r * b.r;
  Result.g := a.g * b.g;
  Result.b := a.b * b.b;
end;

function Color.ToDrawingColor: RGBColor;
begin
  Result.r :=  Color.Clamp(self.r * 255.0);
  Result.g :=  Color.Clamp(self.g * 255.0);
  Result.b :=  Color.Clamp(self.b * 255.0);
  Result.a :=  255;
end;

{ Camera }

constructor Camera.Create(pos, lookAt: Vector);
var
  down:Vector;
begin

  down := Vector.Create(0.0, -3.0, 0.0);

  self.pos        := pos;
  self.forwardDir := (lookAt - self.pos).norm;
  self.right      := 1.5 *  self.forwardDir.cross(down).norm();
  self.up         := 1.5 *  self.forwardDir.cross(self.right).norm() ;
end;

{ Ray }

constructor Ray.Create(start, dir: Vector);
begin
  self.start := start;
  self.dir   := dir;
end;

constructor Ray.Create;
begin
end;

function Ray.ToString: String;
begin
  Result := Format ('[%.4f,%.4f,%.4f]', [dir.x, dir.y, dir.z]);
end;

{ Intersection }

constructor Intersection.Create(thing: Thing; ray: Ray; dist: Double );
begin
  self.thing := thing;
  self.ray := ray;
  self.dist := dist;
end;

class function Intersection.Invalid : Intersection;
begin
  result.thing := nil;
  result.ray := nil;
  result.dist := 0;
end;

function Intersection.IsValid : boolean;
begin
  result := Assigned(self.thing);
end;


{ Light }
constructor Light.Create(pos: Vector; color: Color);
begin
  self.pos := pos;
  self.color := color;
end;

{ Sphere }
constructor Sphere.Create(center: Vector; radius: Double ; surface: Surface);
begin
  self.radius2 := radius * radius;
  self.center := center;
  self._surface := surface;
end;

function Sphere.intersect(ray: Ray): Intersection;
var
  eo:Vector;
  v,dist,disc:Double ;
begin

  eo    := self.center - ray.start;
  v     := eo.dot(ray.dir);
  dist  := 0;

  if (v >= 0) then
  begin
    disc := self.radius2 - (eo.dot(eo) - (v * v));
    if (disc >= 0) then
    begin
      dist := v - Sqrt(disc);
    end;
  end;

  if (dist = 0) then
    Result := Intersection.Invalid()
  else
  begin
    Result := Intersection.Create(self, ray, dist);
  end;

end;

function Sphere.normal(pos: Vector): Vector;
begin
  Result := (pos - self.center).norm;
end;

function Sphere.surface: Surface;
begin
  Result := self._surface;
end;

{ ShinySurface }

function ShinySurface.diffuse(const pos: Vector): Color;
begin
  Result := Colorwhite;
end;

function ShinySurface.reflect(const pos: Vector): Double ;
begin
  Result := 0.7;
end;

function ShinySurface.roughness: Double ;
begin
  Result := 250;
end;

function ShinySurface.specular(const pos: Vector): Color;
begin
  Result := Colorgrey;
end;

{ CheckerboardSurface }
function CheckerboardSurface.diffuse(const pos: Vector): Color;
begin
  if ((floor(pos.z) + floor(pos.x)) mod 2) <> 0 then
    Result := Colorwhite
  else
    Result := Colorblack;
end;

function CheckerboardSurface.reflect(const pos: Vector): Double ;
begin
  if ((floor(pos.z) + floor(pos.x)) mod 2) <> 0 then
    Result := 0.1
  else
    Result := 0.7;
end;

function CheckerboardSurface.roughness: Double ;
begin
  Result := 150.0;
end;

function CheckerboardSurface.specular(const pos: Vector): Color;
begin
  Result := Colorwhite;
end;

{ Plane }

constructor Plane.Create(norm: Vector; offset: Double ; surface: Surface);
begin
  self.norm := norm;
  self.offset := offset;
  self._surface := surface;
end;

function Plane.intersect(ray: Ray): Intersection;
var
  dist, denom : Double ;
begin
  denom := self.norm.dot(ray.dir);
  if (denom > 0) then
  begin
    Result := Intersection.Invalid();
  end else
  begin
    dist := (norm.dot(ray.start) + offset) / (-denom);
    Result := Intersection.Create(self, ray, dist);
  end;

end;

function Plane.normal(pos: Vector): Vector;
begin
  Result := self.norm;
end;

function Plane.surface: Surface;
begin
  result := _surface;
end;

{ RayTracerEngine }

constructor RayTracerEngine.Create;
begin
  Self.maxDepth := 5;
end;

function RayTracerEngine.getNaturalColor(thing: Thing; const pos, norm, rd: Vector;
  scene: Scene): Color;

var
  resultColor:Color;
  item: Light;
  diffuseColor, specularColor: Color;

  function addLight(light:Light): Color;
  var
    ldis, livec :Vector;
    testRay:Ray;
    neatIsect: Double ;
    isInShadow:Boolean;
    illum,specular:Double ;
    lcolor,scolor:Color;

  begin
      ldis      := light.pos - pos;
      livec     := ldis.norm;

      testRay   := Ray.Create(pos,livec);
      neatIsect := self.testRay(testRay, scene);
      testRay.Free;

      if (IsNan(neatIsect)) then
        isInShadow := false
      else
        isInShadow := (neatIsect <= ldis.mag);

      if isInShadow then exit();
      illum := livec.dot(norm);

      if illum > 0 then
        lcolor :=  illum * light.color
      else
        lcolor := ColordefaultColor;

      specular := livec.dot(rd.norm);

      if specular > 0 then
        scolor := Math.Power(specular,thing.surface.roughness) * light.color
      else
        scolor := ColordefaultColor;

      resultColor :=  resultColor + ((diffuseColor  * lcolor) +
                                     (specularColor * scolor));
  end;

begin

  resultColor := ColordefaultColor;
  diffuseColor := thing.surface.diffuse(pos);
  specularColor := thing.surface.specular(pos);

  for item in scene.lights do
  begin
    addLight(item);
  end;
  Result := resultColor;

end;

function RayTracerEngine.getReflectionColor(thing: Thing; const pos, normal,
  rd: Vector; scene: Scene; depth: integer): Color;
var
  r:Ray;
begin

  r := Ray.Create(pos, rd);
  Result :=  thing.surface.reflect(pos) * self.traceRay(r, scene, depth+1);
  r.Free;

end;

function RayTracerEngine.intersections(ray: Ray; scene: Scene): Intersection;
var
  closest:Double ;
  inter,closestInter: Intersection;
  i,n:Integer;
  item:Thing;
begin
  closest := MaxInt;
  closestInter := Intersection.Invalid();

  n := scene.things.Count - 1;

  for i := 0 to n do
  begin
    inter :=  scene.things[i].intersect(ray);
    if (inter.IsValid()) and (inter.dist < closest) then
    begin
         closestInter := inter;
         closest := inter.dist;
    end;
  end;
  result := closestInter;
end;

//procedure RayTracerEngine.render(scene: Scene; img: Vcl.Graphics.TBitmap);
Function RayTracerEngine.render( scene:Scene;
                                 ImgWidth, ImgHeight, //Image total size.
                                 X1,X2 : integer;     //X interval of rendering.
                                 Image : TRawBitmap) : Boolean;

  var
    x,y,w,h : Integer;
    destColor  : Color;
    testRay    : Ray;
    c       : RGBColor;
    stride, pos  : Integer;
    start   : Integer;
    lPixelSize : Byte;
    lmem : Integer;

  function getPoint(x,y:Integer; camera:Camera):Vector;
  var
    recenterX, recenterY:Double ;
  begin
      recenterX := (x - (w / 2.0)) / 2.0 / w;
      recenterY :=  - (y - (h / 2.0)) / 2.0 / h;
      Result :=(camera.forwardDir + ((recenterX * camera.right) + (recenterY * camera.up ))).norm();
  end;

begin
  result := true;
  lPixelSize := SizeOf(Integer);
  { TODO : add try except }

  w := ImgWidth - 1;
  h := ImgHeight - 1;

  start  := 0;
  stride := Image.Width*lPixelSize;
  testRay   := Ray.Create();

  lmem := (Image.Width)*(Image.Height-1)*lPixelSize;
  for y := 0 to h-1 do
  begin
    for x := X1 to X2-1 do
    begin
      testRay.start := scene.xcamera.pos;
      testRay.dir := getPoint(x, y, scene.xcamera);

      destColor := self.traceRay(testRay, scene, 0);
      c := destColor.ToDrawingColor;

      pos :=  ((lmem - y*stride) + (x-X1)*lPixelSize);
      //pos :=  ((y*stride) + (x-X1)*lPixelSize);

      Image.Buffer[pos]   := c.b;
      Image.Buffer[pos+1] := c.g;
      Image.Buffer[pos+2] := c.r;
    end;
  end;

  testRay.Free;
end;

function RayTracerEngine.shade(isect: Intersection; scene: Scene; depth: integer): Color;
var
  d:Vector;
  pos, normal,reflectDir:Vector;
  naturalColor,reflectedColor:Color;

begin
  d   := isect.ray.dir;
  pos := (isect.dist * d) + isect.ray.start;
  normal := isect.thing.normal(pos);
  reflectDir := d - (2.0 * normal.dot(d) * normal);

  naturalColor := Colorbackground + self.getNaturalColor(isect.thing, pos, normal, reflectDir, scene);

 if depth >= self.maxDepth then
    reflectedColor := Colorgrey
 else
    reflectedColor := self.getReflectionColor(isect.thing, pos, normal, reflectDir, scene, depth);

 Result := naturalColor + reflectedColor;
end;


function RayTracerEngine.testRay(ray: Ray; scene: Scene): Double ;
var
  isect: Intersection;
begin
  isect := self.intersections(ray, scene);
  if isect.IsValid() then
  begin
    exit(isect.dist);
  end;
  Result := NaN;
end;

function RayTracerEngine.traceRay(ray: Ray; scene: Scene; depth: integer): Color;
var
  isect:Intersection;
begin
  isect := self.intersections(ray, scene);
  if (isect.IsValid()) then
  begin
    Result := self.shade(isect, scene, depth);
  end else
  begin
    Result := Colorbackground;
  end;
end;

{ Scene }

constructor Scene.Create(varPart : Double);
begin
  shiny:=ShinySurface.Create;
  checkerboard:=CheckerboardSurface.Create;

  things := TObjectList<Thing>.Create();
  lights := TObjectList<Light>.Create();

  things.Add( Plane.Create (Vector.Create(0.0, 1.0, 0.0), 0.0, checkerboard));
//  things.Add( Sphere.Create(Vector.Create(0.0, 1.0, 0.5), 1.0, Shiny ));
  things.Add( Sphere.Create(Vector.Create(0.0, 1.0, VarPart), 1.0, Shiny ));
  things.Add( Sphere.Create(Vector.Create(-1.0, 0.5, 1.5), 0.5, shiny));
//  things.Add( Sphere.Create(Vector.Create(-2.0, 0.2, 0), 1.2, checkerboard));

  lights.Add(Light.Create(Vector.Create(200, 10, 0.0), Color.Create(0.5, 0.5, 0.5)));

  lights.Add(Light.Create(Vector.Create(-2.0, 2.5, 0.0), Color.Create(0.49, 0.07, 0.07)));
  lights.Add(Light.Create(Vector.Create(1.5, 2.5, 1.5),  Color.Create(0.07, 0.07, 0.49)));
  lights.Add(Light.Create(Vector.Create(1.5, 2.5, -1.5), Color.Create(0.07, 0.49, 0.071)));
  lights.Add(Light.Create(Vector.Create(0.0, 3.5, 0.0),  Color.Create(0.21, 0.21, 0.35)));

  self.xcamera := Camera.Create(Vector.Create(3.0, 2.0, 5.0), Vector.Create(-1.0, 0.5, 0.0));

end;

destructor Scene.Destroy;
begin
  FreeAndNil(things);
  FreeAndNil(lights);
  FreeAndNil(xcamera);
  FreeAndNil(shiny);
  FreeAndNil(checkerboard);
  inherited;
end;

{ TRawBitmap }

function TRawBitmap.ScanLine(LineIndex: Integer): Pointer;
begin
  result := Pointer(Integer(@Buffer)+LineIndex*FWidth*SizeOf(Int32));
end;

procedure TRawBitmap.SetSize(aWidth, aHeight: integer);
begin
  FWidth := aWidth;
  FHeight := aHeight;
  SetLength(Buffer,aWidth*aHeight*SizeOf(Int32));
end;

Initialization
  Colorwhite := Color.Create(1,1,1);
  Colorgrey  := Color.Create(0.5,0.5,0.5);
  Colorblack := Color.Create(0,0,0);

  ColordefaultColor := Colorblack;
  Colorbackground   := Colorblack;
end.
