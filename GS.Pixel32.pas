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
{-----------------------------------------------------------------------------
 Unit Name : GS.Pixel32
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic Pxel class.
 Date:     : 2020031
 History   :
 20200301 - Creating unit.

Credits :
 - https://nothings.org/gamedev/rasterize/
 - Yuriy Kotsarenko's TextMapChHe
 - Paul Toth's Execute.Pixels "Flat" code reuse
 - Angus Johnson's Image32 architecture (ideas - Such as "shader" pixel).
 - Anthony Walter's Pixels.pas unit. (effect's architecture).
 - http://www.delphiarea.com for Gradient technichs.

Description :
BASIC IMPLEMENTATION FULL 32BITS PIXEL ACCESS.
For common, fast usage.
For learning purpose : Didactic "how it is working under the wood."
This class will work, as is, in most common scenario. 32Bits ARGB only.
-----------------------------------------------------------------------------}


unit GS.Pixel32;

interface

Uses Classes,
     SysUtils,
     Math,
     GS.Pixel;

Type
TP32 = Uint32;
pTP32 = ^TP32;
TP32Array = array of TP32;

TP32Vertex = record
  x,y,z : Int32;
end;

TP32Rec = packed record
case cardinal of
  0: (Blue: Byte; Green: Byte; Red: Byte; AlphaChannel: Byte);
  1: (Color: TP32);
  2: (Values: packed array[0..3] of Byte);
end;
pTP32Rec = ^TP32Rec;

const
  gspColorNone : TP32 = $00000000;
  gspWhite : TP32 = $FFFFFFFF;
  gspBlack : TP32 = $FF000000;
  gspBlue : TP32 = $FF0000FF;
  gspGreen : TP32 = $FF008000;
  gspRed : TP32 = $FFFF0000;
  gspOrange : TP32 = $FFFF7F00;
  gspAqua :  TP32= $FF00FFFF;
  gspNavy  : TP32 = $FF000080;
  clOlive : TP32 = $FF7F7F00;
  clYellow : TP32 = $FFFFFF00;

Type
TPixel32 = class;

TPixel32CustomShader = class(TPixel32InterfacedObject, iPixShader)
protected
  fsurface : iPixSurface;
  procedure init(surface : iPixSurface);
public
  constructor create(surface : iPixSurface); virtual;
  procedure process; virtual; abstract;
end;

TPixel32BasicColorShader = class(TPixel32CustomShader)
protected
public
  Bits : pTP32;
  Color : TP32;
  procedure process; override;
end;

TPixel32ColorShader = class(TPixel32BasicColorShader)
protected
  fx, fy, fz : Integer;
public
  procedure setXYZ(x,y,z : Int32); virtual;
end;

//Starting from here, the rasterizer will be different.
TCustomPixelChHeShader = class(TPixel32ColorShader)
public
  Texture : TPixel32;
  constructor create(surface : iPixSurface); override;
end;

TPixel32 = Class(TPixel32InterfacedObject, iPixSurface)
private
  function GetFillColor: TP32;
  procedure SetFillColor(const Value: TP32);
  function GetPenColor: TP32;
  procedure SetPenColor(const Value: TP32);
protected
  fsurface : TP32Array; //Memory.
  fwidth,fheight : uInt32;
  fpointMoveToX, fpointMoveToY : Int32;

  fFillShader : TPixel32BasicColorShader;
  fDrawShader : TPixel32ColorShader;
  FCurrentDrawShader : TPixel32ColorShader;

  procedure InternalRasterize(const a, b, c: TP32Vertex);

public
  constructor create(const width : uInt32 = 32; const Height : uInt32 = 32); virtual;
  destructor Destroy; override;

  procedure flipVertical;

  procedure copyTo(target : iPixSurface);
  procedure AlphaLayerReset(const value : byte = 255);



  function ColorGetRValue(c : TP32) : byte;
  function ColorGetBValue(c : TP32) : byte;
  function ColorGetGValue(c : TP32) : byte;
  function ColorGetAValue(c : TP32) : byte;
  function ColorSetAValue(c : TP32; AlphaValue : Byte) : TP32;

  //Shader
  procedure setDrawShader(shader : TPixel32ColorShader);
  procedure ResetDrawShader;

  //iPixSurface impl.
  function getSurfacePtr : pointer;
  function getSurfaceScanLinePtr(LineIndex : Int32) : pointer;
  procedure resize(width,height : Int32);
  function isEmpty : boolean;

  procedure pixel(const x,y : Int32;const z : Int32 = 0);
  procedure moveTo(const x,y : Int32;const z : Int32 = 0);
  procedure lineTo(const x,y : Int32;const z : Int32 = 0);

  procedure rasterize(x,y,x1,y1,x2,y2 : Int32);

  procedure clear;
  function width : uInt32;
  function height : uInt32;

  property color_fill : TP32 read GetFillColor write SetFillColor;
  property color_pen : TP32 read GetPenColor write SetPenColor;
End;

function P32Vertex(const x : integer = 0; const y : integer = 0; const z : integer = 0) : TP32Vertex;

implementation

Uses GS.Pixel32.Rasterize;


function P32Vertex(const x : integer; const y : integer; const z : integer) : TP32Vertex;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

{ TPixel32 }

procedure TPixel32.AlphaLayerReset(const value : byte = 255);
var i : integer;
    b : pTP32;
begin
  b := getSurfacePtr;
  for i := 0 to high(fsurface) do
  begin
    TP32Rec(b^).AlphaChannel := value;
    inc(b);
  end;
end;

procedure TPixel32.clear;
var
  i: Integer;
begin
  fFillShader.Bits := getSurfacePtr;
  for i := 0 to high(fsurface) do
    fFillShader.process;
end;

procedure TPixel32.copyTo(target: iPixSurface);
begin
  assert(assigned(target));
  target.resize(fwidth, fheight);
  move(getSurfacePtr^, target.getSurfacePtr^, Width * Height * SizeOf(TP32));
end;

constructor TPixel32.create(const width, Height: uInt32);
begin
  inherited create;

  fFillShader := TPixel32BasicColorShader.Create(self);
  fDrawShader := TPixel32ColorShader.Create(self);
  FCurrentDrawShader := fDrawShader;

  color_fill := gspWhite;
  color_pen := gspBlack;

  Resize(width,height);
  clear;
end;

destructor TPixel32.Destroy;
begin
  inherited Destroy;
  FreeAndNil(fFillShader);
  FreeAndNil(fDrawShader);
end;

function TPixel32.ColorGetRValue(c : TP32) : byte;
begin
  result := TP32Rec(c).Red;
end;

function TPixel32.ColorSetAValue(c : TP32; AlphaValue : Byte): TP32;
begin
  TP32Rec(c).AlphaChannel := AlphaValue;
  result := c;
end;

function TPixel32.ColorGetAValue(c: TP32): byte;
begin
  result := TP32Rec(c).AlphaChannel;
end;

function TPixel32.ColorGetBValue(c : TP32) : byte;
begin
  result := TP32Rec(c).Blue;
end;

function TPixel32.ColorGetGValue(c : TP32) : byte;
begin
  result := TP32Rec(c).Green;
end;


procedure TPixel32.InternalRasterize(const a, b, c: TP32Vertex);
var aa,bb,cc : TVector3;
var aai,bbi,cci : TVector3i;
begin
  aa := Vector3(a.x,a.y,a.z);
  bb := Vector3(b.x,b.y,b.z);
  cc := Vector3(c.x,c.y,c.z);
  aai := Vector3i(a.x,a.y,a.z);
  bbi := Vector3i(b.x,b.y,b.z);
  cci := Vector3i(c.x,c.y,c.z);


  if FCurrentDrawShader is TCustomPixelChHeShader then
  begin
    TexMap2(Self,nil,cci,bbi,aai,Point2i(0,0),Point2i(511,0),Point2i(511,511))
  end
  else
    TexMap(Self,FCurrentDrawShader,aa,bb,cc,Point2(0,0),Point2(1,0),Point2(1,1));
end;

procedure TPixel32.flipVertical;
var
  i: uInt32;
  a: TP32Array;
  row: pTP32;
begin
  if IsEmpty then
    Exit;
  SetLength(a, fWidth * fHeight);
  row := @fsurface[(fheight-1) * fwidth];
  for i := 0 to fHeight -1 do
  begin
    move(row^, a[i * fWidth], fWidth * SizeOf(TP32));
    dec(row, fWidth);
  end;
  fsurface := a; //copy.
end;

function TPixel32.GetFillColor: TP32;
begin
  result := fFillShader.Color;
end;

function TPixel32.GetPenColor: TP32;
begin
  result := fDrawShader.Color;
end;

function TPixel32.width: uInt32;
begin
  result := fwidth;
end;

function TPixel32.height: uInt32;
begin
  result := fheight;
end;

function TPixel32.isEmpty: boolean;
begin
  Result := (FWidth = 0) or (FHeight = 0) or (fsurface = nil);
end;

procedure TPixel32.lineTo(const x, y: int32; const z : Int32);
var
  i, deltax, deltay, numpixels,
  d, dinc1, dinc2, xx, xinc1, xinc2,
  yy, yinc1, yinc2: integer;
begin
  deltax := abs(x - fpointMovetox); // Calculate deltax and deltay for initialisation
  deltay := abs(y - fpointMovetoy);
  if deltax >= deltay then // Initialize all vars based on which is the independent variable
  begin
    numpixels := deltax + 1; // x is independent variable
    d := (2 * deltay) - deltax;
    dinc1 := deltay shl 1;
    dinc2 := (deltay - deltax) shl 1;
    xinc1 := 1;
    xinc2 := 1;
    yinc1 := 0;
    yinc2 := 1;
  end
  else
  begin
    numpixels := deltay + 1; // y is independent variable
    d := (2 * deltax) - deltay;
    dinc1 := deltax shl 1;
    dinc2 := (deltax - deltay) shl 1;
    xinc1 := 0;
    xinc2 := 1;
    yinc1 := 1;
    yinc2 := 1;
  end;
  if fpointMovetox > x then // Make sure x and y move in the right directions
  begin
    xinc1 := -xinc1;
    xinc2 := -xinc2;
  end;
  if fpointMovetoy > y then
  begin
    yinc1 := -yinc1;
    yinc2 := -yinc2;
  end;
  xx := fpointMovetox; // Start drawing at <x1, y1>
  yy := fpointMovetoy;
  for i := 1 to numpixels do // Draw the pixels
  begin
    //This "If is the equivalent of "PutPixel".
    if (xx<int32(fWidth)) and (xx>-1) and (yy<int32(fheight)) And (yy>-1) then
    begin
      pixel(xx,yy);
//      If Assigned(FDrawEvent) then
//        DrawEvent(X,Y, etLine);
    end;
    if d < 0 then
    begin
      d := d + dinc1;
      xx := xx + xinc1;
      yy := yy + yinc1;
    end
    else
    begin
      d := d + dinc2;
      xx := xx + xinc2;
      yy := yy + yinc2;
    end;
  end;

  //and move :)
  moveTo(x,y);
end;

procedure TPixel32.moveTo(const x, y: int32; const z : Int32);
begin
  fpointMovetox := x;
  fpointMovetoy := y;
end;

procedure TPixel32.pixel(const x, y: int32; const z: Int32);
var shader : TPixel32ColorShader;
begin
  FCurrentDrawShader.Bits := getSurfacePtr;
  FCurrentDrawShader.setXYZ(x,y,z);
  FCurrentDrawShader.process;
end;

procedure TPixel32.rasterize(x, y, x1, y1, x2, y2: Int32);
var a,b,c : TP32Vertex;
begin
  a := P32Vertex(x,y,0);
  b := P32Vertex(x1,y1,0);
  c := P32Vertex(x2,y2,0);

  InternalRasterize(a,b,c);
end;

procedure TPixel32.ResetDrawShader;
begin
  FCurrentDrawShader := fDrawShader;
end;

procedure TPixel32.resize(width, height: Int32);
begin
  SetLength(fsurface,width*Height);
  fheight := height;
  fwidth := width;
  clear; //artefact if not.
end;


procedure TPixel32.setDrawShader(shader: TPixel32ColorShader);
begin
  assert(Assigned(shader));
  FCurrentDrawShader := shader;
end;

procedure TPixel32.SetFillColor(const Value: TP32);
begin
  fFillShader.Color := Value;
end;

procedure TPixel32.SetPenColor(const Value: TP32);
begin
  fDrawShader.Color := Value;
end;

function TPixel32.getSurfacePtr: pointer;
begin
  result := @fSurface[0];
end;

function TPixel32.getSurfaceScanLinePtr(LineIndex: Int32): pointer;
var l : pTP32;
begin
  l := getSurfacePtr;
  inc(l,LineIndex*width);
  result := l;
end;

{ TPixel32CustomShader }

constructor TPixel32CustomShader.create(surface: iPixSurface);
begin
  inherited create;
  init(surface);
end;

procedure TPixel32CustomShader.init(surface: iPixSurface);
begin
  Assert(assigned(surface));
  fsurface := surface;
end;

{ TPixel32BasicColorshader }

procedure TPixel32BasicColorShader.process;
var sColor : TP32Rec;
begin
//  Bits^ := Color;
  sColor := TP32Rec(Color);
  pTP32Rec(Bits).red:=(sColor.AlphaChannel * (sColor.Red - pTP32Rec(Bits).Red) shr 8) + (pTP32Rec(Bits).Red);
  pTP32Rec(Bits).Blue:=(sColor.AlphaChannel * (sColor.Blue - pTP32Rec(Bits).Blue) shr 8) + (pTP32Rec(Bits).Blue);
  pTP32Rec(Bits).Green:=(sColor.AlphaChannel * (sColor.Green - pTP32Rec(Bits).Green) shr 8) + (pTP32Rec(Bits).Green);
  inc(Bits);
end;

{ TPixel32ColorShader }

procedure TPixel32ColorShader.setXYZ(x, y, z : Int32);
begin
  bits := fsurface.getSurfacePtr;
  inc(bits,y*fsurface.width+x);
  fx := x;
  fy := y;
  fz := z;
end;


{ TCustomPixelChHeShader }

constructor TCustomPixelChHeShader.create(surface: iPixSurface);
begin
  inherited;
  Texture := nil;
end;

end.
