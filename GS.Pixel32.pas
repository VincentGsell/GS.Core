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
{$I GSCore.Inc}

unit GS.Pixel32;



interface

Uses Classes,
     SysUtils,
     Math,
     GS.Pixel;

Type
TP32 = int32;
pTP32 = ^TP32;
TP32Array = array of TP32;

TP32Vertex = record
  x,y,z : Int32;
  u,v : Int32;
end;

TP32Rec = packed record
case int32 of
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
  gspOlive : TP32 = $FF7F7F00;
  gspYellow : TP32 = $FFFFFF00;


Type
TPixel32 = class;


TPixel32CustomShader = class(TPixel32InterfacedObject, iPixShader)
protected
  fsurface : iPixSurface;
  fBits : pTP32;
  fColorData : TP32Rec;
  x,y,z : Int32;
public
  procedure init(surface : iPixSurface); virtual;
  procedure setData(_x,_y : Int32; const _z : Int32=0); virtual;
  procedure process; virtual; abstract;

  property ColorData : TP32Rec read FColorData write FColorData;
end;

TPixel32ColorShader = class(TPixel32CustomShader)
protected
public
  procedure process; override;
end;

TPixel32TextureShader = class(TPixel32ColorShader)
protected
public
  Texture : TPixel32;
end;


TCustomPixel32SurfaceEffect = class(TPixel32InterfacedObject, iPixSurfaceEffect)
protected
  fsurface : TPixel32;
  fshader : TPixel32CustomShader;
public
  procedure init(surface : iPixSurface; shader : iPixShader); virtual;
  procedure process; virtual; abstract;
end;

TPixel32SurfaceEffectClear = class(TCustomPixel32SurfaceEffect)
private
public
  procedure process; override;
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

  FDefaultColorFill : TPixel32ColorShader;
  FDefaultColorDraw : TPixel32ColorShader;

  FCurrentDrawShader : TPixel32ColorShader; //Pointer.

  FSurfaceClear : TPixel32SurfaceEffectClear;

  procedure InternalRasterize(const a, b, c: TP32Vertex);

public
  rasterV : Array[0..3] of TP32Vertex;


  constructor create(const width : uInt32 = 32; const Height : uInt32 = 32); virtual;
  destructor Destroy; override;

  procedure flipVertical;

  procedure copyTo(target : iPixSurface);
  procedure alphaLayerReset(const value : byte = 255);
  procedure alphaLayerResetByColor(ColorMask : TP32; const value : byte = 255; const Tolerance : byte = 5);

  function colorGetRValue(c : TP32) : byte;
  function colorGetBValue(c : TP32) : byte;
  function colorGetGValue(c : TP32) : byte;
  function colorGetAValue(c : TP32) : byte;
  function colorSetAValue(c : TP32; AlphaValue : Byte) : TP32;
  function colorP32Rec(r,g,b,a : byte) : TP32rec;


  //Shader
  procedure setDrawShader(shader : TPixel32ColorShader);
  procedure resetDrawShader;

  //iPixSurface impl.
  function getSurfacePtr : pointer;
  function getSurfaceScanLinePtr(LineIndex : Int32) : pointer;
  procedure resize(width,height : Int32);
  function isEmpty : boolean;

  procedure pixel(const x,y : Int32;const z : Int32 = 0);
  procedure moveTo(const x,y : Int32;const z : Int32 = 0);
  procedure lineTo(const x,y : Int32;const z : Int32 = 0);

  procedure setVertex(indice : uInt32; x,y,z,u,v : integer);
  procedure rasterize;

  procedure clear; overload;
  procedure fill(shader : TPixel32ColorShader); overload;
  function width : uInt32;
  function height : uInt32;

  property color_fill : TP32 read GetFillColor write SetFillColor;
  property color_pen : TP32 read GetPenColor write SetPenColor;
  property currentDrawShader : TPixel32ColorShader read FCurrentDrawShader;
End;

function P32Vertex(const x : integer = 0; const y : integer = 0; const z : integer = 0; u : integer =0; v : integer =0) : TP32Vertex;


implementation

Uses GS.Pixel32.Rasterize;


function P32Vertex(const x : integer; const y : integer; const z : integer; u : integer; v : integer) : TP32Vertex;
begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.u := u;
  result.v := v;
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

procedure TPixel32.alphaLayerResetByColor(ColorMask: TP32; const value: byte; const Tolerance : byte);
var i : integer;
    b : pTP32;
begin
  b := getSurfacePtr;
  for i := 0 to high(fsurface) do
  begin
    if ((Abs(TP32Rec(b^).Red - TP32Rec(ColorMask).Red)<Tolerance)) And
       ((Abs(TP32Rec(b^).Green - TP32Rec(ColorMask).Green)<Tolerance)) And
       ((Abs(TP32Rec(b^).Blue - TP32Rec(ColorMask).Blue)<Tolerance))  then
      TP32Rec(b^).AlphaChannel := value;
    inc(b);
  end;
end;

procedure TPixel32.clear;
begin
  fsurfaceClear.init(Self,FDefaultColorFill);
  FSurfaceClear.process;
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
  FDefaultColorFill := TPixel32ColorShader.Create;
  FDefaultColorDraw := TPixel32ColorShader.Create;

  FCurrentDrawShader := FDefaultColorDraw;
  FCurrentDrawShader.init(self);

  FSurfaceClear := TPixel32SurfaceEffectClear.Create;
  FSurfaceClear.init(self,FDefaultColorFill);

  color_fill := gspWhite;
  color_pen := gspBlack;

  Resize(width,height);
  clear;
end;

destructor TPixel32.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDefaultColorFill);
  FreeAndNil(FDefaultColorDraw);
  FreeAndNil(FSurfaceClear);
end;

function TPixel32.ColorGetRValue(c : TP32) : byte;
begin
  result := TP32Rec(c).Red;
end;

function TPixel32.colorP32Rec(r, g, b, a: byte): TP32rec;
begin
  result.Red := r;
  result.Green := g;
  result.Blue := b;
  result.AlphaChannel := a;
end;

function TPixel32.ColorSetAValue(c : TP32; AlphaValue : Byte): TP32;
begin
  TP32Rec(c).AlphaChannel := AlphaValue;
  result := c;
end;

procedure TPixel32.fill(shader: TPixel32ColorShader);
var i,j : integer;
begin
  Assert(Assigned(shader));
  for i:= 0 to fwidth-1 do
    for j:= 0 to fheight-1 do
      pixel(i,j);
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
var  w,h : integer;
    aa,bb,cc : TP32Vertex;
begin

  if FCurrentDrawShader is TPixel32TextureShader then
  begin
    w := TPixel32TextureShader(FCurrentDrawShader).Texture.width;
    h := TPixel32TextureShader(FCurrentDrawShader).Texture.height;
    aa := a;
    bb := b;
    cc := c;

    aa.u := aa.u * w;
    aa.v := aa.v * h;
    bb.u := bb.u * w;
    bb.v := bb.v * h;
    cc.u := cc.u * w;
    cc.v := cc.v * h;
    triangleRasterizeTexMap( Self,
                             aa,bb,cc)
  end
  else
    triangleRasterizeFlat(Self,a, b, c)
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
  result := FDefaultColorFill.ColorData.Color;
end;

function TPixel32.GetPenColor: TP32;
begin
  result := FDefaultColorDraw.ColorData.Color;
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
//    If Assigned(FDrawEvent) then
//      DrawEvent(X,Y, etLine);
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
begin
  FCurrentDrawShader.setData(x,y,z);
  FCurrentDrawShader.process;
end;

procedure TPixel32.rasterize;
begin
  InternalRasterize(rasterV[0],rasterV[1],rasterV[2]);
end;

procedure TPixel32.ResetDrawShader;
begin
  FCurrentDrawShader := FDefaultColorDraw;
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
  shader.init(Self);
  FCurrentDrawShader := shader;
end;

procedure TPixel32.SetFillColor(const Value: TP32);
begin
  FDefaultColorFill.ColorData := TP32Rec(Value);
end;

procedure TPixel32.SetPenColor(const Value: TP32);
begin
  FDefaultColorDraw.ColorData := TP32Rec(Value);
end;

procedure TPixel32.setVertex(indice: uInt32; x, y, z, u, v: integer);
begin
  rasterV[indice].x := x;
  rasterV[indice].y := y;
  rasterV[indice].z := z;
  rasterV[indice].u := u;
  rasterV[indice].v := v;
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

procedure TPixel32CustomShader.init(surface : iPixSurface);
begin
  Assert(assigned(surface));
  fsurface := surface;
end;


procedure TPixel32CustomShader.setData(_x, _y: Int32; const _z: Int32);
begin
  Assert(assigned(fsurface));
  x := _x;
  y := _y;
  z := _z;
  fbits := fsurface.getSurfacePtr;
  inc(fbits,y*fsurface.width+x);
end;

{ TPixel32ColorShader }
procedure TPixel32ColorShader.process;
var sSurfaceColor : TP32Rec; //Current surface's target color.
    fcol : TP32Rec;
begin
  fcol := fColorData; //Original color.
  sSurfaceColor.Color := fBits^;
  fcol.red:=(fColorData.AlphaChannel * (fColorData.Red - sSurfaceColor.Red) shr 8) + (sSurfaceColor.Red);
  fcol.Blue:=(fColorData.AlphaChannel * (fColorData.Blue - sSurfaceColor.Blue) shr 8) + (sSurfaceColor.Blue);
  fcol.Green:=(fColorData.AlphaChannel * (fColorData.Green - sSurfaceColor.Green) shr 8) + (sSurfaceColor.Green);
  fbits^ := fcol.Color;
end;



{ TCustomPixel32SurfaceEffect }

procedure TCustomPixel32SurfaceEffect.init(surface: iPixSurface; shader : iPixShader);
begin
  assert(assigned(surface));
  assert(assigned(shader));
  assert(surface is TPixel32);
  {$ifdef fpc}
  fsurface := surface as TPixel32;
  fshader := shader as TPixel32CustomShader;
  {$else}
  fsurface := TPixel32(surface);
  fshader := TPixel32CustomShader(shader);
  {$endif}
end;

{ TP32SurfaceEffectClear }

procedure TPixel32SurfaceEffectClear.process;
var bits : pTP32;
    i : integer;
    c : TP32;
begin
  bits := fSurface.getSurfacePtr;
  c := TPixel32CustomShader(fshader).ColorData.Color;
  FillChar(bits^,(fsurface.width*fsurface.height)*SizeOf(TP32),c);
end;

end.
