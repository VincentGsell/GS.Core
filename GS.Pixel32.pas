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
 - Anthony Walter's Pixels.pas unit. (effect's architecture).
 - Angus Johnson's Image32 architecture (ideas - Such as "shader" pixel).
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
     GS.Geometry,
     GS.Pixel,
     GS.Pixel32.Types,
     GS.Pixel.Draw;


Type
TPixel32 = class;

TPixel32FragmentShader = class(TPixelInterfacedObject, iPixShader)
protected
  //All the above data are populates by raster process.
  //And usable in Process. Please see fragments below.
  _x,_y : Int32;
  _z : TVecType;
  _interpolationVerticeA,
  _interpolationVerticeB,
  _interpolationVerticeC : single;

  _surface : TPixel32;
  _targetPixelSurface : pTP32;

  _processedColor : TP32Rec;

  _VA,
  _VB,
  _VC : pTP32Vertice;
public
  procedure process; virtual;

  procedure resetColor;

  //In (set by rasterizer
  property x : Int32 read _x write _x;
  property y : Int32 read _y write _y;
  property zProc : TVecType read _z write _z;
  property interpolationVerticeA : single read _interpolationVerticeA write _interpolationVerticeA;
  property interpolationVerticeB : single read _interpolationVerticeB write _interpolationVerticeB;
  property interpolationVerticeC : single read _interpolationVerticeC write _interpolationVerticeC;
  property sourceSurface : TPixel32 read _surface write _surface;
  property sourceSurfaceBits : pTP32 read _targetPixelSurface write _targetPixelSurface;
  property VerticeA : pTP32Vertice read _VA write _VA;
  property VerticeB : pTP32Vertice read _VB write _VB;
  property VerticeC : pTP32Vertice read _VC write _VC;
  //Out. (result)
  property processedColor : TP32Rec read _processedColor;
end;

TPixel32FragmentShaderFlatColor = Class(TPixel32FragmentShader)
protected
  fColor: TP32;
public
  procedure process; override;
  property Color : TP32 read fColor write fColor;
End;

TCustomPixel32SurfaceEffect = class(TPixelInterfacedObject, iPixSurfaceEffect)
protected
  fsurface : TPixel32;
public
  procedure init(surface : iPixSurface); virtual;
  procedure process; virtual; abstract;
end;

TPixel32SurfaceEffectClear = class(TCustomPixel32SurfaceEffect)
protected
  FColor: TP32;
public
  procedure process; override;
  property Color : TP32 read FColor write FColor;
end;

TPixel32 = Class(TPixelInterfacedObject, iPixSurface)
private
  function GetFillColor: TP32;
  procedure SetFillColor(const Value: TP32);
  function GetPenColor: TP32;
  procedure SetPenColor(const Value: TP32);
protected
  frasterMode: TSoftwareRasterizeOption;
  ffragment: TPixel32FragmentShader;

  fsurface : TP32Array; //Memory.
  fwidth,fheight : uInt32;
  fpointMoveToX, fpointMoveToY : Int32;

  FDefaultColorFill : TPixel32FragmentShaderFlatColor;
  FDefaultColorDraw : TPixel32FragmentShaderFlatColor;

  FCurrentDrawShader : TPixel32FragmentShader; //Pointer.

  FSurfaceClear : TPixel32SurfaceEffectClear;

public
  rasterV : TP32triVertices;


  constructor create(const width : uInt32 = 32; const Height : uInt32 = 32); virtual;
  destructor Destroy; override;

  procedure flipVertical;
  procedure line(x1,y1,x2,y2 : Int32);

  procedure copyTo(target : iPixSurface);
  procedure alphaLayerReset(const value : byte = 255);
  procedure alphaLayerResetByColor(ColorMask : TP32; const value : byte = 255; const Tolerance : byte = 5);

  function colorGetRValue(c : TP32) : byte;
  function colorGetBValue(c : TP32) : byte;
  function colorGetGValue(c : TP32) : byte;
  function colorGetAValue(c : TP32) : byte;
  function colorSetAValue(c : TP32; AlphaValue : Byte) : TP32;
  function colorP32Rec(r,g,b,a : byte) : TP32rec;

  procedure resetFragmentShader;

  //Note size must be set before.
  procedure LoadFromARGB32FormatStream(stream : TStream);
  procedure SaveToARGB32FormatStream(stream : TStream);

  //Shader
  procedure setFragmentShader(shader : iPixShader);

  //iPixSurface impl.
  function getSurfacePtr : pointer;
  function getSurfaceScanLinePtr(LineIndex : Int32) : pointer;
  procedure resize(width,height : Int32);
  function isEmpty : boolean;

  procedure pixel(const x,y : Int32);
  procedure moveTo(const x,y : Int32);
  procedure lineTo(const x,y : Int32);
  procedure rectangle(const x,y,xx,yy : Int32);

  procedure setVertice(indice : uInt32; x,y : single); //load vertex data.
//  procedure setVerticeXYZ(indice: uint32; x,y,z : single);
  procedure setVerticeUV(indice : uint32; u,v : single);
  procedure setVerticeColor(indice: uint32; r,g,b,a : single);
  procedure rasterize;
  procedure beginDraw;
  procedure endDraw;

  procedure draw(objToRender : iPixDrawable);

  procedure clear; overload;

  //Get draw surface as vertices collection. Usefull to draw on entire screen a shader result (see fill);
  function getSurfaceVertices : TP32QuadVertices;
  //Draw a fragment shader on the entire surface.
  procedure fill;
  function width : uInt32;
  function height : uInt32;

  property color_fill : TP32 read GetFillColor write SetFillColor;
  property color_pen : TP32 read GetPenColor write SetPenColor;
  property currentFragment : TPixel32FragmentShader read FCurrentDrawShader;

  ///directMode : if true, will never call fragmentshader, but call GS.Pixel32.DirectRasterizer methods in replacement
  ///  Note that, only basic shader is replaced. Debug and evaluation purpose.
  property rasterMode : TSoftwareRasterizeOption read frasterMode write frasterMode;
End;

function P32Vertex( const x : single = 0;
                    const y : single = 0;
                    u : single =0;
                    v : single =0;
                    r : single = 1;
                    g : single = 1;
                    b : single = 1;
                    a : single = 1) : TP32Vertice;


Var
  Pixel32Palette : array of TP32;

implementation

Uses GS.Pixel32.Rasterize,GS.Pixel32.DirectRasterizer;


function P32Vertex( const x : single = 0;
                    const y : single = 0;
                    u : single =0;
                    v : single =0;
                    r : single = 1;
                    g : single = 1;
                    b : single = 1;
                    a : single = 1) : TP32Vertice;
begin
  result.x := x;
  result.y := y;
  result.u := u;
  result.v := v;
  result.rgba.r := r;
  result.rgba.g := g;
  result.rgba.b := b;
  result.rgba.a := a;
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

procedure TPixel32.beginDraw;
begin
  rasterBackBufferInit(self);
end;

procedure TPixel32.clear;
begin
  fsurfaceClear.init(Self);
  FSurfaceClear.Color := FDefaultColorFill.Color;
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
  SetLength(rasterV,3);
  frasterMode := TSoftwareRasterizeOption.roBackBuffer;
  FDefaultColorFill := TPixel32FragmentShaderFlatColor.Create;
  FDefaultColorDraw := TPixel32FragmentShaderFlatColor.Create;

  FCurrentDrawShader := FDefaultColorDraw;

  FSurfaceClear := TPixel32SurfaceEffectClear.Create;
  FSurfaceClear.init(self);

  ffragment := nil;

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

procedure TPixel32.fill;
var i,j : integer;
    l : TP32QuadVertices;
begin
//  Assert(Assigned(shader));
  l := getSurfaceVertices;
//  setFragmentShader(shader);
  beginDraw;
  rasterV[0] := l[0];
  rasterV[1] := l[1];
  rasterV[2] := l[2];
  rasterize;
  rasterV[0] := l[2];
  rasterV[1] := l[3];
  rasterV[2] := l[0];
  rasterize;
  endDraw;
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


procedure TPixel32.line(x1, y1, x2, y2: Int32);
var
  i, deltax, deltay, numpixels,
  d, dinc1, dinc2, xx, xinc1, xinc2,
  yy, yinc1, yinc2: integer;
begin
  deltax := abs(x2 - x1); // Calculate deltax and deltay for initialisation
  deltay := abs(y2 - y1);
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
  if x2 > x1 then // Make sure x and y move in the right directions
  begin
    xinc1 := -xinc1;
    xinc2 := -xinc2;
  end;
  if y2 > y1 then
  begin
    yinc1 := -yinc1;
    yinc2 := -yinc2;
  end;
  xx := x2; // Start drawing at <x1, y1>
  yy := y2;
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
  moveTo(x2,y2);
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
  result := FDefaultColorFill.Color;
end;

function TPixel32.GetPenColor: TP32;
begin
  result := FDefaultColorDraw.Color;
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

procedure TPixel32.lineTo(const x, y: int32);
var
  i, deltax, deltay, numpixels,
  d, dinc1, dinc2, xx, xinc1, xinc2,
  yy, yinc1, yinc2: integer;
begin
  line(fpointMoveToX,fpointMoveToY,x,y);
  moveTo(x,y);
end;

procedure TPixel32.LoadFromARGB32FormatStream(stream: TStream);
begin
  stream.ReadBuffer(fsurface[0],Length(fsurface)*SizeOf(TP32));
end;

procedure TPixel32.SaveToARGB32FormatStream(stream: TStream);
var l : uint32;
begin
  stream.WriteBuffer(fsurface[0],Length(fsurface)*SizeOf(TP32));
end;

procedure TPixel32.moveTo(const x, y: int32);
begin
  fpointMovetox := x;
  fpointMovetoy := y;
end;

procedure TPixel32.pixel(const x, y: int32);
var a : TP32Vertice;
    p : pTP32;
begin
  if frasterMode = TSoftwareRasterizeOption.roDirectMode then
  begin
    TPixel32DirectMode.FlatColor1_drawPixel(self,uInt32(x),uInt32(y),FDefaultColorDraw.Color);
  end
  else
  begin
    FCurrentDrawShader.x := x;
    FCurrentDrawShader.y := x;
    FCurrentDrawShader.interpolationVerticeA := 1;
    FCurrentDrawShader.interpolationVerticeB := 1;
    FCurrentDrawShader.interpolationVerticeC := 1;
    FCurrentDrawShader.sourceSurface := self;
    a := P32Vertex(x,y);
    FCurrentDrawShader.VerticeA := @a;
    FCurrentDrawShader.VerticeB := @a;
    FCurrentDrawShader.VerticeC := @a;
    p := self.getSurfaceScanLinePtr(y);
    inc(p,x);
    FCurrentDrawShader.sourceSurfaceBits := p;
    FCurrentDrawShader.process;
    p^:= FCurrentDrawShader.processedColor.Color;
  end;
end;

procedure TPixel32.rasterize;
begin
  triangleRasterize(Self,rasterV, frasterMode);
end;

procedure TPixel32.draw(objToRender: iPixDrawable);
begin
  TPixelDrawTools.Draw(Self,objToRender);
end;

procedure TPixel32.endDraw;
begin
  //-)
end;

procedure TPixel32.resize(width, height: Int32);
begin
  SetLength(fsurface,width*Height);
  fheight := height;
  fwidth := width;
  clear; //artefact if not.
end;

procedure TPixel32.rectangle(const x, y, xx, yy: Int32);
begin
  line(x,y,xx,y);
  line(x,y,x,yy);
  line(x,yy,xx,yy);
  line(xx,y,xx,yy);
end;

procedure TPixel32.ResetFragmentShader;
begin
  FCurrentDrawShader := FDefaultColorDraw;
end;

procedure TPixel32.setFragmentShader(shader: iPixShader);
begin
  assert(Assigned(shader));
  assert(shader is TPixel32FragmentShader);
//  shader.init(Self);
  FCurrentDrawShader := (shader as TPixel32FragmentShader);
end;

procedure TPixel32.SetFillColor(const Value: TP32);
begin
  FDefaultColorFill.Color := Value;
end;

procedure TPixel32.SetPenColor(const Value: TP32);
begin
  FDefaultColorDraw.Color := Value;
end;

procedure TPixel32.setVertice(indice: uInt32; x, y : single);
begin
  rasterV[indice].x := x;
  rasterV[indice].y := y;
//  rasterV[indice].z := 0;
end;

procedure TPixel32.setVerticeColor(indice: uint32; r, g, b, a: single);
begin
  rasterV[indice].rgba.r := r;
  rasterV[indice].rgba.g := g;
  rasterV[indice].rgba.b := b;
  rasterV[indice].rgba.a := a;
end;

procedure TPixel32.setVerticeUV(indice: Uint32; u, v: single);
begin
  rasterV[indice].u := u;
  rasterV[indice].v := v;
end;

{
procedure TPixel32.setVerticeXYZ(indice: uint32; x, y, z: single);
begin
  rasterV[indice].x := x;
  rasterV[indice].y := y;
//  rasterV[indice].z := z;
end;
}

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


function TPixel32.getSurfaceVertices: TP32QuadVertices;
begin
  result[0] := P32Vertex(0,0,0,0,1,0,0,1);
  result[1] := P32Vertex(width-1,0,1,0,0,1,0,1);
  result[2] := P32Vertex(width-1,height-1,1,1,0,0,1,1);
  result[3] := P32Vertex(0,height-1,0,1,1,1,1,1);
end;

{ TCustomPixel32SurfaceEffect }

procedure TCustomPixel32SurfaceEffect.init(surface: iPixSurface);
begin
  assert(assigned(surface));
  assert(surface is TPixel32);
  {$ifdef fpc}
  fsurface := surface as TPixel32;
  {$else}
  fsurface := TPixel32(surface);
  {$endif}
end;

{ TP32SurfaceEffectClear }

procedure TPixel32SurfaceEffectClear.process;
var bits : pTP32;
    i : NativeInt;
    c : TP32;

    bound : nativeInt;
begin
  bits := fSurface.getSurfacePtr;
  c := Color;
  bound := fsurface.width*fsurface.height;
  for i := 0 to bound-1 do
  begin
    bits^:= c;
    inc(bits);
  end;
end;

{ TPixel32FragmentShader }

procedure TPixel32FragmentShader.process;
begin
  resetColor;
end;

procedure TPixel32FragmentShader.resetColor;
begin
  _processedColor.Color := gspRed;
end;

{ TPixel32FragmentShaderFlatColor }

procedure TPixel32FragmentShaderFlatColor.process;
var sSurfaceColor : TP32Rec; //Current surface's target color.
    fcolorAsRec : TP32Rec;
begin
  fcolorAsRec.Color := fColor;
  sSurfaceColor.Color := sourceSurfaceBits^;
  _processedColor.red:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Red - sSurfaceColor.Red) shr 8) + (sSurfaceColor.Red);
  _processedColor.Blue:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Blue - sSurfaceColor.Blue) shr 8) + (sSurfaceColor.Blue);
  _processedColor.Green:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Green - sSurfaceColor.Green) shr 8) + (sSurfaceColor.Green);
end;


Initialization

  SetLength(Pixel32Palette,10);
  Pixel32Palette[0] := gspWhite;
  Pixel32Palette[1] := gspBlack;
  Pixel32Palette[2] := gspBlue;
  Pixel32Palette[3] := gspGreen;
  Pixel32Palette[4] := gspRed;
  Pixel32Palette[5] := gspOrange;
  Pixel32Palette[6] := gspAqua;
  Pixel32Palette[7] := gspNavy;
  Pixel32Palette[8] := gspOlive;
  Pixel32Palette[9] := gspYellow;

finalization


end.
