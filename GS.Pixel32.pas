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

//Gradiant processing.
  tagRGBQUAD = record
    rgbBlue: Byte;
    rgbGreen: Byte;
    rgbRed: Byte;
    rgbReserved: Byte;
  end;
  TRGBQuad = tagRGBQUAD;
  PRGBQuad = ^TRGBQuad;

  PRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = array[0..1024] of TRGBQuad;

  TGradientColors = array[0..255] of TRGBQuad;

  TGradientShift = -100..100;
  TGradientRotation = -100..100;

const
  gspColorNone : TP32 = $00000000;
  gspWhite : TP32 = $FFFFFFFF;
  gspBlack : TP32 = $FF000000;
  gspBlue : TP32 = $FF0000FF;
  gspGreen : TP32 = $00008000;
  gspRed : TP32 = $FFFF0000;
  gspOrange : TP32 = $FFFF7F00;
  gspAqua :  TP32= $FF00FFFF;
  gspNavy  : TP32 = $FF000080;
  clOlive : TP32 = $FF7F7F00;
  clYellow : TP32 = $FFFFFF00;
Type

TPixel32CustomShader = class(TPixel32InterfacedObject, iPixShader)
protected
  fsurface : iPixSurface;
public
  procedure init(surface : iPixSurface);
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


  procedure RadialRect;
  procedure RadialCentral;
  procedure LinearHorizontal;
  procedure UpdatePattern(colorBegin, colorend : TP32; shift : TGradientShift; rotation : TGradientRotation; out Colors: TGradientColors);
  function GetRValue(c : TP32) : byte;
  function GetBValue(c : TP32) : byte;
  function GetGValue(c : TP32) : byte;

  //iPixSurface impl.
  function getSurfacePtr : pointer;
  function getSurfaceScanLinePtr(LineIndex : Int32) : pointer;
  procedure resize(width,height : Int32);
  function isEmpty : boolean;

  procedure pixel(const x,y : Int32;const z : Int32 = 0);
  procedure moveTo(const x,y : Int32;const z : Int32 = 0);
  procedure lineTo(const x,y : Int32;const z : Int32 = 0);

  procedure rasterize(x,y,x1,y1,x2,y2 : Int32);
  procedure setDrawShader(shader : TPixel32ColorShader);
  procedure ResetDrawShader;



  procedure clear;
  function width : uInt32;
  function height : uInt32;

  property color_fill : TP32 read GetFillColor write SetFillColor;
  property color_pen : TP32 read GetPenColor write SetPenColor;
End;

function P32Vertex(const x : integer = 0; const y : integer = 0; const z : integer = 0) : TP32Vertex;

implementation

Uses GS.Pixel32.TexMapChHe;

function P32Vertex(const x : integer; const y : integer; const z : integer) : TP32Vertex;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

{ TPixel32 }

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
  fFillShader := TPixel32BasicColorShader.Create;
  fDrawShader := TPixel32ColorShader.Create;
  fFillShader.init(Self);
  fDrawShader.init(Self);
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

  function TPixel32.GetRValue(c : TP32) : byte;
  begin
    result := TP32Rec(c).Red;
  end;
  function TPixel32.GetBValue(c : TP32) : byte;
  begin
    result := TP32Rec(c).Blue;
  end;
  function TPixel32.GetGValue(c : TP32) : byte;
  begin
    result := TP32Rec(c).Green;
  end;

procedure TPixel32.UpdatePattern(colorBegin, colorend : TP32; shift : TGradientShift; rotation : TGradientRotation; out Colors: TGradientColors);
var
  dRed, dGreen, dBlue: Integer;
  RGBColor1, RGBColor2: TP32;
  RGB1, RGB2: TRGBQuad;
  Index, rIndex: Integer;
  M, rM: Integer;


  function mul_div(a,b,c : integer) : integer;
  begin
    result := round(a*b/c);
  end;
begin
//  if Reverse then
//  begin
//    RGBColor1 := ColorToRGB(ColorEnd);
//    RGBColor2 := ColorToRGB(ColorBegin);
//  end
//  else
  begin
    RGBColor1 := ColorBegin;
    RGBColor2 := ColorEnd;
  end;

  RGB1.rgbRed := GetRValue(RGBColor1);
  RGB1.rgbGreen := GetGValue(RGBColor1);
  RGB1.rgbBlue := GetBValue(RGBColor1);
  RGB1.rgbReserved := 0;

  RGB2.rgbRed := GetRValue(RGBColor2);
  RGB2.rgbGreen := GetGValue(RGBColor2);
  RGB2.rgbBlue := GetBValue(RGBColor2);
  RGB2.rgbReserved := 0;

//  if Shift > 0 then
//  begin
//    RGB1.rgbRed := Byte(RGB1.rgbRed + mul_div(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
//    RGB1.rgbGreen := Byte(RGB1.rgbGreen + mul_div(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
//    RGB1.rgbBlue := Byte(RGB1.rgbBlue + mul_div(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
//  end
//  else if Shift < 0 then
  begin
    RGB2.rgbRed := Byte(RGB2.rgbRed + mul_div(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    RGB2.rgbGreen := Byte(RGB2.rgbGreen + mul_div(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    RGB2.rgbBlue := Byte(RGB2.rgbBlue + mul_div(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
  end;

  dRed := RGB2.rgbRed - RGB1.rgbRed;
  dGreen := RGB2.rgbGreen - RGB1.rgbGreen;
  dBlue := RGB2.rgbBlue - RGB1.rgbBlue;

  M := mul_div(255, Rotation, 100);
  if M = 0 then
    for Index := 0 to 255 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div 255;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div 255;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div 255;
      end
  else if M > 0 then
  begin
    M := 255 - M;
    for Index := 0 to M - 1 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div M;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div M;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div M;
      end;
    for Index := M to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB1.rgbRed + ((rIndex) * dRed) div (rM);
        rgbGreen := RGB1.rgbGreen + ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB1.rgbBlue + ((rIndex) * dBlue) div (rM);
      end;
  end
  else if M < 0 then
  begin
    M := -M;
    for Index := 0 to M do
      with Colors[Index] do
      begin
        rgbRed := RGB2.rgbRed - (Index * dRed) div M;
        rgbGreen := RGB2.rgbGreen - (Index * dGreen) div M;
        rgbBlue := RGB2.rgbBlue - (Index * dBlue) div M;
      end;
    for Index := M + 1 to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB2.rgbRed - ((rIndex) * dRed) div (rM);
        rgbGreen := RGB2.rgbGreen - ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB2.rgbBlue - ((rIndex) * dBlue) div (rM);
      end;
  end;
end;

procedure TPixel32.RadialRect;
var
  X, Y: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  Colors: TGradientColors;
begin
  UpdatePattern(gspGreen,gspBlue,Random(100)-100,100,Colors);
  for Y := 0 to 255 do
  begin

    // Top & Bottom
    Row1 := PRGBQuadArray(getSurfaceScanLinePtr(Y));
    Row2 := PRGBQuadArray(getSurfaceScanLinePtr(511-Y));

    pRGB := @Colors[y];
    for x:=Y to 511-y do
    begin
      Row1[X] := pRGB^;
      Row2[X] := pRGB^;
    end;

    for x:=0 to y do
    begin
      pRGB := @Colors[x];

      Row1[X] := pRGB^;     // Left
      Row2[X] := pRGB^;

      Row1[511-X] := pRGB^; // Right
      Row2[511-X] := pRGB^;
     end
  end;

end;

procedure TPixel32.RadialCentral;
var
  X, Y, rX: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  PreCalcXs: array[0..180] of Integer;
  Colors: TGradientColors;
begin
  UpdatePattern(gspGreen,gspBlue,Random(100)-100,100,Colors);
//  Pattern.Width := 362;
//  Pattern.Height := 362;

  rX := 0;
  for X := 180 downto 0 do
  begin
    PreCalcXs[rX] := X * X;
    Inc(rX);
  end;

  for Y := 180 downto 0 do
  begin
    Row1 := PRGBQuadArray(getSurfaceScanLinePtr(Y));
    Row2 := PRGBQuadArray(getSurfaceScanLinePtr(361-Y));
    for X := 180 downto 0 do
    begin
      rX := 361 - X;
      pRGB := @Colors[Round(Sqrt(PreCalcXs[X] + PreCalcXs[Y]))];
      Row1[X] := pRGB^;
      Row1[rX] := pRGB^;
      Row2[X] := pRGB^;
      Row2[rX] := pRGB^;
    end;
  end;
end;

procedure TPixel32.LinearHorizontal;
var
  X,Y: Integer;
  Row: PRGBQuadArray;
  Colors: TGradientColors;
begin
  UpdatePattern(gspGreen,gspBlue,Random(100)-100,100,Colors);
//  Pattern.Width := 256;
//  Pattern.Height := 1;
  for Y := 0 to 255 do
  begin
    Row := PRGBQuadArray(getSurfaceScanLinePtr(Y));

    for X := 0 to 255 do
      Row[X] := Colors[X];
  end;
end;

procedure TPixel32.InternalRasterize(const a, b, c: TP32Vertex);
{             1
    0....:....0....:
         a                Lines (x1,x2)
         #                 ( 5,  5)
        #.#                ( 4,  6)
       #...#               ( 3,  7)
      #.....#              ( 2,  8)
     #.......#             ( 1,  9)
  c ###.......#            ( 0, 10)
       ###.....#           ( 3, 11)
          ###...#          ( 6, 12)
             ###.#         ( 9, 13)
                ###        (12, 14)
                    b
}
type
  TFlatLine = record
    x1, x2: Integer;
    z1, z2: Integer;
  end;
var
  Top, Bottom, Count: Integer;
  Lines: array of TFlatLine;
  Index: Integer;

  procedure delta(v: Integer; var delta, incr: Integer);
  begin
    if v < 0 then
    begin
      delta := -v;
      incr := -1;
    end else begin
      delta := v;
      incr := +1;
    end;
  end;

  procedure error(var ex, dx, x, ix: Integer; ee: Integer);
  begin
    if ex > ee then
    begin
      dec(ex, ee);
      inc(x, ix);
    end;
    inc(ex, dx);
  end;

  procedure Scan(const a, b: TP32Vertex);
  var
    dx, dy, dz: Integer;
    ix, iy, iz: Integer;
    ex, ey, ez: Integer;
    x,  y,  z : Integer;
    ee, ii : Integer;
  begin
    delta(Trunc(b.x) - Trunc(a.x), dx, ix);
    delta(Trunc(b.y) - Trunc(a.y), dy, iy);
    delta(Trunc(b.z) - Trunc(a.z), dz, iz);
    ex := dx;
    ey := dy;
    ez := dz;
    ee := Max(ex, Max(ey, ez));
    x := Trunc(a.x);
    y := Trunc(a.y) - Top;
    z := Trunc(a.z);
    for ii := 0 to ee do
    begin
      if x < Lines[y].x1 then
      begin
        Lines[y].x1 := x;
        Lines[y].z1 := z;
      end;
      if x > Lines[y].x2 then
      begin
        Lines[y].x2 := x;
        Lines[y].z2 := z;
      end;
      error(ex, dx, x, ix, ee);
      error(ey, dy, y, iy, ee);
      error(ez, dz, z, iz, ee);
    end;
  end;

  procedure drawLine(y: Integer; Line: TFlatLine);
  var
    dx, dz: Integer;
    ix, iz: Integer;
    ex, ez: Integer;
    x,  z: Integer;
    ee, ii : Integer;
  begin
    if y<0 then
      Exit;
    if y>(fheight-1) then
      exit;

    delta(Line.x2 - Line.x1, dx, ix);
    if ix * dx < 0 then
      Exit;
    delta(Line.z2 - Line.z1, dz, iz);
    ex := dx;
    ez := dz;
    ee := Max(ex, ez);
    x := Line.x1;
    z := Line.z1;
    for ii := 0 to ee + 1 do
    begin
      if (x>-1) and (x<fwidth) then
        pixel(x, y, z);
      error(ex, dx, x, ix, ee);
      error(dz, dz, z, iz, ee);
    end;
  end;

begin
  Top := Trunc(Min(a.y, Min(b.y, c.y)));
  Bottom := Trunc(Max(a.y, Max(b.y, c.y)));
  Count := Bottom - Top + 1;
  SetLength(Lines, Count);
  for Index := 0 to Count - 1 do
  begin
    Lines[Index].x1 := MaxInt;
    Lines[Index].x2 := 1 - MaxInt;
  end;
  Scan(a, b);
  Scan(b, c);
  Scan(c, a);
  for Index := 0 to Count - 1 do
  begin
    drawLine(Index + Top, Lines[Index]);
  end;
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

procedure TPixel32CustomShader.init(surface: iPixSurface);
begin
  Assert(assigned(surface));
  fsurface := surface;
end;

{ TPixel32BasicColorshader }

procedure TPixel32BasicColorShader.process;
begin
  Bits^ := Color;
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


end.
