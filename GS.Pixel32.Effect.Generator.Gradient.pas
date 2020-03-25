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
 Unit Name : GS.Pixel32.Effect.Generator.Gradient
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic Pxel class.
 Date:     : 20200324
 History   :
 20200301 - Creating unit.

Credits :
 - http://www.delphiarea.com for Gradient generaion technics.

Description :
-----------------------------------------------------------------------------}
unit GS.Pixel32.Effect.Generator.Gradient;

interface

Uses Classes,
     SysUtils,
     Math,
     GS.Pixel32,
     GS.Pixel32.Effect.Generator;


Type
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


  TPixel32GeneratorGradient = class(TCustomPixel32Generator)
  protected
    colors : TGradientColors;
    procedure check;
    procedure UpdatePattern(colorBegin, colorend : TP32; shift : TGradientShift; rotation : TGradientRotation; out Colors: TGradientColors);
    procedure RadialRect;
    procedure RadialCentral;
    procedure LinearHorizontal;
  public
    ColorA, ColorB : TP32;
    ShiftGradient, RorateGradient : TGradientShift;
    procedure Process; override;
  end;


implementation

procedure TPixel32GeneratorGradient.UpdatePattern(colorBegin, colorend : TP32; shift : TGradientShift; rotation : TGradientRotation; out Colors: TGradientColors);
var
  dRed, dGreen, dBlue, dReserved: Integer;
  RGBColor1, RGBColor2: TP32;
  RGB1, RGB2: TRGBQuad;
  Index, rIndex: Integer;
  M, rM: Integer;


  function mul_div(a,b,c : integer) : integer;
  begin
    result := round(a*b/c);
  end;
begin
  RGBColor1 := ColorBegin;
  RGBColor2 := ColorEnd;

  RGB1.rgbRed := fsurface.ColorGetRValue(RGBColor1);
  RGB1.rgbGreen := fsurface.ColorGetGValue(RGBColor1);
  RGB1.rgbBlue := fsurface.ColorGetBValue(RGBColor1);
  RGB1.rgbReserved := fsurface.ColorGetAValue(RGBColor1);

  RGB2.rgbRed := fsurface.ColorGetRValue(RGBColor2);
  RGB2.rgbGreen := fsurface.ColorGetGValue(RGBColor2);
  RGB2.rgbBlue := fsurface.ColorGetBValue(RGBColor2);
  RGB2.rgbReserved := fsurface.ColorGetAValue(RGBColor2);

  if Shift > 0 then
  begin
    RGB1.rgbRed := Byte(RGB1.rgbRed + mul_div(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    RGB1.rgbGreen := Byte(RGB1.rgbGreen + mul_div(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    RGB1.rgbBlue := Byte(RGB1.rgbBlue + mul_div(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
    RGB1.rgbReserved := Byte(RGB1.rgbReserved + mul_div(RGB2.rgbReserved - RGB1.rgbReserved, Shift, 100));
  end
  else if Shift < 0 then
  begin
    RGB2.rgbRed := Byte(RGB2.rgbRed + mul_div(RGB2.rgbRed - RGB1.rgbRed, Shift, 100));
    RGB2.rgbGreen := Byte(RGB2.rgbGreen + mul_div(RGB2.rgbGreen - RGB1.rgbGreen, Shift, 100));
    RGB2.rgbBlue := Byte(RGB2.rgbBlue + mul_div(RGB2.rgbBlue - RGB1.rgbBlue, Shift, 100));
    RGB2.rgbReserved := Byte(RGB2.rgbReserved + mul_div(RGB2.rgbReserved - RGB1.rgbReserved, Shift, 100));
  end;

  dRed := RGB2.rgbRed - RGB1.rgbRed;
  dGreen := RGB2.rgbGreen - RGB1.rgbGreen;
  dBlue := RGB2.rgbBlue - RGB1.rgbBlue;
  dReserved := RGB2.rgbReserved - RGB1.rgbReserved;

  for Index := 0 to high(Colors) do
    with Colors[Index] do
    begin
      rgbRed := 0;
      rgbGreen := 0;
      rgbBlue := 0;
      rgbReserved := 0;
    end;

  M := mul_div(255, Rotation, 100);
  if M = 0 then
    for Index := 0 to 255 do
      with Colors[Index] do
      begin
        rgbRed := RGB1.rgbRed + (Index * dRed) div 255;
        rgbGreen := RGB1.rgbGreen + (Index * dGreen) div 255;
        rgbBlue := RGB1.rgbBlue + (Index * dBlue) div 255;
        rgbReserved := RGB1.rgbReserved + (Index * dReserved) div 255;
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
        rgbReserved := RGB1.rgbReserved + (Index * dReserved) div M;
      end;
    for Index := M to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB1.rgbRed + ((rIndex) * dRed) div (rM);
        rgbGreen := RGB1.rgbGreen + ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB1.rgbBlue + ((rIndex) * dBlue) div (rM);
        rgbReserved := RGB1.rgbReserved + ((rIndex) * dReserved) div (rM);
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
        rgbReserved := RGB2.rgbReserved - (Index * dReserved) div M;
      end;
    for Index := M + 1 to 255 do
      with Colors[Index] do
      begin
        rIndex := 255 - Index;
        rM := 255 - M;
        rgbRed := RGB2.rgbRed - ((rIndex) * dRed) div (rM);
        rgbGreen := RGB2.rgbGreen - ((rIndex) * dGreen) div (rM);
        rgbBlue := RGB2.rgbBlue - ((rIndex) * dBlue) div (rM);
        rgbReserved := RGB2.rgbReserved - ((rIndex) * dReserved) div (rM);
      end;
  end;
end;

procedure TPixel32GeneratorGradient.RadialRect;
var
  X, Y: Integer;
  pRGB: PRGBQuad;
  Row1, Row2: PRGBQuadArray;
  Colors: TGradientColors;
begin
  UpdatePattern(ColorA,ColorB,ShiftGradient,RorateGradient,Colors);
  fsurface.resize(512,512);
  for Y := 0 to 255 do
  begin

    // Top & Bottom
    Row1 := PRGBQuadArray(fsurface.getSurfaceScanLinePtr(Y));
    Row2 := PRGBQuadArray(fsurface.getSurfaceScanLinePtr(511-Y));

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

procedure TPixel32GeneratorGradient.RadialCentral;
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
    Row1 := PRGBQuadArray(fsurface.getSurfaceScanLinePtr(Y));
    Row2 := PRGBQuadArray(fsurface.getSurfaceScanLinePtr(361-Y));
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

procedure TPixel32GeneratorGradient.check;
begin
  if not assigned(fsurface) then
    raise Exception.Create('Gradient Generator not initialized properly : Surface not assigned');
end;

procedure TPixel32GeneratorGradient.Process;
begin
  check;
  UpdatePattern(ColorA,ColorB,ShiftGradient,RorateGradient,Colors);
  RadialRect;
end;

procedure TPixel32GeneratorGradient.LinearHorizontal;
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
    Row := PRGBQuadArray(fsurface.getSurfaceScanLinePtr(Y));

    for X := 0 to 255 do
      Row[X] := Colors[X];
  end;
end;


end.
