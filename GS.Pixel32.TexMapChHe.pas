unit GS.Pixel32.TexMapChHe;
//---------------------------------------------------------------------------
// TexMapChHe.pas                                       Modified: 03-Dec-2007
// Perspective Texture Mapping
//
// This is a ported Delphi version of perspective texture mapper originally
// written in C/C++ by Chris Hecker and published in his articles.
// The code is available with the kind permission of the original author.
//
// The original articles and source code can be downloaded at:
//  http://chrishecker.com/Miscellaneous_Technical_Articles
//
// The conversion and minor modifications were made by Yuriy Kotsarenko,
// in November, 2007. Special modifications are marked by "ykot".

// VGS : 2008/10/01Slight modification to work with cDibSystem
//                 WARNING : Please, take the original units on the original author site
//                           if you want original behaviour.
// VGS : 2020/03/20 - Slight modification to work with GS.Pixel32
//                  - WARNING : Please, take the original units on the original author site if you want original behaviour.
//                  - Rename for units coherence.
//---------------------------------------------------------------------------
interface

//---------------------------------------------------------------------------
uses
 GS.Pixel32, sysutils;

 type
 TVector3 = record
  x, y, z: Single;
 end;
 TPoint2 = record
  x, y: Single;
 end;
//---------------------------------------------------------------------------
// ykot:
// The following option enables texture wrapping, particularly useful when
// texture coordinates are located outside [0..1] range.
//---------------------------------------------------------------------------

//---------------------------------------------------------------------------
//{$ifdef TextureWrapping}
//const
 // ykot:
 // The following values should be equal to (Width - 1, Height - 1) for
 // wrapping to work.
// WrapUAnd = $1FF;
// WrapVAnd = $1FF;
//{$endif}

//---------------------------------------------------------------------------
// The following directive enables perspective correction when clipping
// occurs. This is needed to avoid distortion when using perspective correct
// texture mapping.
//---------------------------------------------------------------------------
{$define PerspectiveClip}

//---------------------------------------------------------------------------
type
 PClipItem = ^TClipItem;
 TClipItem = record
  Position: TVector3;
  TexCoord: TPoint2;
  Visible : Boolean;
 end;

//---------------------------------------------------------------------------
// The following parameters define the clipping rectangle.
//---------------------------------------------------------------------------
var
 LeftClip  : Integer = 0;
 RightClip : Integer  = 640;
 TopClip   : Integer = 0;
 BottomClip: Integer = 480;

//---------------------------------------------------------------------------
procedure ClipLeft(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);
procedure ClipRight(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);
procedure ClipTop(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);
procedure ClipBottom(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);


function Vector3(x, y, z: Single): TVector3;
function Point2(x, y: Single): TPoint2;
//---------------------------------------------------------------------------
// Notice: the vertices should be specified in anti-clockwise order.
//---------------------------------------------------------------------------
procedure TexMap(Dest, Texture: TPixel32; const v0, v1, v2: TVector3;
 const uv0, uv1, uv2: TPoint2);

procedure TexMapAffine(Dest, Texture: TPixel32;
 const v0, v1, v2: TVector3;
 const uv0, uv1, uv2: TPoint2);

procedure TexMapAffine_quad(Dest, Texture: TPixel32;
 const v0, v1, v2, v3 : TVector3;
 const uv0, uv1, uv2, uv3 : TPoint2);

//---------------------------------------------------------------------------
implementation

//---------------------------------------------------------------------------
// structures, inlines, and function declarations
//---------------------------------------------------------------------------
type
 TFixed16_16 = Integer;
 TFixed28_4  = Integer;

//---------------------------------------------------------------------------
 PPoint3D = ^TPoint3D;
 TPoint3D = record
  x, y: TFixed28_4;
  z, u, v: Single;
 end;

//---------------------------------------------------------------------------
 PPoint3DArray = ^TPoint3DArray;
 TPoint3DArray = array[0..2] of TPoint3D;

//---------------------------------------------------------------------------
 TGradients = record
  aOneOverZ: array[0..2] of Single; // 1/z for each vertex
  aUOverZ  : array[0..2] of Single; // u/z for each vertex
  aVOverZ  : array[0..2] of Single; // v/z for each vertex
  dOneOverZdX: Single; // d(1/z)/dX
  dOneOverZdY: Single; // d(1/z)/dY
  dUOverZdX  : Single; // d(u/z)/dX
  dUOverZdY  : Single; // d(u/z)/dY
  dVOverZdX  : Single; // d(v/z)/dX
  dVOverZdY  : Single; // d(v/z)/dY
  dUdXModifier: TFixed16_16;
  dVdXModifier: TFixed16_16;
 end;

//---------------------------------------------------------------------------
 PEdge = ^TEdge;
 TEdge = record
  X, XStep, Numerator, Denominator: Integer; // DDA info for x
  ErrorTerm: Integer;
  Y, Height: Integer;// current y and vertical count
  OneOverZ, OneOverZStep, OneOverZStepExtra: Single; // 1/z and step
  UOverZ, UOverZStep, UOverZStepExtra: Single; // u/z and step
  VOverZ, VOverZStep, VOverZStepExtra: Single; // v/z and step
 end;

function Vector3(x, y, z: Single): TVector3;
begin
 Result.x:= x;
 Result.y:= y;
 Result.z:= z;
end;

function Point2(x, y: Single): TPoint2;
begin
 Result.x:= x;
 Result.y:= y;
end;

//---------------------------------------------------------------------------
function Fixed28_4Mul(a, b: TFixed28_4): TFixed28_4;
asm
 imul edx
 shrd eax, edx, 4
end;

//---------------------------------------------------------------------------
function Fixed28_4ToFloat(Value: TFixed28_4): Single; //inline;
begin
 Result:= Value / 16.0;
end;

//---------------------------------------------------------------------------
function FloatToFixed16_16(Value: Single): TFixed16_16;
begin
 Result:= Round(Value * 65536);
end;

//---------------------------------------------------------------------------
function FloatToFixed28_4(Value: Single): TFixed28_4;
begin
 Result:= Round(Value* 16);
end;

//---------------------------------------------------------------------------
function Ceil28_4(Value: TFixed28_4): Integer; //inline;
var
 Numerator: Integer;
begin
 Numerator:= Value - 1 + 16;

 if (Numerator >= 0) then Result:= Numerator div 16
  else
   begin
    // deal with negative numerators correctly
    Result:= -(-Numerator div 16);
    if (-Numerator mod 16 <> 0) then Dec(Result);
   end;
end;

//---------------------------------------------------------------------------
// gradients_fx_fl_a constructor
//---------------------------------------------------------------------------
procedure InitGradients(var Gradients: TGradients;
 pVertices: PPoint3DArray);
const
 Half = $8000;
 PosModifier = Half;
 NegModifier = Half - 1;
var
 Counter: Integer;
 x1y0, x0y1, Delta: TFixed28_4;
 OneOverdX, OneOverdy: Single;
 OneOverZ: Single;
 dUdXIndicator: Single;
 dUdYIndicator: Single;
 dVdXIndicator: Single;
 dVdYIndicator: Single;
begin
 x1y0:= Fixed28_4Mul(pVertices[1].x - pVertices[2].x,
  pVertices[0].y - pVertices[2].y);

 x0y1:= Fixed28_4Mul(pVertices[0].x - pVertices[2].x,
  pVertices[1].y - pVertices[2].y);

 // ykot:
 // The following code caused division by zero when the difference in points is
 // too small (probably because of low fixed-point precision).
 Delta:= x1y0 - x0y1;
 if (Delta <> 0) then OneOverdX:= 1.0 / Fixed28_4ToFloat(Delta)
  else OneOverdX:= 1.0;

 OneOverdY:= -OneOverdX;

 with Gradients do
  begin
   for Counter:= 0 to 2 do
    begin
     OneOverZ:= 1.0;
     if pVertices[Counter].z <> 0 then
       OneOverZ:= 1.0 / pVertices[Counter].z;
     aOneOverZ[Counter]:= OneOverZ;
     aUOverZ[Counter]  := pVertices[Counter].u * OneOverZ;
     aVOverZ[Counter]  := pVertices[Counter].v * OneOverZ;
    end;

   dOneOverZdX:= OneOverdX * (((aOneOverZ[1] - aOneOverZ[2]) *
    Fixed28_4ToFloat(pVertices[0].y - pVertices[2].y)) -
     ((aOneOverZ[0] - aOneOverZ[2]) *
      Fixed28_4ToFloat(pVertices[1].y - pVertices[2].y)));

   dOneOverZdY:= OneOverdY * (((aOneOverZ[1] - aOneOverZ[2]) *
    Fixed28_4ToFloat(pVertices[0].x - pVertices[2].x)) -
     ((aOneOverZ[0] - aOneOverZ[2]) *
      Fixed28_4ToFloat(pVertices[1].x - pVertices[2].x)));

   dUOverZdX:= OneOverdX * (((aUOverZ[1] - aUOverZ[2]) *
    Fixed28_4ToFloat(pVertices[0].y - pVertices[2].y)) -
     ((aUOverZ[0] - aUOverZ[2]) *
      Fixed28_4ToFloat(pVertices[1].y - pVertices[2].y)));

   dUOverZdY:= OneOverdY * (((aUOverZ[1] - aUOverZ[2]) *
    Fixed28_4ToFloat(pVertices[0].x - pVertices[2].x)) -
     ((aUOverZ[0] - aUOverZ[2]) *
      Fixed28_4ToFloat(pVertices[1].x - pVertices[2].x)));

   dVOverZdX:= OneOverdX * (((aVOverZ[1] - aVOverZ[2]) *
    Fixed28_4ToFloat(pVertices[0].y - pVertices[2].y)) -
     ((aVOverZ[0] - aVOverZ[2]) *
      Fixed28_4ToFloat(pVertices[1].y - pVertices[2].y)));

   dVOverZdY:= OneOverdY * (((aVOverZ[1] - aVOverZ[2]) *
    Fixed28_4ToFloat(pVertices[0].x - pVertices[2].x)) -
     ((aVOverZ[0] - aVOverZ[2]) *
      Fixed28_4ToFloat(pVertices[1].x - pVertices[2].x)));

   // set up rounding modifiers
   dUdXIndicator:= dUOverZdX * aOneOverZ[0] - aUOverZ[0] * dOneOverZdX;

   if (dUdXIndicator > 0) then
    begin
     dUdXModifier:= PosModifier;
    end else
   if (dUdXIndicator < 0) then
    begin
     dUdXModifier:= NegModifier;
    end else
     begin // dUdX == 0
      dUdYIndicator:= dUOverZdY * aOneOverZ[0] - aUOverZ[0] * dOneOverZdY;

      if(dUdYIndicator >= 0) then
       begin
        dUdXModifier:= PosModifier;
       end else
       begin
        dUdXModifier:= NegModifier;
       end;
     end;

   dVdXIndicator:= dVOverZdX * aOneOverZ[0] - aVOverZ[0] * dOneOverZdX;

   if (dVdXIndicator > 0) then
    begin
     dVdXModifier:= PosModifier;
    end else
   if (dVdXIndicator < 0) then
    begin
     dVdXModifier:= NegModifier;
    end else
     begin // dVdX == 0
      dVdYIndicator:= dVOverZdY * aOneOverZ[0] - aVOverZ[0] * dOneOverZdY;

      if (dVdYIndicator >= 0) then
       begin
        dVdXModifier:= PosModifier;
       end else
       begin
        dVdXModifier:= NegModifier;
       end;
     end;
  end;
end;

//---------------------------------------------------------------------------
// handle floor divides and mods correctly
//---------------------------------------------------------------------------
procedure FloorDivMod(Numerator, Denominator: Integer; var oFloor,
 oMod: Integer);
begin
 Assert(Denominator > 0); // we assume it's positive

 if (Numerator >= 0) then
  begin
   // positive case, C is okay
   oFloor:= Numerator div Denominator;
   oMod  := Numerator mod Denominator;
  end else
  begin
   // Numerator is negative, do the right thing
   oFloor:= -(-Numerator div Denominator);
   oMod  := -Numerator mod Denominator;

   if (oMod <> 0) then
    begin
     // there is a remainder
     Dec(oFloor);
     oMod:= Denominator - oMod;
    end;
  end;
end;

//---------------------------------------------------------------------------
// edge_fx_fl_a constructor
//---------------------------------------------------------------------------
procedure InitEdge(var Edge: TEdge; const Gradients: TGradients;
 pVertices: PPoint3DArray; Top, Bottom: Integer);
var
 YEnd: Integer;
 dN, dM: Integer;
 InitialNumerator: Integer;
 YPrestep: Single;
 XPrestep: Single;
begin
 with Edge do
  begin
   Y     := Ceil28_4(pVertices[Top].y);
   YEnd  := Ceil28_4(pVertices[Bottom].y);
   Height:= YEnd - Y;

   if (Height <> 0) then
    begin
     dN:= pVertices[Bottom].y - pVertices[Top].y;
     dM:= pVertices[Bottom].x - pVertices[Top].x;

     InitialNumerator:= dM * 16 * Y - dM * pVertices[Top].y +
      dN * pVertices[Top].x - 1 + dN * 16;

     FloorDivMod(InitialNumerator, dN * 16, X, ErrorTerm);
     FloorDivMod(dM * 16, dN * 16, XStep, Numerator);

     Denominator:= dN * 16;

     YPrestep:= Fixed28_4ToFloat(Y * 16 - pVertices[Top].y);
     XPrestep:= Fixed28_4ToFloat(X * 16 - pVertices[Top].x);

     OneOverZ:= Gradients.aOneOverZ[Top] +
      YPrestep * Gradients.dOneOverZdY +
      XPrestep * Gradients.dOneOverZdX;

     OneOverZStep:= XStep * Gradients.dOneOverZdX + Gradients.dOneOverZdY;
     OneOverZStepExtra:= Gradients.dOneOverZdX;

     UOverZ:= Gradients.aUOverZ[Top] +
      YPrestep * Gradients.dUOverZdY +
      XPrestep * Gradients.dUOverZdX;

     UOverZStep:= XStep * Gradients.dUOverZdX + Gradients.dUOverZdY;
     UOverZStepExtra:= Gradients.dUOverZdX;

     VOverZ:= Gradients.aVOverZ[Top] +
      YPrestep * Gradients.dVOverZdY +
      XPrestep * Gradients.dVOverZdX;

     VOverZStep:= XStep * Gradients.dVOverZdX + Gradients.dVOverZdY;
     VOverZStepExtra:= Gradients.dVOverZdX;
   end;
  end;
end;

//---------------------------------------------------------------------------
function EdgeStep(var Edge: TEdge): Integer; //inline;
begin
 with Edge do
  begin
   Inc(X, XStep);
   Inc(Y);
   Dec(Height);

   UOverZ:= UOverZ + UOverZStep;
   VOverZ:= VOverZ + VOverZStep;
   OneOverZ:= OneOverZ + OneOverZStep;

   Inc(ErrorTerm, Numerator);
   if (ErrorTerm >= Denominator) then
    begin
     Inc(X);
     Dec(ErrorTerm, Denominator);

     OneOverZ:= OneOverZ + OneOverZStepExtra;
     UOverZ:= UOverZ + UOverZStepExtra;
     VOverZ:= VOverZ + VOverZStepExtra;
    end;

   Result:= Height;
  end;
end;

//---------------------------------------------------------------------------
// DrawScanLine
//---------------------------------------------------------------------------
procedure DrawScanLine_suba(Dest: TPixel32;
 const Gradients: TGradients; var pLeft, pRight: TEdge;
 Texture: TPixel32);
const
 AffineLength = 8;
var
 XStart, Width: Integer;
 pDestBits   : Pointer;
 pTextureBits: Pointer;
 TextureDeltaScan: Integer;
 OneOverZLeft: Single;
 UOverZLeft: Single;
 VOverZLeft: Single;
 dOneOverZdXAff, dUOverZdXAff, dVOverZdXAff: Single;
 OneOverZRight, UOverZRight, VOverZRight: Single;
 ZLeft, ULeft, VLeft, ZRight, URight, VRight: Single;
 U, V, DeltaU, DeltaV: TFixed16_16;
 Subdivisions, WidthModLength: Integer;
 Counter, UInt, VInt: Integer;
 sourceT : TP32Rec;
begin
  XStart:= pLeft.X;
  Width := pRight.X - XStart;

  if (Width <= 1) then
    Exit;

  //pDestBits:= Dest.Bits;
  pDestBits:= Dest.getSurfacePtr;
  //pTextureBits:= Texture.Bits;
  pTextureBits:= Texture.getSurfacePtr;

  //Inc(Integer(pDestBits), pLeft.Y * Dest.Pitch + XStart * 4);
  Inc(Integer(pDestBits), pLeft.Y * Dest.Width*4 + XStart * 4);
  //TextureDeltaScan:=Texture.Pitch;
  TextureDeltaScan:= Texture.Width*4;

  OneOverZLeft:= pLeft.OneOverZ;
  UOverZLeft  := pLeft.UOverZ;
  VOverZLeft  := pLeft.VOverZ;

  dOneOverZdXAff:= Gradients.dOneOverZdX * AffineLength;
  dUOverZdXAff  := Gradients.dUOverZdX * AffineLength;
  dVOverZdXAff  := Gradients.dVOverZdX * AffineLength;

  OneOverZRight:= OneOverZLeft + dOneOverZdXAff;
  UOverZRight  := UOverZLeft + dUOverZdXAff;
  VOverZRight  := VOverZLeft + dVOverZdXAff;

  ZLeft:= 1.0 / OneOverZLeft;
  ULeft:= ZLeft * UOverZLeft;
  VLeft:= ZLeft * VOverZLeft;

  Subdivisions  := Width div AffineLength;
  WidthModLength:= Width mod AffineLength;

  if (WidthModLength = 0) then
  begin
   Dec(Subdivisions);
   WidthModLength:= AffineLength;
  end;

  while (Subdivisions > 0) do
  begin
    ZRight:= 1.0 / OneOverZRight;
    URight:= ZRight * UOverZRight;
    VRight:= ZRight * VOverZRight;

    U:= FloatToFixed16_16(ULeft) + Gradients.dUdXModifier;
    V:= FloatToFixed16_16(VLeft) + Gradients.dVdXModifier;
    DeltaU:= FloatToFixed16_16(URight - ULeft) div AffineLength;
    DeltaV:= FloatToFixed16_16(VRight - VLeft) div AffineLength;

    for Counter:= 0 to AffineLength - 1 do
    begin
      UInt:= U shr 16;
      VInt:= V shr 16;

      if (UInt<Texture.Width) And (VInt<Texture.Height) then
      begin
       //PCardinal(pDestBits)^:= PCardinal(Integer(pTextureBits) + (VInt * TextureDeltaScan) + (UInt * 4))^;
       sourceT := pTP32Rec(Integer(pTextureBits) + (VInt * TextureDeltaScan) + (UInt * 4))^;

       pTP32Rec(pDestBits).red:=(sourceT.AlphaChannel * (sourceT.Red - pTP32Rec(pDestBits).Red) shr 8) + (pTP32Rec(pDestBits).Red);
       pTP32Rec(pDestBits).Blue:=(sourceT.AlphaChannel * (sourceT.Blue - pTP32Rec(pDestBits).Blue) shr 8) + (pTP32Rec(pDestBits).Blue);
       pTP32Rec(pDestBits).Green:=(sourceT.AlphaChannel * (sourceT.Green - pTP32Rec(pDestBits).Green) shr 8) + (pTP32Rec(pDestBits).Green);
      end;

      Inc(Integer(pDestBits), 4);

      Inc(U, DeltaU);
      Inc(V, DeltaV);
    end;

    ULeft:= URight;
    VLeft:= VRight;

    OneOverZRight:= OneOverZRight + dOneOverZdXAff;
    UOverZRight  := UOverZRight + dUOverZdXAff;
    VOverZRight  := VOverZRight + dVOverZdXAff;

    Dec(Subdivisions);
  end;

  if (WidthModLength <> 0) then
  begin
    ZRight:= 1.0 / (pRight.OneOverZ - Gradients.dOneOverZdX);
    URight:= ZRight * (pRight.UOverZ - Gradients.dUOverZdX);
    VRight:= ZRight * (pRight.VOverZ - Gradients.dVOverZdX);

    U:= FloatToFixed16_16(ULeft) + Gradients.dUdXModifier;
    V:= FloatToFixed16_16(VLeft) + Gradients.dVdXModifier;

    Dec(WidthModLength);
    if(WidthModLength > 0) then
    begin
      // guard against div-by-0 for 1 pixel lines
      DeltaU:= FloatToFixed16_16(URight - ULeft) div WidthModLength;
      DeltaV:= FloatToFixed16_16(VRight - VLeft) div WidthModLength;
    end
    else
    begin
      // ykot: This is to avoid compiler warning about unused variables.
      DeltaU:= 0;
      DeltaV:= 0;
    end;

    for Counter:= 0 to WidthModLength do
    begin
      UInt:= U shr 16;
      VInt:= V shr 16;

      if (UInt<Texture.Width) And (VInt<Texture.Height) then
      begin
//        PCardinal(pDestBits)^:= PCardinal(Integer(pTextureBits) + (VInt * TextureDeltaScan) + (UInt * 4))^;
        sourceT := pTP32Rec(Integer(pTextureBits) + (VInt * TextureDeltaScan) + (UInt * 4))^;

        pTP32Rec(pDestBits).red:=(sourceT.AlphaChannel * (sourceT.Red - pTP32Rec(pDestBits).Red) shr 8) + (pTP32Rec(pDestBits).Red);
        pTP32Rec(pDestBits).Blue:=(sourceT.AlphaChannel * (sourceT.Blue - pTP32Rec(pDestBits).Blue) shr 8) + (pTP32Rec(pDestBits).Blue);
        pTP32Rec(pDestBits).Green:=(sourceT.AlphaChannel * (sourceT.Green - pTP32Rec(pDestBits).Green) shr 8) + (pTP32Rec(pDestBits).Green);
      end;

      Inc(Integer(pDestBits), 4);

      Inc(U, DeltaU);
      Inc(V, DeltaV);
    end;
  end;
end;

//---------------------------------------------------------------------------
procedure TextureMapTriangle(Dest: TPixel32; pVertices: PPoint3DArray;
 Texture: TPixel32);
var
 Top, Middle, Bottom, MiddleForCompare, BottomForCompare, Height: Integer;
 y0, y1, y2: TFixed28_4;
 Gradients : TGradients;
 TopToBottom, TopToMiddle, MiddleToBottom: TEdge;
 pLeft, pRight: PEdge;
 MiddleIsLeft: Integer;
begin
 y0:= pVertices[0].y;
 y1:= pVertices[1].y;
 y2:= pVertices[2].y;

 // sort vertices in y
 if (y0 < y1) then
  if (y2 < y0) then
   begin
    Top   := 2;
    Middle:= 0;
    Bottom:= 1;
    MiddleForCompare:= 0;
    BottomForCompare:= 1;
   end else
   begin
    Top:= 0;

    if (y1 < y2) then
     begin
      Middle:= 1;
      Bottom:= 2;
      MiddleForCompare:= 1;
      BottomForCompare:= 2;
     end else
     begin
      Middle:= 2;
      Bottom:= 1;
      MiddleForCompare:= 2;
      BottomForCompare:= 1;
     end;
   end else
  begin
   if (y2 < y1) then
    begin
     Top   := 2;
     Middle:= 1;
     Bottom:= 0;
     MiddleForCompare:= 1;
     BottomForCompare:= 0;
    end else
    begin
     Top:= 1;
     if (y0 < y2) then
      begin
       Middle:= 0;
       Bottom:= 2;
       MiddleForCompare:= 3;
       BottomForCompare:= 2;
      end else
      begin
       Middle:= 2;
       Bottom:= 0;
       MiddleForCompare:= 2;
       BottomForCompare:= 3;
      end;
    end;
  end;

 InitGradients(Gradients, pVertices);
 InitEdge(TopToBottom, Gradients, pVertices, Top, Bottom);
 InitEdge(TopToMiddle, Gradients, pVertices, Top, Middle);
 InitEdge(MiddleToBottom, Gradients, pVertices, Middle, Bottom);

 // the triangle is clockwise, so if bottom > middle then middle is right
 if(BottomForCompare > MiddleForCompare) then
  begin
   MiddleIsLeft:= 0;
   pLeft := @TopToBottom;
   pRight:= @TopToMiddle;
  end else
  begin
   MiddleIsLeft:= 1;
   pLeft := @TopToMiddle;
   pRight:= @TopToBottom;
  end;

 Height:= TopToMiddle.Height;
 while (Height <> 0) do
  begin
   DrawScanLine_suba(Dest, Gradients, pLeft^, pRight^, Texture);

   EdgeStep(TopToMiddle);
   EdgeStep(TopToBottom);

   Dec(Height);
  end;

 Height:= MiddleToBottom.Height;

 if (MiddleIsLeft <> 0) then
  begin
   pLeft := @MiddleToBottom;
   pRight:= @TopToBottom;
  end else
  begin
   pLeft := @TopToBottom;
   pRight:= @MiddleToBottom;
  end;

 while (Height <> 0) do
  begin
   DrawScanLine_suba(Dest, Gradients, pLeft^, pRight^, Texture);

   EdgeStep(MiddleToBottom);
   EdgeStep(TopToBottom);

   Dec(Height);
  end;
end;

//---------------------------------------------------------------------------
// This routine is provided as an interface to TextureMapTriangle, making
// the conversion from floats to fixed point and converting texture
// coordinates from [0..1] to [0..Size].
//---------------------------------------------------------------------------
procedure TexMap(Dest, Texture: TPixel32; const v0, v1, v2: TVector3;
 const uv0, uv1, uv2: TPoint2);
var
 Pts: TPoint3DArray;
begin
 Pts[2].x:= FloatToFixed28_4(v0.x);
 Pts[2].y:= FloatToFixed28_4(v0.y);
 Pts[2].z:= v0.z;
 Pts[2].u:= uv0.x * Texture.Width;
 Pts[2].v:= uv0.y * Texture.Height;

 Pts[1].x:= FloatToFixed28_4(v1.x);
 Pts[1].y:= FloatToFixed28_4(v1.y);
 Pts[1].z:= v1.z;
 Pts[1].u:= uv1.x * Texture.Width;
 Pts[1].v:= uv1.y * Texture.Height;

 Pts[0].x:= FloatToFixed28_4(v2.x);
 Pts[0].y:= FloatToFixed28_4(v2.y);
 Pts[0].z:= v2.z;
 Pts[0].u:= uv2.x * Texture.Width;
 Pts[0].v:= uv2.y * Texture.Height;

 TextureMapTriangle(Dest, @Pts, Texture);

 end;


//---------------------------------------------------------------------------
procedure ClipLeft(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);
var
 i: Integer;
 CurItem, NextItem, DestItem: PClipItem;
 Theta, nTheta: Single;
 {$ifdef PerspectiveClip}iz0, iz1, zm: Single;{$endif}
begin
 OutCount:= 0;

 CurItem:= inVertices;
 for i:= 0 to InCount - 1 do
  begin
   CurItem.Visible:= CurItem.Position.x >= LeftClip;
   Inc(CurItem);
  end;

 CurItem := inVertices;
 NextItem:= Pointer(Integer(inVertices) + SizeOf(TClipItem));
 DestItem:= outVertices;

 for i:= 0 to InCount - 1 do
  begin
   if (i = InCount - 1) then NextItem:= inVertices;

   if (CurItem.Visible) then
    begin
     Move(CurItem^, DestItem^, SizeOf(TClipItem));
     Inc(DestItem);
     Inc(OutCount);
    end;

   if ((CurItem.Visible <> NextItem.Visible)) then
    begin
     Theta:= (LeftClip - CurItem.Position.x) /
      (NextItem.Position.x - CurItem.Position.x);
     nTheta:= 1.0 - Theta;

     DestItem.Position.x:= LeftClip;
     DestItem.Position.y:= CurItem.Position.y * nTheta +
      NextItem.Position.y * Theta;

     {$ifdef PerspectiveClip}
     iz0:= 1.0 / CurItem.Position.z;
     iz1:= 1.0 / NextItem.Position.z;
     zm := iz0 * nTheta + iz1 * Theta;
     DestItem.Position.z:= 1.0 / zm;
     {$else}
     DestItem.Position.z:= CurItem.Position.z * nTheta +
      NextItem.Position.z * Theta;
     {$endif}

     {$ifdef PerspectiveClip}
     DestItem.TexCoord.x:= ((CurItem.TexCoord.x / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.x / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     DestItem.TexCoord.y:= ((CurItem.TexCoord.y / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.y / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     {$else}
     DestItem.TexCoord.x:= CurItem.TexCoord.x * nTheta +
      NextItem.TexCoord.x * Theta;
     DestItem.TexCoord.y:= CurItem.TexCoord.y * nTheta +
      NextItem.TexCoord.y * Theta;
     {$endif}

     Inc(DestItem);
     Inc(OutCount);
    end;

   Inc(CurItem);
   Inc(NextItem);
  end;
end;

//---------------------------------------------------------------------------
procedure ClipRight(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);
var
 i: Integer;
 CurItem, NextItem, DestItem: PClipItem;
 Theta, nTheta: Single;
 {$ifdef PerspectiveClip}iz0, iz1, zm: Single;{$endif}
begin
 OutCount:= 0;

 CurItem:= inVertices;
 for i:= 0 to InCount - 1 do
  begin
   CurItem.Visible:= CurItem.Position.x <= RightClip;
   Inc(CurItem);
  end;

 CurItem := inVertices;
 NextItem:= Pointer(Integer(inVertices) + SizeOf(TClipItem));
 DestItem:= outVertices;

 for i:= 0 to InCount - 1 do
  begin
   if (i = InCount - 1) then NextItem:= inVertices;

   if (CurItem.Visible) then
    begin
     Move(CurItem^, DestItem^, SizeOf(TClipItem));
     Inc(DestItem);
     Inc(OutCount);
    end;

   if ((CurItem.Visible <> NextItem.Visible)) then
    begin
     Theta:= (RightClip - CurItem.Position.x) /
      (NextItem.Position.x - CurItem.Position.x);
     nTheta:= 1.0 - Theta;

     DestItem.Position.x:= RightClip;
     DestItem.Position.y:= CurItem.Position.y * nTheta +
      NextItem.Position.y * Theta;

     {$ifdef PerspectiveClip}
     iz0:= 1.0 / CurItem.Position.z;
     iz1:= 1.0 / NextItem.Position.z;
     zm := iz0 * nTheta + iz1 * Theta;
     DestItem.Position.z:= 1.0 / zm;
     {$else}
     DestItem.Position.z:= CurItem.Position.z * nTheta +
      NextItem.Position.z * Theta;
     {$endif}

     {$ifdef PerspectiveClip}
     DestItem.TexCoord.x:= ((CurItem.TexCoord.x / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.x / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     DestItem.TexCoord.y:= ((CurItem.TexCoord.y / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.y / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     {$else}
     DestItem.TexCoord.x:= CurItem.TexCoord.x * nTheta +
      NextItem.TexCoord.x * Theta;
     DestItem.TexCoord.y:= CurItem.TexCoord.y * nTheta +
      NextItem.TexCoord.y * Theta;
     {$endif}

     Inc(DestItem);
     Inc(OutCount);
    end;

   Inc(CurItem);
   Inc(NextItem);
  end;
end;

//---------------------------------------------------------------------------
procedure ClipTop(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);
var
 i: Integer;
 CurItem, NextItem, DestItem: PClipItem;
 Theta, nTheta: Single;
 {$ifdef PerspectiveClip}iz0, iz1, zm: Single;{$endif}
begin
 OutCount:= 0;

 CurItem:= inVertices;
 for i:= 0 to InCount - 1 do
  begin
   CurItem.Visible:= CurItem.Position.y >= TopClip;
   Inc(CurItem);
  end;

 CurItem := inVertices;
 NextItem:= Pointer(Integer(inVertices) + SizeOf(TClipItem));
 DestItem:= outVertices;

 for i:= 0 to InCount - 1 do
  begin
   if (i = InCount - 1) then NextItem:= inVertices;

   if (CurItem.Visible) then
    begin
     Move(CurItem^, DestItem^, SizeOf(TClipItem));
     Inc(DestItem);
     Inc(OutCount);
    end;

   if ((CurItem.Visible <> NextItem.Visible)) then
    begin
     Theta:= (TopClip - CurItem.Position.y) /
      (NextItem.Position.y - CurItem.Position.y);
     nTheta:= 1.0 - Theta;

     DestItem.Position.y:= TopClip;
     DestItem.Position.x:= CurItem.Position.x * nTheta +
      NextItem.Position.x * Theta;

     {$ifdef PerspectiveClip}
     iz0:= 1.0 / CurItem.Position.z;
     iz1:= 1.0 / NextItem.Position.z;
     zm := iz0 * nTheta + iz1 * Theta;
     DestItem.Position.z:= 1.0 / zm;
     {$else}
     DestItem.Position.z:= CurItem.Position.z * nTheta +
      NextItem.Position.z * Theta;
     {$endif}

     {$ifdef PerspectiveClip}
     DestItem.TexCoord.x:= ((CurItem.TexCoord.x / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.x / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     DestItem.TexCoord.y:= ((CurItem.TexCoord.y / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.y / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     {$else}
     DestItem.TexCoord.x:= CurItem.TexCoord.x * nTheta +
      NextItem.TexCoord.x * Theta;
     DestItem.TexCoord.y:= CurItem.TexCoord.y * nTheta +
      NextItem.TexCoord.y * Theta;
     {$endif}

     Inc(DestItem);
     Inc(OutCount);
    end;

   Inc(CurItem);
   Inc(NextItem);
  end;
end;

//---------------------------------------------------------------------------
procedure ClipBottom(inVertices, outVertices: PClipItem; InCount: Integer;
 out OutCount: Integer);
var
 i: Integer;
 CurItem, NextItem, DestItem: PClipItem;
 Theta, nTheta: Single;
 {$ifdef PerspectiveClip}iz0, iz1, zm: Single;{$endif}
begin
 OutCount:= 0;

 CurItem:= inVertices;
 for i:= 0 to InCount - 1 do
  begin
   CurItem.Visible:= CurItem.Position.y <= BottomClip;
   Inc(CurItem);
  end;

 CurItem := inVertices;
 NextItem:= Pointer(Integer(inVertices) + SizeOf(TClipItem));
 DestItem:= outVertices;

 for i:= 0 to InCount - 1 do
  begin
   if (i = InCount - 1) then NextItem:= inVertices;

   if (CurItem.Visible) then
    begin
     Move(CurItem^, DestItem^, SizeOf(TClipItem));
     Inc(DestItem);
     Inc(OutCount);
    end;

   if ((CurItem.Visible <> NextItem.Visible)) then
    begin
     Theta:= (BottomClip - CurItem.Position.y) /
      (NextItem.Position.y - CurItem.Position.y);
     nTheta:= 1.0 - Theta;

     DestItem.Position.y:= BottomClip;
     DestItem.Position.x:= CurItem.Position.x * nTheta +
      NextItem.Position.x * Theta;

     {$ifdef PerspectiveClip}
     iz0:= 1.0 / CurItem.Position.z;
     iz1:= 1.0 / NextItem.Position.z;
     zm := iz0 * nTheta + iz1 * Theta;
     DestItem.Position.z:= 1.0 / zm;
     {$else}
     DestItem.Position.z:= CurItem.Position.z * nTheta +
      NextItem.Position.z * Theta;
     {$endif}

     {$ifdef PerspectiveClip}
     DestItem.TexCoord.x:= ((CurItem.TexCoord.x / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.x / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     DestItem.TexCoord.y:= ((CurItem.TexCoord.y / CurItem.Position.z) * nTheta +
      (NextItem.TexCoord.y / NextItem.Position.z) * Theta) *
      DestItem.Position.z;
     {$else}
     DestItem.TexCoord.x:= CurItem.TexCoord.x * nTheta +
      NextItem.TexCoord.x * Theta;
     DestItem.TexCoord.y:= CurItem.TexCoord.y * nTheta +
      NextItem.TexCoord.y * Theta;
     {$endif}

     Inc(DestItem);
     Inc(OutCount);
    end;

   Inc(CurItem);
   Inc(NextItem);
  end;
end;

var
 ClipArray: array[0..31] of TClipItem;

//---------------------------------------------------------------------------
procedure TexMapAffine(Dest, Texture: TPixel32;
 const v0, v1, v2: TVector3;
 const uv0, uv1, uv2: TPoint2);
var
 i, Base, Count, OutCount: Integer;
 Item0, Item1, Item2: PClipItem;
begin
{$ifdef TextureWrapping}
 WrapUAnd := (Texture.Width)-1;
 WrapVAnd := (Texture.Height)-1;
{$endif}

 LeftClip  := 0;
 RightClip := Dest.Width;
 TopClip   := 0;
 BottomClip:= Dest.Height;


 ClipArray[0].Position:= v0;
 ClipArray[0].TexCoord:= uv0;
 ClipArray[1].Position:= v1;
 ClipArray[1].TexCoord:= uv1;
 ClipArray[2].Position:= v2;
 ClipArray[2].TexCoord:= uv2;

 Base := 0;
 Count:= 3;
 ClipLeft(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;
 ClipRight(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;
 ClipTop(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;
 ClipBottom(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;

 for i:= 0 to Count - 3 do
  begin
   Item0:= @ClipArray[Base];
   Item1:= @ClipArray[Base + 1 + i];
   Item2:= @ClipArray[Base + 2 + i];
   TexMap(Dest, Texture, Item2.Position, Item1.Position,
    Item0.Position, Item2.TexCoord, Item1.TexCoord, Item0.TexCoord);
  end;
end;


procedure TexMapAffine_quad(Dest, Texture: TPixel32;
 const v0, v1, v2, v3 : TVector3;
 const uv0, uv1, uv2, uv3 : TPoint2);
var
 i, Base, Count, OutCount: Integer;
 Item0, Item1, Item2, item3: PClipItem;

begin
{$ifdef TextureWrapping}
 WrapUAnd := (Texture.Width)-1;
 WrapVAnd := (Texture.Height)-1;
{$endif}

 LeftClip  := 0;
 RightClip := Dest.Width;
 TopClip   := 0;
 BottomClip:= Dest.Height;


 ClipArray[0].Position:= v0;
 ClipArray[0].TexCoord:= uv0;
 ClipArray[1].Position:= v1;
 ClipArray[1].TexCoord:= uv1;
 ClipArray[2].Position:= v2;
 ClipArray[2].TexCoord:= uv2;
 ClipArray[3].Position:= v3;
 ClipArray[3].TexCoord:= uv3;

 Base := 0;
 Count:= 4;
 ClipLeft(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;
 ClipRight(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;
 ClipTop(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;
 ClipBottom(@ClipArray[Base], @ClipArray[Base + Count],
  Count, OutCount);

 Inc(Base, Count);
 Count:= OutCount;

 for i:= 0 to Count - 4 do
  begin
   Item0:= @ClipArray[Base];
   Item1:= @ClipArray[Base + 1 + i];
   Item2:= @ClipArray[Base + 2 + i];
   Item3:= @ClipArray[Base + 3 + i];

   TexMap(Dest, Texture, Item2.Position, Item1.Position,
    Item0.Position, Item2.TexCoord, Item1.TexCoord, Item0.TexCoord);
   TexMap(Dest, Texture, Item0.Position, Item3.Position,
    Item2.Position, Item0.TexCoord, Item3.TexCoord, Item2.TexCoord);
  end;
end;


end.
