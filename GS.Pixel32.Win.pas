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
 Unit Name : GS.Pixe32.Win
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : Pixel32's Windows DC Bridge.
 Date:     : 2020031
 History   :
 20200301 - Creating unit.

Description :
  Friend class helper to handle Windows's DC access.
  Use it under Delphi VCL (FormPain event) or Lazarus LCL (WMBackground's Message.DC - See demo)
-----------------------------------------------------------------------------}
{$I GSCore.Inc}

unit GS.Pixel32.Win;


interface

Uses SysUtils, Classes, Types, Windows, Graphics, GS.Pixel32.Types, GS.Pixel32;

Type

TPixel32WinHelper = class helper for TPixel32
public
  procedure CopyToDc(dstDc: HDC; x: Integer = 0; y: Integer = 0;
    transparent: Boolean = true; bkColor: TP32 = 0);

  procedure CopyFromDC(srcDc: HDC; const srcRect: Types.TRect);

  procedure loadFromFile(filename : String);
  procedure SaveToFile(filename : String);
end;

var
  ScreenPixelsY : Integer;
  DPIScale : Double;

Implementation


procedure GetDPI;
var
  dc: HDC;
begin
  dc := GetDC(0);
  ScreenPixelsY := GetDeviceCaps(dc, LOGPIXELSY);
  DpiScale := ScreenPixelsY / 96;
  ReleaseDC(0, dc);
end;

function GetCompatibleMemDc(wnd: HWnd = 0): HDC;
var
  dc: HDC;
begin
  dc := Windows.GetDC(wnd);
  try
    Result := CreateCompatibleDC(dc);
  finally
    Windows.ReleaseDC(wnd, dc);
  end;
end;
//------------------------------------------------------------------------------

function Get32bitBitmapInfoHeader(width, height: Integer): TBitmapInfoHeader;
begin
  FillChar(Result, sizeof(Result), #0);
  Result.biSize := sizeof(TBitmapInfoHeader);
  Result.biWidth := width;
  Result.biHeight := height;
  Result.biPlanes := 1;
  Result.biBitCount := 32;
  Result.biSizeImage := width * height * SizeOf(TP32);
  Result.biCompression := BI_RGB;
end;

procedure TPixel32WinHelper.CopyToDc(dstDc: HDC;
  x,y: Integer; transparent: Boolean; bkColor: TP32);
var
  tmp: TPixel32;
  bi: TBitmapInfoHeader;
  bm, oldBm: HBitmap;
  dibBits: Pointer;
  memDc: HDC;
begin
  if (width=0) or (height=0) then
    Exit;

  bi := Get32bitBitmapInfoHeader(Width, Height);

  tmp := TPixel32.create(fwidth,fheight);
  try
    copyTo(TPixel32(tmp));
    TPixel32(tmp).flipVertical;
    memDc := GetCompatibleMemDc;
    try
      bm := CreateDIBSection(memDc, PBITMAPINFO(@bi)^,
        DIB_RGB_COLORS, dibBits, 0, 0);
      if bm = 0 then Exit;
      try
        Move(tmp.getSurfacePtr^, dibBits^, Width * Height * SizeOf(TP32));
        oldBm := SelectObject(memDC, bm);
        BitBlt(dstDc, x,y, Width, Height, memDc, 0,0, SRCCOPY);
        SelectObject(memDC, oldBm);
      finally
        DeleteObject(bm);
      end;
    finally
      DeleteDc(memDc);
    end;
  finally
    FreeAndNil(tmp);
  end;
end;

procedure TPixel32WinHelper.loadFromFile(filename: String);
var b : TBitmap;
begin
  b := TBitmap.Create;
  try
    b.LoadFromFile(filename);
    CopyFromDC(b.Canvas.Handle,Types.rect(0,0,b.Width,b.Height));
  finally
    freeandNil(b); //Free vcl ressources.
  end;
end;

procedure TPixel32WinHelper.SaveToFile(filename: String);
var b : TBitmap;
begin
  b := TBitmap.Create;
  try
    b.PixelFormat := pf32bit;
    b.SetSize(width,height);
    CopyToDc(b.Canvas.Handle);
    b.SaveToFile(filename);
  finally
    freeandNil(b); //Free vcl ressources.
  end;
end;

procedure TPixel32WinHelper.CopyFromDC(srcDc: HDC; const srcRect: TRect);
var
  bi: TBitmapInfoHeader;
  bm, oldBm: HBitmap;
  memDc: HDC;
  pixels: Pointer;
  w,h: integer;
begin
  {$IFDEF FPC}
  w := srcRect.Right - srcRect.Left;
  h := srcRect.Bottom - srcRect.Top;
  {$ELSE}
  w := RectWidth(srcRect);
  h := RectHeight(srcRect);
  {$ENDIF}
  resize(w, h);
  bi := Get32bitBitmapInfoHeader(w, h);
  memDc := GetCompatibleMemDc;
  try
    bm := CreateDIBSection(memDc,
      PBITMAPINFO(@bi)^, DIB_RGB_COLORS, pixels, 0, 0);
    if bm = 0 then Exit;
    try
      oldBm := SelectObject(memDc, bm);
      BitBlt(memDc, 0, 0, w, h, srcDc, srcRect.Left,srcRect.Top, SRCCOPY);
      Move(pixels^, getSurfacePtr^, w * h * sizeOf(TP32));
      SelectObject(memDc, oldBm);
    finally
      DeleteObject(bm);
    end;
  finally
    DeleteDc(memDc);
  end;
  AlphaLayerReset;
  FlipVertical;
end;

end.
