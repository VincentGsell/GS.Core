
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
 Unit Name : GS.Pixel32.DirectRasterizer
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : Direct call rasterizer helper (flat, colored, texture).
 Date:     : 2020031
 History   :
 20200501 - Creating unit.

Credits :
 - ScratchPixel.com : THE ressources. :)

Description :
 - Keep it relatively simple to learn purpose.
 - Relativeley optimized rasterization methods, avoid call stack and reprocess phenomenon
 due to analytics codes organization. (Big question : could I obtain something
 readable/clean AND fast ? ;)).
-----------------------------------------------------------------------------}
{$I GSCore.Inc}

unit GS.Pixel32.DirectRasterizer;

interface

uses sysutils, classes, math,
    GS.Geometry,
    GS.Pixel32,
    GS.Pixel32.types,
    GS.Pixel32.Rasterize {For backbuffer, and edge support function};

Type
TPixel32DirectMode = Class
  //Direct Pixel drawing.
  class procedure FlatColor1_drawPixel(surface : TPixel32; x,y : integer; color : TP32);

  //Triangle draw, with one flat color.
  class procedure FlatColor1_drawTri(surface : TPixel32; const v : TP32triVertices; color : TP32);

  //Triangle draw, with 3 flat colors. Colors parameter taken un vertice.rgb struct.
  class procedure FlatColor3_drawTri(surface : TPixel32; const v : TP32triVertices);

  //Triangle draw, with Texture map. UV mapping provide un vertice.u/v struct.
  class procedure TextureColor_drawTri(surface : TPixel32; const v : TP32triVertices; sourceTexture : TPixel32);
End;

implementation

{ TPixel32DirectMode }

class procedure TPixel32DirectMode.FlatColor1_drawTri(surface: TPixel32; const v: TP32triVertices; color : TP32);
var w,h : Uint32;
    i,j : integer;

    area : TVecType;
    minx, maxx, miny, maxy : integer;
    vi0x,vi0y,vi1x,vi1y,vi2x,vi2y : integer;
    p : TP32Vertice;
    w0,w1,w2 : TVecType; //interpolation.

    bits : pTP32; //frontbuffer.
    bbBits : pTP32; //backbuffer (for alpha)

    sSurfaceColor, colAsRec, res : TP32Rec; //For alpha channel process color.
    sv : TP32Vertice;
begin
  Assert(Assigned(surface));

  area := edgeFunction(v[0],v[1],v[2]);
  if area<=0 then
  begin
    //swap (rev. triangle.)
    sv := v[0];
    v[0] := v[2];
    v[2] := sv;
    area := edgeFunction(v[0],v[1],v[2]);
    if area<=0 then
      Exit;
  end;

  w := surface.width;
  h := surface.height;

  vi0x := round(v[0].x);
  vi0y := round(v[0].y);
  vi1x := round(v[1].x);
  vi1y := round(v[1].y);
  vi2x := round(v[2].x);
  vi2y := round(v[2].y);

  minx:=max(min(min(vi0x,vi1x),vi2x),0);
  miny:=max(min(min(vi0y,vi1y),vi2y),0);

  maxx:=min(max(max(vi0x,vi1x),vi2x),w-1);
  maxy:=min(max(max(vi0y,vi1y),vi2y),h-1);

  if maxx-minx=0 then exit;
  if maxy-miny=0 then exit;

  if TP32Rec(color).AlphaChannel=255 then
  begin
    //Flat color process. No need alpha calculus or backbuffer.
    for j := miny to maxy do
    begin
      bits := surface.getSurfacePtr;
      inc(bits,j*w + minx);
      for i := minx to maxx do
      begin
        p.x := i; p.y := j;
        w0 := edgeFunction(v[1], v[2], p); //Y-Z
        w1 := edgeFunction(v[2], v[0], p); //Z-X
        w2 := edgeFunction(v[0], v[1], p); //X-Y
        if (w0>=0) and (w1>=0) and (w2>=0) then
        begin
          bits^ := color;
        end;
        inc(bits);
      end;
    end;
  end
  else
  begin
    //Alphachannel process.
    colAsRec.Color := color;
    for j := miny to maxy do
    begin
      bits := surface.getSurfacePtr;
      inc(bits,j*w + minx);
      bbBits := BackBuffer.getSurfacePtr;
      inc(bbBits,j*w + minx);

      for i := minx to maxx do
      begin
        if bbBits^ <> BackBuffercol then
        begin
          p.x := i; p.y := j;
          w0 := edgeFunction(v[1], v[2], p);
          w1 := edgeFunction(v[2], v[0], p);
          w2 := edgeFunction(v[0], v[1], p);
          if (w0>=0) and (w1>=0) and (w2>=0) then
          begin
            sSurfaceColor.Color := bits^;
            res.red:=(colAsRec.AlphaChannel * (colAsRec.Red - sSurfaceColor.Red) shr 8) + (sSurfaceColor.Red);
            res.Blue:=(colAsRec.AlphaChannel * (colAsRec.Blue - sSurfaceColor.Blue) shr 8) + (sSurfaceColor.Blue);
            res.Green:=(colAsRec.AlphaChannel * (colAsRec.Green - sSurfaceColor.Green) shr 8) + (sSurfaceColor.Green);
            res.AlphaChannel := colAsRec.AlphaChannel;
            bits^ := res.Color;
            bbBits^ := BackBuffercol;
          end;
        end;
        inc(bits);
        inc(bbBits);
      end;
    end;
  end;
end;

class procedure TPixel32DirectMode.FlatColor3_drawTri(surface: TPixel32; const v: TP32triVertices);
var w,h : Uint32;
    i,j : integer;

    area : TVecType;
    minx, maxx, miny, maxy : integer;
    vi0x,vi0y,vi1x,vi1y,vi2x,vi2y : integer;
    w0,w1,w2 : TVecType; //interpolation.

    bits : pTP32; //frontbuffer.
    bbBits : pTP32; //backbuffer (for alpha)

    r,g,b,a : single;

    sSurfaceColor, colAsRec, res : TP32Rec; //For alpha channel process color.
    sv : TP32Vertice;

    //Optimization (replace edgefunction)
    v2xmv1x,v0xmv2x,v1xmv0x : TVecType;
    v2ymv1y,v0ymv2y,v1ymv0y : TVecType;
    v1ympyMulv2xmv1x, v2ympyMulv0xmv2x, v0ympyMulv1xmv0x : TVecType;
begin
  Assert(Assigned(surface));

  area := edgeFunction(v[0],v[1],v[2]);
  if area<=0 then
  begin
    //swap (rev. triangle.)
    sv := v[0];
    v[0] := v[2];
    v[2] := sv;
    area := edgeFunction(v[0],v[1],v[2]);
    if area<=0 then
      Exit;
  end;

  w := surface.width;
  h := surface.height;

  vi0x := round(v[0].x);
  vi0y := round(v[0].y);
  vi1x := round(v[1].x);
  vi1y := round(v[1].y);
  vi2x := round(v[2].x);
  vi2y := round(v[2].y);

  minx:=max(min(min(vi0x,vi1x),vi2x),0);
  miny:=max(min(min(vi0y,vi1y),vi2y),0);

  maxx:=min(max(max(vi0x,vi1x),vi2x),w-1);
  maxy:=min(max(max(vi0y,vi1y),vi2y),h-1);

  if maxx-minx=0 then exit;
  if maxy-miny=0 then exit;

  v2xmv1x := v[2].x - v[1].x;
  v0xmv2x := v[0].x - v[2].x;
  v1xmv0x := v[1].x - v[0].x;
  v2ymv1y := v[2].y - v[1].y;
  v0ymv2y := v[0].y - v[2].y;
  v1ymv0y := v[1].y - v[0].y;

  if (v[0].rgba.a=1) and
     (v[1].rgba.a=1) and
     (v[2].rgba.a=1) then
  begin
    //Flat color process. No need alpha calculus or backbuffer.

    for j := miny to maxy do
    begin
      bits := surface.getSurfacePtr;
      inc(bits,j*w + minx);

      v1ympyMulv2xmv1x := (v[1].y - j) * (v2xmv1x);
      v2ympyMulv0xmv2x := (v[2].y - j) * (v0xmv2x);
      v0ympyMulv1xmv0x := (v[0].y - j) * (v1xmv0x);
      for i := minx to maxx do
      begin
        //w0 := edgeFunction(v[1], v[2], p); //inlining is good, but calculus is optimized by col/row.
        w0 := (v[1].x - i) * (v2ymv1y) - v1ympyMulv2xmv1x;
        //w1 := edgeFunction(v[2], v[0], p);
        w1 := (v[2].x - i) * (v0ymv2y) - v2ympyMulv0xmv2x;
        //w2 := edgeFunction(v[0], v[1], p);
        w2 := (v[0].x - i) * (v1ymv0y) - v0ympyMulv1xmv0x;


        if (w0>=0) and (w1>=0) and (w2>=0) then
        begin
          w0 := w0 / area;
          w1 := w1 / area;
          w2 := w2 / area;
          r := w0 * v[0].rgba.r + w1 * v[1].rgba.r + w2 * v[2].rgba.r;
          g := w0 * v[0].rgba.g + w1 * v[1].rgba.g + w2 * v[2].rgba.g;
          b := w0 * v[0].rgba.b + w1 * v[1].rgba.b + w2 * v[2].rgba.b;
          bits^ := surface.colorP32Rec(trunc(r*255),trunc(g*255),trunc(b*255),255).Color;
        end;
        inc(bits);
      end;
    end;
  end
  else
  begin
    //Alphachannel process.
    for j := miny to maxy do
    begin
      bits := surface.getSurfacePtr;
      inc(bits,j*w + minx);
      bbBits := BackBuffer.getSurfacePtr;
      inc(bbBits,j*w + minx);

      v1ympyMulv2xmv1x := (v[1].y - j) * (v2xmv1x);
      v2ympyMulv0xmv2x := (v[2].y - j) * (v0xmv2x);
      v0ympyMulv1xmv0x := (v[0].y - j) * (v1xmv0x);
      for i := minx to maxx do
      begin
        if bbBits^ <> BackBuffercol then
        begin
          //w0 := edgeFunction(v[1], v[2], p); //inlining is good, but calculus is optimized by col/row.
          w0 := (v[1].x - i) * (v2ymv1y) - v1ympyMulv2xmv1x;
          //w1 := edgeFunction(v[2], v[0], p);
          w1 := (v[2].x - i) * (v0ymv2y) - v2ympyMulv0xmv2x;
          //w2 := edgeFunction(v[0], v[1], p);
          w2 := (v[0].x - i) * (v1ymv0y) - v0ympyMulv1xmv0x;
          if (w0>=0) and (w1>=0) and (w2>=0) then
          begin
            w0 := w0 / area;
            w1 := w1 / area;
            w2 := w2 / area;
            r := w0 * v[0].rgba.r + w1 * v[1].rgba.r + w2 * v[2].rgba.r;
            g := w0 * v[0].rgba.g + w1 * v[1].rgba.g + w2 * v[2].rgba.g;
            b := w0 * v[0].rgba.b + w1 * v[1].rgba.b + w2 * v[2].rgba.b;
            a := w0 * v[0].rgba.a + w1 * v[1].rgba.a + w2 * v[2].rgba.a;

            colAsRec.Color := surface.colorP32Rec(trunc(r*255),trunc(g*255),trunc(b*255),trunc(a*255)).Color;

            sSurfaceColor.Color := bits^;
            res.red:=(colAsRec.AlphaChannel * (colAsRec.Red - sSurfaceColor.Red) shr 8) + (sSurfaceColor.Red);
            res.Blue:=(colAsRec.AlphaChannel * (colAsRec.Blue - sSurfaceColor.Blue) shr 8) + (sSurfaceColor.Blue);
            res.Green:=(colAsRec.AlphaChannel * (colAsRec.Green - sSurfaceColor.Green) shr 8) + (sSurfaceColor.Green);
            res.AlphaChannel := colAsRec.AlphaChannel;
            bits^ := res.Color;
            bbBits^ := BackBuffercol;
          end;
        end;
        inc(bits);
        inc(bbBits);
      end;
    end;
  end;
end;


class procedure TPixel32DirectMode.FlatColor1_drawPixel(surface: TPixel32; x, y: integer;
  color: TP32);
var b : pTP32;
begin
  if  (y<surface.height) and (x<surface.width) and (y>-1) and (x>-1) then
  begin
    b := surface.getSurfaceScanLinePtr(y);
    inc(b,x);
    b^:= color;
  end;
end;


class procedure TPixel32DirectMode.TextureColor_drawTri(surface: TPixel32;
    const v :TP32TriVertices; sourceTexture: TPixel32);
var w,h : Uint32;
    i,j : integer;

    area,w0,w1,w2 : TVecType;
    p : TP32Vertice;
    minx, maxx, miny, maxy : integer;
    vi0x,vi0y,vi1x,vi1y,vi2x,vi2y : integer;

    bits : pTP32; //frontbuffer.
    bbBits : pTP32; //backbuffer

    //Texture specific.
    sSurfaceColor : TP32Rec; //Current surface's target color.
    fcolorAsRec : TP32Rec;
    tb : pTP32;
    uu,vv : single;
    ui,vi : integer;
    sv : TP32Vertice;
begin
  assert(assigned(sourceTexture));
  Assert(BackBuffer.width = surface.width);
  Assert(BackBuffer.height = surface.height);

  area := edgeFunction(v[0],v[1],v[2]);
  if area<=0 then
  begin
    //swap (rev. triangle.)
    sv := v[0];
    v[0] := v[2];
    v[2] := sv;
    area := edgeFunction(v[0],v[1],v[2]);
    if area<=0 then
      Exit;
  end;

  w := surface.width;
  h := surface.height;

  vi0x := round(v[0].x);
  vi0y := round(v[0].y);
  vi1x := round(v[1].x);
  vi1y := round(v[1].y);
  vi2x := round(v[2].x);
  vi2y := round(v[2].y);

  minx:=max(min(min(vi0x,vi1x),vi2x),0);
  miny:=max(min(min(vi0y,vi1y),vi2y),0);

  maxx:=min(max(max(vi0x,vi1x),vi2x),w-1);
  maxy:=min(max(max(vi0y,vi1y),vi2y),h-1);

  if maxx-minx=0 then exit;
  if maxy-miny=0 then exit;

  for j := miny to maxy do
  begin
    bits := surface.getSurfacePtr;
    inc(bits,j*w + minx);

    bbBits := BackBuffer.getSurfacePtr;
    inc(bbBits,j*w + minx);

    for i := minx to maxx do
    begin
      p.x := i; p.y := j;
      w0 := edgeFunction(v[1], v[2], p);
      w1 := edgeFunction(v[2], v[0], p);
      w2 := edgeFunction(v[0], v[1], p);
      if (w0>=0) and (w1>=0) and (w2>=0) then
      begin
        w0 := w0 / area;
        w1 := w1 / area;
        w2 := w2 / area;


        if bbBits^ <> BackBuffercol then
        begin
          uu := w0 * v[0].u + w1 * v[1].u + w2 * v[2].u;
          vv := w0 * v[0].v + w1 * v[1].v + w2 * v[2].v;

          ui := Trunc(uu * sourceTexture.width);
          vi := Trunc(vv * sourceTexture.height);

          tb := sourceTexture.getSurfacePtr;
          inc(tb,vi*sourceTexture.width+ui);

          if TP32rec(tb^).AlphaChannel=255 then
            fcolorAsRec.Color := tb^
          else
          begin
            fcolorAsRec.Color := tb^;
            sSurfaceColor.Color := bits^;

            fcolorAsRec.red:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Red - sSurfaceColor.Red) shr 8) + (sSurfaceColor.Red);
            fcolorAsRec.Blue:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Blue - sSurfaceColor.Blue) shr 8) + (sSurfaceColor.Blue);
            fcolorAsRec.Green:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Green - sSurfaceColor.Green) shr 8) + (sSurfaceColor.Green);
          end;

          bits^ := fcolorAsRec.Color;
        end;
        bbBits^ := BackBuffercol;
      end;
      inc(bits);
      inc(bbBits);
    end;
  end;
end;
end.
