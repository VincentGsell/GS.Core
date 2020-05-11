
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
 Unit Name : GS.Pixel32.Rasterize
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic Pxel class.
 Date:     : 2020031
 History   :
 20200327 - Creating unit.

Credits :
 - Inspired from Codes-source cs-barbichette authors.

Description :
 - Simple rasterization methods.
-----------------------------------------------------------------------------}
{$I GSCore.Inc}

unit GS.Pixel32.Rasterize;

interface
//---------------------------------------------------------------------------


uses
 sysutils,
 GS.Pixel,
 GS.Pixel32,
 GS.Pixel32.Types,
 GS.Pixel32.Fragments,
 GS.Geometry;



//procedure triangleRasterizeTexMap(Dest : TPixel32; const v[0], v[1], v[2]: TP32Vertice);
//procedure triangleRasterizeFlat( Dest : TPixel32; const v[0], v[1], v[2]: TP32Vertice);

procedure triangleRasterize( Dest : TPixel32;
                   var v : TP32triVertices;
                   const rasterMode : TSoftwareRasterizeOption = TSoftwareRasterizeOption.roBackBuffer);


//---------------------------------------------------------------------------

//BackBuffer's aiming is to draw correctly triangles batches.
//i.e. when drawing polygone (many triangle) edge and border could be overlap.
//the backe buffer avoid that.
var BackBuffer :  TPixel32;
    DepthBuffer : TPixel32;
    BackBuffercol : TP32;

//Must be call before each batch drawing.
procedure rasterBackBufferInit(surface : iPixSurface);

//Main interpolation function.
function edgeFunction(var a,b,c : TP32Vertice) : TVecType; {$IFNDEF DEBUG} inline; {$ENDIF}

implementation

uses math,
     GS.Pixel32.DirectRasterizer;


procedure RnDtriangleRasterize( Dest : TPixel32;
                               const v : TP32triVertices;
                               fragment : TPixel32FragmentShader;
                               options : TSoftwareRasterizeOption); forward;



procedure rasterBackBufferInit(surface : iPixSurface);
begin
  if (surface.width<>BackBuffer.width) Or
     (surface.height<>BackBuffer.height) then
  begin
    BackBuffer.resize(surface.width,surface.height);
    DepthBuffer.resize(surface.width,surface.height);
  end;
  Inc(BackBuffercol);
  BackBuffer.color_pen := BackBuffercol;
end;

procedure triangleRasterize( Dest : TPixel32;
                   var v : TP32triVertices;
                   const rasterMode : TSoftwareRasterizeOption = TSoftwareRasterizeOption.roBackBuffer);
begin
  ///
  ///  WELCOME to the Rasterizer Factory :)
  ///
  ///  Here, you have the fallowing process :
  ///  If DirectMode :
  ///  - will automaticaly determine, with the entry fragshader (ie. dest.currentFragment)
  ///  what TPixel32DirectMode.xxx methodswill be called in replacement.
  ///  As a result
  ///  - No client app change (just directMode switch.)
  ///  - for simple shader, and simple use case (2d, game) you will get extra speed.
  ///  - for complex shader, you'll get FlatColor shader. :/
  ///  - It is a manner to see the gap between FragShader coding, and direct call. (degug stuff)
  ///


  case rasterMode of
    TSoftwareRasterizeOption.roDirectMode :
    begin
      ///
      ///  Direct Mode part.
      ///

      //try to find equivalent proc. from given fragment.
      if Dest.currentFragment is TPixel32FragmentShaderTextureColor then
      begin
        TPixel32DirectMode.TextureColor_drawTri(Dest,v,TPixel32FragmentShaderTextureColor(Dest.currentFragment).Texture);
      end
      else
      if Dest.currentFragment is TPixel32FragmentShaderVerticeColor then
      begin
        TPixel32DirectMode.FlatColor3_drawTri( Dest,v);
      end
      else
      if Dest.currentFragment is TPixel32FragmentShaderFlatColor then
      begin
        TPixel32DirectMode.FlatColor1_drawTri(Dest,v,TPixel32FragmentShaderFlatColor(Dest.currentFragment).Color);
      end
      else //all other.
      begin
        TPixel32DirectMode.FlatColor1_drawTri(Dest,v,gspRed);
      end;
    end;

    else
    begin
      RnDtriangleRasterize(Dest,v,Dest.currentFragment,rasterMode);
    end;
  end;

end;


function edgeFunction(var a,b,c : TP32Vertice) : TVecType; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := (a.x - c.x) * (b.y - a.y) - (a.y - c.y) * (b.x - a.x);
end;


procedure RnDtriangleRasterize( Dest : TPixel32;
                               const v : TP32triVertices;
                               fragment : TPixel32FragmentShader;
                               options : TSoftwareRasterizeOption);
var w,h : Uint32;

    sv : TP32Vertice;

    area,w0,w1,w2 : TVecType;
    p : TP32Vertice;
    minx, maxx, miny, maxy : integer;
    vi0x,vi0y,vi1x,vi1y,vi2x,vi2y : integer;

    bits : pTP32; //frontbuffer.
    bbBits : pTP32; //backbuffer
    zbits : pTP32; //ZBuffer.

    //Only BackBuffer raster (2d, alpha channel pixel perfect poly.)
    procedure BackBufferRaster;
    var i,j : integer;
        //Optimization (replace edgefunction)
        v2xmv1x,v0xmv2x,v1xmv0x : TVecType;
        v2ymv1y,v0ymv2y,v1ymv0y : TVecType;
        v1ympyMulv2xmv1x, v2ympyMulv0xmv2x, v0ympyMulv1xmv0x : TVecType;
    begin
      v2xmv1x := v[2].x - v[1].x;
      v0xmv2x := v[0].x - v[2].x;
      v1xmv0x := v[1].x - v[0].x;
      v2ymv1y := v[2].y - v[1].y;
      v0ymv2y := v[0].y - v[2].y;
      v1ymv0y := v[1].y - v[0].y;

      for j := miny to maxy do
      begin
        bits := Dest.getSurfaceScanLinePtr(j);
        inc(bits,minx);

        bbBits := BackBuffer.getSurfaceScanLinePtr(j);
        inc(bbBits,minx);

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
            if bbBits^ <> BackBuffercol then
            begin
              w0 := w0 / area;
              w1 := w1 / area;
              w2 := w2 / area;

              fragment.x := i;
              fragment.y := j;
              fragment.interpolationVerticeA := w0;
              fragment.interpolationVerticeB := w1;
              fragment.interpolationVerticeC := w2;
              fragment.sourceSurfaceBits := bits;
              fragment.process;

              if fragment.processedColor.AlphaChannel>0 then
                bits^ := fragment.processedColor.Color;
            end;
            bbBits^ := BackBuffercol;
          end;
          inc(bits);
          inc(bbBits);
        end;
      end;
    end;

    //Basic rastering, no backebuffer, no zbuffer. Graphic glitch on 2dalpha, no z order on 3d.
    procedure BasicRaster;
    var i,j : integer;
    begin
      for j := miny to maxy do
      begin
        bits := Dest.getSurfaceScanLinePtr(j);
        inc(bits,minx);

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

            fragment.x := i;
            fragment.y := j;
            fragment.interpolationVerticeA := w0;
            fragment.interpolationVerticeB := w1;
            fragment.interpolationVerticeC := w2;
            fragment.sourceSurfaceBits := bits;
            fragment.process;

            if fragment.processedColor.AlphaChannel>0 then
              bits^ := fragment.processedColor.Color;
          end;
          inc(bits);
        end;
      end;
    end;


begin
  assert(Assigned(fragment));
  Assert(BackBuffer.width = Dest.width);
  Assert(BackBuffer.height = Dest.height);

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

  w := Dest.width;
  h := Dest.height;

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

  fragment.sourceSurface := Dest;
  fragment.resetColor;
  fragment.VerticeA := @v[0];
  fragment.VerticeB := @v[1];
  fragment.VerticeC := @v[2];
  fragment.zProc := 0;

  case options of
    TSoftwareRasterizeOption.robasic: BasicRaster;
    TSoftwareRasterizeOption.roBackBuffer: BackBufferRaster;
  end;
end;



Initialization

BackBuffer :=  TPixel32.create;
DepthBuffer :=  TPixel32.create;
BackBufferCol := $00000000;

Finalization

FreeAndNil(BackBuffer);
FreeAndNil(DepthBuffer);

end.
