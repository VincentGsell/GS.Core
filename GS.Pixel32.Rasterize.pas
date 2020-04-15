
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
 GS.Pixel, GS.Pixel32, sysutils;


procedure triangleRasterizeTexMap(Dest : TPixel32; const v0, v1, v2: TP32Vertex);
procedure triangleRasterizeFlat( Dest : TPixel32; const v0, v1, v2: TP32Vertex);

//---------------------------------------------------------------------------

//BackBuffer's aiming is to draw correctly triangles batches.
//i.e. when drawing polygone (many triangle) edge and border could be overlap.
//the backe buffer avoid that.
var BackBuffer :  TPixel32;
    BackBuffercol : TP32;

//Must be call before each batch drawing.
procedure rasterBackBufferInit(surface : iPixSurface);

implementation

uses math;


procedure rasterBackBufferInit(surface : iPixSurface);
begin
  if surface.width<>BackBuffer.width then
    BackBuffer.resize(surface.width,surface.height);
  Inc(BackBuffercol);
  BackBuffer.color_pen := BackBuffercol;
end;



function InternaltriangleRasterizeFlat( Dest : TPixel32;
                   const v0, v1, v2: TP32Vertex) : boolean;
var
  i,j,x,y,z:integer;
  minx,miny,maxx,maxy:integer;
  ax,ay,bx,by,cx,cy,diviseur:integer;
  temp : integer;

  bb,cc : pTP32;
begin
  result := false;
  assert(assigned(Dest));

  minx:=max(min(min(v0.x,v1.x),v2.x),0);
  miny:=max(min(min(v0.y,v1.y),v2.y),0);

  maxx:=min(max(max(v0.x,v1.x),v2.x),dest.Width-1);
  maxy:=min(max(max(v0.y,v1.y),v2.y),dest.Height-1);

  if maxx-minx=0 then exit;
  if maxy-miny=0 then exit;

  ax:=v1.x-v0.x;
  bx:=v2.x-v0.x;
  cx:=v2.x-v1.x;
  ay:=v1.y-v0.y;
  by:=v2.y-v0.y;
  cy:=v2.y-v1.y;

  // flat triangle, or reverse
  if ax*by-ay*bx<=0 then exit;
  if ax*cy-ay*cx<=0 then exit;

  diviseur:=ay*bx-ax*by;

  y:=miny-v0.y;

  for j:=miny to maxy do
  begin
    x:=minx-v0.x;
    bb := BackBuffer.getSurfacePtr;
    inc(bb,j*BackBuffer.width);
    inc(bb,minx);

    cc := Dest.getSurfacePtr;
    inc(cc,j*Dest.width);
    inc(cc,minx);

    for i:=minx to maxx do
    begin
      if (x*ay-y*ax<=0) and (x*by-y*bx>=0)  and ((i-v1.x)*cy-(j-v1.y)*cx<=0) then
      begin

        if bb^ <> BackBuffercol then
        begin
//          cc^:= Dest.currentDrawShader.ColorData.Color;
          z := trunc(( v0.z + v1.z + v2.z ) / 3);
          dest.pixel(i,j,z);
        end;

        bb^:= BackBuffercol;
      end;
      inc(x);
      inc(bb);
      inc(cc);
    end;
    inc(y);
  end;
  result := true;
end;

procedure triangleRasterizeFlat( Dest : TPixel32;
                   const v0, v1, v2: TP32Vertex);
begin
  if not(InternaltriangleRasterizeFlat(Dest, v0,v1,v2)) then
    InternaltriangleRasterizeFlat(Dest, v2,v1,v0);
end;


function internalTriangleRasterizeTexMap( Dest : TPixel32;
                   const v0, v1, v2: TP32Vertex) : boolean;
type
 tlongarray=array[0..0] of longint;
 plongarray=^tlongarray;
var
  i,j,x,y:integer;
  minx,miny,maxx,maxy:integer;
  ax,ay,bx,by,cx,cy,au,av,bu,bv,diviseur:integer;
  ux,uy,u,v,dx,dy:single;
  l,ll:plongarray;
begin
  result := false;
  assert(assigned(Dest));
  assert(assigned(Dest.currentDrawShader));
  assert(Dest.currentDrawShader is TPixel32TextureShader);

  minx:=max(min(min(v0.x,v1.x),v2.x),0);
  miny:=max(min(min(v0.y,v1.y),v2.y),0);

  maxx:=min(max(max(v0.x,v1.x),v2.x),dest.Width-1);
  maxy:=min(max(max(v0.y,v1.y),v2.y),dest.Height-1);

  if maxx-minx=0 then exit;
  if maxy-miny=0 then exit;

  ax:=v1.x-v0.x;
  bx:=v2.x-v0.x;
  cx:=v2.x-v1.x;
  ay:=v1.y-v0.y;
  by:=v2.y-v0.y;
  cy:=v2.y-v1.y;

  au:=v1.u-v0.u;
  bu:=v2.u-v0.u;
  av:=v1.v-v0.v;
  bv:=v2.v-v0.v;

//  au:=uv1.x-uv0.x;
//  bu:=uv2.x-uv0.x;
//  av:=uv1.y-uv0.y;
//  bv:=uv2.y-uv0.y;

  // flat triangle, or reverse
  if ax*by-ay*bx<=0 then exit;
  if ax*cy-ay*cx<=0 then exit;

  diviseur:=ay*bx-ax*by;

  dx:=by/diviseur;
  dy:=ay/diviseur;

  y:=miny-v0.y;

  for j:=miny to maxy do
  begin
    l:=dest.getSurfaceScanLinePtr(j);

    x:=minx-v0.x;

    ux:=(y*bx-x*by)/diviseur;
    uy:=(x*ay-y*ax)/diviseur;

    for i:=minx to maxx do
    begin
      if (x*ay-y*ax<=0) and (x*by-y*bx>=0)  and ((i-v1.x)*cy-(j-v1.y)*cx<=0) then
      begin
        u:=au*ux+bu*uy+v0.u;
        v:=av*ux+bv*uy+v0.v;

        ll := TPixel32TextureShader(Dest.currentDrawShader).Texture.getSurfaceScanLinePtr(trunc(v));
//        l[i]:=ll[round(u)]; //Fast : Direct memory access.

        //Shader methods : Much slower but very flexible in code.
        Dest.currentDrawShader.ColorData := TP32Rec(longword(ll[trunc(u)]));
        Dest.pixel(i,j);
      end;
      ux:=ux-dx;
      uy:=uy+dy;

      inc(x);
    end;

    inc(y);
  end;
  result := true;
end;

procedure TriangleRasterizeTexMap( Dest : TPixel32;
                   const v0, v1, v2: TP32Vertex);
begin
  if not(internalTriangleRasterizeTexMap(Dest,v0,v1,v2)) then
    internalTriangleRasterizeTexMap(Dest,v2,v0,v1);
end;

Initialization

BackBuffer :=  TPixel32.create;
BackBufferCol := $00000000;

Finalization

FreeAndNil(BackBuffer);

end.
