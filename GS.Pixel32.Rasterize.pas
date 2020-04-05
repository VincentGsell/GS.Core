
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
 GS.Pixel32, sysutils;

 type
 TVector3 = record
  x, y, z: Single;
 end;
 TVector3i = record
  x, y, z: integer;
 end;
 TPoint2 = record
  x, y: Single;
 end;
 TPoint2i = record
  x, y: integer;
 end;

//---------------------------------------------------------------------------
function Vector3(x, y, z: Single): TVector3;
function Point2(x, y: Single): TPoint2;
function Vector3i(x, y, z: integer): TVector3i;
function Point2i(x, y: integer): TPoint2i;

procedure triangleRasterizeTexMap(Dest : TPixel32; Shader : TCustomPixelChHeShader; const v0, v1, v2: TVector3i;
 const uv0, uv1, uv2: TPoint2i);
procedure triangleRasterizeFlat( Dest : TPixel32;
                   Shader : TPixel32ColorShader;
                   const v0, v1, v2: TVector3i);

//---------------------------------------------------------------------------
implementation

uses math;


function Vector3(x, y, z: Single): TVector3;
begin
 Result.x:= x;
 Result.y:= y;
 Result.z:= z;
end;

function Vector3i(x, y, z: integer): TVector3i;
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

function Point2i(x, y: integer): TPoint2i;
begin
 Result.x:= x;
 Result.y:= y;
end;


function InternaltriangleRasterizeFlat( Dest : TPixel32;
                   Shader : TPixel32ColorShader;
                   const v0, v1, v2: TVector3i) : boolean;
var
  i,j,x,y:integer;
  minx,miny,maxx,maxy:integer;
  ax,ay,bx,by,cx,cy,diviseur:integer;
  temp : integer;
begin
  result := false;
  assert(assigned(Dest));
  assert(assigned(Shader));

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
    for i:=minx to maxx do
    begin
      if (x*ay-y*ax<=0) and (x*by-y*bx>=0)  and ((i-v1.x)*cy-(j-v1.y)*cx<=0) then
      begin
        dest.pixel(i,j);
      end;
      inc(x);
    end;
    inc(y);
  end;
  result := true;
end;

procedure triangleRasterizeFlat( Dest : TPixel32;
                   Shader : TPixel32ColorShader;
                   const v0, v1, v2: TVector3i);
begin
  if not(InternaltriangleRasterizeFlat(Dest,Shader,v0,v1,v2)) then
    InternaltriangleRasterizeFlat(Dest,Shader,v2,v1,v0);
end;


function internalTriangleRasterizeTexMap( Dest : TPixel32;
                   Shader : TCustomPixelChHeShader;
                   const v0, v1, v2: TVector3i;
                   const uv0, uv1, uv2: TPoint2i) : boolean;
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
  assert(assigned(Shader));

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

  au:=uv1.x-uv0.x;
  bu:=uv2.x-uv0.x;
  av:=uv1.y-uv0.y;
  bv:=uv2.y-uv0.y;

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
        u:=au*ux+bu*uy+uv0.x;
        v:=av*ux+bv*uy+uv0.y;
        ll := TCustomPixelChHeShader(shader).Texture.getSurfaceScanLinePtr(round(v));
        //l[i]:=ll[round(u)]; //Fast : Direct memory access.

        //Shader methods : Much slower but very flexible in code.
        Shader.ColorData := TP32Rec(longword(ll[round(u)]));
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
                   Shader : TCustomPixelChHeShader;
                   const v0, v1, v2: TVector3i;
                   const uv0, uv1, uv2: TPoint2i);
begin
  if not(internalTriangleRasterizeTexMap(Dest,Shader,v0,v1,v2,uv0,uv1,uv2)) then
    internalTriangleRasterizeTexMap(Dest,Shader,v2,v1,v0,uv2,uv1,uv0);
end;



end.
