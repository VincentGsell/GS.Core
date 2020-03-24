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
 Purpose   : basic Pxel class for texturemapping. (fast, easy to use/anderstand)
 Date:     : 20200310
 History   :
 20200301 - Creating unit.

Credits :
 - Paul Toth's Execute.Pixels "Flat" code reuse

Description :
-----------------------------------------------------------------------------}
unit GS.Pixel32.TexMapBase;

interface

uses
 sysutils, Math,
 GS.Pixel32;


procedure triangleRasterize(surface : TPixel32; const a, b, c: TP32Vertex);


implementation

procedure triangleRasterize(surface : TPixel32; const a, b, c: TP32Vertex);

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
    if y>(surface.height-1) then
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
      if (x>-1) and (x<surface.width) then
        surface.pixel(x, y, z);
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


end.
