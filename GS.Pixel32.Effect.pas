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
 Unit Name : GS.Pixel32.Effect.Generator
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic Pxel class.
 Date:     : 2020031
 History   :
 20200301 - Creating unit.

Credits :

Description :
Pixel32 Generator Class : Such as Gouraud Gradient color, Perlin2d noise, plasma
Aim : One time surface decoration methods, to obtain base graphics for texture, or whatever.
-----------------------------------------------------------------------------}
{$I GSCore.Inc}
unit GS.Pixel32.Effect;

interface

Uses Classes,
     SysUtils,
     Math,
     GS.Pixel,
     GS.Pixel32;

Type

TCustomPixel32Generator = class(TCustomPixel32SurfaceEffect)
private
protected
  function getsurface: TPixel32;
public
  property GeneratorSurface : TPixel32 read getsurface;
end;

implementation

{ TCustomPixel32Generator }

function TCustomPixel32Generator.getsurface: TPixel32;
begin
  result := TPixel32(fsurface);
end;


end.
