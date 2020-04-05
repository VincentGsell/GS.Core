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
 Unit Name : GS.Pixel
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic Pixel class introduced by interface.
 Date:     : 2020031
 History   :
 20200301 - Creating unit.

Credits :

Description :
-----------------------------------------------------------------------------}
{$I GSCore.Inc}

unit GS.Pixel;


interface

Uses Classes, SysUtils;


Type
///
///  Interface for the "real" graphic memory surface. See GS.Pixel32 for a full impl. exemple.
///
iPixSurface = interface;

///
///  The base access to the pixel is happening throught pixel's shader.
///  Even if it is a very slow methods, this implementation allow very clear
///  access (in a code point of view) to the pixel.
///
iPixShader = interface;

///
///  This interface allow to use others graphics lib, such as hundreds available
///  on the net. Pixel designed to be platform agnostic, but service should be
///  platform dedicated. If you need performance, or if you want to not adapt
///  or rewrite existing proven code, service is the way.
///
iPixService = interface;

///
///  All full effect on surface (blur, disolve, matrix will pass throught this
///  interface.
///
iPixSurfaceEffect = interface;

//Using graphics stuff, friend class, fine memory tuning needed :
//Desactivate attached memory management.
TPixel32InterfacedObject = class(TInterfacedObject)
public
  function _AddRef: Integer; stdcall;
  function _Release: Integer; stdcall;
End;

//Pixel shader. (per pixel work)
iPixShader = interface
  procedure init(surface : iPixSurface);
  procedure process;
end;

//Whole surface, use shader in a more direct way.
iPixSurfaceEffect = interface
  procedure init(surface : iPixSurface; shader : iPixShader);
  procedure process;
end;

iPixSurface = interface
  function getSurfacePtr : pointer;
  function getSurfaceScanLinePtr(LineIndex : Int32) : pointer;
  procedure resize(width,height : Int32);
  procedure copyTo(targetPixel32 : iPixSurface);
  function isEmpty : boolean;

  procedure moveTo(const x,y : Int32; const z : Int32 = 0);
  procedure lineTo(const x,y : Int32; const z : Int32 = 0);
  procedure pixel(const x,y : Int32; const z : Int32 = 0);

  procedure rasterize(x,y,x1,y1,x2,y2 : Int32);

//  procedure invoke(service : iPixService);

  procedure clear;

  function width : uInt32;
  function height : uInt32;
end;

iPixService = interface
  function uri : string;
  function id : string;
  procedure Ask(param : TStream);
  procedure Answer(content : TStream; success : boolean; report : TStream);
end;

///
///
///
///  F R O N T  E N D
///
///

TPixel = class
//  procedure ActiveBackend(backName : string);
end;

implementation

{ TPixel32InterfacedObject }

function TPixel32InterfacedObject._AddRef: Integer;
begin
  result := -1;
end;

function TPixel32InterfacedObject._Release: Integer;
begin
  result := -1;
end;



end.
