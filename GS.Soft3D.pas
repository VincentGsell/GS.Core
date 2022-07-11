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
 Unit Name : GS.Soft3D
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic 3d model. Abstract from any hardware implementation
 Date:     : 2020031
 History   :
 20200301 - Creating unit.
 20200424 - Change Math vector library (vendor-specific math lib free)

Credits :
https://learnopengl.com/Getting-started/Coordinate-Systems
https://www.scratchapixel.com/lessons/3d-basic-rendering/computing-pixel-coordinates-of-3d-point/mathematics-computing-2d-coordinates-of-3d-points

Description :
- 3D standalone software 3D system. Mainly for learning purpose.
- This unit is volountary kept simple.
- Large optimization is always possible, but I do not want to break the understandability.
- Must be cross platform. No Asm.

-----------------------------------------------------------------------------}
{$I GSCore.Inc}

unit GS.Soft3D;
interface

uses SysUtils,
     classes,
     contNrs,
     Math,
     Gs.Geometry,
     Gs.Geometry.Triangulation,
     Gs.Geometry.Mesh2d,
     GS.Soft3D.Types;

Type
  TGSPlaneMesh = class(TMesh3d)
  public
    constructor create; override;
  end;

  TGSCubeMesh = class(TMesh3d)
  public
    constructor create; override;
  end;

  TGSPlaneTriMesh = class(TMesh3d)
  public
    constructor create; override;
  end;

  TGSSphere = Class(TMesh3d)
  public
    constructor create; override;
  end;


  //----------------------------------------------------------------------------


  TGSObject3D = class(TS3DObject)
  end;


  TGSCamera = class(TBase3D)
  public
  end;





implementation


{ TCube }

constructor TGSCubeMesh.create;
begin
  inherited;
  addVertex(-0.5,-0.5,-0.5); //0
  addVertex(0.5,-0.5,-0.5);  //1
  addVertex(0.5,0.5,-0.5);   //2
  addVertex(-0.5,0.5,-0.5);  //3

  addVertex(-0.5,-0.5,0.5);  //4
  addVertex(0.5,-0.5,0.5);   //5
  addVertex(0.5,0.5,0.5);    //6
  addVertex(-0.5,0.5,0.5);   //7

  addQuad(0,1,2,3);
  addQuad(4,5,6,7);
  addQuad(0,4,7,3);
  addQuad(5,1,2,6);
  addQuad(0,4,5,1);
  addQuad(3,7,6,2);
end;


{ TGSPlaneMesh }

constructor TGSPlaneMesh.create;
begin
  inherited;
  addVertex(-0.5,-0.5,0);
  addVertex(0.5,-0.5,0);
  addVertex(0.5,0.5,0);
  addVertex(-0.5,0.5,0);
  addQuad(0,1,2,3);
end;


{ TGSPlaneTriMesh }

function edgeFunction(var a,b,c : vec2) : TVecType; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := (a.x - c.x) * (b.y - a.y) - (a.y - c.y) * (b.x - a.x);
end;


constructor TGSPlaneTriMesh.create;
var i : Integer;
    cloud : Array of vec2s;
    mesh2d : TGSRawMesh2d;
    a,b,c,d : vec2;
    uva,uvb,uvc : vec2;
    aa,bb,cc : Vec4;
    area : TVecType;


Const
  CST_POINT_COUNT = 100;
begin
  Randomize;
  mesh2d :=TGSRawMesh2D.Create;
  try
    SetLength(cloud,1);
    SetLength(cloud[0],CST_POINT_COUNT);
    cloud[0][0].x := -(CST_POINT_COUNT div 2);
    cloud[0][0].y := 0;
    cloud[0][CST_POINT_COUNT-1].x := (CST_POINT_COUNT div 2);
    cloud[0][CST_POINT_COUNT-1].y := 0;

    for i := 1 to CST_POINT_COUNT-2 do
    begin
      cloud[0][i].x := i - (CST_POINT_COUNT div 2);
      cloud[0][i].y := 5 + Random(10);
    end;

    TGSTriangulationPortal.PolygoneTriangulation(cloud,mesh2d);
    for i := 0 to mesh2d.getTriangleCount-1 do
    begin
      mesh2d.Triangle(i,a,b,c,uva,uvb,uvc,aa,bb,cc);
      addVertex(a.x,a.y,0); //Todo do something with z. (progressive proportinal curve fellow or other).
      addVertex(b.x,b.y,0);
      addVertex(c.x,c.y,0);
      addTriangle(i*3,i*3+1,i*3+2,0,0,0,0,0,0);
    end;

    meshScale(0.1);
  finally
    FreeAndNil(mesh2d);
  end;

end;

{ TGSSphere }

constructor TGSSphere.create;
begin
  inherited;

end;

end.
