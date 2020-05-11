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
     GS.Geometry,
     GS.Soft3D.Types,
     GS.Soft3D.PipeLine,
     GS.Soft3D.PipeLine.RasterOperation.Pixel32;

Type



  TPlane = class(TMesh3d)
  public
    constructor create; override;
  end;

  TCube = class(TMesh3d)
  public
    constructor create; override;
  end;

  TCamera = class(TBase3D)
  public
  end;



  TView3d = class
  private
    FWireframe: boolean;
    Frasterframe: boolean;

    procedure SetProjection(const Value: TS3DProjectionType);
    function GetProjection: TS3DProjectionType;
    function GetCamZ: single;
    procedure SetCamZ(const Value: single);
  public
    Pixel32PipeLine : TS3DRasterOperationPixel32;
    PipeLine : TS3DPipeline;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure addMesh(_mesh : TMesh3d);
    function addCube(x,y,z : single) : TCube;
    function addPlane(x,y,z : single) : TPlane;

    procedure Execute;

    Property CameraZ : single read GetCamZ Write SetCamZ;
//    Property CameraX : single read FCamX Write FCamX;
//    Property CameraY : single read FCamY Write FCamY;

    property rasterFrame : boolean read Frasterframe write fRasterframe;
    property wireFrame : boolean read FWireframe write FWireframe;

    property Projection : TS3DProjectionType read GetProjection write SetProjection;

  end;

implementation




{ TView3d }

function TView3d.addCube(x, y, z :  single) : TCube;
begin
  Result :=  TCube.Create;
  Result.x := x;
  Result.y := y;
  Result.z := z;
  addMesh(Result);
end;

procedure TView3d.addMesh(_mesh: TMesh3d);
begin
  assert(assigned(_mesh));
  PipeLine.InputData.Meshes.AddMesh(_mesh);
end;

function TView3d.addPlane(x, y, z: single): TPlane;
begin
  Result :=  TPlane.Create;
  Result.x := x;
  Result.y := y;
  Result.z := z;
  addMesh(Result);
end;

constructor TView3d.Create;
begin
  Pixel32PipeLine := TS3DRasterOperationPixel32.Create; //Surface dedicated to Soft3D.
  PipeLine := TS3DPipeline.Create(Pixel32PipeLine); //build all the 3D pipeline.

  CameraZ := -15;
  Projection := TS3DProjectionType.Perspective;
  FWireframe := false;
  Frasterframe := true;
end;

destructor TView3d.Destroy;
begin
  freeAndNil(PipeLine);
  inherited;
end;



function CalculateSurfaceNormal(AVecA, AVecB,
  AVecC: vec3) : vec3;
var
  LVecU, LVecV: vec3;
begin
  LVecU := AVecB;
  LVecU := LVecU - AVecA;

  LVecV := AVecC;
  LVecV := LVecV - AVecA;

  Result.X := (LVecU.Y*LVecV.Z) - (LVecU.Z*LVecV.Y);
  Result.Y := (LVecU.Z*LVecV.X) - (LVecU.X*LVecV.Z);
  Result.Z := (LVecU.X*LVecV.Y) - (LVecU.Y*LVecV.X);
end;

procedure TView3d.Execute;
begin
  PipeLine.Process;
end;

procedure TView3d.SetCamZ(const Value: single);
begin
  PipeLine.InputData.CameraPos.z := value;
end;

procedure TView3d.SetProjection(const Value: TS3DProjectionType);
begin
  PipeLine.InputData.Projection := Value;
end;

function TView3d.GetCamZ: single;
begin
  result := PipeLine.InputData.CameraPos.z;
end;

function TView3d.GetProjection: TS3DProjectionType;
begin
  Result := PipeLine.InputData.Projection;
end;

{ TCube }

constructor TCube.create;
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


{ TPlane }

constructor TPlane.create;
begin
  inherited;
  addVertex(-0.5,-0.5,0);
  addVertex(0.5,-0.5,0);
  addVertex(0.5,0.5,0);
  addVertex(-0.5,0.5,0);
  addQuad(0,1,2,3);
end;


end.
