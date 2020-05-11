unit GS.Soft3D.PipeLine.Types;

interface

Uses Classes,
     SysUtils,
     contNrs,

     GS.Geometry,
     GS.Soft3D.Types;

Type
  TS3DObject = Class
  End;

  TS3DMeshList = Class(TObjectList)
  private
    function GetMesh(Index: Uint32): TMesh3D;
    procedure SetMesh(Index: Uint32; const Value: TMesh3D);
  public
    procedure AddMesh(mesh : TMesh3D);
    property Mesh[Index : Uint32] : TMesh3D read GetMesh write SetMesh; default;
  End;


  TS3DInputData3D = class(TS3DObject)
  public
    Meshes : TS3DMeshList;

    Resolution : TPoint2Di;

    ProjectionMatrix : Mat4;
    ViewMatrix : Mat4;
    CameraMatrix : Mat4;
    CameraPos : Vec3;
    Projection : TS3dProjectionType;

    procedure MatrixProcess;
  end;

  TS3DPipeLineStep = Class(TS3DObject)
  protected
  public
    InputData : TS3DInputData3D;
    procedure Run; virtual; abstract;
  End;


implementation

{ TS3DMeshList }

procedure TS3DMeshList.AddMesh(mesh: TMesh3D);
begin
  if IndexOf(mesh)=-1 then
    Add(mesh);
end;

function TS3DMeshList.GetMesh(Index: Uint32): TMesh3D;
begin
  result := TMesh3D(Items[Index]);
end;

procedure TS3DMeshList.SetMesh(Index: Uint32; const Value: TMesh3D);
begin
  Items[Index] := Value;
end;


{ TS3DInputData3D }

procedure TS3DInputData3D.MatrixProcess;
var aspectRatio, ZFar, ZNear, FOV : single;
    i : integer;
    l : TMesh3D;
    objMat, objTransMatrix : Mat4;
    objRotX, objRotY, objRotZ : Mat4;
    finalMat : Mat4;
begin

  CameraMatrix := mat4Identity;
  ProjectionMatrix := mat4Identity;

  CameraMatrix := mat4CreateLookAtDirLH(vec3.create(CameraPos.x,CameraPos.y,CameraPos.z),vec3.create(0,0,1),vec3.create(0,1,0));

  Case Projection of
    TS3dProjectionType.Perspective :
    begin
      AspectRatio := Resolution.width/Resolution.height;
      ZFar := 1000;
      ZNear := 0;
      FOV := _radians(90.0);

      ProjectionMatrix :=
        mat4CreatePerspectiveFovLH(
            FOV,
            AspectRatio,ZNear,ZFar);
    end;
    TS3DProjectionType.OrthographicIsometric :
    begin
      //ORTHOGRAPHIC PROJECTION.
      // 45° rotation on Y
      ProjectionMatrix :=
//          Mat4CreateRotationY(-45*PI/180)
      // 20° rotation on X
//        * Mat4CreateRotationX(-20*PI/180)
      // move the scene to the back to avoid Z Clipping
//        Mat4CreateTranslation(vec3.Create(0, 0, -500))
      // create an iometric projection
          Mat4CreateOrthoOffCenterRH(
            -Resolution.width, -Resolution.height,
            Resolution.width, Resolution.height,
            0, 1000
          );
    end;
  End;

  //View
  ViewMatrix := CameraMatrix * ProjectionMatrix;


// Rotate all view.
//  FViewMatrix := mat4CreateRotationY(DegToRad(GTest)) * FViewMatrix;


//  TargetCanvas.beginDraw;
  for i := 0 to Meshes.Count-1 do
  begin
    l := TMesh3D(Meshes[i]);

    //Object.
    objTransMatrix := mat4CreateTranslation(vec3.create(l.x,l.y,l.z));
    objRotX := mat4CreateRotationX(_radians(l.rx));
    objRotY := mat4CreateRotationY(_radians(l.ry));
    objRotZ := mat4CreateRotationZ(_radians(l.rz));

    objMat := objRotX;
    objMat := objMat * mat4CreateTranslation(vec3.create(0,0,0));
    objMat := objMat * objRotY;
    objMat := objMat * mat4CreateTranslation(vec3.create(0,0,0));
    objMat := objMat * objRotZ;
    objMat := objMat * mat4CreateTranslation(vec3.create(0,0,0));
    objMat := objMat * objTransMatrix;

    //Projection.
    finalMat := objMat * ViewMatrix;

  end;

end;

end.
