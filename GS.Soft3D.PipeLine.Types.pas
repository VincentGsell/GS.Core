unit GS.Soft3D.PipeLine.Types;

interface

Uses Classes,
     SysUtils,
     contNrs,

     GS.Common.Monitoring,
     GS.Geometry,
     GS.Soft3D.Types,
     GS.Soft3D.PipeLine.Types.InternalFormat;

Type
  TS3DPipeLineObject = Class
  End;

  TS3DInputData3D = class(TS3DPipeLineObject)
  public
    Meshes : TS3DMeshList;
    Objects : TS3DObjectList;

    Resolution : TPoint2Di;

    ProjectionMatrix : Mat4;
    ViewMatrix : Mat4;
    CameraMatrix : Mat4;
    CameraPos : Vec3;
    CameraRot : Vec3;
    Projection : TS3dProjectionType;

    procedure MatrixProcess;

    Constructor Create;
  end;

  TS3DPipeLineData = class(TS3DPipeLineObject)
  public
    Transformed : TS3PLObjectList;
    Constructor Create; virtual;
    Destructor Destroy; Override;
  end;

  TS3DPipeLineStep = Class(TS3DPipeLineObject)
  private
  protected
  public
    PipelineMaster : TObject;        //Pointer on owner.
    InputData : TS3DInputData3D;     //Pointer
    WorkingData : TS3DPipeLineData;  //Pointer
    function Run :  Boolean; virtual; abstract;

    Constructor Create(Owner : TObject; _in : TS3DInputData3D; _wo : TS3DPipeLineData); reintroduce; virtual;
  End;


implementation

uses GS.Soft3d.PipeLine;

{ TS3DInputData3D }

constructor TS3DInputData3D.Create;
begin
  inherited;
  Meshes := nil;
  Objects := nil;
  Resolution.width := 512;
  Resolution.height := 512;
  Projection := TS3DProjectionType.Perspective;
end;

procedure TS3DInputData3D.MatrixProcess;
var aspectRatio, ZFar, ZNear, FOV : single;
    Rot : Mat4;
begin
//  TMonitoring.enter('TS3DInputData3D.MatrixProcess');
  try

  CameraMatrix := mat4Identity;
  ProjectionMatrix := mat4Identity;
  Rot := Mat4Identity;


  CameraMatrix := mat4CreateLookAtDirLH(vec3.create(CameraPos.x,CameraPos.y,CameraPos.z),vec3.create(0,0,1),vec3.create(0,1,0));

  Rot := Rot * Mat4CreateRotationX(CameraRot.x);
  Rot := Rot * Mat4CreateRotationY(CameraRot.y);
  Rot := Rot * Mat4CreateRotationZ(CameraRot.z);

  CameraMatrix := CameraMatrix * Rot;

  Case Projection of
    TS3dProjectionType.Perspective :
    begin
      AspectRatio := Resolution.width/Resolution.height;
      ZFar := 1000;
      ZNear := 0;
      FOV := _radians(45.0);

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
//          Mat4CreateRotationY(-45*PI/180) *
      // 20° rotation on X
//          Mat4CreateRotationX(-20*PI/180) *
      // move the scene to the back to avoid Z Clipping
//        Mat4CreateTranslation(vec3.Create(0, 0, -5)) *
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

  finally
//    TMonitoring.exit('TS3DInputData3D.MatrixProcess');
  end;
end;

{ TS3DPipeLineStep }

constructor TS3DPipeLineStep.Create(Owner: TObject; _in: TS3DInputData3D;
  _wo: TS3DPipeLineData);
begin
  Inherited Create;
  assert(Assigned(Owner));
  assert(Owner is TS3DPipeline);
  assert(Assigned(_in));
  Assert(Assigned(_wo));
  InputData := _in;
  WorkingData := _wo;
  PipelineMaster := Owner;
end;

{ TS3DPipeLineData }

constructor TS3DPipeLineData.Create;
begin
  Transformed := TS3PLObjectList.Create;
end;

destructor TS3DPipeLineData.Destroy;
begin
  FreeAndNil(Transformed);
  inherited;
end;

end.
