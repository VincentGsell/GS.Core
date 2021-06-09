unit GS.Soft3D.View;

interface

uses SysUtils,
     classes,
     contNrs,
     Math,
     GS.Geometry,
     GS.Soft3D,
     GS.Soft3D.Types,
     GS.Soft3D.PipeLine;

Type
  TView3d = class
  private
    FWireframe: boolean;
    Frasterframe: boolean;

    procedure SetProjection(const Value: TS3DProjectionType);
    function GetProjection: TS3DProjectionType;
    function GetCamZ: single;
    procedure SetCamZ(const Value: single);
  public
    PipeLine : TS3DPipeline;

    procedure ResizeBuffers(w,h : Uint32);

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Execute;

    procedure SetCamPos(X,Y,Z : single);
    procedure SetCamRotate(X,Y,Z : single);

    Property CameraZ : single read GetCamZ Write SetCamZ;
//    Property CameraX : single read FCamX Write FCamX;
//    Property CameraY : single read FCamY Write FCamY;

    property rasterFrame : boolean read Frasterframe write fRasterframe;
    property wireFrame : boolean read FWireframe write FWireframe;

    property Projection : TS3DProjectionType read GetProjection write SetProjection;

  end;

implementation

{ TView3d }

constructor TView3d.Create;
begin
  PipeLine := TS3DPipeline.Create; //build all the 3D pipeline.

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

procedure TView3d.SetCamPos(X, Y, Z: single);
begin
  PipeLine.InputData.CameraPos.X := X;
  PipeLine.InputData.CameraPos.Y := Y;
  PipeLine.InputData.CameraPos.Z := Z;
end;

procedure TView3d.SetCamRotate(X, Y, Z: single);
begin
  PipeLine.InputData.CameraRot.X := X;
  PipeLine.InputData.CameraRot.Y := Y;
  PipeLine.InputData.CameraRot.Z := Z;
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


procedure TView3d.ResizeBuffers(w, h: Uint32);
begin
  PipeLine.InputData.Resolution.width := w;
  PipeLine.InputData.Resolution.height := h;
  PipeLine.RasterOperation.Resize(w,h);
  PipeLine.RasterControl.BackMem.resize(w,h);
end;

end.
