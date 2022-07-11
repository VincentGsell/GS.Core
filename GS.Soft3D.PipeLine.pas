unit GS.Soft3D.PipeLine;

interface


uses Classes,
     SysUtils,
     GS.Common.Log,
     GS.Common.Monitoring,
     GS.Soft3D.Types,
     GS.Soft3D.PipeLine.Types,
     GS.Soft3D.PipeLine.VertexShader,
     GS.Soft3D.PipeLine.Tesselation,
     GS.Soft3D.PipeLine.GeometryShader,
     GS.Soft3D.PipeLine.Raster,
     GS.Soft3D.PipeLine.FragmentShader,
     GS.Soft3D.PipeLine.RasterOperation;

Type

  TS3DPipeline = class(TS3DObject)
  protected
    fInput : TS3DInputData3D;
    fWork : TS3DPipeLineData;

    fTessCtrl : TS3DTesselletionControl;
    fVertexCtrl : TS3DVertexShaderControl;
    fGeomCtrl : TS3DGeometryShaderControler;
    fRasterCtrl : TS3DRasterAndInterpolationControl;
    fFragmenCtrl : TS3DFragmentShaderControl;
    fRasterOpCtrl : TS3DRasterOperation;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    Procedure Process; virtual;

    function QueryingBuffer(x,y : Uint32; out objIndex, FaceIndex : Uint32) : Boolean;

    property InputData : TS3DInputData3D read fInput;
    property WorkData : TS3DPipeLineData read fWork;

    property VertexControl : TS3DVertexShaderControl read fVertexCtrl;
    property TesselletionControl : TS3DTesselletionControl read fTessCtrl;
    property RasterControl : TS3DRasterAndInterpolationControl read fRasterCtrl;
    property FragmentControl : TS3DFragmentShaderControl read fFragmenCtrl;
    property RasterOperation : TS3DRasterOperation read fRasterOpCtrl write fRasterOpCtrl;
  end;



implementation


{ TS3DPipeline }

constructor TS3DPipeline.Create;
begin
  inherited create;
  fInput := TS3DInputData3D.Create;
  fWork := TS3DPipeLineData.Create;

  fVertexCtrl := TS3DVertexShaderControl.Create(self,fInput,fWork);
  fTessCtrl := TS3DTesselletionControl.create(self,fInput,fWork);
  fGeomCtrl := TS3DGeometryShaderControler.Create(self,fInput,fWork);
  fRasterCtrl := TS3DRasterAndInterpolationControl.Create(self,fInput,fWork);
  fFragmenCtrl := TS3DFragmentShaderControl.Create(self,fInput,fWork);
  fRasterOpCtrl := Nil;
end;

destructor TS3DPipeline.Destroy;
begin
  FreeAndNil(fInput);
  FreeAndNil(fWork);
  FreeAndNil(fVertexCtrl);
  FreeAndNil(fTessCtrl);
  FreeAndNil(fGeomCtrl);
  FreeAndNil(fRasterCtrl);
  FreeAndNil(fFragmenCtrl);
  FreeAndNil(fRasterOpCtrl);
  inherited;
end;

procedure TS3DPipeline.Process;
begin
  Assert(Assigned(fRasterOpCtrl),'TS3DPipeline.Process : RasterOperation controler not assigned.');
  { TODO 1 -oVGS -cmetrics : error ctrl, timing each steps. Info logs. }
//  TMonitoring.enter('process');
  try

    InputData.MatrixProcess;

    if fVertexCtrl.Run then
    if fTessCtrl.Run then
    if fGeomCtrl.Run then
    if fRasterCtrl.Run then
    if fFragmenCtrl.Run then
    if fRasterOpCtrl.Run then
    begin
    end;

  finally
//    TMonitoring.exit('process');
  end;
end;

function TS3DPipeline.QueryingBuffer(x, y: Uint32; out objIndex,
  FaceIndex: Uint32): Boolean;
begin
  result := RasterControl.QueryingBuffer(x,y,objIndex,FaceIndex);
end;

end.
