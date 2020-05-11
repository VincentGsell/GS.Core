unit GS.Soft3D.PipeLine;

interface


uses Classes,
     SysUtils,
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
    fVertexCtrl : TS3DVertexShaderControl;
    fTessCtrl : TS3DTesselletionControl;
    fGeomCtrl : TS3DGeometryShaderControler;
    fRasterCtrl : TS3DRasterAndInterpolationControl;
    fFragmenCtrl : TS3DFragmentShaderControl;
    fRasterOpCtrl : TS3DRasterOperation;
  public
    Constructor Create(rasterOpInstance : TS3DRasterOperation); virtual;
    Destructor Destroy; override;

    Procedure Process; virtual;

    property InputData : TS3DInputData3D read fInput;
  end;



implementation


{ TS3DPipeline }

constructor TS3DPipeline.Create(rasterOpInstance : TS3DRasterOperation);
begin
  inherited create;
  assert(assigned(rasterOpInstance));
  fInput := TS3DInputData3D.Create;
  fVertexCtrl := TS3DVertexShaderControl.Create(fInput);
  fTessCtrl := TS3DTesselletionControl.create(fVertexCtrl);
  fGeomCtrl := TS3DGeometryShaderControler.Create(fTessCtrl);
  fRasterCtrl := TS3DRasterAndInterpolationControl.Create(fGeomCtrl, fInput.Resolution.width,fInput.Resolution.height);
  fFragmenCtrl := TS3DFragmentShaderControl.Create(fRasterCtrl);
  fRasterOpCtrl := rasterOpInstance;
  rasterOpInstance.FragShaderData := fFragmenCtrl;
end;

destructor TS3DPipeline.Destroy;
begin
  FreeAndNil(fInput);
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
  { TODO 1 -oVGS -cmetrics : error ctrl, timing each steps. Info logs. }
  InputData.MatrixProcess;

  fVertexCtrl.Run;
  fTessCtrl.Run;
  fGeomCtrl.Run;
  fRasterCtrl.Run;
  fFragmenCtrl.Run;
  fRasterOpCtrl.Run;
end;

end.
