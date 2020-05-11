unit GS.Soft3D.PipeLine.FragmentShader;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.Raster;

Type

  TS3DFragmentShaderControl = class(TS3DPipeLineStep)
    RasterData : TS3DRasterAndInterpolationControl;
    Constructor Create(rasterStep : TS3DRasterAndInterpolationControl); reintroduce;
    procedure Run; Override;
  end;
    TS3DFragmentShader = class(TS3DObject)
      Controler : TS3DFragmentShaderControl;
    end;
    TS3DFragmentShader_Color = class(TS3DFragmentShader)
    end;


implementation


{ TS3DFragmentShaderControl }

constructor TS3DFragmentShaderControl.Create(
  rasterStep: TS3DRasterAndInterpolationControl);
begin
  inherited Create;
  Assert(assigned(rasterStep));
  InputData := rasterStep.InputData;
  RasterData :=  rasterStep;
end;

procedure TS3DFragmentShaderControl.Run;
begin
  inherited;

end;

end.
