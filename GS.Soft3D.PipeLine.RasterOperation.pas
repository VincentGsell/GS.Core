unit GS.Soft3D.PipeLine.RasterOperation;

interface


uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.FragmentShader;
Type

  TS3DRasterOperation = class(TS3DPipeLineStep)
  public
    FragShaderData : TS3DFragmentShaderControl;

    //Must be implemented into surface enabled techno descendant (such as Pixel32)
    procedure BuildImage; virtual; abstract;

    procedure Run; override;
  end;


implementation

{ TS3DRasterOperation }

procedure TS3DRasterOperation.Run;
begin
  Assert(assigned(FragShaderData));
  InputData := FragShaderData.InputData;
  BuildImage;
end;

end.
