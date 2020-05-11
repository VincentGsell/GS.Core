program ProjectRasterPipeLine;

uses
  Vcl.Forms,
  fmain.RasterPipeLine in 'fmain.RasterPipeLine.pas' {Form7},
  GS.Soft3D.MeshTools in '..\..\..\..\..\GS.Soft3D.MeshTools.pas',
  GS.Soft3D in '..\..\..\..\..\GS.Soft3D.pas',
  GS.Soft3D.PipeLine.FragmentShader in '..\..\..\..\..\GS.Soft3D.PipeLine.FragmentShader.pas',
  GS.Soft3D.PipeLine.GeometryShader in '..\..\..\..\..\GS.Soft3D.PipeLine.GeometryShader.pas',
  GS.Soft3D.PipeLine in '..\..\..\..\..\GS.Soft3D.PipeLine.pas',
  GS.Soft3D.PipeLine.Raster in '..\..\..\..\..\GS.Soft3D.PipeLine.Raster.pas',
  GS.Soft3D.PipeLine.RasterOperation in '..\..\..\..\..\GS.Soft3D.PipeLine.RasterOperation.pas',
  GS.Soft3D.PipeLine.RasterOperation.Pixel32 in '..\..\..\..\..\GS.Soft3D.PipeLine.RasterOperation.Pixel32.pas',
  GS.Soft3D.PipeLine.Tesselation in '..\..\..\..\..\GS.Soft3D.PipeLine.Tesselation.pas',
  GS.Soft3D.PipeLine.Types in '..\..\..\..\..\GS.Soft3D.PipeLine.Types.pas',
  GS.Soft3D.PipeLine.VertexShader in '..\..\..\..\..\GS.Soft3D.PipeLine.VertexShader.pas',
  GS.Soft3D.Types in '..\..\..\..\..\GS.Soft3D.Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.Run;
end.
