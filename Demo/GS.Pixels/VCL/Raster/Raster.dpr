program Raster;

uses
  Vcl.Forms,
  Raster.fmain in 'Raster.fmain.pas' {Form2},
  GS.Pixel32.Effect.Generator in '..\..\..\..\GS.Pixel32.Effect.Generator.pas',
  GS.Pixel32.Effect.Generator.Gradient in '..\..\..\..\GS.Pixel32.Effect.Generator.Gradient.pas',
  GS.Pixel32.Rasterize in '..\..\..\..\GS.Pixel32.Rasterize.pas',
  GS.Pixel32.Draw in '..\..\..\..\GS.Pixel32.Draw.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
