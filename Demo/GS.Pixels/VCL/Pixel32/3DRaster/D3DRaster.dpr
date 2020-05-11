program D3DRaster;

uses
  Vcl.Forms,
  D3DRaster.fmain in 'D3DRaster.fmain.pas' {Form1},
  GS.SoftwareRaster3D.ExempleStandAlone in '..\..\..\..\..\GS.SoftwareRaster3D.ExempleStandAlone.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
