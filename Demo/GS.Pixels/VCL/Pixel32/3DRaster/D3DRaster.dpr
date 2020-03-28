program D3DRaster;

uses
  Vcl.Forms,
  D3DRaster.fmain in 'D3DRaster.fmain.pas' {Form1};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := false;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
