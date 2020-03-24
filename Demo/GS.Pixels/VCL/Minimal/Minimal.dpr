program Minimal;

uses
  Vcl.Forms,
  Minimal.fmain in 'Minimal.fmain.pas' {Form1},
  GS.Pixel in '..\..\..\..\GS.Pixel.pas',
  GS.Pixel32.VCL in '..\..\..\..\GS.Pixel32.VCL.pas',
  GS.Pixel32 in '..\..\..\..\GS.Pixel32.pas';

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
