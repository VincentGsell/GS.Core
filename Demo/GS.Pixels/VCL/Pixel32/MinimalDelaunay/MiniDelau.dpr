program MiniDelau;

uses
  Vcl.Forms,
  MiniDelau.fmain in 'MiniDelau.fmain.pas' {Form1},
  GS.Pixel32.Service.PXL in '..\..\..\..\..\GS.Pixel32.Service.PXL.pas',
  GS.Pixel.Service in '..\..\..\..\..\GS.Pixel.Service.pas';

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
