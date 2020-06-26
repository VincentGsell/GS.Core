program ServiceStandAlone;

uses
  Vcl.Forms,
  serviceStandAlone.fmain in 'serviceStandAlone.fmain.pas' {Form5},
  GS.Pixel32.Service.Image32.Types in '..\..\..\..\..\..\GS.Pixel32.Service.Image32.Types.pas',
  GS.Pixel32.Service.Image32.Backend in '..\..\..\..\..\..\GS.Pixel32.Service.Image32.Backend.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm5, Form5);
  Application.Run;
end.
