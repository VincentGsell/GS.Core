program ServiceStandAlone;

uses
  Vcl.Forms,
  serviceStandAlone.fmain in 'serviceStandAlone.fmain.pas' {Form5};

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
