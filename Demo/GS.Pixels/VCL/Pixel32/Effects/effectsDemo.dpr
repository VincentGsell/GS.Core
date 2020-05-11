program effectsDemo;

uses
  Vcl.Forms,
  effectsDemo.fmain in 'effectsDemo.fmain.pas' {Form6};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
