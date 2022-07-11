program BasicCamera;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {Form42};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm42, Form42);
  Application.Run;
end.
