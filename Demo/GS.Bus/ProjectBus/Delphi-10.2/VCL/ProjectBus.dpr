program ProjectBus;

uses
  Vcl.Forms,
  fmainBus in 'fmainBus.pas' {Form2};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
