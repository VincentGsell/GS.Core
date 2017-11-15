program TaskMessageVCL;

uses
  Vcl.Forms,
  TaskMessage.fmain in 'TaskMessage.fmain.pas' {Form3};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  ReportMemoryLeaksOnShutdown := True;
  Application.Run;
end.
