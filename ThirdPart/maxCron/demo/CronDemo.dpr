program CronDemo;

uses
  Vcl.Forms,
  CronDemoMainForm in 'CronDemoMainForm.pas' {Form2},
  maxCron in '..\maxCron.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
