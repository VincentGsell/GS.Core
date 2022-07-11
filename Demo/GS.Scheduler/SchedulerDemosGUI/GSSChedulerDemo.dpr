program GSSChedulerDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  schedulerDemo.fmain in 'schedulerDemo.fmain.pas' {fMain},
  GS.System.Scheduler in '..\..\..\GS.System.Scheduler.pas',
  GS.System.Scheduler.CronLike in '..\..\..\GS.System.Scheduler.CronLike.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TfMain, fMain);
  Application.Run;
end.
