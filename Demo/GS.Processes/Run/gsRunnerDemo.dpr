program gsRunnerDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  gsRunner.fmain in 'gsRunner.fmain.pas' {Form42},
  GS.System.Processes in '..\..\..\GS.System.Processes.pas',
  GS.System.Processes.Python in '..\..\..\GS.System.Processes.Python.pas',
  GS.System.Processes.CommandLine in '..\..\..\GS.System.Processes.CommandLine.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm42, Form42);
  Application.Run;
end.
