program ProjectDuplify;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {Form15},
  GS.Stream in '..\..\..\..\GS.Stream.pas',
  GS.FileOp in '..\..\..\..\GS.FileOp.pas',
  GS.CPUUsage in '..\..\..\..\GS.CPUUsage.pas',
  GS.FileOp.Glacify.Protocol.Default in '..\..\..\..\GS.FileOp.Glacify.Protocol.Default.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm15, Form15);
  Application.Run;
end.
