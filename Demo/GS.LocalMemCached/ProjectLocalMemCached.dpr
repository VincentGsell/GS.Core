program ProjectLocalMemCached;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {Form1},
  GS.LocalMemcached in '..\..\GS.LocalMemcached.pas',
  GS.Bus in '..\..\GS.Bus.pas',
  GS.Reference in '..\..\GS.Reference.pas',
  GS.Reference.Persister.SingleFile in '..\..\GS.Reference.Persister.SingleFile.pas',
  GS.Reference.Persister in '..\..\GS.Reference.Persister.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
