program MiniDelau;

uses
  Vcl.Forms,
  MiniDelau.fmain in 'MiniDelau.fmain.pas' {Form1};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
