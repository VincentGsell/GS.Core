program ProjectMVVM;

uses
  Vcl.Forms,
  fmain in 'View\fmain.pas' {Form1},
  dmData in 'Model\dmData.pas' {DataModule1: TDataModule},
  GS.MVVM in '..\..\..\GS.MVVM.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
