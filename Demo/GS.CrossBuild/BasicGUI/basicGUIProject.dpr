program basicGUIProject;

uses
  Vcl.Forms,
  basicGUI.fmain in 'basicGUI.fmain.pas' {Form1},
  GS.CrossBuild in '..\..\..\GS.CrossBuild.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
