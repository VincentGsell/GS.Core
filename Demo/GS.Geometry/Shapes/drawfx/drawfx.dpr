program drawfx;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {Form1},
  GS.Drawing.Shape in 'GS.Drawing.Shape.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
