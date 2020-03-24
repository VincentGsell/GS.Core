program Raster;

uses
  Vcl.Forms,
  Raster.fmain in 'Raster.fmain.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
