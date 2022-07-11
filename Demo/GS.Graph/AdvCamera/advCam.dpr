program advCam;

uses
  System.StartUpCopy,
  FMX.Forms,
  advcam.fmain in 'advcam.fmain.pas' {Form42};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm42, Form42);
  Application.Run;
end.
