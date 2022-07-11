program otptest;

uses
  System.StartUpCopy,
  FMX.Forms,
  otptest.fmain in 'otptest.fmain.pas' {Form42};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm42, Form42);
  Application.Run;
end.
