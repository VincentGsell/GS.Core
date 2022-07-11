program gsSysInfosDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {FormMain},
  GS.System.Infos.Extended in '..\..\GS.System.Infos.Extended.pas',
  GS.System.Infos.Extended.Default in '..\..\GS.System.Infos.Extended.Default.pas',
  GS.System.Infos.Extended.ZDeviceInfo in '..\..\GS.System.Infos.Extended.ZDeviceInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
