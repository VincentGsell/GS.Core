program ProjectPeriodic;



uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {Form1},
  unFMXElementObject in 'unFMXElementObject.pas',
  unElementobject in '..\..\data\unElementobject.pas',
  GS.Json in '..\..\..\..\..\GS.Json.pas',
  Jsons in '..\..\..\..\..\ThirdPart\Json4Delphi\src\Jsons.pas',
  JsonsUtilsEx in '..\..\..\..\..\ThirdPart\Json4Delphi\src\JsonsUtilsEx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
