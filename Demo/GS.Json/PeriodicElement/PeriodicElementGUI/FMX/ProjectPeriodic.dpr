program ProjectPeriodic;



uses
  System.StartUpCopy,
  FMX.Forms,
  fmain in 'fmain.pas' {Form1},
  unFMXElementObject in 'unFMXElementObject.pas',
  unElementobject in '..\..\data\unElementobject.pas',
  Jsons in '..\..\..\..\..\ThirdPart\Json4Delphi\src\Jsons.pas',
  JsonsUtilsEx in '..\..\..\..\..\ThirdPart\Json4Delphi\src\JsonsUtilsEx.pas',
  GS.JSON in '..\..\..\..\..\GS.JSON.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
