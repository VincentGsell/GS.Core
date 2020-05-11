program ProjectAssetsMesh;

uses
  Vcl.Forms,
  assetsMesh.fmain in 'assetsMesh.fmain.pas' {Form3};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
