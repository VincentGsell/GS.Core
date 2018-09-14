unit unFMXElementObject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layers3D, FMX.Controls3D, FMX.Objects3D;

type

TFMXElementRepresentation = class(TLayer3d)
Protected
  FL: TLabel;
  FTitle : TLabel;
Public
  constructor Create(AOwner: TComponent); override;
  Procedure Setup;

  Property SymbolLabel : TLabel Read FL;
  Property SymbolName : TLabel Read FTitle;
end;


implementation

{ TFMXElementRepresentation }

constructor TFMXElementRepresentation.Create(AOwner: TComponent);
begin
  inherited;
  Position.X := 0.0;
  Position.Y := 0.0;
  Position.Z := 0.0;
  Width := 3.13;
  Height := 2.35;
  Resolution := 50;
  StyleLookup := 'backgroundstyle';
  Setup;
end;

procedure TFMXElementRepresentation.Setup;
begin
  FL := TLabel.Create(Self);
  FL.Parent := Self;
  FL.Size.PlatformDefault := False;
  FL.Size.Width := 50;
  FL.Size.Height := 50;
  FL.TextSettings.Font.Family := 'Arial';
  FL.TextSettings.Font.Size := 50.0;
  FL.TextSettings.HorzAlign := TTextAlign.Center;
  FL.Position.X := 8.0;
  FL.Position.Y := 8.0;
  FL.Size.Width := 145.0;
  FL.Size.Height := 73.0;
  FL.Text := 'X';

  FTitle := TLabel.Create(Self);
  FTitle.PArent := Self;
  FTitle.Text := 'X';
  FTitle.Size.PlatformDefault := False;
  FTitle.TextSettings.Font.Family := 'Arial';
  FTitle.TextSettings.Font.Size := 50.0;
  FTitle.TextSettings.HorzAlign := TTextAlign.Center;
  FTitle.Position.X := 8.0;
  FTitle.Position.Y := 80.0;
  FTitle.Size.Width := 145.0;
  FTitle.Size.Height := 17.0;
  FTitle.Text := 'Name';
end;

end.
