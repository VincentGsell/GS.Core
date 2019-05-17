unit unFMXElementObject;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layers3D, FMX.Controls3D, FMX.Objects3D;

type

TFMXElementRepresentation = class(TLayer3d)
private
Protected
  FL: TLabel;
  FTitle : TLabel;
  FData: TObject;
  Procedure Setup;
Public
  constructor Create(AOwner: TComponent); override;

  Property SymbolLabel : TLabel Read FL;
  Property SymbolName : TLabel Read FTitle;
  property Data : TObject read FData Write FData; //Pointer;
end;


implementation

{ TFMXElementRepresentation }

constructor TFMXElementRepresentation.Create(AOwner: TComponent);
begin
  inherited;
  Position.X := 0.0;
  Position.Y := 0.0;
  Position.Z := -1.0;

//  Fill.Kind := TBrushKind.Gradient;
//  Fill.Gradient.Color1 := TAlphaColorRec.Red;
//  Fill.Gradient.Color := TAlphaColorRec.null;
//  Fill.Gradient.Style := TGradientStyle.Radial;

  Fill.Kind := TBrushKind.Solid;
  Fill.Color := TAlphaColorRec.Null;

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
  FL.Position.X := 0.0;
  FL.Position.Y := 0.0;
  FL.Size.Width := 160.0;
  FL.Size.Height := 80.0;
  FL.TextSettings.Font.Family := 'Arial';
  FL.TextSettings.Font.Style := [TFontStyle.fsBold];
  FL.TextSettings.Font.Size := 60.0;
  FL.TextSettings.HorzAlign := TTextAlign.Center;
  FL.TextSettings.VertAlign := TTextAlign.Center;
  FL.Text := 'X';

  FL.StyledSettings := [TStyledSetting.FontColor];
  FL.Size.Width := 160.000000000000000000;
  FL.Size.Height := 81.000000000000000000;
  FL.Size.PlatformDefault := False;
  FL.TextSettings.Font.Family := 'Arial';
  FL.TextSettings.Font.Size := 80.000000000000000000;
//  FL.TextSettings.Font.StyleExt := {00070000000000000004000000};
  FL.TextSettings.HorzAlign := TTextAlign.Center;
  FL.TextSettings.VertAlign := TTextAlign.Center;
  FL.Text := 'X';

//  FL.OnMouseMove = HMouseMove


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
