unit fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layers3D, FMX.Controls3D, FMX.Objects3D,
  unElementobject, unFMXElementObject, System.JSON;
type
  TForm1 = class(TForm3D)
    Grid3D1: TGrid3D;
    Camera1: TCamera;
    Layer3D1: TLayer3D;
    H: TLabel;
    Label1: TLabel;
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MendeleevTable : TPeriodicTable;
  end;

var
  Form1: TForm1;

implementation

Uses GS.JSon;

{$R *.fmx}

procedure TForm1.Form3DCreate(Sender: TObject);
const lf = '..\..\..\..\data\PeriodicTableJSON.json';
var ls : TStringList;
    l : TElement;
    lt : TFMXElementRepresentation;
begin
  Layer3D1.Visible := False;

  MendeleevTable := Nil;
  if FileExists(lf) then
  begin
    ls := TStringList.Create;
    try
      ls.LoadFromFile(lf);
      MendeleevTable :=  TPeriodicTable.Create;
      TGSJson.JsonToObject(ls.Text,TObject(MendeleevTable));
    finally
      FreeAndNil(ls);
    end;
  end;
  Assert(Assigned(MendeleevTable));

  for l in MendeleevTable.Elements do
  begin
    lt := TFMXElementRepresentation.Create(nil);
    AddObject(lt);
    lt.Position.X := l.XPos * (lt.Width + 0.1) - 10;
    lt.Position.Y := l.YPos * (lt.Height + 0.1) - 10;
    lt.SymbolLabel.Text := l.Symbol;
    lt.SymbolName.Text := l.Name;
    lt.Hint := l.Summary;
  end;
end;

procedure TForm1.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta / 10;
end;

end.
