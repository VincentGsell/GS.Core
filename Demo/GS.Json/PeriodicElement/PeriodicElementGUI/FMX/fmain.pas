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
    Camera1: TCamera;
    Layer3D1: TLayer3D;
    H: TLabel;
    Label1: TLabel;
    Image3D1: TImage3D;
    Layer3D2: TLayer3D;
    Label2: TLabel;
    Label3: TLabel;
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DCreate(Sender: TObject);
    procedure HMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure Layer3D1MouseLeave(Sender: TObject);
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
const lf = 'PeriodicTableJSON.json';
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
      TGSJson.JsonToObject(ls.Text,TObject(MendeleevTable)); //Inject Json into object.
    finally
      FreeAndNil(ls);
    end;
  end;
  Assert(Assigned(MendeleevTable));

  for l in MendeleevTable.Elements do
  begin
    lt := TFMXElementRepresentation.Create(nil);
    AddObject(lt);
    lt.Position.X := l.XPos * (lt.Width + 0.1) - 30;
    lt.Position.Y := l.YPos * (lt.Height + 0.1) - 10;
    lt.SymbolLabel.Text := l.Symbol;
    lt.SymbolName.Text := l.Name;
    lt.Hint := l.Symbol+' ('+l.Name+' - '+l.Category+')';
    lt.Data := l;
    lt.OnLayerMouseMove := HMouseMove;
    lt.OnMouseLeave := Layer3D1MouseLeave;
  end;
end;

procedure TForm1.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Camera1.Position.Z := Camera1.Position.Z + WheelDelta / 10;
end;

procedure TForm1.HMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
var s : TElement;
begin
  s := TElement(TFMXElementRepresentation(Sender).Data);
  Label2.Text := s.Summary;
  Label3.Text := s.Symbol+' ('+s.Name+') ('+IntToStr(s.Number)+')';
  TFMXElementRepresentation(sender).Fill.Color := TAlphaColorRec.Blue;
end;

procedure TForm1.Layer3D1MouseLeave(Sender: TObject);
begin
  Label2.Text := '';
  Label3.Text := '';
  TFMXElementRepresentation(sender).Fill.Color := TAlphaColorRec.null;
end;

end.
