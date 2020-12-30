unit fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Types3D, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, System.Math.Vectors, FMX.Objects, FMX.Controls3D, FMX.Layers3D,
  FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  GS.Drawing.Shape;

type

  TForm1 = class(TForm3D)
    Layer3D1: TLayer3D;
    Selection1: TSelection;
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    Selection2: TSelection;
    procedure Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; var Handled: Boolean);
    procedure Form3DCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Selection1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  f : TQuadPoly;
  g : TRegularPoly;

implementation


{$R *.fmx}


procedure TForm1.Button1Click(Sender: TObject);
begin
  f.EditorEnabled := Not(f.EditorEnabled);
end;

procedure TForm1.Button2Click(Sender: TObject);
var ff : TQuadPoly;
    sel : TSelection;
begin
  ff := TQuadPoly.Create(Self);
  sel := TSelection.Create(Self);
  Layer3D1.addObject(Sel);
  sel.AddObject(ff);
  ff.Align := TAlignLayout.Client;
  ff.HitTest := False;
  ff.EditorEnabled := false;
end;

procedure TForm1.Form3DCreate(Sender: TObject);
begin
  f := TQuadPoly.Create(Self);
  Selection1.AddObject(f);
  f.Align := TAlignLayout.Client;
  f.HitTest := False;
  f.EditorEnabled := true;

  g := TRegularPoly.Create(Self);
  Selection2.AddObject(g);
  g.Align := TAlignLayout.Client;
  g.HitTest := False;
  g.EditorEnabled := true;
end;

procedure TForm1.Form3DMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; var Handled: Boolean);
begin
  Layer3D1.Position.Z := Layer3D1.Position.Z + WheelDelta/1000;
end;

procedure TForm1.Selection1Change(Sender: TObject);
begin
  memo1.Lines.Add(format('Cg %f %f %f %f',[Selection1.Position.X,Selection1.Position.Y,Selection1.Width,Selection1.Height]));
end;

end.
