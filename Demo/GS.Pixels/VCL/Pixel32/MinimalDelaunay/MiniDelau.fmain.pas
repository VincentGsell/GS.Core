unit MiniDelau.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  GS.Pixel32,
  GS.Pixel32.Win,
  GS.Pixel32.PixelShader,
  GS.Geometry.Delaunay,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;
    Delaunay : TDelaunay;
    currentShader : TPixel32ColorShader;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Repaint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pixel := TPixel32.create;
  Delaunay := TDelaunay.Create;
//  currentShader := TPixel32ShaderSquaredMotif.create(pixel);
//  currentShader := TPixel32ShaderRandomizer.create(pixel);
  currentShader := TPixel32ShaderColorTest.create(pixel);
//  currentShader := TPixel32ShaderPlasma.create(pixel);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pixel);
  FreeAndNil(Delaunay);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pixel.moveTo(x,y);
  Delaunay.AddPoint(x,y);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssleft in Shift then
  begin
    pixel.lineTo(x,y);
    pixel.moveTo(x,y);
    Delaunay.AddPoint(x,y);
    Delaunay.Mesh;
    Delaunay.Mesh;
    Delaunay.Mesh;
    Caption := IntToStr(Delaunay.HowMany)+' triangle(s)';
  end;
  repaint;
end;

procedure TForm1.FormPaint(Sender: TObject);
var i : integer;
    x1,y1 : integer;
    x2,y2 : integer;
    x3,y3 : integer;
begin
  //pixel.ResetDrawShader;
  pixel.clear;
  pixel.color_pen := gspBlue;
  for i:= 1 to Delaunay.HowMany do
  begin
    x1 := Round(Delaunay.Vertex^[Delaunay.Triangle^[i].vv0].x);
    y1 := Round(Delaunay.Vertex^[Delaunay.Triangle^[i].vv0].y);
    x2 := Round(Delaunay.Vertex^[Delaunay.Triangle^[i].vv1].x);
    y2 := Round(Delaunay.Vertex^[Delaunay.Triangle^[i].vv1].y);
    x3 := Round(Delaunay.Vertex^[Delaunay.Triangle^[i].vv2].x);
    y3 := Round(Delaunay.Vertex^[Delaunay.Triangle^[i].vv2].y);
    pixel.color_pen := pixel.ColorSetAValue(gspGreen,255);
    pixel.setDrawShader(currentShader);
    pixel.rasterize(x1,y1,x2,y2,x3,y3);
    pixel.resetDrawShader;
    if  CheckBox1.Checked then
    begin
      pixel.color_pen := gspBlack;
      pixel.moveTo(x1,y1);
      pixel.lineTo(x2,y2);
      pixel.lineTo(x3,y3);
      pixel.lineTo(x1,y1);
    end;
  end;
  pixel.CopyToDc(Canvas.handle);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pixel.resize(Width,Height);
end;

procedure TForm1.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

end.
