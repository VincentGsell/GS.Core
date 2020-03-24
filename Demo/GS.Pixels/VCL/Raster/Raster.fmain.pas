unit Raster.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,  Vcl.StdCtrls,
  GS.Geometry.Direction,
  GS.Pixel32,
  GS.Pixel32.PixelShader,
  GS.Pixel32.Effect.Generator.Gradient,
  GS.Pixel32.VCL;

type
  TStar = record
    coord : TP32Vertex;
    len : integer;
    col : TP32;
    startAngle : integer;
    angle : Integer;
    velocity : integer;
  end;
  TQuadVertices = Array[0..3] of TP32Vertex;
  TQuadColors = array[0..3] of TP32;


  TForm2 = class(TForm)
    TimerRepaint: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure TimerRepaintTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;
    quads : array of TStar;
    mouse : TP32Vertex;
    mouseLeft : boolean;

    scotish : TPixel32ShaderSquaredMotif;

    textureShaderTest : TCustomPixelChHeShader;

    gradientTexture : TPixel32;

    //Use low level raster.
    function buildQuad(len,cx,cy,angleInDegree : Integer) : TQuadVertices;
    procedure drawQuad(const vertices : TQuadVertices; color : TP32);
    procedure drawQuads;
  end;


var
  Form2: TForm2;

implementation

{$R *.dfm}

Const QUADS_COUNT = 1000;

{ TForm2 }

function TForm2.buildQuad(len,cx,cy,angleInDegree : Integer) : TQuadVertices;
var a : TDirectionalObject;
begin
  //processing quad edge using TDirectionalObject. (easy and more intuitive than matrix for simple geo.)
  a := TDirectionalObject.Create(cx,cy,len div 2);
  try
    a.TurnBy(angleInDegree);
    a.TurnBy(90);
    Result[0] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y));
    a.TurnBy(90);
    Result[1] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y));
    a.TurnBy(90);
    Result[2] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y));
    a.TurnBy(90);
    Result[3] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y));
  finally
    FreeandNil(a);
  end;

end;


procedure TForm2.drawQuad(const vertices : TQuadVertices; color: TP32);
begin
  //Draw 2 triangles for the quad.
  pixel.color_pen := color;
  pixel.rasterize( vertices[0].x, vertices[0].y, vertices[1].x, vertices[1].y, vertices[2].x, vertices[2].y);
  pixel.rasterize( vertices[2].x, vertices[2].y, vertices[3].x, vertices[3].y, vertices[0].x, vertices[0].y);

  pixel.moveTo( vertices[0].x, vertices[0].y);

  //Draw border.
  pixel.color_pen := gspBlack;
  pixel.lineTo( vertices[1].x, vertices[1].y);
  pixel.lineTo( vertices[2].x, vertices[2].y);
  pixel.lineTo( vertices[3].x, vertices[3].y);
  pixel.lineTo( vertices[0].x, vertices[0].y);
end;

procedure TForm2.drawQuads;
var i : integer;
    vv : TQuadVertices;
begin
  for i := 0 to QUADS_COUNT-1 do
  begin
    vv := buildQuad(quads[i].len,quads[i].coord.x,quads[i].coord.y,quads[i].startAngle + quads[i].angle);
    drawQuad(vv,quads[i].col);
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
var i : integer;
    grad : TPixel32GeneratorGradient;
begin
  pixel :=  TPixel32.create;
  mouseLeft := false;

  scotish := TPixel32ShaderSquaredMotif.Create(pixel);

  //Generate a picture, for a texture.

  gradientTexture := TPixel32.create;
  grad := TPixel32GeneratorGradient.Create;
  try
    grad.init(gradientTexture);
    grad.ColorA := gspWhite;
    grad.ColorB := gspRed;
    grad.ShiftGradient := -50;
    grad.RorateGradient := 0;
    grad.process;
  finally
    FreeAndNil(grad);
  end;

  textureShaderTest := TCustomPixelChHeShader.Create(pixel);
  textureShaderTest.Texture := gradientTexture;

end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FreeAndnil(pixel);
  FreeAndNil(scotish);
  FreeAndNil(gradientTexture);
  FreeAndNil(textureShaderTest);
end;

procedure TForm2.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pixel.moveTo(x,y);
end;

procedure TForm2.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  mouse := P32Vertex(x,y);
  mouseLeft := ssleft in Shift;
end;

procedure TForm2.FormPaint(Sender: TObject);
var i : integer;
    vv : TQuadVertices;

begin
  pixel.clear;

  //Update angle.
  for i := 0 to QUADS_COUNT-1 do
  begin
    quads[i].angle := mouse.x;
    quads[i].coord.x := quads[i].coord.x + quads[i].velocity;
    if quads[i].coord.x > Width + quads[i].len then
      quads[i].coord.x := 0-quads[i].len;
  end;

  drawQuads;

  if mouseLeft then
  begin
    vv := buildQuad(200,mouse.x,mouse.y,mouse.x);
    pixel.setDrawShader(textureShaderTest);
    drawQuad(vv,gspOrange);
    pixel.ResetDrawShader; //back to standart colo drawer.
  end;

  vv := buildQuad(100,120,120,mouse.y);
  drawQuad(vv,gspGreen);
  vv := buildQuad(100,120,140,mouse.y+10);
  drawQuad(vv,gspRed);
  vv := buildQuad(100,120,160,mouse.y+20);
  drawQuad(vv,gspBlue);
  vv := buildQuad(100,120,180,mouse.y+30);
  drawQuad(vv,gspOrange);
  vv := buildQuad(100,120,200,mouse.y+40);
  drawQuad(vv,gspAqua);

  scotish.SetDataColor(gspBlue,gspWhite,5);
  pixel.setDrawShader(scotish);
  vv := buildQuad(100,120,220,mouse.y+50);
  drawQuad(vv,gspWhite);

  pixel.color_pen := gspBlue;
  pixel.moveTo(width div 2, height div 2);
  pixel.lineTo(mouse.x,mouse.y);
  pixel.ResetDrawShader; //back to standart colo drawer.


  //VCL specific.
  pixel.CopyToDc(GetDC(Handle));
end;

procedure TForm2.FormResize(Sender: TObject);
var i : integer;
begin
  pixel.resize(Width,Height);
  SetLength(quads,QUADS_COUNT);
  for i := 0 to QUADS_COUNT-1 do
  begin
    quads[i].len := 5 + Random(20);
    quads[i].velocity := 1 + Random(5);
    quads[i].coord := P32Vertex(Random(Width),Random(Height));
    quads[i].col := gspRed;
    quads[i].startAngle := Random(360);
    quads[i].angle := Random(360);
  end;

end;

procedure TForm2.TimerRepaintTimer(Sender: TObject);
begin
  repaint;
end;

procedure TForm2.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

end.
