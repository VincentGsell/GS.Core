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
    TimerUpdate: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
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
    procedure drawQuad(const vertices : TQuadVertices; const color : TP32 = $FF000000);
    procedure drawQuads;
  end;


var
  Form2: TForm2;

implementation

{$R *.dfm}

Const QUADS_COUNT = 1000;

{ TForm2 }
procedure TForm2.FormCreate(Sender: TObject);
var i : integer;
    grad : TPixel32GeneratorGradient;
begin
  mouseLeft := false;

  //Build specialized 32bit surface.
  pixel :=  TPixel32.create;

  //Build a "scotish" shader (squared motif, with no rotation (screen oriented))
  scotish := TPixel32ShaderSquaredMotif.Create(pixel);

  //Generate a gradient "star" picture, for texturing.
  gradientTexture := TPixel32.create; //Storage.
  grad := TPixel32GeneratorGradient.Create; //Generator.
  try
    grad.init(gradientTexture);
    grad.ColorA := pixel.ColorSetAValue(gspWhite,120);
    grad.ColorB := gspRed;
    grad.ShiftGradient := -50;
    grad.RorateGradient := 0;
    grad.process;
  finally
    FreeAndNil(grad); //No more need generator.
  end;

  //Assign this texture a texture shader.
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

function TForm2.buildQuad(len,cx,cy,angleInDegree : Integer) : TQuadVertices;
var a : TDirectionalObject;
begin
  //processing quad edge using GS.TDirectionalObject. (easy and more intuitive than matrix for simple geometry handling.)
  a := TDirectionalObject.Create(cx,cy,len div 2);
  try
    a.TurnBy(angleInDegree);
    a.TurnBy(90); //Turn 90°...
    Result[0] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y)); //...Get coord...
    a.TurnBy(90); //...And so on.
    Result[1] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y));
    a.TurnBy(90);
    Result[2] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y));
    a.TurnBy(90);
    Result[3] := P32Vertex(Round(a.GetPointedCoord.X),Round(a.GetPointedCoord.y));
  finally
    FreeandNil(a);
  end;
end;


procedure TForm2.drawQuad(const vertices : TQuadVertices; const color: TP32);
begin
  //Draw 2 triangles for the quad. (clockwise !)
  pixel.color_pen := color;
  //Note rasterization color depends of the current shader ! See OnPaint event.
  pixel.rasterize( vertices[0].x, vertices[0].y, vertices[1].x, vertices[1].y, vertices[2].x, vertices[2].y);
  pixel.rasterize( vertices[2].x, vertices[2].y, vertices[3].x, vertices[3].y, vertices[0].x, vertices[0].y);

  //Draw border. If shader permit it. :)
  pixel.color_pen := gspBlack;
  pixel.moveTo( vertices[0].x, vertices[0].y);
  pixel.lineTo( vertices[1].x, vertices[1].y);
  pixel.lineTo( vertices[2].x, vertices[2].y);
  pixel.lineTo( vertices[3].x, vertices[3].y);
  pixel.lineTo( vertices[0].x, vertices[0].y);
end;

procedure TForm2.drawQuads;
var i : integer;
    vv : TQuadVertices;
begin
  //Draw stars with current shader.
  for i := 0 to QUADS_COUNT-1 do
  begin
    vv := buildQuad(quads[i].len,quads[i].coord.x,quads[i].coord.y,quads[i].startAngle + quads[i].angle);
    drawQuad(vv,pixel.ColorSetAValue(quads[i].col,100));
  end;
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

  //Draw stars with texture shader.
  pixel.setDrawShader(textureShaderTest);
  drawQuads;

  if mouseLeft then
  begin
    //Select scotish shader...
    pixel.setDrawShader(scotish);
    ///..And set alpha to basic color of the shader.
    scotish.SetDataColor(pixel.ColorSetAValue(gspBlue,200),pixel.ColorSetAValue(gspWhite,50),5);
    vv := buildQuad(200,mouse.x,mouse.y,mouse.x);
    drawQuad(vv); //Color useless here, because of use of a special shader.
  end;

  pixel.ResetDrawShader; //Back to classical shader (Color shader)
  vv := buildQuad(100,120,120,mouse.y);
  drawQuad(vv,pixel.ColorSetAValue(gspGreen,100));
  vv := buildQuad(100,120,140,mouse.y+10);
  drawQuad(vv,pixel.ColorSetAValue(gspRed,100));
  vv := buildQuad(100,120,160,mouse.y+20);
  drawQuad(vv,pixel.ColorSetAValue(gspBlue,100));
  vv := buildQuad(100,120,180,mouse.y+30);
  drawQuad(vv,pixel.ColorSetAValue(gspOrange,100));
  vv := buildQuad(100,120,200,mouse.y+40);
  drawQuad(vv,pixel.ColorSetAValue(gspAqua,100));

  //Again scotish shader, with variance in color and alpha
  scotish.SetDataColor(pixel.ColorSetAValue(gspWhite,200),gspBlack,5);
  pixel.setDrawShader(scotish);
  vv := buildQuad(100,120,220,mouse.y+50);
  drawQuad(vv);


  //And a last one, with texture.
  pixel.setDrawShader(textureShaderTest);
  vv := buildQuad(200,120,Pixel.height-200 ,mouse.x+50);
  drawQuad(vv);

  //We draw a line... With the scotish shader (fun effect)
  pixel.setDrawShader(scotish);
  scotish.SetDataColor(gspBlue,gspWhite,5);
  pixel.color_pen := gspBlue;
  pixel.moveTo(mouse.x,0);
  pixel.lineTo(mouse.x,pixel.height);
  pixel.moveTo(0,mouse.y);
  pixel.lineTo(pixel.width,mouse.y);
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

procedure TForm2.TimerUpdateTimer(Sender: TObject);
var i : integer;
begin
  //Update angle of stars. (rotate)
  for i := 0 to QUADS_COUNT-1 do
  begin
    quads[i].angle := mouse.x;
    quads[i].coord.x := quads[i].coord.x + quads[i].velocity;
    if quads[i].coord.x > Width + quads[i].len then
      quads[i].coord.x := 0-quads[i].len;
  end;

  repaint;
end;

procedure TForm2.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

end.
