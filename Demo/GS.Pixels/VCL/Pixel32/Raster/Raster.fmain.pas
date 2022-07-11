unit Raster.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,  Vcl.StdCtrls, pngImage,
  GS.Geometry.Direction,
  GS.Pixel32,
  GS.Pixel32.Types,
  GS.Pixel32.Fragments,
  GS.Pixel32.Effect.Gradient,
  GS.Pixel32.Win;

type
  TStar = record
    coord : TP32Vertice;
    len : integer;
    col : TP32;
    startAngle : integer;
    angle : Integer;
    velocity : integer;
  end;
  TQuadVertices = Array[0..3] of TP32Vertice;
  TQuadColors = array[0..3] of TP32;


  TForm2 = class(TForm)
    TimerUpdate: TTimer;
    TimerFPS: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure TimerUpdateTimer(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;
    quads : array of TStar;
    mouse : TP32Vertice;
    mouseLeft : boolean;

    scotish : TPixel32ShaderSquaredMotif;
    dice : TPixel32FragmentShaderTextureColor;

    textureShaderTest : TPixel32FragmentShaderTextureColor;
    gradientTexture : TPixel32;

    FPS,CurrentFPS,MinFPS,MaxFPS : Integer;

    //Use low level raster.
    function buildQuad(lenx,leny,cx,cy,angleInDegree : single) : TQuadVertices;
    procedure drawQuad(const vertices : TQuadVertices; const color : TP32 = $FF000000);
    procedure drawQuadBorder(const vertices : TQuadVertices; const color : TP32 = $FF000000);
    procedure drawQuads;
  end;


var
  Form2: TForm2;

implementation

{$R *.dfm}

Const QUADS_COUNT = 1000;

{ TForm2 }
procedure TForm2.FormCreate(Sender: TObject);
var grad : TPixel32GeneratorGradient;
begin
  mouseLeft := false;

  //Build specialized 32bit surface.
  pixel :=  TPixel32.create;

  //Build a "scotish" shader (squared motif, with no rotation (screen oriented))
  scotish := TPixel32ShaderSquaredMotif.Create;

  //Build a texture based shader.
  dice := TPixel32FragmentShaderTextureColor.Create;
  dice.Texture := TPixel32.create;
  dice.Texture.loadFromFile('../../../../../assets/alpha-dice.bmp');
  dice.Texture.alphaLayerResetByColor(gspBlack,0); //Make it transparent (starting from bmp)
  dice.Texture.alphaLayerResetByColor(gspWhite,0); //Dice hole too ;)
//  dice.Texture.alphaLayerReset(100);

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
    FreeAndNil(grad); //Generator not needed anymore.
  end;

  //Assign this texture a texture shader.
  textureShaderTest := TPixel32FragmentShaderTextureColor.Create;
  textureShaderTest.Texture := gradientTexture;

  FPS := 0;
  MinFPS := 9999;
  MaxFPS := 0;
end;

procedure TForm2.FormDestroy(Sender: TObject);
var l : TPixel32;
begin
  FreeAndnil(pixel);
  FreeAndNil(scotish);
  FreeAndNil(gradientTexture);
  FreeAndNil(textureShaderTest);
  l := dice.Texture;
  FreeAndNil(l);
  FreeAndNil(dice);
end;

function TForm2.buildQuad(lenx,leny,cx,cy,angleInDegree : single) : TQuadVertices;
var a : TDirectionalObject;
//    tw,th : integer;
begin
  //processing quad edge using GS.TDirectionalObject. (easy and more intuitive than matrix for simple geometry handling.)

  Result[0] := P32Vertex(trunc(cx-lenx/2),trunc(cy-leny/2),0,0,0);
  Result[1] := P32Vertex(trunc(cx+lenx/2),trunc(cy-leny/2),0,0,0);
  Result[2] := P32Vertex(trunc(cx+lenx/2),trunc(cy+leny/2),0,0,0);
  Result[3] := P32Vertex(trunc(cx-lenx/2),trunc(cy+leny/2),0,0,0);

  a := TDirectionalObject.Create(cx,cy,10);
  try
    a.SetPointedCoord(Point(result[0].x,result[0].Y));
    a.TurnBy(angleInDegree);
    Result[0] := P32Vertex(trunc(a.GetPointedCoord.X),trunc(a.GetPointedCoord.y),0,0); //...Get coord...

//    a.TurnBy(90); //...And so on.
    a.SetPointedCoord(Point(result[1].x,result[1].Y));
    a.TurnBy(angleInDegree);
    Result[1] := P32Vertex(trunc(a.GetPointedCoord.X),trunc(a.GetPointedCoord.y),1,0);

//    a.TurnBy(90);
    a.SetPointedCoord(Point(result[2].x,result[2].Y));
    a.TurnBy(angleInDegree);
    Result[2] := P32Vertex(trunc(a.GetPointedCoord.X),trunc(a.GetPointedCoord.y),1,1);

//    a.TurnBy(90);
    a.SetPointedCoord(Point(result[3].x,result[3].Y));
    a.TurnBy(angleInDegree);
    Result[3] := P32Vertex(trunc(a.GetPointedCoord.X),trunc(a.GetPointedCoord.y),0,1);
  finally
    FreeandNil(a);
  end;

end;


procedure TForm2.drawQuad(const vertices : TQuadVertices; const color: TP32);
begin
  //Draw 2 triangles for the quad. (clockwise !)
  pixel.color_pen := color;
  //Note rasterization color depends of the current shader ! See OnPaint event.

  pixel.beginDraw;
  pixel.rasterV[0] := vertices[0];
  pixel.rasterV[1] := vertices[1];
  pixel.rasterV[2] := vertices[2];
  pixel.rasterize;
  pixel.rasterV[0] := vertices[2];
  pixel.rasterV[1] := vertices[3];
  pixel.rasterV[2] := vertices[0];
  pixel.rasterize;
  pixel.endDraw;
end;

procedure TForm2.drawQuadBorder(const vertices: TQuadVertices;
  const color: TP32);
begin
  pixel.color_pen := gspBlack;
  pixel.moveTo( round(vertices[0].x), round(vertices[0].y));
  pixel.lineTo( round(vertices[1].x), round(vertices[1].y));
  pixel.lineTo( round(vertices[2].x), round(vertices[2].y));
  pixel.lineTo( round(vertices[3].x), round(vertices[3].y));
  pixel.lineTo( round(vertices[0].x), round(vertices[0].y));
end;

procedure TForm2.drawQuads;
var i : integer;
    vv : TQuadVertices;
begin
  //Draw stars with current shader.
  for i := 0 to QUADS_COUNT-1 do
  begin
    //Draw stars with texture shader.
    vv := buildQuad(quads[i].len,quads[i].len,quads[i].coord.x,quads[i].coord.y,quads[i].startAngle + quads[i].angle);
    drawQuad(vv,pixel.ColorSetAValue(quads[i].col,100));
//    pixel.ResetDrawShader;
//    drawQuadBorder(vv,pixel.ColorSetAValue(quads[i].col,100));
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
const dicesize = 1.0; //0.5
var vv : TQuadVertices;
    lx,ly : integer;
begin
  pixel.color_fill := gspWhite;
  pixel.clear;

  pixel.setFragmentShader(textureShaderTest);
  drawQuads;

  pixel.resetFragmentShader; //Back to classical shader (Color shader)
  vv := buildQuad(100,100,120,120,mouse.y);
  drawQuad(vv,pixel.ColorSetAValue(gspGreen,100));
  vv := buildQuad(100,100,120,140,mouse.y+10);
  drawQuad(vv,pixel.ColorSetAValue(gspRed,100));
  vv := buildQuad(100,100,120,160,mouse.y+20);
  drawQuad(vv,pixel.ColorSetAValue(gspBlue,100));
  vv := buildQuad(100,100,120,180,mouse.y+30);
  drawQuad(vv,pixel.ColorSetAValue(gspOrange,100));
  vv := buildQuad(100,100,120,200,mouse.y+40);
  drawQuad(vv,pixel.ColorSetAValue(gspAqua,100));

  pixel.setFragmentShader(dice);
  //Build a bounding dice
  vv := buildQuad( round(dice.Texture.width/dicesize*cos(GetTickCount/1000)),
                   round(dice.Texture.height/dicesize*cos(GetTickCount/1000)),
                   pixel.width-dice.Texture.width,dice.Texture.height,MOUSE.x);
  drawQuad(vv,pixel.ColorSetAValue(gspAqua,100));

  //Again scotish shader, with variance in color and alpha
  scotish.colorFill := pixel.ColorSetAValue(gspWhite,10);
  scotish.colorMotif := gspBlack;
  scotish.Squarelen := 5;
  pixel.setFragmentShader(scotish);
  vv := buildQuad(100,100,120,220,mouse.y+50);
  drawQuad(vv);

  if mouseLeft then
  begin
    //Select scotish shader...
    pixel.setFragmentShader(scotish);
    ///..And set alpha to basic color of the shader.
    scotish.colorFill := pixel.ColorSetAValue(gspBlue,200);
    scotish.colorMotif := pixel.ColorSetAValue(gspWhite,50);
    scotish.Squarelen := 5;
    vv := buildQuad(200,200,mouse.x,mouse.y,mouse.x);
    drawQuad(vv); //Color useless here, because of use of a special shader.
  end;


  //And a last one, with texture.
  pixel.setFragmentShader(textureShaderTest);
  vv := buildQuad(200,200,120,Pixel.height-200 ,mouse.x+50);
  drawQuad(vv);

  //We draw a line... With the scotish shader (fun effect)
  pixel.setFragmentShader(scotish);
  scotish.colorFill := gspBlue;
  scotish.colorMotif := gspWhite;
  scotish.Squarelen := 2;
  pixel.color_pen := gspBlue;
  lx := round(mouse.x);
  ly := round(mouse.y);
  //Avoid line cross to be draw where shader will produce "white" screen :)
  if (lx mod 5) = 1 then
   inc(lx);
  if (ly mod 5) = 1 then
   inc(ly);
  pixel.moveTo(lx,0);
  pixel.lineTo(lx,pixel.height);
  pixel.moveTo(0,ly);
  pixel.lineTo(pixel.width,ly);
  pixel.resetFragmentShader; //back to standart colo drawer.


  //VCL specific.
  pixel.CopyToDc(GetDC(Handle));
  Inc(FPS);
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

procedure TForm2.TimerFPSTimer(Sender: TObject);
begin
  CurrentFPS := FPS;
  if MinFPS>FPS then
    MinFPS := FPS;
  if MaxFPS<FPS then
    MaxFPS := FPS;
  FPS := 0;
end;

procedure TForm2.TimerUpdateTimer(Sender: TObject);
var i : integer;
begin
  //Update angle of stars. (rotate)
  for i := 0 to QUADS_COUNT-1 do
  begin
    quads[i].angle := round(mouse.x);
    quads[i].coord.x := quads[i].coord.x + quads[i].velocity;
    if quads[i].coord.x > Width + quads[i].len then
      quads[i].coord.x := 0-quads[i].len;
  end;

  repaint;
  Caption := format('GS.Pixel32 Raster demo FPS : %d (count : %d Min : %d, Max : %d)',[CurrentFPS,FPS, MinFPS,MaxFPS]);
end;

procedure TForm2.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

end.
