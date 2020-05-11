unit MiniDelau.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  GS.Pixel32,
  GS.Pixel32.Types,
  GS.Pixel32.Win,
  GS.Pixel32.Fragments,
  GS.Geometry,
  GS.Geometry.Triangulation,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls;

type
  TDelaunaySpecs = record
    color : TP32;
  end;

  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;
    DelaunayArray : Array of TGSTriangulationDelaunay;
    DelaunaySpecs : Array of TDelaunaySpecs;
    paletteRoll : Uint32;
    CurrentDelaunayIndex : Uint32;
    currentShader : TPixel32FragmentShader;

    procedure addDelaunay;
    function Delaunay : TGSTriangulationDelaunay;
    function TotalTriangle : Uint32;

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.addDelaunay;
begin
  inc(paletteRoll);
  if paletteRoll>Length(Pixel32Palette)-1 then
    paletteRoll := 0;
  CurrentDelaunayIndex := Length(DelaunayArray);
  SetLength(DelaunayArray,CurrentDelaunayIndex+1);
  SetLength(DelaunaySpecs,CurrentDelaunayIndex+1);
  DelaunayArray[CurrentDelaunayIndex] := TGSTriangulationDelaunay.Create;
  DelaunaySpecs[CurrentDelaunayIndex].color := pixel.colorSetAValue(Pixel32Palette[paletteRoll],100);
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Repaint;
end;

function TForm1.Delaunay: TGSTriangulationDelaunay;
begin
  result := DelaunayArray[CurrentDelaunayIndex];
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pixel := TPixel32.create;
  paletteRoll := 0;
  addDelaunay;
//  currentShader := TPixel32ShaderSquaredMotif.create;
//  currentShader := TPixel32ShaderRandomizer.create;
//  currentShader := TPixel32ShaderColorTest.create;
//  currentShader := TPixel32ShaderPlasma.create; //!
end;

procedure TForm1.FormDestroy(Sender: TObject);
var i : integer;
begin
  FreeAndNil(pixel);
  for i := 0 to Length(DelaunayArray)-1 do
  begin
    FreeAndNil(DelaunayArray[i]);
  end;
  if assigned(currentShader) then
    FreeAndNil(currentShader);
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
    Delaunay.process;
    Caption := IntToStr(TotalTriangle)+' triangle(s)';
  end;
  repaint;
end;

procedure TForm1.FormPaint(Sender: TObject);
var i,j : integer;
    a,b,c,ua,ub,uc : Vec2; ca,cb,cc : Vec4;
    Delau : TGSTriangulationDelaunay;
begin
//  pixel.beginDraw; // <--- GroupB
{ TODO : Desactivate here and activate A. Blending will be interrupted on all paint. side effect of backbuffer. To study. }
  pixel.clear;
  pixel.beginDraw;
  for j := 0 to Length(DelaunayArray)-1 do
  begin
    Delau := DelaunayArray[j];

    if Delau.Mesh.getTriangleCount=0 then
      Continue;

    //Manual draw of a mesh.
    pixel.beginDraw; // <--- GroupA
    for i:= 0 to Delau.Mesh.getTriangleCount-1 do
    begin
      pixel.resetFragmentShader;
      pixel.color_pen := DelaunaySpecs[j].color;
      if Assigned(currentShader) then
        pixel.setFragmentShader(currentShader);
      //get current triangle.
      Delau.Mesh.Triangle(i,a,b,c,ua,ub,uc,ca,cb,cc);
      //Pixel32 has only 3 vertices : We have to draw triangle by triangle.
      pixel.setVertice(0,a.x,a.y);
      pixel.setVertice(1,b.x,b.y);
      pixel.setVertice(2,c.x,c.y);
      //raster call.
      pixel.rasterize;

      //Draw triangle border.
      pixel.resetFragmentShader;
      if  CheckBox1.Checked then
      begin
        pixel.color_pen := pixel.colorSetAValue(gspBlack,50);
        pixel.moveTo(trunc(a.x),trunc(a.y));
        pixel.lineTo(trunc(b.x),trunc(b.y));
        pixel.lineTo(trunc(c.x),trunc(c.y));
        pixel.lineTo(trunc(a.x),trunc(a.y));
      end;
    end;
    pixel.endDraw;   // <--- GroupA

  end;
//  pixel.endDraw; // <--- GroupB
  pixel.CopyToDc(Canvas.handle);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pixel.resize(Width,Height);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  addDelaunay;

  pixel.color_pen := pixel.colorSetAValue(gspRed,50+Random(140));
end;

function TForm1.TotalTriangle: Uint32;
var i : integer;
begin
  result := 0;
  for i := 0 to Length(DelaunayArray)-1 do
    result := result + DelaunayArray[i].Mesh.getTriangleCount;
end;

procedure TForm1.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

end.
