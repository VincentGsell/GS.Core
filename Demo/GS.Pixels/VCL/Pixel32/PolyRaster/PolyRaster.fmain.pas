unit PolyRaster.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GS.Assets,
  GS.Geometry,
  GS.Pixel,
  GS.Pixel.Draw,
  GS.Pixel32,
  GS.Pixel32.Types,
  GS.Pixel32.Fragments,
  GS.Pixel32.Draw,
  GS.Pixel32.Rasterize,
  GS.Pixel32.Win;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Image1: TImage;
    Label1: TLabel;
    TimerSec: TTimer;
    Label2: TLabel;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimerSecTimer(Sender: TObject);
  private
    { Private declarations }
    FMouse : TPoint;
    FFPS : Integer;

    flatColorFragment : TPixel32FragmentShaderFlatColor;
    verticeColorFragment : TPixel32FragmentShaderVerticeColor;
    textureColorFragment : TPixel32FragmentShaderTextureColor;
    checkerBoardColorFragment : TPixel32FragmentShaderCheckerBoard;

    procedure internalPaint;
    procedure internalDrawMouseCross;

    procedure internalAppIdle(sender : TObject; var Done : Boolean);

  public
    { Public declarations }
    pixel : TPixel32;
    MyShape : TPixelShape;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var asset : TGSAssetRoundedShapeMesh;
begin
  pixel := TPixel32.create;
  asset := TGSAssetRoundedShapeMesh.Create;
  asset.MeshData.SetUpQuad(256);
  MyShape := TPixelShape.Create(asset);
  FFPS := 0;

  flatColorFragment := TPixel32FragmentShaderFlatColor.Create;
  verticeColorFragment := TPixel32FragmentShaderVerticeColor.Create;
  textureColorFragment := TPixel32FragmentShaderTextureColor.Create;
  checkerBoardColorFragment := TPixel32FragmentShaderCheckerBoard.Create;

  flatColorFragment.Color := pixel.colorP32Rec(255,255,255,100).Color;

  textureColorFragment.Texture := TPixel32.create;
  textureColorFragment.Texture.loadFromFile('..\..\..\..\..\assets\avatar.bmp');
  textureColorFragment.Texture.alphaLayerReset(220);

  verticeColorFragment := TPixel32FragmentShaderVerticeColor.Create;

  checkerBoardColorFragment := TPixel32FragmentShaderCheckerBoard.Create;

  Application.OnIdle := internalAppIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  freeAndNil(Pixel);
  freeAndNil(MyShape);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Image1.Picture.Bitmap.SetSize(Image1.ClientWidth,Image1.ClientHeight);
  pixel.resize(Image1.ClientWidth,Image1.ClientHeight);
  internalPaint;
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var a,b,c : TP32Vertice;
begin
  Label1.Caption := Format('Mouse pos. : (%d,%d)',[X,Y]);
  FMouse := Point(X,Y);
end;

procedure TForm1.internalDrawMouseCross;
begin
  pixel.color_pen := pixel.colorSetAValue(gspWhite,100);
  pixel.moveTo(0,FMouse.Y);
  pixel.lineTo(pixel.width,FMouse.Y);
  pixel.moveTo(FMouse.X,0);
  pixel.lineTo(FMouse.X,pixel.height);
end;

procedure TForm1.internalPaint;
var i : integer;
    va,vb,vc, vua,vub,vuc : Vec2;
    ca,cb,cc : Vec4;
begin
  pixel.rasterMode := TSoftwareRasterizeOption.roBackBuffer;
  if CheckBox1.Checked then
    pixel.rasterMode := TSoftwareRasterizeOption.roDirectMode;

  pixel.beginDraw;

  pixel.resetFragmentShader;
  pixel.color_fill := gspBlue;
  pixel.clear;

  pixel.setFragmentShader(verticeColorFragment);
  pixel.fill;

  MyShape.ResetMeshFromAsset;
  MyShape.Mesh.Rotate(FMouse.Y);
  MyShape.Mesh.Pan(200,200);
  pixel.setFragmentShader(flatColorFragment);
  flatColorFragment.Color := gspWhite;
  pixel.draw(MyShape);

  MyShape.ResetMeshFromAsset;
  MyShape.Mesh.Rotate(FMouse.Y);
  MyShape.Mesh.Pan(400,200);
  pixel.setFragmentShader(flatColorFragment);
  flatColorFragment.Color := pixel.colorSetAValue(gspGreen,150);
  pixel.draw(MyShape);

  MyShape.ResetMeshFromAsset;
  MyShape.Mesh.Rotate(FMouse.Y);
  MyShape.Mesh.Pan(600,200);
  pixel.setFragmentShader(textureColorFragment);
  pixel.draw(MyShape);

  MyShape.ResetMeshFromAsset;
  MyShape.Mesh.Rotate(FMouse.X);
  MyShape.Mesh.Pan(800,200);
  pixel.setFragmentShader(verticeColorFragment);
  pixel.draw(MyShape);


  pixel.resetFragmentShader;
  pixel.color_pen := pixel.colorSetAValue(gspWhite,100);
  internalDrawMouseCross;

  //Transfert to Image VCL component.
  pixel.CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
  //Ask VCL refresh.
  Image1.Repaint;
  pixel.endDraw;
end;

procedure TForm1.TimerSecTimer(Sender: TObject);
begin
  Label2.Caption := IntToStr(FFPS);
  FFPS := 0;
end;

procedure TForm1.internalAppIdle(sender: TObject; var Done: Boolean);
begin
  internalPaint;
  Done := false; //boost.
  inc(FFPS);
end;

end.
