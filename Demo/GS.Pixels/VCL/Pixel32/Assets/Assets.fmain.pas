unit Assets.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.ExtCtrls, Vcl.StdCtrls,
  GS.Geometry,
  GS.Geometry.Mesh2d,
  GS.Geometry.Mesh2d.Tools,
  GS.Assets,
  GS.Pixel.Draw,
  GS.Pixel32,
  GS.Pixel32.Types,
  GS.Pixel32.Fragments,
  GS.Pixel32.Draw,
  GS.Pixel32.Win;

type
  TForm1 = class(TForm)
    Image1: TImage;
    TimerFPS: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
  private
    { Private declarations }
  public
    pixel : TPixel32;
    M : TPoint;
    FPS : Integer;
    { Public declarations }
    imageAsset : TPixel32AssetImageSource;
    image : TPixel32Image;

    //shape
    shapeAsset : TGSAssetRoundedShapeMesh;
    shape : TPixelShape;
    shape2 : TPixelShape; //Same assets !

    //fun shader.
    shaderFun1,shaderFun2 : TPixel32FragmentShader;

    procedure appIdle(sender : TObject; var Done : Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var m : TMemoryStream;
begin
  pixel := TPixel32.create;  //Main surface

  //We ponctualy used pixel surface to load and build an asset.
  //Normaly, we load asset directly.

  //Build manualy a image asset.
  pixel.loadFromFile('../../../../../assets/avatar.bmp');
  pixel.alphaLayerResetByColor(gspWhite,0); //delete background for transparent texture.

  m := TMemoryStream.Create;
  try
    pixel.SaveToARGB32FormatStream(m);
    pixel.clear;
    imageAsset := TPixel32AssetImageSource.Create;
    TMemoryStream(imageAsset.Image.BinaryData).LoadFromStream(m);
    imageAsset.Image.Width := pixel.width;
    imageAsset.Image.Height := pixel.height;
  finally
    freeAndNil(m);
  end;

  imageAsset.Image.ImageFormat := TGSAssetImageFormat.aifRawARGB32BitFormat;
  imageAsset.Image.AssetImageSource := '';
  imageAsset.Image.ImageDescription := 'an avatar with an axes.';
  imageAsset.Atlas.addZone(0,0,ImageAsset.Image.width,ImageAsset.Image.height,'*');
  imageAsset.Atlas.addZone(60,0,150,100,'Head');

  image := TPixel32Image.Create(imageAsset);

  //A shape...
  //-> the asset, reprensenting a shape and its data.
  shapeAsset := TGSAssetRoundedShapeMesh.Create;
  //shapeAsset.PreShapedModel := TGSShape2dType.circle50; //circle.
  shapeAsset.PreShapedModel := TGSShape2dType.octo; //octogone based.
  shapeAsset.MeshData.Scale(10,10);

  shape := TPixelShape.Create(shapeAsset);
  shape2 := TPixelShape.Create(shapeAsset);

  //Some shaders.
  shaderFun1 := TPixel32ShaderSquaredMotif.Create;
  TPixel32ShaderSquaredMotif(shaderFun1).colorFill :=  gspColorNone;
  TPixel32ShaderSquaredMotif(shaderFun1).colorMotif :=  gspBlue;
//  shaderFun2 := TPixel32ShaderPlasma.Create(200);
//  shaderFun2 := TPixel32ShaderRandomizer.Create(100);
//  shaderFun2 := TPixel32ShaderColorTest.Create(150);
  shaderFun2 := TPixel32ShaderStaticTextureTiledOnSurface.Create(imageAsset.GetShader.Texture,150);
//  shaderFun2 :=   TPixel32ShaderStaticTextureStretchOnSurface.Create(te.Texture,150);

  pixel.rasterMode := TSoftwareRasterizeOption.roBackBuffer;
//  pixel.rasterMode := TSoftwareRasterizeOption.robasic;
  Application.OnIdle := appIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  freeAndNil(pixel);
  FreeAndNil(imageAsset);

  FreeAndNil(shapeAsset);
  freeAndNil(image);

  FreeAndNil(shape);
  FreeAndNil(shape2);

  FreeAndNil(shaderFun1);
  FreeAndNil(shaderFun2);
end;


procedure TForm1.appIdle(sender: TObject; var Done: Boolean);
var i,j : integer;
    r : single;
    c : vec2;
    a : single;

    va,vb,vc,vua,vub,vuc : vec2;
begin
  pixel.beginDraw;
  pixel.clear;

  //Draw a background.
  r := GetTickCount/1000;
  pixel.resetFragmentShader;
  pixel.color_pen := pixel.colorSetAValue(gspOlive,50);
  j := 0;
  i := 0;
  while j*40<pixel.height do
  begin
    while i*40<pixel.width do
    begin
      shape2.ResetMeshFromAsset;
      shape2.Mesh.Rotate(r);
      shape2.Mesh.Scale(2+1*cos(r),2+1*sin(r));
      shape2.Mesh.Pan(i*40,j*40);
      pixel.draw(shape2);
      inc(i);
    end;
    inc(j);
    i := 0;
  end;


  c := vec2.create(width/2,height/2);
  a := M.X/2;

  //draw mesh
  image.ResetMeshFromAsset;
  image.Mesh.Scale(100+M.X,100+M.X);
  image.Mesh.Rotate(a);
  image.Mesh.Pan(M.X,c.y + (c.y - M.Y));
  pixel.draw(image);

  pixel.resetFragmentShader;
  shape.ResetMeshFromAsset;
  shape.Mesh.Scale(15,15);
  shape.Mesh.Rotate(-a);
  shape.Mesh.Pan(M.X,M.Y);

  shape2.ResetMeshFromAsset;
  shape2.Mesh.Scale(5,5);
  shape2.Mesh.Pan(100,100);
  pixel.draw(shape2);

  //Shading composition : 2 draw call.
  pixel.setFragmentShader(shaderFun1);
  pixel.draw(shape);
  pixel.setFragmentShader(shaderFun2);
  pixel.draw(shape);

  pixel.CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
  Image1.Repaint;
  pixel.endDraw;
  inc(FPS);
  Done := false;
end;



procedure TForm1.FormResize(Sender: TObject);
begin
  Image1.Picture.Bitmap.SetSize(width,height);
  pixel.resize(width,Height);
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  M := Point(X,Y);
end;

procedure TForm1.TimerFPSTimer(Sender: TObject);
begin
 Caption := 'Asset''s Usage - FPS : '+IntToStr(FPS);
 FPS := 0;
end;

end.
