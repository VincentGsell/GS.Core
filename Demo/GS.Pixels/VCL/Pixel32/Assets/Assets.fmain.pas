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
  GS.Pixel32.PixelShader,
  GS.Pixel32.Draw,
  GS.Pixel32.Win;

type
  TForm1 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    pixel : TPixel32;
    te : TPixel32TextureShader;
    M : TPoint;
    { Public declarations }
    mesh : TGSRawMesh2D;
    imageAsset : TGSAssetImageSource;
    image : TPixel32Image;

    //shape
    shapeAsset : TGSAssetShapeMesh;
    shape : TPixelShape;
    shape2 : TPixelShape; //Same assets !

    //fun shader.
    shaderFun1,shaderFun2 : TPixel32ColorShader;

    procedure appIdle(sender : TObject; var Done : Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  mesh := TGSRawMesh2D.create;
  pixel := TPixel32.create;

  te := TPixel32TextureShader.Create;
  te.Texture := TPixel32.create;
  te.Texture.loadFromFile('../../../../../assets/avatar.bmp');
  te.Texture.alphaLayerResetByColor(gspWhite,0); //delete background of texture.

//  te.Texture.loadFromFile('plasma.bmp');
//  te.Texture.loadFromFile('alpha-dice.bmp');
//  te.Texture.alphaLayerResetByColor(gspBlack,0);

  imageAsset := TGSAssetImageSource.Create;

  TMemoryStream(imageAsset.Image.BinaryData).LoadFromFile('../../../../../assets/avatar.bmp');
  imageAsset.Image.ImageFormat := TGSAssetImageFormat.aifBitmap;
  imageAsset.Image.AssetImageSource := '';
  imageAsset.Image.ImageDescription := 'an avatar with an axes.';
  imageAsset.Atlas.addZone(0,0,te.Texture.width,te.Texture.height,'*');
  imageAsset.Atlas.addZone(60,0,150,100,'Head');
  //imageAsset.shape ?

  image := TPixel32Image.Create(imageAsset);

  //A shape...
  //-> the asset, reprensenting a shape and its data.
  shapeAsset := TGSAssetShapeMesh.Create;
  //shapeAsset.PreShapedModel := TGSShape2dType.circle50; //circle.
  shapeAsset.PreShapedModel := TGSShape2dType.octo; //octogone based.
  shapeAsset.MeshData.Scale(10,10);

  shape := TPixelShape.Create(shapeAsset);
  shape2 := TPixelShape.Create(shapeAsset);


  //Some shaders.
  shaderFun1 := TPixel32ShaderSquaredMotif.Create;
  TPixel32ShaderSquaredMotif(shaderFun1).SetDataColor(gspColorNone,gspBlue);
//  shaderFun2 := TPixel32ShaderPlasma.Create(200);
//  shaderFun2 := TPixel32ShaderRandomizer.Create(100);
//  shaderFun2 := TPixel32ShaderColorTest.Create(150);
  shaderFun2 := TPixel32ShaderStaticTextureTiledOnSurface.Create(te.Texture,150);
//  shaderFun2 :=   TPixel32ShaderStaticTextureStretchOnSurface.Create(te.Texture,150);

  Application.OnIdle := appIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  freeAndNil(pixel);
  FreeAndNil(mesh);
  FreeAndNil(imageAsset);
  FreeAndNil(te.Texture);
  FreeAndNil(te);

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
  pixel.clear;
  c := vec2.create(width/2,height/2);
  a := M.X/2;

  imageAsset.Shape.Side := 250+M.X;
  imageAsset.Shape.MeshData.copy(mesh); //Take data from the assets.

  mesh.Rotate(a);
  mesh.Pan(0,-(c.y-M.y));

  //Draw a background.
  r := GetTickCount/1000;
  pixel.resetDrawShader;
  pixel.color_pen := gspNavy;
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


  //draw mesh
  pixel.color_pen := pixel.colorSetAValue(gspNavy,150);
  pixel.setDrawShader(te);
  for i := 0 to mesh.getTriangleCount-1 do
  begin
    mesh.Triangle(i,va,vb,vc,vua,vub,vuc);
    pixel.setVertex(0,trunc(va.x+c.x),trunc(va.y+c.y),0,trunc(vua.x),trunc(vua.y));
    pixel.setVertex(1,trunc(vb.x+c.x),trunc(vb.y+c.y),0,trunc(vub.x),trunc(vub.y));
    pixel.setVertex(2,trunc(vc.x+c.x),trunc(vc.y+c.y),0,trunc(vuc.x),trunc(vuc.y));
    pixel.rasterize;
  end;

  pixel.resetDrawShader;

  shape.ResetMeshFromAsset;
  shape.Mesh.Scale(15,15);
  shape.Mesh.Rotate(-a);
  shape.Mesh.Pan(M.X,M.Y);

  shape2.ResetMeshFromAsset;
  shape2.Mesh.Scale(5,5);
  shape2.Mesh.Pan(100,100);
  pixel.draw(shape2);

  //Shading composition : 2 draw call.
  pixel.setDrawShader(shaderFun1);
  pixel.draw(shape);
  pixel.setDrawShader(shaderFun2);
  pixel.draw(shape);

  pixel.CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
  Image1.Repaint;
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

end.
