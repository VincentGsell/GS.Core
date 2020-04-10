unit D3DRaster.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, contnrs,
  GS.Geometry.Soft3D,
  GS.Pixel32.PixelShader,
  GS.Pixel32.Effect.Gradient,
  GS.Pixel32,
  GS.Pixel32.Win;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    cbWireFrame: TCheckBox;
    Label1: TLabel;
    TimerFPS: TTimer;
    cbRasterFrame: TCheckBox;
    RadioGroup1: TRadioGroup;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure cbWireFrameClick(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;     //surface for drwaing 3d stuff
    viewport : TViewport; //virtual window on 3d world.
    FPS : integer;

    mem : TObjectList;

    //classic computed Texture shader.
    clatextureShader_computed : TPixel32TextureShader;
    //Classic image Texture shader.
    clatextureShader_imageGoldo, clatextureShader_imageDice : TPixel32TextureShader;
    //fun "squared2 shader (squared motif, with no rotation (screen oriented))
    scot : TPixel32ShaderSquaredMotif;
    //Classical color shader.
    colShader : TPixel32ColorShader;
    //Shader toy inspired texture shader.
    ShaderToyShader : TPixel32ColorShader; //The shader
    TextureShader_ShaderToy : TPixel32TextureShader;  //Texture shader where the above shader will process.

    //And finally, a specialized DethColorTextureShader. (default)
    ///TODO  dctShader := ....

    gradientTexture : TPixel32;
    imageTexGoldo, imageTexDice, imageTexShader : TPixel32;

    procedure appIdle(Sender: TObject; var Done: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var grad : TPixel32GeneratorGradient;
    b : TBitmap; //VCL specific bitmap.
begin
  mem := TObjectList.Create;

  pixel := TPixel32.create;     //surface for drwaing 3d stuff
  viewport := TViewport.Create; //virtual window on 3d world.
  viewport.TargetCanvas := pixel;
  viewport.CameraZ := -5;
  viewport.wireFrame := cbWireFrame.Checked;
  viewport.rasterFrame := cbRasterFrame.Checked;

//  viewport.addCube(0,0,0);
  viewport.addCube(-2,0,0);
  viewport.addCube(2,0,0);

//  viewport.addCube(0,0,0);
  viewport.addPlane(0,0,0);
//  viewport.addPlane(2,0,0);


  //Generate a gradient "star" picture, for texturing.
  gradientTexture := TPixel32.create; //Storage.
  mem.Add(gradientTexture);
  grad := TPixel32GeneratorGradient.Create; //Generator.
  try
    grad.init(gradientTexture,pixel.currentDrawShader);
    grad.ColorA := pixel.ColorSetAValue(gspWhite,120);
    grad.ColorB := gspBlack;
    grad.ShiftGradient := -50;
    grad.RorateGradient := 0;
    grad.process;
  finally
    FreeAndNil(grad); //No more need generator.
  end;


  //Build a "scotish" shader (squared motif, with no rotation (screen oriented))
  scot := TPixel32ShaderSquaredMotif.Create;
  mem.Add(scot);


  //Build a classic shader, alpha enabled.
  colShader := TPixel32ColorShader.create;
  colShader.ColorData := TP32Rec(pixel.colorSetAValue(gspRed,100));
  mem.Add(colshader);

  //Assign this texture a texture shader.
  clatextureShader_computed := TPixel32TextureShader.Create;
  clatextureShader_computed.Texture := gradientTexture;
  mem.Add(clatextureShader_computed);


  clatextureShader_imageGoldo := TPixel32TextureShader.Create;
  imageTexGoldo := TPixel32.create;
  imageTexGoldo.loadFromFile('../../../../../assets/avatar.bmp');
  //imageTexGoldo.alphaLayerResetByColor(gspWhite,0); //:)
  clatextureShader_imageGoldo.Texture := imageTexGoldo;
  mem.Add(clatextureShader_imageGoldo);
  mem.Add(imageTexGoldo);

  clatextureShader_imageDice := TPixel32TextureShader.Create;
  imageTexDice := TPixel32.create;
  imageTexDice.loadFromFile('../../../../../assets/alpha-dice.bmp');
  //It is a bmp, alpha value is lost. But on this image, it is easy to rebuild. ;)
  imageTexDice.alphaLayerResetByColor(gspBlack,0); //get tranparent alpha layer, from black pixel of piture.
  clatextureShader_imageDice.Texture := imageTexDice;
  mem.Add(clatextureShader_imageDice);
  mem.Add(imageTexDice);


  ShaderToyShader :=  TPixel32ShaderPlasma.create(100);
  mem.Add(ShaderToyShader);

  imageTexShader := TPixel32.create(64,64);
  imageTexShader.setDrawShader(ShaderToyShader);
  imageTexShader.fill(ShaderToyShader); //clear with shader :)
  TextureShader_ShaderToy := TPixel32TextureShader.create;
  TextureShader_ShaderToy.Texture := imageTexShader;
  mem.Add(imageTexShader);
  mem.Add(TextureShader_ShaderToy);

  //begin with texture shader.
//  TPixel32(viewport.TargetCanvas).setDrawShader(TPixel32ShaderColorTest.create(100));
//  TPixel32(viewport.TargetCanvas).setDrawShader(TPixel32ShaderPlasma.create(100));
//  TPixel32(viewport.TargetCanvas).setDrawShader(TPixel32ShaderRandomizer.create(100));
//  TPixel32(viewport.TargetCanvas).setDrawShader(TextureShader_ShaderToy);
//  TPixel32(viewport.TargetCanvas).setDrawShader(clatextureShader_imageDice);
//  TPixel32(viewport.TargetCanvas).setDrawShader(clatextureShader_imageGoldo);
  TPixel32(viewport.TargetCanvas).setDrawShader(colShader);

  viewport.CameraZ := 7;
  Application.OnIdle := appIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pixel);
  FreeAndNil(mem);
  FreeAndNil(viewport);
end;

procedure TForm1.appIdle(Sender: TObject; var Done: Boolean);
begin
  pixel.clear;
  pixel.color_pen := pixel.ColorSetAValue(gspBlue,20);
  if pixel.currentDrawShader = TextureShader_ShaderToy then
    imageTexShader.fill(ShaderToyShader); //Update texture via shader for each frame..
  viewport.Execute;
  TPixel32(viewport.TargetCanvas).CopyToDc(Image1.Canvas.handle);
  Image1.Repaint;
  Inc(FPS);
  Done := false;
end;

procedure TForm1.cbWireFrameClick(Sender: TObject);
begin
  viewport.wireframe := cbWireFrame.Checked;
  viewport.rasterFrame := cbRasterFrame.Checked;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  viewport.CameraZ := viewport.CameraZ + WheelDelta/100;
  if viewport.CameraZ<4 then
    viewport.CameraZ := 4;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Image1.Picture.Bitmap.SetSize(Width,Height);
  pixel.resize(Width,Height);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  viewport.Projection := TViewportProjectionType(RadioGroup1.ItemIndex);
end;

procedure TForm1.TimerFPSTimer(Sender: TObject);
begin
 Caption := 'FPS : '+IntToStr(FPS);
 FPS := 0;
end;

end.
