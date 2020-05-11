unit fmain.RasterPipeLine;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm7 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    cbWireFrame: TCheckBox;
    cbRasterFrame: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    TimerFPS: TTimer;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

procedure TForm7.FormCreate(Sender: TObject);
begin
{var grad : TPixel32GeneratorGradient;
    i,j : integer;
    l : TMesh3D;
begin
  mem := TObjectList.Create;

  pixel := TPixel32.create;     //surface for drwaing 3d stuff
  pixel.rasterMode := TSoftwareRasterizeOption.roZBuffer;
  viewport := TView3d.Create; //virtual window on 3d world.
  viewport.TargetCanvas := pixel;
  viewport.CameraZ := -15;
  viewport.wireFrame := false;
  viewport.rasterFrame := true;

  for i := -2 to 2 do
  begin
    viewport.addCube(i,0,0);
  end;

  viewport.addPlane(0,2,0);
  viewport.addPlane(0,-2,0);

  //Generate a gradient "star" picture, for texturing.
  gradientTexture := TPixel32.create; //Storage.
  mem.Add(gradientTexture);
  grad := TPixel32GeneratorGradient.Create; //Generator.
  try
    grad.init(gradientTexture);
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
  colShader := TPixel32FragmentShaderFlatColor.create;
  colShader.Color := TP32Rec(pixel.colorSetAValue(gspRed,100)).Color;
  mem.Add(colshader);

  //Assign this texture a texture shader.
  clatextureShader_computed := TPixel32FragmentShaderTextureColor.Create;
  clatextureShader_computed.Texture := gradientTexture;
  mem.Add(clatextureShader_computed);

  clatextureShader_imageGoldo := TPixel32FragmentShaderTextureColor.Create;
  imageTexGoldo := TPixel32.create;
  imageTexGoldo.loadFromFile('../../../../../assets/avatar.bmp');
  clatextureShader_imageGoldo.Texture := imageTexGoldo;
  mem.Add(clatextureShader_imageGoldo);
  mem.Add(imageTexGoldo);

  clatextureShader_imageCrate := TPixel32FragmentShaderTextureColor.Create;
  imageTexCrate := TPixel32.create;
  imageTexCrate.loadFromFile('../../../../../assets/Crate_256.bmp');
  clatextureShader_imageCrate.Texture := imageTexCrate;
  mem.Add(clatextureShader_imageCrate);
  mem.Add(imageTexCrate);

  clatextureShader_imageDice := TPixel32FragmentShaderTextureColor.Create;
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
  imageTexShader.setFragmentShader(ShaderToyShader);
  imageTexShader.fill; //clear with shader :)
  TextureShader_ShaderToy := TPixel32FragmentShaderTextureColor.create;
  TextureShader_ShaderToy.Texture := imageTexShader;
  mem.Add(imageTexShader);
  mem.Add(TextureShader_ShaderToy);


  TPixel32(viewport.TargetCanvas).setFragmentShader(clatextureShader_imageCrate);

  viewport.CameraZ := 7;
  Application.OnIdle := appIdle;
}
end;

end.
