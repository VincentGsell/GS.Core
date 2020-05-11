unit effectsDemo.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,Vcl.StdCtrls, Vcl.Samples.Spin, Vcl.ExtCtrls, Vcl.ComCtrls,
  GS.Pixel32,
  GS.Pixel32.Win,
  GS.Pixel32.Effect.Gradient,
  GS.Pixel32.Effect.Classics;

type
  TForm6 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Image1: TImage;
    ListBox1: TListBox;
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
     pixelImage : TPixel32; //to store the image which will be "effecterized".
     pixelImage2 : TPixel32; //to store the image which will be "effecterized" for blend..
     pixel : TPixel32; //Primary surface.
     pixEffect : TPixel32SurfaceEffectClassicOperations; //classic Op.
     pixEffectBlend : TPixel32SurfaceEffectClassicBlendOps; //classic BlendOp.
     pixErase : TPixel32SurfaceEffectClear; //this one is used in TPixel32.Clear :)
     pixGradient : TPixel32GeneratorGradient; //this one is used as helper to generate texture in raster demo.

     procedure imageRepaint;
  end;

var
  Form6: TForm6;

implementation

{$R *.dfm}

procedure TForm6.FormCreate(Sender: TObject);
var i : integer;
begin
  pixelImage := TPixel32.create;
  pixelImage.loadFromFile('../../../../../assets/fightGamePirateScape.bmp');

  pixelImage2 := TPixel32.create;
  pixelImage2.loadFromFile('../../../../../assets/fightGamePirateScape2.bmp');

  pixel := TPixel32.create;
  pixelImage.copyTo(pixel);

  pixEffect := TPixel32SurfaceEffectClassicOperations.Create;
  pixEffect.init(pixel);

  pixEffectBlend := TPixel32SurfaceEffectClassicBlendOps.Create;
  pixEffectBlend.init(pixel);

  pixErase := TPixel32SurfaceEffectClear.Create;
  pixErase.init(pixel);

  pixGradient := TPixel32GeneratorGradient.Create;
  pixGradient.init(pixel);

  imageRepaint;

  for i:= 0 to Length(cPixelUnitOpEN)-1 do
  begin
    ListBox1.AddItem(cPixelUnitOpEN[TPixelUnitOp(i)],nil);
  end;

  for i:= 0 to Length(cPixelUnitBlendOpEN)-1 do
  begin
    ListBox1.AddItem(cPixelUnitBlendOpEN[TPixelUnitBlendOp(i)],nil);
  end;
end;

procedure TForm6.FormDestroy(Sender: TObject);
begin
  freeAndNil(pixelImage);
  freeAndNil(pixelImage2);
  freeAndNil(pixel);
  freeAndNil(pixEffect);
  freeAndNil(pixEffectBlend);
  freeAndNil(pixErase);
  freeAndNil(pixGradient);
end;

procedure TForm6.imageRepaint;
begin
  Image1.Picture.Bitmap.SetSize(pixel.width,pixel.height);
  pixel.CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
  Image1.Repaint;
end;

procedure TForm6.ListBox1Click(Sender: TObject);
var op : TPixelUnitOp;
    opb : TPixelUnitBlendOp;
    ls : string;
    lev : single;
begin
  if ListBox1.ItemIndex<0 then
    exit;

  ls := ListBox1.Items[ListBox1.ItemIndex];
  lev := TrackBar1.Position/100;

  if opNameToOp(ls,op) then
  begin
    pixelImage.copyTo(pixel);
    pixEffect.setOpParameter(lev,op);
    pixEffect.process; //Apply effectively.
  end
  else
  if opNameToBlendOp(ls,opb) then
  begin
    pixelImage.copyTo(pixel);
    pixEffectBlend.setOpParameter(lev,opb,pixelImage2);
    pixEffectBlend.process; //Apply effectively.
  end
  else
  begin
    if ls = 'clear' then
      pixErase.process;
    if ls = 'gradient' then
    begin
      pixGradient.process;
    end;
    if ls = 'original' then
    begin
      pixelImage.copyTo(pixel);
    end;
  end;
  imageRepaint;
end;

end.
