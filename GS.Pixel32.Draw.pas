unit GS.Pixel32.Draw;

interface

uses classes,sysutils,
     GS.Geometry,
     GS.Geometry.Mesh2d,
     GS.Assets,
     GS.Pixel,
     GS.Pixel.Draw,
     GS.Pixel32,
     GS.Pixel32.Fragments;

Type

TPixel32AssetImageSource = Class(TGSAssetImageSource)
protected
  fshader : TPixel32FragmentShaderTextureColor;
public
  Destructor Destroy; override;
  function GetShader : TPixel32FragmentShaderTextureColor; virtual;
End;


TPixel32Image = class(TPixelImage)
protected
  FAssetImageSourceAsPixel32One : TPixel32AssetImageSource;
public
  Constructor Create(asset :  TGSAssetImageSource); override;

  function getShader : iPixShader; override;
end;


implementation



{ TPixel32AssetImageSource }

destructor TPixel32AssetImageSource.Destroy;
var l : TPixel32;
begin
  if Assigned(fshader) then
  begin
    l := fshader.Texture;
    FreeAndNil(l);
    FreeAndNil(fshader);
  end;
  inherited;
end;

function TPixel32AssetImageSource.GetShader: TPixel32FragmentShaderTextureColor;
begin
  if not(Assigned(fshader)) then
  begin
    fshader :=  TPixel32FragmentShaderTextureColor.Create;
    fshader.Texture := TPixel32.create(Image.Width,Image.Height);
    Image.BinaryData.Position := 0;
    fshader.Texture.LoadFromARGB32FormatStream(Image.BinaryData);
  end;
  result := fshader;
end;

{ TPixel32Image }

constructor TPixel32Image.Create(asset: TGSAssetImageSource);
begin
  inherited Create(asset);
  Assert(asset is TPixel32AssetImageSource);
  FAssetImageSourceAsPixel32One := TPixel32AssetImageSource(asset);
end;

function TPixel32Image.getShader: iPixShader;
begin
  Assert(Assigned(FAssetImageSourceAsPixel32One));
  result := FAssetImageSourceAsPixel32One.GetShader;
end;

end.
