unit GS.Pixel32.Draw;

interface

uses classes,sysutils,
     GS.Pixel,
     GS.Pixel.Draw,
     GS.Geometry,
     GS.Geometry.Mesh2d,
     GS.Assets,
     GS.Pixel32;

Type
TPixel32Image = class(TPixelImage)
protected
  procedure Init; Override;
public
end;


implementation


{ TPixel32Image }

procedure TPixel32Image.Init;
var t : TPixel32TextureShader;
begin
//  t :=  TPixel32TextureShader.Create;

end;

end.
