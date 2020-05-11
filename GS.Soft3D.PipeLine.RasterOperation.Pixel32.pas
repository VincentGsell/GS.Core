unit GS.Soft3D.PipeLine.RasterOperation.Pixel32;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.PipeLine.RasterOperation,
     GS.Pixel32;

Type

  TS3DRasterOperationPixel32 = class(TS3DRasterOperation)
  public
    PixelSurface : TPixel32;
    //Must be implemented into surface enabled techno descendant (such as Pixel32)
    procedure BuildImage; Override;
  end;


implementation

{ TS3DRasterOperationPixel32 }

procedure TS3DRasterOperationPixel32.BuildImage;
begin
  PixelSurface.resize(FragShaderData.RasterData.Buffer.width,FragShaderData.RasterData.Buffer.height);
  //Transfering Pixel.
end;

end.
