unit GS.Pixel32.RasterizePXLBridge;

interface


uses classes,
     sysutils,
     GS.Pixel32,
     PXL.Types,
     PXL.TypeDef,
     PXL.Surfaces,
     PXL.Rasterizer.SRT;

type
  TPixel32PxlBridge = class
    Class procedure TriangleRasterize(surface : TPixel32; a,b,c : TP32Vertex; Shader : TCustomPixelChHeShader);
  end;


implementation

{ TPixel32PxlBridge }

class procedure TPixel32PxlBridge.TriangleRasterize(surface : TPixel32; a, b,
  c: TP32Vertex; Shader: TCustomPixelChHeShader);
var l,t : TPixelSurface;
begin
  l :=  TPixelSurface.CreateForPixel32(surface.getSurfacePtr,surface.width,surface.height);
  t :=  TPixelSurface.CreateForPixel32( TCustomPixelChHeShader(shader).Texture.getSurfacePtr,
                                        TCustomPixelChHeShader(shader).Texture.width,
                                        TCustomPixelChHeShader(shader).Texture.height);
  try
    DrawTriangle( l,
                  nil,
                  point2f(a.x,a.y),
                  point2f(b.x,b.y),
                  point2f(c.x,c.y),
                  point2f(0,0),
                  point2f(1,0),
                  point2f(1,1),
                  gspBlack,
                  gspRed,
                  gspBlack,
                  IntRect(0,0,surface.width,surface.height),
                  false
                  );
  finally
//    FreeAndNil(l);
//    FreeAndNil(t);
  end;
end;

end.
