unit GS.Pixel.Draw;

interface

uses classes,sysutils,
     GS.Pixel,
     GS.Geometry,
     GS.Geometry.Mesh2d,
     GS.Assets;

Type
TPixelDrawable = class(TPixelInterfacedObject, iPixDrawable)
protected
  FAsset : TGSAsset2DMeshedObject; //Pointer !
  fCurrentProcessedMesh : TGSRawMesh2D;

public
  Constructor Create(asset :  TGSAsset2DMeshedObject); reintroduce;
  Destructor Destroy; override;
  procedure ResetMeshFromAsset; virtual;
  function Asset : TGSAsset2DMeshedObject; virtual;

  //iPixDrawable;
  function Mesh : TGSRawMesh2D; virtual;
  function getShader : iPixShader; virtual;

end;

TPixelDrawTools = class
public
  class procedure Draw(surface : iPixSurface; drawObject : iPixDrawable);
end;

TPixelShape = class(TPixelDrawable)
private
  FAssetAsShape : TGSAssetShapeMesh; //Pointer !
public
  Constructor Create(asset :  TGSAssetShapeMesh); reintroduce; virtual;
end;

TPixelImage = class(TPixelDrawable)
private
  FAssetAsImageSource : TGSAssetImageSource; //Pointer !;
public
  Constructor Create(asset :  TGSAssetImageSource); reintroduce; virtual;
  property ImageSourceAsset : TGSAssetImageSource read FassetAsImagesource;
end;


implementation

USES GS.Pixel32.Rasterize;

{ TPixelDrawable }

function TPixelDrawable.Asset: TGSAsset2DMeshedObject;
begin
  result := FAsset;
end;

constructor TPixelDrawable.Create(asset :  TGSAsset2DMeshedObject);
begin
  assert(Assigned(asset));
  inherited Create;
  FAsset := asset; //Original asset. Do not touch.
  fCurrentProcessedMesh := TGSRawMesh2D.Create;  //Mesh for display.
  ResetMeshFromAsset;
end;

destructor TPixelDrawable.Destroy;
begin
  FreeAndNil(fCurrentProcessedMesh);
  inherited;
end;

function TPixelDrawable.getShader: iPixShader;
begin
  result := nil;
end;

function TPixelDrawable.Mesh: TGSRawMesh2D;
begin
  result := fCurrentProcessedMesh;
end;

procedure TPixelDrawable.ResetMeshFromAsset;
begin
  FAsset.MeshData.copy(fCurrentProcessedMesh);{ TODO : HERE }
end;


{ TPixelShape }

constructor TPixelShape.Create(asset: TGSAssetShapeMesh);
begin
  assert(assigned(asset));
  FAssetAsShape := asset;
  inherited create(FAssetAsShape);
end;

{ TPixelImage }

constructor TPixelImage.Create(asset: TGSAssetImageSource);
begin
  assert(assigned(asset));
  FAssetAsImageSource := asset;
  inherited create(FAssetAsImageSource.Shape);
end;

{ TPixelDrawHelper }

class procedure TPixelDrawTools.Draw(surface : iPixSurface; drawObject : iPixDrawable);
var i : integer;
    va,vb,vc,
    vua,vub,vuc : vec2;
    ca,cb,cc : vec4;
    l : iPixShader;
begin
  assert(assigned(drawObject));
  assert(assigned(surface));
  surface.beginDraw;
  l := drawObject.getShader;
  if assigned(l) then
    surface.setFragmentShader(l);
  for i := 0 to drawObject.Mesh.getTriangleCount-1 do
  begin
    drawObject.Mesh.Triangle(i,va,vb,vc,vua,vub,vuc,ca,cb,cc);
    surface.setVertice(0,va.x,va.y);
    surface.setVerticeUV(0,vua.x,vua.y);
    surface.setVerticeColor(0,ca.r,ca.g,ca.b,ca.a);
    surface.setVertice(1,vb.x,vb.y);
    surface.setVerticeUV(1,vub.x,vub.y);
    surface.setVerticeColor(1,cb.r,cb.g,cb.b,cb.a);
    surface.setVertice(2,vc.x,vc.y);
    surface.setVerticeUV(2,vuc.x,vuc.y);
    surface.setVerticeColor(2,cc.r,cc.g,cc.b,cc.a);
    surface.rasterize;
  end;
  surface.endDraw;
end;


end.
