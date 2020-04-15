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

  //Called by constructor, must be override by tech. provider.
  procedure Init; virtual; abstract;
public
  Constructor Create(asset :  TGSAsset2DMeshedObject); reintroduce;
  Destructor Destroy; override;
  procedure ResetMeshFromAsset; virtual;


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
  Constructor Create(asset :  TGSAssetShapeMesh); reintroduce;
  procedure Init; override;
end;

TPixelImage = class(TPixelDrawable)
private
  FAssetAsImageSource : TGSAssetImageSource; //Pointer !;
public
  Constructor Create(asset :  TGSAssetImageSource); reintroduce;
  property ImageSourceAsset : TGSAssetImageSource read FassetAsImagesource;
end;


implementation

USES GS.Pixel32.Rasterize;

{ TPixelDrawable }

constructor TPixelDrawable.Create(asset :  TGSAsset2DMeshedObject);
begin
  assert(Assigned(asset));
  inherited Create;
  FAsset := asset; //Original asset. Do not touch.
  fCurrentProcessedMesh := TGSRawMesh2D.Create;  //Mesh for display.
  ResetMeshFromAsset;
  Init;
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
  FAsset.MeshData.copy(fCurrentProcessedMesh);
end;


{ TPixelShape }

constructor TPixelShape.Create(asset: TGSAssetShapeMesh);
begin
  assert(assigned(asset));
  FAssetAsShape := asset;
  inherited create(FAssetAsShape);
end;

procedure TPixelShape.Init;
begin
  //..
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
    va,vb,vc,vua,vub,vuc : vec2;
    l : iPixShader;
begin
  assert(assigned(drawObject));
  assert(assigned(surface));
  surface.beginDraw;
  l := drawObject.getShader;
  if assigned(l) then
    surface.setDrawShader(l);
  for i := 0 to drawObject.Mesh.getTriangleCount-1 do
  begin
    drawObject.Mesh.Triangle(i,va,vb,vc,vua,vub,vuc);
    surface.setVertex(0,trunc(va.x),trunc(va.y),0,trunc(vua.x),trunc(vua.y));
    surface.setVertex(1,trunc(vb.x),trunc(vb.y),0,trunc(vub.x),trunc(vub.y));
    surface.setVertex(2,trunc(vc.x),trunc(vc.y),0,trunc(vuc.x),trunc(vuc.y));
    surface.rasterize;
  end;
  surface.endDraw;
end;


end.
