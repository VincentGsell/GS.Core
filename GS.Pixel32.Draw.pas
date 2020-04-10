unit GS.Pixel32.Draw;

interface

uses classes,sysutils,
     GS.Geometry,
     GS.Geometry.Mesh2d,
     GS.Assets,
     GS.Pixel32;

Type
TPixel32Drawable = class
protected
  FAsset : TGSAsset2DMeshedObject; //Pointer !
  fCurrentProcessedMesh : TGSRawMesh2D;
public
  Constructor Create(asset :  TGSAsset2DMeshedObject); reintroduce;
  Destructor Destroy; override;
  procedure ResetMeshFromAsset; virtual;

  procedure Draw(surface : TPixel32); virtual;

  property Mesh : TGSRawMesh2D read fCurrentProcessedMesh;
end;


TPixel32Shape = class(TPixel32Drawable)
private
  FAssetAsShape : TGSAssetShapeMesh; //Pointer !
public
  Constructor Create(asset :  TGSAssetShapeMesh); reintroduce;
//  Property MeshAsShape : TGSAssetShapeMesh read GetAssetAsShape;
end;

TPixel32Image = class(TPixel32Drawable)
private
  FAssetAsImageSource : TGSAssetImageSource; //Pointer !
public
  Constructor Create(asset :  TGSAssetImageSource); reintroduce;
//  Property ImageShape : TGSAssetSquareMesh read ...;
end;


implementation

{ TPixel32Drawable }

constructor TPixel32Drawable.Create(asset :  TGSAsset2DMeshedObject);
begin
  assert(Assigned(asset));
  inherited Create;
  FAsset := asset; //Original asset. Do not touch.
  fCurrentProcessedMesh := TGSRawMesh2D.Create;  //Mesh for display.
  ResetMeshFromAsset;
end;

destructor TPixel32Drawable.Destroy;
begin
  FreeAndNil(fCurrentProcessedMesh);
  inherited;
end;

procedure TPixel32Drawable.Draw(surface : TPixel32);
var i : integer;
    va,vb,vc,vua,vub,vuc : vec2;
begin
  for i := 0 to fCurrentProcessedMesh.getTriangleCount-1 do
  begin
    fCurrentProcessedMesh.Triangle(i,va,vb,vc,vua,vub,vuc);
    surface.setVertex(0,trunc(va.x),trunc(va.y),0,trunc(vua.x),trunc(vua.y));
    surface.setVertex(1,trunc(vb.x),trunc(vb.y),0,trunc(vub.x),trunc(vub.y));
    surface.setVertex(2,trunc(vc.x),trunc(vc.y),0,trunc(vuc.x),trunc(vuc.y));
    surface.rasterize;
  end;
end;

procedure TPixel32Drawable.ResetMeshFromAsset;
begin
  FAsset.MeshData.copy(fCurrentProcessedMesh);
end;


{ TPixel32Shape }

constructor TPixel32Shape.Create(asset: TGSAssetShapeMesh);
begin
  assert(assigned(asset));
  FAssetAsShape := asset;
  inherited create(FAssetAsShape);
end;

{ TPixel32Image }

constructor TPixel32Image.Create(asset: TGSAssetImageSource);
begin
  assert(assigned(asset));
  FAssetAsImageSource := asset;
  inherited create(FAssetAsImageSource.Shape);
end;

end.
