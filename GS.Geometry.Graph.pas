unit GS.Geometry.Graph;

interface

uses classes,sysutils,
     Generics.Collections,
     GS.Asserts,
     GS.Geometry,
     GS.Geometry.Mesh2d,
     GS.Assets;

Type

iGraphItem = interface
end;

iGraphCam = interface
end;

iGraphBoard = interface
end;

TGraphItem = class(TInterfacedObject, iGraphItem)
protected
  FAsset : TGSAsset2DMeshedObject; //Pointer !
  fCurrentProcessedMesh : TGSRawMesh2D;

public
  Constructor Create(asset :  TGSAsset2DMeshedObject); reintroduce;
  Destructor Destroy; override;
  procedure ResetMeshFromAsset; virtual;
  function Asset : TGSAsset2DMeshedObject; virtual;

  function Mesh : TGSRawMesh2D; virtual;
end;

TGraphShape = class(TGraphItem)
private
  FAssetAsShape : TGSAssetShapeMesh; //Pointer !
public
  Constructor Create(asset :  TGSAssetShapeMesh); reintroduce; virtual;
end;


type

TGraphCam = Class(TInterfacedObject, iGraphCam) //2D cam;
private
  function GetPosition: vec2;
  function GetViewPort: vec2;
  function getAngle: single;
protected
  _TransformationMatrix : Mat3;

  _Position : vec2;
  _viewPort : vec2;
  _Angle : single;

  procedure updateMatrix;
public
  constructor Create(viewPortWidth, viewPortHeight : single); reintroduce;

  procedure setPosition(x,y : single);

  procedure moveUp(amount : single);
  procedure moveDown(amount : single);
  procedure moveLeft(amount : single);
  procedure moveRight(amount : single);

  procedure setRotation(newangle : single);

  property Matrix : Mat3 read _TransformationMatrix;

  property Position : vec2 read GetPosition;
  property Viewport : vec2 read GetViewPort;
  property Angle : single read getAngle;
End;



TGraphBoard = class(TInterfacedObject, iGraphBoard)
private
protected
  FProjMatrix : Mat3;
Public
  ShapeList : TObjectList<TGraphShape>;

  Constructor Create; virtual;
  Destructor Destroy; override;
  function AddShapeFromAsset(asset : TGSAssetShapeMesh) : TGraphShape;
end;


TGraphBoardResolver = class
protected
  FGraphBoard : TGraphBoard; //pointer
  FCamera : TGraphCam;
public
  ShapeList : TList<TGraphShape>; //Pointer on Graphboard shape list ? Or Not ? Opton ?
//  procedure process;
end;


implementation

{ TGraphItem }

function TGraphItem.Asset: TGSAsset2DMeshedObject;
begin
  result := FAsset;
end;

constructor TGraphItem.Create(asset :  TGSAsset2DMeshedObject);
begin
  assert(Assigned(asset));
  inherited Create;
  FAsset := asset; //Original asset. Do not touch.
  fCurrentProcessedMesh := TGSRawMesh2D.Create;  //Mesh for processing.
  ResetMeshFromAsset;
end;

destructor TGraphItem.Destroy;
begin
  FreeAndNil(fCurrentProcessedMesh);
  inherited;
end;

function TGraphItem.Mesh: TGSRawMesh2D;
begin
  result := fCurrentProcessedMesh;
end;

procedure TGraphItem.ResetMeshFromAsset;
begin
  FAsset.MeshData.copy(fCurrentProcessedMesh);
end;


{ TGraphItem }

constructor TGraphShape.Create(asset: TGSAssetShapeMesh);
begin
  AssertAssigned(asset);
  FAssetAsShape := asset;
  inherited create(FAssetAsShape);
end;


{ TGraphBoard }

constructor TGraphBoard.Create;
begin
  Inherited;
  ShapeList := TObjectList<TGraphShape>.Create(True);
end;

destructor TGraphBoard.Destroy;
begin
  FreeAndNil(ShapeList);
  inherited;
end;

function TGraphBoard.AddShapeFromAsset(asset: TGSAssetShapeMesh): TGraphShape;
begin
  AssertAssigned(asset);
  result := TGraphShape.Create(asset);
  ShapeList.Add(result);
end;


{ TGraphCam }

constructor TGraphCam.Create(viewPortWidth, viewPortHeight: single);
begin
  inherited Create;
  _TransformationMatrix := Mat3Identity;
  _viewPort := vec2.create(viewPortWidth,viewPortHeight);
  setPosition(0,0);
  _Angle := 0;
end;

function TGraphCam.getAngle: single;
begin
  result := _Angle;
end;

function TGraphCam.GetPosition: vec2;
begin
  result := _Position;
end;

function TGraphCam.GetViewPort: vec2;
begin
  result := _viewPort;
end;

procedure TGraphCam.moveDown(amount: single);
begin
  setPosition(_Position.X,_Position.Y + abs(amount));
end;

procedure TGraphCam.moveLeft(amount: single);
begin
  setPosition(_Position.X - abs(amount),_Position.Y);
end;

procedure TGraphCam.moveRight(amount: single);
begin
  setPosition(_Position.X + abs(amount),_Position.Y);
end;

procedure TGraphCam.moveUp(amount: single);
begin
  setPosition(_Position.X,_Position.Y - abs(amount));
end;

procedure TGraphCam.setPosition(x, y: single);
begin
  _Position.X := x;
  _Position.Y := y;
  updateMatrix;
end;

procedure TGraphCam.setRotation(newangle: single);
begin
  _Angle := newangle;
  updateMatrix;
end;

procedure TGraphCam.updateMatrix;
var translationMatrix : Mat3;
    rotationMatrix : Mat3;
begin
  rotationMatrix := Mat3CreateRotation(Angle*Pi/180);
  translationMatrix := Mat3CreateTranslation(_Position.X,_Position.Y);
  translationMatrix := translationMatrix.Inverse * rotationMatrix * Mat3CreateTranslation(Viewport.X/2,Viewport.Y/2);
  _TransformationMatrix := translationMatrix;
end;

end.

