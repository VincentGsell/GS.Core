unit fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Graphics,
  FMX.StdCtrls, FMX.Controls.Presentation,
  System.Math.Vectors,FMX.Layouts, FMX.Menus,
  System.Generics.Collections,
  GS.Asserts,
  GS.Assets,
  GS.Geometry.Graph;

type
  TForm42 = class(TForm)
    Image1: TImage;
    PopupMenu1: TPopupMenu;
    MenuItem1: TMenuItem;
    Selection1: TSelection;
    Image2: TImage;
    Panel1: TPanel;
    Label1: TLabel;
    Selection2: TSelection;
    ArcDial1: TArcDial;
    procedure FormCreate(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Selection2Track(Sender: TObject);
    procedure ArcDial1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    lMouse : TPointf;
    lShift : TshiftState;

    GB : TGraphBoard;
    cam : TGraphCam;
    theAsset : TGSAssetSquareMesh; //the uniq asset we need, for all square
    bm1,bm2 : TBitmap; //Native FMX bmp.

    procedure UpdateCamView;
  end;

  TStrucShape = packed record
    shape : TGraphShape;
    scale : TPointf;
    pos : TPointf;
    angle : Single;
    color : TAlphaColor;
  end;


const ShapeCount = 100;

var
  Form42: TForm42;
  shapes : Array[0..ShapeCount-1] of TStrucShape;


  procedure paintProcess(aShapeList : TList<TGraphShape>; aCanvas : TCanvas; EraseColor : TAlphaColor; Const Erase : Boolean = true);
  function meshToPol(Source : TGraphShape) : TPolygon;


implementation

{$R *.fmx}

procedure TForm42.ArcDial1Change(Sender: TObject);
begin
  Selection2.RotationAngle := -ArcDial1.Value;
  Selection2Track(Sender);
end;

procedure TForm42.FormCreate(Sender: TObject);
var i : integer;
    s : single;
begin
  //Initialize view
  bm1 := TBitmap.Create(Width,Height);
  bm2 := TBitmap.Create(round(Selection1.Width),Round(Selection1.Height));
  Image1.Bitmap := bm1;
  Image1.Bitmap.Clear(TAlphaColorRec.Black);
  Image1.Width := Image1.Bitmap.Width;
  Image1.Height := Image1.Bitmap.Height;

  //Init. graph board.
  GB := TGraphBoard.Create;
  TheAsset := TGSAssetSquareMesh.Create;
  for I := 0 to length(shapes)-1 do
  begin
    shapes[i].shape := GB.AddShapeFromAsset(theAsset);
    s := 5+Round(10);
    shapes[i].scale := pointf(s,s);
    shapes[i].pos := pointf(Random(Screen.Width),Random(Screen.Height));
    shapes[i].angle := Random(360);
  end;

  //Camera's view
  Image2.Bitmap := bm2;

  //Camera setup
  cam := TGraphCam.Create(Selection1.Width,Selection1.Height);
end;


procedure TForm42.FormDestroy(Sender: TObject);
begin
  FreeAndNil(GB);
  FreeAndNil(Cam);
  FreeAndNil(TheAsset);
  FreeAndNil(bm1);
  FreeAndNil(bm2);
end;

procedure TForm42.FormResize(Sender: TObject);
var i : integer;
begin
  Image1.Bitmap.Resize(Width,Height);
  for I := 0 to length(shapes)-1 do
  begin
    shapes[i].shape.ResetMeshFromAsset;
    shapes[i].shape.Mesh.Scale(shapes[i].scale.X,shapes[i].scale.Y);
    shapes[i].shape.Mesh.Modify(shapes[i].pos.X,shapes[i].pos.Y,shapes[i].angle);
  end;

  paintProcess(GB.ShapeList,Image1.Bitmap.Canvas,TAlphaColorRec.Black);
  UpdateCamView;
end;

function meshToPol(Source : TGraphShape) : TPolygon;
var i,h : integer;
begin
  h := length(Source.Mesh.vertices);
  SetLength(result,h);
  for i:= 0 to h-1 do begin
    Result[i].X := Source.Mesh.vertices[i].x;
    Result[i].Y := Source.Mesh.vertices[i].Y;
  end;
end;

procedure paintProcess(aShapeList : TList<TGraphShape>; aCanvas : TCanvas;EraseColor : TAlphaColor; Const Erase : Boolean = true);
var l : TGraphShape;
    i,j,h : integer;
begin
  AssertAssigned(aShapeList);

  if aShapeList.Count=0 then
    Exit;

  aCanvas.BeginScene;
  try
    if Erase then begin
      aCanvas.Fill.Color := EraseColor;
      aCanvas.FillRect(rectf(0,0,aCanvas.Width,aCanvas.Height),1.0);
    end;

    for i := 0 to aShapeList.Count-1 do begin
      l := aShapeList[i];
      h := Length(l.Mesh.vertices)-1;

      aCanvas.Stroke.Color := TAlphaColorRec.red;
      aCanvas.Stroke.Thickness := 1;

      for j:= 1 to h do begin
          aCanvas.DrawLine( pointf(l.Mesh.vertices[j-1].x,l.Mesh.vertices[j-1].y),
                                         pointf(l.Mesh.vertices[j].x,l.Mesh.vertices[j].y),
                                         0.2);

        aCanvas.DrawLine( pointf(l.Mesh.vertices[0].x,l.Mesh.vertices[0].y),
                                     pointf(l.Mesh.vertices[h].x,l.Mesh.vertices[h].y),
                                     0.2);

        aCanvas.Fill.Color := TAlphaColorRec.Blueviolet;
        aCanvas.FillPolygon(meshToPol(l),0.2);
      end;
    end;

  finally
    aCanvas.EndScene;
  end;

end;



procedure TForm42.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var l : TGraphShape;
    i : integer;
begin
  if GB.ShapeList.Count=0 then
    Exit;

  for I := 0 to length(shapes)-1 do
  begin
    shapes[i].shape.ResetMeshFromAsset;
    shapes[i].shape.Mesh.Scale(shapes[i].scale.X,shapes[i].scale.Y);
    shapes[i].shape.Mesh.Modify(shapes[i].pos.X,shapes[i].pos.Y,shapes[i].angle);
  end;

  LShift := Shift;
  LMouse := Pointf(X,Y);
  paintProcess(GB.ShapeList,Image1.Bitmap.Canvas,TAlphaColorRec.Black);
  UpdateCamView;
end;


procedure TForm42.Selection2Track(Sender: TObject);
var l : TGraphShape;
begin
  if GB.ShapeList.Count=0 then
    Exit;

  ArcDial1.Position.Point := Selection2.Position.Point + Pointf(Selection2.Width,0);

  UpdateCamView;
end;

procedure TForm42.UpdateCamView;
var l : TGraphShape;
    i,j,h : integer;
    SW,SH : single;
begin
  //Reset shape.
  for I := 0 to length(shapes)-1 do
  begin
    shapes[i].shape.ResetMeshFromAsset;
    shapes[i].shape.Mesh.Scale(shapes[i].scale.X,shapes[i].scale.Y);
    shapes[i].shape.Mesh.Modify(shapes[i].pos.X,shapes[i].pos.Y,shapes[i].angle);
  end;

  cam.setPosition(Selection2.Position.X+Selection2.RotationCenter.X*Selection2.width,Selection2.Position.Y+Selection2.RotationCenter.Y*Selection2.height);
  cam.setRotation(ArcDial1.Value);

  //Coords convert.
  for I := 0 to GB.ShapeList.Count-1 do
    for j := 0 to length(GB.ShapeList[i].mesh.Vertices)-1 do begin
      GB.ShapeList[i].Mesh.vertices[j].x := GB.ShapeList[i].Mesh.vertices[j].x - cam.Position.x;
      GB.ShapeList[i].Mesh.vertices[j].y := GB.ShapeList[i].Mesh.vertices[j].y - cam.Position.y;
    end;

  //Viewport deformation / view rendering bitmap
  SW := Image2.Bitmap.Width/Selection2.Width;
  SH := Image2.Bitmap.Height/Selection2.Height;
  for I := 0 to GB.ShapeList.Count-1 do
    for j := 0 to length(GB.ShapeList[i].mesh.Vertices)-1 do begin
      GB.ShapeList[i].Mesh.vertices[j].x := GB.ShapeList[i].Mesh.vertices[j].x * SW;
      GB.ShapeList[i].Mesh.vertices[j].y := GB.ShapeList[i].Mesh.vertices[j].y * SH;
    end;

  //transform by cam's matrix
  for I := 0 to GB.ShapeList.Count-1 do
      GB.ShapeList[i].Mesh.UpdateVertices(cam.Matrix);

  paintProcess(GB.ShapeList,Image2.Bitmap.Canvas, TAlphaColorRec.White, true);

end;

end.
