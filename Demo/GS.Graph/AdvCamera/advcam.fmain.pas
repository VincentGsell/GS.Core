unit advcam.fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Objects,
  FMX.TabControl,
  System.Generics.Collections,
  System.Math.Vectors,
  GS.Asserts,
  GS.Assets,
  GS.Geometry.Graph, FMX.StdCtrls;


const
  CST_LEVEL_SIZE = 1000;
type

  TLevelItem = record
    elem : char;
    mShape : TGraphShape;
  end;
  TForm42 = class(TForm)
    TabControl1: TTabControl;
    TabItem_playground: TTabItem;
    TabItem_ed: TTabItem;
    Image1: TImage;
    Memo1: TMemo;
    Panel1: TPanel;
    Selection1: TSelection;
    procedure FormCreate(Sender: TObject);
    procedure TabControl1Resize(Sender: TObject);
    procedure TabControl1Change(Sender: TObject);
  private
    { Private declarations }
    procedure internalLoadLevel;
    procedure internalPaintProcess(aCanvas : TCanvas;EraseColor : TAlphaColor; Const Erase : Boolean = true);
    procedure internalUpdateCamView;

    procedure internalAppIdle(sender : TObject; var Done : Boolean);
  public
    { Public declarations }

    GB : TGraphBoard;
    cam : TGraphCam;
    theAsset : TGSAssetSquareMesh; //the uniq asset we need, for all square
    bm : TBitmap; //Native FMX bmp.

    level : Array[0..CST_LEVEL_SIZE, 0..CST_LEVEL_SIZE] of TLevelItem;
    levelwidth,levelheight : single;
  end;

var
  Form42: TForm42;

implementation

{$R *.fmx}

procedure TForm42.FormCreate(Sender: TObject);
var i : integer;
    s : single;
begin
  //Initialize view
  bm := TBitmap.Create(clientWidth,clientHeight);
  Image1.Bitmap := bm;

  //Init. graph board.
  GB := TGraphBoard.Create;
  TheAsset := TGSAssetSquareMesh.Create;

  //Load level.
  InternalLoadLevel;

  //Camera setup
  cam := TGraphCam.Create(bm.Width,bm.Height);

  Application.OnIdle := internalAppIdle;
end;

procedure TForm42.internalAppIdle(sender: TObject; var Done: Boolean);
begin
  internalUpdateCamView;
  Done := false;
end;

procedure TForm42.internalLoadLevel;
var i,j : integer;
    l : TGraphShape;
begin
  levelwidth := 1;
  levelheight := 1;
  for j := 0 to CST_LEVEL_SIZE do for i :=0 to CST_LEVEL_SIZE do level[i,j].elem := ' ';
  for j:= 0 to Memo1.Lines.Count-1 do
    if length(Memo1.lines[j])>0 then
      for i := 1 to length(Memo1.lines[j]) do
        if Memo1.Lines[j][i]<>' ' then begin
          level[i-1,j].elem := Memo1.Lines[j][i];
          l := GB.AddShapeFromAsset(theAsset);
          level[i-1,j].mShape := l;
          if levelwidth<i then
            levelwidth := i;
          if levelheight<j then
            levelheight := j;
        end;
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

procedure TForm42.internalPaintProcess(aCanvas : TCanvas;EraseColor : TAlphaColor; Const Erase : Boolean = true);
var l : TGraphShape;
    i,j,h : integer;
    c : integer;
begin
  c := 0;
  aCanvas.BeginScene;
  try
    if Erase then begin
      aCanvas.Fill.Color := EraseColor;
      aCanvas.FillRect(rectf(0,0,aCanvas.Width,aCanvas.Height),1.0);
    end;

    for j := 0 to CST_LEVEL_SIZE do begin
      for i := 0 to CST_LEVEL_SIZE do begin
        if level[i,j].elem<>' ' then begin
          l := level[i,j].mShape;
          h := Length(l.Mesh.vertices)-1;

          aCanvas.Stroke.Color := TAlphaColorRec.red;
          aCanvas.Fill.Color := TAlphaColorRec.red;
          aCanvas.Stroke.Thickness := 1;


          for h:= 1 to h do begin
            aCanvas.DrawLine( pointf(l.Mesh.vertices[h-1].x,l.Mesh.vertices[h-1].y),
                                           pointf(l.Mesh.vertices[h].x,l.Mesh.vertices[h].y),
                                           0.2);

            aCanvas.DrawLine( pointf(l.Mesh.vertices[0].x,l.Mesh.vertices[0].y),
                                       pointf(l.Mesh.vertices[h].x,l.Mesh.vertices[h].y),
                                       0.2);

            aCanvas.Fill.Color := TAlphaColorRec.Blueviolet;
          end;

          aCanvas.FillPolygon(meshToPol(l),0.2);
          inc(c);
        end;
      end;
    end;

  finally
    aCanvas.EndScene;
  end;

  Caption := IntToStr(c);

end;


procedure TForm42.internalUpdateCamView;
var l : TGraphShape;
    i,j,h : integer;
    SW,SH : single;
begin
  //Reset shape.
  for j := 0 to CST_LEVEL_SIZE do begin
    for i := 0 to CST_LEVEL_SIZE do begin
      if level[i,j].elem<>' ' then begin
        l := level[i,j].mShape;
        l.ResetMeshFromAsset;
        l.Mesh.SetUpRect(10,10);
        l.Mesh.Modify(i*10,j*10,0,1,1);
      end;
    end;
  end;

  cam.setPosition(((Selection1.Position.X+Selection1.RotationCenter.X)),(Selection1.Position.Y+Selection1.RotationCenter.Y));
  //cam.setRotation(ArcDial1.Value);

  //Coords convert,
  //Viewport deformation / view rendering bitmap
  //transform by cam's matrix
  SW := Panel1.Width/Selection1.Width;
  SH := Panel1.Height/Selection1.Height;
  for j := 0 to CST_LEVEL_SIZE do begin
    for i := 0 to CST_LEVEL_SIZE do begin
      if level[i,j].elem<>' ' then begin
        l := level[i,j].mShape;
        for h := 0 to Length(l.Mesh.vertices)-1 do begin
          l.Mesh.vertices[h].x := l.Mesh.vertices[h].x - cam.Position.x;
          l.Mesh.vertices[h].y := l.Mesh.vertices[h].y - cam.Position.y;
          l.Mesh.vertices[h].x := l.Mesh.vertices[h].x * SW;
          l.Mesh.vertices[h].y := l.Mesh.vertices[h].y * SH;
        end;
        l.Mesh.UpdateVertices(cam.Matrix);
      end;
    end;
  end;

  internalPaintProcess(Image1.Bitmap.Canvas,TAlphaColorRec.White);
end;

procedure TForm42.TabControl1Change(Sender: TObject);
begin
  //Load level.
  InternalLoadLevel;
end;

procedure TForm42.TabControl1Resize(Sender: TObject);
begin
  if Assigned(bm) then
    Image1.Bitmap.Resize(ClientWidth,ClientHeight);
  Panel1.Width := ClientWidth/5 ;
  Panel1.Height := ClientHeight/5 ;
  Panel1.Position.X := ClientWidth-Panel1.Width-20;
  Panel1.Position.Y := ClientHeight-Panel1.Height-20;
end;


end.
