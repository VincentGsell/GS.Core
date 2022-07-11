unit assetsMesh.fmain;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.ContNrs,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  GS.Assets,
  GS.Geometry,
  GS.Geometry.Mesh2D,
  GS.Geometry.Mesh2D.Tools,
  GS.Geometry.Triangulation,
  GS.Pixel.Draw,
  GS.Pixel32,
  GS.Pixel32.Types,
  GS.Pixel32.Win, Vcl.Menus;

type
  TShapeAssetManager = Class
  private
    flist : TObjectList;
    function GetAsset(Index: Uint32): TGSAssetShapeMesh;
    function GetAssetCount: Uint32;
    procedure SetAsset(Index: Uint32; const Value: TGSAssetShapeMesh);
  public
    Constructor Create; virtual;
    Destructor Destroy; override;

    function AssetFromMouseDrawing( data : Vec2sArray;
                                       var newAsset : TGSAssetShapeMesh;
                                       const TriangulationMethod : TTiangulationMethod = TTiangulationMethod.tmDelaunay) : boolean;

    procedure addAsset(asset : TGSAssetShapeMesh);
    property Assets[Index : Uint32] : TGSAssetShapeMesh read GetAsset Write SetAsset;
    property AssetCount : Uint32 read GetAssetCount;
  End;

  TForm3 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TimerFPS: TTimer;
    btnConvert: TButton;
    ListBox1: TListBox;
    btnCreate: TButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListBox2: TListBox;
    Label7: TLabel;
    chkDrawMode: TCheckBox;
    SaveDialog1: TSaveDialog;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    Button3: TButton;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    PopupMenu1: TPopupMenu;
    pmLoadMesh: TMenuItem;
    PopupMenu2: TPopupMenu;
    pmSaveMeshFile: TMenuItem;
    cbLineMode: TCheckBox;
    btnNewPoly: TButton;
    cbDrawBorder: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TimerFPSTimer(Sender: TObject);
    procedure btnConvertClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure btnCreateClick(Sender: TObject);
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure pmLoadMeshClick(Sender: TObject);
    procedure pmSaveMeshClick(Sender: TObject);
    procedure btnNewPolyClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    FPS : Uint32;
    pixel : TPixel32;
    assetManager : TShapeAssetManager;
    idxLoc, idxTriLoc : integer;
    movObjCoord : vec2;
    movObjCenter : vec2;
    movObj : TPixelShape;

    //draw list
    DrawList : Array of TPixelDrawable;

    //drawing manager.
    CurrentDrawData : Vec2sArray;
    CurrentPolyIndex : Integer;
    procedure addDrawPoint(x,y : single);
    procedure clearDraw;


    procedure OnAppIdle(Sender: TObject; var Done: Boolean);
  end;


var
  Form3: TForm3;

implementation

{$R *.dfm}

{ TForm3 }

procedure TForm3.addDrawPoint(x, y : Single);
var index : Uint32;
begin
  if length(CurrentDrawData)<CurrentPolyIndex then
    raise Exception.Create('CurrentPolyIndex overlflow bug.');
  index := Length(CurrentDrawData[CurrentPolyIndex]);
  SetLength(CurrentDrawData[CurrentPolyIndex],index+1);
  CurrentDrawData[CurrentPolyIndex][index].create(x,y);
end;

procedure TForm3.btnConvertClick(Sender: TObject);
var a,b,c : TGSAssetShapeMesh;
    sc,ind : Uint32;

begin
  Assert(Length(CurrentDrawData)>0);
  btnConvert.Enabled := false;
  a := nil;
  b := nil;
  c := nil;
  ind := 0;
  sc :=0;

  Label4.Caption := 'Delaumay Cliping : - ';
  if assetManager.AssetFromMouseDrawing(CurrentDrawData,a,TTiangulationMethod.tmDelaunay) then
  begin
    Label4.Caption := 'Delaumay Cliping : '+IntToStr(a.MeshData.getTriangleCount);
    sc := a.MeshData.getTriangleCount;
    ind := 1;
  end;

  Label5.Caption := 'Siebel : - ';
  if assetManager.AssetFromMouseDrawing(CurrentDrawData,b,TTiangulationMethod.tmSeibel) then
  begin
    Label5.Caption := 'Siebel : '+IntToStr(b.MeshData.getTriangleCount);
    if sc>b.MeshData.getTriangleCount then
    begin
      sc := b.MeshData.getTriangleCount;
      ind := 2;
    end;
  end;

  Label6.Caption := 'Bero : - ';
  if assetManager.AssetFromMouseDrawing(CurrentDrawData,c,TTiangulationMethod.tmBeRo) then
  begin
    Label6.Caption := 'Bero : '+IntToStr(c.MeshData.getTriangleCount);
    if sc>c.MeshData.getTriangleCount then
    begin
      ind := 3;
    end;
  end;

  if ind>0 then
  begin
    case ind of
      1:
      begin
        assetManager.addAsset(a);
        a.BordersCopyFrom(CurrentDrawData);
        ListBox1.AddItem('Asset '+IntToStr(assetManager.AssetCount),a);
        if assigned(b) then freeAndNil(b);
        if assigned(c) then freeAndNil(c);
      end;
      2:
      begin
        assetManager.addAsset(b);
        b.BordersCopyFrom(CurrentDrawData);
        ListBox1.AddItem('Asset '+IntToStr(assetManager.AssetCount),b);
        if assigned(a) then freeAndNil(a);
        if assigned(c) then freeAndNil(c);
      end;
      3:
      begin
        assetManager.addAsset(c);
        c.BordersCopyFrom(CurrentDrawData);
        ListBox1.AddItem('Asset '+IntToStr(assetManager.AssetCount),c);
        if assigned(a) then freeAndNil(a);
        if assigned(b) then freeAndNil(b);
      end;
    end;
    clearDraw;
  end
  else
  begin
    raise Exception.Create('no result');
    clearDraw;
  end;
end;

procedure TForm3.btnCreateClick(Sender: TObject);
var asset : TGSAssetShapeMesh;
    index : Uint32;
begin
  if ListBox1.ItemIndex>-1 then
  begin
    asset := TGSAssetShapeMesh(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    index := Length(DrawList);
    SetLength(DrawList,index+1);
    DrawList[index] := TPixelShape.Create(asset);
    ListBox2.AddItem(format('Shape %d (%d tri. | %d edge)',[index,DrawList[index].Mesh.getTriangleCount,length(DrawList[index].Asset.Borders)]),DrawList[index]);
    clearDraw;
  end;
end;

procedure TForm3.btnNewPolyClick(Sender: TObject);
begin
  CurrentPolyIndex := -1;
end;

procedure TForm3.Button1Click(Sender: TObject);
var asset : TGSAssetShapeMesh;
    l : TFileStream;
begin
  if ListBox1.ItemIndex>-1 then
  begin
    asset := TGSAssetShapeMesh(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    if SaveDialog1.Execute then
    begin
      l := TFileStream.Create(SaveDialog1.FileName,fmCreate);
      try
        asset.SaveToStream(l);
        ShowMessage(format('"%s" Saved',[SaveDialog1.FileName]));
      finally
        FreeAndNil(l);
      end;
    end;
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var asset : TGSAssetShapeMesh;
    l : TFileStream;
begin
  if OpenDialog1.Execute then
  begin
    l := TFileStream.Create(OpenDialog1.FileName,fmOpenRead);
    try
      asset := TGSAssetShapeMesh.Create;
      asset.LoadFromStream(l);
      asset.MeshData.fullRefreshBounding;
      assetManager.addAsset(asset);
      ListBox1.AddItem('Asset '+IntToStr(assetManager.AssetCount),asset);
    finally
      FreeAndNil(l);
    end;
  end;
end;

procedure TForm3.Button3Click(Sender: TObject);
var a,b,c : TPixelShape;
    i : integer;
    tr : TGS2DSubOpp;
begin
  a := nil;
  if ListBox2.SelCount = 2 then
  begin
    for I := 0 to ListBox2.Count-1 do
    begin
      if ListBox2.Selected[i] then
      begin
        a := TPixelShape(ListBox2.Items.Objects[i]);
        break;
      end;
    end;
    for I := ListBox2.Count-1 downto 0 do
    begin
      if ListBox2.Selected[i] then
      begin
        b := TPixelShape(ListBox2.Items.Objects[i]);
        break;
      end;
    end;

    c := TPixelShape.Create(TGSAssetShapeMesh(a.Asset));

Assert(false,'todo');
//    tr := TGS2DSubOpp.Create;
//    tr.Subject := a;


  end;
end;

procedure TForm3.clearDraw;
begin
  CurrentDrawData := nil;
  SetLength(CurrentDrawData,1); //At least 1.
  CurrentPolyIndex := 0;
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  pixel := TPixel32.create;
  pixel.color_pen := pixel.colorSetAValue(gspBlack,100);
  assetManager := TShapeAssetManager.Create;
  clearDraw;
  Application.OnIdle := OnAppIdle;
  FPS := 0;
end;

procedure TForm3.FormDestroy(Sender: TObject);
var p : TPixelDrawable;
begin
  for p in DrawList do
  begin
    p.Free;
  end;
  FreeAndNil(assetManager);
  FreeandNil(pixel);
end;

procedure TForm3.FormResize(Sender: TObject);
begin
  Image1.Picture.Bitmap.SetSize(Image1.width,Image1.height);
  pixel.resize(Image1.width,Image1.Height);
end;

procedure TForm3.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var i : integer;
    vi : UInt32;
begin
  if chkDrawMode.Checked then
  begin
    if CurrentPolyIndex <> length(CurrentDrawData)-1 then begin
      CurrentPolyIndex := length(CurrentDrawData);
      SetLength(CurrentDrawData,CurrentPolyIndex+1);
    end;
    addDrawPoint(x,y);
    btnConvert.Enabled := true;
  end
  else
  begin
//    if Button = TMouseButton.mbLeft then
    begin
      for i:= 0 to Length(DrawList)-1 do
      begin
        if DrawList[i].Mesh.isPointInside(x,y,vi) then
        begin
          movObj := TPixelShape(DrawList[i]);
          movObjCoord.x := x;
          movObjCoord.y := y;
          movObjCenter := movObj.Mesh.BoudingCenter;
          break;
        end;
      end;
    end;
  end;
end;

procedure TForm3.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var i : integer;
   vi : Uint32;
begin
  idxLoc := -1;
  idxTriLoc := -1;

  Label7.Caption := '...';

  if chkDrawMode.Checked then
  begin
    //Drawing part.
    if ssLeft in Shift then
      addDrawPoint(x,y)
    else
    begin
      for i:= 0 to Length(DrawList)-1 do
      begin
        if DrawList[i].Mesh.isPointInside(x,y,vi) then
        begin
          Label7.Caption := 'Shape '+intTostr(i)+'(triangle '+intToStr(vi)+')';;
          idxLoc :=i;
          idxTriLoc := vi;
          break;
        end;
      end;
    end;
  end
  else
  begin
    //moving part
    if Assigned(movObj) then
    begin
      if ssLeft in Shift then
      begin
        movObj.Mesh.Pan(x - movObjCoord.x,y-movObjCoord.y);
        movObjCoord.x := x;
        movObjCoord.y := y;
      end;
      if ssRight in Shift then
      begin
        movObj.Mesh.Pan(-movObjCenter.x,-movObjCenter.y);
        movObj.Mesh.Rotate(x - movObjCoord.x);
        movObj.Mesh.Pan(movObjCenter.x,movObjCenter.y);
        movObjCoord.x := x;
        movObjCoord.y := y;
      end;
      if ssMiddle in Shift then
      begin
        movObj.Mesh.Pan(-movObjCenter.x,-movObjCenter.y);
        movObj.Mesh.Scale(1+(x - movObjCoord.x)/100,1+(x - movObjCoord.x)/100);
        movObj.Mesh.Pan(movObjCenter.x,movObjCenter.y);
        movObjCoord.x := x;
        movObjCoord.y := y;
      end;
    end;
  end;
end;

procedure TForm3.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  movObj := nil;
  if chkDrawMode.Checked then
    if not cbLineMode.Checked then
      CurrentPolyIndex := -1;
end;

procedure TForm3.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex>-1 then
  begin
    btnCreate.Enabled := true;
  end;
end;

procedure TForm3.OnAppIdle(Sender: TObject; var Done: Boolean);
const cst_ct = 5;
var i,j,k : integer;
    a,b,c : vec2;
    aa,bb,cc : vec2;
    ca,cb,cco : vec4;
    bound : vec4;
    ct : vec2;
begin
  pixel.clear;
  Done := false;

  for i:= 0 to Length(DrawList)-1 do
  begin
    pixel.color_pen := pixel.colorSetAValue(gspBlue,10);
    pixel.draw(DrawList[i]);

    pixel.beginDraw;
    //Draw triangle.
    //if  CheckBox1.Checked then
    begin
      for j := 0 to DrawList[i].Mesh.getTriangleCount-1 do
      begin
        DrawList[i].Mesh.Triangle(j,a,b,c,aa,bb,cc,ca,cb,cco);
        pixel.color_pen := pixel.colorSetAValue(gspBlack,20);
        if idxLoc = i then
          if idxTriLoc = j then
          begin
            pixel.color_pen := pixel.colorSetAValue(gspGreen,60);
            pixel.setVertice(0,trunc(a.x),trunc(a.y));
            pixel.setVertice(1,trunc(b.x),trunc(b.y));
            pixel.setVertice(2,trunc(c.x),trunc(c.y));
            pixel.rasterize;
          end;

        if  CheckBox1.Checked then
        begin
          pixel.moveTo(trunc(a.x),trunc(a.y));
          pixel.lineTo(trunc(b.x),trunc(b.y));
          pixel.lineTo(trunc(c.x),trunc(c.y));
          pixel.lineTo(trunc(a.x),trunc(a.y));
        end;
      end;
    end;

   //Draw border.
   if (cbDrawBorder.Checked) and (length(DrawList[i].Asset.Borders)>0) then begin
     pixel.color_pen := pixel.colorSetAValue(gspRed,200);
     for k := 0 to length(DrawList[i].Asset.Borders)-1 do begin
       if length(DrawList[i].Asset.Borders[k])=0 then
         continue;
       pixel.moveTo(trunc(DrawList[i].Asset.Borders[k][0].x),trunc(DrawList[i].Asset.Borders[k][0].y));
       for j:= 1 to length(DrawList[i].Asset.Borders[k])-1 do
         pixel.lineTo(trunc(DrawList[i].Asset.Borders[k][j].x),trunc(DrawList[i].Asset.Borders[k][j].y));
       pixel.lineto(trunc(DrawList[i].Asset.Borders[k][0].x),trunc(DrawList[i].Asset.Borders[k][0].y));
     end;
   end;

    //Draw boundingbox
    //if  CheckBox1.Checked then
    begin
      pixel.color_pen := pixel.colorSetAValue(gspRed,20);
      bound := DrawList[i].Mesh.BoundingBox;
      pixel.rectangle(trunc(bound.Left),trunc(bound.top),trunc(bound.right),trunc(bound.bottom));
      ct := DrawList[i].Mesh.BoudingCenter;
      pixel.rectangle(trunc(ct.x-cst_ct),trunc(ct.y-cst_ct),trunc(ct.x+cst_ct),trunc(ct.y+cst_ct));
    end;
    pixel.endDraw;
  end;

  //User drawing...
  if length(CurrentDrawData)>0 then begin
    for j := 0 to length(CurrentDrawData)-1 do
      if length(CurrentDrawData[j])>0 then begin
        pixel.color_pen := pixel.colorSetAValue(gspBlack,150);
        pixel.moveTo(trunc(currentDrawData[j][0].x),trunc(currentDrawData[j][0].y));
        for i := 0 to length(CurrentDrawData[j])-1 do
          pixel.lineTo(trunc(currentDrawData[j][i].x),trunc(currentDrawData[j][i].y));
      end;
  end;
  pixel.CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
  Image1.Repaint;
  inc(FPS);
end;

procedure TForm3.pmLoadMeshClick(Sender: TObject);
var asset : TGSAssetShapeMesh;
    l : TFileStream;
begin
  if OpenDialog1.Execute then
  begin
    l := TFileStream.Create(OpenDialog1.FileName,fmOpenRead);
    try
      asset := TGSAssetShapeMesh.Create;
      asset.LoadFromStreamMeshPart(l);
      asset.MeshData.fullRefreshBounding;
      assetManager.addAsset(asset);
      ListBox1.AddItem('MeshAsset '+IntToStr(assetManager.AssetCount),asset);
      ListBox1.PopupMenu := PopupMenu2;
    finally
      FreeAndNil(l);
    end;
  end;
end;

procedure TForm3.pmSaveMeshClick(Sender: TObject);
var asset : TGSAssetShapeMesh;
    l : TFileStream;
begin
  if ListBox1.ItemIndex>-1 then
  begin
    asset := TGSAssetShapeMesh(ListBox1.Items.Objects[ListBox1.ItemIndex]);
    if SaveDialog1.Execute then
    begin
      l := TFileStream.Create(SaveDialog1.FileName,fmCreate);
      try
        asset.SaveToStreamMeshPart(l);
        ShowMessage(format('"%s" mesh only Saved',[SaveDialog1.FileName]));
      finally
        FreeAndNil(l);
      end;
    end;
  end;
end;

procedure TForm3.TimerFPSTimer(Sender: TObject);
begin
  Caption := IntToStr(FPS);
  FPS := 0;
end;

{ TShapeAssetManager }

procedure TShapeAssetManager.addAsset(asset: TGSAssetShapeMesh);
begin
  assert(assigned(asset));
  if flist.IndexOf(asset)=-1 then
    flist.Add(asset)
end;

function TShapeAssetManager.AssetFromMouseDrawing( data: Vec2sArray;
                                                   var newAsset : TGSAssetShapeMesh;
                                                   const TriangulationMethod : TTiangulationMethod) : boolean;
var v : vec2;
    i : integer;
    a,b,c : vec2;

    res : boolean;
    lvar :  TGSRawMesh2D;
begin
  result := false;
  if not assigned(newAsset) then
  begin
    newAsset := TGSAssetShapeMesh.Create;
    newAsset.MeshData.reset;
  end;

  lvar := newAsset.MeshData;
  newAsset.BordersCopyFrom(data);

  result := TGSTriangulationPortal.PolygoneTriangulation(data,lvar,TriangulationMethod)>0;
end;


constructor TShapeAssetManager.Create;
begin
 flist :=  TObjectList.Create(true);
end;

destructor TShapeAssetManager.Destroy;
begin
  freeandNil(flist);
  inherited;
end;

function TShapeAssetManager.GetAsset(Index: Uint32): TGSAssetShapeMesh;
begin
  result := TGSAssetShapeMesh(flist[index]);
end;

function TShapeAssetManager.GetAssetCount: Uint32;
begin
  result := flist.Count;
end;

procedure TShapeAssetManager.SetAsset(Index: Uint32;
  const Value: TGSAssetShapeMesh);
begin
  flist[index] := Value;
end;

end.
