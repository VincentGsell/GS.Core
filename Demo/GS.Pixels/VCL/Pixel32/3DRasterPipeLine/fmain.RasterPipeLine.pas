unit fmain.RasterPipeLine;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.AppEvnts,
  Vcl.ComCtrls,
  ContNrs,
  GS.Common.Monitoring,
  GS.Pixel32,
  GS.Pixel32.Types,
  GS.Pixel32.Effect,
  GS.Pixel32.Effect.Gradient,
  GS.Pixel32.Fragments,
  GS.Soft3D,
  GS.Soft3D.Types,
  GS.Soft3D.PipeLine,
  GS.Soft3D.PipeLine.Types,
  GS.Soft3D.PipeLine.FragmentShader,
  GS.Soft3D.View,
  GS.Soft3D.PipeLine.RasterOperation.Pixel32,
  GS.Pixel32.Win;

type
  TForm7 = class(TForm)
    Image1: TImage;
    Panel1: TPanel;
    cbWireFrame: TCheckBox;
    cbRasterFrame: TCheckBox;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    GroupBox1: TGroupBox;
    ComboBox1: TComboBox;
    TimerFPS: TTimer;
    ApplicationEvents1: TApplicationEvents;
    Panel2: TPanel;
    TreeView1: TTreeView;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure cbClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    M,MLD : TPoint;

    procedure GUI_UpdateLog;
  public
    { Public declarations }
    viewport : TView3d;          //Abstract virtual 3d view.
    pixel : TPixel32;     //surface for drwaing 3d stuff, throught viewport.

    MyMeshList : TS3DMeshList;
    MyObjectList : TS3DObjectList;

    MyNiceCube : TGSCubeMesh;
    My3DObj : TS3DObject;

    MyMountain : TS3DObject;






  end;

var
  Form7: TForm7;

implementation

{$R *.dfm}

procedure TForm7.ApplicationEvents1Idle(Sender: TObject; var Done: Boolean);
var oi,fi : Uint32;
begin
  viewport.Execute;
  viewport.Projection := TS3DProjectionType(RadioGroup1.ItemIndex);
  TPixel32( TS3DRasterOperationPixel32(viewport.PipeLine.RasterOperation).PixelSurface).CopyToDc(Image1.Canvas.handle);

  Image1.Repaint;
  Done := False;

  MyMountain.rx := MyMountain.rx + 2;

  Caption := '';
  if viewport.PipeLine.QueryingBuffer(M.X,M.Y,oi,fi) then
  begin
    Caption := format('found Object %d - face %d',[oi,fi]);
  end;

end;

procedure TForm7.cbClick(Sender: TObject);
begin
  viewPort.PipeLine.RasterOperation.EnableFullWireFrame := cbWireFrame.Checked;
  viewPort.PipeLine.RasterOperation.EnableRasterEngine := cbRasterFrame.Checked;
end;

procedure TForm7.FormCreate(Sender: TObject);
//var grad : TPixel32GeneratorGradient;
var q,i,j : integer;
    ll : TS3DObject;
begin
  MyObjectList := TS3DObjectList.Create; //3d Object.
  MyMeshList := TS3DMeshList.Create; //Assets for 3d object !

  pixel := TPixel32.create;     //final surface drawing... (image)

  viewport := TView3d.Create; //virtual window on 3d world.

  { TODO : BEAUTIFY !! }
  viewPort.PipeLine.RasterOperation := TS3DRasterOperationPixel32.Create(viewport.PipeLine,viewport.PipeLine.InputData,viewport.PipeLine.WorkData);
  TS3DRasterOperationPixel32(viewPort.PipeLine.RasterOperation).PixelSurface := Pixel;

  //Change default shader. (Defaut shader is base object color);
  //viewPort.PipeLine.RasterOperation.FragControl.defaultShader.Free;
  //viewPort.PipeLine.RasterOperation.FragControl.defaultShader := TS3DFragmentShader_DepthColor.Create;

  viewport.ResizeBuffers(ClientWidth,ClientHeight);

  viewport.CameraZ := 6;

  viewport.wireFrame := false;
  viewport.rasterFrame := true;

  //Important : Assign your mesh and object list (can be shared)
  viewport.PipeLine.InputData.Meshes := MyMeshList;
  viewport.PipeLine.InputData.Objects := MyObjectList;

  //Create a mesh reference.
  MyNiceCube := TGSCubeMesh.Create; //a cube.
  MyMeshList.AddMesh(MyNiceCube); //Add to our list.

  //create a 3d object.
  My3DObj := TS3DObject.Create;
  My3DObj.MeshAsset := MyNiceCube; //link to an asset's mesh.

  //do not forget to add to object list !
  MyObjectList.AddObject(My3DObj);

  //another one
  ll := TS3DObject.Create;
  ll.MeshAsset := MyNiceCube;
  ll.x := -2;
  ll.DefaultColor.create(1,1,0,1);
  MyObjectList.AddObject(ll);
  ll := nil;

  ll := TS3DObject.Create;
  ll.MeshAsset := MyNiceCube;
  ll.y := -2;
  ll.DefaultColor.create(0,1,0,1);
  MyObjectList.AddObject(ll);

  ll := TS3DObject.Create;
  ll.MeshAsset := TGSPlaneMesh.Create;
  MyMeshList.AddMesh(ll.MeshAsset);
  ll.x := 2;
  MyObjectList.AddObject(ll);

  MyMountain := TS3DObject.Create;
  MyMountain.MeshAsset := TGSPlaneTriMesh.Create;
  MyMeshList.AddMesh(MyMountain.MeshAsset);
  MyMountain.y := 2;
  MyObjectList.AddObject(MyMountain);

  //Cube on the rear

  q := 5;
  for i := -q to q do
  for j := -q to q do
  begin
    ll := TS3DObject.Create;
    ll.MeshAsset := MyNiceCube; //Note that we shared the model
    ll.z := -7;
    ll.x := 1.2 * i;
    ll.y := 1.2 * j;
    ll.DefaultColor.create(Random(100)/100,Random(100)/100,Random(100)/100,1);
    MyObjectList.AddObject(ll);
  end;

//  My3DObj := viewport.addPlane(0,0,0);

//  for i := -2 to 2 do
//  begin
//    viewport.addCube(i,0,0);
//  end;

//  viewport.addPlane(0,0,0);
//  viewport.addPlane(0,-2,0);
{
  //Generate a gradient "star" picture, for texturing.
  gradientTexture := TPixel32.create; //Storage.
  mem.Add(gradientTexture);
  grad := TPixel32GeneratorGradient.Create; //Generator.
  try
    grad.init(gradientTexture);
    grad.ColorA := pixel.ColorSetAValue(gspWhite,120);
    grad.ColorB := gspBlack;
    grad.ShiftGradient := -50;
    grad.RorateGradient := 0;
    grad.process;
  finally
    FreeAndNil(grad); //No more need generator.
  end;


  //Build a "scotish" shader (squared motif, with no rotation (screen oriented))
  scot := TPixel32ShaderSquaredMotif.Create;
  mem.Add(scot);


  //Build a classic shader, alpha enabled.
  colShader := TPixel32FragmentShaderFlatColor.create;
  colShader.Color := TP32Rec(pixel.colorSetAValue(gspRed,100)).Color;
  mem.Add(colshader);

  //Assign this texture a texture shader.
  clatextureShader_computed := TPixel32FragmentShaderTextureColor.Create;
  clatextureShader_computed.Texture := gradientTexture;
  mem.Add(clatextureShader_computed);

  clatextureShader_imageGoldo := TPixel32FragmentShaderTextureColor.Create;
  imageTexGoldo := TPixel32.create;
  imageTexGoldo.loadFromFile('../../../../../assets/avatar.bmp');
  clatextureShader_imageGoldo.Texture := imageTexGoldo;
  mem.Add(clatextureShader_imageGoldo);
  mem.Add(imageTexGoldo);

  clatextureShader_imageCrate := TPixel32FragmentShaderTextureColor.Create;
  imageTexCrate := TPixel32.create;
  imageTexCrate.loadFromFile('../../../../../assets/Crate_256.bmp');
  clatextureShader_imageCrate.Texture := imageTexCrate;
  mem.Add(clatextureShader_imageCrate);
  mem.Add(imageTexCrate);

  clatextureShader_imageDice := TPixel32FragmentShaderTextureColor.Create;
  imageTexDice := TPixel32.create;
  imageTexDice.loadFromFile('../../../../../assets/alpha-dice.bmp');
  //It is a bmp, alpha value is lost. But on this image, it is easy to rebuild. ;)
  imageTexDice.alphaLayerResetByColor(gspBlack,0); //get tranparent alpha layer, from black pixel of piture.
  clatextureShader_imageDice.Texture := imageTexDice;
  mem.Add(clatextureShader_imageDice);
  mem.Add(imageTexDice);


  ShaderToyShader :=  TPixel32ShaderPlasma.create(100);
  mem.Add(ShaderToyShader);

  imageTexShader := TPixel32.create(64,64);
  imageTexShader.setFragmentShader(ShaderToyShader);
  imageTexShader.fill; //clear with shader :)
  TextureShader_ShaderToy := TPixel32FragmentShaderTextureColor.create;
  TextureShader_ShaderToy.Texture := imageTexShader;
  mem.Add(imageTexShader);
  mem.Add(TextureShader_ShaderToy);


  TPixel32(viewport.TargetCanvas).setFragmentShader(clatextureShader_imageCrate);
}

viewport.Execute;

TimerFPS.Enabled := true;

end;

procedure TForm7.FormDestroy(Sender: TObject);
begin
  FreeAndNil(viewport);
  FreeAndNil(pixel);
  FreeAndNil(MyObjectList);
  FreeAndNil(MyMeshList);
end;

procedure TForm7.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  viewport.CameraZ := viewport.CameraZ + WheelDelta/1000;
end;

procedure TForm7.FormResize(Sender: TObject);
begin
  viewport.ResizeBuffers(ClientWidth,ClientHeight);
  Image1.Picture.Bitmap.SetSize(ClientWidth,ClientHeight);
end;

procedure TForm7.GUI_UpdateLog;
begin
end;

procedure TForm7.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var oi, fi : Uint32;
begin
  MLD := point(X,Y);

  if viewport.PipeLine.QueryingBuffer(X,Y,oi,fi) then
  begin
    My3DObj := MyObjectList[oi];
  end
  else
  begin
    My3DObj := nil;
  end;

end;

procedure TForm7.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  var oi,fi : Uint32;
begin
  M := Point(x,y);

  if ssLeft in shift then
  begin
    if Assigned(My3DObj) then
    begin
      My3DObj.ry := MLD.X-x;
      My3DObj.rx := MLD.Y-y;
    end;
  end;
end;

procedure TForm7.Timer1Timer(Sender: TObject);
var al : single;
begin
  al := Cos((TThread.GetTickCount/1000));
  viewport.SetCamPos(0,0,16+al*2);
//  viewport.SetCamRotate(0,al,0);
end;

procedure TForm7.TimerFPSTimer(Sender: TObject);
begin
  GUI_UpdateLog;
end;


end.
