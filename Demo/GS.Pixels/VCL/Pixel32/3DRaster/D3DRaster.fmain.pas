unit D3DRaster.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, contnrs,
  GS.SoftwareRaster3D.ExempleStandAlone,
  GS.Pixel32,
  GS.Pixel32.Types,
  GS.Pixel32.Win;

type
  TForm1 = class(TForm)
    TimerFPS: TTimer;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;     //surface for drwaing 3d stuff
    viewport : TView3d; //virtual window on 3d world.
    FPS : integer;

    procedure appIdle(Sender: TObject; var Done: Boolean);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i,j : integer;
    l : TMesh3D;
begin

  pixel := TPixel32.create;     //surface for drwaing 3d stuff
  viewport := TView3d.Create; //virtual window on 3d world.
  viewport.TargetCanvas := pixel;
  viewport.CameraZ := -15;
  viewport.wireFrame := false;
  viewport.rasterFrame := true;

  for i := -2 to 2 do
  begin
    viewport.addCube(i,0,0);
  end;

  viewport.addPlane(0,2,0);
  viewport.addPlane(0,-2,0);

{
  for i := 0 to 5 do
    for j := 0 to 5 do
    begin
      l := TMesh3D(viewport.addCube(i-3,j-3,-12+i));
      l.meshScale(0.5);
    end;
}

  viewport.CameraZ := 4;
  Application.OnIdle := appIdle;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pixel);
  FreeAndNil(viewport);
end;

procedure TForm1.appIdle(Sender: TObject; var Done: Boolean);
begin
  pixel.color_fill := gspBlue;
  pixel.clear;
  pixel.color_pen := pixel.ColorSetAValue(gspBlue,20);
  viewport.Execute;
  TPixel32(viewport.TargetCanvas).CopyToDc(Image1.Canvas.handle);
  Image1.Repaint;
  Inc(FPS);
  Done := false;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  viewport.CameraZ := viewport.CameraZ + WheelDelta/100;
  if viewport.CameraZ<4 then
    viewport.CameraZ := 4;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  Image1.Picture.Bitmap.SetSize(Width,Height);
  pixel.resize(Width,Height);
end;

procedure TForm1.TimerFPSTimer(Sender: TObject);
begin
  Caption := Format('3DSoftware renderer - FPS : %d - Res : (%d,%d)',[FPS,pixel.width,pixel.height]);
  FPS := 0;
end;

end.
