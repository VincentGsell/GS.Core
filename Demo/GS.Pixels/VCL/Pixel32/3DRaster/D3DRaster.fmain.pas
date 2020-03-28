unit D3DRaster.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  GS.Geometry.Soft3D, GS.Pixel32, GS.Pixel32.Win, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    TimerDraw: TTimer;
    Panel1: TPanel;
    cbWireFrame: TCheckBox;
    Label1: TLabel;
    TimerFPS: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure TimerDrawTimer(Sender: TObject);
    procedure cbWireFrameClick(Sender: TObject);
    procedure TimerFPSTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;     //surface for drwaing 3d stuff
    viewport : TViewport; //virtual window on 3d world.
    FPS : integer;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.cbWireFrameClick(Sender: TObject);
begin
  viewport.wireframe := cbWireFrame.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  pixel := TPixel32.create;     //surface for drwaing 3d stuff
  viewport := TViewport.Create; //virtual window on 3d world.
  viewport.TargetCanvas := pixel;
  viewport.CameraZ := -5;

  viewport.addCube(0,0,0);
  viewport.addCube(-2,0,0);
  viewport.addCube(2,0,0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
//  FreeAndNil(viewport);
//  FreeAndNil(pixel);
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  viewport.CameraZ := viewport.CameraZ + WheelDelta/100;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  TPixel32(viewport.TargetCanvas).CopyToDc(getDC(handle));
  Inc(FPS);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pixel.resize(Width,Height);
end;

procedure TForm1.TimerDrawTimer(Sender: TObject);
begin
  pixel.clear;
  viewport.Execute;
  Repaint;
end;

procedure TForm1.TimerFPSTimer(Sender: TObject);
begin
 Caption := 'FPS : '+IntToStr(FPS);
 FPS := 0;
end;

procedure TForm1.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

end.
