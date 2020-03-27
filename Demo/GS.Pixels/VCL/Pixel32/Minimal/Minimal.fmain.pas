unit Minimal.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, GS.Pixel32, GS.Pixel32.VCL;

type
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  pixel := TPixel32.create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(pixel);
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  pixel.moveTo(x,y);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if ssleft in Shift then
  begin
    pixel.lineTo(x,y);
    pixel.moveTo(x,y);
  end;
  repaint;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  pixel.CopyToDc(GetDC(handle));
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  pixel.resize(Width,Height);
end;

procedure TForm1.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

end.
