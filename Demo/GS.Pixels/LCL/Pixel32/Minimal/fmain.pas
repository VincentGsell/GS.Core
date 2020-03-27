unit fmain;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Windows, LMessages, GS.Pixel32;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  private
  public
    pixel : TPixel32;
  end;

var
  Form1: TForm1;

implementation

uses GS.Pixel32.Win;

{$R *.lfm}

{ TForm1 }

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

procedure TForm1.FormResize(Sender: TObject);
begin
  pixel.resize(width,height);
end;

procedure TForm1.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  pixel.CopyToDc(Message.DC);
end;


end.

