unit Euler.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  GS.Pixel32,
  GS.Pixel.Draw,
  GS.Pixel32.Win;

type
  TForm4 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    pixel : TPixel32;
    D,M : TPoint;
    procedure drawEuler(const zoom : real = 4; const angle : real = 0);
    procedure squares(x,y,a:real; n:byte; const mode : byte = 0);
    procedure squaresEx(n,l,x,y:integer);
    procedure crux(x,y,a:real; n:byte);
    procedure tri(x,y,a:real; n:byte);
  end;

var
  Form4: TForm4;

implementation

{$R *.dfm}

procedure TForm4.Button1Click(Sender: TObject);
begin
  drawEuler;
end;


procedure TForm4.squaresEx(n,l,x,y:integer);
begin
  if n>0 then
  begin
   pixel.line(x,y,x+(l div 2),y);
   pixel.line(x+(l div 2),y,x+l,y-(l div 2));
   pixel.line(x+l,y-(l div 2),x+(l div 2),y-l);
   pixel.line(x+(l div 2),y-l,x,y-(l div 2));
   pixel.line(x,y-(l div 2),x+(l div 4),y-(l div 4));
   squaresEx(n-1,(l div 2),x+(l div 4),y-(l div 4));
   pixel.line(x+(l div 4),y-(l div 4),x+(l div 2),y); {B}
   pixel.line(x+(l div 2),y,x+l,y);   {C}
   pixel.line(x+l,y,x+l,y-l); {E}
   pixel.line(x+l,y-l,x,y-l); {G}
   pixel.line(x,y-l,x,y);{A}
  end;
end;

procedure TForm4.squares(x,y,a:real; n:byte; const mode : byte);
begin
  pixel.Rectangle(round(x-a/2),round(y-a/2),round(x+a/2),round(y+a/2));
  if n>1 then
  begin
    case mode of
    0 :
    begin
      squares(x,y-3*a/2,a/2,n-1);
      squares(x,y+3*a/2,a/2,n-1);
      squares(x-3*a/2,y,a/2,n-1);
      squares(x+3*a/2,y,a/2,n-1);
    end
    else
      squares(x-a/2,y-a/2,a/2,n-1);
      squares(x-a/2,y+a/2,a/2,n-1);
      squares(x+a/2,y-a/2,a/2,n-1);
      squares(x+a/2,y+a/2,a/2,n-1);
    end;
  end;
end;

procedure TForm4.crux(x,y,a:real; n:byte);
var h:real;
begin
  h:=sqrt(sqr(a)-sqr(a/2));
  pixel.moveTo(round(x),round(y));
  pixel.lineTo(round(x),round(y+2*h/3));
  pixel.moveTo(round(x),round(y));
  pixel.lineTo(round(x-a/2),round(y-h/3));
  pixel.moveTo(round(x),round(y));
  pixel.lineTo(round(x+a/2),round(y-h/3));
  if n>1 then
  begin
    crux(x,y+2*h/3,a/2,n-1);
    crux(x-a/2,y-h/3,a/2,n-1);
    crux(x+a/2,y-h/3,a/2,n-1);
  end;
end;

procedure TForm4.tri(x,y,a:real; n:byte);
var h:real;
begin
  h:=sqrt(sqr(a)-sqr(a/2));
  if odd(n) then h:=-h;
  pixel.moveTo(round(x),round(y-2*h/3));
  pixel.lineTo(round(x-a/2),round(y+h/3));
  pixel.moveTo(round(x-a/2),round(y+h/3));
  pixel.lineTo(round(x+a/2),round(y+h/3));
  pixel.moveTo(round(x+a/2),round(y+h/3));
  pixel.lineTo(round(x),round(y-2*h/3));
  if n>1 then
  begin
    tri(x,y-2*h/3,a/2,n-1);
    tri(x-a/2,y+h/3,a/2,n-1);
    tri(x+a/2,y+h/3,a/2,n-1);
  end;
end;

procedure TForm4.drawEuler(const zoom : real; const angle : real);
const
  a=11;
var
  wx,wy,wa:real;
  i : integer;

  procedure lineAngle;
  var t : real;
  begin
    pixel.moveTo(trunc(wx),trunc(wy));
    t := wa * Pi/180;
    wx := wx + zoom * cos(t);
    wy := wy + zoom * sin(t);
    wa := wa + (i*a);
    pixel.lineTo(round(wx),round(wy));
  end;

begin
  wx := pixel.width / 2;
  wy := pixel.height / 2;
  wa := angle;
  i := 0;
  repeat
    lineAngle;
    i := i + 1;
  until i>2000;
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  pixel := TPixel32.create;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  freeandNil(Pixel);
end;

procedure TForm4.FormResize(Sender: TObject);
begin
  image1.Picture.Bitmap.SetSize(image1.Width,Image1.Height);
  pixel.resize(image1.Picture.Bitmap.Width,Image1.Picture.Bitmap.Height);
  drawEuler(M.X, M.Y);
  image1.Repaint;
end;

procedure TForm4.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  D := point(X,Y);
  M := D;
end;

procedure TForm4.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const c = 30;
begin
  if ssLeft in Shift then
  begin
  pixel.clear;
  pixel.rectangle(X-(c div 2),Y-(c div 2),X+(c div 2),Y+(c div 2));

    M := D - Point(X,Y);
    drawEuler(M.X, M.Y);
    squares(pixel.width/2,pixel.height/2,M.X,5,0);
//    tri(pixel.width/2,pixel.height/2,M.X,5);
//    crux(pixel.width/2,pixel.height/2,M.X,5);
//    squaresEx(8,M.X, pixel.width div 2 - (M.X div 2), pixel.height div 2 -  - (M.X div 2));
  pixel.CopyToDc(Image1.Picture.Bitmap.Canvas.Handle);
  Image1.Repaint;
  end;

end;

end.
