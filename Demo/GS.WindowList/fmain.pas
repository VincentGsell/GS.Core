unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Gs.System.WindowList, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    ListView1: TListView;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    DesktopCanvas : TCanvas;
    { Public declarations }
    procedure DrawStuff;

    procedure GUIRefreshList;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DrawStuff;
var l : TGSWindow;
begin
  l := GSGlobalOSVisibleWindows[0];
  DesktopCanvas.Ellipse(l.X,l.Y,l.X+20,l.Y-20);
  DesktopCanvas.moveTo(l.X,l.Y-20);
  DesktopCanvas.LineTo(l.X+20,l.Y);
  DesktopCanvas.moveTo(l.X+20,l.Y-20);
  DesktopCanvas.LineTo(l.X,l.Y);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DesktopCanvas := TCanvas.Create;
  DesktopCanvas.Handle := GetDC(GetDesktopWindow);
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  GUIRefreshList;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  GUIRefreshList;
end;

procedure TForm1.GUIRefreshList;
var i : integer;
begin
  ListView1.Clear;
  GetOSVisibleWindow;
  for i := 0 to Length(GSGlobalOSVisibleWindows)-1 do
  begin
    ListView1.AddItem(GSGlobalOSVisibleWindows[i].ToString,nil);
  end;
end;


end.
