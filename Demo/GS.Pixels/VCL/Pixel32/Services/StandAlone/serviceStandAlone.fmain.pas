unit serviceStandAlone.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Gs.Pixel,
  GS.Pixel32,
  GS.Pixel32.Win,
  GS.Pixel32.Service.PXL,
  GS.Pixel32.Service.Image32, Vcl.ExtCtrls;

type
  TForm5 = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    l : TPixel32Image32Service;
  end;

var
  Form5: TForm5;

implementation

uses GS.Bus;

{$R *.dfm}

procedure TForm5.FormCreate(Sender: TObject);
var a : TBusMessage;
begin
  l := TPixel32Image32Service.Create;
  l.StarsPath(20,100,8);
end;

end.
