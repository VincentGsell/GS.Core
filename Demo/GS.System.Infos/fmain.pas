unit fmain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  GS.System.Infos,
  GS.System.Infos.Extended,
  GS.System.Infos.Extended.Default,     //nornally, you will use only one impl. Default is very limited : Here for example of implementation.
  GS.System.Infos.Extended.ZDeviceInfo, //complete impl, all platforme except Linux.
  GS.System.CPU, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.ListBox, FMX.Controls.Presentation;

type
  TFormMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ComboBox1Change(Sender: TObject);
var l : IgsSysInfo;
    i : integer;
begin
  Memo1.Lines.Clear;
  l := TgsSysInfoImplManager.GetImpl(ComboBox1.Selected.Text);
  Memo1.Lines.Add(l.getPlatform);
  Memo1.Lines.Add(l.getPlatformVer);
  Memo1.Lines.Add(l.getDevice);
  Memo1.Lines.Add(l.getArchitecture);
  Memo1.Lines.Add('------------------');
  Memo1.Lines.Add('Ext fields');
  Memo1.Lines.Add('------------------');
  Memo1.Lines.Add('Field''s count : '+l.getExtFieldCount.ToString);
  for I := 0 to l.getExtFieldCount-1 do
    Memo1.Lines.Add(format('Field %d - %s = %s',[i,l.getExtFieldName(i),l.getExtFieldValue(l.getExtFieldName(i))]));
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ComboBox1.Items.Text := TgsSysInfoImplManager.ImplList;
  ComboBox1.ItemIndex := ComboBox1.Items.Count-1; //trig OnChange.
end;

end.
