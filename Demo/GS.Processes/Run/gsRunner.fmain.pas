unit gsRunner.fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  FMX.TabControl,
  GS.System.Processes;

type
  TForm42 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Panel1: TPanel;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo2: TMemo;
    Button1: TButton;
    Button2: TButton;
    OpenDialog1: TOpenDialog;
    TabItem3: TTabItem;
    TabItem4: TTabItem;
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form42: TForm42;

implementation

{$R *.fmx}

procedure TForm42.Button2Click(Sender: TObject);
var l : IGSCodeRunner;
begin
  l := TGSRegisteredRunnerFactory.GetRunnerInstance('python3');
  Memo2.Text := l.Run(Memo1.Lines.Text);
end;

end.
