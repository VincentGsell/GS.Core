unit otptest.fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  GoogleOTP, FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.Layouts, FMX.ListBox,
  GS.Common.OneTimePassword;

type
  TForm42 = class(TForm)
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Timer: TTimer;
    Label3: TLabel;
    Edit2: TEdit;
    Label4: TLabel;
    Button1: TButton;
    ListBox1: TListBox;
    ListBox2: TListBox;
    procedure Memo1Change(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Process;
  end;




var
  Form42: TForm42;

implementation

{$R *.fmx}

procedure TForm42.Button1Click(Sender: TObject);
var lt : integer;
begin
  lt := StrToIntDef(Edit2.Text,0);
  if lt=0 then
    ShowMessage('only number')
  else
  begin
    if ListBox1.Items.IndexOf(intToStr(lt))=-1 then
      ListBox1.Items.Add(IntToStr(lt));
  end;
end;

procedure TForm42.Memo1Change(Sender: TObject);
begin
  Process;
end;


procedure TForm42.Process;
var
	Token: string;
  l : IGSOTP;
begin
  l:= gsOTPGetImplementation;
	Token := l.generate(Memo1.Text);
	Edit1.Text := Token;
end;

procedure TForm42.TimerTimer(Sender: TObject);
var i : integer;
  l : IGSOTP;
  lt : String;
begin
  l:= gsOTPGetImplementation;

  Label3.Text := DateTimeToStr(Now);
  Process;

  ListBox2.BeginUpdate;
  try
    if ListBox2.Items.Count <> ListBox1.Items.Count then
    begin
      ListBox2.Clear;
      ListBox2.Items.Text := ListBox1.Items.Text;
    end;
    for I := 0 to ListBox2.Items.Count-1 do begin
      lt := ListBox1.Items[i];
      ListBox2.Items[i] := 'denied';
      if l.validate(Memo1.Text,lt) then
        ListBox2.Items[i] := 'Granted';
    end;
  finally
    ListBox2.EndUpdate;
  end;
end;

end.
