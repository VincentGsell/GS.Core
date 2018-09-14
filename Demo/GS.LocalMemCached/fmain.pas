unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  GS.LocalMemcached, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    LabeledEdit1: TLabeledEdit;
    GroupBox1: TGroupBox;
    Button2: TButton;
    Memo2: TMemo;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Button4: TButton;
    Button5: TButton;
    LabeledEdit2: TLabeledEdit;
    Button6: TButton;
    Button7: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MemCached : TLocalMemcached;
    clientMemCached : TLocalMemcachedClient;

    Procedure OnMemCachedReady(Sender : TObject);
    Procedure OnWarmInitLoad(Sender : TObject; PercentProgress : Double);

  end;

var
  Form1: TForm1;

implementation

//Const CST_STRESS_COUNT = 1000; //Cool one.
//Const CST_STRESS_COUNT = 10000; //Ok
Const CST_STRESS_COUNT = 100000; //Ok InMemory (On my machine) and Buffer file. 32bits. File size : 34Mo.
//Const CST_STRESS_COUNT = 1000000; //Ok Buffer file, 32bit and 64 bit, about 30 sec. on "my machine". InMemory : out of memory. File size : 344Mo.
//Const CST_STRESS_COUNT = 10000000; //Ok Buffer file in 64bit. File size : 3.4 GB (Warning : 10 minutes of create/warm "on my machine"!! )!

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MemCached := TLocalMemcached.create;
  MemCached.OnReady := OnMemCachedReady;
  MemCached.OnWarm := OnWarmInitLoad;
  clientMemCached := TLocalMemcachedClient.Create(MemCached);
  RadioButton1.Checked := true;
//  MemCached.FileName := EmptyStr; //Without file name explicetely, it is "In memory" only (by default, it is not !)
  MemCached.Open;
end;

procedure TForm1.Button1Click(Sender: TObject);
var la: TMemoryStream;
begin
  if RadioButton1.Checked then
  begin
    Memo1.Lines.Add('Get as Text : "'+LabeledEdit1.Text+'" : "'+ClientMemCached.GetValue(LabeledEdit1.Text)+'"');
  end
  else
  begin
    la := TMemoryStream.Create;
    try
      ClientMemCached.GetValue(LabeledEdit1.Text,la);
      Memo1.Lines.Add('Get as stream : "'+LabeledEdit1.Text+'" : '+IntToStr(la.Size)+' byte(s)');
    Finally
      freeAndNil(la);
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  clientMemCached.SetValue(LabeledEdit1.Text,Memo2.Text);
  Caption := 'Memcached items count : '+IntToStr(MemCached.ItemCount);
end;

procedure TForm1.Button3Click(Sender: TObject);
var la : TMemoryStream;
begin
  if OpenDialog1.Execute then
  begin
    la := TMemoryStream.Create;
    try
      la.LoadFromFile(OpenDialog1.FileName);
      la.Position := 0;
      clientMemCached.SetValue(LabeledEdit1.Text,la);
      Caption := 'Memcached items count : '+IntToStr(MemCached.ItemCount);
    finally
      FreeAndNil(la);
    end;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Memo1.Text := MemCached.GetCVSSnapShot;
end;

procedure TForm1.Button5Click(Sender: TObject);
var i : integer;
    ls : TMemoryStream;
begin
  Randomize;
  for i := 1 to CST_STRESS_COUNT do
  begin

    if random(100)>49 then
    begin
      if random(10)>4 then
      begin
        clientMemCached.SetValue(LabeledEdit2.Text+'.'+IntToStr(i),'This is test number '+inttostr(i));
      end
      else
      begin
        clientMemCached.SetValue(LabeledEdit2.Text+'.'+IntToStr(i),'Hello World'); //Same "value" field.
      end;
    end
    else
    begin
      ls := TMemoryStream.Create;
      try
        ls.SetSize(Random(1024)+10);
        clientMemCached.SetValue(LabeledEdit2.Text+'Stream.'+IntToStr(i),ls);
      finally
        FreeAndNil(ls);
      end;
    end;

    if (i mod (CST_STRESS_COUNT Div (1000))) = 0 Then
    begin
      Memo1.Lines.Add(IntToStr(i)+'/'+IntToStr(CST_STRESS_COUNT));
      Caption := 'Memcached items count : '+IntToStr(MemCached.ItemCount);
      Application.ProcessMessages;
    end;
  end;
  Memo1.Lines.Add('Done !');
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  Memo1.Text := MemCached.GetCVSSnapShotAsString(0,999);
end;

procedure TForm1.Button7Click(Sender: TObject);
begin
  Memo1.Text := MemCached.GetCVSSnapShotAsString(MemCached.ItemCount-999,MemCached.ItemCount);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MemCached);
  FreeAndNil(clientMemCached);
end;

procedure TForm1.OnMemCachedReady(Sender: TObject);
begin
  TThread.Queue(nil,Procedure begin
                      Caption := 'Memcached items count : '+IntToStr(MemCached.ItemCount);
                    end);
end;

procedure TForm1.OnWarmInitLoad(Sender: TObject; PercentProgress: Double);
begin
  TThread.Queue(nil,Procedure begin
                      Caption := 'Loading : '+FloatToStr(PercentProgress)+'% complete.';
                      Application.ProcessMessages;
                    end);
end;

procedure TForm1.RadioButton2Click(Sender: TObject);
begin
  Button2.Enabled := RadioButton1.Checked;
  Button3.Enabled := RadioButton2.Checked;
end;

end.
