unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  GS.Language.AlgoProgram,
  GS.Language.VM,
  GS.Language.Compiler.Pascal,
  GS.Language.Formula.Solver,
  GS.Language.WordTokenizer;

type
  TForm34 = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    PageControl1: TPageControl;
    tsCode: TTabSheet;
    tsVM: TTabSheet;
    Memo1: TMemo;
    Button1: TButton;
    Panel3: TPanel;
    Memo2: TMemo;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ListBox1: TListBox;
    tsFormula: TTabSheet;
    but: TPanel;
    cbFormula: TComboBox;
    Memo3: TMemo;
    Panel5: TPanel;
    lvVar: TListView;
    btFormulaRun: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure btFormulaRunClick(Sender: TObject);
    procedure RadioButton2Click(Sender: TObject);
    procedure RadioButton3Click(Sender: TObject);
    procedure RadioButton4Click(Sender: TObject);
    procedure RadioButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure InternalCompile;
  public
    { Public declarations }
    procedure Compile;
  end;

var
  Form34: TForm34;

implementation

{$R *.dfm}

procedure TForm34.btFormulaRunClick(Sender: TObject);
var
    s : TFormulaSolver;
begin
  Memo3.Clear;

  s := TFormulaSolver.Create;
  try
    s.updateVar('a',10);
    s.updateVar('b',5);
    s.updateVar('c',2);
    s.updateVar('d',200);
    s.updateVar('e33',4);

    if s.Compile(cbFormula.Text) then
    begin
      Memo3.Lines.Add('compilation ok');
      Memo3.Lines.Add(s.FlushPostFixFormula);
      Memo3.Lines.Add('Running...');
      s.Run;
      Memo3.Lines.Add('*************************************');
      Memo3.Lines.Add('Result : '+s.getVarValueAsString('result'));
      Memo3.Lines.Add('*************************************');
    end
    else
    begin
      Memo3.Lines.Add('compilation error : '+s.LastError);
    end;
    Memo3.Lines.Add('');
    Memo3.Lines.Add('Postfix : '+s.FlushPostFixFormula);
    Memo3.Lines.Add('-----');
    Memo3.Lines.Add(s.FlushCompiledCode);
  finally
    FreeAnDnil(s);
  end;
end;

procedure TForm34.Button2Click(Sender: TObject);
begin
  Compile;
end;

procedure TForm34.Internalcompile;
var l : TPascalCompiler;
    vm : TVirtualMachineRAPI;
begin
  l := TPascalCompiler.Create;
  try
    if l.Compile(Memo1.Lines.Text) then
    begin
      vm := TVirtualMachineRAPI.Create;
      try
//      raise Exception.Create('Error Message');
//        vm.run(l.CompiledProgram);
      finally
        FreeAndNil(vm);
      end;
    end;
  finally
    ListBox1.Clear;
    ListBox1.Items.Text := l.parser.logs.Text;
    freeAndNil(l);
  end;
end;

procedure TForm34.FormCreate(Sender: TObject);
begin
//  Compile;
end;

procedure TForm34.Compile;
begin
  Memo2.Clear;
  try
    Internalcompile;
    Memo2.Lines.Add('Succeed !');
  except
    On e : exception do
    begin
      Memo2.Lines.Add(e.Message);
    end;
  end;
end;

procedure TForm34.Memo1Change(Sender: TObject);
begin
  if CheckBox1.Checked then
    Compile;
end;

procedure TForm34.RadioButton1Click(Sender: TObject);
begin
memo1.Lines.Text :=
'//simple basic program'+#13#10+
'program test;'+#13#10+
'var a,b,c : integer;'+#13#10+
'begin'+#13#10+
'a : = 10;'+#13#10+
'b := 20;'+#13#10+
'c := 30;'+#13#10+
'd :integer := a*b+c;'+#13#10+
'println(d);'+#13#10+
'end.';
end;

procedure TForm34.RadioButton2Click(Sender: TObject);
begin
memo1.Lines.Text :=
'//simple basic program'+#13#10+
'program test;'+#13#10+
'var a,b,c : integer;'+#13#10+
'begin'+#13#10+
'a : = 10;'+#13#10+
'b := 20;'+#13#10+
'c := 30; e :integer; e+= c+1;'+#13#10+
'10+a+10;'+#13#10+
'd :integer := a*b+c;'+#13#10+
'println(d);'+#13#10+
'end.'+#13#10;
end;


procedure TForm34.RadioButton3Click(Sender: TObject);
begin
memo1.Lines.Text :=
'//simple basic program'+#13#10+
'program test;'+#13#10+
'var a,b,c : integer;'+#13#10+
'begin'+#13#10+
'a : = 10;'+#13#10+
'b := 20; b++-1;'+#13#10+
'c := 30; e :integer; e+= c+1;'+#13#10+
'10+a+10;'+#13#10+
'd :integer := a*b+c;'+#13#10+
'f :double += a*b+c/d;'+#13#10+
'println(d);'+#13#10+
'end.'+#13#10;
end;

procedure TForm34.RadioButton4Click(Sender: TObject);
begin
memo1.Lines.Text :=
'//simple basic program'+#13#10+
'program test;'+#13#10+
'var a,b,c : integer;'+#13#10+
'begin'+#13#10+
'a:=5; a+=1; //a vaut 6.'+#13#10+
'a *= 10;'+#13#10+
'b := 20; e:double += 10;'+#13#10+
'c := 30; cos(a); zz:integer +=pow(pow(10,2));'+#13#10+
'd :integer := a*b+c+pow(3,4,a);'+#13#10+
'g : double := (cos(a*b+c+pow(3,4,a))/10);'+#13#10+
'println(d);'+#13#10+
'end.'+#13#10;
end;


end.
