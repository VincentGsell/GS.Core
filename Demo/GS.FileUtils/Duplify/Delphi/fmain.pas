unit fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  GS.FileOp, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm15 = class(TForm)
    Memo1: TMemo;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Edit2: TEdit;
    Button1: TButton;
    Label3: TLabel;
    Button2: TButton;
    Button3: TButton;
    ProgressBar1: TProgressBar;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Shape1: TShape;
    Shape2: TShape;
    Edit3: TEdit;
    Label4: TLabel;
    Button9: TButton;
    Button10: TButton;
    Button11: TButton;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure Button7Click(Sender: TObject);
    procedure Button8Click(Sender: TObject);
    procedure Button10Click(Sender: TObject);
    procedure Button9Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure Log(const aText : String);
    function GlacierTarget : String;
  end;

var
  Form15: TForm15;

implementation

{$R *.dfm}

procedure TForm15.Button10Click(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TForm15.Button11Click(Sender: TObject);
var i : Integer;
begin
  if OpenDialog1.Execute then
  begin
    for I := 0 to OpenDialog1.Files.Count-1 do
    begin
      TGSBin.GlacifyFile(OpenDialog1.Files[i],Edit3.Text)
    end;

  end;
end;

procedure TForm15.Button1Click(Sender: TObject);
begin
  if TGSBin.CopyFile(Edit1.Text, Edit2.Text) then
    Log('copy ok')
  else
    log('copy not ok');
end;

procedure TForm15.Button2Click(Sender: TObject);
var s,t: TStringStream;
begin
  s := TStringStream.Create('Ceci est un test');
  t := TStringStream.Create('');
  try
    log('String to encrypt : '+InTToStr(s.Size)+ ' -- ' + s.DataString);
    TGSBin.Crypto_EncryptDecryptXor(TStream(s),TStream(t));
    log('String encrypted : '+InTToStr(t.Size)+ ' -- ' + t.DataString);
    s.Clear;
    TGSBin.Crypto_EncryptDecryptXor(TStream(t),TStream(s));
    log('Decrypting : ' +InTToStr(s.Size)+ ' -- ' + s.DataString);
  finally
    FreeAndNil(s);
    FreeAndNil(t);
  end;

end;

procedure TForm15.Button3Click(Sender: TObject);
var lc : TCopyFileProcessInfo;
    l : Integer;
begin
  ProgressBar1.Position := 0;
  InitCopyFileProcessInfo(Edit1.Text,Edit2.Text,lc); //Setup of Structure (Mandatory !)
  lc.InFileCopyBehaviour := [];               //Delete destination file if exits, no crc check
  while Not TGSBin.CopyFileProcess(lc) do            //Loop unit true (true = copy finished)
    if l<>round(lc.OutPercentDone) then
    begin
      //Remember that display take a lot of time.
      //I advise you, in a real app, to put this in a thread, and launch message (see GS.Bus, to update gui).
      l := round(lc.OutPercentDone);
      ProgressBar1.Position := round(lc.OutPercentDone);
    end;
//    log(FloatToStr(lc.OutPercentDone)+'%');
  begin end;
  if lc.OutOperationCode = cfpSuccess then
    log('Copy ok - ('+FloatTostr(lc.OutStats_TimeTakenForOp)+'ms)')
  else
    log('Copy failed');
end;

procedure TForm15.Button4Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    DeleteFile(GlacierTarget);
  Button5.Enabled:= TGSBin.GlacifyFile(Edit1.Text,GlacierTarget);

  if Button5.Enabled then
  begin
    Log('glacification ok')
  end
      else
    log('glacification failed');
end;

procedure TForm15.Button5Click(Sender: TObject);
begin
  log(TGSBin.GlacifyAnalytic_flush(GlacierTarget));
end;

procedure TForm15.Button6Click(Sender: TObject);
var ltarget : String;
    afinfo : TFileInformationStructure;
    i : integer;
begin
  ltarget := Edit1.Text;

  log('File info : '+ltarget);
  TGSBin.GlacifyAnalytic_FileSystem_FileInformation(ltarget,afinfo);
  log('Finished');
  log('Flush file info : ');
  log('Filename       : '+afinfo.fileName);
  log('File size      : '+IntToStr(afinfo.fileSize));
  log('F. Block count : '+IntToStr(Length(afinfo.fileBlock))+' block(s)');
  for I := 0 to Length(afinfo.fileBlock)-1 do
  begin
  log('F. Block        : '+IntToStr(i));
  log('F. Block Index  : '+IntToStr(afinfo.fileBlock[i].blockindex));
  log('F. Block Size   : '+IntToStr(afinfo.fileBlock[i].blockSize));
  log('F. Block CRC32  : '+IntToStr(afinfo.fileBlock[i].blockCRC32));
  end;

end;

procedure TForm15.Button7Click(Sender: TObject);
var ltarget : String;
    afinfo : TFileInformationStructure;
    i : integer;
begin
  ltarget := Edit1.Text;
  log('File info : '+ltarget);
  TGSBin.GlacifyAnalytic_FileInformation(GlacierTarget, ltarget, afinfo);
  log('Finished');
  log('Flush file info : ');
  log('Filename       : '+afinfo.fileName);
  log('File size      : '+IntToStr(afinfo.fileSize));
  log('Block count : '+IntToStr(Length(afinfo.fileBlock))+' block(s)');
  for I := 0 to Length(afinfo.fileBlock)-1 do
  begin
    log('   Block Index  : '+IntToStr(afinfo.fileBlock[i].blockindex));
    log('   Block Size   : '+IntToStr(afinfo.fileBlock[i].blockSize));
    log('   Block CRC32  : '+IntToStr(afinfo.fileBlock[i].blockCRC32));
    log(' ------------------');
  end;
end;

procedure TForm15.Button8Click(Sender: TObject);
var ltarget : String;
    afinfo : TFileInformationStructure;
    i : integer;
begin
  ltarget := Edit1.Text;
  log('File compare : '+ltarget);
  if TGSBin.GlacifyAnalytic_Compare(GlacierTarget, ltarget, ltarget) then
    log('binary same.')
  else
    log('Not binary same');
end;

procedure TForm15.Button9Click(Sender: TObject);
var aFileList : TFilesList;
    i : integer;
    lh : Uint64;
begin
  if TGSBin.GlacifyAnalytic_FileList(GlacierTarget,aFileList) then
  begin
    log(IntToStr(Length(aFileList))+' file(s) in glacier');
    lh := 0;
    for i := 0 to Length(aFileList)-1 do
    begin
      log(aFileList[i].FileName + ' / Version '+IntToStr(aFileList[i].Version));
      lh := lh + aFileList[i].OriginalFileSize;
    end;
    log(IntToStr(lh)+' byte(s)...');
  end;
end;

procedure TForm15.FormCreate(Sender: TObject);
begin
  Edit1.Text := ParamStr(0);
  Edit2.Text := ParamStr(0)+'.copy';
  Edit3.Text := ParamStr(0)+'.Glacier';
  Button5.Enabled :=  FileExists(GlacierTarget);
end;

function TForm15.GlacierTarget: String;
begin
  result := Edit3.Text;
end;

procedure TForm15.Log(const aText: String);
begin
  Memo1.Lines.Add(DateTimeToStr(Now)+' - '+aText)
end;

end.
