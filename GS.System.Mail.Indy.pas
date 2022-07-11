unit GS.System.Mail.Indy;

interface

uses
  IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdExplicitTLSClientServerBase, IdMessageClient, IdPOP3, idSMTP,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  idMessage, IdGlobal,
  System.SysUtils, System.classes,
  System.Generics.collections,
  IdText, IdAttachment, idAttachmentMemory, idAttachmentFile,
  GS.System.Mail,
  GS.Common.Log,
  GS.Common;

//https://www.experts-exchange.com/questions/21333608/Indy-10-GMail-and-POP3.html
//http://www.lastaddress.net/2013/05/sending-email-with-attachments-using.html

Type

TMailMessageAttachement = Class abstract
End;

  TMailMessageAttachementText = class(TMailMessageAttachement)
    PlainText : String;
  end;

  TMailMessageAttachementFile = class(TMailMessageAttachement)
    OriginalFileName : String;
    Content : TMemoryStream;
    Constructor Create; Virtual;
    Destructor Destroy; Override;
  end;

TSimpleMailMessage = Class
Public
  Messagedate : TDateTime;
  MessageRetrivalIndex : Integer;
  MessageId, MailFrom, Body, Subject : string;
  MailTo, MailCC : TArray<String>;
  Attachements : TObjectList<TMailMessageAttachement>;

  Constructor Create; virtual;
  Destructor Destroy; override;
End;


TCheckMailSSL = Class
private
  FHost: String;
  FPort: Word;
  FSSL : TIdSSLIOHandlerSocketOpenSSL;
public
  function GetHandler : TIdSSLIOHandlerSocketOpenSSL;

  Constructor Create; virtual;
  Destructor Destroy; Override;

  property Host : String read FHost Write FHost;
  property Port : Word read FPort Write FPort;
End;

TCheckMailPOP3 = class
Private
  FPort: Word;
  FPassword: string;
  FHost: String;
  FUserName: string;
  FLastError: String;
  FPop3 : TIdPOP3;
  FSSLAddIn: TCheckMAilSSL;

  Function InternalGetMail : Boolean;
  procedure SetSSLAddIn(const Value: TCheckMAilSSL);
Protected

Public
  MailContent : TObjectList<TSimpleMailMessage>;

  Constructor Create; virtual;
  Destructor Destroy; Override;

  Function GetMail : Boolean;
  Function DeleteMailServerSide(aMail : TSimpleMailMessage) : Boolean;

  property Host : String read FHost Write FHost;
  property Port : Word read FPort Write FPort;
  property UserName : string read FUserName Write FUserName;
  property password : string read FPassword Write FPassword;
  property LastError : String read FLastError;
  property SSLAddIn : TCheckMAilSSL read FSSLAddIn Write SetSSLAddIn;
end;

TSendMailSMTP = Class
Private
    FPort: Word;
    FPassword: string;
    FHost: String;
    FLastError: String;
    FUserName: String;

    procedure OnFailedRecv(Sender: TObject; const AAddress, ACode, AText: String;  var VContinue: Boolean);
Public
  MailsToSend : TObjectList<TSimpleMailMessage>;

  Constructor Create; virtual;
  Destructor Destroy; Override;

  Function Send : Boolean;

  property Host : String read FHost Write FHost;
  property Port : Word read FPort Write FPort;
  property password : string read FPassword Write FPassword;
  property UserName : String read FUserName Write FUserName;
  property LastError : String read FLastError;
end;


TGSSendMailSMTPIndy = class(TInterfacedObject, IGSMail)
public
  procedure sendSimpleMail(_to : TArray<String>; _from : String; _subject, _Body : String; _host : string; _port : integer; _user,_password : string);
end;

Function StripUserNameFromMailAddress(aUserName : string) : string;
function ExtractEmailDomainFromMailAddress(const email: string):string;
function ExtractMailFromMailAddress(const email: string):string;
implementation

Function StripUserNameFromMailAddress(aUserName : string) : string;
begin
  if pos('@',aUserName)>0 then
    result := Copy(aUserName,1,pos('@',aUserName)-1)
end;


function ExtractEmailDomainFromMailAddress(const email: string):string;
  var
     Pos_AT : integer;
  begin
     Pos_AT := Pos('@', email) ;
     Result := Copy(email, Pos_AT + 1, Length(email) - Pos_AT);
     Result := trim(Result);
  end;

//from "john doe <jdoe@gmail.com>" to "jdoe@gmail.com"
function ExtractMailFromMailAddress(const email: string):string;
  var
     Pos_L : integer;
     Pos_R : integer;
  begin
     result:= trim(email);
     if (Pos('<', email)=0) and ((Pos('>', email)=0)) then
       exit;
     Pos_L := Pos('<', result) ;
     Result := Copy(result, Pos_L + 1, Length(Result));
     Pos_R := Pos('>', result) ;
     Result := Copy(Result, 1,Pos_R-1);
     Result := trim(Result);
  end;

{ TCheckMailPOP3 }

constructor TCheckMailPOP3.Create;
begin
  inherited;
  MailContent := TObjectList<TSimpleMailMessage>.Create;
  FPort := 110;
  FPassword := EmptyStr;
  FHost := EmptyStr;
  FUserName := EmptyStr;
  FLastError := EmptyStr;
  FPop3 := TIdPOP3.Create(Nil);
  FSSLAddIn := Nil;
end;

function TCheckMailPOP3.DeleteMailServerSide(
  aMail: TSimpleMailMessage): Boolean;
begin
  Assert(Assigned(aMail));
  Assert(FPop3.Connected);
  Result := FPop3.Delete(aMail.MessageRetrivalIndex);
end;

destructor TCheckMailPOP3.Destroy;
begin
  if FPop3.Connected then
    FPop3.Disconnect;
  FreeAndNil(MailContent);
  FreeAndNil(FPop3);
  inherited;
end;

function TCheckMailPOP3.GetMail : Boolean;
begin
  Result := InternalGetMail;
end;

Function TCheckMailPOP3.InternalGetMail : Boolean;
var
  lMsg: TIdMessage;
  lOutMsg : TSimpleMailMessage;
  liCount: Integer;
  liMessages: Integer;
  i,j : integer;

  lStrLst : TStringList;
  lAttach : TMailMessageAttachement;
  ltool: IGSStringList;

begin
  ltool := TGSStringList.Create;
  FPop3.Disconnect;
  MailContent.Clear;
  Result := False;
  try
    FPop3.AutoLogin := True;
    FPop3.Password := FPassword;
    //FPop3.Username := Stripusername(FUserName);
    FPop3.Username := FUserName;
    FPop3.Host := FHost;
    FPop3.Port := FPort;
    if Assigned(FSSLAddIn) then
    begin
      FPop3.IOHandler := FSSLAddIn.GetHandler;
      FPop3.UseTLS := utUseImplicitTLS;
    end;


    FPop3.Connect;
    liMessages := FPop3.CheckMessages;
    Writeln('CheckMessages: ' + IntToSTr(liMessages));
    Writeln('Mail box size :' + IntToSTr(FPop3.RetrieveMailBoxSize));

    for i := 1 to liMessages do
    begin
      lMsg := TIdMessage.Create;
      LMsg.ContentType := 'text/plain';
      LMsg.CharSet := 'UTF-8';
      FPop3.Retrieve(i, lMsg);
      lOutMsg := TSimpleMailMessage.Create;
      lOutMsg.MessageRetrivalIndex := i;
      lOutMsg.MessageId := lMsg.MsgId;
      lStrLst := TStringList.Create;
      try
        for j := 0 to lMsg.FromList.Count-1 do
        begin
          lStrLst.Add(lMsg.FromList[j].Address);
        end;
        lOutMsg.Messagedate := lMsg.Date;
        lOutMsg.MailFrom := lStrLst.Text;
        lOutMsg.Subject := lMsg.Subject;
        lStrLst.Clear;
        for j := 0 to lMsg.Recipients.Count-1 do
        begin
          lStrLst.Add(lMsg.Recipients[j].Address);
        end;
        ltool.SetDelimiter(',');
        ltool.SetDelimitedText(lStrLst.Text);
        lOutMsg.MailTo := ltool.ToStringArray;
        lOutMsg.Body := lMsg.Body.Text;

        for liCount := 0 to lMsg.MessageParts.Count-1 do
        begin
          if lMsg.MessageParts[liCount] is TIdText then
          begin
            lAttach := TMailMessageAttachementText.Create;
            TMailMessageAttachementText(lAttach).PlainText := (lMsg.MessageParts[liCount] as TIdText).Body.Text;
            lOutMsg.Attachements.Add(lAttach);
            lOutMsg.Body := lOutMsg.Body + sLineBreak + TMailMessageAttachementText(lAttach).PlainText;
          end
          else
          if lMsg.MessageParts[liCount] is TIdAttachmentFile then
          begin
            lAttach := TMailMessageAttachementFile.Create;
            TIdAttachmentFile(lmsg.MessageParts[liCount]).SaveToStream(TMailMessageAttachementFile(lAttach).Content);
            TMailMessageAttachementFile(lAttach).OriginalFileName := TIdAttachmentFile(lmsg.MessageParts[liCount]).FileName;
            TMailMessageAttachementFile(lAttach).Content.Position := 0;
            lOutMsg.Attachements.Add(lAttach);
            lOutMsg.Body := lOutMsg.Body + sLineBreak + 'File attached : "'+
                            TMailMessageAttachementFile(lAttach).OriginalFileName+
                            '" ('+
                            IntToStr(TMailMessageAttachementFile(lAttach).Content.Size)+' byte(s))';
          end;

        end;
        MailContent.Add(lOutMsg);


      finally
        FreeAndNil(lStrLst);
      end;

    end;
    Result := true;
  Except
    On e : Exception do
    begin
      Result := False;
      FLastError := e.Message;
    end;
  end;
end;

procedure TCheckMailPOP3.SetSSLAddIn(const Value: TCheckMAilSSL);
begin
  FSSLAddIn := Value;
  if Assigned(FSSLAddIn) then
  begin
    FSSLAddIn.Host := Host;
    FSSLAddIn.Port := 995;
  end;
end;

{ TCheckMailSSL }

constructor TCheckMailSSL.Create;
begin
  FSSL := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FSSL.MaxLineAction := maException;
  FSSL.Port := 995;
  FSSL.DefaultPort := 0;
  FSSL.SSLOptions.Method := sslvTLSv1;
  FSSL.SSLOptions.Mode := sslmClient;
  FSSL.SSLOptions.VerifyMode := [];
  FSSL.SSLOptions.VerifyDepth := 0;
end;

destructor TCheckMailSSL.Destroy;
begin
  FreeAndNil(FSSL);
  inherited;
end;

function TCheckMailSSL.GetHandler: TIdSSLIOHandlerSocketOpenSSL;
begin
  FSSL.Host := FHost;
  FSSL.Port := FPort;
  Result := FSSL;
end;

{ TSimpleMailMessage }

constructor TSimpleMailMessage.Create;
begin
  inherited;
  Attachements := TObjectList<TMailMessageAttachement>.Create;
end;

destructor TSimpleMailMessage.Destroy;
begin
  FreeAndNil(Attachements);
  inherited;
end;

{ TMailMessageAttachementFile }

constructor TMailMessageAttachementFile.Create;
begin
  Inherited Create;
  Content := TMemoryStream.Create;
end;

destructor TMailMessageAttachementFile.Destroy;
begin
  FreeAndNil(Content);
  inherited;
end;

{ TSendMailSMTP }

constructor TSendMailSMTP.Create;
begin
  MailsToSend := TObjectList<TSimpleMailMessage>.Create;
  FPort := 110;
  FPassword := EmptyStr;
end;

destructor TSendMailSMTP.Destroy;
begin
  FreeAndNil(MailsToSend);
  inherited;
end;

procedure TSendMailSMTP.OnFailedRecv(Sender: TObject; const AAddress, ACode,
  AText: String; var VContinue: Boolean);
begin
  TLog.error(format('%s.OnFailedRecv : Address "%s" incorrect code %s : %s',[className,AAddress,ACode,AText]));
  VContinue := true;
end;

Function TSendMailSMTP.Send : Boolean;
var i,j : integer;
    lk : String;
    aEng : TIdSMTP;
    aMail : TIdMessage;
    lidatt : TIdAttachmentMemory;
    lTextPart : TIdText;
begin
  Result := true;
  if MailsToSend.Count>0 then
  begin
    aEng := TIdSMTP.Create(nil);
    aEng.Port := FPort;
    aEng.Host := FHost;
    aEng.Password := FPassword;
    aEng.Username := FUserName;
    aEng.OnFailedRecipient := OnFailedRecv;
    aEng.Connect;
    try
      for I := 0 to MailsToSend.Count-1 do
      begin
        aMail := TIdMessage.Create(nil);
        try
          aMail.ContentType := 'multipart/mixed';
          aMail.CharSet := 'UTF-8';
          for lk in MailsToSend[i].MailTo do
            aMail.Recipients.Add.Address := lk;

          aMail.From.Address := MailsToSend[i].MailFrom;
          aMail.Subject := MailsToSend[i].Subject;

          //aMail.Body.Text := MailsToSend[i].Body;
          lTextPart := TIdText.Create(aMail.MessageParts);
          lTextPart.ContentType := 'text/plain';
          lTextPart.ContentTransfer := '7bit';
          lTextPart.Body.Text := MailsToSend[i].Body;

          for j := 0 to MailsToSend[i].Attachements.Count-1 do
          begin
            if MailsToSend[i].Attachements[j] is TMailMessageAttachementFile then
            begin
//IdText := TIdText.Create(IdMessage1.MessageParts, nil);
//IdText.ContentType := 'text/plain';
//IdText.Body.Text := 'here is an attachment for you.';

lidatt := TIdAttachmentMemory.Create(aMail.MessageParts,TMailMessageAttachementFile(MailsToSend[i].Attachements[j]).Content);
//lidatt.ContentType := 'text/plain';
lidatt.FileName := TMailMessageAttachementFile(MailsToSend[i].Attachements[j]).OriginalFileName;

//              lidatt := TIdAttachmentFile.Create(aMail.MessageParts);
//              lidatt.FileName := TMailMessageAttachementFile(MailsToSend[i].Attachements[j]).OriginalFileName;
//              lidatt.LoadFromStream(TMailMessageAttachementFile(MailsToSend[i].Attachements[j]).Content);
            end;
          end;

          try
            aEng.Send(aMail);
          Except
            On E : Exception do
            begin
              Result := false;
              FLastError := ClassName+' mail index '+IntToStr(i)+' - '+ e.Message;
            end;
          end;

        finally
          FreeAndNil(aMail);
        end;
      end;
    finally
      FreeAndNil(aEng);
    end;
  end;
end;

{ TGSSendMailSMTPIndy }

procedure TGSSendMailSMTPIndy.sendSimpleMail(_to : TArray<String>; _from : String; _subject, _body : String; _host : string; _port : integer; _user,_password : string);
var l : TSendMailSMTP;
    m : TSimpleMailMessage;

    function toStringWithComma(_t : TArray<string>) : String;
    var ls : IGSStringList;
        ly : String;
    begin
      ls := TGSStringList.Create;
      if length(_t)>0 then begin
        for ly in _t do
          ls.add(ly);
      end;
      ls.SetDelimiter(',');
      result := ls.DelimitedText;
    end;

begin
  l := TSendMailSMTP.Create;
  m := TSimpleMailMessage.Create;
  try
    m.Messagedate := now;
    m.MailTo := _to;
    m.MailFrom := _from;
    m.Subject := UTF8Decode(_Subject);
    m.Body := UTF8Decode(_body);


    l.UserName := _user;
    l.password := _password;
    l.Host := _host;
    l.Port := _port;
    l.MailsToSend.Add(m);

    if not l.Send then
      Tlog.error(className+'.SendSimple : sending mail "'+m.Subject+'" failure.');
  finally
    FreeAndNil(l);
    //FreeAnDNil(m); //autofree.
  end;
end;

end.

