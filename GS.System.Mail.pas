unit GS.System.Mail;

interface

uses
  sysutils
  , classes;

Type

IGSMail = interface
  procedure sendSimpleMail( _to : TArray<String>; _from : String; _subject, _Body : String; _host : string; _port : Integer; _user,_password : string);
end;

function GSGetMailImplementation : IGSMail;

implementation

uses GS.System.Mail.Indy;

function GSGetMailImplementation : IGSMail;
begin
  result := TGSSendMailSMTPIndy.Create;
end;

end.
