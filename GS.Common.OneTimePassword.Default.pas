unit GS.Common.OneTimePassword.Default;

interface

uses sysutils
     ,classes
     ,GS.Common.OneTimePassword
     ,Base32U
     ,GoogleOTP;

Type
TGSOPTSha1TimeBased = class(TInterfacedObject, IGSOTP)
private
  FWindowRange: byte;
  FTokenLen: byte;
public
  constructor Create; virtual;
  function validate(secret : string; token : String) : boolean;
  function generate(secret : string) : String;

  property ValidationWindowRangeInSec : byte read FWindowRange write FWindowRange;
  property tokenLen : byte read FTokenLen write FTokenLen;
end;

implementation

{ TGSOPTSha1TimeBased }

constructor TGSOPTSha1TimeBased.Create;
begin
  inherited;
  FWindowRange := 4;
  FTokenLen := 6;
end;

function TGSOPTSha1TimeBased.generate(secret: string): String;
begin
  result := IntToStr(CalculateOTP(secret,-1,FTokenLen));
end;

function TGSOPTSha1TimeBased.validate(secret, token: String): boolean;
var li : integer;
begin
  li := StrToIntDef(token,0);
  Assert(li>0);
  result := ValidateTOPTTimeBased(secret,li,FWindowRange,FTokenLen);
end;

end.
