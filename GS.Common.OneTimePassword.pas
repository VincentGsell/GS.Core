unit GS.Common.OneTimePassword;

interface

uses Sysutils, classes;

Type

IGSOTP = interface
  function validate(secret : string; token : String) : boolean;
  function generate(secret : string) : String;
end;

function gsOTPGetImplementation(const Id : String = '') : iGSOTP;

implementation

uses GS.Common.OneTimePassword.Default;

function gsOTPGetImplementation(const Id : String = '') : iGSOTP;
begin
  result := TGSOPTSha1TimeBased.Create;
end;


end.
