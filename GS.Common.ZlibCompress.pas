unit GS.Common.ZlibCompress;

interface

uses classes, sysutils, NetEncoding, ZLib;

Type

TGSZLibCompressTools = class
  class function StringToZCompressedBase64String(value : String) : string;
  class function ZCompressedBase64StringToString(value : String) : string;
end;

implementation

class function TGSZLibCompressTools.StringToZCompressedBase64String(value : String) : string;
var le : TBase64Encoding;
    lcps : TBytes;
begin
  lcps := ZCompressStr(value);
  le := TBase64Encoding.Create;
  try
    result := le.EncodeBytesToString(Pointer(@lcps[0]),length(lcps));
  finally
    FreeAndNil(le);
  end;
end;

class function TGSZLibCompressTools.ZCompressedBase64StringToString(value : String) : string;
var le : TBase64Encoding;
    lcps : TBytes;
begin
  le := TBase64Encoding.Create;
  try
    lcps := le.DecodeStringToBytes(value);
  finally
    FreeAndNil(le);
  end;
  result := ZDecompressStr(lcps);
end;

end.
