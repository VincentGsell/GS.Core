unit Base1024Tests;

{$mode delphiunicode}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  BaseTests,
  BcpBase1024,
  BcpUtils;

type

  TBase1024Tests = class(TTestCase)

  strict private
    class var
    F_btInstance: TBaseNcodingTests;

  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure EncodeDecodeTest(const str: string);
    procedure EncodeDecodeTestDefaultTextEncoding(const str: string);
    procedure EncodeDecodeTestUnicodeTextEncoding(const str: string);
  published
    procedure CallEncodeDecodeTest();
    procedure CallEncodeDecodeTestDefaultTextEncoding();
    procedure CallEncodeDecodeTestUnicodeTextEncoding();
    procedure TailTests;
  end;

implementation

procedure TBase1024Tests.SetUp;
begin
  F_btInstance := TBaseNcodingTests.Create();
  F_btInstance.FConverter := TBase1024.Create();
end;

procedure TBase1024Tests.TearDown;
begin
  F_btInstance.Free;
end;

procedure TBase1024Tests.CallEncodeDecodeTest();
var
  i: integer;
begin
  for i := 0 to Length(F_btInstance.sArray) - 1 do
  begin
    EncodeDecodeTest(F_btInstance.sArray[i]);
  end;
end;

procedure TBase1024Tests.EncodeDecodeTest(const str: string);
var
  encoded, decoded: string;
begin
  encoded := F_btInstance.FConverter.EncodeString(str);
  decoded := F_btInstance.FConverter.DecodeToString(encoded);

  CheckEquals(str, decoded);
end;

procedure TBase1024Tests.CallEncodeDecodeTestDefaultTextEncoding();

begin

    EncodeDecodeTestDefaultTextEncoding(F_btInstance.sArray[0]);

end;

procedure TBase1024Tests.EncodeDecodeTestDefaultTextEncoding(const str: string);
var
  encoded, decoded: string;
begin
  F_btInstance.FConverter.Encoding := TEncoding.Default;
  encoded := F_btInstance.FConverter.EncodeString(str);
  decoded := F_btInstance.FConverter.DecodeToString(encoded);

  CheckEquals(str, decoded);
end;

procedure TBase1024Tests.CallEncodeDecodeTestUnicodeTextEncoding();
var
  i: integer;
begin
  for i := 0 to Length(F_btInstance.sArray) - 1 do
  begin
    EncodeDecodeTestUnicodeTextEncoding(F_btInstance.sArray[i]);
  end;
end;

procedure TBase1024Tests.EncodeDecodeTestUnicodeTextEncoding(const str: string);
var
  encoded, decoded: string;
begin
  F_btInstance.FConverter.Encoding := TEncoding.Default;
  encoded := F_btInstance.FConverter.EncodeString(str);
  decoded := F_btInstance.FConverter.DecodeToString(encoded);

  CheckEquals(str, decoded);
end;

procedure TBase1024Tests.TailTests;
var
  testChar: char;
  strBuilder: string;
  bitsPerChar, bitsPerByte, charByteBitsLcm, maxTailLength, Count, i: integer;
  str, encoded: string;
  c: char;
begin
  testChar := 'a';
  strBuilder := '';
  if ((F_btInstance.FConverter.HaveSpecial) and
    (Trunc(F_btInstance.FConverter.BitsPerChars) mod 1 = 0) and
    (TUtils.IsPowerOf2(UInt32(Trunc(F_btInstance.FConverter.BitsPerChars))))) then
  begin
    bitsPerChar := Trunc(F_btInstance.FConverter.BitsPerChars);
    bitsPerByte := 8;
    charByteBitsLcm := TUtils.LCM(bitsPerByte, bitsPerChar);
    maxTailLength := charByteBitsLcm div bitsPerByte - 1;

    for i := 0 to (maxTailLength + 2) do
    begin
      str := strBuilder;
      encoded := F_btInstance.FConverter.EncodeString(str);
      Count := 0;
      for c in encoded do
      begin
        if c = F_btInstance.FConverter.Special then
          Inc(Count);
      end;

      if i = 0 then
        CheckEquals(0, Count)
      else
        CheckEquals
        ((maxTailLength - (i - 1) mod (maxTailLength + 1)), Count);

      CheckEquals(str, F_btInstance.FConverter.DecodeToString(encoded));
      strBuilder := strBuilder + (testChar);
    end;
  end;
end;

initialization

  RegisterTest(TBase1024Tests);
end.
