unit GS.Common.TextConversion;

interface

uses SysUtils, Generics.Collections;

Type

IGSFloatConvertEngine = interface
  function convert(aValue : Single) : string;
end;

TGSCustomnFloatConvertEngine = class(TInterfacedObject,IGSFloatConvertEngine)
  Constructor Create; virtual;
  function convert(aValue : Single) : string; Virtual; abstract;
  Class function name : string; virtual; Abstract;
end;

TGSFloatConvertEngineClass = class of TGSCustomnFloatConvertEngine;

TGSJustForFunRomanConversion = class(TGSCustomnFloatConvertEngine)
  function convert(aValue : Single) : string; override;
  class function name : string; override;
end;

TGSTextConversionFactory = class
  private
  class var FDict : TDictionary<String,TGSFloatConvertEngineClass>;
  class procedure CheckDict;
  class procedure RelDict;
  public
  class function GetImplFloatToStringConverter(aName : string) : IGSFloatConvertEngine;
  class procedure register(aName : String; aClass : TGSFloatConvertEngineClass);
end;

implementation



function HexToString(Hex: String): String;
var I : Integer;
begin
  Result:= '';
  for I := 1 to length (Hex) div 2 do
    Result:= Result+Char(StrToInt('$'+Copy(Hex,(I-1)*2+1,2)));
end;

function StringToHex(Str: String): String;
var I: Integer;
begin
  Result:= '';
  for I := 1 to length (Str) do
    Result:= Result+IntToHex(ord(Str[i]),2);
end;

function RomanNumber(Number:integer):string;
   var
      i,j,l: integer;
      digit,valtest: integer;
      numstr: string[4];
      romtab: array[1..4] of string[3];
begin
   romtab[1] := 'XVI';
   romtab[2] := 'CLX';
   romtab[3] := 'MDC';
   romtab[4] := '  M';
   result := '';
   str(number,numstr);
   l := length(numstr);
   for i := 1 to l do begin
      val(numstr[(l-i)+1],digit,valtest);
      case digit of
         0..3:
         for j := 0 to digit-1 do begin
            result := romtab[i][3] + result;
         end; {end for}
         4: result := romtab[i][3] + romtab[i][2] + result;
         5..8:
         begin
            for j := 5 to digit-1 do begin
               result := romtab[i][3] + result;
            end; {end for}
            result := romtab[i][2] + result
         end;
         9: result := romtab[i][3] + romtab[i][1] + result;
      end; {end case}
   end; {end for}
end; { roman }



{ TGSJustForFunRomanConversion }

function TGSJustForFunRomanConversion.convert(aValue: Single): string;
begin
  result := RomanNumber(Round(aValue));
end;

class function TGSJustForFunRomanConversion.name: string;
begin
  result := 'Roman';
end;


{ TGSTextConversionFactory }

class procedure TGSTextConversionFactory.CheckDict;
begin
  if Not(Assigned(FDict)) then
    FDict := TDictionary<String,TGSFloatConvertEngineClass>.Create;
end;

class function TGSTextConversionFactory.GetImplFloatToStringConverter(
  aName: string): IGSFloatConvertEngine;
var l : TGSFloatConvertEngineClass;
begin
  if FDict.TryGetValue(aName,l) then
    result := (l.Create) as IGSFloatConvertEngine;
 end;

class procedure TGSTextConversionFactory.register(aName : String; aClass: TGSFloatConvertEngineClass);
var l : TGSFloatConvertEngineClass;
begin
  if not FDict.TryGetValue(aName,l) then
    FDict.Add(aName,aClass);
end;

class procedure TGSTextConversionFactory.RelDict;
begin
  FreeAndNil(FDict);
end;

{ TGSCustomnFloatConvertEngine }


{ TGSCustomnFloatConvertEngine }

constructor TGSCustomnFloatConvertEngine.Create;
begin
  //Must be here to be called by inherited.
end;

Initialization
  TGSTextConversionFactory.CheckDict;
  TGSTextConversionFactory.register(TGSJustForFunRomanConversion.Name,  TGSJustForFunRomanConversion);

Finalization
  TGSTextConversionFactory.RelDict;

end.


