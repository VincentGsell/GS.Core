unit GS.Common.TextConversion.NumToWord;

interface

uses sysutils, classes,
     GS.Common.TextConversion,
     NumW,
     NumW_Fra,
     NumW_Ita,
     NumW_Eng;


Type
TGSNumToHumanWordConversion = class(TGSCustomnFloatConvertEngine)
Protected
  FEng : TNumWordsLangDriver;
Public
  Destructor Destroy; Override;
  function convert(aValue : Single) : string; override;
end;


TGSNumToHumanWordFrancaisConversion = class(TGSNumToHumanWordConversion)
Public
  Constructor Create; Override;
  class function name : string; override;
end;

TGSNumToHumanWordEnglishConversion = class(TGSNumToHumanWordConversion)
Public
  Constructor Create; Override;
  class function name : string; override;
end;

TGSNumToHumanWordItalianoConversion = class(TGSNumToHumanWordConversion)
Public
  Constructor Create; Override;
  class function name : string; override;
end;



implementation


{ TGSNumToHumanWordConversion }

function TGSNumToHumanWordConversion.convert(aValue: Single): string;
begin
  result := FEng.EMoneyInWords(aValue,false);
end;

destructor TGSNumToHumanWordConversion.Destroy;
begin
  FreeAndNil(FEng);
  inherited;
end;

{ TGSNumToHumanWordFrancaisConversion }

constructor TGSNumToHumanWordFrancaisConversion.Create;
begin
  inherited;
  FEng :=TNumWordsFrancais.Create;
end;

class function TGSNumToHumanWordFrancaisConversion.name: string;
begin
  result := 'NumberToHumanWord_Francais'
end;


{ TGSNumToHumanWordEnglishConversion }

constructor TGSNumToHumanWordEnglishConversion.Create;
begin
  inherited;
  FEng :=TNumWordsEnglish.Create;
end;

class function TGSNumToHumanWordEnglishConversion.name: string;
begin
  result := 'NumberToHumanWord_English'
end;

{ TGSNumToHumanWordItalianoConversion }

constructor TGSNumToHumanWordItalianoConversion.Create;
begin
  inherited;
  FEng :=TNumWordsItaliano.Create;
end;

class function TGSNumToHumanWordItalianoConversion.name: string;
begin
  result := 'NumberToHumanWord_Italiano'
end;

Initialization

TGSTextConversionFactory.register(TGSNumToHumanWordFrancaisConversion.Name,  TGSNumToHumanWordFrancaisConversion);
TGSTextConversionFactory.register(TGSNumToHumanWordEnglishConversion.Name,  TGSNumToHumanWordEnglishConversion);
TGSTextConversionFactory.register(TGSNumToHumanWordItalianoConversion.Name,  TGSNumToHumanWordItalianoConversion);

Finalization


end.
