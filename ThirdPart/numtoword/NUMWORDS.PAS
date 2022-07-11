Unit NumWords;

{ Andy Preston, Apollo Developments, http://www.apollod.omnia.co.uk/aa/

  Numbers in words - Version 4.6

  Copyright (C) 1999 - 2001 Andy Preston

  Object-Pascal Units to express a number as words in various languages

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. }

{
20220124 - VGS - Remove Windows unit dependancy.
}

Interface

Uses
  NumW;

Const
  noEuro : Boolean = False;
  useEuro : Boolean = True;

Type
  TNumWords = Class
  Private
    FLanguageDriver : TNumWordsLangDriver;
  Public
    Constructor Create(langCode : TLangCode; countryCode : Integer); Reintroduce;
    Destructor Destroy; Override;

    Function HasGender (Gender : TGender) : Boolean;
    Function Number (TheNumber : Integer; Gender : TGender) : String;
    Function EMoney (TheAmount : Extended; EuroMode : Boolean) : String;
    { Major in Pounds/Dollars etc, Minor in Pence/Cents etc }
    Function IMoney (Major, Minor : Integer; EuroMode : Boolean) : String;
  End;

Implementation

Uses
  SysUtils, NumW_Eng, NumW_Deu, NumW_Esp, NumW_Ita, NumW_Fra, NumW_Trk;

Constructor TNumWords.Create(langCode : TLangCode; countryCode : Integer);
Begin
  Inherited Create;
  FLanguageDriver := Nil;
  Case LangCode Of
    lcENG : FLanguageDriver := TNumWordsEnglish.Create;
    lcDEU : FLanguageDriver := TNumWordsDeutch.Create;
    lcESP : FLanguageDriver := TNumWordsEspanol.Create;
    lcITA : FLanguageDriver := TNumWordsItaliano.Create;
    lcFRA : FLanguageDriver := TNumWordsFrancais.Create;
    lcTRK : FLanguageDriver := TNumWordsTurkce.Create;
    Else Raise EConvertError.Create ('Language not supported: ' +IntToStr(countryCode));
  End;
End;

Destructor TNumWords.Destroy;
Begin
  If FLanguageDriver <> Nil Then
    FreeAndNil(FLanguageDriver);
  Inherited Destroy;
End;


Function TNumWords.HasGender (Gender : TGender) : Boolean;
Begin
  Assert(Assigned(FLanguageDriver));
  Result := FLanguageDriver.HasGender (Gender);
End;

Function TNumWords.Number (TheNumber : Integer; Gender : TGender) : String;
Begin
  Assert(Assigned(FLanguageDriver));
  Result := FLanguageDriver.NumberInWords (TheNumber, Gender);
End;

Function TNumWords.EMoney (TheAmount : Extended; EuroMode : Boolean) : String;
Begin
  Assert(Assigned(FLanguageDriver));
  Result := FLanguageDriver.EMoneyInWords (TheAmount, EuroMode);
End;

Function TNumWords.IMoney (Major, Minor : Integer; EuroMode : Boolean) : String;
Begin
  Assert(Assigned(FLanguageDriver));
  Result := FLanguageDriver.IMoneyInWords (Major, Minor, EuroMode);
End;


End.