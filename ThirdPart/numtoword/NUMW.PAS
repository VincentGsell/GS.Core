Unit NumW;

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

Interface

Type
  TLangCode = (lcDEU,lcENG,lcESP,lcFRA,lcITA,lcTRK); //Do not mind : Alphabetical order. ;)
  TGender = (genMale, genFemale, genNeuter);

  TNumWordsLangDriver = Class (TObject)
  Protected
    { Currency Unit Variables }
    BigSing, BigPlural, SmallSing, SmallPlural : String;
    CurrencyGender : TGender;
  Public
    Function HasGender (Gender : TGender) : Boolean; Virtual; Abstract;
    Function NumberInWords (TheNumber : Integer; Gender : TGender) : String; Virtual; Abstract;
    { Major in Pounds/Dollars etc, Minor in Pence/Cents etc }
    Function IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String; Virtual; Abstract;
    Function EMoneyInWords (TheAmount : Extended; EuroMode : Boolean) : String;
  End;

Implementation

Uses
  SysUtils;

// As you can tell from my comments, I don't like my solution to this.
// I would like to replace this routine with the one from bVerbose, Marco's code is better than mine - Andy

Function TNumWordsLangDriver.EMoneyInWords (TheAmount : Extended; EuroMode : Boolean) : String;
Var
  AsString : String;
  P : Integer;
Begin
  // It would seem logical at first to do something like this:
  // Major:=Int(TheAmount);
  // Result:=IMoneyInWords (Trunc (Major), Trunc (Round((TheAmount-Major)*100.0)));
  // (Under Delphi 2.0 at least) you get loads of very small (but significant) floating point errors.
  // If I'm being stupid, please e-mail and tell me so. But in the meantime here's this:
  AsString := FormatFloat ('0.00', TheAmount);
  P := Pos (FormatSettings.DecimalSeparator, AsString); // 06.09.1999 Andreas Schmidt (Originally used a hard coded '.')
  Result := IMoneyInWords (StrToInt (Copy (AsString, 1, P - 1)),
    StrToInt (Copy (AsString, P + 1, Length (AsString) - P)), EuroMode);
End;

Const
  CTRY_TURKEY = 90;

{

Constructor TNumWordsLangDriver.Create (CountryCode : Integer);
// This procedure should get A LOT shorter when all the Euro countries have abandoned their national currencies
Begin
  Inherited Create;
  Case CountryCode Of
    2, 1, 64, 61 : Begin
        // Currency Gender is probably not correct
        // (It only applies to French/Canada anyway) <-- nop. German Only. Anyway, Gender idea was bad. (VGS)
        // English (Canada / USA / Oz / New Zealand) does not have gender. Noi
        CurrencyGender := genMale;
        BigSing := 'Dollar';
        BigPlural := 'Dollars';
        SmallSing := 'Cent';
        SmallPlural := 'Cents';
      End;
    44 : Begin //CTRY_UNITED_KINGDOM
        CurrencyGender := genNeuter; // No gender in English
        BigSing := 'Pound';
        BigPlural := 'Pounds';
        SmallSing := 'Penny';
        SmallPlural := 'Pence';
      End;
    353 : Begin
        CurrencyGender := genNeuter; // No gender in English
        BigSing := 'Pound';
        BigPlural := 'Pounds';
        SmallSing := 'Penny';
        SmallPlural := 'Pence';
      End;
    39 : Begin
        // Currency Gender is probably not correct
        CurrencyGender := genMale;
        BigSing := 'Mark'; // Thanks to Andreas Schmidt
        BigPlural := 'Mark';
        SmallSing := 'Pfennig';
        SmallPlural := 'Pfennige';
      End;
    43 : Begin
        // Currency Gender is probably not correct
        CurrencyGender := genMale;
        BigSing := 'Schilling'; // 06.09.1999 Andreas Schmidt
        BigPlural := 'Schillinge';
        SmallSing := 'Grosche';
        SmallPlural := 'Groschen';
      End;
    CTRY_SPAIN : Begin
        // Currency Gender is probably not correct
        CurrencyGender := genMale;
        BigSing := 'Euro';
        BigPlural := 'Euros';
        SmallSing := 'Centime';
        SmallPlural := 'Centimes';
      End;

    CTRY_SWITZERLAND : Begin
        // The whole currency gender thing could go mad here,
        // what with French, Itallian and Genrman all being used

        // Currency Gender is probably not correct
        CurrencyGender := genMale;
        BigSing := 'Franken'; // 06.09.1999 Andreas Schmidt
        BigPlural := 'Franken';
        SmallSing := 'Rappen';
        SmallPlural := 'Rappen';
      End;
    CTRY_TURKEY  : Begin
        BigSing := 'Lira';
        BigPlural := 'Lira';
        SmallSing := 'Kuruþ';
        SmallPlural := 'Kuruþ';
      End;
    //CTRY_NETHERLANDS : Begin
    //    CurrencyGender:=genMale;
    //      BigSing:='Guilder';
    //      BigPlural:='????';
    //      SmallSing:='Cent';
    //      SmallPlural:='???';
    //  End;
  Else Raise EConvertError.Create ('NumWords - Country Not Supported');
  End;
End;
}

// CTRY_BRAZIL, CTRY_MEXICO, CTRY_SPAIN,
// CTRY_PORTUGAL,
// CTRY_NORWAY, CTRY_FINLAND, CTRY_ICELAND,
// CTRY_SWEDEN, CTRY_DENMARK, CTRY_BELGIUM,
// CTRY_JAPAN, CTRY_PRCHINA, CTRY_SOUTH_KOREA,
// CTRY_TAIWAN

End.