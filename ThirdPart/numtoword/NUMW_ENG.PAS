Unit NumW_Eng;

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

Uses
  Numw;

Type
  TNumWordsEnglish = Class (TNumWordsLangDriver)
  Private
    Function DoTriplet (TheNumber : Integer) : String;
  Public
    Function NumberInWords (TheNumber : Integer; Gender : TGender) : String; Override;
    Function IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String; Override;
    Function HasGender (Gender : TGender) : Boolean; Override;
  End;

Implementation

Uses
  SysUtils;

Const
  Digits : Array [1..9] Of String = (
    'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine'
    );

  Teens : Array [1..9] Of String = (
    'eleven', 'twelve', 'thirteen', 'fourteen', 'fifteen', 'sixteen', 'seventeen', 'eighteen', 'nineteen'
    );

  TenTimes : Array [1..9] Of String = (
    'ten', 'twenty', 'thirty', 'forty', 'fifty', 'sixty', 'seventy', 'eighty', 'ninety'
    );


Function TNumWordsEnglish.HasGender (Gender : TGender) : Boolean;
Begin
  Result := False;
End;

Function TNumWordsEnglish.DoTriplet (TheNumber : Integer) : String;
Var
  Digit, Num : Integer;
Begin
  Result := '';
  Num := TheNumber Mod 100;
  If (Num > 10) And (Num < 20) Then Begin
    Result := Teens [Num - 10];
    Num := TheNumber Div 100;
  End
  Else Begin
    Num := TheNumber;
    Digit := Num Mod 10;
    Num := Num Div 10;
    If Digit > 0 Then Result := Digits [Digit];
    Digit := Num Mod 10;
    Num := Num Div 10;
    If Digit > 0 Then Result := TenTimes [Digit] + ' ' + Result;
    Result := Trim (Result);
  End;
  Digit := Num Mod 10;
  If (Result <> '') And (Digit > 0) Then Result := 'and ' + Result;
  If Digit > 0 Then Result := Digits [Digit] + ' hundred ' + Result;
  Result := Trim (Result);
End;

Function TNumWordsEnglish.NumberInWords (TheNumber : Integer; Gender : TGender) : String;
Var
  Num, Triplet, Pass : Integer;
Begin
  If TheNumber < 0 Then Result := 'Minus ' + NumberInWords (-TheNumber, Gender)
  Else Begin
    Result := '';
    Num := TheNumber;
    If Num > 999999999 Then
      Raise Exception.Create ('Can''t express more than 999,999,999 in words');
    For Pass := 1 To 3 Do Begin
      Triplet := Num Mod 1000;
      Num := Num Div 1000;
      If Triplet > 0 Then Begin
        If (Pass > 1) And (Result <> '') Then Result := ', ' + Result;
        Case Pass Of
          2 : Result := ' thousand' + Result;
          3 : Result := ' million' + Result;
        End;
        Result := Trim (DoTriplet (Triplet) + Result);
      End;
    End;
  End;
End;

Function TNumWordsEnglish.IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String;
Var
  Big, Small : String;
Begin
  If EuroMode Then Begin
    If Major = 1 Then Big := 'Euro' Else Big := 'Euros';
    If Minor = 1 Then Small := 'Cent' Else Small := 'Cents';
  End
  Else Begin
    If Major = 1 Then Big := BigSing Else Big := BigPlural;
    If Minor = 1 Then Small := SmallSing Else Small := SmallPlural;
  End;
  If Major = 0 Then Result := 'no' Else Result := NumberInWords (Major, CurrencyGender);
  Result := Result + ' ' + Big;
  If Minor = 0 Then Result := Result + ' only'
  Else Result := Result + ' and ' + DoTriplet (Minor) + ' ' + Small;
End;

End.