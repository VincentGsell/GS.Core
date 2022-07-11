Unit NumW_Trk;

{ Andy Preston, Apollo Developments, http://www.apollod.omnia.co.uk/aa/

  Numbers in words - Version 4.6

  Copyright (C) 1999 Andy Preston
  Copyright (C) 1999 Egemen Sen

  Object-Pascal Units to express a number as words in various languages

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  This unit is a modified version of NumW_Eng }

Interface

Uses
  Numw;

Type
  TNumWordsTurkce = Class (TNumWordsLangDriver)
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
    'bir', 'iki', 'üç', 'dört', 'beþ', 'altý', 'yedi', 'sekiz', 'dokuz'
    );

  Teens : Array [1..9] Of String = (
    'on bir', 'on iki', 'on üç', 'on dört', 'on beþ', 'on altý', 'on yedi', 'on sekiz', 'on dokuz'
    );

  TenTimes : Array [1..9] Of String = (
    'on', 'yirmi', 'otuz', 'kýrk', 'elli', 'altmýþ', 'yetmiþ', 'seksen', 'doksan'
    );


Function TNumWordsTurkce.HasGender (Gender : TGender) : Boolean;
Begin
  Raise Exception.Create ('Gender support not available in NumW_Trk.pas');
  Result := False;
End;

Function TNumWordsTurkce.DoTriplet (TheNumber : Integer) : String;
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
  If (Result <> '') And (Digit > 0) Then Result := ' ' + Result;
  If Digit > 0 Then Begin
    If Digit > 1 Then
      Result := Digits [Digit] + ' yüz ' + Result
    Else
      Result := 'yüz ' + Result;
  End;
  Result := Trim (Result);
End;

Function TNumWordsTurkce.NumberInWords (TheNumber : Integer; Gender : TGender) : String;
// Gender is currently ignored is that correct for Turkish?
Var
  Num, Triplet, Pass : Integer;
Begin
  If TheNumber < 0 Then Result := 'Minus ' + NumberInWords (-TheNumber, Gender)
  Else Begin
    Result := '';
    Num := TheNumber;
    If Num > 999999999 Then
      Raise Exception.Create ('999,999,999 dan büyük sayýlar yazýya çevirilemiyor');
    For Pass := 1 To 4 Do Begin
      Triplet := Num Mod 1000;
      Num := Num Div 1000;
      If Triplet > 0 Then Begin
        If (Pass > 1) And (Result <> '') Then Result := ' ' + Result;
        Case Pass Of
          2 : Result := ' bin' + Result;
          3 : Result := ' milyon' + Result;
        End;
        If ((TheNumber Div 1000) <> 1) Or (Num = 1) Then
          Result := Trim (DoTriplet (Triplet) + Result)
        Else
          Result := Trim (Result);
      End;
    End;
  End;
End;

Function TNumWordsTurkce.IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String;
Begin
  If Major = 0 Then Result := 'no' Else Result := NumberInWords (Major, CurrencyGender);
  If Major = 1 Then Result := Result + ' ' + BigSing Else Result := Result + ' ' + BigPlural;
  If Minor = 0 Then Result := Result + ' only'
  Else Begin
    Result := Result + ' ' + DoTriplet (Minor);
    If Minor = 1 Then Result := Result + ' ' + SmallSing Else Result := Result + ' ' + SmallPlural;
  End;
End;

End.