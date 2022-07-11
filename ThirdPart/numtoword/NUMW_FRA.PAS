Unit NumW_Fra;

{ Andy Preston, Apollo Developments, http://www.apollod.omnia.co.uk/aa/

  Numbers in words - Version 4.6

  Copyright (C) 1999 Andy Preston
  Copyright (C) 1992 Tom Peiffer

  Object-Pascal Units to express a number as words in various languages

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  This unit an 'objectified' version of NoLettr By Tom Peiffer
}

Interface

Uses
  NumW;

Type
  TNumWordsFrancais = Class (TNumWordsLangDriver)
  Private
    Procedure a_cent (chiff1, chiff2 : Word; Var sCent : String);
  Public
    Constructor Create; Reintroduce;

    Function NumberInWords (TheNumber : Integer; Gender : TGender) : String; Override;
    Function IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String; Override;
    Function HasGender (Gender : TGender) : Boolean; Override;
  End;

Implementation

Const
  Cvingt : Array [0..19] Of String = (
    '', 'un', 'deux', 'trois', 'quatre', 'cinq', 'six', 'sept', 'huit', 'neuf', 'dix',
    'onze', 'douze', 'treize', 'quatorze', 'quinze', 'seize', 'dix-sept', 'dix-huit', 'dix-neuf');

  Ccent : Array [2..9] Of String = ('vingt', 'trente', 'quarante', 'cinquante',
    'soixante', 'soixante', 'quatre-vingt', 'quatre-vingt');

constructor TNumWordsFrancais.Create;
begin
  Inherited;
  CurrencyGender := genMale;
  BigSing := 'Euro';
  BigPlural := 'Euros';
  SmallSing := 'Centime';
  SmallPlural := 'Centimess';
end;

Function TNumWordsFrancais.HasGender (Gender : TGender) : Boolean;
Begin
  Result := (Gender <> genNeuter);
End;

Procedure TNumWordsFrancais.a_cent (chiff1, chiff2 : Word; Var sCent : String);
Var
  x10, prem, dern : Word;
Begin
  x10 := 10 * chiff1 + chiff2;
  prem := chiff1;
  dern := chiff2;
  If x10 <= 19 Then insert (Cvingt [x10], sCent, 1)
  Else If prem In [7, 9] Then Begin
    If dern <> 0 Then Begin
      insert (Cvingt [dern + 10], sCent, 1);
      If dern In [1] Then Begin
        If prem <> 9 Then insert (' et ', sCent, 1) Else insert ('-', sCent, 1);
      End
      Else insert ('-', sCent, 1);
      insert (Ccent [prem], sCent, 1);
    End
    Else insert (Ccent [prem] + '-' + Cvingt [dern + 10], sCent, 1);
  End
  Else { if not prem in [7,9] }  Begin
    If dern <> 0 Then insert (Cvingt [dern], sCent, 1);
    If dern = 0 Then If prem = 8 Then insert ('s', sCent, 1);
    Begin
      If dern = 1 Then Begin
        If prem <> 8 Then insert (' et ', sCent, 1) Else insert ('-', sCent, 1)
      End;
      If dern In [2..9] Then insert ('-', sCent, 1);
    End;
    insert (Ccent [prem], sCent, 1);
  End;
End;

Function TNumWordsFrancais.NumberInWords (TheNumber : Integer; Gender : TGender) : String;
// Note the new parameter, Gender (genMale, genFemale, or genNeuter)
Var
  s, strn : String;
  res : Integer;
  p, c1, c2, c3, nc, i : Word;
  chiff : Array [0..9] Of Byte;
Begin
  For i := 0 To 9 Do chiff [i] := 0;
  Result := '';

  str (TheNumber : 0, strn);
  val (strn, TheNumber, res);
  nc := Length (strn);
  For i := 1 To nc Do Begin
    s := strn [i];
    val (s, chiff [i], res);
  End;
  If TheNumber = 0 Then Result := 'zéro'
  Else If nc = 1 Then Result := Cvingt [chiff [nc]]
  Else If nc > 1 Then Begin
    a_cent (chiff [nc - 1], chiff [nc], Result);
    c1 := 0;
    {c2 := 0;  Hint Value assigned to c2 never used Andy Preston 19-Oct-1999 }
    If nc >= 3 Then Begin
      c2 := chiff [nc - 2];
      If (Result = '') And (c2 > 1) Then insert ('s', Result, 1)
      Else
        insert (' ', Result, 1);
      If c2 > 0 Then insert (' cent', Result, 1);
      If c2 > 1 Then a_cent (c1, c2, Result);
    End;
    If nc >= 4 Then Begin
      c1 := 0;
      c2 := 0;
      c3 := 0;
      If nc <= 4 Then c3 := chiff [nc - 3];
      If nc > 4 Then Begin
        c2 := chiff [nc - 4];
        c3 := chiff [nc - 3];
      End;
      If nc >= 5 Then c1 := chiff [nc - 5];
      insert (' ', Result, 1);
      If c1 * 100 + c2 * 10 + c3 > 0 Then insert (' mille', Result, 1);
      If c2 * 10 + c3 > 1 Then a_cent (c2, c3, Result);
      If c1 > 0 Then Begin
        insert (' cent ', Result, 1);
        If c1 > 1 Then insert (Cvingt [c1], Result, 1);
      End;
    End;
    If nc >= 7 Then Begin
      c1 := 0;
      c2 := 0;
      c3 := 0;
      If nc <= 7 Then c3 := chiff [nc - 6];
      If nc > 7 Then Begin
        c2 := chiff [nc - 7];
        c3 := chiff [nc - 6];
      End;
      If nc >= 8 Then c1 := chiff [nc - 8];
      insert (' ', Result, 1);
      If c1 * 100 + c2 * 10 + c3 > 1 Then insert ('s', Result, 1);
      If c1 * 100 + c2 * 10 + c3 > 0 Then insert (' million', Result, 1);
      a_cent (c2, c3, Result);
      If c1 > 0 Then Begin
        insert (' cent ', Result, 1);
        If c1 > 1 Then insert (Cvingt [c1], Result, 1);
      End;
    End;
    If nc >= 10 Then Begin
      c1 := 0;
      c2 := 0;
      c3 := 0;
      If nc <= 10 Then c3 := chiff [nc - 9];
      If nc > 10 Then Begin
        c2 := chiff [nc - 10];
        c3 := chiff [nc - 9];
      End;
      If nc >= 11 Then c1 := chiff [nc - 11];
      insert (' ', Result, 1);
      If c1 * 100 + c2 * 10 + c3 > 1 Then insert ('s', Result, 1);
      If c1 * 100 + c2 * 10 + c3 > 0 Then insert (' milliard', Result, 1);
      a_cent (c2, c3, Result);
      If c1 > 0 Then Begin
        insert (' cent ', Result, 1);
        If c1 > 1 Then insert (Cvingt [c1], Result, 1);
      End;
    End;
  End;

  Repeat
    p := Pos ('  ', Result);
    If p <> 0 Then delete (Result, p, 1);
  Until p = 0;
  If Result [Length (Result)] = ' ' Then Result := Copy (Result, 1, Length (Result) - 1);
  If Result [1] = ' ' Then Result := Copy (Result, 2, Length (Result));
End;

// This is my (Andy Preston) own code but is 'inspired' by Thierry Boulestin's Nb2Lettre

Function TNumWordsFrancais.IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String;
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
  Result := NumberInWords (Major, CurrencyGender) + ' ' + Big;
  If Minor > 0 Then Result := Result + ' et ' + NumberInWords (Minor, CurrencyGender) + ' ' + Small;
End;

End.