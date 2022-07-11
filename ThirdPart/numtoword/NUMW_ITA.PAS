Unit NumW_Ita;

{ Andy Preston, Apollo Developments, http://www.apollod.omnia.co.uk/aa/

  Numbers in words - Version 4.6

  Copyright (C) 1999 Andy Preston
  Copyright (C) 1999 Marco Bianchini

  Object-Pascal Units to express a number as words in various languages

  This unit is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  This unit an modified version of bVerbose By Marco Bianchini m.bianchini@mo.nettuno.it }

Interface

Uses
  NumW, Windows;

Type
  TNumWordsItaliano = Class (TNumWordsLangDriver)
  Private
    Function CarToVerb (Value : Cardinal) : String;
    Function VerbBilions (Value : Integer) : String;
    Function VerbMilions (Value : Integer) : String;
    Function Verb1000 (Value : Integer) : String;
    Function Verb1000000 (Value : Integer) : String;
    Function Verb100 (Value : Integer) : String;
    Procedure KillVocalConflict (Var S1, S2 : String);
  Public
    Constructor Create; Reintroduce;

    Function NumberInWords (TheNumber : Integer; Gender : TGender) : String; Override;
    Function IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String; Override;
    Function HasGender (Gender : TGender) : Boolean; Override;
  End;

Implementation

Uses
  Math;

Const
  cVenti : Array [0..20] Of String = ('Zero', 'Uno', 'Due', 'Tre', 'Quattro', 'Cinque',
    'Sei', 'Sette', 'Otto', 'Nove', 'Dieci', 'Undici', 'Dodici', 'Tredici', 'Quattordici',
    'Quindici', 'Sedici', 'Diciassette', 'Diciotto', 'Diciannove', 'Venti');

  cDecine : Array [2..9] Of String = ('Venti', 'Trenta', 'Quaranta', 'Cinquanta',
    'Sessanta', 'Settanta', 'Ottanta', 'Novanta');

  Vocals = ['A', 'E', 'I', 'O', 'U'];

constructor TNumWordsItaliano.Create;
begin
  // Currency Gender is probably not correct
  CurrencyGender := genFemale;
  BigSing := 'Euro'; // 06.09.1999 Andreas Schmidt
  BigPlural := 'Euro';
  SmallSing := '';
  SmallPlural := '';
end;

Function TNumWordsItaliano.HasGender (Gender : TGender) : Boolean;
Begin
  Result := (Gender <> genNeuter);
End;

Procedure TNumWordsItaliano.KillVocalConflict (Var S1, S2 : String);
Begin
  If (S1 <> '') And (S2 <> '') Then
    If (UpCase (S1 [Length (s1)]) In Vocals) And (UpCase (S2 [1]) In Vocals) Then
      S1 := Copy (S1, 1, Length (S1) - 1);
End;

Function TNumWordsItaliano.Verb100 (Value : Integer) : String;
Var
  R, U, D : Integer;
  s1, s2 : String;
Begin
  If Value <= 20 Then Begin
    If Value > 0 Then
      Result := cVenti [Value]
    Else
      Result := '';
  End
  Else Begin
    R := Value;
    D := R Div 10;
    R := R - d * 10;
    U := R;
    S1 := cDecine [D];
    If U > 0 Then
      S2 := cVenti [U]
    Else
      S2 := '';
    KillVocalConflict (S1, S2);
    Result := s1 + s2;
  End;
End;

Function TNumWordsItaliano.Verb1000 (Value : Integer) : String;
Var
  R, C : Integer;
  s1, s2 : String;
Begin
  R := Value;
  C := R Div 100;
  R := R - C * 100;
  S2 := Verb100 (R);
  If C > 1 Then Begin
    S1 := cVenti [C] + 'cento';
  End
  Else
    If c = 1 Then Begin
      S1 := 'Cento';
    End
    Else
      S1 := '';
  KillVocalConflict (S1, S2);
  Result := s1 + s2;
End;

Function TNumWordsItaliano.Verb1000000 (Value : Integer) : String;
Var
  R, M : Integer;
Begin
  M := Value Div 1000;
  R := Value - M * 1000;
  While M >= 1000 Do
    Dec (M, 1000);
  If M = 0 Then
    Result := ''
  Else
    If M = 1 Then
      Result := 'Mille'
    Else
      Result := Verb1000 (M) + 'mila';
  If R > 0 Then Begin
    Result := Result + Verb1000 (R);
  End;
End;

Function TNumWordsItaliano.VerbMilions (Value : Integer) : String;
Var
  R, M : Integer;
Begin
  M := Value Div 1000000;
  R := Value - M * 1000000;
  While M >= 1000000 Do
    Dec (M, 1000000);
  If M = 0 Then
    Result := ''
  Else
    If M = 1 Then
      Result := 'Unmilione'
    Else
      Result := Verb1000 (M) + 'milioni';
  If R > 0 Then Begin
    Result := Result + Verb1000000 (R);
  End;
End;

Function TNumWordsItaliano.VerbBilions (Value : Integer) : String;
Var
  R, M : Integer;
Begin
  M := Value Div 1000000000;
  R := Value - M * 1000000000;
  If M = 0 Then
    Result := ''
  Else
    If M = 1 Then
      Result := 'Unmiliardo'
    Else
      Result := VerbMilions (M) + 'miliardi';
  If R > 0 Then Begin
    Result := Result + VerbMilions (R);
  End;
End;

Function TNumWordsItaliano.CarToVerb (Value : Cardinal) : String;
Begin
  If Value < 20 Then Result := cVenti [Value] Else Result := VerbBilions (Value);
End;

Function TNumWordsItaliano.NumberInWords (TheNumber : Integer; Gender : TGender) : String;
// Note the new parameter, Gender (genMale, genFemale, or genNeuter) is currently ignored
Begin
  Result := CarToVerb (Abs (TheNumber));
  If TheNumber < 0 Then Result := 'Meno ' + Result;
End;

Function TNumWordsItaliano.IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String;
Var
  V1, V2 : Integer;
Begin
  If EuroMode Then Begin
    Result := 'Euro currency support not yet included';
  End
  Else Begin
    // 06.09.1999 Andreas Schmidt
    // !!!! this should be checked by Marco Bianchini    :-)
    //If (Major >= -1) And (Major <= 1) Then
    //  Result := NumberInWords (Major, CurrencyGender) + ' ' + BigSing
    //Else
    //  Result := NumberInWords (Major, CurrencyGender) + ' ' + BigPlural;

    // This is an attempt at copying Marco's original code.
    // I'm not sure if I've done it right but it looks OK to me - Andy Preston 8-November-1999
    V1 := Abs (Major);
    V2 := Abs (Minor);
    If Major < 20 Then Result := cVenti [V1] Else Result := VerbBilions (V1);
    If V2 > 0 Then Result := Result + ' virgola ' + VerbBilions (V2);
    If (Major < 0) Or (Minor < 0) Then Result := 'Meno ' + Result;
  End;
End;

End.