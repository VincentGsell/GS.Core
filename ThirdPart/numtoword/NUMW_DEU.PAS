Unit NumW_Deu;

{ Andy Preston, Apollo Developments, http://www.apollod.omnia.co.uk/aa/

  Numbers in words - Version 4.6

  Copyright (C) 1999, 2000 Andy Preston
  Copyright (C) 1998, 1999 Andreas Schmidt and friends

  Object-Pascal Units to express a number as words in various languages

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  This unit an 'objectified' version of zahl2txt By Andreas Schmidt and friends


  Zahl2Txt original header continues:

  Converts a Number to Plain Text (German) Konvertiert eine Zahl in deutschen Klartext

  z.B. zum Beschriften von Zahlkarten oder Schecks

  Version: 1.0  Copyright (C) 1998-99 Andreas Schmidt and friends

  Freeware for use with Delphi 2/3/4

  bug-reports, enhancements:  mailto:shmia@bizerba.de or a_j_schmidt@rocketmail.com
  get latest version from:    http://members.tripod.de/AJSchmidt/index.html

  Ich bin nicht dafür verantwortlich wenn auf einer Zahlkarte 'sechsundreissigmillionen'
  steht obwohl nur 3,60 DM drauf sein sollten.  :-)

  Falls Ihr meine Funktion verwendet, denkt auch an mich und schickt mir auch mal ein Stückchen Source-Code.

  To Do: falls jemand Lust hat, bitte auf Delphi 1 umbauen und mir den Sourcecode schicken.
}

Interface

Uses
  Numw;

Type
  TNumWordsDeutch = Class (TNumWordsLangDriver)
  Private
    Function ConvNumber (i : Integer) : String;
  Public
    Constructor Create; Reintroduce;

    Function NumberInWords (TheNumber : Integer; Gender : TGender) : String; Override;
    Function IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String; Override;
    Function HasGender (Gender : TGender) : Boolean; Override;
  End;

Implementation

Const
  numNamesA : Array [1..17] Of String = (
     ('ein'), ('zwei'), ('drei'), ('vier'), ('fünf'), ('sechs'), ('sieben'), ('acht'), ('neun'),
     ('zehn'), ('elf'), ('zwölf'), ('dreizehn'), ('vierzehn'), ('fünfzehn'), ('sechzehn'), ('siebzehn')
    );

  numNamesB : Array [1..9] Of String = (
     ('zehn'), ('zwanzig'), ('dreissig'), ('vierzig'), ('fünfzig'), ('sechzig'), ('siebzig'), ('achtzig'), ('neunzig')
    );

  hundert : String = 'hundert';
  tausend = 'tausend';
  und = 'und';
  million = 'million';

constructor TNumWordsDeutch.Create;
begin

end;

Function TNumWordsDeutch.HasGender (Gender : TGender) : Boolean;
Begin
  Result := True;
End;

Function TNumWordsDeutch.ConvNumber (i : Integer) : String;
Var
  j : Integer;
Begin
  If i = 0 Then Result := ''
  Else If i < 17 Then Result := numNamesA [i]
  Else If i < 20 Then Result := numNamesA [i Mod 10] + numNamesB [1]
  Else If i < 100 Then Begin
    If (i Mod 10) = 0 Then Result := numNamesB [i Div 10] Else Result := numNamesA [i Mod 10] + und + numNamesB [i Div 10];
  End
  Else If i < 1000 Then Result := ConvNumber (i Div 100) + hundert + ConvNumber (i Mod 100)
  Else If i < 1000000 Then Result := ConvNumber (i Div 1000) + tausend + ConvNumber (i Mod 1000)
  Else Begin
    j := i Div 1000000;
    If j = 1 Then Result := 'eine' Else Result := ConvNumber (j);
    Result := Result + million;
    If j > 1 Then Result := Result + 'en';
    Result := Result + ConvNumber (i Mod 1000000);
  End;
End;

{ This was once Andreas' IntegerToKlartext Function }

Function TNumWordsDeutch.NumberInWords (TheNumber : Integer; Gender : TGender) : String;
// Note the new parameter, Gender (genMale, genFemale, or genNeuter)
Begin
  If TheNumber < 0 Then Result := 'minus ' + NumberInWords (-TheNumber, Gender)
  Else If TheNumber = 0 Then Result := 'null'
  Else If TheNumber = 1 Then Result := 'eins'
  Else Result := ConvNumber (TheNumber);
End;

Function TNumWordsDeutch.IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String;
Var
  BigS, SmallS, BigP, SmallP : String;
Begin
  If EuroMode Then Begin
    BigS := 'Euro';
    BigP := 'Euros';
    SmallS := 'Cent';
    SmallP := 'Cents';
  End
  Else Begin
    BigS := BigSing;
    BigP := BigPlural;
    SmallS := SmallSing;
    SmallP := SmallPlural;
  End;
  // 06.09.1999 Andreas Schmidt  there is a irregularity in the german language :
  // you count      eins, zwei, drei, ...
  // but you say    ein  Pfennig
  // and            eine Mark
  // note the difference in the Value 1
  If Major = 1 Then Result := 'eine ' + BigS
  Else If Major = -1 Then Result := 'minus eine ' + BigS // negative Value
  Else If Major <> 0 Then Result := NumberInWords (Major, CurrencyGender) + ' ' + BigP
  Else If Minor = 0 Then Result := 'null ' + BigS // (Major = 0) and (Minor = 0)
  Else Result := '';
  If Minor > 0 Then Begin
    If Major = 0 Then Begin
      If Minor = 1 Then Result := 'ein ' + SmallS
      Else Result := NumberInWords (Minor, CurrencyGender) + ' ' + SmallP;
    End
    Else Result := Result + ' ' + NumberInWords (Minor, CurrencyGender);
  End;
End;

End.