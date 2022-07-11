Unit NumW_Esp;

{ Andy Preston, Apollo Developments, http://www.apollod.omnia.co.uk/aa/

  Numbers in words - Version 4.6

  Copyright (C) 1999 Andy Preston
  Copyright (C) 1999 Carlos García Trujillo

  Object-Pascal Units to express a number as words in various languages

  This unit is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

  This unit an modified version of NLetra By Carlos García Trujillo
  mailto:cgar1136@geocities.com http://www.geocities.com/ResearchTriangle/Node/2174/ }

Interface

Uses
  NumW;

Type
  TNumWordsEspanol = Class (TNumWordsLangDriver)
  Private
    Function convierte (s : LongInt) : String;
  Public
    Constructor Create; Reintroduce;

    Function NumberInWords (TheNumber : Integer; Gender : TGender) : String; Override;
    Function IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String; Override;
    Function HasGender (Gender : TGender) : Boolean; Override;
  End;

Implementation

Uses
  SysUtils;

constructor TNumWordsEspanol.Create;
begin
  Inherited
end;

Function TNumWordsEspanol.HasGender (Gender : TGender) : Boolean;
Begin
  Result := (Gender <> genNeuter);
End;

Function TNumWordsEspanol.convierte (s : LongInt) : String;
Begin
  Case s Of
    1 : convierte := 'uno';
    2 : convierte := 'dos';
    3 : convierte := 'tres';
    4 : convierte := 'cuatro';
    5 : convierte := 'cinco';
    6 : convierte := 'seis';
    7 : convierte := 'siete';
    8 : convierte := 'ocho';
    9 : convierte := 'nueve';
    10 : convierte := 'diez';
    11 : convierte := 'once';
    12 : convierte := 'doce';
    13 : convierte := 'trece';
    14 : convierte := 'catorce';
    15 : convierte := 'quince';
    20 : convierte := 'veinte';
    30 : convierte := 'treinta';
    40 : convierte := 'cuarenta';
    50 : Convierte := 'cincuenta';
    60 : convierte := 'sesenta';
    70 : convierte := 'setenta';
    80 : convierte := 'ochenta';
    90 : convierte := 'noventa';
    100 : convierte := 'cien';
    500 : convierte := 'quinientos';
    700 : convierte := 'setecientos';
    900 : convierte := 'novecientos';
  End;
End;

Function TNumWordsEspanol.NumberInWords (TheNumber : Integer; Gender : TGender) : String;
// Note the new parameter, Gender (genMale, genFemale, or genNeuter)
Var
  N, {erro,} unidades, Dec, cent, {salva,} miles, decmil, centmil, mill, aux : LongInt;
  cad, cad2 : String;
Begin
  If TheNumber < 0 Then Raise EConvertError.Create ('NumWords - Debe Ser un Número Positivo!!');
  If TheNumber > 999999999 Then Raise EConvertError.Create ('NumWords - Lo siento, Esto es solo un entero ...');
  // if n>999999999999 then showmessage('Debe ser un número menor a un billón')
  // else
  N := TheNumber;
  Begin
    // If we comment out the use of N that doesn't get used down there we get Hint: Value assigned to 'salva' never used
    // salva:=N;
    aux := N Div 1000000;
    mill := aux;
    n := n Mod 1000000;
    aux := n Div 100000;
    centmil := aux;
    n := n Mod 100000;
    aux := n Div 10000;
    decmil := aux;
    n := n Mod 10000;
    aux := n Div 1000;
    miles := aux;
    n := n Mod 1000;
    aux := n Div 100;
    cent := aux;
    n := n Mod 100;
    aux := n Div 10;
    Dec := aux;
    n := n Mod 10;
    aux := n;
    unidades := aux;
    cad := '';
    If mill > 0 Then
      If mill = 1 Then cad := 'un millon ' Else Begin
        n := mill;
        cad := NumberInWords (N, Gender) + ' millones ';
      End;
    // n:=salva; Hint: Value assigned to 'N' never used, Andy Preston
    {empieza la conversión de las centenas de miles}
    cad2 := '';
    Case centmil Of
      1 : If (miles = 0) And (decmil = 0) Then cad2 := convierte (100) Else cad2 := 'ciento ';
      2 : cad2 := convierte (2) + 'cientos ';
      3 : cad2 := convierte (3) + 'cientos ';
      4 : cad2 := convierte (4) + 'cientos ';
      5 : cad2 := convierte (500) + ' ';
      6 : cad2 := convierte (6) + 'cientos ';
      7 : cad2 := convierte (700) + ' ';
      8 : cad2 := convierte (8) + 'cientos ';
      9 : cad2 := convierte (900) + ' ';
    End;
    cad := cad + cad2;
    {Termina la conversión de las centenas}


   {empieza la conversión de las decenas y unidades de miles}
    cad2 := '';
    Case decmil Of
      0 : If miles <> 1 Then cad2 := convierte (miles);
      1 : If miles <= 5 Then cad2 := convierte (decmil * 10 + miles)
        Else cad2 := 'dieci' + convierte (miles);
      2 : If miles = 0 Then cad2 := convierte (20)
        Else If miles = 1 Then cad2 := 'veintiun'
        Else cad2 := 'veinti' + convierte (miles);
      3 : If miles = 0 Then cad2 := convierte (30)
        Else If miles = 1 Then cad2 := 'treinta y un'
        Else cad2 := 'treinta y ' + convierte (miles);
      4 : If miles = 0 Then cad2 := convierte (40)
        Else If miles = 1 Then cad2 := 'cuarenta y un'
        Else cad2 := 'cuarenta y ' + convierte (miles);
      5 : If miles = 0 Then cad2 := convierte (50)
        Else If miles = 1 Then cad2 := 'cincuenta y un'
        Else cad2 := 'cincuenta y ' + convierte (miles);
      6 : If miles = 0 Then cad2 := convierte (60)
        Else If miles = 1 Then cad2 := 'sesenta y un'
        Else cad2 := 'sesenta y ' + convierte (miles);
      7 : If miles = 0 Then cad2 := convierte (70)
        Else If miles = 1 Then cad2 := 'setenta y un'
        Else cad2 := 'setenta y ' + convierte (miles);
      8 : If miles = 0 Then cad2 := convierte (80)
        Else If miles = 1 Then cad2 := 'ochenta y un'
        Else cad2 := 'ochenta y ' + convierte (miles);
      9 : If miles = 0 Then cad2 := convierte (90)
        Else If miles = 1 Then cad2 := 'noventa y un'
        Else cad2 := 'noventa y ' + convierte (miles);
    End;
    If (miles > 0) Or (decmil > 0) Or (centmil > 0) Then cad2 := cad2 + ' mil ';
    cad := cad + cad2;
    {termina la conversión de las decenas y unidades de miles}

 {empieza la conversión de las centenas}
    cad2 := '';
    Case cent Of
      1 : If (unidades = 0) And (Dec = 0) Then cad2 := convierte (100) Else cad2 := 'ciento ';
      2 : cad2 := convierte (2) + 'cientos ';
      3 : cad2 := convierte (3) + 'cientos ';
      4 : cad2 := convierte (4) + 'cientos ';
      5 : cad2 := convierte (500) + ' ';
      6 : cad2 := convierte (6) + 'cientos ';
      7 : cad2 := convierte (700) + ' ';
      8 : cad2 := convierte (8) + 'cientos ';
      9 : cad2 := convierte (900) + ' ';
    End;
    cad := cad + cad2;
    {Termina la conversión de las centenas}

   {empieza la conversión de las decenas y unidades}
    cad2 := '';
    Case Dec Of
      0 : cad2 := convierte (unidades);
      1 : If unidades <= 5 Then cad2 := convierte (Dec * 10 + unidades)
        Else cad2 := 'dieci' + convierte (unidades);
      2 : If unidades = 0 Then cad2 := convierte (20)
        Else cad2 := 'veinti' + convierte (unidades);
      3 : If unidades = 0 Then cad2 := convierte (30)
        Else cad2 := 'treinta y ' + convierte (unidades);
      4 : If unidades = 0 Then cad2 := convierte (40)
        Else cad2 := 'cuarenta y ' + convierte (unidades);
      5 : If unidades = 0 Then cad2 := convierte (50)
        Else cad2 := 'cincuenta y ' + convierte (unidades);
      6 : If unidades = 0 Then cad2 := convierte (60)
        Else cad2 := 'sesenta y ' + convierte (unidades);
      7 : If unidades = 0 Then cad2 := convierte (70)
        Else cad2 := 'setenta y ' + convierte (unidades);
      8 : If unidades = 0 Then cad2 := convierte (80)
        Else cad2 := 'ochenta y ' + convierte (unidades);
      9 : If unidades = 0 Then cad2 := convierte (90)
        Else cad2 := 'noventa y ' + convierte (unidades);
    End;
    cad := cad + cad2;
    {termina la conversión de las decenas y unidades}
    If cad <> '' Then Result := cad Else Result := 'Cero';
  End;
End;

Function TNumWordsEspanol.IMoneyInWords (Major, Minor : Integer; EuroMode : Boolean) : String;
Begin
  Result := 'Currency support not yet included';
End;

End.