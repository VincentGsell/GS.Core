Unit QRDBCash;

{ Andy Preston, Apollo Developments, http://www.apollod.omnia.co.uk/aa/

  Numbers in words - Version 4.6

  Copyright (C) 1999 - 2001 Andy Preston

  Quick-Reports components to express a number as words in various languages

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later
  version.

  This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the
  Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. }

Interface

Uses
  Classes, Controls, Forms, QRCusDBT, NumW;

Type
  TQRDBMoneyWords = Class (TQRCustomDBText)
  Public
    Constructor Create (AOwner : TComponent); Override;
  Private
    fEuroMode : Boolean;
    fGender : TGender;
  Protected
    Procedure Print (OfsX, OfsY : Integer); Override;
  Published
    Property EuroMode : Boolean Read fEuroMode Write fEuroMode;
    Property Gender : TGender Read fGender Write fGender;
    Property DataSet;
    Property DataField;
    Property Alignment;
    Property AlignToBand;
    Property AutoSize;
    Property AutoStretch;
    Property Color;
    Property Font;
    Property ParentFont;
    Property Transparent;
    Property WordWrap;
    Property OnPrint;
  End;


Implementation

Uses
  SysUtils, DB, NumWords;

Constructor TQRDBMoneyWords.Create (AOwner : TComponent);
Begin
  Inherited Create (AOwner);
  EuroMode := False;
  Gender := genNeuter;
End;

Procedure TQRDBMoneyWords.Print (OfsX, OfsY : Integer);
Begin
  PrePrint;
  If Not assigned (Field) Then
    Caption := ''
  Else Begin
    Try
      If (Field Is TMemoField) Or (Field Is TBlobField) Then
        Lines.Text := TMemoField (Field).AsString
      Else If Field Is TStringField Then Begin
        If Not (Field Is TBlobField) Then Caption := Field.DisplayText Else Caption := Field.AsString;
      End
      Else If (Field Is TIntegerField) Or (Field Is TSmallIntField) Or (Field Is TWordField) Then
        Caption := InWords.Number (TIntegerField (Field).Value, Gender)
      Else If (Field Is TFloatField) Or (Field Is TCurrencyField) Or (Field Is TBCDField) Then
        Caption := InWords.EMoney (TFloatField (Field).Value, EuroMode)
      Else If (Field Is TDateTimeField) Or (Field Is TDateField) Or (Field Is TTimeField) Then
        Caption := DateTimeToStr (TDateTimeField (Field).Value);
    Except
      Caption := '';
    End;
  End;
  Inherited Print (OfsX, OfsY);
End;


End.