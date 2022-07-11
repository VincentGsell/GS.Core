Unit QRCusDBT;

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
  Classes, Controls, Forms, QuickRpt, QrCtrls, DB;

// The plan was to inherit from Somewhere in-between TQRCustomLabel and TQRDBText
// But no such component exists, so I've had to clone most of TQRDBText from the QR Sources.

Type
  TQRCustomDBText = Class (TQRCustomLabel)
  Private
    FieldOK : Boolean;
    DataSourceName : String [30];
    FDataSet : TDataSet;
    FDataField : String;
    FieldNo : Integer;
    Procedure SetDataSet (Value : TDataSet);
    Procedure SetDataField (Value : String);
  Protected
    Field : TField;
    Procedure Loaded; Override;
    Procedure ReadValues (Reader : TReader); Virtual;
    Procedure WriteValues (Writer : TWriter); Virtual;
    Procedure DefineProperties (Filer : TFiler); Override;
    Procedure Notification (AComponent : TComponent; Operation : TOperation); Override;
    Procedure Prepare; Override;
    Procedure Unprepare; Override;
    Procedure PrePrint;
  Public
    Constructor Create (AOwner : TComponent); Override;
    Property DataSet : TDataSet Read FDataSet Write SetDataSet;
    Property DataField : String Read FDataField Write SetDataField;
  End;


Implementation

Constructor TQRCustomDBText.Create (AOwner : TComponent);
Begin
  Inherited Create (AOwner);
  DataSourceName := '';
End;

Procedure TQRCustomDBText.SetDataSet (Value : TDataSet);
Begin
  FDataSet := Value;
  If Value <> Nil Then Value.FreeNotification (Self);
End;

Procedure TQRCustomDBText.SetDataField (Value : String);
Begin
  FDataField := Value;
  Caption := Value;
End;

Procedure TQRCustomDBText.Loaded;
Var
  aComponent : TComponent;
Begin
  Inherited Loaded;
  If DataSourceName <> '' Then Begin
    aComponent := Owner.FindComponent (DataSourceName);
    If (aComponent <> Nil) And (aComponent Is TDataSource) Then DataSet := TDataSource (aComponent).DataSet;
  End;
End;

Procedure TQRCustomDBText.DefineProperties (Filer : TFiler);
Begin
  Filer.DefineProperty ('DataSource', ReadValues, WriteValues, False);
  Inherited DefineProperties (Filer);
End;

Procedure TQRCustomDBText.ReadValues (Reader : TReader);
Begin
  DataSourceName := Reader.ReadIdent;
End;

Procedure TQRCustomDBtext.WriteValues (Writer : TWriter);
Begin
End;

Procedure TQRCustomDBText.Notification (AComponent : TComponent; Operation : TOperation);
Begin
  Inherited Notification (AComponent, Operation);
  If Operation = opRemove Then If AComponent = FDataSet Then FDataSet := Nil;
End;

Procedure TQRCustomDBText.Prepare;
Begin
  Inherited Prepare;
  If assigned (FDataSet) Then Begin
    Field := FDataSet.FindField (FDataField);
    If Field <> Nil Then Begin
      FieldNo := Field.Index;
      FieldOK := True;
      If (Field Is TMemoField) Or (Field Is TBlobField) Then Caption := '';
    End;
  End
  Else Begin
    Field := Nil;
    FieldOK := False;
  End;
End;

Procedure TQRCustomDBText.Unprepare;
Begin
  Field := Nil;
  Inherited Unprepare;
  If DataField <> '' Then
    SetDataField (DataField) { Reset component caption }
  Else
    SetDataField (Name);
End;

Procedure TQRCustomDBText.PrePrint;
Begin
  If Not FieldOK Then
    Field := Nil
  Else If FDataSet.DefaultFields Then Field := FDataSet.Fields [FieldNo];
End;


End.