Unit QRNumW;

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
  Classes, Controls, Forms, DB, DsgnIntf;

Type
  TADQRFieldProperty = Class (TStringProperty)
  Public
    Function GetAttributes : TPropertyAttributes; Override;
    Procedure GetValueList (List : TStrings);
    Procedure GetValues (Proc : TGetStrProc); Override;
    Function GetDataSetPropName : String; Virtual;
  End;

Procedure Register;

Implementation

Uses
  QrDbCash, QRCusDBT, TypInfo;

/////////////////////////////////////////////////////////////////////////////////////////

// Look I can't even inherit the property editor I've got to clone it.

Function TADQRFieldProperty.GetAttributes : TPropertyAttributes;
Begin
  Result := [paValueList, paSortList, paMultiSelect];
End;

Procedure TADQRFieldProperty.GetValues (Proc : TGetStrProc);
Var
  I : Integer;
  Values : TStringList;
Begin
  Values := TStringList.Create;
  Try
    GetValueList (Values);
    For I := 0 To Values.Count - 1 Do Proc (Values [I]);
  Finally
    Values.Free;
  End;
End;

Function TADQRFieldProperty.GetDataSetPropName : String;
Begin
  Result := 'DataSet';
End;

Procedure TADQRFieldProperty.GetValueList (List : TStrings);
Var
  Instance : TComponent;
  PropInfo : PPropInfo;
  DataSet : TDataSet;
Begin
  Instance := TComponent (GetComponent (0));
  PropInfo := TypInfo.GetPropInfo (Instance.ClassInfo, GetDataSetPropName);
  If (PropInfo <> Nil) And (PropInfo^.PropType^.Kind = tkClass) Then Begin
    DataSet := TObject (GetOrdProp (Instance, PropInfo)) As TDataSet;
    If (DataSet <> Nil) Then DataSet.GetFieldNames (List);
  End;
End;

Procedure Register;
Begin
  RegisterComponents ('Samples', [TQRDBMoneyWords]);
  RegisterPropertyEditor (TypeInfo (String), TQRDBMoneyWords, 'DataField', TADQRFieldProperty);
End;

End.