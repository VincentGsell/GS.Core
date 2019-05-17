object Form15: TForm15
  Left = 0
  Top = 0
  Caption = 'Form15'
  ClientHeight = 667
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    823
    667)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 8
    Width = 62
    Height = 13
    Caption = 'Source file...'
  end
  object Label2: TLabel
    Left = 48
    Top = 56
    Width = 73
    Height = 13
    Caption = 'Destination File'
  end
  object Label3: TLabel
    Left = 13
    Top = 403
    Width = 29
    Height = 13
    Caption = 'Log...'
  end
  object Shape1: TShape
    Left = 207
    Top = 242
    Width = 1
    Height = 35
  end
  object Shape2: TShape
    Left = 256
    Top = 290
    Width = 60
    Height = 1
  end
  object Label4: TLabel
    Left = 48
    Top = 104
    Width = 49
    Height = 13
    Caption = 'Glacier file'
  end
  object Memo1: TMemo
    Left = 48
    Top = 400
    Width = 737
    Height = 259
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Edit1: TEdit
    Left = 48
    Top = 27
    Width = 737
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = 'Edit1'
  end
  object Edit2: TEdit
    Left = 48
    Top = 72
    Width = 737
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 48
    Top = 171
    Width = 97
    Height = 38
    Anchors = [akTop]
    Caption = 'Copy...'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 663
    Top = 171
    Width = 90
    Height = 38
    Caption = 'Encrypt'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 151
    Top = 171
    Width = 98
    Height = 38
    Caption = 'Copy with notif.'
    TabOrder = 5
    OnClick = Button3Click
  end
  object ProgressBar1: TProgressBar
    Left = 48
    Top = 377
    Width = 737
    Height = 17
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
  end
  object Button4: TButton
    Left = 328
    Top = 171
    Width = 203
    Height = 38
    Caption = 'Glacify'
    TabOrder = 7
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 328
    Top = 247
    Width = 203
    Height = 26
    Caption = 'Flush file'
    Enabled = False
    TabOrder = 8
    OnClick = Button5Click
  end
  object CheckBox1: TCheckBox
    Left = 328
    Top = 212
    Width = 97
    Height = 17
    Caption = 'Delete if exists'
    TabOrder = 9
  end
  object Button6: TButton
    Left = 46
    Top = 215
    Width = 203
    Height = 25
    Caption = 'File Glacify simulation info struct.'
    TabOrder = 10
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 328
    Top = 279
    Width = 203
    Height = 25
    Caption = 'File Glacified simulation info struct.'
    TabOrder = 11
    OnClick = Button7Click
  end
  object Button8: TButton
    Left = 174
    Top = 279
    Width = 75
    Height = 25
    Caption = 'Compare'
    TabOrder = 12
    OnClick = Button8Click
  end
  object Edit3: TEdit
    Left = 48
    Top = 120
    Width = 737
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 13
    Text = 'Edit1'
  end
  object Button9: TButton
    Left = 328
    Top = 310
    Width = 203
    Height = 25
    Caption = 'Glacier Files list'
    TabOrder = 14
    OnClick = Button9Click
  end
  object Button10: TButton
    Left = 2
    Top = 422
    Width = 41
    Height = 50
    Caption = 'cls'
    TabOrder = 15
    OnClick = Button10Click
  end
  object Button11: TButton
    Left = 537
    Top = 171
    Width = 88
    Height = 38
    Caption = 'Glacify Files'
    TabOrder = 16
    OnClick = Button11Click
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 616
    Top = 224
  end
end
