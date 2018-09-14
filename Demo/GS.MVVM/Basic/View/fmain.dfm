object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 422
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 272
    Top = 133
    Width = 103
    Height = 13
    Caption = 'Response from Model'
  end
  object LblReflect: TLabel
    Left = 272
    Top = 96
    Width = 104
    Height = 19
    Caption = '<Response>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 120
    Top = 80
    Width = 22
    Height = 13
    Caption = 'View'
  end
  object LblReflectFromModel: TLabel
    Left = 272
    Top = 145
    Width = 104
    Height = 19
    Caption = '<Response>'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label4: TLabel
    Left = 273
    Top = 80
    Width = 75
    Height = 13
    Caption = 'Diret link to edit'
  end
  object LabelTime: TLabel
    Left = 8
    Top = 60
    Width = 45
    Height = 13
    Caption = 'Labeltime'
  end
  object Edit1: TEdit
    Left = 120
    Top = 96
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 58
    Align = alTop
    Caption = 
      'Basic demo : "View edit is linked to a model property, which is ' +
      'reflected by label.'
    Color = clMoneyGreen
    ParentBackground = False
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 216
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 247
    Width = 498
    Height = 166
    Lines.Strings = (
      'Memo1')
    TabOrder = 3
  end
end
