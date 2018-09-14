object Form1: TForm1
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 334
  ClientWidth = 505
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
  object Memo1: TMemo
    Left = 215
    Top = 48
    Width = 281
    Height = 225
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 48
    Width = 193
    Height = 33
    Caption = 'Get'
    TabOrder = 1
    OnClick = Button1Click
  end
  object LabeledEdit1: TLabeledEdit
    Left = 8
    Top = 21
    Width = 193
    Height = 21
    EditLabel.Width = 18
    EditLabel.Height = 13
    EditLabel.Caption = 'Key'
    TabOrder = 2
    Text = 'TestKey'
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 87
    Width = 201
    Height = 186
    TabOrder = 3
    DesignSize = (
      201
      186)
    object Memo2: TMemo
      Left = 3
      Top = 40
      Width = 94
      Height = 89
      Lines.Strings = (
        'Exemple of text '
        '!'
        'Try me !')
      TabOrder = 0
    end
    object RadioButton1: TRadioButton
      Left = 3
      Top = 18
      Width = 113
      Height = 17
      Caption = 'Set as text'
      TabOrder = 1
      OnClick = RadioButton2Click
    end
    object RadioButton2: TRadioButton
      Left = 103
      Top = 18
      Width = 90
      Height = 17
      Caption = 'Set as stream'
      TabOrder = 2
      OnClick = RadioButton2Click
    end
    object Button3: TButton
      Left = 103
      Top = 40
      Width = 90
      Height = 89
      Caption = 'Load and Set'
      Enabled = False
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button2: TButton
      Left = 3
      Top = 135
      Width = 187
      Height = 34
      Anchors = [akLeft, akBottom]
      Caption = 'Set'
      TabOrder = 4
      OnClick = Button2Click
    end
  end
  object Button4: TButton
    Left = 364
    Top = 279
    Width = 130
    Height = 25
    Caption = 'Full CSV Snapshot (!)'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 419
    Top = 17
    Width = 75
    Height = 25
    Caption = 'Stress'
    TabOrder = 5
    OnClick = Button5Click
  end
  object LabeledEdit2: TLabeledEdit
    Left = 326
    Top = 17
    Width = 88
    Height = 21
    EditLabel.Width = 51
    EditLabel.Height = 13
    EditLabel.Caption = 'Stress Key'
    TabOrder = 6
    Text = 'StressTest'
  end
  object Button6: TButton
    Left = 215
    Top = 279
    Width = 143
    Height = 25
    Caption = 'First 1000 rec.'
    TabOrder = 7
    OnClick = Button6Click
  end
  object Button7: TButton
    Left = 215
    Top = 303
    Width = 143
    Height = 25
    Caption = 'Last 1000 rec.'
    TabOrder = 8
    OnClick = Button7Click
  end
  object OpenDialog1: TOpenDialog
    Left = 168
    Top = 184
  end
end
