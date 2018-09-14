object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 693
  ClientWidth = 1045
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    1045
    693)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 143
    Top = 8
    Width = 894
    Height = 533
    Anchors = [akLeft, akTop, akRight, akBottom]
    Center = True
    Proportional = True
    Stretch = True
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 57
    Caption = 'No thread'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 71
    Width = 129
    Height = 57
    Caption = 'GS.Task'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Memo1: TMemo
    Left = 143
    Top = 547
    Width = 894
    Height = 138
    Anchors = [akLeft, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
  object Button3: TButton
    Left = 8
    Top = 197
    Width = 129
    Height = 59
    Caption = 'ITask'
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 134
    Width = 129
    Height = 57
    Caption = 'Classic TThread'
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 335
    Width = 129
    Height = 57
    Caption = 'GS.Bus.Services'
    TabOrder = 5
    OnClick = Button5Click
  end
  object RadioGroup1: TRadioGroup
    Left = 8
    Top = 262
    Width = 129
    Height = 67
    Caption = 'GS.Bus.Service'
    ItemIndex = 0
    Items.Strings = (
      'Classic Thread'
      'Service Thread')
    TabOrder = 6
  end
end
