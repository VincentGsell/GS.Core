object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Form4'
  ClientHeight = 763
  ClientWidth = 1056
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  DesignSize = (
    1056
    763)
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 8
    Width = 1040
    Height = 747
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    ExplicitWidth = 844
    ExplicitHeight = 614
  end
  object RadioGroup1: TRadioGroup
    Left = 24
    Top = 40
    Width = 105
    Height = 105
    Caption = 'RadioGroup1'
    ItemIndex = 0
    Items.Strings = (
      'Square'
      'Tri'
      'Crux'
      'SquareEx')
    TabOrder = 0
    OnClick = CheckBox1Click
  end
  object CheckBox1: TCheckBox
    Left = 24
    Top = 17
    Width = 97
    Height = 17
    Caption = 'Euler curve'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = CheckBox1Click
  end
end
