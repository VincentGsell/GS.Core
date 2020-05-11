object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'Form7'
  ClientHeight = 566
  ClientWidth = 890
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    890
    566)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 890
    Height = 566
    Align = alClient
    AutoSize = True
    ExplicitLeft = 16
  end
  object Panel1: TPanel
    Left = 675
    Top = 8
    Width = 207
    Height = 233
    Anchors = [akTop, akRight]
    DoubleBuffered = False
    ParentDoubleBuffered = False
    TabOrder = 0
    object cbWireFrame: TCheckBox
      Left = 16
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Wireframe'
      TabOrder = 0
    end
    object cbRasterFrame: TCheckBox
      Left = 104
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Rasterframe'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 31
      Width = 193
      Height = 50
      Caption = ' Projection '
      Columns = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -8
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 0
      Items.Strings = (
        'Perspective'
        'Orthographic')
      ParentFont = False
      TabOrder = 2
    end
    object RadioGroup2: TRadioGroup
      Left = 6
      Top = 80
      Width = 193
      Height = 82
      Caption = 'Raster Mode'
      Columns = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -8
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = 4
      Items.Strings = (
        'DirectMode'
        'basic'
        'BackBuffer'
        'ZBuffer'
        'Textured')
      ParentFont = False
      TabOrder = 3
    end
    object GroupBox1: TGroupBox
      Left = 8
      Top = 168
      Width = 193
      Height = 57
      Caption = 'Global texture'
      TabOrder = 4
      object ComboBox1: TComboBox
        Left = 8
        Top = 20
        Width = 177
        Height = 21
        TabOrder = 0
        Text = 'ComboBox1'
      end
    end
  end
  object TimerFPS: TTimer
    Left = 56
    Top = 16
  end
end
