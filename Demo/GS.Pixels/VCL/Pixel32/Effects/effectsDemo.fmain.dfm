object Form6: TForm6
  Left = 0
  Top = 0
  Caption = 'Effects'
  ClientHeight = 609
  ClientWidth = 1035
  Color = clBtnFace
  DoubleBuffered = True
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
  object Image1: TImage
    Left = 185
    Top = 54
    Width = 850
    Height = 555
    Align = alClient
    Proportional = True
    Stretch = True
    ExplicitLeft = 249
    ExplicitTop = 92
  end
  object Panel1: TPanel
    Left = 0
    Top = 54
    Width = 185
    Height = 555
    Align = alLeft
    Caption = 'Panel1'
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 25
      Height = 13
      Caption = 'Level'
    end
    object ListBox1: TListBox
      Left = 16
      Top = 89
      Width = 153
      Height = 183
      ItemHeight = 13
      Items.Strings = (
        'original'
        'clear'
        'gradient')
      TabOrder = 0
      OnClick = ListBox1Click
    end
    object TrackBar1: TTrackBar
      Left = 16
      Top = 38
      Width = 150
      Height = 45
      Max = 100
      TabOrder = 1
      OnChange = ListBox1Click
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 1035
    Height = 54
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 1
  end
end
