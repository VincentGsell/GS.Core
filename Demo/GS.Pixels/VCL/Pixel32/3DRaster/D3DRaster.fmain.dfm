object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 520
  ClientWidth = 878
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
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  DesignSize = (
    878
    520)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 878
    Height = 520
    Align = alClient
    AutoSize = True
    ExplicitLeft = 48
    ExplicitTop = 32
    ExplicitWidth = 833
    ExplicitHeight = 529
  end
  object Panel1: TPanel
    Left = 663
    Top = 8
    Width = 207
    Height = 201
    Anchors = [akTop, akRight]
    DoubleBuffered = False
    ParentDoubleBuffered = False
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 128
      Width = 123
      Height = 13
      Caption = 'here : pixel shader choice'
    end
    object cbWireFrame: TCheckBox
      Left = 16
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Wireframe'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbWireFrameClick
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
      OnClick = cbWireFrameClick
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
        'Perspective Iso'
        'Orthographic')
      ParentFont = False
      TabOrder = 2
      OnClick = RadioGroup1Click
    end
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 8
    Top = 16
  end
end
