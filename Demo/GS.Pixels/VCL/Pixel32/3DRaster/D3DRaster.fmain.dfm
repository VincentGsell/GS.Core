object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 527
  ClientWidth = 825
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  OnPaint = FormPaint
  OnResize = FormResize
  DesignSize = (
    825
    527)
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 688
    Top = 8
    Width = 129
    Height = 89
    Anchors = [akTop, akRight]
    TabOrder = 0
    object Label1: TLabel
      Left = 0
      Top = 56
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
      TabOrder = 0
      OnClick = cbWireFrameClick
    end
  end
  object TimerDraw: TTimer
    Interval = 1
    OnTimer = TimerDrawTimer
    Left = 32
    Top = 8
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 408
    Top = 272
  end
end
