object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Polygone Raster'
  ClientHeight = 729
  ClientWidth = 1008
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    1008
    729)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 8
    Top = 47
    Width = 992
    Height = 674
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseMove = Image1MouseMove
    ExplicitWidth = 781
    ExplicitHeight = 425
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1008
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 14
      Width = 31
      Height = 13
      Caption = 'Label1'
    end
    object Label2: TLabel
      Left = 152
      Top = 14
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object CheckBox1: TCheckBox
      Left = 632
      Top = 13
      Width = 97
      Height = 17
      Caption = 'DirectMode'
      TabOrder = 0
    end
  end
  object TimerSec: TTimer
    OnTimer = TimerSecTimer
    Left = 392
    Top = 248
  end
end
