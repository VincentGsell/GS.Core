object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Raster 3D'
  ClientHeight = 505
  ClientWidth = 814
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
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 814
    Height = 505
    Align = alClient
    AutoSize = True
    ExplicitLeft = 48
    ExplicitTop = 32
    ExplicitWidth = 833
    ExplicitHeight = 529
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 40
    Top = 16
  end
end
