object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Assets usage'
  ClientHeight = 596
  ClientWidth = 994
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
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 994
    Height = 596
    Align = alClient
    OnMouseMove = Image1MouseMove
    ExplicitLeft = 432
    ExplicitTop = 224
    ExplicitWidth = 105
    ExplicitHeight = 105
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 488
    Top = 304
  end
end
