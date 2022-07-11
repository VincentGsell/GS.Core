object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Form2'
  ClientHeight = 583
  ClientWidth = 777
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object TimerUpdate: TTimer
    Interval = 1
    OnTimer = TimerUpdateTimer
    Left = 32
    Top = 48
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 32
    Top = 104
  end
end
