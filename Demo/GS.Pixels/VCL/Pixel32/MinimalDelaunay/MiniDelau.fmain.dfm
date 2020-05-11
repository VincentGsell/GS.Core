object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MiniDelaunay'
  ClientHeight = 644
  ClientWidth = 1042
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnMouseMove = FormMouseMove
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 185
    Height = 105
    Caption = 'Panel1'
    TabOrder = 0
    object SpeedButton1: TSpeedButton
      Left = 152
      Top = 8
      Width = 23
      Height = 22
      Caption = '+'
      OnClick = SpeedButton1Click
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Display triangles'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = CheckBox1Click
    end
  end
end
