object Form3: TForm3
  Left = 0
  Top = 0
  Anchors = [akTop, akRight]
  Caption = 'Form3'
  ClientHeight = 629
  ClientWidth = 957
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
    957
    629)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 112
    Top = 95
    Width = 837
    Height = 514
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    OnMouseUp = Image1MouseUp
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 140
    Height = 13
    Caption = '(1) Draw a shape with mouse'
  end
  object Label2: TLabel
    Left = 8
    Top = 24
    Width = 323
    Height = 13
    Caption = 
      '(2) Click "convert" button to convert hand made shape to an asse' +
      't.'
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 387
    Height = 13
    Caption = 
      '(3)   Choose  an asset in list, and  push "create" to build a sh' +
      'ape from this assets'
  end
  object Label4: TLabel
    Left = 112
    Top = 76
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label5: TLabel
    Left = 364
    Top = 76
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label6: TLabel
    Left = 659
    Top = 76
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Label7: TLabel
    Left = 112
    Top = 611
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Label4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnConvert: TButton
    Left = 3
    Top = 64
    Width = 103
    Height = 25
    Caption = 'Convert'
    Enabled = False
    TabOrder = 0
    OnClick = btnConvertClick
  end
  object ListBox1: TListBox
    Left = 3
    Top = 95
    Width = 103
    Height = 178
    ItemHeight = 13
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object btnCreate: TButton
    Left = 3
    Top = 279
    Width = 103
    Height = 25
    Caption = 'Create'
    Enabled = False
    TabOrder = 2
    OnClick = btnCreateClick
  end
  object ListBox2: TListBox
    Left = 3
    Top = 310
    Width = 103
    Height = 203
    ItemHeight = 13
    TabOrder = 3
    OnClick = ListBox1Click
  end
  object chkDrawMode: TCheckBox
    Left = 659
    Top = 8
    Width = 290
    Height = 17
    Caption = 'Draw mode (Uncheck to move shapes)'
    Checked = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    State = cbChecked
    TabOrder = 4
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 448
    Top = 16
  end
end
