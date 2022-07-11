object Form7: TForm7
  Left = 0
  Top = 0
  Caption = 'Form7'
  ClientHeight = 597
  ClientWidth = 1040
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  DesignSize = (
    1040
    597)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 1040
    Height = 597
    Align = alClient
    AutoSize = True
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    ExplicitLeft = 16
    ExplicitWidth = 890
    ExplicitHeight = 566
  end
  object Panel1: TPanel
    Left = 825
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
      OnClick = cbClick
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
      OnClick = cbClick
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
  object Panel2: TPanel
    Left = 760
    Top = 247
    Width = 272
    Height = 322
    Anchors = [akTop, akRight]
    Caption = 'Panel2'
    TabOrder = 1
    DesignSize = (
      272
      322)
    object TreeView1: TTreeView
      Left = 6
      Top = 5
      Width = 259
      Height = 311
      Anchors = [akLeft, akTop, akRight, akBottom]
      DoubleBuffered = True
      Indent = 19
      ParentDoubleBuffered = False
      TabOrder = 0
    end
  end
  object TimerFPS: TTimer
    Enabled = False
    OnTimer = TimerFPSTimer
    Left = 40
    Top = 24
  end
  object ApplicationEvents1: TApplicationEvents
    OnIdle = ApplicationEvents1Idle
    Left = 40
    Top = 72
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 512
    Top = 304
  end
end
