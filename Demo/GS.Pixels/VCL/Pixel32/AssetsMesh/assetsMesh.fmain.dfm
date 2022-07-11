object Form3: TForm3
  Left = 0
  Top = 0
  Anchors = [akTop, akRight]
  Caption = 'Form3'
  ClientHeight = 708
  ClientWidth = 1139
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
    1139
    708)
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 112
    Top = 95
    Width = 1019
    Height = 593
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnMouseDown = Image1MouseDown
    OnMouseMove = Image1MouseMove
    OnMouseUp = Image1MouseUp
    ExplicitWidth = 837
    ExplicitHeight = 514
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
    Top = 690
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
    ExplicitTop = 611
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
    PopupMenu = PopupMenu1
    TabOrder = 1
    OnClick = ListBox1Click
  end
  object btnCreate: TButton
    Left = 3
    Top = 360
    Width = 103
    Height = 25
    Caption = 'Create'
    Enabled = False
    TabOrder = 2
    OnClick = btnCreateClick
  end
  object ListBox2: TListBox
    Left = 3
    Top = 391
    Width = 103
    Height = 203
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 3
    OnClick = ListBox1Click
  end
  object chkDrawMode: TCheckBox
    Left = 841
    Top = 8
    Width = 290
    Height = 17
    Anchors = [akTop, akRight]
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
  object Button1: TButton
    Left = 3
    Top = 279
    Width = 103
    Height = 25
    Caption = 'Save'
    TabOrder = 5
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 3
    Top = 310
    Width = 103
    Height = 25
    Caption = 'Load'
    TabOrder = 6
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 3
    Top = 596
    Width = 22
    Height = 25
    Caption = '-'
    TabOrder = 7
    OnClick = Button3Click
  end
  object GroupBox1: TGroupBox
    Left = 841
    Top = 31
    Width = 290
    Height = 58
    Anchors = [akTop, akRight]
    Caption = ' Render '
    TabOrder = 8
    object CheckBox1: TCheckBox
      Left = 16
      Top = 14
      Width = 169
      Height = 17
      Caption = 'Draw triangles'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object cbLineMode: TCheckBox
      Left = 17
      Top = 37
      Width = 169
      Height = 17
      Caption = 'line mode'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object btnNewPoly: TButton
      Left = 88
      Top = 33
      Width = 75
      Height = 25
      Caption = 'btnNewPoly'
      TabOrder = 2
      OnClick = btnNewPolyClick
    end
    object cbDrawBorder: TCheckBox
      Left = 112
      Top = 14
      Width = 97
      Height = 17
      Caption = 'Draw border'
      TabOrder = 3
    end
  end
  object TimerFPS: TTimer
    OnTimer = TimerFPSTimer
    Left = 448
    Top = 16
  end
  object SaveDialog1: TSaveDialog
    Left = 472
    Top = 320
  end
  object OpenDialog1: TOpenDialog
    Left = 504
    Top = 360
  end
  object PopupMenu1: TPopupMenu
    Left = 160
    Top = 160
    object pmLoadMesh: TMenuItem
      Caption = 'Load mesh file'
      OnClick = pmLoadMeshClick
    end
  end
  object PopupMenu2: TPopupMenu
    Left = 248
    Top = 184
    object pmSaveMeshFile: TMenuItem
      Caption = 'Save mesh file...'
      OnClick = pmSaveMeshClick
    end
  end
end
