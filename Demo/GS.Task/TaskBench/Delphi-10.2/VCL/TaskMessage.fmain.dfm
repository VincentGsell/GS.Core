object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Tasks Bench'
  ClientHeight = 428
  ClientWidth = 796
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    796
    428)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 152
    Width = 121
    Height = 49
    Caption = 'Send 1000 Messages (all)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 8
    Width = 121
    Height = 50
    Caption = 'Create Task "Wait"'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = Button2Click
  end
  object ListView1: TListView
    Left = 135
    Top = 8
    Width = 655
    Height = 410
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Status'
        MaxWidth = 100
        MinWidth = 50
        Width = 100
      end
      item
        Caption = 'Task Class'
        Width = 150
      end
      item
        Caption = 'Progress'
        MaxWidth = 150
        Width = 100
      end
      item
        Caption = 'Message In Queue'
        Width = 150
      end
      item
        Caption = 'Message Delivered'
        Width = 150
      end>
    DoubleBuffered = True
    ReadOnly = True
    RowSelect = True
    ParentDoubleBuffered = False
    TabOrder = 2
    ViewStyle = vsReport
    OnClick = ListView1Click
  end
  object Button3: TButton
    Left = 8
    Top = 207
    Width = 121
    Height = 50
    Caption = 'Send 500 to selected "Wait"'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 8
    Top = 64
    Width = 121
    Height = 50
    Caption = 'Create Task 400k'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 8
    Top = 280
    Width = 121
    Height = 49
    Caption = 'Kill Task'
    TabOrder = 5
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 8
    Top = 335
    Width = 121
    Height = 58
    Caption = 'Remove Task'
    TabOrder = 6
    OnClick = Button6Click
  end
  object Timer1: TTimer
    Interval = 100
    OnTimer = Timer1Timer
    Left = 296
    Top = 216
  end
end
