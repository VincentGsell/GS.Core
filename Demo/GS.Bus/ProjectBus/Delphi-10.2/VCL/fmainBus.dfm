object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Embeded Bus'
  ClientHeight = 576
  ClientWidth = 993
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
    993
    576)
  PixelsPerInch = 96
  TextHeight = 13
  object pnl2: TPanel
    Left = 8
    Top = 47
    Width = 977
    Height = 521
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    DesignSize = (
      977
      521)
    object Label3: TLabel
      Left = 151
      Top = 18
      Width = 50
      Height = 13
      Caption = 'to channel'
    end
    object Label2: TLabel
      Left = 733
      Top = 80
      Width = 158
      Height = 13
      Caption = 'GUI Received event'#39's hit count : '
    end
    object Label1: TLabel
      Left = 892
      Top = 80
      Width = 6
      Height = 13
      Caption = '0'
    end
    object lblChannels: TLabel
      Left = 436
      Top = 80
      Width = 123
      Height = 13
      Caption = 'Channels (Data from Bus)'
    end
    object lbl2: TLabel
      Left = 431
      Top = 269
      Width = 138
      Height = 13
      Caption = 'Subscribters (Data from Bus)'
    end
    object edt1: TEdit
      Left = 207
      Top = 15
      Width = 202
      Height = 21
      TabOrder = 0
      Text = 'My New Channel...'
    end
    object btn2: TButton
      Left = 16
      Top = 9
      Width = 129
      Height = 34
      Caption = 'Add subscribters'
      TabOrder = 1
      OnClick = btn2Click
    end
    object lst2: TListBox
      Left = 16
      Top = 80
      Width = 393
      Height = 430
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 2
    end
    object btn4: TButton
      Left = 16
      Top = 46
      Width = 129
      Height = 32
      Caption = 'Remove subscribters'
      TabOrder = 3
      OnClick = btn4Click
    end
    object btn3: TButton
      Tag = 1000
      Left = 566
      Top = 46
      Width = 129
      Height = 31
      Caption = 'Send 1000 Messages'
      TabOrder = 4
      OnClick = btn3Click
    end
    object btn1: TButton
      Left = 431
      Top = 46
      Width = 129
      Height = 32
      Caption = 'Send Message'
      TabOrder = 5
      OnClick = btn1Click
    end
    object ListView1: TListView
      Left = 431
      Top = 288
      Width = 538
      Height = 222
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'ID'
        end
        item
          Caption = 'Channel'
          Width = 200
        end
        item
          Caption = 'Pending'
          Width = 70
        end
        item
          Caption = 'Processed'
          Width = 70
        end>
      DoubleBuffered = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      TabOrder = 6
      ViewStyle = vsReport
    end
    object ListView2: TListView
      Left = 431
      Top = 99
      Width = 538
      Height = 164
      Anchors = [akLeft, akTop, akRight]
      Columns = <
        item
          Caption = 'ChannelID'
          Width = 200
        end
        item
          Caption = 'Type'
          Width = 100
        end
        item
          Caption = 'IsPersits'
          Width = 70
        end
        item
          Caption = 'Received'
        end
        item
          Caption = 'Consumed'
        end
        item
          Caption = 'Persists'
        end
        item
          Caption = 'Subscribter count'
        end>
      DoubleBuffered = True
      ReadOnly = True
      RowSelect = True
      ParentDoubleBuffered = False
      TabOrder = 7
      ViewStyle = vsReport
    end
    object GroupBox1: TGroupBox
      Left = 436
      Top = 5
      Width = 533
      Height = 35
      Caption = ' Bus Stats'
      TabOrder = 8
      object Label4: TLabel
        Left = 15
        Top = 17
        Width = 95
        Height = 13
        Caption = 'Message Sended'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
    end
  end
  object Panel1: TPanel
    Left = 8
    Top = 3
    Width = 977
    Height = 41
    Caption = 'Panel1'
    TabOrder = 1
  end
  object TimerGui: TTimer
    Interval = 250
    OnTimer = TimerGuiTimer
    Left = 280
    Top = 112
  end
  object TimerBusQuery: TTimer
    Interval = 100
    OnTimer = TimerBusQueryTimer
    Left = 280
    Top = 176
  end
end
