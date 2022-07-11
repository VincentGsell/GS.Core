object Form34: TForm34
  Left = 0
  Top = 0
  Caption = 'Form34'
  ClientHeight = 1665
  ClientWidth = 2554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -27
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  PixelsPerInch = 216
  TextHeight = 33
  object Panel1: TPanel
    Left = 0
    Top = 1566
    Width = 2554
    Height = 99
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alBottom
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 2554
    Height = 151
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 1
    object Button2: TButton
      Left = 9
      Top = 25
      Width = 461
      Height = 103
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      Caption = 'Compile'
      TabOrder = 0
      OnClick = Button2Click
    end
    object CheckBox1: TCheckBox
      Left = 563
      Top = 41
      Width = 753
      Height = 85
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      Caption = 'Real time compilation'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 151
    Width = 2554
    Height = 1415
    Margins.Left = 7
    Margins.Top = 7
    Margins.Right = 7
    Margins.Bottom = 7
    ActivePage = tsFormula
    Align = alClient
    TabOrder = 2
    object tsCode: TTabSheet
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      Caption = 'Code'
      DesignSize = (
        2534
        1348)
      object Memo1: TMemo
        Left = 25
        Top = 137
        Width = 1021
        Height = 1238
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Anchors = [akLeft, akTop, akBottom]
        Lines.Strings = (
          '//simple basic program'
          'program test;'
          'var a,b,c : integer;'
          'begin'
          'a : = 10;'
          'b := 20;'
          'c := 30;'
          'd :integer := a*b+c;'
          '//if (a+b)=c then print('#39'ok'#39') else print('#39'ko'#39');'
          'println(d);'
          'end.')
        TabOrder = 0
        OnChange = Memo1Change
      end
      object Memo2: TMemo
        Left = 25
        Top = 2097
        Width = 2554
        Height = 41
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
      end
      object ListBox1: TListBox
        Left = 1053
        Top = 137
        Width = 1492
        Height = 1238
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 33
        TabOrder = 2
      end
      object RadioButton1: TRadioButton
        Left = 25
        Top = 20
        Width = 574
        Height = 86
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Caption = 'RadioButton1'
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = RadioButton1Click
      end
      object RadioButton2: TRadioButton
        Left = 486
        Top = 20
        Width = 572
        Height = 86
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Caption = 'RadioButton2'
        TabOrder = 4
        OnClick = RadioButton2Click
      end
      object RadioButton3: TRadioButton
        Left = 932
        Top = 20
        Width = 571
        Height = 86
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Caption = 'RadioButton2'
        TabOrder = 5
        OnClick = RadioButton3Click
      end
      object RadioButton4: TRadioButton
        Left = 1418
        Top = 20
        Width = 571
        Height = 86
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Caption = 'RadioButton2'
        TabOrder = 6
        OnClick = RadioButton4Click
      end
    end
    object tsVM: TTabSheet
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      Caption = 'VM'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 162
        Top = 122
        Width = 1463
        Height = 814
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Caption = 'Panel3'
        TabOrder = 0
        object Button1: TButton
          Left = 122
          Top = 81
          Width = 578
          Height = 243
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
          Caption = 'Run'
          TabOrder = 0
        end
      end
    end
    object tsFormula: TTabSheet
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      Caption = 'Formula'
      ImageIndex = 2
      object but: TPanel
        Left = 0
        Top = 0
        Width = 2534
        Height = 207
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alTop
        TabOrder = 0
        object cbFormula: TComboBox
          Left = 41
          Top = 72
          Width = 1341
          Height = 41
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
          ItemIndex = 0
          TabOrder = 0
          Text = '6/2*(1+2)'
          Items.Strings = (
            '6/2*(1+2)'
            '6/2(1+2)'
            '2(3-5)7'
            'a+10'
            'a+b'
            'a+b*c'
            '(a)+(b+c)'
            '(a-b+c)*d'
            'a'
            '((a))'
            '(a-15)+c*(a+b)')
        end
      end
      object Memo3: TMemo
        Left = 1422
        Top = 207
        Width = 1112
        Height = 1141
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alClient
        Lines.Strings = (
          'logs...'
          '')
        TabOrder = 1
      end
      object Panel5: TPanel
        Left = 0
        Top = 207
        Width = 1422
        Height = 1141
        Margins.Left = 16
        Margins.Top = 16
        Margins.Right = 16
        Margins.Bottom = 16
        Align = alLeft
        Caption = 'Panel5'
        TabOrder = 2
        DesignSize = (
          1422
          1141)
        object lvVar: TListView
          Left = 38
          Top = 189
          Width = 1353
          Height = 1006
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
          Anchors = [akLeft, akTop, akBottom]
          Color = 15859711
          Columns = <
            item
              Caption = 'Variable'
              Width = 254
            end
            item
              Caption = 'Value'
              Width = 254
            end>
          DoubleBuffered = True
          GridLines = True
          Items.ItemData = {
            05C30000000500000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000
            000161000231003000D0F9E41E00000000FFFFFFFFFFFFFFFF01000000FFFFFF
            FF0000000001620001350020291D1C00000000FFFFFFFFFFFFFFFF01000000FF
            FFFFFF0000000001630001320090BF360200000000FFFFFFFFFFFFFFFF010000
            00FFFFFFFF0000000001640003320030003000B06FE31F00000000FFFFFFFFFF
            FFFFFF01000000FFFFFFFF00000000036500330033000134006846E405FFFFFF
            FFFFFFFFFFFFFF}
          ParentDoubleBuffered = False
          TabOrder = 0
          ViewStyle = vsReport
          ExplicitHeight = 965
        end
        object btFormulaRun: TButton
          Left = 41
          Top = 32
          Width = 1352
          Height = 126
          Margins.Left = 16
          Margins.Top = 16
          Margins.Right = 16
          Margins.Bottom = 16
          Caption = 'Run'
          TabOrder = 1
          OnClick = btFormulaRunClick
        end
      end
    end
  end
end
