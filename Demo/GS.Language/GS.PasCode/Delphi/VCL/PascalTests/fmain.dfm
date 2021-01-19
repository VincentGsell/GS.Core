object Form34: TForm34
  Left = 0
  Top = 0
  Caption = 'Form34'
  ClientHeight = 776
  ClientWidth = 1080
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 735
    Width = 1080
    Height = 41
    Align = alBottom
    TabOrder = 0
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 1080
    Height = 62
    Align = alTop
    Caption = 'Panel2'
    TabOrder = 1
    object Button2: TButton
      Left = 4
      Top = 8
      Width = 101
      Height = 48
      Caption = 'Compile'
      TabOrder = 0
      OnClick = Button2Click
    end
    object CheckBox1: TCheckBox
      Left = 111
      Top = 8
      Width = 149
      Height = 17
      Caption = 'Real time compilation'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 62
    Width = 1080
    Height = 673
    ActivePage = tsFormula
    Align = alClient
    TabOrder = 2
    object tsCode: TTabSheet
      Caption = 'Code'
      DesignSize = (
        1072
        645)
      object Memo1: TMemo
        Left = 5
        Top = 27
        Width = 599
        Height = 381
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
        Left = 5
        Top = 414
        Width = 1064
        Height = 228
        Anchors = [akLeft, akTop, akRight, akBottom]
        TabOrder = 1
      end
      object ListBox1: TListBox
        Left = 610
        Top = 3
        Width = 459
        Height = 555
        Anchors = [akLeft, akTop, akRight, akBottom]
        ItemHeight = 13
        TabOrder = 2
      end
      object RadioButton1: TRadioButton
        Left = 5
        Top = 4
        Width = 113
        Height = 17
        Caption = 'RadioButton1'
        Checked = True
        TabOrder = 3
        TabStop = True
        OnClick = RadioButton1Click
      end
      object RadioButton2: TRadioButton
        Left = 96
        Top = 4
        Width = 113
        Height = 17
        Caption = 'RadioButton2'
        TabOrder = 4
        OnClick = RadioButton2Click
      end
      object RadioButton3: TRadioButton
        Left = 184
        Top = 4
        Width = 113
        Height = 17
        Caption = 'RadioButton2'
        TabOrder = 5
        OnClick = RadioButton3Click
      end
      object RadioButton4: TRadioButton
        Left = 280
        Top = 4
        Width = 113
        Height = 17
        Caption = 'RadioButton2'
        TabOrder = 6
        OnClick = RadioButton4Click
      end
    end
    object tsVM: TTabSheet
      Caption = 'VM'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 32
        Top = 24
        Width = 289
        Height = 161
        Caption = 'Panel3'
        TabOrder = 0
        object Button1: TButton
          Left = 24
          Top = 16
          Width = 114
          Height = 48
          Caption = 'Run'
          TabOrder = 0
        end
      end
    end
    object tsFormula: TTabSheet
      Caption = 'Formula'
      ImageIndex = 2
      object but: TPanel
        Left = 0
        Top = 0
        Width = 1072
        Height = 41
        Align = alTop
        TabOrder = 0
        object cbFormula: TComboBox
          Left = 8
          Top = 14
          Width = 265
          Height = 21
          ItemIndex = 0
          TabOrder = 0
          Text = '2*(3-5)+7'
          Items.Strings = (
            '2*(3-5)+7'
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
        Left = 281
        Top = 41
        Width = 791
        Height = 604
        Align = alClient
        Lines.Strings = (
          'logs...'
          '')
        TabOrder = 1
      end
      object Panel5: TPanel
        Left = 0
        Top = 41
        Width = 281
        Height = 604
        Align = alLeft
        Caption = 'Panel5'
        TabOrder = 2
        DesignSize = (
          281
          604)
        object lvVar: TListView
          Left = 8
          Top = 37
          Width = 267
          Height = 558
          Anchors = [akLeft, akTop, akBottom]
          Color = 15859711
          Columns = <
            item
              Caption = 'Variable'
            end
            item
              Caption = 'Value'
            end>
          DoubleBuffered = True
          GridLines = True
          Items.ItemData = {
            059A0000000400000000000000FFFFFFFFFFFFFFFF01000000FFFFFFFF000000
            000161000231003000F0647A1500000000FFFFFFFFFFFFFFFF01000000FFFFFF
            FF00000000016200013500D8D5CB0000000000FFFFFFFFFFFFFFFF01000000FF
            FFFFFF0000000001630001320058CBCB0000000000FFFFFFFFFFFFFFFF010000
            00FFFFFFFF0000000001640003320030003000E0EFCB00FFFFFFFFFFFFFFFF}
          ParentDoubleBuffered = False
          TabOrder = 0
          ViewStyle = vsReport
          ExplicitHeight = 452
        end
        object btFormulaRun: TButton
          Left = 8
          Top = 6
          Width = 267
          Height = 25
          Caption = 'Run'
          TabOrder = 1
          OnClick = btFormulaRunClick
        end
      end
    end
  end
end
