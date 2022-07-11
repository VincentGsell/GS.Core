object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'maxCron Demo'
  ClientHeight = 735
  ClientWidth = 683
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 677
    Height = 729
    ActivePage = tsEventLog
    Align = alClient
    TabOrder = 0
    object tsIntervals: TTabSheet
      Caption = 'Showcase'
      object pnlIntervalTest: TGroupBox
        AlignWithMargins = True
        Left = 323
        Top = 3
        Width = 343
        Height = 695
        Align = alClient
        Caption = 'Interval preview'
        TabOrder = 0
        object btnCalculate: TBitBtn
          AlignWithMargins = True
          Left = 5
          Top = 90
          Width = 333
          Height = 25
          Align = alTop
          Caption = 'Calculate 100 intervalls'
          TabOrder = 0
          OnClick = btnCalculateClick
        end
        object memCalculatedIntervals: TMemo
          AlignWithMargins = True
          Left = 5
          Top = 167
          Width = 333
          Height = 523
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object StaticText12: TStaticText
          AlignWithMargins = True
          Left = 5
          Top = 121
          Width = 333
          Height = 17
          Align = alTop
          Caption = '(Note: Ignores executionlimit)'
          TabOrder = 2
        end
        object Panel1: TPanel
          Left = 2
          Top = 15
          Width = 339
          Height = 72
          Align = alTop
          BevelOuter = bvNone
          ShowCaption = False
          TabOrder = 3
          object StaticText8: TStaticText
            Left = 16
            Top = 17
            Width = 97
            Height = 17
            Caption = 'Start date and time'
            TabOrder = 0
          end
          object dtDate: TDateTimePicker
            Left = 56
            Top = 41
            Width = 113
            Height = 21
            Date = 43563.000000000000000000
            Time = 0.798362870373239300
            TabOrder = 1
          end
          object dtTime: TDateTimePicker
            Left = 175
            Top = 41
            Width = 113
            Height = 21
            Date = 43563.000000000000000000
            Time = 0.798362870373239300
            DateFormat = dfLong
            Kind = dtkTime
            TabOrder = 2
          end
        end
        object StaticText13: TStaticText
          AlignWithMargins = True
          Left = 5
          Top = 144
          Width = 333
          Height = 17
          Align = alTop
          Caption = 'Results:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 4
        end
      end
      object pnlCronString: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 314
        Height = 695
        Align = alLeft
        Caption = 'Cron string decoder'
        TabOrder = 1
        DesignSize = (
          314
          695)
        object labSample: TStaticText
          Left = 24
          Top = 82
          Width = 281
          Height = 30
          Anchors = [akLeft, akTop, akRight]
          AutoSize = False
          Caption = '(every minute)'
          TabOrder = 21
        end
        object edCronString: TEdit
          Left = 24
          Top = 55
          Width = 202
          Height = 21
          TabOrder = 0
          Text = '* * * * * * * *'
          OnChange = edCronStringChange
        end
        object StaticText1: TStaticText
          Left = 8
          Top = 32
          Width = 57
          Height = 17
          Caption = 'Cron string'
          TabOrder = 1
        end
        object btnReset: TBitBtn
          Left = 115
          Top = 358
          Width = 190
          Height = 25
          Caption = 'Reset'
          TabOrder = 2
          OnClick = btnResetClick
        end
        object StaticText2: TStaticText
          Left = 8
          Top = 118
          Width = 101
          Height = 17
          Caption = 'Cron string decoded'
          TabOrder = 3
        end
        object edMinute: TEdit
          Left = 115
          Top = 137
          Width = 190
          Height = 21
          TabOrder = 4
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText3: TStaticText
          Left = 8
          Top = 141
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Minute'
          TabOrder = 5
        end
        object edHour: TEdit
          Left = 115
          Top = 164
          Width = 190
          Height = 21
          TabOrder = 6
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText4: TStaticText
          Left = 8
          Top = 168
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Hour'
          TabOrder = 7
        end
        object edDayOfMonth: TEdit
          Left = 115
          Top = 191
          Width = 190
          Height = 21
          TabOrder = 8
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText5: TStaticText
          Left = 8
          Top = 195
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Day of the Month'
          TabOrder = 9
        end
        object edMonthOfYear: TEdit
          Left = 115
          Top = 218
          Width = 190
          Height = 21
          TabOrder = 10
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText6: TStaticText
          Left = 8
          Top = 222
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Month of the Year'
          TabOrder = 11
        end
        object edDayOfTheWeek: TEdit
          Left = 115
          Top = 245
          Width = 190
          Height = 21
          TabOrder = 12
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText7: TStaticText
          Left = 8
          Top = 249
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Day of the Week'
          TabOrder = 13
        end
        object edYear: TEdit
          Left = 115
          Top = 272
          Width = 190
          Height = 21
          TabOrder = 14
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText9: TStaticText
          Left = 8
          Top = 276
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Year'
          TabOrder = 15
        end
        object edExecutionLimit: TEdit
          Left = 115
          Top = 326
          Width = 190
          Height = 21
          TabOrder = 16
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText11: TStaticText
          Left = 8
          Top = 330
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'ExecutionLimit'
          TabOrder = 17
        end
        object edSecond: TEdit
          Left = 115
          Top = 299
          Width = 190
          Height = 21
          TabOrder = 18
          Text = '*'
          OnChange = edExecutionLimitChange
        end
        object StaticText10: TStaticText
          Left = 8
          Top = 303
          Width = 101
          Height = 17
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Second'
          TabOrder = 19
        end
        object btnSamples: TButton
          Left = 232
          Top = 53
          Width = 75
          Height = 25
          Caption = 'Samples'
          DropDownMenu = popSamples
          Style = bsSplitButton
          TabOrder = 20
          OnClick = btnSamplesClick
        end
      end
    end
    object tsEventLog: TTabSheet
      Caption = 'Scheduled events log'
      ImageIndex = 1
      object memLog: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 663
        Height = 695
        Align = alClient
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
  object popSamples: TPopupMenu
    Left = 1417
    Top = 67
  end
end
