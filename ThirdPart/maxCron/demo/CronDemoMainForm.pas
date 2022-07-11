Unit CronDemoMainForm;

Interface

Uses
  maxCron,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus;

Type
  TForm2 = Class(TForm)
    popSamples: TPopupMenu;
    PageControl1: TPageControl;
    tsIntervals: TTabSheet;
    pnlIntervalTest: TGroupBox;
    btnCalculate: TBitBtn;
    memCalculatedIntervals: TMemo;
    pnlCronString: TGroupBox;
    labSample: TStaticText;
    edCronString: TEdit;
    StaticText1: TStaticText;
    btnReset: TBitBtn;
    StaticText2: TStaticText;
    edMinute: TEdit;
    StaticText3: TStaticText;
    edHour: TEdit;
    StaticText4: TStaticText;
    edDayOfMonth: TEdit;
    StaticText5: TStaticText;
    edMonthOfYear: TEdit;
    StaticText6: TStaticText;
    edDayOfTheWeek: TEdit;
    StaticText7: TStaticText;
    edYear: TEdit;
    StaticText9: TStaticText;
    edExecutionLimit: TEdit;
    StaticText11: TStaticText;
    edSecond: TEdit;
    StaticText10: TStaticText;
    btnSamples: TButton;
    tsEventLog: TTabSheet;
    memLog: TMemo;
    StaticText12: TStaticText;
    Panel1: TPanel;
    StaticText8: TStaticText;
    dtDate: TDateTimePicker;
    dtTime: TDateTimePicker;
    StaticText13: TStaticText;
    Procedure FormCreate(Sender: TObject);
    Procedure btnResetClick(Sender: TObject);
    Procedure edCronStringChange(Sender: TObject);
    Procedure edExecutionLimitChange(Sender: TObject);
    Procedure btnCalculateClick(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure btnSamplesClick(Sender: TObject);
  Private
    ChronScheduler: TmaxCron;
    FDynamicEvent: TmaxCronEvent;
    fSamples: TStringList;
    Procedure OnScheduleTrigger(Sender: TmaxCronEvent);
    Procedure log(Const msg: String);
    Function showDate(Const aDateTime: TDateTime): String;
    Procedure prepareSamples;
    Function GetSampleCronString(Const sample: String): String;
    Function GetSampleCaption(Const sample: String): String;
    Procedure SelectSample(Sender: TObject); Overload;
    Procedure SelectSample(index: Integer); Overload;
    Procedure calculateIntervalls;
  Public

  End;

Var
  Form2: TForm2;

Implementation

{$R *.dfm}


Procedure TForm2.FormCreate(Sender: TObject);
Var
  NewSchedule: TmaxCronEvent;
  startDate, StopDate: TDateTime;
  plan: TPlan;
  x: Integer;
Begin
  PageControl1.activepageIndex := 0;

  dtDate.date := now();
  dtTime.time := time();
  prepareSamples;

  log('now is ' + showDate(now));

  // create a new TmaxCron  that will hold all the events
  ChronScheduler := TmaxCron.Create;

  NewSchedule := ChronScheduler.Add('Event1', '1 * * * * *', OnScheduleTrigger).Run;
  log(NewSchedule.name + ' next scheduled date is ' + showDate(NewSchedule.NextSchedule));

  // you can use anonymous methods as well
  NewSchedule := ChronScheduler.Add('Event2');
  NewSchedule.EventPlan := '*/2 * * * * *';
  NewSchedule.OnScheduleproc := Procedure(aEvent: TmaxCronEvent)
    Begin
      OnScheduleTrigger(aEvent);
    End;
  NewSchedule.Run;
  log(NewSchedule.name + ' next scheduled date is ' + showDate(NewSchedule.NextSchedule));

  // using a shorter adding syntax
  NewSchedule := ChronScheduler.Add('Event4', '1 * * * * *',
    Procedure(aEvent: TmaxCronEvent)
    Begin
      OnScheduleTrigger(aEvent);
    End).Run;
  log(NewSchedule.name + ' next scheduled date is ' + showDate(NewSchedule.NextSchedule));

  // using the TPlan helper
  // The TPlan is a small record that allows you to specify the parts in a more friendly way and then convert them to a cron string
  plan := Default (TPlan); // it is a record, so initialize it properly
  // you can use the reset method to reset all the values to their defaults like this:
  plan.reset;
  // you can access any of the fields just like that:
  plan.Second := '30';
  // now create a new event using our new plan
  NewSchedule := ChronScheduler.Add('EventFromTPlan', plan.text, OnScheduleTrigger).Run;
  log(NewSchedule.name + ' next scheduled date is ' + showDate(NewSchedule.NextSchedule));

  // start stop dynamic event
  FDynamicEvent := ChronScheduler.Add('DynamicSchedule');
  plan.reset;
  plan.Second := '15';
  FDynamicEvent.EventPlan := plan.text;
  FDynamicEvent.OnScheduleEvent := OnScheduleTrigger;
  FDynamicEvent.Run;
  log(FDynamicEvent.name + ' next scheduled date is ' + showDate(FDynamicEvent.NextSchedule));

  // now ad a event with a valid range

  // start time is in 50 seconds
  startDate := now() + 1 / 24 / 60 / 60 * 50;
  // and stop 5 minutes afterwards
  StopDate := startDate + 1 / 24 / 60 * 5;
  log('Ranged Event start date: ' + showDate(startDate));
  log('Ranged Event stop date: ' + showDate(StopDate));
  NewSchedule := ChronScheduler.Add('RangedSchedule');
  NewSchedule.EventPlan := '0 0 */2 * 1,5,10 7 *';
  NewSchedule.OnScheduleEvent := OnScheduleTrigger;
  NewSchedule.ValidFrom := startDate;
  NewSchedule.ValidTo := StopDate;
  NewSchedule.Run;
  log(NewSchedule.name + ' next scheduled date is ' + showDate(NewSchedule.NextSchedule));

End;

Procedure TForm2.OnScheduleTrigger(Sender: TmaxCronEvent);
Begin
  log(Format('Event "%s"  was trigered at : %s, next scheduled date is %s',
    [Sender.name,
    showDate(now),
    showDate(Sender.NextSchedule)]));
End;

Procedure TForm2.log(Const msg: String);
Begin
  memLog.lines.Add(msg);
  // scroll to bottom to move the new added line into view.
  memLog.Perform(WM_VSCROLL, SB_BOTTOM, 0);
End;

Function TForm2.showDate(Const aDateTime: TDateTime): String;
Begin
  result := formatDateTime(
    'yyyy"-"mm"-"dd" "hh":"nn":"ss"."zzz', aDateTime);
End;

Procedure TForm2.btnResetClick(Sender: TObject);
Begin
  edCronString.setFocus;
  edCronString.text := '* * * * * * * *';
End;

Procedure TForm2.edCronStringChange(Sender: TObject);
Var
  plan: TPlan;
Begin
  If edCronString.focused Then
  Begin
      plan.text := edCronString.text;

    edMinute.text := plan.minute;
    edHour.text := plan.hour;
    edDayOfMonth.text := plan.DayOfTheMonth;
    edMonthOfYear.text := plan.Month;
    edDayOfTheWeek.text := plan.DayOfTheWeek;
    edYear.text := plan.Year;
    edSecond.text := plan.Second;
    edExecutionLimit.text := plan.ExecutionLimit;
    labSample.Caption := '';
  End;
End;

Procedure TForm2.edExecutionLimitChange(Sender: TObject);
Var
  edit: TEdit;
  plan: TPlan;
Begin
  edit := Sender As TEdit;
  If edit.focused Then
  Begin
      plan.reset;

    plan.minute := edMinute.text;
    plan.hour := edHour.text;
    plan.DayOfTheMonth := edDayOfMonth.text;
    plan.Month := edMonthOfYear.text;
    plan.DayOfTheWeek := edDayOfTheWeek.text;
    plan.Year := edYear.text;
    plan.Second := edSecond.text;
    plan.ExecutionLimit := edExecutionLimit.text;

    edCronString.text := plan.text;
    labSample.Caption := '';
  End;
End;

Procedure TForm2.btnCalculateClick(Sender: TObject);
Begin
  calculateIntervalls;
End;

Procedure TForm2.calculateIntervalls;
Var
  dt: TDateTime;
  plan: TPlan;
  schedule: TCronSchedulePlan;
  x: Integer;
Begin
  schedule := TCronSchedulePlan.Create;
  memCalculatedIntervals.lines.beginUpdate;
  Try
    memCalculatedIntervals.lines.clear;
    dt := trunc(dtDate.DateTime) + frac(dtTime.DateTime);

    schedule.Parse(edCronString.text);

    For x := 0 To 99 Do
    Begin
      If Not schedule.FindNextScheduleDate(dt, dt) Then
        break;

      memCalculatedIntervals.lines.Add(
        formatDateTime('ddd', dt) + #9 +
        showDate(dt));
    End;
  Finally
    memCalculatedIntervals.lines.endUpdate;
    schedule.free;
  End;

End;

Procedure TForm2.prepareSamples;
Var
  mi: TMenuItem;
  x: Integer;
  s: String;
Begin
  // first prepare the list
  // I;ve made it as a simple stringList, in a readable format, we will get rd of the extra spaces later on.
  fSamples := TStringList.Create;
  With fSamples Do
  Begin
      Add('59 23 31 12 5 *                       |One minute  before the end of year if the last day of the year is Friday');
    Add('59 23 31 DEC Fri *                    |Same as above (different notation)');
    Add('45 17 7 6 * *                         |Every  year, on June 7th at 17:45');
    Add('45 17 7 6 * 2001,2002                 |Once a   year, on June 7th at 17:45, if the year is 2001 or  2002');
    Add('0,15,30,45 0,6,12,18 1,15,31 * 1-5 *  |At 00:00, 00:15, 00:30, 00:45, 06:00, 06:15, 06:30, 06:45, 12:00, 12:15, 12:30, 12:45, 18:00, 18:15, 18:30, 18:45, on 1st, 15th or  31st of each  month, but not on weekends');
    Add('*/15 */6 1,15,31 * 1-5 *              |Same as above (different notation)');
    Add('0 12 * * 1-5 * 0 12 * * Mon-Fri      |At midday on weekdays');
    Add('* * * 1,3,5,7,9,11 * *                |Each minute in January,  March,  May, July, September, and November');
    Add('1,2,3,5,20-25,30-35,59 23 31 12 * *   |On the  last day of year, at 23:01, 23:02, 23:03, 23:05, 23:20, 23:21, 23:22, 23:23, 23:24, 23:25, 23:30, 23:31, 23:32, 23:33, 23:34, 23:35, 23:59');
    Add('0 9 1-7 * 1 *                         |First Monday of each month, at 9 a.m.');
    Add('0 0 1 * * *                           |At midnight, on the first day of each month');
    Add('* 0-11 * * *                          |Each minute before midday');
    Add('* * * 1,2,3 * *                       |Each minute in January, February or March');
    Add('* * * Jan,Feb,Mar * *                 |Same as above (different notation)');
    Add('0 0 * * * *                           |Daily at midnight');
    Add('0 0 * * 3 *                           |Each Wednesday at midnight');
    Add('0 0 * * * * *                         |Daily at midnight every second. That is 60 executions');
    Add('0 0 * * * * 15,30                     |Daily 15 and 30 second after midnight');
    Add('0 0 * * * * * 3                       |Daily at midnight every second. But limited to 3 executions');
  End;

  // now build the popup menu
  For x := 0 To fSamples.count - 1 Do
  Begin
      mi := TMenuItem.Create(popSamples);
    mi.Caption := GetSampleCaption(fSamples[x]);
    mi.onClick := SelectSample;
    mi.tag := x;
    popSamples.items.Add(mi);
    mi.VISIBLE := TRUE;
  End;
End;

Procedure TForm2.FormDestroy(Sender: TObject);
Begin
  fSamples.free;
End;

Function TForm2.GetSampleCronString(Const sample: String): String;
Begin
  result := trim(copy(sample, 1, pos('|', sample) - 1));
End;

Function TForm2.GetSampleCaption(Const sample: String): String;
Begin
  result := trim(copy(sample, pos('|', sample) + 1, length(sample)));
End;

Procedure TForm2.SelectSample(Sender: TObject);
Var
  mi: TMenuItem;
Begin
  mi := Sender As TMenuItem;
  SelectSample(mi.tag);
End;

Procedure TForm2.SelectSample(index: Integer);
Var
  s: String;
Begin
  edCronString.setFocus;
  edCronString.text := GetSampleCronString(fSamples[Index]);
  s := GetSampleCaption(fSamples[Index]);
  labSample.Caption := '(' + s + ')';

  calculateIntervalls;

  // now inject the samle info as the first line in the memo.
  memCalculatedIntervals.lines.insert(0, s + sLineBreak);
End;

Procedure TForm2.btnSamplesClick(Sender: TObject);
Var
  p: Tpoint;
Begin
  p := point(0, btnSamples.height);
  p := btnSamples.ClientToScreen(p);
  popSamples.Popup(p.x, p.y);
End;

End.
