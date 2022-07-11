unit schedulerDemo.fmain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.Edit, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation,
  GS.Common.Log,
  GS.System.Scheduler;

type
  TfMain = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Memo1: TMemo;
    Panel2: TPanel;
    btnAddShedule: TButton;
    Edit1: TEdit;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure OnInternalLog (Const _logCat : TLogCat; const _LogText : String; const _LogComment : string = '');
  public
    MyScheduler : IGSScheduler;

    procedure OnEventSummon(Sender : TObject);
    { Public declarations }
  end;

var
  fMain: TfMain;

implementation

{$R *.fmx}

procedure TfMain.FormCreate(Sender: TObject);
begin
  GS_Global_Log.AutomaticAddDefaultLoggerIfNoOtherImplementation := False;
  GS_Global_Log.OnLog := OnInternalLog;

  MyScheduler := GSDefaultImplementation;
  MyScheduler.SetSchedulerEvent(OnEventSummon);

  //Minute Hour DayOfTheMonth Month DayOfTheWeek Year Second ExecLimit
  //Warning : Classic Cron is not second level. If you want Classic cron behaviour, do not
  //forget to remove second and execlimit.

//  MyScheduler.AddAcheduleFormula('All 1 second', '* * * * * * * *'); //not very efficient : 2 second
  MyScheduler.AddAcheduleFormula('All 5 seconds',  '* * * * * * */5');
  MyScheduler.AddAcheduleFormula('All 1 minute',   '*/1 * * * * *');
  MyScheduler.AddAcheduleFormula('All 10 minute',  '*/10 * * * * * * *');
  MyScheduler.AddAcheduleFormula('All 1 hour   ',  '0 * * * * * * *');
  MyScheduler.AddAcheduleFormula('everyday 2PM ',  '0 14 * * * * 0 0');
  MyScheduler.AddAcheduleFormula('everyday 3PM ',  '0 15');
  MyScheduler.AddAcheduleFormula('All 14.02 monday', '2 14 * * 1');

end;

procedure TfMain.OnEventSummon(Sender: TObject);
var l : IGSSCheduleItem;
begin
  l := TGSSCustomScheduleEvent(sender).Event;
  Tlog.info('Event '+l.Name+ ' triggered '+DateTimeToStr(Now));
end;

procedure TfMain.OnInternalLog(const _logCat: TLogCat; const _LogText,
  _LogComment: string);
begin
  Memo1.Lines.Add(_LogText);
end;

end.
