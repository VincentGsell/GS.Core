unit GS.System.Scheduler.CronLike;
//based upon MaxCron implementation. (see MaxCron units.)
interface

uses sysutils
     ,classes
     ,MaxCron
     ,GS.Common.Log
     ,GS.System.Scheduler;

Type

TGSSCheduleItem = class(TInterfacedObject,IGSSCheduleItem)
private
protected
  FMaxCronEvent : TMaxCronEvent;
  FOnEvent: TNotifyEvent;
public
  Constructor Create(aNative : TMaxCronEvent); reintroduce;

  function Name : String;
  function ScheduleFormula : String;
  function NextUp : TDateTime;
end;


TGSScheduler = class(TInterfacedObject, IGSScheduler)
private
  FMaxCron : TmaxCron;
  FOnEvent: TNotifyEvent;
  procedure FInternalScheduleEvent(Sender: TmaxCronEvent);

  procedure OnSchedulerEvent(Sender : TObject; ScheduleItem : IGSSCheduleItem);
public
  Constructor Create; virtual;
  Destructor Destroy; override;

  procedure AddAcheduleFormula(aScheduleName, aScheduleText : String);
  function GetScheduleItems : TArray<IGSSCheduleItem>;
  Procedure SetSchedulerEvent(aEvent : TNotifyEvent);
  procedure update;


  Property OnScheduleEvent : TNotifyEvent read FOnEvent Write FOnEvent;
end;

{ Possible formula
       59 23 31 12 5 *                     |One minute  before the end of year if the last day of the year is Friday');
     59 23 31 DEC Fri *                    |Same as above (different notation)');
     45 17 7 6 * *                         |Every  year, on June 7th at 17:45');
     45 17 7 6 * 2001,2002                 |Once a   year, on June 7th at 17:45, if the year is 2001 or  2002');
     0,15,30,45 0,6,12,18 1,15,31 * 1-5 *  |At 00:00, 00:15, 00:30, 00:45, 06:00, 06:15, 06:30, 06:45, 12:00, 12:15, 12:30, 12:45, 18:00, 18:15, 18:30, 18:45, on 1st, 15th or  31st of each  month, but not on weekends');
     */15 */6 1,15,31 * 1-5 *              |Same as above (different notation)');
     0 12 * * 1-5 * 0 12 * * Mon-Fri       |At midday on weekdays');
     * * * 1,3,5,7,9,11 * *                |Each minute in January,  March,  May, July, September, and November');
     1,2,3,5,20-25,30-35,59 23 31 12 * *   |On the  last day of year, at 23:01, 23:02, 23:03, 23:05, 23:20, 23:21, 23:22, 23:23, 23:24, 23:25, 23:30, 23:31, 23:32, 23:33, 23:34, 23:35, 23:59');
     0 9 1-7 * 1 *                         |First Monday of each month, at 9 a.m.');
     0 0 1 * * *                           |At midnight, on the first day of each month');
     * 0-11 * * *                          |Each minute before midday');
     * * * 1,2,3 * *                       |Each minute in January, February or March');
     * * * Jan,Feb,Mar * *                 |Same as above (different notation)');
     0 0 * * * *                           |Daily at midnight');
     0 0 * * 3 *                           |Each Wednesday at midnight');
     0 0 * * * * *                         |Daily at midnight every second. That is 60 executions');
     0 0 * * * * 15,30                     |Daily 15 and 30 second after midnight');
     0 0 * * * * * 3                       |Daily at midnight every second. But limited to 3 executions');
}

implementation

{ TGSScheduler }

procedure TGSScheduler.AddAcheduleFormula(aScheduleName, aScheduleText: String);
var l : TMaxCronEvent;
begin
  l := FMaxCron.Add(aScheduleName,aScheduleText,FInternalScheduleEvent);
  l.UserDataInterface := TGSSCheduleItem.Create(l);
  l.Run;
end;

constructor TGSScheduler.Create;
begin
  Inherited;
  FMaxCron := TMaxCron.Create;
end;


destructor TGSScheduler.Destroy;
begin
  FreeAndNil(FMAxCron);
  inherited;
end;

procedure TGSScheduler.FInternalScheduleEvent(Sender: TmaxCronEvent);
begin
  OnSchedulerEvent(Self,TGSSCheduleItem(Sender.UserDataInterface));
end;

function TGSScheduler.GetScheduleItems: TArray<IGSSCheduleItem>;
var i : integer;
begin
  SetLength(result,FMaxCron.Count);
  for i := 0 to FMaxCron.Count-1 do begin
   result[i] := TGSSCheduleItem.Create(FMaxCron.Events[i]);
  end;
end;

procedure TGSScheduler.OnSchedulerEvent(Sender: TObject; ScheduleItem: IGSSCheduleItem);
var l : TGSSCustomScheduleEvent;
begin
  try
    if Assigned(FOnEvent) then begin
      l := TGSSCustomScheduleEvent.Create;
      try
        l.Event := ScheduleItem;
        FOnEvent(l);
      finally
        FreeAndNil(l);
      end;
    end;
  Except
    On E : Exception do
      TLog.error(ClassName+'.OnScheduleEvent -> ' + E.Message);
  end;
end;

procedure TGSScheduler.SetSchedulerEvent(aEvent: TNotifyEvent);
begin
  FOnEvent := aEvent;
end;

procedure TGSScheduler.update;
begin
  FMaxCron.Update;
end;

{ TGSSCheduleItem }

constructor TGSSCheduleItem.Create(aNative : TMaxCronEvent);
begin
  FMaxCronEvent := aNative;
end;

function TGSSCheduleItem.Name: String;
begin
  result := FMAxCronEvent.Name;
end;

function TGSSCheduleItem.NextUp: TDateTime;
begin
  result := FMAxCronEvent.NextSchedule;
end;

function TGSSCheduleItem.ScheduleFormula: String;
begin
  result := FMAxCronEvent.EventPlan;
end;

end.
