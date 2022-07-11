unit GS.System.Scheduler;

interface

uses classes,sysutils;

Type
IGSSCheduleItem = Interface;

IGSSCheduleItem = interface
  function Name : String;
  function ScheduleFormula : String;
  function NextUp : TDateTime;
end;

TGSSCustomScheduleEvent = Class
  Event : IGSSCheduleItem;
End;

IGSScheduler = Interface
  procedure AddAcheduleFormula(aScheduleName, aScheduleText : String);
  function GetScheduleItems : TArray<IGSSCheduleItem>;

  procedure OnSchedulerEvent(Sender : TObject; ScheduleItem : IGSSCheduleItem);

  Procedure SetSchedulerEvent(aEvent : TNotifyEvent);

  procedure update;
End;

function GSSchedulerDefaultImplementation : IGSScheduler;

implementation

uses GS.System.Scheduler.CronLike;

function GSSchedulerDefaultImplementation : IGSScheduler;
begin
  result := TGSScheduler.Create;
end;


end.
