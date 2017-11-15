///-------------------------------------------------------------------------------
/// Title      : GS.Task
/// Short Desc : Task manager with task intercommunication layer.
/// Source     : https://github.com/VincentGsell
/// Aim        : - Task manager (a "task" = a thread)
///              - Easy communication mnethod between task.
///              - Advanced "breath" method, to let idling switching contexte
///              --> threaded is not essentialy "speed up", but "paralelized"
///              - Easely Kill or Terminate task an remain it in memory. (See demo)
///-------------------------------------------------------------------------------
unit GS.Task;
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  Generics.Collections,
  SyncObjs,
{$ELSE}
  System.Types,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Stream,
  GS.Threads;

Const
  TopTaskStatusStr : Array[0..10] of String = ('Wait for start','In begin','In Loop','In Progress','In Interrupt','In Message','Wait for Message','Send Message','In finish','Terminated','Exception');
  opTaskInternalMessageTimeOutInMS = 100;
  opTaskMark = 958;

  CST_SLEEP_RETAIN_MODE = 1;   //Millisec.
  CST_WAITFOR_TIMEOUT   = 250; //millisec.

type

  TopTaskStatus = ( optWaitToStart,
                    optbegin,
                    optLoop,
                    optOnProgress,
                    optOnInterrupt,
                    optOnMessage,
                    optOnMessageWait,
                    optOnSendMessage,
                    optfinish,
                    optTerminated,
                    optException);

  TTaskManager = Class;

  //***************************************************************************/
  //Inter-Thread message. (Communication between thread)
  //This message is exchange between thread.
  TopTaskMessage = Record
    Bytes : TBytes;
  end;
  pTopTaskMessage = ^TopTaskMessage;

  TopMessageType = (mtUnkown, mtPureString, mtMemoryStream);

  TopTaskMessageHelper = Record Helper for TopTaskMessage
    Procedure BuidStringMessage(aString : String);
    Procedure BuildStreamMessage(aStreamMessage : TMemoryStream);
    Function MessageType : TopMessageType;
    Function AsString : String;
    Function AsMemoryStream : TMemoryStream;
  end;

  TopTaskMessageList = class
  Private
    FL : TList<pTopTaskMessage>;
    FCS : TCriticalSection;
  Public
    Function Lock : TList<pTopTaskMessage>;
    Procedure UnLock;
    Procedure Clean;
    Constructor Create; Virtual;
    Destructor Destroy; Override;
  end;
  //***************************************************************************/

  //***************************************************************************/
  //INTERNAL Thread message. (String inner thread message :
  // --> To harvest by TaskManager.Log;

  TopInternalTaskLogType = (itmString, itmStream, itmCoded);
  TopInternalTaskLog = Record
  Public
    MessageCreation : TDateTime;
    TaskStatusSnapShot :TopTaskStatus;
    MessageType : TopInternalTaskLogType;
    Buffer : TBytes;
    TagObject : TObject;
  end;
  pTopInternalTaskLog = ^TopInternalTaskLog;

  TopTaskLog = class
  Private
    FL : TList<pTopInternalTaskLog>;
    FCS : TCriticalSection;
  Public
    Procedure AddStringMessage(aTaskStatus : TopTaskStatus; aMessageString : String);
    Procedure AddStreamMessage(aTaskStatus : TopTaskStatus; aMessageStream : TMemoryStream);
    Procedure AddEmptyMessage(aTaskStatus : TopTaskStatus);

    Function Lock : TList<pTopInternalTaskLog>;
    Procedure UnLock;
    Procedure Clean;
    Constructor Create; Virtual;
    Destructor Destroy; Override;
  end;
  //***************************************************************************/

  TProtectedTaskStatus = Class(TProtectedValue<TopTaskStatus>);


  {TopStandAloneMessagingSystem provide a integrated message distribution system
  build for inter thread communication. It is used in TopTask.
  The system could be used outside of TopTask : It should be embeded to a TThread for provide
  communication with it. See GRID.Services.xxxx to see usage.}
  TopStandAloneMessagingSystem = class
  Private
    FMessageList : TopTaskMessageList;
    FMessageEvent : TEvent;
  Protected
  Public
    Constructor create; Virtual;
    Destructor Destroy; Override;

    Procedure DeliverMeMessage(var aNewMessaqe: TopTaskMessage);
    Function ProcessMessage(Const aWait : Boolean = True) : Boolean;

    Procedure OnTimeOut(var StopWait : Boolean); Virtual;
    Procedure OnWaiting; Virtual;
    Procedure OnMessageDelivered(var aNewMessage : TopTaskMessage; const MessageRemaining : Cardinal); Virtual;
  end;


  TopTask = class;
  TopTaskClass = Class of TopTask;

  TopTaskMessagingSystem = class(TopStandAloneMessagingSystem)
  Public
    Master : TopTask;
    Procedure OnWaiting; Override;
    Procedure OnTimeOut(var StopWait : Boolean); Override;
    Procedure OnMessageDelivered( var aNewMessage : TopTaskMessage;
                                  const MessageRemaining : Cardinal); Override;
  end;

  TopTask = Class(TThread)
  Private
    FTaskStatus : TProtectedTaskStatus;
    FLastProgress : TProtectedString;
    FInternalCounter : TProtectedInt64;
    FDateTimeBegin : TProtectedDateTime;
    FDateTimeEnd : TProtectedDateTime;
    FTickCountBegin : Int64;
    FTickCountFinish : Int64;
    FTickCountLast : Int64;
    FTickCountFromLastLoop : Int64;
    FException : Exception;
    FTickCountEachLoopAverage : TProtectedDouble;
    FBreathAllowed : TProtectedBoolean;
    FBreathDuration : TProtectedNativeUInt;
    FBreathEveryNCycle : TProtectedNativeUInt;
    FTotalBreathTime : TProtectedNativeUInt;
    FBreathEveryNCycleAllowed : TProtectedBoolean;
    FBreathEveryProgressCall : TProtectedBoolean;
    FMessageDeliveryAccepted : TProtectedBoolean;

    //<FMessageInterruptFlag is used internaly to manage interrupt process.
    //Use EndOfInterrupt to call and reset it.
    FMessageInterruptFlag : TEvent;

    //<FExternalMessageSystem is the messaging system to communicate with other Task.
    FExternalMessageSystem : TopTaskMessagingSystem;

    FMessageInQueue : TProtectedNativeUInt;
    FMessageDelivered : TProtectedNativeUInt;

    //<FInternalMessageLog is the messaging system to communicate with TaskManager.
    FInternalMessageLog : TopTaskLog;

    Procedure InternalDoTaskBegin;
    Procedure InternalDoTaskFinish;
    Procedure InternalDoTaskException(E : Exception);
    Procedure InternalDoTaskProgress(aProgressString : String); Overload;
    Procedure InternalDoTaskProgress(aProgressStream : TMemoryStream); Overload;
    Procedure InternalDoTaskInterrupt;

    Procedure InternalBreathManagement;

    Procedure InternalThreadTerminate(SEnder : TObject); //TNotify Event.

    function GetProgress: String;
    function GetBreathAllowed: Boolean;
    procedure SetBreathAllowed(const Value: Boolean);
    function GetDateTimeBegin: TDateTime;
    function GetDateTimeFinish: TDateTime;
    function GetLoopAverageDurationInMilliSec: Single;
    function GetLoopCount: Int64;
    function GetBreathDuration: NativeUInt;
    function GetBreathEveryNCycle: NativeUInt;
    function GetBreathEveryProgressCall: Boolean;
    procedure SetBreathDuration(const Value: NativeUInt);
    procedure SetBreathEveryNCycle(const Value: NativeUInt);
    procedure SetBreathEveryProgressCall(const Value: Boolean);
    function GetTaskStatus: TopTaskStatus;
    function GetTotalBreathTime: NativeUInt;
    function GetBreathAllowedEveryNCycle: Boolean;
    procedure SetBreathAllowedEveryNCycle(const Value: Boolean);
    function GetTotalActiveTime: NativeUInt;
    function GetMessageDeliveryAccepted: Boolean;
    function GetMessageInQueue: NativeUInt;
    function GetMessageDelivered: NativeUInt;
    function GetTaskStatusAsString: String;
  Protected

    Procedure CheckRun; Virtual; //<Executed at each loop. (Make stat, manage breath, check terminate)
    Procedure Execute; Override;
    Procedure HandleException; Virtual;


    //<DoProcessMessage :
    //<Call that in RunLoop, in order to active the Inter-task process Message system.
    //<It is the responsability of the user to place right DoProcessMessage inside
    //<RunLoop in order to process the incoming message with efficience.
    //<Wait for message will stop the thread until a message, an error, or a terminate order is detected.
    //<It will permit to build thread communication with this message system.
    //<i.e. :
    //<      procedure Execute; Override;
    //<      begin
    //<        DoProcessMessage(True); //This will wait until a message comes ! (Thread CPU time = 0.000(xxx)1 !)
    //<        //Here, a MessageDelivered procedure will be called If message is detected (Called in DoProcessMessage).
    //<        ...Your code here...
    //<
    //<
    Procedure DoProcessMessage(Const WaitForMessage : Boolean = False);

    //<Used normally only by the TaskManager to deliver a message to the task.
    //<But it can be use by alien thread to put message in the task.
    //<It will try OnMessageDelivered when ready, in the topTaskContext :
    //<It is THE way to send message from another context and to exploit it
    //<in local context.
    //<Thread safe : Call from whatever.
    //<Tips : Use TopTaskMessageHelper to build String or stream message.
    Procedure DeliverMessage(var aNewMessage : TopTaskMessage); Virtual;

    //<Override OnMessageDelivered to process message accordingly.
    //<Tips : Use TopTaskMessageHelper to scan for String or stream message.
    //<Not thread safe : It will be executed only in the topTask thread context.
    Procedure OnMessageDelivered(var aNewMessaqe : TopTaskMessage); Virtual;

    //<DoTaskProgress(String) :
    //<Call that to ignitiate a OnProgress event on TaskManager's side.
    //<It is developper responsability to localize rightly the call in RunLoop.
    Procedure DoTaskProgress(aProgressString : String); Overload; Virtual;

    //<DoTaskProgress(Stream) :
    //<Call that to ignitiate a OnProgress event on TaskManager's side.
    //<It is developper responsability to localize rightly the call in RunLoop.
    Procedure DoTaskProgress(aProgressStream : TMemoryStream); Overload; Virtual;

    //<DoTaskInterrupt :
    //<Idem, call that to ignitiate a on interrupt on TaskManager side.
    Procedure DoTaskInterrupt; Virtual;

    //<BroadcastMessage : Send message to all other task of the task manager.
    //<Not thread safe : Call only form this task.
    Procedure BroadcastMessage(var aMessage : TopTaskMessage); Overload; Virtual;

    //<BroadcastMessage : Send message to all other task inherited from TargetTaskClass.
    //<Not thread safe : Call only form this task.
//    Procedure BroadCastMessage(aMessage : TopTaskMessage; TargetTaskClass : TopTaskClass); Overload; Virtual;

  Public

    TaskManager : TTaskManager; //Pointer

    Constructor Create; Reintroduce; Virtual;
    Destructor Destroy; Override;

    Procedure BeforeRun; Virtual;
    Procedure RunLoop; Virtual; Abstract;
    Procedure AfterRun; Virtual;

    {$IFDEF FPC}
        Function Started : Boolean;
    {$ELSE}
    {$ENDIF}

    Procedure EndOfInterrupt;

    Property MessageDeliveryAccepted : Boolean read GetMessageDeliveryAccepted;

    Property LastProgressString : String read GetProgress;
    Property BreathAllowed : Boolean read GetBreathAllowed Write SetBreathAllowed;
    Property BreathAllowedEveryNCycle : Boolean read GetBreathAllowedEveryNCycle Write SetBreathAllowedEveryNCycle;
    Property BreathEveryNCycle : NativeUInt read GetBreathEveryNCycle Write SetBreathEveryNCycle;
    Property BreathEveryProgressCall : Boolean read GetBreathEveryProgressCall Write SetBreathEveryProgressCall;
    Property BreathDurationInMilliSec : NativeUInt read GetBreathDuration Write SetBreathDuration;
    Property TotalBreathTime : NativeUInt read GetTotalBreathTime;
    Property TotalActiveTime : NativeUInt read GetTotalActiveTime;
//    Property TotalLoopTime : NativeUInt Read GetTotalLoopTime;
//    Property TotalEventTime : NativeUInt read GetTotalEventTime;
    Property DateTimeBegin : TDateTime read GetDateTimeBegin;
    Property DateTimeFinish : TDateTime read GetDateTimeFinish;
    Property TaskStatus : TopTaskStatus read GetTaskStatus;
    Property TaskStatusAsString : String read GetTaskStatusAsString;
    Property LoopCount : Int64 read GetLoopCount;
    Property LoopAverageDurationInMilliSec : Single read GetLoopAverageDurationInMilliSec;
    Property TotalMessageInQueue : NativeUInt read GetMessageInQueue;
    Property TotalMessageDelivered : NativeUInt read GetMessageDelivered;
    Property TimeFromLastLoopInMilliSec : Int64 read FTickCountFromLastLoop;
  End;


  TNotifyTaskException = Procedure(Sender : TObject; Task : TopTask; E : Exception) Of Object;
  TNotifyTask = Procedure(Sender : TObject; Task : TopTask) Of Object;
  TNotifyTaskProgress = Procedure(Sender : TObject; Task : TopTask; aProgressString : String) Of Object;
  TNotifyTaskProgressData = Procedure(Sender : TObject; Task : TopTask; aProgressStream : TMemoryStream) Of Object;

  { TTaskManager }

  TTaskManager = class
  Private
    FKillInProgress : TProtectedBoolean;

    //Those two msust remain synchrone !
    FTaskList : TThreadList;
    FTaskMessageSubList : TThreadList;

    FTaskCount : NativeUInt;
    FTaskCountActive : NativeUInt;
    FTaskCountFinish : NativeUInt;

    FOnTaskNotifyException : TNotifyTaskException;
    FOnTaskNotifyBegin: TNotifyTask;
    FOnTaskNotifyFinish: TNotifyTask;
    FOnTaskProgress: TNotifyTaskProgress;
    FOnTaskProgressData: TNotifyTaskProgressData;
    FOnTaskNotifyInterrupt: TNotifyTask;

    FCSTaskActivity : TCriticalSection;


    //Event proc. Althought it call Synchro, we DO NOT call synchro. It's done
    //by TaskManager.processMessages;
    Procedure DoSynchroException(Task : TopTask; E : Exception);
    Procedure DoSynchroTaskBegin(Task : TopTask);
    Procedure DoSynchroTaskProgress(Task : TopTask; aProgressString : String);
    Procedure DoSynchroTaskProgressData(Task : TopTask; aProgressStream : TMemoryStream);
    Procedure DoSynchroTaskFinish(Task : TopTask);
    Procedure DoSynchroTaskInterrupt(Task : TopTask);

    //<Called by Task ! Keep by double CS : One on TaskManager level, the other on tasklevel.
    Procedure TaskSendMessage(aTask : TopTask; var aMessage: TopTaskMessage); Overload;
//    Procedure TaskSendMessage(aTask : TopTask; aMessage: TopTaskMessage; TargetTask : TopTaskClass); Overload;

    Procedure InternalTerminate(aTask : TopTask);
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Function CreateTask( aTaskClass : TopTaskClass;
                          Const ImmediateRun : Boolean = False;
                          Const LetBreath : Boolean = True) : TopTask;
    Function AddTask( aTask: TopTask;
                          Const ImmediateRun : Boolean = False;
                          Const LetBreath : Boolean = True) : TopTask;
    Function StartTask(Index : Integer) : Boolean;
    Function KillTask(Index : Integer) : Boolean;
    Function KillAndRemoveTask(Index : Integer) : Boolean;

    Procedure ClearTasks; //Call Kill task and clean all list : Ready to reuse.
    Procedure KillAll;
    Procedure StartAll;
    Procedure WaitForAll;

    Function TaskCount : Integer;
    Function TaskCountActive : Integer;
    Function TaskCountFinish : Integer;

    Procedure TaskReport(var aStringList : TStringList);


    //Get the entire log of Internal message.
    // It return the complete log between 2 Log() call.
    // If ProcessEvent Param, it will fired accordingly the OnTaskxxx below, for the correponding log.
    //Result log belongs to the main thread : Do not forget to delete it.
    Function Log(Const ProcessEvent : Boolean = True) : TopTaskLog;

//    {$IFNDEF FPC}
//    Function Lock : TList<TopTask>;
//    Procedure Unlock;
//    {$ENDIF}
    Function TaskListLock : TList;
    Procedure TaskListUnLock;

    //Send message to all task.
    Procedure SendMessage(aMessage : TOpTaskMessage);

    //Event Handler : To perform the event call, you have to call TaskManager.ProcessMessage.
    Property OnTaskBegin : TNotifyTask read FOnTaskNotifyBegin Write FOnTaskNotifyBegin;
    Property OnTaskFinish : TNotifyTask read FOnTaskNotifyFinish Write FOnTaskNotifyFinish;
    Property OnTaskException : TNotifyTaskException read FOnTaskNotifyException Write FOnTaskNotifyException;

    //Call "DoTaskProgress(aString) in your RunLoop code to call this event on taskmanager level.
    //DoTaksProgress will perfom :
    //  - a "breath" if you permit it (Parameter on thread creation MyTask.BreathOnProgress)
    //    Warning : use BreathOnProgress in GUI oriented task, it is time consuming in thread level.
    //  - A terminated test : It will finalyze your thread more quickly if needed.
    Property OnTaskProgress : TNotifyTaskProgress read FOnTaskProgress Write FOnTaskProgress;

    //Call "DoTaskProgress(aStream) in your RunLoop code to call this event on taskmanager level.
    //DoTaksProgress will perfom :
    //  - Pass to TaskManager a data Stream.
    //  - a "breath" if you permit it (Parameter on thread creation MyTask.BreathOnProgress)
    //    Warning : use BreathOnProgress in GUI oriented task, it is time consuming in thread level.
    //  - A terminated test : It will finalyze your thread more quickly if needed.
    Property OnTaskProgressData : TNotifyTaskProgressData read FOnTaskProgressData Write FOnTaskProgressData;

    //Call "DoTaskInterupt" in your RunLoop code to call This event on taskmanager level.
    //DoTaskInterrupt will perfom apparently the same code than a DoTaskProgress, but in
    //this case, the thread will WAIT the reading of this message from the TaskManager context !
    //In conserquence, the thread shall be stoped during the message processing.
    //This event must be used for these case : In order to get or manipulate data directly in task.
    //  Given the fact that thread il stoped, all data is secured to exploit in the TaskMAnager.OnInterrupt context ONLY.
    //  (To be entirely clean, you have to protect thread data access by a Critical section. Or uset TProtectedValue system.)
    Property OnTaskInterrupt : TNotifyTask read FOnTaskNotifyInterrupt Write FOnTaskNotifyInterrupt;


  end;

  //----------------------------------------------
  TTaskTest = Class(TopTask)
  private
    i : Integer;
  Public
    Procedure BeforeRun; Override;
    Procedure RunLoop; Override;
  End;


implementation

{ TopTask }


procedure TopTask.AfterRun;
begin
  //General purpose after run (define var)
end;

procedure TopTask.BeforeRun;
begin
  //General purpose before run (define var)
end;

{
procedure TopTask.BroadcastMessage(aMessage: TopTaskMessage;
  TargetTaskClass: TopTaskClass);
begin
  if Terminated then
    Exit;
  FTaskStatus.Value := optOnSendMessage;
  TaskManager.TaskSendMessage(Self,aMessage,TargetTaskClass);
end;
}
procedure TopTask.CheckRun;
begin
  FInternalCounter.Inc;
  FTickCountFromLastLoop := (GetTickCount - FTickCountLast);
  FTickCountLast := GetTickCount;
  FTickCountEachLoopAverage.Value := FTickCountFromLastLoop / FInternalCounter.Value;

  InternalBreathManagement;
  //if FInternalCounter mod 1000 = 0 then
  //begin
  //end;

    //TODO : Calculate here the speed of the loop.
    // - If the loop is very low, or locked ?
    // - If the loop is fast, and if a progress have not been reported from a long time, we cas send report every second ?

    //TODO : Alive metrics...
end;

constructor TopTask.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FLastProgress := TProtectedString.Create('');
  FInternalCounter := TProtectedInt64.Create(0);
  FDateTimeBegin := TProtectedDateTime.Create(Now);
  FDateTimeEnd := TProtectedDateTime.Create(0);
  FTickCountEachLoopAverage := TProtectedDouble.Create(-1.0);
  FTaskStatus := TProtectedTaskStatus.Create(optWaitToStart);
  FBreathAllowed := TProtectedBoolean.Create(True);
  FBreathDuration := TProtectedNativeUInt.Create(1); //1 Millisec.
  FBreathEveryNCycleAllowed := TProtectedBoolean.Create(True);
  FBreathEveryProgressCall := TProtectedBoolean.Create(True);
  FMessageDeliveryAccepted := TProtectedBoolean.Create(False); //Pass to true if ProcessMessage is called (by user) in RunLoop.
  FBreathEveryNCycle := TProtectedNativeUInt.Create(1000);
  FTotalBreathTime := TProtectedNativeUInt.Create(0);
  FMessageInQueue := TProtectedNativeUInt.Create(0);
  FMessageDelivered := TProtectedNativeUInt.Create(0);
  FMessageInterruptFlag := TEvent.Create(nil,False,False,EmptyStr);
  FInternalMessageLog := TopTaskLog.Create;
  FExternalMessageSystem := TopTaskMessagingSystem.create;
  FExternalMessageSystem.Master := self;

  OnTerminate := InternalThreadTerminate;
end;

procedure TopTask.DeliverMessage(var aNewMessage: TopTaskMessage);
begin
  FExternalMessageSystem.DeliverMeMessage(aNewMessage);
end;

destructor TopTask.Destroy;
begin
  EndOfInterrupt;
  if (Started) And Not(FTaskStatus.Value in [optFinish, optTerminated, optException]) then
    WaitFor;
  FInternalMessageLog.Clean;
  FExternalMessageSystem.Free;

  FInternalMessageLog.Free;
  FLastProgress.Free;
  FInternalCounter.Free;
  FMessageInQueue.Free;
  FDateTimeBegin.Free;
  FDateTimeEnd.Free;
  FBreathAllowed.Free;
  FBreathDuration.Free;
  FBreathEveryNCycle.Free;
  FTotalBreathTime.Free;
  FTickCountEachLoopAverage.Free;
  FBreathEveryNCycleAllowed.Free;
  FBreathEveryProgressCall.Free;
  FMessageDelivered.Free;
  FMessageDeliveryAccepted.Free;
  FTaskStatus.Free;
  FMessageInterruptFlag.Free;
  inherited;
end;

procedure TopTask.DoProcessMessage(Const WaitForMessage : Boolean = False);
begin
  FMessageDeliveryAccepted.Value := True;
  if Terminated then
    Exit;
  if Not(FExternalMessageSystem.ProcessMessage(WaitForMessage)) Then
  begin
    Terminate;
  end;
end;

procedure TopTask.BroadCastMessage(var aMessage: TopTaskMessage);
begin
  if Terminated then
    Exit;
  FTaskStatus.Value := optOnSendMessage;
  TaskManager.TaskSendMessage(Self,aMessage);
end;

procedure TopTask.InternalDoTaskBegin;
begin
  FTaskStatus.Value := optbegin;
  FInternalMessageLog.AddEmptyMessage(optbegin);
end;

procedure TopTask.InternalDoTaskException(E : Exception);
begin
  FTaskStatus.Value := optException;
  FInternalMessageLog.AddStringMessage(optException,E.Message);
  //FInternalMessageLog.AddEmptyMessage(optException); //TODO : AddExceptionMessage.
end;

procedure TopTask.InternalDoTaskFinish;
begin
  FTaskStatus.Value := optfinish;
  FInternalMessageLog.AddEmptyMessage(optfinish);
end;

procedure TopTask.DoTaskInterrupt;
begin
  if Terminated then
    Exit;
  InternalDoTaskInterrupt;
  InternalBreathManagement;
end;

procedure TopTask.DoTaskProgress(aProgressStream: TMemoryStream);
begin
  if Terminated then
    Exit;
  InternalDoTaskProgress(aProgressStream);
  InternalBreathManagement;
end;

procedure TopTask.DoTaskProgress(aProgressString: String);
begin
  if Terminated then
    Exit;
  InternalDoTaskProgress(aProgressString);
  InternalBreathManagement;
end;
{$IFDEF FPC}
Function TopTask.Started : Boolean;
begin
  Result := not(Suspended);
end;

{$ELSE}
{$ENDIF}

procedure TopTask.EndOfInterrupt;
begin
  FMessageInterruptFlag.SetEvent;
end;

procedure TopTask.Execute;
begin
  FDateTimeBegin.Value := Now;
  FDateTimeEnd.Value := 0;
  FTickCountBegin := GetTickCount;
  FTickCountFinish := -1;
  InternalDoTaskBegin;
  try
    try
      FInternalCounter.Value := 0;
      BeforeRun;
      While Not(Terminated) do
      begin
        FTaskStatus.Value := optLoop;
        //Those "If" is not useless : Each step could be long, and status could change every time.
        if Not(Terminated) then
          CheckRun;
        if Not(Terminated) then
          RunLoop;
      end;
      AfterRun;
    Except
      HandleException;
    end;
    InternalDoTaskFinish;
  finally
    FTickCountFinish := GetTickCount;
    FDateTimeEnd.Value := Now;
    FTaskStatus.Value := optTerminated;
  end;
end;

function TopTask.GetBreathDuration: NativeUInt;
begin
  Result := FBreathDuration.Value;
end;

function TopTask.GetBreathEveryNCycle: NativeUInt;
begin
  Result := FBreathEveryNCycle.Value;
end;

function TopTask.GetBreathEveryProgressCall: Boolean;
begin
  Result := FBreathEveryProgressCall.Value;
end;

function TopTask.GetBreathAllowed: Boolean;
begin
  Result := FBreathAllowed.Value;
end;

function TopTask.GetBreathAllowedEveryNCycle: Boolean;
begin
  Result := FBreathEveryNCycleAllowed.Value;
end;

function TopTask.GetDateTimeBegin: TDateTime;
begin
  Result := FDateTimeBegin.Value;
end;

function TopTask.GetDateTimeFinish: TDateTime;
begin
  Result := FDateTimeEnd.Value;
end;

function TopTask.GetLoopAverageDurationInMilliSec: Single;
begin
  Result := FTickCountEachLoopAverage.Value;
end;

function TopTask.GetLoopCount: Int64;
begin
  Result := FInternalCounter.Value;
end;

function TopTask.GetMessageDelivered: NativeUInt;
begin
  result := FMessageDelivered.Value;
end;

function TopTask.GetMessageDeliveryAccepted: Boolean;
begin
  Result := FMessageDeliveryAccepted.Value;
End;

function TopTask.GetMessageInQueue: NativeUInt;
begin
  Result := FMessageInQueue.Value;
end;

function TopTask.GetProgress: String;
begin
  Result := FLastProgress.Value;
end;

function TopTask.GetTaskStatus: TopTaskStatus;
begin
  Result := FTaskStatus.Value;
end;

function TopTask.GetTaskStatusAsString: String;
var a : TopTaskStatus;
begin
  a := FTaskStatus.Value;
  Result := TopTaskStatusStr[Ord(a)];
end;

function TopTask.GetTotalActiveTime: NativeUInt;
var a, b : TDateTime;
begin
  Result := 0;
  a := FDateTimeBegin.Value;
  b := FDateTimeEnd.Value;
  if a<b then
    Result := Trunc( (b - a) * (24*3600*1000) );
end;

function TopTask.GetTotalBreathTime: NativeUInt;
begin
  Result := FTotalBreathTime.Value;
end;

procedure TopTask.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    InternalDoTaskException(FException);
  finally
  end;
end;

procedure TopTask.InternalBreathManagement;
var bBreath : Boolean;
begin
  if FBreathAllowed.Value then //Breath  : Let a chance to other thread (GUI above all) to work.
  begin
    bBreath := false;
    if FBreathEveryNCycleAllowed.Value then
      if FInternalCounter.Value mod FBreathEveryNCycle.Value = 0 Then
      begin
        bBreath := True;
      end;

    if FTaskStatus.Value = optOnProgress then
      if FBreathEveryProgressCall.Value then
        bBreath := True;

    if bBreath then
    begin
      Sleep(FBreathDuration.Value);
      FTotalBreathTime.Add(FBreathDuration.Value);
    end;
  end;
end;


procedure TopTask.InternalDoTaskInterrupt;
begin
  FTaskStatus.Value := optOnInterrupt;
  FMessageInterruptFlag.ResetEvent;
  FInternalMessageLog.AddEmptyMessage(optOnInterrupt);
  While not(terminated) do
  begin
    case FMessageInterruptFlag.WaitFor(CST_WAITFOR_TIMEOUT) of
      wrSignaled :
      begin
        Exit;
      end;
    end;
  end;
end;

procedure TopTask.InternalDoTaskProgress(aProgressStream: TMemoryStream);
begin
  FTaskStatus.Value := optOnProgress;
  FInternalMessageLog.AddStreamMessage(optOnProgress,aProgressStream);
end;

procedure TopTask.InternalThreadTerminate(SEnder: TObject);
begin
  if Assigned(FatalException) then
  begin
    if FatalException is Exception then
    begin
      raise Exception.Create(ClassName + ' : '+Exception(FatalException).Message);
    end;
  end;
end;

procedure TopTask.InternalDoTaskProgress(aProgressString : String);
begin
  FTaskStatus.Value := optOnProgress;
  FLastProgress.Value := aProgressString;
  FInternalMessageLog.AddStringMessage(optOnProgress,aProgressString);
end;

procedure TopTask.OnMessageDelivered(var aNewMessaqe: TopTaskMessage);
begin
  raise Exception.Create('OnMessageDelivered : You must override this method to define its behaviour.');
end;

procedure TopTask.SetBreathDuration(const Value: NativeUInt);
begin
  if Value = 0 then
    FBreathDuration.Value := 1
  else
    FBreathDuration.Value := Value;
end;

procedure TopTask.SetBreathEveryNCycle(const Value: NativeUInt);
begin
  if Value = 0 then
    FBreathEveryNCycle.Value := 1
  else
    FBreathEveryNCycle.Value := Value;
end;

procedure TopTask.SetBreathEveryProgressCall(const Value: Boolean);
begin
  FBreathEveryProgressCall.Value := Value;
end;

procedure TopTask.SetBreathAllowed(const Value: Boolean);
begin
  FBreathAllowed.Value := Value;
end;

procedure TopTask.SetBreathAllowedEveryNCycle(const Value: Boolean);
begin
  FBreathEveryNCycleAllowed.Value := Value;
end;

{ TTaskManager }

function TTaskManager.AddTask(aTask: TopTask; const ImmediateRun,
  LetBreath: Boolean): TopTask;
var a : TopTask;
    l : TList;
begin
  Assert(Assigned(aTask));
  a := aTask;
  Result := a;
//  Result.Priority := TThreadPriority.tpTimeCritical;
//  SetThreadAffinityMask(a.Handle,TaskCount);  //20160215 : System do it Well. Do not do this.

  a.TaskManager := Self;
  a.BreathAllowed := LetBreath;

  l := FTaskList.LockList;
  try
    l.Add(a);
    inc(FTaskCount);
  finally
    FTaskList.UnlockList;
  end;

  //Second list of thread : Avoid lock when Task sendmessage BEFORE entiring the BeginLoop
  //(Exemple : OnCreate of a derivate TopTask.
  l := FTaskMessageSubList.LockList;
  try
    l.Add(a);
  finally
    FTaskMessageSubList.UnlockList;
  end;

  if ImmediateRun then
    a.Start;
end;

procedure TTaskManager.ClearTasks;
var i : Integer;
    l : TList;
begin
  KillAll;
  l := FTaskList.LockList;
  try
    for I := 0 to l.Count-1 do
    begin
      TopTask(l[i]).Free;
    end;
  finally
    FTaskList.UnlockList;
  end;
  FTaskList.Clear;
  FTaskMessageSubList.Clear;
end;

constructor TTaskManager.Create;
begin
  Inherited Create;
  FTaskList := TThreadList.Create;
  FTaskMessageSubList := TThreadList.Create;
  FKillInProgress := TProtectedBoolean.Create(False);
  FCSTaskActivity := TCriticalSection.Create;
  FTaskCount := 0;
  FTaskCountActive := 0;
  FTaskCountFinish := 0;
end;

function TTaskManager.CreateTask(aTaskClass: TopTaskClass;
  const ImmediateRun: Boolean; const LetBreath: Boolean): TopTask;
begin
  Result := AddTask(aTaskClass.Create,ImmediateRun,LetBreath);
end;

destructor TTaskManager.Destroy;
begin
  ClearTasks;
  FreeAndNil(FTaskList);
  FreeAndNil(FTaskMessageSubList);
  FreeAndNil(FCSTaskActivity);
  FreeAndNil(FKillInProgress);
  inherited;
end;

procedure TTaskManager.DoSynchroException(Task : TopTask; E : Exception);
begin
  if Assigned(FOnTaskNotifyBegin) then
  begin
    FOnTaskNotifyException(Self,Task, E);
  end;
end;

procedure TTaskManager.DoSynchroTaskBegin(Task : TopTask);
begin
  inc(FTaskCountActive);
  if Assigned(FOnTaskNotifyBegin) then
  begin
    FOnTaskNotifyBegin(Self,Task);
  end;
end;

procedure TTaskManager.DoSynchroTaskFinish(Task : TopTask);
begin
  Dec(FTaskCountActive);
  Inc(FTaskCountFinish);
  if Assigned(FOnTaskNotifyFinish) then
  begin
    FOnTaskNotifyFinish(Self,Task);
  end;
end;

procedure TTaskManager.DoSynchroTaskInterrupt(Task: TopTask);
begin
  if Assigned(FOnTaskNotifyInterrupt) then
  begin
    FOnTaskNotifyInterrupt(Self,Task);
  end;
end;

procedure TTaskManager.DoSynchroTaskProgress( Task : TopTask;
                                              aProgressString : String);
begin
  if Assigned(FOnTaskProgress) then
  begin
    FOnTaskProgress(Self,Task,aProgressString);
  end;
end;

procedure TTaskManager.DoSynchroTaskProgressData(Task: TopTask;
  aProgressStream: TMemoryStream);
begin
  if Assigned(FOnTaskProgressData) then
  begin
    FOnTaskProgressData(Self,Task,aProgressStream);
  end;
end;

procedure TTaskManager.InternalTerminate(aTask: TopTask);
begin
  Assert(Assigned(aTask));
  aTask.Terminate;
  aTask.EndOfInterrupt;
  aTask.Sleep(CST_SLEEP_RETAIN_MODE);
  if not(aTask.TaskStatus in [optfinish,optTerminated,optException]) then
    aTask.WaitFor;
end;

procedure TTaskManager.KillAll;
var a : TopTask;
    l : TList;
    i : Integer;
begin
  if FKillInProgress.Value then
    Exit;
  try
    FKillInProgress.Value := true;
    l := FTaskList.LockList;
    try
      for I := 0 to l.Count-1 do
      begin
        a := TopTask(l[i]);
        InternalTerminate(a);
      end;
    finally
      FTaskList.UnlockList;
    end;
  finally
    FKillInProgress.Value := False;
  end;
end;

function TTaskManager.KillAndRemoveTask(Index: Integer): Boolean;
var a : TopTask;
    l : TList;
begin
  KillTask(Index);

  l := FTaskList.LockList;
  try
    a := TopTask(l[Index]);
    l.Remove(a);
  finally
    FTaskList.UnlockList;
  end;

  l := FTaskMessageSubList.LockList;
  try
    a := TopTask(l[Index]);
    l.Remove(a);
  finally
    FTaskMessageSubList.UnlockList;
  end;

  FreeAndNil(a);

  Result := True;
end;

function TTaskManager.KillTask(Index: Integer): Boolean;
var a : TopTask;
    l : TList;
begin
  Result := False;
  l := FTaskList.LockList;
  try
    if Index<l.Count then
    begin
      a := TopTask(l[Index]);
      if a.Started then
      begin
        if Not(a.Terminated) then
        begin
          InternalTerminate(a);
        end;
      end;
      Result := True;
    end;
  finally
    FTaskList.UnlockList;
  end;
end;

function TTaskManager.TaskListLock: TList;
begin
  Result := FTaskList.LockList;
  FTaskMessageSubList.LockList;
end;

procedure TTaskManager.TaskListUnlock;
begin
  FTaskList.UnlockList;
  FTaskMessageSubList.UnlockList;
end;


function TTaskManager.Log(const ProcessEvent: Boolean): TopTaskLog;
var
    ll : TList;
    al,l : TList<pTopInternalTaskLog>;
    p : pTopInternalTaskLog;
    I,J :  Integer;
    streamtemp : TMemoryStream;
begin
  Result := TopTaskLog.Create;

  al := Result.Lock;
  try

    //Deep copy. (To fast release the thread work).
    ll := FTaskMessageSubList.LockList;
    try
      for I := 0 to ll.Count-1 do
      begin
        l := TopTask(ll[i]).FInternalMessageLog.Lock;
        try
          for J := 0 to l.Count-1 do
          begin
            l[j].TagObject := ll[i];
            al.Add(l[j]);
          end;
          l.Clear; //And not cleaN ! record is now in our local list.
        finally
          TopTask(ll[i]).FInternalMessageLog.UnLock;
        end;
      end;
    finally
      FTaskMessageSubList.UnlockList;
    end;

    if ProcessEvent then
    begin
      //Throught LOCAL list. (Free time for thread, no critical section here.)
      for I := 0 to al.Count-1 do
      begin
        p := al[i];
        case p.TaskStatusSnapShot of
          optbegin :
          begin
            DoSynchroTaskBegin(TopTask(p.TagObject));
          end;
          optOnProgress :
          begin
            Case p.MessageType of
              itmString :
              begin
                DoSynchroTaskProgress(TopTask(p.TagObject),StringOf(p.Buffer));
              end;
              itmStream :
              begin
                streamtemp := TMemoryStream.Create;
                try
                  streamtemp.Write(p.Buffer[0],Length(p.Buffer));
                  streamtemp.Position := 0;
                  DoSynchroTaskProgressData(TopTask(p.TagObject),streamtemp);
                finally
                  FreeAndNil(streamtemp);
                end;
              end;
              itmCoded : ;
            End;
          end;
          optOnInterrupt :
          begin
            DoSynchroTaskInterrupt(TopTask(p.TagObject));
          end;
          optfinish :
          begin
            DoSynchroTaskFinish(TopTask(p.TagObject));
          end;
          optException :
          begin
            DoSynchroException(TopTask(p.TagObject),Exception.Create(Stringof(p.Buffer)));
          end;
        end;
      end;

      //Dispose(p); //Disposed by Result.free. (Must be done by user.)
    end;
  finally
    Result.UnLock;
  end;
end;

procedure TTaskManager.SendMessage(aMessage: TOpTaskMessage);
var
    l : TList;
    i : integer;
begin
  FCSTaskActivity.Enter;
  try

  if FKillInProgress.Value then
    Exit;

  l := FTaskMessageSubList.LockList;
  try
    for I := 0 to l.Count-1 do
    begin
      if TopTask(l[i]).MessageDeliveryAccepted then
      begin
        TopTask(l[i]).DeliverMessage(aMessage);
      end;
    end;
  finally
    FTaskMessageSubList.UnlockList;
  end;

  finally
    FCSTaskActivity.Leave;
  end;
end;

procedure TTaskManager.StartAll;
var a : TopTask;
    l : TList;
    I : integer;
begin
  l := FTaskList.LockList;
  try
    for I := 0 to l.Count-1 do
    begin
      a := TopTask(l[i]);
      if Not(a.Started) then
      begin
        a.Start;
      end;
    end;
  finally
    FTaskList.UnlockList;
  end;
end;

function TTaskManager.StartTask(Index: Integer): Boolean;
var a : TopTask;
    l : TList;
begin
  Result := False;
  l := FTaskList.LockList;
  try
    if Index<l.Count then
    begin
      a := TopTask(l[Index]);
      if Not(a.Started) then
      begin
        Result := True;
        a.Start;
      end;
    end;
  finally
    FTaskList.UnlockList;
  end;
end;

function TTaskManager.TaskCount: Integer;
begin
  Result := FTaskCount;
end;
function TTaskManager.TaskCountActive: Integer;
begin
  Result := FTaskCountActive;
end;

function TTaskManager.TaskCountFinish: Integer;
begin
  Result := FTaskCountFinish;
end;

procedure TTaskManager.TaskReport( var aStringList: TStringList);
var a : TopTask;
    l : TList;
    i : integer;
    s : String;
begin
  if not(Assigned(aStringList)) then
    aStringList := TStringList.Create;
  aStringList.Clear;

  s := ';';
  l := FTaskList.LockList;
  try
    aStringList.Add( 'ThreadID;Terminated;Started;StartOn;FinishedOn;' +
                     'LoopCount;LoopAverageDurationInMilliSec;' +
                     'BreathAllowed;BreathTimeConsumingInMilliSec;' +
                     'TotalActiveTimeInMilliSec;MessageCountPending;' +
                     'MessageCountDelivered');
    For i := 0 to l.Count-1 do
    begin
      a := TopTask(l[i]);
      aStringList.Add(
                       IntToStr(a.ThreadID)+s+InttoStr(Integer(a.Terminated))+s+
                       IntToStr(Integer(a.Started))+s+DateTimeToStr(a.DateTimeBegin)+s+
                       DateTimeToStr(a.DateTimeFinish)+s+IntToStr(a.LoopCount)+s+
                       FloatToStr(a.LoopAverageDurationInMilliSec)+s+
                       IntToStr(Integer(a.BreathAllowed))+s+
                       IntToStr(a.TotalBreathTime)+s+
                       IntToStr(a.TotalActiveTime)+s+
                       IntToStr(a.TotalMessageInQueue)+s+
                       IntToStr(a.TotalMessageDelivered)
                     );
    end;
  finally
    FTaskList.UnlockList;
  end;
end;


procedure TTaskManager.TaskSendMessage(aTask: TopTask;
  var aMessage: TopTaskMessage);
var
    l : TList;
    i : integer;
begin
  FCSTaskActivity.Enter;
  try

  if FKillInProgress.Value then
    Exit;

  l := FTaskMessageSubList.LockList;
  try
    for I := 0 to l.Count-1 do
    begin
      if aTask <> TopTask(l[i]) then
      begin
        if Not(TopTask(l[i]).Terminated) then
        begin
          if TopTask(l[i]).MessageDeliveryAccepted then
          begin
            TopTask(l[i]).DeliverMessage(aMessage);
          end;
        end;
      end;
    end;
  finally
    FTaskMessageSubList.UnlockList;
  end;


  finally
    FCSTaskActivity.Leave;
  end;
end;


procedure TTaskManager.WaitForAll;
var a : TopTask;
    l : TList;
    I : integer;
begin
  l := FTaskList.LockList;
  try

    for I := 0 to l.Count-1 do
    begin
      a := TopTask(l[i]);
      a.WaitFor;
    end;
  finally
    FTaskList.UnlockList;
  end;
end;

{ TTaskTest }

procedure TTaskTest.BeforeRun;
begin
  inherited;
  i := 0;
end;

procedure TTaskTest.RunLoop;
begin
  //We are already in a loop ! Just put your code.
  //- The var must be defined in the thread. (Or put static)
  //- Put initialization in BeforeLoop.
  //- You are included in a loop While Not(Terminated) :
  //  you can call terminate to go outside the loop, as you do usualy.

  Inc(i); //Here is our business task :)

  if LoopCount>=2000000 then
  begin
    //End of the thread after 2 millions of do-loop cycle.
    Terminate;
  end;

  if LoopCount mod 1000 = 0 then
  begin
    DoProcessMessage;
    //Every 1000 cycle, we call a progress.
    //If you put "BreathEveryProgressCall, a Breath will be performed inside.
    DoTaskProgress('Hello '+IntToStr(i));
  end;
end;


{ TopTaskMessageList }

procedure TopTaskMessageList.Clean;
var i : Integer;
    s : pTopTaskMessage;
begin
  FCS.Enter;
  try
    for I := 0 to FL.Count-1 do
    begin
      s := FL[i];
      Dispose(s);
    end;
    FL.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TopTaskMessageList.Create;
begin
  inherited;
  FL :=  TList<pTopTaskMessage>.Create;
  FCS := TCriticalSection.Create;
end;

destructor TopTaskMessageList.Destroy;
begin
  Clean;
  FreeAndNil(FL);
  FreeAndNil(FCS);
  inherited;
end;

function TopTaskMessageList.Lock: TList<pTopTaskMessage>;
begin
  FCS.Enter;
  Result := FL;
end;

procedure TopTaskMessageList.UnLock;
begin
  FCS.Leave;
end;


{ TopTaskLog }

procedure TopTaskLog.AddEmptyMessage(
  aTaskStatus: TopTaskStatus);
var a : pTopInternalTaskLog;
begin
  new(a);
  a.MessageCreation := Now;
  a.TaskStatusSnapShot := aTaskStatus;
  a.MessageType := TopInternalTaskLogType.itmCoded;
  FCS.Enter;
  try
    FL.Add(a);
  finally
    FCS.Leave;
  end;
end;

procedure TopTaskLog.AddStreamMessage(aTaskStatus: TopTaskStatus;
  aMessageStream: TMemoryStream);
var a : pTopInternalTaskLog;
begin
  Assert(Assigned(aMessageStream));
  new(a);
  a.MessageCreation := Now;
  a.TaskStatusSnapShot := aTaskStatus;
  a.MessageType := TopInternalTaskLogType.itmStream;
  SetLength(a.Buffer,aMessageStream.Size);
  aMessageStream.Position:=0;
  aMessageStream.Read(a.Buffer[0],aMessageStream.Size);
  FCS.Enter;
  try
    FL.Add(a);
  finally
    FCS.Leave;
  end;
end;

procedure TopTaskLog.AddStringMessage( aTaskStatus : TopTaskStatus;
                                                          aMessageString: String);
var a : pTopInternalTaskLog;
begin
  new(a);
  a.MessageCreation := Now;
  a.TaskStatusSnapShot := aTaskStatus;
  a.MessageType := TopInternalTaskLogType.itmString;
  a.Buffer := BytesOf(aMessageString);
  FCS.Enter;
  try
    FL.Add(a);
  finally
    FCS.Leave;
  end;
end;

procedure TopTaskLog.Clean;
var i : Integer;
    s : pTopInternalTaskLog;
begin
  FCS.Enter;
  try
    for I := 0 to FL.Count-1 do
    begin
      s := FL[i];
      Dispose(s);
    end;
    FL.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TopTaskLog.Create;
begin
  FL :=  TList<pTopInternalTaskLog>.Create;
  FCS := TCriticalSection.Create;
end;

destructor TopTaskLog.Destroy;
begin
  Clean;
  FreeAndNil(FL);
  FreeAndNil(FCS);
  inherited;
end;

function TopTaskLog.Lock: TList<pTopInternalTaskLog>;
begin
  FCS.Enter;
  Result := FL;
end;

procedure TopTaskLog.UnLock;
begin
  FCS.Leave;
end;

{ TopStandAloneMessagingSystem }

constructor TopStandAloneMessagingSystem.create;
begin
  Inherited;
  FMessageList := TopTaskMessageList.Create;
  FMessageEvent := TEvent.Create(nil,False,False,EmptyStr);
end;

procedure TopStandAloneMessagingSystem.DeliverMeMessage(
  var aNewMessaqe: TopTaskMessage);
var a : pTopTaskMessage;
    l : TList<pTopTaskMessage>;
begin
  l := FMessageList.Lock;
  try
    new(a);
    a^:= aNewMessaqe; //Deep copy.
    l.Add(a);
  finally
    FMessageList.UnLock;
  end;

  FMessageEvent.SetEvent;
end;

destructor TopStandAloneMessagingSystem.Destroy;
begin
  FMessageList.Clean;
  FMessageList.Free;
  FMessageEvent.Free;
  inherited;
end;

procedure TopStandAloneMessagingSystem.OnMessageDelivered(
  var aNewMessage: TopTaskMessage; const MessageRemaining : Cardinal);
begin
  //Override this one to process Message;
end;

procedure TopStandAloneMessagingSystem.OnTimeOut(var StopWait : Boolean);
begin
  //Override this one to update status and so on;
end;

procedure TopStandAloneMessagingSystem.OnWaiting;
begin
  //Override this one to update status and so on;
end;

Function TopStandAloneMessagingSystem.ProcessMessage(const aWait: Boolean) : Boolean;
var aMessage, acp : pTopTaskMessage;
    l : TList<pTopTaskMessage>;
    bTest : Boolean;
    iMessCount : Cardinal;

    Function CheckForMessage : Boolean;
    begin
      l := FMessageList.Lock;
      try
        Result := l.Count>0;
        if Result then
      finally
        FMessageList.UnLock;
      end;
    end;

begin
  Result := true;
  if aWait then
  begin
     OnWaiting;
     while Not(CheckForMessage) do
     begin
       case (FMessageEvent.WaitFor(opTaskInternalMessageTimeOutInMS)) of
         wrSignaled :
         begin
           Break;
         end;
         wrTimeOut :
         begin
           bTest := False;
           OnTimeOut(bTest);
           if bTest then
           begin
             Result := False;
             Exit;
           end;
         end;
         wrAbandoned, wrError :
         begin
           Result := False;
           Exit;
         end;
       end;
     end;
  end;

  if CheckForMessage then
  begin
    l := FMessageList.Lock;
    try
      aMessage := l[0];
      new(acp);
      acp^ := aMessage^; //Deep copy for exec outside the MessageList.lock CS (Allowed reentrance).
      iMessCount := l.Count-1;
      l.Delete(0);
      Dispose(aMessage);
    finally
      FMessageList.UnLock;
    end;

    try
      OnMessageDelivered(acp^,iMessCount);
    finally
      Dispose(acp);
    end;
  end;

end;

{ TopTaskMessagingSystem }

procedure TopTaskMessagingSystem.OnMessageDelivered(
  var aNewMessage: TopTaskMessage; const MessageRemaining : Cardinal);
begin
  Master.OnMessageDelivered(aNewMessage);
  Master.FMessageInQueue.Value := MessageRemaining;
  Master.FMessageDelivered.Inc;
end;

procedure TopTaskMessagingSystem.OnTimeOut(var StopWait : Boolean);
begin
  StopWait := Master.Terminated;
end;

procedure TopTaskMessagingSystem.OnWaiting;
begin
  Master.FTaskStatus.Value := topTaskStatus.optOnMessageWait;
end;

{ TopTaskMessageHelper }

function TopTaskMessageHelper.AsMemoryStream: TMemoryStream;
var a : TMemoryStream;
begin
  Result := TMemoryStream.Create;
  case MessageType of
    mtMemoryStream :
    begin
      a := TMemoryStream.Create;
      try
        a.SetSize(Length(Bytes));
        a.WriteBuffer(Bytes[0],Length(Bytes));
        a.Position := 0;
        ReadInt32(a); //opTaskMark (Verified by MessageType)
        ReadByte(a); //mtMemoryStream (Verified by MessageType);
        ReadStream(a,Result);
        Result.Position := 0;
      finally
        a.free;
      end;
    end;
  end;
end;

function TopTaskMessageHelper.AsString : String;
var a : TMemoryStream;
begin
  Result := '';
  case MessageType of
    mtPureString :
    begin
      a := TMemoryStream.Create;
      try
        a.SetSize(Length(Bytes));
        a.WriteBuffer(Bytes[0],Length(Bytes));
        a.Position := 0;
        ReadInt32(a); //opTaskMark (Verified by IsUnderstandable)
        ReadByte(a); //mtPureString (Verified by MessageType);
        Result := ReadString(a);
      finally
        a.Free;
     end;
    end;
  end;
end;

procedure TopTaskMessageHelper.BuidStringMessage(aString: String);
var a : TMemoryStream;
begin
  Self := Default(TopTaskMessage);
  a := TMemoryStream.Create;
  try
    WriteInt32(a,opTaskMark);
    WriteByte(a,Byte(mtPureString));
    WriteString(a,aString);
    SetLength(Bytes,a.Size);
    a.Position := 0;
    a.ReadBuffer(Bytes[0],a.Size);
  finally
    a.Free;
  end;
end;

procedure TopTaskMessageHelper.BuildStreamMessage(
  aStreamMessage: TMemoryStream);
var a : TMemoryStream;
begin
  Self := Default(TopTaskMessage);
  Assert(Assigned(aStreamMessage));
  a := TMemoryStream.Create;
  try
    WriteInt32(a,opTaskMark);
    WriteByte(a,Byte(mtMemoryStream));
    WriteStream(a,aStreamMessage);
    SetLength(Bytes,a.Size);
    a.Position := 0;
    a.ReadBuffer(Bytes[0],a.Size);
  finally
    a.Free;
  end;
end;

function TopTaskMessageHelper.MessageType: TopMessageType;
type pInt32 = ^Int32;
var t : int32;
    m : TopMessageType;
begin
  Result := mtUnkown;
  if Length(Bytes)>SizeOf(Int32) then //at least 5 bytes.
  begin
    t := pInt32(@Bytes[0])^;
    if (t = opTaskMark) then
    begin
      m := TopMessageType(Bytes[4]);
      case m  of
        mtUnkown: ;
        mtPureString: Result := mtPureString;
        mtMemoryStream: Result := mtMemoryStream;
      end;
    end;
  end;
end;


end.
