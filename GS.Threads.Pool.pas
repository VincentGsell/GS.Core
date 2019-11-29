///-------------------------------------------------------------------------------
/// Title      : GS.Threads.Pool
/// Short Desc : Introduce simple and efficient pool capability.
/// Source     : https://github.com/VincentGsell
/// Aim        : - Simple way to submit "task" and execute it ASAP accordingly to
///                a defined resident managed thread pool.
///-------------------------------------------------------------------------------
/// Thread minimalist pool implementation.
/// 1) Create a TStackThreadPool instance. This instance should be resident. It'll wait for TStackTask object.
/// 2) Implement your "task" on a inheritated TStackTask
/// 3) Call YourThreadPool instance as is : YourThreaDPool.Submit(YourTask);
/// 4) Depending yourTask options, YourTask will be delivered by an event or not.
unit GS.Threads.Pool;

{$I GSCore.inc}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  {$IFDEF USE_GENERIC}
  Generics.Collections,
  {$ENDIF}
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  {$IFDEF USE_GENERIC}
  System.Generics.Collections,
  {$ENDIF}
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Common,
  GS.Threads;

Const
  CST_THREAD_POOL_WAIT_DURATION     = 250;  //Millisec.
  CST_DEFAULT_POOL_CAPACITY         = 4;    //thread number by default.
  CST_MAXTHREADCONTINIOUSIDLINGTIME = 1000; //In case of dynamic thread pool, if a thread is idling continously during this time, it will terminate.

Type

  //Make choice...
  //...Overide this to make your task (this one is good for task "Without loop" inside...
  TStackTask = Class
  public
    Procedure Execute(Worker : TThread); Virtual; abstract;
  end;

  TThreadTask = class; //real TThread descendant (Resident "reused" thread)
                       //This may not to be intended to changed, normaly.
  TStackThreadPool = class; //Pool object.

  TThreadTaskStatus = (WaitForStart, Idle, Processing, Terminating);
  TThreadTask = Class(TThread)
  private
    FEventTask : TStackTask;
    FTaskTick : UInt64;
    Procedure InternalDoStackTaskEventStart;
    Procedure InternalDoStackTaskEventFinished;
  protected
    FStatus : TGSProtectedByte;
    FThreadIndex : UInt32;
    FWorkNow : TEvent;
    FThreadPool : TStackThreadPool; //Pointer.
    function GetStatus: TThreadTaskStatus;
  Public
    Constructor Create(aThreadPool : TStackThreadPool); Reintroduce;
    Destructor Destroy; Override;
    Procedure Execute; Override;
    Procedure Run; Virtual;
    Property Status : TThreadTaskStatus read GetStatus;
  End;

  {$IFDEF USE_GENERIC}
  TList_TThreadTask = TList<TThreadTask>;
  TList_TStackTask = TList<TStackTask>;
  {$ELSE}
  TList_TThreadTask = Class(TList_ObjectArray)
  private
    function GetThreadTaskItem(Index: Uint32): TThreadTask;
    procedure SetThreadTaskItem(Index: Uint32; const Value: TThreadTask);
  Public
    Procedure Add(aTask : TThreadTask);
    Property Items[Index : Uint32] : TThreadTask read GetThreadTaskItem Write SetThreadTaskItem; Default;
  End;
  TList_TStackTask = Class(TList_ObjectArray)
  private
    function GetStackTaskItem(Index: Uint32): TStackTask;
    procedure SetStackTaskItem(Index: Uint32; const Value: TStackTask);
  Public
    Procedure Add(aTask : TStackTask);
    Property Items[Index : Uint32] : TStackTask read GetStackTaskItem Write SetStackTaskItem; Default;
  End;
  {$ENDIF}


  TStackTaskEvent = Procedure(Const aThreadIndex : UInt32; aIStackTask : TStackTask; TaskProcessTimeValue : UInt64) of Object;
  ///Named TStackThreadPool mainly because of Delphi's TThreadPool.
  ///
  /// TStackThreadPool : a Thread pool which :
  ///  - Keep a list of task submited.
  ///  - Can free the task after processing, or not (Property FreeTaskOnceProcessed (Default : True))
  ///  - Summon <PoolCapacity> number of thread, no more, no less.
  ///  - Once you have called "Warm", or once your first task submited, all ressource are up and ready.
  ///  - Once ressource up and ready, it is very efficient to take task and process (because threads are up and ready)
  ///  - In delphi, it can replace ITask, it is as efficient as ITask, with more Metrics.
  ///
  TStackThreadPool = class
  private
  protected
    FFreeTaskOnceProcessed: boolean;
    FCurrentStackProtector : TCriticalSection;
    FCurrentStack : TList_TStackTask;
    FSynchoEvent : TGSProtectedBoolean;
    FPoolCapacity: Uint32;
    FOnStackStart: TStackTaskEvent;
    FOnStackFinished: TStackTaskEvent;
    function GetSynchronized: Boolean;
    procedure SetSynchronized(const Value: Boolean);

    function GetPoolCapacity: Uint32; virtual;
    function GetStackTaskCount: Uint32;
    function GetAllThreadAreIdling: Boolean;
    function InternalThreadIdling(alist : TList_TThreadTask) : Boolean;
    function ThreadIdling : Boolean; //Pool protected.

    //On first submit, ressource are allocated. If there are no submit, no thread exists.
    Procedure check; virtual;
    procedure clean; virtual;
    procedure clean_(lt : TList_TThreadTask); virtual; //Unprotected !
  public
    Pool : TGSProtectedObject;
    function LockStack : TList_TStackTask;
    procedure UnlockStack;

    /// aInitialPoolCapacity : Minimal thread count allotated at start.
    ///  Warm : By defautl, thread allocation is done on first task submission.
    ///         If True, allocation are done directly, and thus, ThreadPool is ready to serve.
    Constructor Create(const aInitialPoolCapacity : UInt32 = CST_DEFAULT_POOL_CAPACITY; const Warm : Boolean = False); Reintroduce;
    Destructor Destroy; Override;
    procedure Terminate; //Terminate send terminate at all thread and TStackTask (if they are TStackTaskProc)

    //Procedure GetPoolState : UnicodeString;
    Procedure Submit(aIStackTask : TStackTask); Virtual;

    Property OnTaskStart : TStackTaskEvent read FOnStackStart Write FOnStackStart;
    Property OnTaskFinished : TStackTaskEvent read FOnStackFinished Write FOnStackFinished;

    Property PoolCapacity : Uint32 read GetPoolCapacity;
    Property Synchronized : Boolean read GetSynchronized Write SetSynchronized;
    Property StackTaskCount : Uint32 read GetStackTaskCount;
    Property FreeTaskOnceProcessed : boolean read FFreeTaskOnceProcessed Write FFreeTaskOnceProcessed;

    Property Idle : Boolean read GetAllThreadAreIdling;
  end;

  //Idem than TStackThreadPool, but dynamic thread capacity and management.
  TStackDynamicThreadPool = Class(TStackThreadPool)
  private
    FMaxThreadCount: TGSProtectedUint32;
    FMaxThreadContiniousIdlingTime: TGSProtectedUint32;
  protected
    procedure Check; Override;
    function GetPoolCapacity: Uint32; Override;
  public
    Constructor Create(const Warm : Boolean = False); Reintroduce;
    destructor Destroy; Override;

    Property MaxPoolCapacity : TGSProtectedUint32 read FMaxThreadCount write FMaxThreadCount;
    property MaxThreadContiniousIdlingTime : TGSProtectedUint32 read FMaxThreadContiniousIdlingTime Write FMaxThreadContiniousIdlingTime;
  End;


implementation

{ TStackThreadPool }

procedure TStackThreadPool.Check;
var i : Integer;
    lt : TList_TThreadTask;
begin
  lt := TList_TThreadTask(Pool.Lock);
  try
    if lt.Count=0 then
    begin
      if FPoolCapacity < 1 then
        FPoolCapacity := 1;
      for I := 1 to FPoolCapacity do
      begin
        lt.Add(TThreadTask.Create(Self));
        lt[lt.Count-1].Run;
      end;
    end;
    for I := 0 to lt.Count-1 do
    begin
      if TVisibilityThread(TThread(lt[i])).Started then
        lt[i].Run; //Pulse
    end;
  finally
    Pool.Unlock;
  end;
end;

procedure TStackThreadPool.clean;
var lt : TList_TThreadTask;
begin
  lt := TList_TThreadTask(Pool.Lock);
  try
    clean_(lt);
  finally
    Pool.Unlock;
  end;
end;

procedure TStackThreadPool.clean_(lt: TList_TThreadTask);
var i : integer;
begin
  for i := lt.Count-1 downto 0 do
  begin
    if lt[i].Terminated then
    begin
      lt[i].WaitFor;
      lt[i].Free;
      lt.Delete(i);
    end;
  end;
end;

constructor TStackThreadPool.Create(const aInitialPoolCapacity: UInt32; const Warm : Boolean);
begin
  FSynchoEvent := TGSProtectedBoolean.Create(False);
  FCurrentStackProtector := TCriticalSection.Create;
  FCurrentStack := TList_TStackTask.Create;

  Pool := TGSProtectedObject.Create(TList_TThreadTask.Create);

  FPoolCapacity := aInitialPoolCapacity;
  FFreeTaskOnceProcessed := True;

  if Warm then
    check;
end;

destructor TStackThreadPool.Destroy;
var i : integer;
    lt : TList_TThreadTask;
begin
  FOnStackFinished := Nil;
  FOnStackStart := Nil;

  lt := TList_TThreadTask(Pool.Lock);
  try
    for I := 0 to lt.Count-1 do
    begin
      if TVisibilityThread(TThread(lt[i])).Started then
      begin
        lt[i].Terminate;
        lt[i].Run;
        lt[i].WaitFor;
      end;
      lt[i].Free;
    end;
  finally
    pool.Unlock;
  end;
  FreeAndNil(Pool);
  FreeAndNil(FSynchoEvent);

  //Freeing task not yet executed.
  FCurrentStackProtector.Acquire;
  try
    for I := 0 to FCurrentStack.Count-1 do
    begin
      TObject(FCurrentStack[i]).Free;
    end;
  finally
    FCurrentStackProtector.Release;
  end;
  FreeAndNil(FCurrentStack);
  FreeAndNil(FCurrentStackProtector);
end;

function TStackThreadPool.GetAllThreadAreIdling: Boolean;
begin
  Result := ThreadIdling;
end;

function TStackThreadPool.GetPoolCapacity: Uint32;
begin
  result := FPoolCapacity;
end;

function TStackThreadPool.GetStackTaskCount: Uint32;
begin
  FCurrentStackProtector.Acquire;
  try
    result := FCurrentStack.Count;
  finally
    FCurrentStackProtector.Release;
  end;
end;

function TStackThreadPool.GetSynchronized: Boolean;
begin
  result := FSynchoEvent.Value;
end;

function TStackThreadPool.InternalThreadIdling(
  alist: TList_TThreadTask): Boolean;
var i : integer;
begin
  Result := True;
  for i := 0 to aList.Count-1 do
  begin
    result := result and ((aList[i].Status = TThreadTaskStatus.Idle) Or (aList[i].Terminated));
    if not result then
      Exit;
  end;

  if Result then
    result := result and (StackTaskCount=0);  //All thread are idling *AND* there are no work in queue.
  //--> idle on pool level means, there are *no* work to do currently.
  //--> If we split those 2 notions, it could become hard to use the lib, in practice -> We stuck those 2 notions to make it easier.
end;

function TStackThreadPool.LockStack: TList_TStackTask;
begin
  FCurrentStackProtector.Enter;
  result := FCurrentStack;
end;

procedure TStackThreadPool.SetSynchronized(const Value: Boolean);
begin
  FSynchoEvent.Value := Value;
end;


procedure TStackThreadPool.Submit(aIStackTask: TStackTask);
begin
  FCurrentStackProtector.Acquire;
  try
    FCurrentStack.Add(aIStackTask);
  finally
    FCurrentStackProtector.Release;
  end;
  check;
end;

procedure TStackThreadPool.Terminate;
var lt : TList_TThreadTask;
    i : integer;
begin
  lt := TList_TThreadTask(Pool.Lock);
  try
    for i := 0 to lt.Count-1 do
    begin
      lt[i].Terminate;
    end;
  finally
    Pool.Unlock;
  end;
end;

function TStackThreadPool.ThreadIdling: Boolean;
var lt : TList_TThreadTask;
begin
  result := false;
  if pool.tryLock(lt) then
  try
    result := InternalThreadIdling(lt);
  finally
    Pool.Unlock;
  end;
end;

procedure TStackThreadPool.UnlockStack;
begin
  FCurrentStackProtector.Leave;
end;

{ TThreadTask }

constructor TThreadTask.Create(aThreadPool: TStackThreadPool);
var lt : TList_TThreadTask;
begin
  Inherited Create(true);
  Assert(Assigned(aThreadPool));
  FStatus := TGSProtectedByte.Create(Byte(TThreadTaskStatus.WaitForStart));
  FreeOnTerminate := False;
  FThreadPool := aThreadPool;
  FWorkNow := TEvent.Create(nil,false,false,emptystr);
  lt := TList_TThreadTask(aThreadPool.Pool.Lock);
  try
    FThreadIndex := lt.Count;
  finally
    aThreadPool.Pool.Unlock;
  end;
  {$IFDEF DEBUG}
  NameThreadForDebugging(Format('%s - num. %d',[ClassName,FThreadIndex]));
  {$ENDIF}
end;

destructor TThreadTask.Destroy;
begin
  if Assigned(FEventTask) then
  begin
    if FThreadPool.FreeTaskOnceProcessed  then
    begin
      FreeAndNil(FEventTask);
    end;
  end;
  Terminate;
  WaitFor;
  FreeAndNil(FStatus);
  FreeAndNil(FWorkNow);
  inherited;
end;

procedure TThreadTask.Execute;
var FTiming : TThread.TSystemTimes;
    FIdleTime : Uint64;
    FCurrentStackCount : Uint32;

  Procedure DoEventStart;
  begin
    if FThreadPool.Synchronized then
    begin
      if assigned(FThreadPool.OnTaskStart) then
        Synchronize(InternalDoStackTaskEventStart);
    end
    else
    begin
      if assigned(FThreadPool.OnTaskStart) then
        InternalDoStackTaskEventStart;
    end;
  end;

  Procedure DoEventFinished;
  begin
    if FThreadPool.Synchronized then
    begin
      if assigned(FThreadPool.OnTaskFinished) then
        Synchronize(InternalDoStackTaskEventFinished);
    end
    else
    begin
      if assigned(FThreadPool.OnTaskStart) then
        InternalDoStackTaskEventFinished;
    end;
  end;

  Procedure ExecuteTask;
  begin
    try
      try
        FStatus.Value := Byte(TThreadTaskStatus.Processing);
        TThread.GetSystemTimes(FTiming);
        FTaskTick := FTiming.UserTime;
        DoEventStart;
        FEventTask.Execute(Self);
        TThread.GetSystemTimes(FTiming);
        FTaskTick := FTiming.UserTime - FTaskTick;
        DoEventFinished;
        if Terminated then Exit;
      Except
        //Event Error
      end;
    finally
      try
        if FThreadPool.FreeTaskOnceProcessed  then
        begin
          FreeAndNil(FEventTask);
        end;
      Except
        //Event free error.
      end;
    end;
  end;

begin
  FIdleTime := 0;
  while Not Terminated do
  begin
    case FWorkNow.WaitFor(CST_THREAD_POOL_WAIT_DURATION) of
      wrSignaled :
      begin
        //Ask if a task is available to run
        Repeat
          if Terminated then Break;
          FEventTask := nil;
          FThreadPool.FCurrentStackProtector.Acquire;
          try
            FCurrentStackCount := FThreadPool.FCurrentStack.Count;
            if FCurrentStackCount>0 then
            begin
              FEventTask := FThreadPool.FCurrentStack[0];
              FThreadPool.FCurrentStack.Delete(0);
              FIdleTime := 0;
            end;
          finally
            FThreadPool.FCurrentStackProtector.Release;
          end;

          if Assigned(FEventTask) then
          begin
            ExecuteTask;
          end;
        Until (FEventTask = nil) or (Terminated);
        FStatus.Value := Byte(TThreadTaskStatus.Idle);
      end;
      wrTimeout :
      begin
        if Terminated then Break;

        if FThreadPool is TStackDynamicThreadPool then
        begin

          FThreadPool.FCurrentStackProtector.Acquire;
          try
            FCurrentStackCount := FThreadPool.FCurrentStack.Count;
          finally
            FThreadPool.FCurrentStackProtector.Release;
          end;

          if FCurrentStackCount=0 then
          begin
            FThreadPool.clean;
            FIdleTime := FIdleTime + CST_THREAD_POOL_WAIT_DURATION;
            if FIdleTime > TStackDynamicThreadPool(FthreadPool).MaxThreadContiniousIdlingTime.Value then
            begin
              if  FThreadPool.PoolCapacity>1 then //I'm really not the last one ?
                Terminate; //No : suicide. :/
            end
            else
            begin
              FWorkNow.SetEvent;  //Check stack again.
            end;

          end;
        end;

      end;
      wrAbandoned, wrError :
      begin
        Break;
      end;
    end;
  end;
  FStatus.Value := Byte(TThreadTaskStatus.Terminating);
  if Assigned(FEventTask) then
  begin
    if FThreadPool.FreeTaskOnceProcessed  then
    begin
      FreeAndNil(FEventTask);
    end;
  end;
end;

function TThreadTask.GetStatus: TThreadTaskStatus;
begin
  result := TThreadTaskStatus(FStatus.Value);
end;

procedure TThreadTask.InternalDoStackTaskEventFinished;
begin
  FThreadPool.OnTaskFinished(FThreadIndex, FEventTask, FTaskTick);
end;

procedure TThreadTask.InternalDoStackTaskEventStart;
begin
  FThreadPool.OnTaskStart(FThreadIndex, FEventTask, 0);
end;

procedure TThreadTask.Run;
begin
  if Not(Terminated) and Not(TVisibilityThread(TThread(Self)).Started) then
    Start;
  if Not(Terminated) then
    FWorkNow.SetEvent;
end;

{ TStackDynaminThreadPool }

procedure TStackDynamicThreadPool.check;
var i : Integer;
    lt : TList_TThreadTask;
begin
  lt := TList_TThreadTask(Pool.Lock);
  try
    Clean_(lt);
    if lt.Count=0 then
    begin
      //We need a least one thread.
      lt.Add(TThreadTask.Create(Self));
      lt[0].Run;
    end
    else
    begin
      //Delete Terminated thread.
      if Not(InternalThreadIdling(lt)) then  //Is all thread are occupied ?
      begin
        //Yes : in this case : Create new thread if limit not reached.
        // if MaxThreadCount is reach, TStackTask will put in a stack. Once liberate from its internal task, a thread will take it in charge.
        //(n.b. : Automatic ressource recovery are done in TTaskThread).
        if lt.Count<MaxPoolCapacity.Value then
        begin
          lt.Add(TThreadTask.Create(Self)); //this new thread will get task.
          lt[lt.Count-1].Run;
        end
        else
        begin
          // todo/Idea : An Overload property, which indicate if there are too many task ?
        end;
      end
      else
      begin
        //Pulse : Look for the first in idling and push it.
        for I :=0 to lt.Count-1 do
        begin
          if lt[i].Status = TThreadTaskStatus.Idle then
          begin
            lt[i].Run;
            break;
          end;
        end;
      end;
    end;
  finally
    Pool.Unlock;
  end;
end;

constructor TStackDynamicThreadPool.Create(const Warm: Boolean);
begin
  Inherited Create(1);
  FMaxThreadCount := TGSProtectedUint32.Create(CPUCount);
  FMaxThreadContiniousIdlingTime := TGSProtectedUint32.Create(CST_MAXTHREADCONTINIOUSIDLINGTIME);
  if Warm then
    check;
end;

destructor TStackDynamicThreadPool.Destroy;
begin
  FreeAndNil(FMaxThreadCount);
  FreeAndNil(FMaxThreadContiniousIdlingTime);
  inherited;
end;


function TStackDynamicThreadPool.GetPoolCapacity: Uint32;
var lt : TList_TThreadTask;
begin
  lt := TList_TThreadTask(Pool.Lock);
  try
    Result := lt.Count;
  finally
    Pool.Unlock;
  end;
end;

{ TList_TThreadTask }
{$IFNDEF USE_GENERIC}

procedure TList_TThreadTask.Add(aTask: TThreadTask);
begin
  ManagedAdd(aTask);
end;

function TList_TThreadTask.GetThreadTaskItem(Index: Uint32): TThreadTask;
begin
  result := TThreadTask(FArray[Index]);
end;

procedure TList_TThreadTask.SetThreadTaskItem(Index: Uint32;
  const Value: TThreadTask);
begin
  ManagedSet(Index,Value);
end;

{ TList_TStackTask }

procedure TList_TStackTask.Add(aTask: TStackTask);
begin
  ManagedAdd(aTask);
end;

function TList_TStackTask.GetStackTaskItem(Index: Uint32): TStackTask;
begin
  Result := TStackTask(FArray[Index]);
end;

procedure TList_TStackTask.SetStackTaskItem(Index: Uint32;
  const Value: TStackTask);
begin
  ManagedSet(Index,Value);
end;

{$ENDIF}
end.
