unit GS.System.Processes.Python;

interface

Uses
  sysutils,
  classes,
  GS.Common,
  GS.Common.Log,
  GS.System.Processes,
  GS.Bus, //To resolve the "threading equation of P4D/Python env."
  PythonEngine; //Delphi4Python.

Const
  cst_pythonrunnerchannel = 'pythonrunner';

Type
  TGSPythonRunner = Class(TInterfacedObject, IGSCodeRunner)
  private
  protected
    FRes : IGSStringList;
    FClientBus : TBusClientReader;
  public
    constructor Create; virtual;
    destructor Destroy; Override;

    function Run(aCode : string) : string;
  End;

///
/// Thread : internal use only.
///

  TGSPythonRunnerThread = class(TPythonThread)
  private
  public
    Constructor Create; reintroduce;
  end;

  TGSPythonRunnerThreadOnBus = Class(TGSPythonRunnerThread)
  protected
    FCode : String;
    FClientChannel : String;
    procedure OnPyThreadTerminate(sender: TObject);
    procedure sendToClient(atxt : String);
  public
    constructor Create(Code : string; Clientchannel : string); Reintroduce;
    procedure ExecuteWithPython; Override;
  end;

///
///
///

  TGSCodeRunnerFactoryPython = class(TCustomnGSCodeRunnerFactory)
    class function RunnerName : String; override;
    class function GetInstance : IGSCodeRunner; override;
  end;

  TGSDummyPythonIO = Class
    procedure busMessage(Var aMessage : TBusEnvelop);
    procedure SendPythonDataU(Sender: TObject; const Data : UnicodeString);
    procedure RecvReceivePythonData(Sender: TObject; var Data : String);
  End;


implementation

  Procedure EnablePythonThreading; forward;
  Procedure DisablePythonThreading; forward;

var
  gsGPythonEngine : TPythonEngine;
  gsGPythonIO : TGSDummyPythonIO;
  gsPythonBus : TBus;


procedure EnablePythonThreading;
begin
  if Assigned(gsGPythonEngine) then
    Exit;
  gsGPythonIO := TGSDummyPythonIO.Create;
  gsGPythonEngine := TPythonEngine.Create(nil);
  gsGPythonEngine.IO := TPythonInputOutput.Create(nil);
  gsGPythonEngine.IO.OnSendUniData := gsGPythonIO.SendPythonDataU;
  gsGPythonEngine.IO.OnReceiveUniData := gsGPythonIO.RecvReceivePythonData;
  gsGPythonEngine.IO.UnicodeIO := true;
  gsGPythonEngine.InitThreads := true;
  gsGPythonEngine.LoadDll;
  gsPythonBus := TBus.Create;
  gsPythonBus.Start;
  gsPythonBus.ChannelSetOnBeforeDeliverMessageEvent(cst_pythonrunnerchannel,gsGPythonIO.busMessage);
  TPythonThread.Py_Begin_Allow_Threads;
end;

procedure DisablePythonThreading;
begin
  if Not Assigned(gsGPythonEngine) then
    exit;

  FreeAndNil(gsPythonBus);
  TPythonThread.Py_End_Allow_Threads;
  FreeAndNil(gsGPythonEngine.IO);
  FreeAndNil(gsGPythonEngine);
  FreeAndNil(gsGPythonIO);
end;


{ TGSPythonRunner }

function TGSPythonRunner.Run(aCode: string): string;
var l : TBusMessage;
    le : TBusEnvelopList;
    i : integer;
    ls : string;
    leof : boolean;
begin
  l.FromString(aCode);
  gsPythonBus.Send(l,cst_pythonrunnerchannel,'',FClientBus.ChannelListening);

  le := TBusEnvelopList.Create;
  leof := false;
  repeat
    BusProcessMessages([FClientBus],le);
    if le.Items.count>0 then begin
      for i := 0 to le.items.count-1 do begin
        ls := le.Items[i].ContentMessage.AsString;
        if length(ls)>=3 then
          leof := copy(ls,1,3)='EOF';
        FRes.add(ls);
      end;
      le.ClearAndDispose;
    end;
  until (leof);
  result := FRes.text;
end;

constructor TGSPythonRunner.Create;
var l : TGUID;
begin
  Inherited;
  l := TGUID.NewGuid;
  FRes := TGSStringList.Create;
  FClientBus := gsPythonBus.Subscribe(format('pycli_%s',[l.ToString]),nil);
end;

destructor TGSPythonRunner.Destroy;
begin
  gsPythonBus.UnSubscribe(FClientBus);
  inherited;
end;


{ TGSCodeRunnerFactoryPython }

class function TGSCodeRunnerFactoryPython.GetInstance: IGSCodeRunner;
begin
  result := TGSPythonRunner.Create;
end;

class function TGSCodeRunnerFactoryPython.RunnerName: String;
begin
  result := 'python3';
end;



{ TGSPythonRunnerThread }

constructor TGSPythonRunnerThread.Create;
begin
  inherited Create(true);
  FreeOnTerminate := true;
//  ThreadExecMode := TThreadExecMode.emNewInterpreter;  //PythonEngine.IO disconnected.
end;

{ TGSDummyPythonIO }

procedure TGSDummyPythonIO.busMessage(var aMessage: TBusEnvelop);
var l : TGSPythonRunnerThreadOnBus;
begin
  l := TGSPythonRunnerThreadOnBus.Create(aMessage.ContentMessage.AsString,aMessage.ResponseChannel);
  l.Start;
end;

procedure TGSDummyPythonIO.RecvReceivePythonData(Sender: TObject;
  var Data: String);
begin
  raise Exception.Create('RECV Error Message '+Sender.ClassName);
end;

procedure TGSDummyPythonIO.SendPythonDataU(Sender: TObject;
  const Data: UnicodeString);
begin
//  raise Exception.Create('SEND Error Message '+Sender.ClassName);
  if TThread.CurrentThread is TGSPythonRunnerThreadOnBus then
    TGSPythonRunnerThreadOnBus(TThread.CurrentThread).sendToClient(data);
//  Writeln(data);
end;

{ TGSPythonRunnerThreadOnBus }

constructor TGSPythonRunnerThreadOnBus.Create(Code: string; Clientchannel : string);
begin
  assert(assigned(gsPythonBus));
  assert(Not Clientchannel.Trim.IsEmpty);
  inherited Create;
  OnTerminate := OnPyThreadTerminate;
  FCode := code;
  FClientChannel := Clientchannel.Trim;
end;

procedure TGSPythonRunnerThreadOnBus.ExecuteWithPython;
var lok : boolean;
begin
  lok := false;
  try
    try
      gsGPythonEngine.ExecString(FCode);
      sendToClient(format('-> python %d thread finished Ok ',[ThreadID]));
      lok := true;
    Except
      On E : Exception do begin
        TLog.error(E.Message,'GS.System.Processes.Python');
        sendToClient(format('-> python %d thread finished fail %s ',[ThreadID,e.Message]));
      end;
    end;
  finally
    if lok then
      sendToClient('EOF/OK')
    else
      sendToClient('EOF/KO')
  end;
end;

procedure TGSPythonRunnerThreadOnBus.OnPyThreadTerminate(sender: TObject);
var
  ex: TObject;
begin
  //this event comes too late to sendToClient be safetly use.
  Assert(Sender is TThread);
  ex := TThread(Sender).FatalException;
  if Assigned(ex) then begin // Thread terminated due to an exception
    if ex is Exception then begin
      TLog.Error(format('-> python %d thread finished with Exception (%s) ',[ThreadID,Exception(Ex).Message]));
    end;
  end else begin // Thread terminated cleanly
  end;
end;


procedure TGSPythonRunnerThreadOnBus.sendToClient(atxt: String);
var ls : TBusMessage;
begin
  ls.FromString(atxt);
  gsPythonBus.Send(ls,FClientChannel);
end;

Initialization

EnablePythonThreading;
TGSRegisteredRunnerFactory.RegisterRunnerFactory(TGSCodeRunnerFactoryPython);

Finalization

DisablePythonThreading;

end.
