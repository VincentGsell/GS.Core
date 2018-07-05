unit GS.MVVM;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses Classes,
     SysUtils,
     SyncObjs,
     Generics.Collections,
     GS.Bus, GS.Stream, GS.BOList, GS.JSON,
     rtti,
     Typinfo;
Type

TGSMVVMItem = Class
Public
  RessourceID : String;
  ViewName : String;
  ObjectInstance : String;
  ObjectClassName : String;
  PropertyName : String;
End;

TGSMVVMItemList = Class(TDictionary<String,TGSMVVMItem>)
Public
  Destructor Destroy; Override;
End;

///
///
///  METRICS (TProc call, mectrics, logs, trace, and perf.)
///
///
///

TGSMVVMMetricExceptionWrap = class
private
  FExClass: String;
  FExMes: String;
  FRessourceID: string;
public
published
  property ExceptionClass : String read FExClass Write FExClass;
  Property ExceptionMessage : String read FExMes Write FExMes;
  property RessourceId : string read FRessourceID Write FRessourceID;
end;

TGSMVVMMetricLogItem = class
private
  FDateTimeLog: TDateTime;
  FLogText: String;
public
published
  property DateTimeLog : TDateTime read FDateTimeLog Write FDateTimeLog;
  Property LogText : String read FLogText Write FLogText;
end;
TGSMVVMMetricLogs = Array of TGSMVVMMetricLogItem;

TGSMVVMMetricPerf = Class
private
  FEndOn: TDateTime;
  FBeginOn: TDateTime;
  FDMS: Int64;
public
published
  property BeginOn : TDateTime read FBeginOn Write FBeginOn;
  property EndOn : TDateTime read FEndOn Write FEndOn;
  property DurationInMilliSec : Int64 read FDMS write FDMS;
End;

TGSMVVMMetric = class
private
  FUnsuccessDesc: String;
  FSuccess: boolean;
  FExW: TGSMVVMMetricExceptionWrap;
  FLogs: TGSMVVMMetricLogs;
  FPerf: TGSMVVMMetricPerf;
Public
  constructor Create; Virtual;
  destructor Destroy; Override;
published
  property Success : boolean read fSuccess write FSuccess;
  property UnsuccessDesc : String read FUnsuccessDesc write FUnsuccessDesc;
  property ExceptionWrap : TGSMVVMMetricExceptionWrap read FExW write FExW;
  property Logs : TGSMVVMMetricLogs read FLogs Write FLogs;
  property Performance : TGSMVVMMetricPerf read FPerf Write FPerf;
end;


TGSMVVMForeignCallProc = Reference to Procedure(Const aClient : TBusClientReader;
                               Const aRessourceName : String;
                               Const aInput : TObject;
                               Var aOutPut : TObject;
                               Out aMetrics : TGSMVVMMetric);


TGSMVVMForeignItem = class
public
  RessourceID : String;
  RessourcePOP : String;
  InputClassName : string;
  OutPutClassName : string;
  ExecProc : TGSMVVMForeignCallProc;
end;

TGSMVVMForeignItemList = Class(TObjectDictionary<String,TGSMVVMForeignItem>)
Public
End;


TGSMVVMLinkMode = (None, fullDuplex, AtoB, BtoA);
TGSMVVMLink = class
private
  FRessourceB: TGSMVVMItem;
  FRessourceA: TGSMVVMItem;
  FLinkMode: TGSMVVMLinkMode;
Public
  constructor Create(aA, aB : TGSMVVMItem; aMode : TGSMVVMLinkMode); reintroduce;
  Property RessourceA : TGSMVVMItem read FRessourceA;
  Property RessourceB : TGSMVVMItem read FRessourceB;
  Property LinkMode : TGSMVVMLinkMode read FLinkMode;
end;

TGSMVVMLinkedList = Class(TList<TGSMVVMLink>)
Public
  Destructor Destroy; Override;
End;


TGSMVVMEngine = Class(TBus)
Private
  ///
  ///
  //LOCAL API (RTTI) var.
  ///
  ///
  FList : TGSMVVMItemList;
  FLink : TGSMVVMLinkedList;
  FDeclaration, Fdeclarationb, Factivity : TBusClientReader;
  FCurrentVal : TGSMVVMLink;


  ///
  ///
  ///  FOREIGN API var
  ///
  ///
  FForeignList : TGSMVVMForeignItemList;
  FForeignDeclaration : TBusClientReader;


  ///
  ///
  //LOCAL API (RTTI) methods.
  ///
  ///

  //message process.
  Procedure InternalDeclarationChannel(Sender : TBus;Var Packet : TBusEnvelop);
  procedure InternalDeclarationLink(Sender : TBus;Var Packet : TBusEnvelop);

  procedure InternalDeclareActivity(Sender : TBus;Var Packet : TBusEnvelop);

  Procedure MVVMProcess(aInstance : string);
    Procedure DoJobAB; //synchronized.
    Procedure DoJobBA; //synchronized.

  //Utility
  procedure InternalRttiProcess(aLink: TGSMVVMLink; Const aMode : TGSMVVMLinkMode = AtoB); //To run in a synchronized part.


  ///
  ///
  ///  FOREIGN API Methods
  ///
  ///
  //message process.
  Procedure InternalForeignDeclarationChannel(Sender : TBus;Var Packet : TBusEnvelop);


Public
  Constructor Create; Override;
  Destructor Destroy; Override;

  Procedure Execute; Override;
End;



/// TGSMVVMNest is just a simple thread which monitor foreign ask to local ressource, and redrive the call as specified.
/// The RPC call could be then executed inside the nest, in anonymous thread, or Synchonized nor queued.
///

TGSMVVMNest = class(TThread)
protected
  FClientList : TBusClientReaderList;
  FRessources : TofBusinessObjectList<Integer>;         //Synchro one-to-one with Client.
  FInputClassRegistration : TList<TClass>;              //Idem
  FOutputClassRegistration : TList<TClass>;             //Idem

  procedure InternalIncomingCall(Sender : TBus;Var Packet : TBusEnvelop); Virtual;
public
  Procedure RegisterRessource(aRessourceName, aRessourcePOP : String; aInputClassName, aOutPutClassName : TClass; aExecProc : TGSMVVMForeignCallProc);
  //Procedure UnregsiterClient
  constructor Create; Reintroduce;
  Destructor Destroy; Override;

  Procedure Execute; Override;
end;


//Wrapper for local bus synchrone response. (for TGSMVVM.ForeignCall)
TGSMVVMResponse = Object
  Response : TObject;
  Metric : TGSMVVMMetric;
  Procedure CallBack(Sender : TBus;Var Packet : TBusEnvelop);
end;

TGSMVVM = Class
Private
Public
  //RTTI based : Use Synchronize to work automaticaly with FSMVVMEngine.
  Procedure Activity(aObject : TObject);
  Function Declare(aViewName : String; aObjectInstance : TObject; PropertyName : String; Const aNewRessourceID : String = '') : Boolean;
  Function Link(aRessourceId_A : String; aRessourceId_B: String; Const aLinkMode : TGSMVVMLinkMode = fullDuplex) : Boolean;


  ///Bottom methods are not based upon rtti, and make your app backend compatible to be network powered (so, physicaly split from your app.)
  ///But it could be work in a simple way, such as a backend (dataModule, object, program) isolated into a thread : Then, ForeignXXXX api
  ///is a solution to exchange object (by JSON/Binary serialization).

  // Declare a local ressource, available as ressource for an external "client" (i.e. a program, foreign thread an so on)
  //This ressource will run ExecPoint in the local ressource thread. (thread which call bus.Process api)
  Function ForeignDeclare( aRessourceName : String;
                           aRessourcePOP : string;
                           aInPutClassName : TClass;
                           aOutputClassName : TClass;
                           ExecPoint : TGSMVVMForeignCallProc;
                           const aNest : TGSMVVMNest = Nil) : Boolean;

  //Direct RPC call to a foreign ressource.
  Function ForeignCall ( const aPOP : String;
                         aInputObject : TObject;
                         var aOutputObject : TObject;
                         var aMetric : TGSMVVMMetric) : Boolean;

  //Up to date CSV : List all foreign ressources availables.
  Function ForeignRessources : String;

  //Ge new nest (thread) to monitoring foreign declaration query.
  Function GetNewNest : TGSMVVMNest;

End;


Var GSMVVMEngine : TGSMVVMEngine;
implementation

{ TGSMVVMEngine }

constructor TGSMVVMEngine.Create;
begin
  inherited Create;
  FList := TGSMVVMItemList.Create;
  FLink := TGSMVVMLinkedList.Create;
  FDeclaration := Subscribe('MVVMDeclare', InternalDeclarationChannel);
  FDeclarationb := Subscribe('MVVMDeclareLink', InternalDeclarationLink);
  Factivity := Subscribe('MVVMActivity', InternalDeclareActivity);

  //Foreign
  FForeignList := TGSMVVMForeignItemList.Create;
  FForeignDeclaration := Subscribe('MVVMForeignDeclare', InternalDeclarationChannel);

end;

{ TGSMVVM }

destructor TGSMVVMEngine.Destroy;
var i : integer;
begin
  UnSubscribe(Factivity);
  UnSubscribe(FDeclaration);
  UnSubscribe(Fdeclarationb);
  UnSubscribe(FForeignDeclaration);

  FreeAndNil(Factivity);
  FreeAndNil(FDeclaration);
  FreeAndNil(FDeclarationb);
  FreeAndNil(FList);
  FreeAndNil(FLink);

  FreeAndNil(FForeignList);
  FreeAndNil(FForeignDeclaration);
  inherited;
end;

procedure TGSMVVMEngine.DoJobAB;
begin
  InternalRttiProcess(FCurrentVal);
end;

procedure TGSMVVMEngine.DoJobBA;
begin
  InternalRttiProcess(FCurrentVal,BtoA);
end;


procedure TGSMVVMEngine.Execute;
begin
  while Not(Terminated) do
  begin
    Inherited BusExecute;
    ProcessMessages([FDeclaration,Fdeclarationb,Factivity]);
  end;
end;

procedure TGSMVVMEngine.InternalRttiProcess(aLink: TGSMVVMLink; Const aMode : TGSMVVMLinkMode = AtoB);
var  lprop: TRttiProperty;
     lval, lvalb: TValue;
     lvalok : Boolean;
     lcontext : TRttiContext;
     la,lb : TObject;
begin
  case aMode of
    None: Exit;
    fullDuplex: Exit;
    AtoB:
    begin
      la := TObject(Pointer(NativeInt(StrToInt(aLink.RessourceA.ObjectInstance))));
      lb := TObject(Pointer(NativeInt(StrToInt(aLink.RessourceB.ObjectInstance))));

      lvalok := false;
      for lprop in lcontext.GetType(la.ClassType).GetProperties do
      begin
        if UpperCase(lprop.name) = upperCase(aLink.FRessourceA.PropertyName) then
        begin
          lval := lprop.GetValue(la);
          lvalok := true;
          break;
        end;
      end;

      if lvalok then
      begin
        for lprop in lcontext.GetType(lb.ClassType).GetProperties do
        begin
          if UpperCase(lprop.name) = upperCase(aLink.FRessourceB.PropertyName) then
          begin
            lprop.SetValue(lb,lval.ToString);
            break;
          end;
        end;
      end;

    end;
    BtoA:
    begin
      la := TObject(Pointer(NativeInt(StrToInt(aLink.RessourceB.ObjectInstance))));
      lb := TObject(Pointer(NativeInt(StrToInt(aLink.RessourceA.ObjectInstance))));

      lvalok := false;
      for lprop in lcontext.GetType(la.ClassType).GetProperties do
      begin
        if UpperCase(lprop.name) = upperCase(aLink.FRessourceB.PropertyName) then
        begin
          lval := lprop.GetValue(la);
          lvalok := true;
          break;
        end;
      end;

      if lvalok then
      begin
        for lprop in lcontext.GetType(lb.ClassType).GetProperties do
        begin
          if UpperCase(lprop.name) = upperCase(aLink.FRessourceA.PropertyName) then
          begin
            lprop.SetValue(lb,lval.ToString);
            break;
          end;
        end;
      end;

    end;
  end;
end;

procedure TGSMVVMEngine.InternalDeclarationChannel(Sender: TBus;
  var Packet: TBusEnvelop);
var lObjView : String;
    lObjInstanceValue : String;
    lObjClassName : String;
    lObjPropName : String;
    lRessourceID : String;

    lStream : TMemoryStream;
    lValue : TGSMVVMItem;
begin
  //Arriving here all declaration from everywhere.
  lStream := Packet.ContentMessage.AsStream;
  try
    lRessourceID      := ReadString(lStream);
    lObjView          := ReadString(lStream);
    lObjInstanceValue := ReadString(lStream);
    lObjClassName     := ReadString(lStream);
    lObjPropName      := ReadString(lStream);
  finally
    FreeAndNil(lStream);
  end;

  if Not FList.TryGetValue(lRessourceID,lValue) then
  begin
    lValue := TGSMVVMItem.Create;
    lValue.ViewName := lObjView;
    lValue.ObjectInstance := lObjInstanceValue;
    lValue.ObjectClassName := lObjClassName;
    lValue.PropertyName := lObjPropName;
    lValue.RessourceId := lRessourceId;
    Flist.Add(lRessourceID,lValue);
  end
  else
  begin
    raise Exception.Create('Error Message');
  end;

  ///
  ///
  ///  ...

end;

procedure TGSMVVMEngine.InternalDeclarationLink(Sender: TBus;
  var Packet: TBusEnvelop);
var lLink : TGSMVVMLink;
    la,lb : TGSMVVMItem;
    lra,lrb : String;
    lrm : TGSMVVMLinkMode;
    lm : TMemoryStream;
begin
  lm := Packet.ContentMessage.AsStream;
  try
    lra := ReadString(lm);
    lrb := ReadString(lm);
    lrm := TGSMVVMLinkMode(Readinteger(lm));
  finally
    FreeAndNil(lm);
  end;

  FList.TryGetValue(lra,lA);
  FList.TryGetValue(lrb,lb);
  if Assigned(la) And Assigned(lb) then
  begin
    lLink := TGSMVVMLink.Create(la,lb,lrm);
    FLink.Add(lLink);
  end;
end;

procedure TGSMVVMEngine.InternalDeclareActivity(Sender: TBus;
  var Packet: TBusEnvelop);
var lm : TMemoryStream;
    ls : String;
begin
  lm := Packet.ContentMessage.AsStream;
  try
    ls := ReadString(lm);
    MVVMProcess(ls);
  finally
    FreeAndNil(lm);
  end;
end;

procedure TGSMVVMEngine.InternalForeignDeclarationChannel(Sender: TBus;
  var Packet: TBusEnvelop);
var lRessourceID : String;
    lRessurcePOP : String;
    lInputClassName : String;
    lOutputClassName : String;
    lExecProc : String;

    lStream : TMemoryStream;
    lValue : TGSMVVMForeignItem;
begin
  //Arriving here all declaration from everywhere.
  lStream := Packet.ContentMessage.AsStream;
  try
    lRessourceID     := ReadString(lStream);
    lRessurcePOP     := ReadString(lStream);
    lInputClassName  := ReadString(lStream);
    lOutputClassName := ReadString(lStream);
    lExecProc        := ReadString(lStream);
  finally
    FreeAndNil(lStream);
  end;

  if Not FForeignList.TryGetValue(lRessourceID,lValue) then
  begin
    lValue := TGSMVVMForeignItem.Create;
    lValue.RessourceID := lRessourceID;
    lValue.RessourcePOP := lRessurcePOP;
    lValue.InputClassName := lInputClassName;
    lValue.OutPutClassName := lOutputClassName;
    lValue.ExecProc := TGSMVVMForeignCallProc(Pointer(NativeInt(StrToInt(lRessourceId)))^);
    FForeignList.Add(lRessourceID,lValue);
  end
  else
  begin
    raise Exception.Create('Error Message');
  end;
end;

procedure TGSMVVMEngine.MVVMProcess(aInstance : string);
var lval : TGSMVVMLink;
begin
  for lval in FLink do
  begin
    case lval.LinkMode of
      None: ;
      fullDuplex: ;
      AtoB:
      begin
        if lval.RessourceA.ObjectInstance = aInstance then
        begin
          FCurrentVal := lval;
          TThread.Synchronize(nil,DoJobAB);
        end
      end;
      BtoA:
      begin
        if lval.RessourceB.ObjectInstance = aInstance then
        begin
          FCurrentVal := lval;
          TThread.Synchronize(nil,DoJobBA);
        end;
      end;
    end;
  end;
end;

{ TGSMVVM }

Function BuildDeclareMessage(aViewName : String; aObject : TObject; aPropName : String; aRessourceID : String) : TBusMessage;
var lStream : TMemoryStream;
begin
  Assert(Length(ARessourceID)>0);
  lStream := TMemoryStream.Create;
  try
    WriteString(lStream,aRessourceID);
    WriteString(lStream,aViewName);
    WriteString(lStream,IntToStr(NativeInt(Pointer(aObject))));
    WriteString(lStream,aObject.ClassName);
    WriteString(lStream,aPropName);
    Result.FromStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;

Function BuildDeclareForeignMessage(aRessourceID, aRessourcePOP, aInpuClassName, aOutputClassName : String; aExecProc : TGSMVVMForeignCallProc) : TBusMessage;
var lStream : TMemoryStream;
begin
  Assert(Length(ARessourceID)>0);
  lStream := TMemoryStream.Create;
  try
    WriteString(lStream,aRessourceID);
    WriteString(lStream,aRessourcePOP);
    WriteString(lStream,aInpuClassName);
    WriteString(lStream,aOutputClassName);
    WriteString(lStream,IntToStr(NativeInt(Pointer(@aExecProc))));
    Result.FromStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;

Function BuildCallForeignMessage(aRessourcePOP, aInpuClassNameJsonized : String) : TBusMessage;
var lStream : TMemoryStream;
begin
  Assert(Length(aRessourcePOP)>0);
  lStream := TMemoryStream.Create;
  try
    WriteString(lStream,aInpuClassNameJsonized);
    Result.FromStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;


Function BuildDeclareMessageLink(aResA, aResB : String; aLinkMode : TGSMVVMLinkMode) : TBusMessage;
var lStream : TMemoryStream;
begin
  Assert(Length(aResA)>0);
  Assert(Length(aResB)>0);
  lStream := TMemoryStream.Create;
  try
    WriteString(lStream,aResA);
    WriteString(lStream,aResB);
    WriteInteger(lStream,Integer(aLinkMode));
    Result.FromStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;

Function BuildActivityMessage(aRes : String) : TBusMessage;
var lStream : TMemoryStream;
begin
  Assert(Length(aRes)>0);
  lStream := TMemoryStream.Create;
  try
    WriteString(lStream,aRes);
    Result.FromStream(lStream);
  finally
    FreeAndNil(lStream);
  end;
end;


procedure TGSMVVM.Activity(aObject: TObject);
var la : TBusMessage;
begin
  la := BuildActivityMessage(InttoStr(NativeInt(aObject)));
  GSMVVMEngine.Send(la,'MVVMActivity');
end;

function TGSMVVM.Declare(aViewName : String; aObjectInstance: TObject; PropertyName: String;
  const aNewRessourceID: String): Boolean;
var la : TBusMessage;
begin
  la := BuildDeclareMessage(aViewName, aObjectInstance, PropertyName, aNewRessourceID);
  GSMVVMEngine.Send(la,'MVVMDeclare');
  Result := True;
end;

{ TGSMVVMResponse }

procedure TGSMVVMResponse.CallBack(Sender: TBus; var Packet: TBusEnvelop);
var lStream : TMemoryStream;
    lOutObj : String;
    lMetric : String;
begin
  lStream :=  Packet.ContentMessage.AsStream;
  try
    lOutObj := ReadString(lStream);
    lMetric := ReadString(lStream);
    TGSJson.JsonToObject(lOutObj,Response);
    TGSJson.JsonToObject(lMetric,TObject(Metric));
  finally
    FreeAndNil(lStream);
  end;
end;

function TGSMVVM.ForeignCall(const aPOP: String;
                              aInputObject: TObject;
                              var aOutputObject : TObject;
                              var aMetric : TGSMVVMMetric): Boolean;

var aMes : TBusMessage;
    lJsoncontent : String;
    lc : TBusClientReader;
    lResponse : TGSMVVMResponse;
begin
  Assert(Assigned(aInputObject));
  Assert(Assigned(aOutputObject));
  Assert(Assigned(aMetric));

  //TODO Verify if aPop is konwn by GSMVVMEngine !!! (else, locked.  )

  Result := False;
  lc := GSMVVMEngine.Subscribe(aPOP+'_Answer',lResponse.CallBack);
  try
    lJsoncontent := TGSJson.ObjectToJson(aInputObject);
    aMes.FromString(lJsoncontent);                         //Input object as json.
    lResponse.Response := aOutputObject;                   //lResponse in a local object which receive a resposne from bus.
    lResponse.Metric := aMetric;
    GSMVVMEngine.Send(aMes,aPOP,EmptyStr,aPOP+'_Answer');  //Send data to anothre thread, which have the aPop ressorce. Waiting response on aPop+'_Answer' channel.

    //Snchro call : Writing part.
    lc.Event := GSMVVMEngine.GetNewEvent;
    lc.Event.ResetEvent;
    while True do
    begin
      case lc.Event.WaitFor(INFINITE) of
        wrSignaled :
        begin
         GSMVVMEngine.ProcessMessages([lc]); //Trig lResponse.CallBack (TGSMVVMResponse.CallBack)
         Result := True;
         break;
        end
        else
        begin
          raise Exception.Create(ClassName+'.ForeignCall : Time out.');
        end;
      end;
    end;

  finally
    GSMVVMEngine.UnSubscribe(lc);
    lc.Event.Free;
    FreeAndNil(lc);
  end;
end;

function TGSMVVM.ForeignDeclare( aRessourceName,
                                 aRessourcePOP : String;
                                 aInPutClassName,
                                 aOutputClassName: TClass;
                                 ExecPoint: TGSMVVMForeignCallProc;
                                 const aNest : TGSMVVMNest = Nil): Boolean;
var la : TBusMessage;
begin
  la := BuildDeclareForeignMessage(aRessourceName, aRessourcePOP, aInPutClassName.ClassName, aOutputClassName.ClassName,ExecPoint);

  //Declare to our local Nest, to be ready to receive call properly.
  if Assigned(aNest) then
  begin
    aNest.RegisterRessource(aRessourceName,aRessourcePOP,aInPutClassName,aOutputClassName,ExecPoint);
  end;

  //Declare to the engine the entry point.
  GSMVVMEngine.Send(la,'MVVMForeignDeclare');
  Result := True;
end;

function TGSMVVM.ForeignRessources: String;
begin
  result := 'TODO : CSV.';
end;

function TGSMVVM.GetNewNest: TGSMVVMNest;
begin
  Result := TGSMVVMNest.Create;
end;

Function TGSMVVM.Link(aRessourceId_A, aRessourceId_B: String;
  const aLinkMode: TGSMVVMLinkMode) : Boolean;
var la : TBusMessage;
begin
  la := BuildDeclareMessageLink(aRessourceId_A, aRessourceId_B, aLinkMode);
  GSMVVMEngine.Send(la,'MVVMDeclareLink');
  Result := True;
end;

{ TGSMVVMItemList }

destructor TGSMVVMItemList.Destroy;
var Val : TGSMVVMItem;
begin
  for val in Values do
  begin
    val.Free;
  end;
  inherited;
end;

{ TGSMVVMLink }

constructor TGSMVVMLink.Create(aA, aB: TGSMVVMItem; aMode: TGSMVVMLinkMode);
begin
  Inherited Create;
  Assert(assigned(aA));
  Assert(assigned(aB));
  Assert(aA<>aB);
  //Is aB yet linked to aA ? and vice versa ? And via other ? (Cascade !!)
  FRessourceA := aA;
  FRessourceB := aB;
  FLinkMode := aMode;
end;

{ TGSMVVMLinkedList }

destructor TGSMVVMLinkedList.Destroy;
var Val : TGSMVVMLink;
begin
  for val in Self do
  begin
    val.Free;
  end;
  inherited;
end;

{ TGSMVVMNest }

constructor TGSMVVMNest.Create;
begin
  Inherited Create(False);
  FClientList := TBusClientReaderList.Create;
  FRessources := TofBusinessObjectList<Integer>.Create;
  FInputClassRegistration := TList<TClass>.Create;
  FOutputClassRegistration := TList<TClass>.Create;
end;

destructor TGSMVVMNest.Destroy;
var i : Integer;
    Ll : Tlist<TBusClientReader>;
begin
  Ll := FClientList.Lock;
  try
    for I := 0 to Ll.Count-1 do
    begin
      GSMVVMEngine.UnSubscribe(Ll[i]);
      ll[i].Free;
    end;
    for I := 0 to FRessources.Count-1 do
      FRessources.ByIndex[i].Free;
  finally
    FCLientList.Unlock;
  end;
  Terminate;
  WaitFor;
  FClientList.Free;
  FRessources.Free;
  FInputClassRegistration.Free;
  FOutputClassRegistration.Free;
  inherited;
end;

procedure TGSMVVMNest.Execute;
begin
  while Not(Terminated) do
  begin
    GSMVVMEngine.ProcessMessages(FClientList.ToArray);
    Sleep(1) //TODO : Temprorary for energy eco. to be Replace when time by TEvent.
    //See perhaps : http://seanbdurkin.id.au/pascaliburnus2/archives/230
  end;
end;


procedure TGSMVVMNest.RegisterRessource( aRessourceName, aRessourcePOP : String;
                                         aInputClassName, aOutPutClassName : TClass; aExecProc : TGSMVVMForeignCallProc);
var Ll : Tlist<TBusClientReader>;
    lItem : TGSMVVMForeignItem;
begin
  Ll := FClientList.Lock;
  try
    lItem := TGSMVVMForeignItem.Create;
    lItem.RessourceID := aRessourceName;
    lItem.RessourcePOP := aRessourcePOP;
    lItem.InputClassName := aInputClassName.ClassName;
    lItem.OutPutClassName := aOutPutClassName.ClassName;
    lItem.ExecProc := aExecProc;

    FRessources.Add(FRessources.Count,lItem);

    Ll.Add(GSMVVMEngine.Subscribe(aRessourcePOP,InternalIncomingCall));
    FInputClassRegistration.Add(aInputClassName);
    FOutPutClassRegistration.Add(aOutputClassName);
  finally
    FClientList.Unlock;
  end;
end;

procedure TGSMVVMNest.InternalIncomingCall(Sender: TBus;
  var Packet: TBusEnvelop);
var lJsonContent : String;
    lInObject, lOutObject : TObject;
    lr :TGSMVVMForeignItem;
    lt : TGSMVVMForeignCallProc;

    lclient : TBusClientReader;
    lInputClass, lOutputClass : TClass;
    lMetric : TGSMVVMMetric;
    lGetTickCount : UInt64;

    lResponse : TBusMessage;
    lResStream : TMemoryStream;
    lList : TList<TBusClientReader>;

function GetRessourceFromPOP( aRessourcePOP : String;
                                Out aClient : TBusClientReader;
                                Out aInputClass : TClass;
                                Out aOutPutclass : TClass) : TGSMVVMForeignItem;
var i : integer;
begin
  //This function must be call inside FClientList.Lock.
  Result := Nil;
  for i := 0 to FRessources.Count-1 do
    if LowerCase(TGSMVVMForeignItem(FRessources.ByIndex[i]).RessourcePop) = lowercase(aRessourcePOP) then
    begin
      Result := TGSMVVMForeignItem(FRessources.ByIndex[i]);
      aClient := lList[i];
      aInputClass := FInputClassRegistration[i];
      aOutPutclass := FOutputClassRegistration[i];
      Break;
    end;
end;

begin
  //Real client call !
  lList := FClientList.Lock;
  try
    lr := GetRessourceFromPOP(Packet.TargetChannel,lClient,lInputClass,lOutputclass);
    if Assigned(lr) then
    begin
      lJsonContent := Packet.ContentMessage.AsString;

      lResStream := TMemoryStream.Create;
      lInObject := lInputClass.Create;
      lOutObject := lOutputClass.Create;
      lMetric := TGSMVVMMetric.Create;
      lMetric.Performance := TGSMVVMMetricPerf.Create;
      try
        lMetric.Performance.BeginOn := Now;
        lGetTickCount :=  GetTickCount;

        TGSJson.JsonToObject(lJsonContent,lInObject);
        lt := lr.ExecProc;

        //TODO : Here implement Queued/Synchro, Alien thread exec or local exec (as it is actually)
        try
          //Here is exec of service.
          lt(lClient,lr.RessourceID,lInObject,lOutObject,lMetric); //Local exec.
          lMetric.Success := true;
        Except
          On E : Exception do
          begin
            lMetric.Success := False;
            lMetric.ExceptionWrap := TGSMVVMMetricExceptionWrap.Create;
            lMetric.ExceptionWrap.ExceptionClass := E.ClassName;
            lMetric.ExceptionWrap.ExceptionMessage := E.Message;
            lMetric.ExceptionWrap.RessourceId := lr.RessourceID;
            //TODO : Get info stack ? E.GetStackInfoStringProc
          end;
        end;

        lJsonContent := TGSJson.ObjectToJson(lOutObject);
        WriteString(lResStream,lJsonContent); //Outobject in stream.
        lJsonContent := TGSJson.ObjectToJson(lMetric);
        WriteString(lResStream,lJsonContent); //Metrics in stream.
        lMetric.Performance.DurationInMilliSec := GetTickCount - lGetTickCount;
        lMetric.Performance.EndOn := Now;
        lResStream.Position := 0;
        lResponse.FromStream(lResStream);
      finally
        FreeAndNil(lInObject);
        FreeAndNil(lOutObject);
        FreeAndNil(lMetric);
        FreeAndNil(lResStream);
      end;

      GSMVVMEngine.Send(lResponse,Packet.ResponseChannel);
    end
    else
    begin
      raise Exception.Create('Ressource for '+Packet.TargetChannel+' not found');
    end;
  finally
    FClientList.Unlock;
  end;

end;



{ TGSMVVMMetric }

constructor TGSMVVMMetric.Create;
begin
  Inherited;
  FExW := Nil;
  FLogs := Nil;
  FPerf := Nil;
end;

destructor TGSMVVMMetric.Destroy;
var i : Integer;
begin
  if Assigned(FExW) then
    FreeAndNil(FExW);
  if Assigned(FPerf) then
    FreeAndNil(FPerf);
  if Length(FLogs)>0 then
  begin
    for I := Low(FLogs) to High(FLogs) do
    begin
      FLogs[i].Free;
    end;
    FLogs := Nil;
  end;
  inherited;
end;

Initialization

GSMVVMEngine := TGSMVVMEngine.Create;
GSMVVMEngine.Start;

Finalization

GSMVVMEngine.Terminate;
GSMVVMEngine.WaitFor;
GSMVVMEngine.Free;

end.
