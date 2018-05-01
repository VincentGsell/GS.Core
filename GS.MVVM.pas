unit GS.MVVM;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses Classes,
     SysUtils,
     Generics.Collections,
     GS.Bus, GS.Stream,
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
  FList : TGSMVVMItemList;
  FLink : TGSMVVMLinkedList;
  FDeclaration, Fdeclarationb, Factivity : TBusClientReader;
  FCurrentVal : TGSMVVMLink;
  //message vackend process.
  Procedure InternalDeclarationChannel(Sender : TBus;Var Packet : TBusEnvelop);
  procedure InternalDeclarationLink(Sender : TBus;Var Packet : TBusEnvelop);

  procedure InternalDeclareActivity(Sender : TBus;Var Packet : TBusEnvelop);

  Procedure MVVMProcess(aInstance : string);
    Procedure DoJobAB; //synchronized.
    Procedure DoJobBA; //synchronized.

  //Utility
  procedure InternalRttiProcess(aLink: TGSMVVMLink; Const aMode : TGSMVVMLinkMode = AtoB); //To run in a synchronized part.
Public
  Constructor Create; Override;
  Destructor Destroy; Override;

  Procedure Execute; Override;
End;

TGSMVVM = Class
Private
Public
  Procedure Activity(aObject : TObject);
  Function Declare(aViewName : String; aObjectInstance : TObject; PropertyName : String; Const aNewRessourceID : String = '') : Boolean;
  Function Link(aRessourceId_A : String; aRessourceId_B: String; Const aLinkMode : TGSMVVMLinkMode = fullDuplex) : Boolean;
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
end;

{ TGSMVVM }

destructor TGSMVVMEngine.Destroy;
var i : integer;
begin
  FreeAndNil(Factivity);
  FreeAndNil(FDeclaration);
  FreeAndNil(FDeclarationb);
  FreeAndNil(FList);
  FreeAndNil(FLink);
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

Initialization

GSMVVMEngine := TGSMVVMEngine.Create;
GSMVVMEngine.Start;

Finalization

GSMVVMEngine.Terminate;
GSMVVMEngine.WaitFor;
GSMVVMEngine.Free;

end.
