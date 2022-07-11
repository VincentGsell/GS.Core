unit GS.Language.Compiler;

interface

uses sysUtils,
     classes,
     contnrs,
     GS.Language.WordTokenizer;

const
  CST_DEFAULTNODENAME = 'nonamed node';

type
TGSCNode = class;
TChecker = class;
  TSyntaxCheck = class;
  TSemanticProcessor = class;

///
///
///  SYNTAX PART
///
///

//GSCRoot object keep track of unique "path" throught syntax process.
//It has log feature.
//Consider it as your "diary" throught the amazing syntax analyze journey.
TGSCRoot = class
private
  function GetNodeCount: integer;
protected
  flogs : TStringList;
  fnodes : TObjectList;
  function GetNodes(index: integer): TGSCNode; virtual;
public
  constructor Create; virtual;
  destructor Destroy; override;
  procedure log(_log : string);

  //Just fo memory management.
  procedure AddNode(_node : TGSCNode);

  property Nodes[index : integer] : TGSCNode read GetNodes;
  property NodeCount : integer read GetNodeCount;

  property logs : TStringList read flogs;
End;

TGSCNode = Class
protected
  froot : TGSCRoot;
public
  constructor create(_root : TGSCRoot); reintroduce;
End;

TGSCWorkNode = Class(TGSCNode)
private
protected
  FnodeName: String;
  fnext: TGSCNode;
  fcomeFrom : TGSCNode;

  procedure InternalProcess; virtual; abstract;
public
  Constructor Create(_root : TGSCRoot; const _NodeName : String = ''; const comeFrom : TGSCNode = Nil); reintroduce; virtual;

  procedure init; virtual;
  function process : TGSCNode; virtual;

  property NodeName : String read FnodeName write FnodeName;

  property Next : TGSCNode read fnext write fNext;
End;

TGSCNode_Start = class(TGSCNode)
protected
public
  First : TGSCWorkNode;
end;

TGSCNode_Finish = class(TGSCNode)
protected
public
end;


//Base checker for syntax and semantic.
TChecker = class
private
protected
  froot : TGSCRoot;
  fStart : TGSCNode_Start;
  fFinish : TGSCNode_Finish;
  FTokenizer: TWordTokenizer;
  function GetLog: TStringList; virtual;
public
  constructor Create(_root : TGSCRoot; _tokenizer : TWordTokenizer); virtual;
  destructor Destroy; override;

  function Check : boolean; virtual;

  property root : TGSCRoot read Froot;
  property Start : TGSCNode_Start read fStart;
  property Finish : TGSCNode_Finish read fFinish;
  property Tokenizer : TWordTokenizer read FTokenizer;
  property Log : TStringList read GetLog;
end;

////Reserved Word check, statement sequence, brackets balance, and so on.
TSyntaxCheck = class(TChecker)
end;

///
///
///  SEMANTIC PART
///
///
///

TSemanticItem = class
protected
  fname : string;
public
  SemProc : TSemanticProcessor;

  constructor create; virtual;
  property Name : String read FName;
  function process : boolean; virtual; abstract;
end;

TS_VarDecl = class(TSemanticItem)
protected
  FVarType: string;
public
  Constructor Create(varName,varType : string); reintroduce;
  function process : boolean; override;
  property varType : string read FVarType;
end;

TS_VarAssignation_constString = class(TSemanticItem)
protected
  FVarValue: string;
public
  constructor create(_varName : string; _varValue : string); reintroduce;
  property varValue : string read FVarValue;
end;

TS_VarAssignation_constInteger = class(TSemanticItem)
protected
  FVarValue: integer;
public
  constructor create(_varName : string; _varValue : integer); reintroduce;
  property varValue : integer read FVarValue;
end;

TS_VarAssignation_ConstNumber = class(TSemanticItem)
protected
  FVarValue: double;
public
  constructor create(_varName : string; _varValue : double); reintroduce;
  property varValue : double read FVarValue;
end;

TS_VarAssignation_Formula = class(TSemanticItem)
protected
  FFormula: string;
public
  constructor create(_varName : string; _formula : string); reintroduce;
  property Formula : string read FFormula;
end;



////Type, class checking, advanced syntax checking. AST Builder.
///
TVarStruct = record
  varName : String;
  varType : string;
end;

TSemanticProcessor = class
private
protected
  froot : TGSCRoot;
  FElements : TStringList;

public
  constructor Create(_root : TGSCRoot); virtual;
  destructor Destroy; override;

  function process : boolean;

  procedure AddElement(_elem : TSemanticItem);
  function getVar(const varname : String; var varStruct : TVarStruct) : boolean;

  procedure log(_txt : string);

  property root : TGSCRoot read froot;
end;

/// Use all of above to build a program.
TCompiler = class
public
  function Compile(const _code : String) : boolean; virtual; abstract;
end;

implementation

{ TGSCWorkNode }

constructor TGSCWorkNode.Create(_root : TGSCRoot; const _NodeName: String; const comeFrom : TGSCNode);
begin
  inherited create(_root);
  FNodeName := _NodeName;
  if Length(_NodeName)=0 then
    FnodeName := CST_DEFAULTNODENAME;
  fnext := Nil;
  fcomeFrom := comeFrom;
end;

procedure TGSCWorkNode.init;
begin
  //none here.
  //Use it to init var, without to have to override constructor.
end;

function TGSCWorkNode.process : TGSCNode;
begin
  InternalProcess;
  result := fnext
end;

{ TChecker }

function TChecker.Check: boolean;
var n : TGSCNode;
begin
  Assert(Assigned(FTokenizer));
  Assert(Assigned(fStart));
  Assert(Assigned(fFinish));

  result := false;
  n := fStart.First;
  while (n<>fFinish) and (n<>nil)  do
  begin
    if n is TGSCWorkNode then
    begin
      n := TGSCWorkNode(n).process;
    end;
  end;

  if n=nil then
  begin
    froot.log('Syntax Check : incomplete');
  end
  else
  begin
    result := n=fFinish;
  end;
end;

constructor TChecker.Create(_root : TGSCRoot; _Tokenizer: TWordTokenizer);
begin
  inherited Create;
  assert(assigned(_tokenizer));
  assert(assigned(_root));
  froot := _root;
  fStart := TGSCNode_Start.create(froot);
  fFinish := TGSCNode_Finish.create(froot);
  FTokenizer := _tokenizer;
end;

destructor TChecker.Destroy;
begin
  freeAndNil(froot);
  inherited;
end;

function TChecker.GetLog: TStringList;
begin
  result := froot.logs;
end;

{ TGSCNode }

constructor TGSCNode.create(_root: TGSCRoot);
begin
  inherited creatE;
  assert(assigned(_root));
  froot := _root;
  froot.AddNode(self);
end;

{ TGSCRoot }

procedure TGSCRoot.AddNode(_node: TGSCNode);
begin
  assert(assigned(_node));
  if fnodes.IndexOf(_node)=-1 then
    fnodes.Add(_node);
end;

constructor TGSCRoot.Create;
begin
  flogs := TStringList.Create;
  fnodes := TObjectList.create;
end;

destructor TGSCRoot.Destroy;
begin
  freeAndNil(flogs);
  freeAndNil(fnodes);
  inherited;
end;

function TGSCRoot.GetNodeCount: integer;
begin
  result := fnodes.Count;
end;

function TGSCRoot.GetNodes(index: integer): TGSCNode;
begin
  result := TGSCNode(fnodes[index]);
end;

procedure TGSCRoot.log(_log: string);
begin
  flogs.Add(FormatDateTime('%c',Now)+'|'+_log);
end;

{ TS_VarDecl }

Constructor TS_VarDecl.Create(varName, varType: string);
begin
  inherited create;
  FVarType := varType;
  FName := varName;
end;

function TS_VarDecl.process: boolean;
begin
  result := true;
end;

{ TSemanticProcessor }

procedure TSemanticProcessor.AddElement(_elem: TSemanticItem);
begin
  Assert(Assigned(_elem));
  _elem.SemProc := self;
  FElements.AddObject(_elem.Name, _elem);
end;

constructor TSemanticProcessor.Create(_root : TGSCRoot);
begin
  inherited create;
  Assert(assigned(_root));
  froot := _root;
  FElements := TStringList.Create;
end;

destructor TSemanticProcessor.Destroy;
var i : integer;
begin
  for i := 0 to FElements.Count-1 do FElements.Objects[i].Free;
  FElements.Free;
  inherited;
end;

function TSemanticProcessor.getVar(const varname: String;
  var varStruct: TVarStruct): boolean;
var i : integer;
begin
  result := false;
  for I := 0 to FElements.Count-1 do
  begin
    if FElements.Objects[i] is TS_VarDecl then
      if lowercase(TS_VarDecl(FElements.Objects[i]).Name) = lowercase(varname) then
      begin
        result := true;
        varStruct.varName := TS_VarDecl(FElements.Objects[i]).Name;
        varStruct.varType := TS_VarDecl(FElements.Objects[i]).varType;
        break;
      end;
  end;
end;

procedure TSemanticProcessor.log(_txt: string);
begin
  froot.log(_txt);
end;

function TSemanticProcessor.process: boolean;
var i : integer;
    l : boolean;
begin
  l := true;  //Temporary : to ovverride boolean eval optimisation. (Want all "process" method to be run)
  for i := 0 to FElements.Count-1 do
  begin
    if not TSemanticItem(FElements.Objects[i]).process then
      l := false;
  end;
  result := l;
end;

{ TS_VarAssignation_constString }

constructor TS_VarAssignation_constString.create(_varName, _varValue: string);
begin
  inherited create;
  FName := _varName;
  FvarValue := _varValue;
end;

{ TS_VarAssignation_Formula }

constructor TS_VarAssignation_Formula.create(_varName, _formula: string);
begin
  inherited create;
  FName := _varName;
  FFormula := _formula;
end;

{ TSemanticItem }

constructor TSemanticItem.create;
begin
  inherited create;
  fname := 'noname semantic item';
end;

{ TS_VarAssignation_constInteger }

constructor TS_VarAssignation_constInteger.create(_varName: string;
  _varValue: integer);
begin
  inherited create;
  FName := _varName;
  FvarValue := _varValue;
end;

{ TS_VarAssignation_ConstNumber }

constructor TS_VarAssignation_ConstNumber.create(_varName: string;
  _varValue: double);
begin
  inherited create;
  FName := _varName;
  FvarValue := _varValue;
end;

end.
