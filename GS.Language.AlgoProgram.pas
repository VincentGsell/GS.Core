unit GS.Language.AlgoProgram;
///
///
///  Abstract implementation of "Algorithm" program.
///  This will be used by compiler tools to emit high level descriptioncode,
///  starting from a script (Such as TFormula) or a AST (abstract Syntax Tree).
///  Unit GS.AlgoProgram.Runner implement a program runner for AlgoProgram.
///
interface

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}


uses sysUtils, classes,
      Generics.Collections;

type
  ///
  ///
  ///  Core
  ///
  ///
  TAlgoProgram = class;        //Pure algorithme (i.e. symbolic) program.
  TAlgoVar = class;            //Variable representation.
  TAlgoInstruction = class;    //Instruction representation.

  TAlgoData = class
  end;

  TAlgoVar = class(TAlgoData)
  protected
    function _AsString : string; virtual; abstract;
    function _AsDouble : double; virtual; abstract;
    function _AsInteger : integer; virtual; abstract;
  public
    name, _Type : string;
    procedure assign(source : TAlgoVar); virtual; abstract;

    property AsString : String read _AsString;
    property AsDouble : Double read _AsDouble;
    property AsInteger : Integer read _AsInteger;
  end;

  TAlgoInstruction = class
  protected
    prog : TAlgoProgram;
  public
    constructor create(_prog : TAlgoProgram); reintroduce;
    function AsString : String; virtual;
  end;
  TAlgoInstructionClass = class of TAlgoInstruction;

  ///
  ///
  ///
  ///  Variable
  ///
  ///
  ///
  ///

  //Int
  TAlgoInteger = class(TAlgoVar)
    value : integer;
    procedure assign(source : TAlgoVar); override;
    function _AsString : string; override;
    function _AsDouble : double; override;
    function _AsInteger : integer; override;
  end;

  TAlgoConstInteger = class(TAlgoInteger)
    procedure assign(source : TAlgoVar); override;
  end;

  //float
  TAlgoDouble = class(TalgoVar)
    value : double;
    procedure assign(source : TAlgoVar); override;
    function _AsString : string; override;
    function _AsDouble : double; override;
    function _AsInteger : integer; override;
  end;

  TAlgoConstDouble = class(TAlgoDouble)
    procedure assign(source : TAlgoVar); override;
  end;

  //string
  TAlgoString = class(TAlgoVar)
    value : UTF8String;
    function _AsString : string; override;
  end;

  TAlgoConstString = class(TAlgoString)
    procedure assign(source : TAlgoVar); override;
  end;

  ///
  ///
  ///
  ///
  ///  OPERATION (Intruction)
  ///
  ///
  ///
  ///
  ///
  ///


  TAlgo_CreateVar = class(TAlgoInstruction)
    varname,vartype : String;
    function AsString : String; override;
  end;

  TAlgo_CreateConst = class(TAlgoInstruction)
    varname,vartype : String;
    function AsString : String; override;
  end;

  TAlgo_AssignVarVar = class(TAlgoInstruction)
    varnameAssigned : String;
    varnameSource : String;
    function AsString : String; override;
  end;

  TAlgo_AssignVarConst = class(TAlgoInstruction)
    varnameAssigned : String;
    valueSource : string;
    _type : string;
    _valueAsString : string;
    function AsString : String; override;
  end;

  TAlgo_AssignVarEqualVarOperandVar = class(TAlgoInstruction)
    varnameAssigned : String;
    varnameLeft : String;
    varnameRight : String;
  end;

  TAlgo_AssignVarEqualVarPlusVar = class(TAlgo_AssignVarEqualVarOperandVar)
  public
    function AsString : String; override;
  end;
  TAlgo_AssignVarEqualVarMinusVar = class(TAlgo_AssignVarEqualVarOperandVar)
  public
    function AsString : String; override;
  end;
  TAlgo_AssignVarEqualVarMulVar = class(TAlgo_AssignVarEqualVarOperandVar)
  public
    function AsString : String; override;
  end;
  TAlgo_AssignVarEqualVarDivVar = class(TAlgo_AssignVarEqualVarOperandVar)
  public
    function AsString : String; override;
  end;


  TAlgoProgram = Class
  private
    function GetInstructionCode(Index: Integer): TAlgoInstruction;
    function GetInstructionCount: integer;
  protected
    codes : TObjectList<TAlgoInstruction>;

    procedure AddInstruction(_Instr : TAlgoInstruction);

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure reset;
    function flushCode : string;

    procedure emit_createVar(name : String; const _type : String= 'integer');
    procedure emit_createConst(name : String; const _type : String= 'integer');
    procedure emit_assignVar(resultName, ValueName : String);
    procedure emit_assignVarConst(resultName : String; typename : String; Value : string); overload;
    procedure emit_baseAssignOp(Op : TAlgoInstructionClass; resultName, op1Name, op2Name : String);

    property Instruction[Index : Integer] : TAlgoInstruction read GetInstructionCode;
    property InstructionCount : integer read GetInstructionCount;
  End;


implementation

{ TAlgoInstruction }

function TAlgoInstruction.AsString: String;
begin
  result := className;
end;

constructor TAlgoInstruction.create(_prog: TAlgoProgram);
begin
  assert(assigned(_prog));
  prog := _prog;
end;


{ TAlgoProgram }

procedure TAlgoProgram.AddInstruction(_Instr: TAlgoInstruction);
begin
  assert(assigned(_Instr));
  codes.Add(_Instr);
end;


constructor TAlgoProgram.Create;
begin
   inherited create;
   codes := TObjectList<TAlgoInstruction>.Create;
end;

destructor TAlgoProgram.Destroy;
begin
  FreeAndNil(codes);
  inherited;
end;

procedure TAlgoProgram.emit_assignVar(resultName, ValueName : String);
var a : TAlgo_AssignVarVar;
    i,o : TAlgoVar;
begin
  a := TAlgo_AssignVarVar.create(self);
  a.varnameAssigned := resultName;
  a.varnameSource := ValueName;
  AddInstruction(a);
end;

procedure TAlgoProgram.emit_assignVarConst(resultName : String; typename : String; Value : string);
var a : TAlgo_AssignVarConst;
    n : string;
begin
  n := format('CST_%s_%s_%s',[uppercase(resultName),typename,value]);
  emit_createConst(n,typename);

  a := TAlgo_AssignVarConst.create(self);
  a.varnameAssigned := resultName;
  a.valueSource := n;
  a._type := typename;
  a._valueAsString := Value;
  AddInstruction(a);
end;

procedure TAlgoProgram.emit_baseAssignOp(Op: TAlgoInstructionClass; resultName,
  op1Name, op2Name: String);
var a : TAlgo_AssignVarEqualVarOperandVar;
begin
  a := TAlgo_AssignVarEqualVarOperandVar(Op.create(Self));
  a.varnameAssigned := resultName;
  a.varnameLeft := op1Name;
  a.varnameRight := op2Name;
  AddInstruction(a);
end;

procedure TAlgoProgram.emit_createConst(name: String; const _type: String);
var a : TAlgo_CreateConst;
    n : String;
begin
  assert(length(name)>0);
  n := trim(name);
  a := TAlgo_CreateConst.create(self);
  a.varname := name;
  a.vartype := _Type;
  AddInstruction(a);
end;

procedure TAlgoProgram.emit_createVar(name: String; const _type : string);
var a : TAlgo_CreateVar;
    n : String;
begin
  assert(length(name)>0);
  n := trim(name);
  a := TAlgo_CreateVar.create(self);
  a.varname := name;
  a.vartype := _Type;
  AddInstruction(a);
end;

function TAlgoProgram.flushCode: string;
var i : integer;
    l : TStringList;
begin
  l := TStringList.Create;
  try
    for I := 0 to codes.Count-1 do
    begin
      l.Add(codes[i].AsString);
    end;
    result := l.Text;
  finally
    FreeAndNil(l);
  end;
end;

function TAlgoProgram.GetInstructionCode(Index: Integer): TAlgoInstruction;
begin
 result := codes[Index];
end;

function TAlgoProgram.GetInstructionCount: integer;
begin
  result := codes.Count;
end;


procedure TAlgoProgram.reset;
begin
  codes.Clear;
end;

{ TAlgo_CreateVar }

function TAlgo_CreateVar.AsString: String;
begin
  result := format('%s : VAR %s (%s)',[className,varname,vartype]);
end;

{ TAlgo_AssignVarVar }

function TAlgo_AssignVarVar.AsString: String;
begin
  result := format('%s : %s <- %s',[className,varnameAssigned,varnameSource]);
end;

{ TAlgo_AssignVarConst }

function TAlgo_AssignVarConst.AsString: String;
begin
  result := format('%s : %s <- %s (%s)',[className,varnameAssigned,valueSource,_valueAsString]);
end;



{ TAlgoInteger }

function TAlgoInteger._AsDouble: double;
begin
  result := double(value);
end;

function TAlgoInteger._AsInteger: integer;
begin
  result := value;
end;

procedure TAlgoInteger.assign(source: TAlgoVar);

begin
  if source is TAlgoInteger then
    value := TalgoInteger(source).value
  else
  if source is TAlgoDouble then
    value := trunc(TalgoDouble(source).value)
  else
  if source is TAlgoString then
  begin
    try
      value := StrToInt(TAlgoString(source).value)
    Except
      raise Exception.Create(ClassName+'.Assign Error : cannot convert string into integer');
    end;
  end
  else
    raise Exception.Create(ClassName+' Error : unknown source type ('+source.ClassName+')');
end;

function TAlgoInteger._AsString: string;
begin
  result := IntToStr(value);
end;

{ TAlgoConstInteger }

procedure TAlgoConstInteger.assign(source: TAlgoVar);
begin
  raise Exception.Create('Const integer "name" cannot be assigned');
end;

{ TAlgoDouble }

procedure TAlgoDouble.assign(source: TAlgoVar);
begin
  if source is TAlgoInteger then
    value := TalgoInteger(source).AsDouble
  else
  if source is TAlgoDouble then
    value := TalgoDouble(source).value
  else
  if source is TAlgoString then
  begin
    try
      value := StrToFloat(TAlgoString(source).value);
    Except
      raise Exception.Create(ClassName+'.Assign Error : cannot convert string into float');
    end;
  end
  else
    raise Exception.Create(ClassName+' Error : unknown source type ('+source.ClassName+')');
end;

function TAlgoDouble._AsDouble: double;
begin
  result := value;
end;

function TAlgoDouble._AsInteger: integer;
begin
  result := trunc(value);
end;

function TAlgoDouble._AsString: string;
begin
  result := FloatToStr(value);
end;

{ TAlgoString }

function TAlgoString._AsString: string;
begin
  result := String(value);
end;

{ TAlgoConstDouble }

procedure TAlgoConstDouble.assign(source: TAlgoVar);
begin
  raise Exception.Create('Const double "name" cannot be assigned');
end;

{ TAlgoConstString }

procedure TAlgoConstString.assign(source: TAlgoVar);
begin
  raise Exception.Create('Const string "name" cannot be assigned');
end;

{ TAlgo_CreateConst }

function TAlgo_CreateConst.AsString: String;
begin
  result := format('%s : CONST %s (%s)',[className,varname,vartype]);
end;

{ TAlgo_AssignVarEqualVarPlusVar }

function TAlgo_AssignVarEqualVarPlusVar.AsString: String;
begin
  result := format('%s : %s <- %s + %s',[className,varnameAssigned,varnameLeft,varnameRight]);
end;

{ TAlgo_AssignVarEqualVarMinusVar }

function TAlgo_AssignVarEqualVarMinusVar.AsString: String;
begin
  result := format('%s : %s <- %s - %s',[className,varnameAssigned,varnameLeft,varnameRight]);
end;

{ TAlgo_AssignVarEqualVarMulVar }

function TAlgo_AssignVarEqualVarMulVar.AsString: String;
begin
  result := format('%s : %s <- %s * %s',[className,varnameAssigned,varnameLeft,varnameRight]);
end;

{ TAlgo_AssignVarEqualVarDivVar }

function TAlgo_AssignVarEqualVarDivVar.AsString: String;
begin
  result := format('%s : %s <- %s / %s',[className,varnameAssigned,varnameLeft,varnameRight]);
end;


end.

