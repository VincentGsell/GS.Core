unit GS.Language.AlgoProgram.Runner;
///
///
/// Execution support part of TalgoProgram (GS.AlgoProgram).
///
///

interface

uses SysUtils, Classes, ContNrs,
     Generics.Collections,
     GS.Language.AlgoProgram;

Type
  TAlgoProgramRunner = class; //Runnable program, starting from a TalgoProgram

  TMemoryHolder = class(TObjectDictionary<string,TAlgoVar>)
  public
    constructor create; virtual;
  end;

  TAlgoProcessBase = class
    Source :  TAlgoInstruction;
    owner : TAlgoProgramRunner;
    constructor create(_owner : TAlgoProgramRunner; _source : TAlgoInstruction); virtual;
    procedure execute; virtual; abstract;
  end;

  TCodeHolder = class(TObjectList<TAlgoProcessBase>)
  public
    constructor create; virtual;
  end;

  TAlgoProgramRunner = class
  private
    function GetInstructionCode(Index: Integer): TAlgoProcessBase;
    function GetInstructionCodeCount: integer;
  protected
    fmemory : TMemoryHolder;
    fcode : TCodeHolder;
    fprog : TAlgoProgram; //pointer.
    fLastError : string;

  public
    procedure AddVar(name : String; _Var : TAlgoVar);
    function GetVar(_varName : string) : TAlgoVar;
    function TryGetVar(_varName : string; out _v : TAlgoVar) : boolean;
    function GetVarByValue(_var : TAlgoVar) : String;

    constructor Create; virtual;
    destructor destroy; override;

    procedure reset;
    function build(source : TAlgoProgram) : boolean;

    property InstructionCode[Index : Integer] : TAlgoProcessBase read GetInstructionCode;
    property InstructionCodeCount : integer read GetInstructionCodeCount;

    property AlgoProgram : TAlgoProgram read fprog;
    property lastError : string read fLastError;
  end;

  TAP_CreateVar = class(TAlgoProcessBase)
    varCreated : TAlgoVar; //pointer
    procedure execute; override;
  end;

  TAP_AssignVarVar = class(TAlgoProcessBase)
    varResult : TAlgoVar; //pointer
    varSource : TAlgoVar; //pointer
    procedure execute; override;
  end;

  TAP_AssignVarConst = class(TAlgoProcessBase)
    varResult : TAlgoVar;
    constSource : TAlgoVar;
    procedure execute; override;
  end;

  TAP_AssignVarEqualVarOperandVar = class(TAlgoProcessBase)
    varResult : TAlgoVar;
    varLeft : TAlgoVar;
    varRight : TAlgoVar;
  end;

  TAP_AssignVarEqualVarPlusVar = class(TAP_AssignVarEqualVarOperandVar)
    procedure execute; override;
  end;
  TAP_AssignVarEqualVarMinusVar = class(TAP_AssignVarEqualVarOperandVar)
    procedure execute; override;
  end;
  TAP_AssignVarEqualVarMulVar = class(TAP_AssignVarEqualVarOperandVar)
    procedure execute; override;
  end;
  TAP_AssignVarEqualVarDivideVar = class(TAP_AssignVarEqualVarOperandVar)
    procedure execute; override;
  end;



implementation


//TAlgoProgramRunner

function TAlgoProgramRunner.build(source : TAlgoProgram): boolean;
var i : integer;
    instr : TAlgoInstruction;
    instrVarCreate : TAP_CreateVar;
    instrAssignVarVar : TAP_AssignVarVar;
    instrAssignVarConst : TAP_AssignVarConst;
    instrOp : TAP_AssignVarEqualVarOperandVar;
begin
  Assert(Assigned(source));
  result := true;
  reset;
  fprog := source;

  for i := 0 to fprog.InstructionCount-1 do
  begin
    instr := fprog.Instruction[i];
    if instr is TAlgo_CreateVar then
    begin
      instrVarCreate := TAP_CreateVar.create(Self,instr);
      if (TAlgo_CreateVar(instr).vartype = 'integer') then
        instrVarCreate.varCreated := TAlgoInteger.Create
      else
      if TAlgo_CreateVar(instr).vartype = 'double' then
        instrVarCreate.varCreated := TAlgoDouble.Create
      else
      if TAlgo_CreateVar(instr).vartype = 'string' then
        instrVarCreate.varCreated := TAlgoString.Create
      else
        raise Exception.Create('Error Message');

      instrVarCreate.varCreated.name := TAlgo_CreateVar(instr).varname;
      instrVarCreate.varCreated._Type := TAlgo_CreateVar(instr).vartype;

      fmemory.Add(instrVarCreate.varCreated.name,instrVarCreate.varCreated); //:/

      fcode.Add(instrVarCreate);
    end
    else
    if instr is TAlgo_CreateConst then
    begin
      instrVarCreate := TAP_CreateVar.create(Self,instr);
      if (TAlgo_CreateConst(instr).vartype = 'integer') then
        instrVarCreate.varCreated := TAlgoConstInteger.Create
      else
      if TAlgo_CreateConst(instr).vartype = 'double' then
        instrVarCreate.varCreated := TAlgoConstDouble.Create
      else
      if TAlgo_CreateConst(instr).vartype = 'string' then
        instrVarCreate.varCreated := TAlgoConstString.Create
      else
        raise Exception.Create('Error : const type "'+TAlgo_CreateConst(instr).vartype+'" not supported');

      instrVarCreate.varCreated.name := TAlgo_CreateConst(instr).varname;
      instrVarCreate.varCreated._Type := TAlgo_CreateConst(instr).vartype;

      fmemory.Add(instrVarCreate.varCreated.name,instrVarCreate.varCreated); //:/

      fcode.Add(instrVarCreate);
    end
    else
    if instr is TAlgo_AssignVarVar then
    begin
      instrAssignVarVar := TAP_AssignVarVar.create(self,instr);
      instrAssignVarVar.varResult := fmemory[TAlgo_AssignVarVar(instr).varnameAssigned];
      instrAssignVarVar.varSource := fmemory[TAlgo_AssignVarVar(instr).varnameSource];

      fcode.Add(instrAssignVarVar);
    end
    else
    if instr is TAlgo_AssignVarConst then
    begin
      instrAssignVarConst := TAP_AssignVarConst.create(self,instr);
      instrAssignVarConst.varResult := fmemory[TAlgo_AssignVarConst(instr).varnameAssigned];
      instrAssignVarConst.constSource := fmemory[TAlgo_AssignVarConst(instr).valueSource];

      if instrAssignVarConst.constSource is TAlgoConstInteger  then
          TAlgoInteger(instrAssignVarConst.constSource).value := StrToInt(TAlgo_AssignVarConst(instr)._valueAsString)
      else
      if instrAssignVarConst.constSource is TAlgoConstDouble  then
          TAlgoDouble(instrAssignVarConst.constSource).value := StrToFloat(TAlgo_AssignVarConst(instr)._valueAsString)
      else
      if instrAssignVarConst.constSource is TAlgoConstString  then
          TAlgoString(instrAssignVarConst.constSource).value := TAlgo_AssignVarConst(instr)._valueAsString
      else
        raise Exception.Create('Error : Builder expected right type : AlgoProgram type is not define.');

      fcode.Add(instrAssignVarConst);
    end
    else
    if instr is TAlgo_AssignVarEqualVarOperandVar then
    begin
      if instr is TAlgo_AssignVarEqualVarPlusVar then
        instrOp := TAP_AssignVarEqualVarPlusVar.create(self,instr)
      else
      if instr is TAlgo_AssignVarEqualVarMinusVar then
        instrOp := TAP_AssignVarEqualVarMinusVar.create(self,instr)
      else
      if instr is TAlgo_AssignVarEqualVarMulVar then
        instrOp := TAP_AssignVarEqualVarMulVar.create(self,instr)
      else
      if instr is TAlgo_AssignVarEqualVarDivVar then
        instrOp := TAP_AssignVarEqualVarDivideVar.create(self,instr)
      else
        raise Exception.Create('Error Message : Operand type not found '+instr.ClassName);

      instrOp.varResult := fmemory[TAlgo_AssignVarEqualVarOperandVar(instr).varnameAssigned];
      instrOp.varLeft := fmemory[TAlgo_AssignVarEqualVarOperandVar(instr).varnameLeft];
      instrOp.varRight := fmemory[TAlgo_AssignVarEqualVarOperandVar(instr).varnameRight];

      fcode.Add(instrOp);
    end;

  end;
end;

constructor TAlgoProgramRunner.Create;
begin
  inherited create;
  fLastError := '';
  fmemory := TMemoryHolder.Create;
  fcode := TCodeHolder.create;
end;

destructor TAlgoProgramRunner.destroy;
begin
  freeAndNil(fmemory);
  freeAndNil(fcode);
  inherited;
end;

function TAlgoProgramRunner.GetInstructionCode(
  Index: Integer): TAlgoProcessBase;
begin
 result := fcode[Index];
end;

function TAlgoProgramRunner.GetInstructionCodeCount: integer;
begin
  result := fcode.Count;
end;

function TAlgoProgramRunner.GetVar(_varName: string) : TAlgoVar;
var v : TAlgoVar;
begin
  if not fmemory.TryGetValue(_varName,v) then
    raise Exception.Create(format('[%s].getVar : Error var "%s" not found',[className,_varName]));
  result := v;
end;

function TAlgoProgramRunner.GetVarByValue(_var: TAlgoVar): String;
var l : TPair<string,TAlgoVar>;
begin
  result := '';
  for l in fmemory do
    if l.Value = _var then
    begin
      result := l.Key;
      break;
    end;
end;

procedure TAlgoProgramRunner.reset;
begin
  fmemory.Clear;
  fcode.Clear;
end;

function TAlgoProgramRunner.TryGetVar(_varName: string;
  out _v: TAlgoVar): boolean;
begin

end;

procedure TAlgoProgramRunner.AddVar(name : String; _Var : TAlgoVar);
var v : TAlgoVar;
begin
  assert(length(name)>0);
  assert(assigned(_Var));
  if not fmemory.TryGetValue(name,v) then
  begin
    fmemory.Add(name, _var);
  end
  else
    raise Exception.Create(className+' : Error var "'+name+'" already exists');
end;



{ TMemoryHolder }

constructor TMemoryHolder.create;
begin
  inherited create([doOwnsValues]);
end;

{ TCodeHolder }

constructor TCodeHolder.create;
begin
  inherited create;
end;

{ TAlgoProcessBase }

constructor TAlgoProcessBase.create(_owner: TAlgoProgramRunner;
  _source: TAlgoInstruction);
begin
  assert(assigned(_owner));
  assert(assigned(_source));
  owner := _owner;
  Source := _source;
end;

{ TAP_CreateVar }

procedure TAP_CreateVar.execute;
begin
//  owner.AddVar(varCreated.name,varCreated); :/
end;

{ TAP_AssignVar }

procedure TAP_AssignVarVar.execute;
begin
  varResult.assign(varSource);
end;

{ TAP_AssignVarConst }

procedure TAP_AssignVarConst.execute;
begin
  varResult.assign(constSource);
end;

{ TAP_AssignVarEqualVarPlusVar }

procedure TAP_AssignVarEqualVarPlusVar.execute;
begin
  if varResult is TAlgoInteger then
     TalgoInteger(varResult).value := varLeft.AsInteger + varRight.AsInteger
  else
  if varResult is TAlgoDouble then
     TAlgoDouble(varResult).value := varLeft.AsDouble + varRight.AsDouble
  else
  if varResult is TAlgoString then
     TAlgoString(varResult).value := varLeft.AsString + varRight.AsString
  else
    raise Exception.Create(ClassName+'.execute Error : unknown "'+varResult.ClassName+'" type');
end;

{ TAP_AssignVarEqualVarMinusVar }

procedure TAP_AssignVarEqualVarMinusVar.execute;
begin
  if varResult is TAlgoInteger then
     TalgoInteger(varResult).value := varLeft.AsInteger - varRight.AsInteger
  else
  if varResult is TAlgoDouble then
     TAlgoDouble(varResult).value := varLeft.AsDouble - varRight.AsDouble
  else
  if varResult is TAlgoString then
  begin
     raise Exception.Create(ClassName+'.execute Error : Minus operator not applicable on String type "'+varResult.ClassName+'" type');
     //TAlgoString(varResult).value := varLeft.AsString + varRight.AsString
  end
  else
    raise Exception.Create(ClassName+'.execute Error : unknown "'+varResult.ClassName+'" type');
end;

{ TAP_AssignVarEqualVarMulVar }

procedure TAP_AssignVarEqualVarMulVar.execute;
begin
  if varResult is TAlgoInteger then
     TalgoInteger(varResult).value := varLeft.AsInteger * varRight.AsInteger
  else
  if varResult is TAlgoDouble then
     TAlgoDouble(varResult).value := varLeft.AsDouble * varRight.AsDouble
  else
  if varResult is TAlgoString then
  begin
    TAlgoString(varResult).value := FloaTToStr(varLeft.AsDouble * varRight.AsDouble);
  end
  else
    raise Exception.Create(ClassName+'.execute Error : unknown "'+varResult.ClassName+'" type');
end;

{ TAP_AssignVarEqualVarDivideVar }

procedure TAP_AssignVarEqualVarDivideVar.execute;
begin
  if varResult is TAlgoInteger then
     TalgoInteger(varResult).value := varLeft.AsInteger Div varRight.AsInteger
  else
  if varResult is TAlgoDouble then
     TAlgoDouble(varResult).value := varLeft.AsDouble / varRight.AsDouble
  else
  if varResult is TAlgoString then
  begin
    TAlgoString(varResult).value := FloaTToStr(varLeft.AsDouble / varRight.AsDouble);
  end
  else
    raise Exception.Create(ClassName+'.execute Error : unknown "'+varResult.ClassName+'" type');
end;


end.
