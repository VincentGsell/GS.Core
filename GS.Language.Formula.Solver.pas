unit GS.Language.Formula.Solver;
///
///  Independant, RAPI ("Raw AlgoProgram Interpreter") VM-based solver.
///  This solver will not used in compiler : It is a independant implemnentation.
///  it is just an fully functionnal solver witch can be used as independant
///  numerical solver in a any program. It is a good example of what we can
///  achieve with all the tokenizer, parser, and formula tools.
///
///  Note :
///  - This solver DO NOT perform syntax analyze, your formula must
///  be syntaxicaly correct.
///  - This solver use only var to perfom all calculation. Const, will be
///  change in var on the fly. You can check IR Code by the commands
///  FlushCompiledCode/FlushPostFixFormula after compilation step.
///
interface

uses classes, sysutils, contnrs,
     GS.Language.WordTokenizer, //Full code string to token.
     GS.Language.Formula,       //Forumla parser and types
     GS.Language.AlgoProgram,   //Abstract program layer.
     GS.Language.VM;            //We use RIPA VM : runing raw AlgoProgram code.

Type
TFormulaSolver = class
private
  fformula: string;
  fResult: string;
  fResultAsNumber: double;
  fLastError: string;

  fprog : TAlgoProgram;
  fMachine : TVirtualMachineRAPI;
  fPostFixFormula : String;

protected
  procedure InternalEmitProgram(_postFixedFormula : TTokenItemArray; var TargetProgram : TAlgoProgram); virtual;
public
  constructor Create; virtual;
  destructor Destroy; override;

  procedure updateVar(_varName : string; value : integer);
  function getVarValueAsInteger(_varName : string) : integer;
  function getVarValueAsFloat(_varName : string) : double;
  function getVarValueAsString(_varName : string) : string;
  function FlushCompiledCode : string;
  function FlushPostFixFormula : string;

  function Compile(const _formula : string) : boolean;
  procedure Run;

  property Formula : string read fformula write fformula;
  property SolvedResult : string read FResult;
  property SolvedResultAsNumber : double read FResultAsNumber;
  property LastError : string read FLastError;
end;

implementation


{ TFormulaSolver }

constructor TFormulaSolver.Create;
begin
  inherited create;
  fprog := TAlgoProgram.Create;
  fMachine := TVirtualMachineRAPI.Create;
  fPostFixFormula := '';
end;

destructor TFormulaSolver.Destroy;
begin
  FreeAndNil(fprog);
  FreeAndNil(fMachine);
  inherited;
end;

function TFormulaSolver.FlushCompiledCode: string;
begin
  result := fprog.flushCode;
end;

function TFormulaSolver.FlushPostFixFormula: string;
begin
  result := fPostFixFormula;
end;

function TFormulaSolver.getVarValueAsFloat(_varName: string): double;
begin
  result := fMachine.GetVarAsDouble(_varName);
end;

function TFormulaSolver.getVarValueasInteger(_varName: string): integer;
begin
  result := fMachine.GetVarAsInteger(_varName);
end;

function TFormulaSolver.getVarValueAsString(_varName: string): string;
begin
  result := fMachine.GetVarAsString(_varName);
end;

procedure TFormulaSolver.InternalEmitProgram(_postFixedFormula: TTokenItemArray;
  var TargetProgram: TAlgoProgram);
var i: integer;
    l,m : TList;
    c : integer;
    t,g :  TTokenItem;
    memory : TObjectList;
    v : String;
    vl, vr : string;
    LastWord : string; //Who have the last word ? ;) This keep last variable used, which is the formula solution.

    procedure copyListWithoutOp;
    var j : integer;
    begin
      m.Clear;
      for j := 0 to l.Count-1 do
      begin
        if (j<>i-2) and (j<>i-1) and (j<>i) then
          m.Add(l[j]);
        if j=i then
        begin
          g := TTokenItem.create(ttword,v,-1,-1);
          m.Add(g);
          memory.Add(g);
        end;
      end;
      l.Clear;
      for j := 0 to m.Count-1 do
      begin
        l.Add(m[j]);
      end;
    end;

    procedure OperandProcess; //change const into var. (formula based process is var based.)
    begin
      //Left
      if TTokenItem(l[i-2]).TokType = TTokenType.ttnumber then
      begin
        vl := 'CST'+IntToStr(c); inc(c);
        TargetProgram.emit_createVar(vl,'double');
        TargetProgram.emit_assignVarConst(vl,'double',TTokenItem(l[i-2]).item);
      end
      else
      begin
        vl := TTokenItem(l[i-2]).item;
      end;

      //Right
      if TTokenItem(l[i-1]).TokType = TTokenType.ttnumber then
      begin
        vr := 'CST'+IntToStr(c); inc(c);
        TargetProgram.emit_createVar(vr,'double');
        TargetProgram.emit_assignVarConst(vr, 'double', TTokenItem(l[i-1]).item);
      end
      else
      begin
        vr := TTokenItem(l[i-1]).item;
      end;

    end;
begin
  l := TList.Create;
  m := TList.Create;
  memory := TObjectList.Create;
  try
    for i := Low(_postFixedFormula) to High(_postFixedFormula) do
      l.Add(_postFixedFormula[i]);

    TargetProgram.emit_createVar('result','double');

    //Pass throught Token and emit program's instruction.
    i := 0;
    c := 1;
    while (i<l.Count) do
    begin
      t := TTokenItem(l[i]);
      if t.TokType = TTokenType.ttsymbol then
      begin
        if i>1 then
        begin
          if t.item = '+' then
          begin
            v := 'R'+IntToStr(c); inc(c);
            TargetProgram.emit_createVar(v,'double');
            OperandProcess;
            TargetProgram.emit_baseAssignOp(TAlgo_AssignVarEqualVarPlusVar,v,vl,vr);
            copyListWithoutOp;
            i := -1;
          end
          else
          if t.item = '-' then
          begin
            v := 'R'+IntToStr(c); inc(c);
            TargetProgram.emit_createVar(v,'double');
            OperandProcess;
            TargetProgram.emit_baseAssignOp(TAlgo_AssignVarEqualVarMinusVar,v,vl,vr);
            copyListWithoutOp;
            i := -1;
          end
          else
          if t.item = '*' then
          begin
            v := 'R'+IntToStr(c); inc(c);
            TargetProgram.emit_createVar(v,'double');
            OperandProcess;
            TargetProgram.emit_baseAssignOp(TAlgo_AssignVarEqualVarMulVar,v,vl,vr);
            copyListWithoutOp;
            i := -1;
          end
          else
          if t.item = '/' then
          begin
            v := 'R'+IntToStr(c); inc(c);
            TargetProgram.emit_createVar(v,'double');
            OperandProcess;
            TargetProgram.emit_baseAssignOp(TAlgo_AssignVarEqualVarDivVar,v,vl,vr);
            copyListWithoutOp;
            i := -1;
          end
          else
          if t.item = '^' then
          begin
            v := 'R'+IntToStr(c); inc(c);
            TargetProgram.emit_createVar(v,'double');
            OperandProcess;
            TargetProgram.emit_baseAssignOp(TAlgo_AssignVarEqualVarMulVar,v,vl,vr);
            copyListWithoutOp;
            i := -1;
          end;
        end;
      end
      else
      begin
        lastWord := t.item;
      end;
      inc(i);
    end;
    TargetProgram.emit_assignVar('result',LastWord);
  finally
    freeAndNil(l);
    freeAndNil(m);
    FreeAndNil(memory); //erase supplementary tokenItems
  end;
end;

function TFormulaSolver.Compile(const _formula: string) : boolean;
var t : String;
    l : TWordTokenizer;
    r : TTokenItemArray;
    fparser : TFormulaParser;
begin
  FLastError := '';
  t := trim(_formula);

  if t<>'' then
    fformula := t;

  l := TWordTokenizer.Create;
  fparser := TFormulaParser.Create;
  try
    l.Tokenize(fformula);
    if fparser.ParseFormula(l.getAsTokenItemArray,r) then
    begin
      fPostFixFormula := fparser.TokenItemsToString(r);
      InternalEmitProgram(r,fprog);
      result := fLastError='';
    end
    else
      fLastError := 'Parser Error : '+fparser.LastError;
  finally
    result := fLastError='';
    FreeAndNil(l);
    FreeAndNil(fParser);
  end;
end;

procedure TFormulaSolver.Run;
begin
  fMachine.load(fprog);
  fMachine.run;
end;



procedure TFormulaSolver.updateVar(_varName: string; value: integer);
begin
  fprog.emit_createVar(_varName);
  fprog.emit_assignVarConst(_varName,'integer',inttostr(value));
end;

end.
