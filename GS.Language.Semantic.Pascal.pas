unit GS.Language.Semantic.Pascal;

interface


uses
  Classes, SysUtils, ContNrs,
  GS.Language.Tokenizer.Pascal,
  GS.Language.Compiler;

Type
  //a := integer const
  TS_VarAssignation_PascalConstInteger = class(TS_VarAssignation_constInteger)
  protected
  public
    function process : boolean; override;
  end;

  TS_VarAssignation_PascalConstFloat = class(TS_VarAssignation_constNumber)
  protected
  public
    function process : boolean; override;
  end;

  TS_VarAssignation_PascalConstString = class(TS_VarAssignation_constString)
  protected
  public
    function process : boolean; override;
  end;

  TS_VarAssignation_PascalFormula = class(TS_VarAssignation_Formula)
  protected
    FormulaElements : TPascalTokenItemArray;
  public
    constructor create(_varName : string; _formula : string; _Item : TPascalTokenItemArray); reintroduce;

    function process : Boolean; override;
  end;

  //SEMANTIC.
  TPascalSemanticProcessor = Class(TSemanticProcessor)
  public
  end;


implementation

const localconst_logStr = '[ERROR] SemanticCheck/Error/type : ';
      localconst_logDetailvar = 'Try to assign const %s to "%s" var, which is "%s" type';
      localcont_logStrNone = 'Variable does not exists';

procedure logSemanticCheckError(Sem : TSemanticProcessor; targetType, vname,vtype :String);
begin
  if vname <> '' then //if variable is not define, this will be empty.
    Sem.log(format(localconst_logStr+localconst_logDetailvar,[targetType,vname,vtype]))
  else
    Sem.log(localconst_logStr+localcont_logStrNone);
end;

procedure logSemanticInfo(Sem : TSemanticProcessor; txt : string);
begin
  Sem.log(format('[INFO] %s',[txt]))
end;



{ TS_VarAssignation_PascalConstFloat }

function TS_VarAssignation_PascalConstFloat.process: boolean;
var l : TVarStruct;
begin
  result := false;
  if SemProc.getVar(fname,l) then
  begin
    result := (l.varType = 'double')
  end;

  if not result then
    logSemanticCheckError(SemProc,'double',l.varName,l.varType);
end;

{ TS_VarAssignation_PascalConstInteger }

function TS_VarAssignation_PascalConstInteger.process: boolean;
var l : TVarStruct;
begin
  result := false;
  if SemProc.getVar(fname,l) then
  begin
    result := (l.varType = 'integer');

    if not result then begin
      result := (l.varType = 'double'); //other compativle type

    if result then
      logSemanticInfo(SemProc, format('"%s" is %s - type is compatible with integer affectation',[fname,l.varType]));
    end;

  end;

  if not result then
    logSemanticCheckError(SemProc,'integer',l.varName,l.varType);
end;


{ TS_VarAssignation_PascalConstString }

function TS_VarAssignation_PascalConstString.process: boolean;
var l : TVarStruct;
begin
 result := false;
  if SemProc.getVar(fname,l) then
  begin
    result := l.varType = 'string';
  end;

  if not result then
    logSemanticCheckError(SemProc,'string', l.varName,l.varType);
end;


{ TS_VarAssignation_PascalFormula }

constructor TS_VarAssignation_PascalFormula.create(_varName, _formula: string;
  _Item: TPascalTokenItemArray);
begin
  inherited create(_varName,_formula);
  FormulaElements := _Item;
end;

function TS_VarAssignation_PascalFormula.process : boolean;
var l,h : TVarStruct;
    ft : string;

    function GetFormulaResultType : string;
    var i : integer;
    begin
      result := 'integer';
      for i:= 0 to Length(FormulaElements)-1 do
      begin
        if FormulaElements[i].PascalTokenType <> TPascalTokenType.pttnumberConstInt then
        begin
          if FormulaElements[i].PascalTokenType = TPascalTokenType.pttsymbol then
          begin
            if FormulaElements[i].item = '/' then
              result := 'double';
          end
          else
          if FormulaElements[i].PascalTokenType = TPascalTokenType.pttword then
          begin
             if SemProc.getVar(FormulaElements[i].item,h) then
             begin
               if result <> h.varType then
                 result := h.varType;
             end;
          end
          else
          begin
            result := 'double';
          end;
        end;
      end;
    end;

    function TypeAffinity(target, source : string) : boolean; //i.e. double := integer;
    begin
      result := target = source;
      if not(result) then
      begin
        if target = 'double' then
        begin
          if source = 'integer' then
            result := true;
        end;
      end;

    end;

begin
  inherited;
  SemProc.log('[OK] SemanticCheck/Assigned Formula : '+Name+' | '+Formula+' '+intToStr(Length(FormulaElements))+' element(s)');
  if Name <> 'RESULT' then
  begin
    if semProc.getVar(Name,l) then
    begin
      //Scan all const, var, and '/' sign to detect other stuf than type
      ft := GetFormulaResultType;
      result := typeaffinity(l.vartype,ft);
      if not result then //todo : Add test type compatibility. (integer assigneg to double)
        SemProc.log('[ERROR] SemanticCheck/Assigned Formula : '+Formula+' send "'+ft+'" type, result "'+l.varName+'" is a "'+l.varType+'"');
    end;
  end
  else
  begin
    result := true;
  end;


  if result then
  begin
    ///
  end;

end;

end.
