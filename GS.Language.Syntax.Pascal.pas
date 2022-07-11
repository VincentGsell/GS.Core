unit GS.Language.Syntax.Pascal;

interface

uses
  Classes, SysUtils, ContNrs,
  GS.Language.WordTokenizer,
  GS.Language.Tokenizer.Pascal,
  GS.Language.Semantic.Pascal,
  GS.Language.Compiler;

Type
  TPascalSymbolCodeArray = array of TPascalSymbolTokenType;
  TPascalSyntaxCheck = Class;
  TGSCRootEx = class(TGSCRoot)
  public
    Tokenizer : TPascalTokenizer;
    SyntaxCheck : TPascalSyntaxCheck;
    SemanticProcessor : TPascalSemanticProcessor;
  end;

  ///
  ///  SYNTAX ANALYZE :
  ///  PASCAL PROJECT HEADER
  ///
  TGSCNode_checkPascalProjectHeader = class(TGSCWorkNode)
  protected
    procedure InternalProcess; override;
  end;

  //uses
  TGSCNode_checkProgramUses = class(TGSCWorkNode)
    procedure InternalProcess; override;
  end;

  TGSCNode_checkProgramType = class(TGSCWorkNode)
    procedure InternalProcess; override;
  end;

  TGSCNode_checkProgramConst = class(TGSCWorkNode)
    procedure InternalProcess; override;
  end;

  TGSCNode_checkProgramVar = class(TGSCWorkNode)
    procedure InternalProcess; override;
  end;

  TGSCNode_checkProgramBegin = class(TGSCWorkNode)
    procedure InternalProcess; override;
  end;

  TGSCNode_checkSyntax_CodeAnalyze = class(TGSCWorkNode)
  protected
    local : TPascalTokenItem;
    lr : TGSCRootEx;
    procedure NextToken;
    procedure PreviousToken;
    function isSequenceConforme(_data : Array of TPascalSymbolTokenType) : boolean;
    procedure InternalProcess; override;
  public
  end;

  TVarAffectationOp = (vaoAssign, vaoPlus, vaoMinus, vaoMul, vaoDiv);
  TGSCNode_checkSyntax_CodeAnalyze_VarAffectation = class(TGSCNode_checkSyntax_CodeAnalyze)
  protected
    op : TVarAffectationOp;
    procedure InternalProcess; override;
  public
  end;

  TGSCNode_checkSyntax_Call_Method = class(TGSCNode_CheckSyntax_CodeAnalyze)
  protected
    procedure InternalProcess; override;
  public
  end;

  { TGSCNode_checkSyntay_FormulaCheck }

  TGSCNode_checkSyntax_FormulaCheck = class(TGSCNode_checkSyntax_CodeAnalyze)
    procedure InternalProcess; override;
  end;

  TPascalSyntaxVarItem = class
    varname : string;
    vartype : string;
  end;

  TPascalSyntaxCheck = Class(TSyntaxCheck)
  private
    function GetRootASRootEx: TGSCRootEx;
  protected
    projectHeader : TGSCNode_checkPascalProjectHeader;
    varList : TStringList;
    MethodList : TStringList;
    procedure AddNewVar(name : string; const varType : String = '');
  public
    function IsPascalRegisteredFunction(_word : string) : boolean;
    function IsPascalKeyWord(_word : string) : boolean;
    function IsFreeWord(_word : string) : boolean; //Not key word, reserved (built in type) word, yet defined var or type word.
    function IsVar(_word : string) : boolean;
    function VarType(_word : string) : string;
    function IsType(_word : string) : boolean;
    function IsTypeEx(_word : string) : boolean;
    function isConstNumber(_item : TPascalTokenItem) : Boolean;
    function isConst(_item : TPascalTokenItem) : Boolean;

    constructor Create(_tokenizer : TPascalTokenizer); reintroduce;
    destructor Destroy; override;

    procedure setVarType(name :string; varType : string);

    property Root : TGSCRootEx read GetRootASRootEx;
  end;



implementation


{ TGSCNode_checkSyntax_FormulaCheck }

procedure TGSCNode_checkSyntax_FormulaCheck.InternalProcess;
var p : integer;
    op : integer;
    formulaItem : TS_VarAssignation_PascalFormula;
    f : string;
    ll : TPascalTokenItemArray;

    procedure addArrayNode(aItem : TPascalTokenItem);
    begin
      f := f + aItem.item;
      SetLength(ll,length(ll)+1);
      ll[length(ll)-1] := aItem;
    end;

var lastOp : string;
begin
  lr := TGSCRootEx(froot);
  local := lr.Tokenizer.Current;
  //Back to business by default.
  Next := TGSCNode_checkSyntax_CodeAnalyze.Create(lr,'Code analyze fellowing formula check.');

  if local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon then
  begin
    lr.log('SyntaxCheck/FormulaCheck/ error : Empty assign');
    Exit;
  end;


  p := 0;
  op := 0;
  f := '';
  while not(( local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon) or (local.item = 'end')) do
  begin
    if local.item = '(' then
    begin
      addArrayNode(local);
      inc(p)
    end
    else
    if local.item = ')' then
    begin
      addArrayNode(local);
      dec(p)
    end
    else
    if local.item = ',' then
    begin
      if p>0 then
      begin
        addArrayNode(local);
      end
      else
      begin
        lr.log('SyntaxCheck/FormulaCheck/ error : comma (as method''s spearator) misplaced. "'+local.item+'"');
        op := 1; break;
      end;
    end
    else
    if (local.item = '+') or
       (local.item = '-') or
       (local.item = '*') or
       (local.item = '/') then
    begin
      if op=1 then
      begin
        if lastOp = local.item then
        begin
          op:=2;
          lastOp := '';
          addArrayNode(local);
        end
        else
        begin
          lr.log('SyntaxCheck/FormulaCheck/ error : Unexpected operation statement in formula "'+local.item+'"');
          op := 1; break;
        end;
      end
      else
      begin
        lastOp := local.item;
        addArrayNode(local);
        op := 1;
      end;
    end
    else
    if lr.SyntaxCheck.isConstNumber(local) then
    begin
      if op=2 then
       begin
         lr.log('SyntaxCheck/FormulaCheck/ error : Missing operator');
         op := 1; break;
       end;
      addArrayNode(local);
      op := 0;
    end
    else
    if local.item = '^' then
    begin
      if op=2 then
       begin
         lr.log('SyntaxCheck/FormulaCheck/ error : Missing operator');
         op := 1; break;
       end;
      addArrayNode(local);
      op := 0;
    end
    else
    if (local.PascalTokenType = TPascalTokenType.pttword) then
    begin
       if op=2 then
       begin
         lr.log('SyntaxCheck/FormulaCheck/ error : Missing operator');
         op := 1; break;
       end;

      if (lr.SyntaxCheck.IsType(local.item)) or
          (lr.SyntaxCheck.IsPascalKeyWord(local.item)) then
       begin
         lr.log('SyntaxCheck/FormulaCheck/ error : Unexpected type/langage statement in formula "'+local.item+'"');
         op := 1; break;
       end
       else
       begin
         if lr.SyntaxCheck.IsFreeWord(local.item) then
         begin
           //Note : form "var : Integer :=" is sub formula. Not supported yet.
           lr.log('SyntaxCheck/FormulaCheck/ error : Unknown element in formula "'+local.item+'"');
           op := 1; break;
         end
         else
         begin
//------------>  Next := TGSCNode_checkSyntax_Call_Method.Create(lr,local.item);
// transferer ce code de checksyntax de method localement ?
           addArrayNode(local);
           op := 0;
         end;
       end;
    end
    else
    begin
      lr.log('SyntaxCheck/FormulaCheck/ error : Unknown statement in formula "'+local.item+'"');
      op := 1; break;
    end;
    NextToken;
  end;

  while not(( local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon) or (local.item = 'end')) do
    NextToken;


  if p<>0 then
  begin
    lr.log('SyntaxCheck/FormulaCheck/ error : parenthesis open/close discrepancy');
  end;
  if op=1 then
  begin
    lr.log('SyntaxCheck/FormulaCheck/ error : operand syntax error');
  end;

  if (p=0) and ((op=0) or (op=2)) then
  begin
    /// Here : Formula is syntaxicali correct : Transfert it to semantic system (type check, suffix pass).
    formulaItem := TS_VarAssignation_PascalFormula.create(NodeName,f,ll);
    lr.SemanticProcessor.AddElement(formulaItem);
  end;
end;

{ TPascalSyntaxCheck }

procedure TPascalSyntaxCheck.AddNewVar(name: string;const varType : String = '');
var l : TPascalSyntaxVarItem;
begin
  if varList.IndexOf(name)=-1 then
  begin
    l := TPascalSyntaxVarItem.Create;
    l.varname := name;
    l.vartype := vartype;
    varList.AddObject(name,l);
  end
  else
    raise Exception.Create('TPascalSyntaxCheck.AddNewVar : Variable "'+name+'" already exists.');
end;

constructor TPascalSyntaxCheck.Create(_tokenizer : TPascalTokenizer);
var lroot : TGSCRootEx;
begin
  lroot := TGSCRootEx.Create;
  inherited Create(lroot, TWordTokenizer(_tokenizer));
  lroot.Tokenizer := _tokenizer;
  lroot.SyntaxCheck := self;
  projectHeader := TGSCNode_checkPascalProjectHeader.Create(froot, 'Project header check');
  Start.First := projectHeader;
  varList := TStringList.Create;
  MethodList := TStringList.Create;
  MethodList.Add('println');
  MethodList.Add('print');
  MethodList.Add('writeln');
  MethodList.Add('write');
  MethodList.Add('cos');
  MethodList.Add('pow');
end;


destructor TPascalSyntaxCheck.destroy;
var i : integer;
begin
  for i :=0 to varList.Count-1 do
    varList.Objects[i].Free;
  freeandNil(varList);
  FreeAndNil(MethodList);
  inherited;
end;

function TPascalSyntaxCheck.GetRootASRootEx: TGSCRootEx;
begin
  result := TGSCRootEx(froot);
end;

function TPascalSyntaxCheck.IsFreeWord(_word: string): boolean;
begin
  result := Not(IsPascalKeyWord(_word)) And
            Not(IsType(_word)) And
            Not(IsVar(_word)) And
            Not(IsPascalRegisteredFunction(_Word)) And
            Not(IsTypeEx(_word));
end;

function TPascalSyntaxCheck.isConstNumber(_item: TPascalTokenItem): Boolean;
begin
  result := (_item.PascalTokenType = TPascalTokenType.pttnumberConstInt) or
            (_item.PascalTokenType = TPascalTokenType.pttnumberConstFloat);
end;

function TPascalSyntaxCheck.isConst(_item: TPascalTokenItem): Boolean;
begin
  result := (_item.PascalTokenType = TPascalTokenType.pttnumberConstInt) or
            (_item.PascalTokenType = TPascalTokenType.pttnumberConstFloat) or
            (_item.PascalTokenType = TPascalTokenType.pttstringConst);
end;

function TPascalSyntaxCheck.IsPascalKeyWord(_word: string): boolean;
var i : integer;
begin
  result := false;
  for I := 1 to length(CST_TPascalNativeKeyWord)-2 do  //1 to length-2 to jump uknown and extention.
    if _word = CST_TPascalNativeKeyWord[TPascalNativeKeyWord(i)] then
    begin
      result := true;
      break;
    end;
end;

function TPascalSyntaxCheck.IsPascalRegisteredFunction(_word: string): boolean;
begin
  result := MethodList.IndexOf(_word)>-1;
end;

function TPascalSyntaxCheck.IsType(_word: string): boolean;
var i : integer;
begin
  result := false;
  for I := 1 to length(CST_TPascalNativeType)-2 do  //1 to length-2 to jump uknown and extention.
    if _word = CST_TPascalNativeType[TPascalNativeType(i)] then
    begin
      result := true;
      break;
    end;
end;

function TPascalSyntaxCheck.IsTypeEx(_word: string): boolean;
var i : integer;
begin
  result := false;
  for I := 1 to length(CST_TPascalNativeTypeExt)-2 do //1 to length-2 to jump uknown and extention.
    if _word = CST_TPascalNativeTypeExt[TPascalNativeTypeExt(i)] then
    begin
      result := true;
      break;
    end;
end;

function TPascalSyntaxCheck.IsVar(_word: string): boolean;
begin
  result := varList.IndexOf(_word)>-1;
end;

procedure TPascalSyntaxCheck.setVarType(name, varType: string);
var idx : integeR;
    l : TPascalSyntaxVarItem;
begin
  idx := varList.IndexOf(name);
  if idx>-1 then
  begin
    l := TPascalSyntaxVarItem(varList.Objects[idx]);
    l.vartype := vartype;
  end
  else
  begin
    AddNewVar(name,vartype);
  end;
end;

function TPascalSyntaxCheck.VarType(_word: string): string;
var i : integer;
begin
  i := varList.IndexOf(_word);
  if i>-1 then
  begin
    result := TPascalSyntaxVarItem(varList.Objects[i]).vartype;
  end
  else
  begin
    raise Exception.Create('"'+_word+'" not a type.');
  end;
end;

{ TGSCNode_checkPascalProjectHeader }

procedure TGSCNode_checkPascalProjectHeader.InternalProcess;
var local : TPascalTokenItem;
    lr : TGSCRootEx;
    lpn : string;
begin
  lr := TGSCRootEx(froot);
  local := lr.Tokenizer.Next;
  Next := TGSCNode_checkPascalProjectHeader.Create(lr,'header, new tentative'); //fail case.

  if (lr.SyntaxCheck.isPascalKeyWord(local.item)) and (local.item = 'program') then
  begin
    local := lr.Tokenizer.Next;
    if (local.PascalTokenType = TPascalTokenType.pttword) and (lr.SyntaxCheck.IsFreeWord(local.item)) then
    begin
      lpn := local.item;
      local := lr.Tokenizer.Next;
      if not(local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon) then
      begin
        lr.log('";" required after program name.');
      end
      else
      begin
        lr.log('[OK] Program "'+lpn+'" opened.');
        local := lr.Tokenizer.Next;
      end;
    end;
  end;

  if lr.SyntaxCheck.isPascalKeyWord(local.item) then
  begin
    if local.item = 'uses' then
      Next := TGSCNode_checkProgramUses.Create(lr,'program main "Uses" part',self)
    else
    if local.item = 'type' then
      Next := TGSCNode_checkProgramType.Create(lr,'program main "types" part',self)
    else
    if local.item = 'var' then
      Next := TGSCNode_checkProgramVar.Create(lr,'program main "var" part',self)
    else
    if local.item = 'begin' then
      Next := TGSCNode_checkProgramBegin.Create(lr,'program main "begin" part',self)
    else
    begin
      lr.log('Expected uses, types, var or begin part : "'+local.item+'" encountered');
    end;
  end
  else
  begin
    lr.log('Header statement : unkown expression "'+local.item+'"');
  end;

end;


{ TGSCNode_checkProgramUses }

procedure TGSCNode_checkProgramUses.InternalProcess;
begin
  //TODO !
  Next := TGSCNode_checkProgramType.Create(froot,'Type checking...');
end;

{ TGSCNode_checkProgramType }

procedure TGSCNode_checkProgramType.InternalProcess;
begin
  //TODO !
  Next := TGSCNode_checkProgramConst.Create(froot,'const checking...');
end;

{ TGSCNode_checkProgramVar }

Procedure TGSCNode_checkProgramVar.InternalProcess;
var local,saved : TPascalTokenItem;
    lr : TGSCRootEx;
    localNewVar : TStringList;
    i : integer;
begin
  localNewVar := TStringList.Create;
  try

    lr := TGSCRootEx(froot);
    local := lr.Tokenizer.Current;
    if (lr.SyntaxCheck.isPascalKeyWord(local.item)) and (lowercase(local.item) = 'var') then
    begin
      local := lr.Tokenizer.Next;
      if (local.PascalTokenType = TPascalTokenType.pttword) and (lr.SyntaxCheck.IsFreeWord(local.item)) then
      begin
        //define var loop...

        while not((lr.SyntaxCheck.IsPascalKeyWord(local.item))) do
        begin

          while (local.PascalTokenType = TPascalTokenType.pttword) or
                (local.PascalSymbolTokenType = TPascalSymbolTokenType.pttComma) do
          begin
            if (local.PascalTokenType = TPascalTokenType.pttword) then
            begin
              if lr.SyntaxCheck.IsFreeWord(local.item) And (localNewVar.IndexOf(local.item)=-1) then
              begin
                //Variable defines !
                localNewVar.Add(local.item);
              end
              else
              begin
                lr.log('cannot use "'+local.item+'" as a var name : already defined or reserved.');
                //exit;
              end;
            end;

            local := lr.Tokenizer.Next;
          end;

          //":" before type...
          if Not(local.PascalSymbolTokenType = TPascalSymbolTokenType.ptt2Point) then
          begin
            lr.log('var enumeration error : unexpected symbol "'+local.item+'"');
            exit;
          end
          else
          begin
            saved := local;
            local := lr.Tokenizer.Previous;
            if local.PascalSymbolTokenType = TPascalSymbolTokenType.pttComma then
            begin
              lr.log('too many comma, or missed a var specification "'+local.item+'"');
              exit;
            end;
            lr.Tokenizer.SetCursorTo(saved);
          end;

          local := lr.Tokenizer.Next;
          if (local.PascalTokenType = TPascalTokenType.pttword) and (lr.SyntaxCheck.IsType(local.item)) then
          begin
            //Type ok
            //--> Assign type.
            for i:= 0 to localNewVar.Count-1 do
            begin
              lr.SyntaxCheck.setVarType(localNewVar[i],local.item);
              lr.log('[OK] New var added ("'+localNewVar[i]+'/'+local.item+'")');

              //Final var Name and type definition -> Add to semantic system.
              lr.SemanticProcessor.AddElement(TS_VarDecl.create(localNewVar[i],local.item));
            end;
            localNewVar.Clear;

            local := lr.Tokenizer.Next;
            if local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon then
              local := lr.Tokenizer.Next
            else
            begin
              lr.log('";" expected after type definition. "'+local.item+'" found.');
              exit;
            end;
          end
          else
          begin
            lr.log('[ERROR] Type not found "'+local.item+'"');
            exit;
          end;
        end; //endof while true.

        if (local.item = 'type') then
        begin
          Next := TGSCNode_checkProgramType.Create(lr,'type check');
        end
        else
        if (local.item = 'const') then
        begin
          Next := TGSCNode_checkProgramConst.Create(lr,'const check');
        end
        else
        if (local.item = 'var') then
        begin
          Next := TGSCNode_checkProgramVar.Create(lr,'var check');
        end
        else
        if local.item = 'begin' then
        begin
          Next := TGSCNode_checkProgramBegin.Create(lr,'Begin seek...');
        end
        else
        begin
          lr.log('Unexptected key word found after var statement. Found "'+local.item+'"');
        end;
      end;
    end;

  finally
    FreeAndNil(localNewVar);
  end;
end;

{ TGSCNode_checkProgramConst }

procedure TGSCNode_checkProgramConst.InternalProcess;
begin
  //TODO !
  Next := TGSCNode_checkProgramVar.Create(froot,'Var checking...');
end;


{ TGSCNode_checkProgramBegin }

procedure TGSCNode_checkProgramBegin.InternalProcess;
var local : TPascalTokenItem;
    lr : TGSCRootEx;
begin
  lr := TGSCRootEx(froot);
  local := lr.Tokenizer.Current;
  if (lr.SyntaxCheck.isPascalKeyWord(local.item)) and (lowercase(local.item) = 'begin') then
  begin
    //begin !

    //check if code is empty.
    local := lr.Tokenizer.Next;
    if (local.item = 'end') then
    begin
      local := lr.Tokenizer.Next;
      if (local.item = '.') then
      begin
        Next := lr.SyntaxCheck.Finish;
      end;
    end
    else
    begin
      Next := TGSCNode_checkSyntax_CodeAnalyze.Create(lr,'Main code analyze.');
    end;
  end
  else
  begin
    lr.log('excpected "begin", "'+local.item+' found');
  end;
end;


{ TGSCNode_checkSyntax_CodeAnalyze }

procedure TGSCNode_checkSyntax_CodeAnalyze.InternalProcess;
var saved : TPascalTokenItem;
    lv : string; //var name.
    currentExpr : string;

    procedure CheckAffectationCase;
    var llo : string; //local name.
    begin
      llo := local.item;
      NextToken;
      if local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon then
        exit;
      //Error case : reassignement, or other illegal assignement statement.
      if (isSequenceConforme([TPascalSymbolTokenType.ptt2Point,TPascalSymbolTokenType.pttNA])) then
      begin
        local := lr.Tokenizer.SetCursorTo(saved);
        NextToken; NextToken;
        lr.log('SyntaxCheck/Code analyze : Affectation : Incorrect symbol for "'+llo+'" var assignement');
        if  lr.SyntaxCheck.IsType(local.item) then
          lr.log('SyntaxCheck/Code analyze : Affectation : --> Variable "'+llo+'" tentatively redefined. Fatal.');
        next := nil;
      end
      else
      begin
        local := lr.Tokenizer.SetCursorTo(saved);
        NextToken;
        if (isSequenceConforme([TPascalSymbolTokenType.ptt2Point,TPascalSymbolTokenType.pttEqual])) then
        begin
          Next := TGSCNode_checkSyntax_CodeAnalyze_VarAffectation.Create(lr,lv,self);
        end
        else
        begin
          local := lr.Tokenizer.SetCursorTo(saved);
          NextToken;
          //check add (+=);
          if (isSequenceConforme([TPascalSymbolTokenType.pttPlus,TPascalSymbolTokenType.pttEqual])) then
          begin
            Next := TGSCNode_checkSyntax_CodeAnalyze_VarAffectation.Create(lr,lv,self);
            TGSCNode_checkSyntax_CodeAnalyze_VarAffectation(Next).op := TVarAffectationOp.vaoPlus;
          end
          else //check less (-=);
          begin
            local := lr.Tokenizer.SetCursorTo(saved);
            NextToken;
            if (isSequenceConforme([TPascalSymbolTokenType.pttMinus,TPascalSymbolTokenType.pttEqual])) then
            begin
              Next := TGSCNode_checkSyntax_CodeAnalyze_VarAffectation.Create(lr,lv,self);
              TGSCNode_checkSyntax_CodeAnalyze_VarAffectation(Next).op := TVarAffectationOp.vaoMinus;
            end
            else //check mul (*=) (squared.)
            begin
              local := lr.Tokenizer.SetCursorTo(saved);
              NextToken;
              if (isSequenceConforme([TPascalSymbolTokenType.pttMul,TPascalSymbolTokenType.pttEqual])) then
              begin
                Next := TGSCNode_checkSyntax_CodeAnalyze_VarAffectation.Create(lr,lv,self);
                TGSCNode_checkSyntax_CodeAnalyze_VarAffectation(Next).op := TVarAffectationOp.vaoMul;
              end
              else
              begin
                local := lr.Tokenizer.SetCursorTo(saved);
                Next := TGSCNode_checkSyntax_FormulaCheck.Create(lr,'RESULT',self);
              end;
            end;
          end
        end;
      end;
    end;


begin
  lr := TGSCRootEx(froot);
  local := lr.Tokenizer.Current;

  while (local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon) do
    NextToken;

  //Analyse "possimpable" universe of code possibility.
  saved := local;

  //check normal end of code.
  if (local.item = 'end') then
  begin
    local := lr.Tokenizer.Next;
    if (local.item = '.') then
    begin
      Next := lr.SyntaxCheck.Finish;
      exit;
    end;
    lr.Tokenizer.SetCursorTo(saved);
    local := saved;
  end;

  //check abnormal eof.
  if lr.Tokenizer.IsLast then
  begin
    lr.log('SyntaxCheck/Code analyze : Reached end of code without markers.');
    Next := nil;
    exit;
  end;

  Next := TGSCNode_checkSyntax_CodeAnalyze.Create(lr,'');

  currentExpr := local.item;
  lv := local.item;
  //var related
  if lr.SyntaxCheck.IsVar(local.item) then //we have a var.
  begin
    CheckAffectationCase;
  end
  else
  if lr.SyntaxCheck.isConstNumber(local) then
  begin
    local := lr.Tokenizer.SetCursorTo(saved);
    Next := TGSCNode_checkSyntax_FormulaCheck.Create(lr,'RESULT',self);
  end
  else
  if lr.SyntaxCheck.IsFreeWord(local.item) then
  begin
    local := lr.Tokenizer.SetCursorTo(saved);
    NextToken;
    if isSequenceConforme([TPascalSymbolTokenType.ptt2Point,TPascalSymbolTokenType.pttEqual]) then
    begin
      lv := saved.item;
      lr.log('SyntaxCheck/Code analyze : Assignement : unknown or incorrect statement "'+lv+'"');
    end
    else
    begin
      //check create var on the fly before affectation.
      local := lr.Tokenizer.SetCursorTo(saved);
      NextToken;
      if local.PascalSymbolTokenType = TPascalSymbolTokenType.ptt2Point then
      begin
        nextToken;
        if lr.SyntaxCheck.IsType(local.item) then
        begin
          lr.SyntaxCheck.setVarType(saved.item,local.item);
          lr.log('[OK] New var added dynamically ("'+saved.item+'/'+local.item+'")');
          //Final var Name and type definition -> Add to semantic system.
          lr.SemanticProcessor.AddElement(TS_VarDecl.create(saved.item,local.item));
          saved := local;
          CheckAffectationCase;


{          nextToken;
          if isSequenceConforme([TPascalSymbolTokenType.ptt2Point,TPascalSymbolTokenType.pttEqual]) then
          begin
            Next := TGSCNode_checkSyntax_CodeAnalyze_VarAffectation.Create(lr,saved.item,self);
          end
          else
          begin
            local := lr.Tokenizer.SetCursorTo(saved);
            NextToken;
            NextToken;
            NextToken;
            if local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon then
            begin
              NextToken; //end of instruction. continue analyzing.
              Next := TGSCNode_checkSyntax_CodeAnalyze.Create(lr,'');
            end
            else
            begin
              lr.log('SyntaxCheck/Code analyze : Dynamic var creation need ";" to be effective, or fellowed by an assignation. "'+local.item+'"');
            end;
          end;
}
        end
        else
        begin
          lr.log('SyntaxCheck/Code analyze : Dynamic var creation need type "'+local.item+'"');
        end;
      end
      else
      begin
        lr.log('SyntaxCheck/Code analyze : Unkown expression "'+currentExpr+'"');
      end;
    end;
  end
  else
  if lr.SyntaxCheck.IsPascalRegisteredFunction(local.item) then
  begin
    CheckAffectationCase;
  end
  else
  begin
    lr.log('SyntaxCheck/Code analyze : Unkown litteral "'+local.item+'"');
  end;
end;


function TGSCNode_checkSyntax_CodeAnalyze.isSequenceConforme(
  _data: Array of TPascalSymbolTokenType): boolean;
var i : integer;
begin
  result := true;
  for i := Low(_data) to High(_data) do
  begin
    if local.PascalSymbolTokenType = _data[i] then
      NextToken
    else
    begin
      result := false;
      break;
    end;
  end;
end;

procedure TGSCNode_checkSyntax_CodeAnalyze.NextToken;
begin
  local := lr.Tokenizer.Next;
end;

procedure TGSCNode_checkSyntax_CodeAnalyze.PreviousToken;
begin
  local := lr.Tokenizer.Previous;
end;

{ TGSCNode_checkSyntax_CodeAnalyze_VarAffectation }

procedure TGSCNode_checkSyntax_CodeAnalyze_VarAffectation.InternalProcess;
var s : TPascalTokenItem;
    fFormula : boolean;
    saved : TPascalTokenItem;

begin
  lr := TGSCRootEx(froot);
  local := lr.Tokenizer.Current;


  //Back to business by default.
  Next := TGSCNode_checkSyntax_CodeAnalyze.Create(lr,'Code analyze fellowing var affectation.');
  s := local;

  if lr.Tokenizer.IsLast then
  begin
    lr.log('unexpected end of file in affectation.');
    exit;
  end;

  fFormula := true;
  //Affectation code.
  saved := local;

  if op = TVarAffectationOp.vaoAssign then
    case local.PascalTokenType of
      pttnumberConstInt:
      begin
        NextToken;
        if (local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon) or (local.Item = 'end') then
        begin
          //TODO if result Generate INTEGER assignation. (Check type --> Semantic.)
          fFormula := false;
          lr.SemanticProcessor.AddElement(TS_VarAssignation_PascalconstInteger.create(NodeName,strToInt(saved.item)));
          lr.log(format('[OK] SyntaxCheck/CodeAnalyze_VarAffectation : Integer assignation %s %d',[NodeName, strToInt(saved.item)]));
        end
      end;
      pttnumberConstFloat:
      begin
        NextToken;
        if (local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon) or (local.item = 'end') then
        begin
          fFormula := false;
          //if result Generate FLOAT assignation. (Check type --> Semantic.)
          lr.SemanticProcessor.AddElement(TS_VarAssignation_PascalConstFloat.create(NodeName,StrToFloat(saved.item)));
          lr.log(format('[OK] SyntaxCheck/CodeAnalyze_VarAffectation : double assignation %s %d',[NodeName, StrToFloat(saved.item)]));
        end;
      end;
      pttstringConst:
      begin
        NextToken;
        if (local.PascalSymbolTokenType = TPascalSymbolTokenType.pttSemiColon) or (local.Item = 'end') then
        begin
          fFormula := false;
          //if result Generate STRING assignation. (Check type --> Semantic.)
          lr.SemanticProcessor.AddElement(TS_VarAssignation_PascalconstString.create(NodeName,saved.item));
          lr.log(format('[OK] SyntaxCheck/CodeAnalyze_VarAffectation : String assignation %s %d',[NodeName, saved.item]));
        end;
      end;
    end;

  if fFormula then
  begin
    local := lr.Tokenizer.SetCursorTo(s);
    //Generate FORMULA (COMPLEX) assignation(operation (:= +=, NONE,  and so on) --> Semantic.
    Next := TGSCNode_checkSyntax_FormulaCheck.Create(lr,NodeName,self);
  end;
end;


{ TGSCNode_checkSyntax_Call_Method }

procedure TGSCNode_checkSyntax_Call_Method.InternalProcess;
var equ : integer;
    doCall : boolean;
    Params,P : TStringList;
    i : integer;
begin
  lr := TGSCRootEx(froot);
  local := lr.Tokenizer.Current;
  Params := TStringList.Create;
  P := TStringList.Create;
  try

  //Back to business by default.
  Next := TGSCNode_checkSyntax_CodeAnalyze.Create(lr,'Code analyze fellowing '+nodeName+' call.');

  equ := 0;
  doCall := true;
  NextToken;
  while ((local.PascalSymbolTokenType <> TPascalSymbolTokenType.pttSemiColon) and (local.item <> 'end')) and Not(lr.Tokenizer.IsLast) do
  begin
    if local.item = '(' then
    begin
      if equ>1 then
        P.AddObject(local.item,local);
      inc(equ);
    end
    else
    if local.item = ')' then
    begin
      dec(equ);
      if equ>1 then
        P.AddObject(local.item,local);
    end
    else
    if lr.SyntaxCheck.IsPascalKeyWord(local.item) then
    begin
      lr.log('SyntaxCheck/method call : Abnormal key word presence in call '+local.item);
      doCall := false;
    end
    else
    begin
      if equ>0 then
      begin
        if local.PascalSymbolTokenType = TPascalSymbolTokenType.pttComma then
        begin
          if P.Count>0 then
          begin
            for i := 0 to P.Count-1 do
              Params.AddObject(P[i],P.Objects[i]);
            p.Clear;
          end;
        end
        else
        begin
          P.AddObject(local.item,local)
        end
      end
      else
      begin
        lr.log('SyntaxCheck/method call : Syntax error/'+NodeName+' call : unknown statement after "'+local.item+'"');
        doCall := false;
      end;
    end;

    NextToken;
  end;

  //last param.
  if P.Count>0 then
  begin
    for i := 0 to P.Count-1 do
      Params.AddObject(P[i],P.Objects[i]);
    p.Clear;
  end;


  if equ<>0 then
    lr.log('SyntaxCheck/method call : Syntax error/'+NodeName+' call : Parenthesis mismatch');


  doCall := doCall And (equ=0);
  if doCall then
  begin
    //TODO : Resolve all parameter *before* semantic transfert.
    //Semantic Generate call + parameter list.
    lr.log('[OK] SyntaxCheck/method "'+NodeName+'" related call : Semantic generated. ('+intToStr(Params.Count)+' parameter(s))');
  end
  else
  begin
    lr.log('SyntaxCheck/method call : print call not generated.');
  end;


  finally
    FreeAndNil(params);
    FreeAndNil(P);
  end;
end;

initialization

  FormatSettings.DecimalSeparator := '.';

end.
