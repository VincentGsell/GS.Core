unit GS.Language.Formula;

interface

uses classes, SysUtils, contnrs,
     GS.Language.WordTokenizer,
     GS.Language.AlgoProgram,
     GS.Language.VM;

Type

//this class is used for above formulaseolver, and in compiler. Will be inherited for each compiler.
//WARNING : Formula MUST be syntaxicaly/semanticaly correct ! No check provide here.
TFormulaParser = Class
const
  CST_LEADING_LEFT = 'l';
  CST_LEADING_RIGHT = 'r';

type
  TFormulaLexem = Class
  public
    ItemType : string;
    ItemValue : string;
    TokenLink : TTokenItem;
    constructor Create(_TokenLink : TTokenItem; _type : string; value : string); virtual;
  End;

  TFNumber = Class(TFormulaLexem)
  public
    Value : double;
    constructor Create(_TokenLink : TTokenItem; _value : double); reintroduce;
  End;

  TFOperator = Class(TFormulaLexem)
    postFix : string;
    priority : byte;
    leading : char;
    constructor Create(_TokenLink : TTokenItem; _value : string); reintroduce;
  End;

  TFParenthesis = class(TFormulaLexem)
  public
    priority : byte;
    constructor Create(_TokenLink : TTokenItem; _value : string); reintroduce;
  end;

  TFFunc = Class(TFormulaLexem)
  public
    funcName : string;
    priority : byte;
    leading : char;
    constructor Create(_TokenLink : TTokenItem; _value : string); reintroduce;
  End;

  TFVar = Class(TFormulaLexem)
  public
    VarName : string;
    constructor Create(_TokenLink : TTokenItem; _value : string); reintroduce;
  End;


  TFormulaLexemArray = array of TFormulaLexem;
private
protected
  fstack : TObjectStack;
  FLastError: String;
public
  constructor Create; virtual;
  destructor Destroy; Override;
  function ParseFormula(infixedFormula : TTokenItemArray; var postFixedSimplifiedFormula :  TTokenItemArray) : boolean;
  function TokenItemsToString(items : TTokenItemArray) : String; //debug purpose.

  property LastError : String read FLastError;
End;


implementation


{ TFormulaParser }

constructor TFormulaParser.Create;
begin
  inherited create;
  fstack := TObjectStack.Create;
end;

destructor TFormulaParser.Destroy;
begin
  fstack.Free;
  inherited;
end;

function TFormulaParser.ParseFormula(infixedFormula: TTokenItemArray;
  var postFixedSimplifiedFormula: TTokenItemArray): boolean;
var i : integer;
    ll : TObjectList;
    l : TFormulaLexemArray;
    temp : TFormulaLexem;

    procedure addToResult(item : TTokenItem);
    var arl : integer;
    begin
      arl := length(postFixedSimplifiedFormula);
      SetLength(postFixedSimplifiedFormula,arl+1);
      postFixedSimplifiedFormula[arl] := item;
    end;

    procedure TokenArrayToLexemList;
    var i : integer;
    begin
      for i := Low(infixedFormula) to High(infixedFormula) do
      begin
        case infixedFormula[i].TokType of
          TTokenType.ttsymbol :
          begin
            if (infixedFormula[i].item = '(') or (infixedFormula[i].item = ')') then
            begin
              ll.Add(TFParenthesis.Create(infixedFormula[i],infixedFormula[i].item));
            end
            else
            if (infixedFormula[i].item = '+') or
            (infixedFormula[i].item = '-') or
            (infixedFormula[i].item = '*') or
            (infixedFormula[i].item = '/') or
            (infixedFormula[i].item = '^') then
            begin
              ll.Add(TFOperator.Create(infixedFormula[i],infixedFormula[i].item));
            end
            else
            begin
              FLastError := 'Unknown operator : '+infixedFormula[i].item;
            end;
          end;
          TTokenType.ttnumber :
          begin
            ll.Add(TFNumber.Create(infixedFormula[i],StrToFloat(infixedFormula[i].item)));
          end;
          TTokenType.ttword :
          begin
            if trim(lowercase(infixedFormula[i].item)) = 'pow' then
            begin
              //TODO :
              //1) add all parsed parameter into list of para.
              //2) Each parameter is a lexemlist.
              ll.Add(TFFunc.Create(infixedFormula[i],infixedFormula[i].item));
            end
            else
              ll.Add(TFVar.Create(infixedFormula[i],infixedFormula[i].item));
          end;
        end;
      end;
    end;

begin
  assert(length(infixedFormula)>0);
  result := false;
  FLastError := '';
  ll := TObjectList.Create;
  try

    //transfert token based formula into lexem based one.
    TokenArrayToLexemList;

    SetLength(l,ll.Count);
    for i := 0 to ll.Count-1 do
    begin
      l[i] := TFormulaLexem(ll[i]);
    end;

    //Suffix based writing into stack or out.
    for i := low(l) to high(l) do
    begin
      if (l[i] is TFParenthesis) then
      begin
        if l[i].ItemValue = '(' then
          fstack.Push(l[i])
        else
        begin
          temp := TFormulaLexem(fstack.pop);
          while temp.ItemValue <> '(' do
          begin
            addToResult(temp.TokenLink);
            temp := TFormulaLexem(fstack.Pop);
          end;
        end;
      end
      else
      if (l[i] is TFNumber) or  (l[i] is TFVar) then
      begin
        addToResult(l[i].TokenLink);
      end
      else
      if (l[i] is TFOperator) then
      begin
        if fstack.Count>0 then
        begin
          temp := TFormulaLexem(fstack.Peek);
          if (temp is TFOperator) then
          begin
            if (TFOperator(temp).priority>TFOperator(l[i]).priority) then
            begin
              //Opertor in stack has higher priority : swap.
              temp := TFormulaLexem(fstack.Pop);
              addToResult(temp.TokenLink);
              fstack.Push(l[i]);
            end
            else
            if (TFOperator(temp).priority=TFOperator(l[i]).priority) then
            begin
              if (TFOperator(temp).leading=TFOperator(l[i]).leading) then
              begin
                //Opertor in stack has same priority and same leading : swap (left)   TODO RIGHT (^)
                temp := TFormulaLexem(fstack.Pop);
                addToResult(temp.TokenLink);
                fstack.Push(l[i]);
              end
              else
              begin
                fstack.Push(l[i]);
              end;
            end
            else
            begin //lower priority.
              fstack.Push(l[i]);
            end;
          end
          else
          begin //no operator on top of stack : Stack current operator.
            fstack.Push(l[i]);
          end;
        end
        else
        begin//Stack empty.
          fstack.Push(l[i]);
        end;
      end
      else
      if (l[i] is TFFunc) then //TODO !
      begin
        fstack.Push(l[i]);
      end
      else
      begin
        FLastError := 'Class not processed : '+l[i].ClassName;
      end;
    end;

    //comlete out, by emptying stack.
    while fstack.Count>0 do
      addToResult(TFormulaLexem(fstack.Pop).TokenLink);

  finally
    freeAndNil(ll); //Free all math lexem.
  end;
  result := FLastError='';
end;

function TFormulaParser.TokenItemsToString(items: TTokenItemArray): String;
var i : integer;
begin
  result := '';
  for i := Low(items) to High(items) do
    if result = '' then
      result := items[i].item
    else
      result := result + '|' + items[i].item;
end;

{ TFormulaParser.TFormulaLexeme }

constructor TFormulaParser.TFormulaLexem.Create(_TokenLink : TTokenItem; _type: string; value: string);
begin
  inherited create;
  ItemType := _type;
  ItemValue :=  value;
  TokenLink := _TokenLink;
end;

{ TFormulaParser.TFNumber }

constructor TFormulaParser.TFNumber.Create(_TokenLink : TTokenItem; _value : double);
begin
  inherited Create(_TokenLink,'n',FloatToStr(_Value));
  Value :=_value;
end;

{ TFormulaParser.TFOperator }

constructor TFormulaParser.TFOperator.Create(_TokenLink: TTokenItem;
  _value: string);
begin
  inherited Create(_TokenLink,'op',_Value);
  if _value = '+' then
  begin
    postFix := '+';
    priority := 1;
    leading := CST_LEADING_LEFT;
  end
  else
  if _value = '-' then
  begin
    postFix := '-';
    priority := 1;
    leading := CST_LEADING_LEFT;
  end
  else
  if _value = '*' then
  begin
    postFix := '*';
    priority := 2;
    leading := CST_LEADING_LEFT;
  end
  else
  if _value = '/' then
  begin
    postFix := '/';
    priority := 2;
    leading := CST_LEADING_LEFT;
  end
  else
  if _value = '^' then
  begin
    postFix := '^';
    priority := 3;
    leading := CST_LEADING_RIGHT;
  end;
end;

{ TFormulaParser.TFParenthesis }

constructor TFormulaParser.TFParenthesis.Create(_TokenLink: TTokenItem;
  _value: string);
begin
  inherited Create(_TokenLink,'par',_Value);
  priority := 0;
end;

{ TFormulaParser.TFFunc }

constructor TFormulaParser.TFFunc.Create(_TokenLink: TTokenItem;
  _value: string);
begin
  inherited Create(_TokenLink,'f',_value);
  funcName := _value;
  priority := 10;
  leading := CST_LEADING_RIGHT;
end;

{ TFormulaParser.TFVar }

constructor TFormulaParser.TFVar.Create(_TokenLink: TTokenItem; _value: string);
begin
  inherited Create(_TokenLink,'v',_value);
  VarName := _value;
end;


end.
