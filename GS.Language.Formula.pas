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

  TFCommaSeparator = class(TFormulaLexem)
  public
    constructor Create(_TokenLink : TTokenItem; _value : string); reintroduce;
  end;

  TFPointSeparator = class(TFormulaLexem)
  public
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

  TFormulaLexemObjectList = class
  private
    FList : TObjectList;

    function GetItems(Index: Integer): TFormulaLexem;
    procedure SetItems(Index: Integer; const Value: TFormulaLexem);
    function Getcount: Uint32;

  public
    constructor Create; virtual;
    Destructor Destroy; override;

    Procedure Add(aLexem : TFormulaLexem);
    Procedure Remove(Index : Uint32);

    property Items[Index: Integer]: TFormulaLexem read GetItems write SetItems; default;
    property Count : Uint32 read Getcount;
  end;


private
  procedure InternalFormulaSpecificTokenizeImplicitMul(var aTokenizedArray : TTokenItemArray);
  procedure InternalFormulaSpecificTokenizeImplicitNeg(var aTokenizedArray : TTokenItemArray);
  procedure InternalTokenArrayToLexemList(var infixedFormula: TTokenItemArray; ll : TFormulaLexemObjectList);
  procedure InternalFilterLexemListAndTransfert(var LexemArray: TFormulaLexemArray; ll : TFormulaLexemObjectList);
  function InternalSeekForErronousStatement(var LexemArray: TFormulaLexemArray) : Boolean;

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

procedure TFormulaParser.InternalFormulaSpecificTokenizeImplicitMul( var aTokenizedArray: TTokenItemArray);
var i,j : integer;
    lTemp : TTokenItemArray;

    procedure localTransfert;
    begin
      j := length(ltemp);
      SetLength(ltemp,j+1);
      ltemp[j] := aTokenizedArray[i];
    end;

begin
  for i := Low(aTokenizedArray) to High(aTokenizedArray) do begin

    if (i>0) and
       (aTokenizedArray[i].item = '(') and
       ((aTokenizedArray[i-1].TokType = TTokenType.ttnumber) or
       (aTokenizedArray[i-1].TokType = TTokenType.ttword)) then  begin //var ?
      j := length(ltemp);
      SetLength(ltemp,j+1);
      ltemp[j] := TTokenItem.create(TTokenType.ttsymbol,'*',-1,-1);
      localTransfert;
    end
    else
    if (i<High(aTokenizedArray)) and
       (aTokenizedArray[i].item = ')') and
       ((aTokenizedArray[i+1].TokType = TTokenType.ttnumber) or
       (aTokenizedArray[i+1].TokType = TTokenType.ttword)) then  begin //var ?
      localTransfert;
      j := length(ltemp);
      SetLength(ltemp,j+1);
      ltemp[j] := TTokenItem.create(TTokenType.ttsymbol,'*',-1,-1);
    end
    else
      localTransfert;
  end;
  aTokenizedArray := lTemp;
end;

procedure TFormulaParser.InternalFormulaSpecificTokenizeImplicitNeg(
  var aTokenizedArray: TTokenItemArray);
begin
end;

function TFormulaParser.InternalSeekForErronousStatement(
  var LexemArray: TFormulaLexemArray): Boolean;
begin
  //
end;

procedure TFormulaParser.InternalTokenArrayToLexemList(var infixedFormula: TTokenItemArray; ll : TFormulaLexemObjectList);
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
        if (infixedFormula[i].item = ',') then
        begin
          ll.Add(TFCommaSeparator.Create(infixedFormula[i],infixedFormula[i].item));
        end
        else
        if (infixedFormula[i].item = '.') then
        begin
          ll.Add(TFPointSeparator.Create(infixedFormula[i],infixedFormula[i].item));
        end
        else
        begin
          if Not(infixedFormula[i].item.Trim.IsEmpty) then
            FLastError := 'Unknown operator : '+infixedFormula[i].item;
        end;
      end;
      TTokenType.ttnumber :
      begin
        if ll.count>0 then begin
          if ll[ll.Count-1] is TFPointSeparator then
            ll.Add(TFNumber.Create(infixedFormula[i],StrToFloat('0.'+infixedFormula[i].item)))  //@L->Abya
          else
            ll.Add(TFNumber.Create(infixedFormula[i],StrToFloat(infixedFormula[i].item)))
        end
        else
          ll.Add(TFNumber.Create(infixedFormula[i],StrToFloat(infixedFormula[i].item)))
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

procedure TFormulaParser.InternalFilterLexemListAndTransfert(var LexemArray: TFormulaLexemArray; ll : TFormulaLexemObjectList);
var i,j : integer;
    ltemp : TFormulaLexemArray;

    procedure localTransfert;
    begin
      j := length(ltemp);
      SetLength(ltemp,j+1);
      ltemp[j] := ll[i];
    end;
begin
  i := 0;
  While i<ll.Count do
  begin
    if (ll[i] is TFNumber) And (i<ll.count-1) then
    begin
      if ll[i+1] is TFPointSeparator then
      begin
        if (i+2<ll.Count) then
        begin
          if (ll[i+2] is TFNumber) then //Example : replace "10",".","5" lexems by "10.5" value.
          begin
            TFNumber(ll[i]).Value := TFNumber(ll[i]).Value + TFNumber(ll[i+2]).Value;  //@L<-Abya
            ll[i].ItemValue := FloatToStr(TFNumber(ll[i]).Value);
            ll[i].TokenLink.ChangeValueRequest(ll[i].ItemValue);
            localTransfert;
            i := i+2;
          end
          else begin //not a number
            //c style of writing floating value such as "a = 1."
            ll[i].ItemValue := ll[i].ItemValue+'.0';
            ll[i].TokenLink.ChangeValueRequest(ll[i].ItemValue);
            localTransfert;
            i := i+1;
          end;
        end
        else
        begin
          //c style of writing floating value such as "a = 1."
          ll[i].ItemValue := ll[i].ItemValue+'.0';
          ll[i].TokenLink.ChangeValueRequest(ll[i].ItemValue);
          localTransfert;
          i := i+1;
        end;
      end
      else
        localTransfert; //integer.
    end
    else
      localTransfert;  //All other case, just transfert.

    inc(i);
  end;

  //Transfert results into main list.
  SetLength(LexemArray,ll.Count);
  for i := 0 to ll.Count-1 do begin
    LexemArray[i] := ll[i];
  end;
end;

function TFormulaParser.ParseFormula(infixedFormula: TTokenItemArray;
  var postFixedSimplifiedFormula: TTokenItemArray): boolean;
var i : integer;
    ll : TFormulaLexemObjectList;
    l : TFormulaLexemArray;
    temp : TFormulaLexem;

    procedure addToResult(item : TTokenItem);
    var arl : integer;
    begin
      arl := length(postFixedSimplifiedFormula);
      SetLength(postFixedSimplifiedFormula,arl+1);
      postFixedSimplifiedFormula[arl] := item;
    end;

begin
  assert(length(infixedFormula)>0);
  result := false;
  FLastError := '';
  ll := TFormulaLexemObjectList.Create;
  try

    //Negative transformation (i.e. (-x) -> (0-x)
    InternalFormulaSpecificTokenizeImplicitNeg(infixedFormula);

    //Base mul transformation (i.e. a(b+c) -> a*(b+c)
    InternalFormulaSpecificTokenizeImplicitMul(infixedFormula);

    //transfert token based formula into lexem based one.
    InternalTokenArrayToLexemList(infixedFormula,ll);

    //clean and pretranslate token to resolve trivial type.
    InternalFilterLexemListAndTransfert(l,ll);


    if length(l)=0 then begin
      FLastError := 'formula empty';
      Exit;
    end;

    //Seek for erronous statement (duplicate, not allowed char...).
    result := InternalSeekForErronousStatement(l);

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


{ TFormulaParser.TFCommaSeparator }

constructor TFormulaParser.TFCommaSeparator.Create(_TokenLink: TTokenItem;
  _value: string);
begin
  inherited Create(_TokenLink,'comma',_Value);
end;

{ TFormulaParser.TFPointSeparator }

constructor TFormulaParser.TFPointSeparator.Create(_TokenLink: TTokenItem;
  _value: string);
begin
  inherited Create(_TokenLink,'pt',_Value);
end;

{ TFormulaLexemObjectList }

procedure TFormulaParser.TFormulaLexemObjectList.Add(aLexem: TFormulaLexem);
begin
  FList.Add(aLexem);
end;

constructor TFormulaParser.TFormulaLexemObjectList.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TFormulaParser.TFormulaLexemObjectList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TFormulaParser.TFormulaLexemObjectList.Getcount: Uint32;
begin
  result := FList.Count;
end;

function TFormulaParser.TFormulaLexemObjectList.GetItems(Index: Integer): TFormulaLexem;
begin
  result := TFormulaLexem(FList[Index]);
end;

procedure TFormulaParser.TFormulaLexemObjectList.Remove(Index: Uint32);
begin
  Assert(Index<FList.Count);
  FList.Remove(FList[Index]);
end;

procedure TFormulaParser.TFormulaLexemObjectList.SetItems(Index: Integer;
  const Value: TFormulaLexem);
begin
  FList[Index] := Value;
end;

end.
