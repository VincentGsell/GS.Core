unit GS.Language.Tokenizer.Twee;


{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}

interface

uses
  Classes, SysUtils, ContNrs,
  GS.Language.WordTokenizer,
  GS.Language.Compiler;

type

  //Main symbol type.
  TTweeTokenType = ( tttword,tttsymbol,tttnumber,tttLF, tttCR,
                     tttnumberInt, tttnumberFloat, tttstring,
                     tttPassageMark, tttGlobalVariable, tttLocalVariable
                    );

  TTweeTokenItem = class
  public
    TweeTokType : TTweeTokenType;
    TokenSource : TTokenItem;
    ItemStringValue : String;
    Constructor Create(_tokType : TTweeTokenType; tokSource : TTokenItem); reintroduce;
  end;


  TTweeTokenList = class(TObjectList)
  protected
    FSave : TStack;
    FIndex : Integer;
    function GetTweeTokenItem(Index: Uint32): TTweeTokenItem;
    function GetTweeTokenCount: Uint32;
  public
    function AddToken(_tokType : TTweeTokenType; tokSource : TTokenItem) : TTweeTokenItem;
    property TweeTokenCount : Uint32 read GetTweeTokenCount;
    property TweeTokens[Index : Uint32] : TTweeTokenItem read GetTweeTokenItem;

    constructor Create; virtual;
    destructor Destroy; override;

    procedure Push;
    function Pop : TTweeTokenItem;
    function IsLast : boolean;
    function First : TTweeTokenItem;
    function Current : TTweeTokenItem;
    function Next : TTweeTokenItem;
    function Previous : TTweeTokenItem;

    function getDataBetween(start,finish : string) : string;
    
    function IsNextTokenSequenceConformTo(Test : Array of TTweeTokenType) : boolean;
    function IsNextItemSequenceConformTo(Test : Array of String) : boolean;
    function NextSequenceToString(number : UInt32) : string;

    property TokenPos : Int32 read FIndex;
  end;



  TTweeTokenizer = Class(TWordTokenizer)
  Type TTokenizeState = (normal, betweenbracket);
  private
    ftweeItems: TTweeTokenList;
    falreadyAdded : boolean;
    finternalState : TTokenizeState;

    function GetTweeTokenItem(index: integer): TTweeTokenItem;

    function GetTweeTokenCount: Uint32;

  protected
    //state
    procedure internal_common_getnumber;

    procedure InternalTokenize_Normal;
      procedure internal_normal_symbol;

    procedure InternalTokenize_Bracket;
    //tools
    procedure addTweeTokenFromStdTokenCurrent;
    procedure addTweeTokenWithTokenCurrent(_tweetokType : TTweeTokenType; const ItemStr : string = '');

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Tokenize(const _source : String); override;

    property TweeTokenCount : Uint32 read GetTweeTokenCount;
    property TweeTokens[index : integer] : TTweeTokenItem read GetTweeTokenItem;
    property TT : TTweeTokenList read ftweeItems;

   End;


implementation

procedure SetTweeKeySymbols(var _symbols: DyArrayOfChar);
begin
   _symbols := DyArrayOfChar.Create(' ',';',':','=','+','-','*','[',']','\','/','{','}',',','(',')','"','.',',','''',#13,#10);
end;

  { TTweeTokenizer }

procedure TTweeTokenizer.addTweeTokenFromStdTokenCurrent;
{$IFDEF DEBUG} var l : TTokenItem;{$ENDIF}
begin
  if not(falreadyAdded) then
  begin
    //4 first states is same. (see TokenType vs TweeTokenType)
    assert(Ord(_Current.TokType)<5);
    ftweeItems.AddToken(TTweeTokenType(_Current.TokType),_current);
{$IFDEF DEBUG} l := _Current;{$ENDIF}
    falreadyAdded := true;
  end;
end;

procedure TTweeTokenizer.addTweeTokenWithTokenCurrent(_tweetokType : TTweeTokenType; const ItemStr : string = '');
var l : TTweeTokenItem;
begin
  if not(falreadyAdded) then
  begin
    l := ftweeItems.AddToken(_tweetokType,_current);
    falreadyAdded := true;
    if ItemStr<>'' then
      l.ItemStringValue := ItemStr;
  end;
end;

constructor TTweeTokenizer.Create;
begin
  inherited;
  SetTweeKeySymbols(KeySymbols);
  ftweeItems := TTweeTokenList.Create;
end;

destructor TTweeTokenizer.Destroy;
begin
  FreeAndNil(ftweeItems);
  inherited;
end;

function TTweeTokenizer.GetTweeTokenCount: Uint32;
begin
  result := ftweeItems.count;
end;


function TTweeTokenizer.GetTweeTokenItem(index: integer): TTweeTokenItem;
begin
  result := ftweeItems.TweeTokens[index];
end;

///------------------------------------------------------------------------
///------------------------------------------------------------------------
///------------------------------------------------------------------------
///------------------------------------------------------------------------
///------------------------------------------------------------------------
///------------------------------------------------------------------------

procedure TTweeTokenizer.InternalTokenize_Bracket;
begin
  case _Current.TokType of
    ttsymbol :
    begin
      if _current.item = '}' then
      begin
        finternalState := TTokenizeState.normal;
      end
    end;
    ttnumber :
    begin
      internal_common_getnumber;
    end;
  end;
end;

procedure TTweeTokenizer.InternalTokenize_Normal;
begin
  case _Current.TokType of
    ttsymbol :
    begin
      internal_normal_symbol;
    end;
    ttnumber :
    begin
      internal_common_getnumber;
    end;
  end;
end;

procedure TTweeTokenizer.internal_common_getnumber;
var l : string;
    procedure getNum;
    begin
      while not(_IsLast) and (_Current.IsNumber) do
      begin
        l := l + _Current.item;
        _next;
      end;
    end;

begin
  l := '';

  //Real part
  getNum;
  if not(_IsLast) and (_Current.item = '.') then
  begin
    l := l + '.';
    _next;
    //Mantissa.
    getNum;
    _Previous;
    addTweeTokenWithTokenCurrent(TTweeTokenType.tttnumberFloat,l);
  end
  else
  begin
    _Previous;
    addTweeTokenWithTokenCurrent(TTweeTokenType.tttnumberInt,l);
  end;
end;

procedure TTweeTokenizer.internal_normal_symbol;
begin
  if _current.item = '{' then
  begin
    finternalState := TTokenizeState.betweenbracket;
  end;
end;

procedure TTweeTokenizer.Tokenize(const _source: String);
begin
  inherited Tokenize(_source);
  finternalState := TTokenizeState.normal;
  _First;
  while Not(_IsLast) do
  begin
    falreadyAdded := false; //addTweeTokenFromStdTokenCurrent add a std only if false; (only one token by loop)
    case finternalState of
      normal: InternalTokenize_Normal;
      betweenbracket: InternalTokenize_Bracket;
    end;
    addTweeTokenFromStdTokenCurrent;
    _next;
  end;
end;


{ TTweeTokenItem }

constructor TTweeTokenItem.Create(_tokType: TTweeTokenType;
  tokSource: TTokenItem);
begin
  inherited Create;
  assert(assigned(tokSource));
  TweeTokType := _tokType;
  TokenSource := TokSource;
  ItemStringValue := tokSource.item;
end;

{ TTweeTokenList }

function TTweeTokenList.AddToken(_tokType: TTweeTokenType; tokSource: TTokenItem) : TTweeTokenItem;
begin
  result := TTweeTokenItem.Create(_tokType,tokSource);
  Add(result);
end;

function TTweeTokenList.GetTweeTokenItem(Index: Uint32): TTweeTokenItem;
begin
  result := TTweeTokenItem(Items[Index]);
end;

function TTweeTokenList.GetTweeTokenCount: Uint32;
begin
  result:= Count;
end;

constructor TTweeTokenList.Create;
begin
  inherited Create(true);
  FSave := TStack.Create;
  FIndex := -1;
end;

function TTweeTokenList.Current: TTweeTokenItem;
begin
  result := tweetokens[FIndex];
end;

destructor TTweeTokenList.Destroy;
begin
  FreeAndNil(FSave);
  inherited;
end;

function TTweeTokenList.IsLast: boolean;
begin
  Result := FIndex = Count-1;
end;

function TTweeTokenList.IsNextItemSequenceConformTo(
  Test: array of String): boolean;
var i,j : integer;
begin
  result := false;
  if length(test)=0 then
    Exit;
  j := 0;
  if FIndex + length(Test) < Count then
  begin
    for i :=  Findex to (FIndex + length(Test)-1) do
    begin
      result := (TTweeTokenItem(Items[i]).ItemStringValue) = test[j];
      if not result then
        break;
      inc(j);
    end;
  end;
end;

function TTweeTokenList.IsNextTokenSequenceConformTo(
  Test: array of TTweeTokenType): boolean;
var i,j : integer;
begin
  result := false;
  if length(test)=0 then
    Exit;
  j := 0;
  if FIndex + length(Test) < Count then
  begin
    for i :=  Findex to (FIndex + length(Test)-1) do
    begin
      result := TTweeTokenItem(Items[i]).TweeTokType = test[j];
      if not result then
        break;
      inc(j);
    end;
  end;
end;

function TTweeTokenList.First: TTweeTokenItem;
begin
  FIndex := -1;
  result := Next;
end;


function TTweeTokenList.getDataBetween(start, finish: string): string;
begin
  result := '';
  if (Current.ItemStringValue = start) or (start = '') then
  begin
    Next;
    while Not(IsLast) And (Current.ItemStringValue <> finish) do
    begin
      result := result + Current.ItemStringValue;
      Next;
    end;
  end;
end;

function TTweeTokenList.Next: TTweeTokenItem;
begin
  Assert(Findex<Count);
  result := nil;
  inc(FIndex);
  While (FIndex<Count-1) and (
        (TTweeTokenItem(Items[FIndex]).TweeTokType = TTweeTokenType.tttLF) or
        (TTweeTokenItem(Items[FIndex]).TweeTokType = TTweeTokenType.tttCR) or
        (TTweeTokenItem(Items[FIndex]).ItemStringValue = ' ')) do
    inc(FIndex);
  if FIndex<Count then
    result := TweeTokens[FIndex];
end;

function TTweeTokenList.NextSequenceToString(number: UInt32): string;
var i : integer;
begin
  result := '';
  if FIndex + number < Count then
  begin
    for i :=  Findex to (FIndex + number-1) do
    begin
      result := result + TTweeTokenItem(Items[i]).ItemStringValue;
    end;
  end;
end;

function TTweeTokenList.Pop : TTweeTokenItem;
begin
  result := TTweeTokenItem(FSave.Pop);
  FIndex := IndexOf(result);
end;

function TTweeTokenList.Previous: TTweeTokenItem;
begin
  Assert(Findex>0);
  dec(FIndex);
  While (FIndex>0) and
        (TTweeTokenItem(Items[FIndex]).TweeTokType = TTweeTokenType.tttLF) or
        (TTweeTokenItem(Items[FIndex]).TweeTokType = TTweeTokenType.tttCR) or
        (TTweeTokenItem(Items[FIndex]).ItemStringValue = ' ') do
    Dec(FIndex);
  result := TweeTokens[FIndex];
end;

procedure TTweeTokenList.Push;
begin
  Assert(Findex>-1);
  FSave.Push(TweeTokens[FIndex]);
end;


end.
