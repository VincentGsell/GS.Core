unit GS.Language.WordTokenizer;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  Generics.Collections;

type
  DyArrayOfChar = array of char;
  TTokenType = (ttword,ttsymbol,ttnumber,ttLF, ttCR);

  { TTokenItem }

  TTokenItem = class
  private
    FCol: integer;
    FItem: string;
    FLine: integer;
    FType: TTokenType;
    function getIsSymbol: boolean;
    function getIsEndLineChars: boolean;
    function getIsNumber: boolean;
    function getIsWord: boolean;
  public
    constructor create(_tokType : TTokenType; _item : String; _line : integer; _col : integer); virtual;

    procedure ChangeValueRequest(_newItem : string);

    property TokType : TTokenType read FType;
    property item : string read FItem;
    property line : integer read FLine;
    property col : integer read FCol;
    property IsSymbol : boolean read getIsSymbol;
    property IsWord : boolean read getIsWord;
    property IsNumber : boolean read getIsNumber;
    property IsEndLineChars : boolean read getIsEndLineChars;
  end;
  TTokenItemArray = Array of TTokenItem;

  { TWordTokenizer }
  TWordTokenizerList = class(TObjectList)
  private
    function GetTokenItem(index: integer): TTokenItem;
  protected
    FSave : TStack;
    FIndex : Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Push;
    procedure Pop;
    function IsLast : boolean;
    function First : TTokenItem;
    function Current : TTokenItem;
    function Next : TTokenItem;
    function Previous : TTokenItem;

    function IsNextTokenSequenceConformTo(Test : Array of TTokenType) : boolean;
    function IsNextItemSequenceConformTo(Test : Array of String) : boolean;

    function getAsTokenItemArray : TTokenItemArray;

    property Tokens[index : integer] : TTokenItem read GetTokenItem;
    property TokenPos : Int32 read FIndex;
  end;

  TWordTokenizer = class
  private
    function GetTokenCount: integer;
    function GetTokenItem(index : integer): TTokenItem;
  protected
    FItem : TWordTokenizerList;
  public
    code : TStringList;
    KeySymbols : DyArrayOfChar;
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Tokenize(const _source : String); virtual;

    function getAsTokenItemArray : TTokenItemArray;
    function IsNextTokenSequenceConformTo(Test : Array of TTokenType) : boolean;
    function IsNextItemSequenceConformTo(Test : Array of String) : boolean;



    procedure _Push;
    procedure _Pop;
    function _IsLast : boolean;
    function _First : TTokenItem;
    function _Current : TTokenItem;
    function _Next : TTokenItem;
    function _Previous : TTokenItem;


    property tokens[index : integer] : TTokenItem read GetTokenItem;
    property tokenCount : integer read GetTokenCount;
  end;

  procedure SetStandartKeySymbols(var _symbols : DyArrayOfChar);

implementation

procedure SetStandartKeySymbols(var _symbols: DyArrayOfChar);
begin
  _symbols := DyArrayOfChar.Create(' ','=','+','-','*','/','%','&','{','}',',','(',')','"','.',#13,#10);
end;


{ TTokenItem }

procedure TTokenItem.ChangeValueRequest(_newItem: string);
begin
  FItem := _newItem;
end;

constructor TTokenItem.create(_tokType: TTokenType; _item: String;
  _line: integer; _col: integer);
begin
  inherited create;
  assert(Length(_item)>0);
  FType:= _tokType;
  FItem:= _item;
  FLine:= _line;
  FCol:= _col;
end;

function TTokenItem.getIsEndLineChars: boolean;
begin
  result := (tokType = TTokenType.ttLF) or (tokType = TTokenType.ttCR);
end;

function TTokenItem.getIsNumber: boolean;
begin
  result := TokType = TTokenType.ttnumber;
end;

function TTokenItem.getIsSymbol: boolean;
begin
  result := TokType = TTokenType.ttsymbol;
end;

function TTokenItem.getIsWord: boolean;
begin
  result := TokType = TTokenType.ttword;
end;

{ TWordTokenizer }

function TWordTokenizer.GetTokenCount: integer;
begin
  result := FItem.Count;
end;

function TWordTokenizer.GetTokenItem(index : integer): TTokenItem;
begin
  result := TTokenItem(FItem[index]);
end;

function TWordTokenizer.IsNextItemSequenceConformTo(
  Test: array of String): boolean;
begin
  result := FItem.IsNextItemSequenceConformTo(Test);
end;

function TWordTokenizer.IsNextTokenSequenceConformTo(
  Test: array of TTokenType): boolean;
begin
  result := FItem.IsNextTokenSequenceConformTo(Test);
end;

constructor TWordTokenizer.create;
begin
  FItem:= TWordTokenizerList.Create;
  code := TStringList.Create;
  SetStandartKeySymbols(KeySymbols);
end;

destructor TWordTokenizer.Destroy;
begin
  FreeAndNil(FItem);
  FreeAndNil(code);
end;

function TWordTokenizer.getAsTokenItemArray: TTokenItemArray;
begin
  result := FItem.getAsTokenItemArray;
end;

procedure TWordTokenizer.Tokenize(const _source: String);

function isKeySymbols(t : char) : boolean;
var i : integer;
begin
  result := false;
  for i := low(KeySymbols) to high(KeySymbols) do
  begin
    if t = KeySymbols[i] then
    begin
      result := true;
      break;
    end;
  end;
end;

var lcode, _word : String;
    i : integer;
    ln,lc : integer;

procedure wordProcess;
var t : TTokenType;
begin
  if length(_word)>0 then
  begin
    t := TTokenType.ttword;
    if (StrToInt64Def(_word,-1)<>-1) then
      t := TTokenType.ttnumber;
    FItem.add(TTokenItem.Create(t,_word,ln,lc));
    _word:= '';
  end;
end;

procedure keySymbolProcess;
begin
  if isKeySymbols(lcode[i]) then
  begin
    wordProcess;
    if lcode[i] = #13 then
    begin
      FItem.add(TTokenItem.Create(TTokenType.ttLF,lcode[i],ln,lc));
      inc(ln);
      lc := 1;
    end
    else
    if lcode[i] = #10 then
    begin
      FItem.add(TTokenItem.Create(TTokenType.ttCR,lcode[i],ln,lc));
    end
    else
    begin
      FItem.add(TTokenItem.Create(TTokenType.ttsymbol,lcode[i],ln,lc));
      inc(lc);
    end;
  end
  else
  begin
    _word:= _word + lcode[i];
    inc(lc);
  end;
end;

begin
  code.Text := _source;
  lcode := _source; //via stringlist, perf down. (need stringlist later.)
  _word := '';
  lc := 0;
  ln := 0;
  for i:= 1 to length(lcode) do
  begin
    keySymbolProcess;
  end;
  wordProcess;
end;


function TWordTokenizer._Current: TTokenItem;
begin
  result := FItem.Current;
end;

function TWordTokenizer._First: TTokenItem;
begin
  result := FItem.First;
end;

function TWordTokenizer._IsLast: boolean;
begin
  result := FItem.IsLast;
end;

function TWordTokenizer._Next: TTokenItem;
begin
  result := FItem.Next;
end;

procedure TWordTokenizer._Pop;
begin
  FItem.Pop;
end;

function TWordTokenizer._Previous: TTokenItem;
begin
  result := FItem.Previous;
end;

procedure TWordTokenizer._Push;
begin
  FItem.Push;
end;

{ TWordTokenizerList }

constructor TWordTokenizerList.Create;
begin
  inherited Create(true);
  FSave := TStack.Create;
  FIndex := -1;
end;

function TWordTokenizerList.Current: TTokenItem;
begin
  result := tokens[FIndex];
end;

destructor TWordTokenizerList.Destroy;
begin
  FreeAndNil(FSave);
  inherited;
end;


function TWordTokenizerList.getAsTokenItemArray: TTokenItemArray;
var i : integer;
begin
  SetLength(result,Count);
  for i := 0 to Count-1 do
  begin
    Result[i] := TTokenItem(Items[i]);
  end;
end;

function TWordTokenizerList.GetTokenItem(index: integer): TTokenItem;
begin
  result := TTokenItem(Items[index]);
end;

function TWordTokenizerList.IsLast: boolean;
begin
  Result := FIndex = Count-1;
end;

function TWordTokenizerList.IsNextItemSequenceConformTo(
  Test: array of String): boolean;
var i,j : integer;
    l : integer;
begin
  result := false;
  l := FIndex;
  j := 0;
  if FIndex + length(Test) < Count then
  begin
    for i :=  Findex to (FIndex + length(Test)) do
    begin
      result := TTokenItem(Items[i]).item = test[j];
      if not result then
        break;
      inc(j);
    end;
  end;
end;

function TWordTokenizerList.IsNextTokenSequenceConformTo(
  Test: array of TTokenType): boolean;
var i,j : integer;
    l : integer;
begin
  result := false;
  l := FIndex;
  j := 0;
  if FIndex + length(Test) < Count then
  begin
    for i :=  Findex to (FIndex + length(Test)) do
    begin
      result := TTokenItem(Items[i]).TokType = test[j];
      if not result then
        break;
      inc(j);
    end;
  end;
end;

function TWordTokenizerList.First: TTokenItem;
begin
  FIndex := -1;
  result := Next;
end;

function TWordTokenizerList.Next: TTokenItem;
begin
  Assert(Findex<Count);
  result := nil;
  inc(FIndex);
  While (FIndex<Count-1) and (
        (TTokenItem(Items[FIndex]).TokType = TTokenType.ttLF) or
        (TTokenItem(Items[FIndex]).TokType = TTokenType.ttCR) or
        (TTokenItem(Items[FIndex]).item = ' ')) do
    inc(FIndex);
  if FIndex<Count then
    result := Tokens[FIndex];
end;

procedure TWordTokenizerList.Pop;
var l :  TTokenItem;
begin
  l := TTokenItem(FSave.Pop);
  FIndex := IndexOf(l);
end;

function TWordTokenizerList.Previous: TTokenItem;
begin
  Assert(Findex>0);
  dec(FIndex);
  While (FIndex>0) and
        (TTokenItem(Items[FIndex]).TokType = TTokenType.ttLF) or
        (TTokenItem(Items[FIndex]).TokType = TTokenType.ttCR) or
        (TTokenItem(Items[FIndex]).item = ' ') do
    Dec(FIndex);
  result := Tokens[FIndex];
end;

procedure TWordTokenizerList.Push;
begin
  Assert(Findex>-1);
  FSave.Push(tokens[FIndex]);
end;


end.

