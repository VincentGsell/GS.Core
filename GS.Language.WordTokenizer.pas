unit GS.Language.WordTokenizer;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}

interface

uses
  Classes, SysUtils, contnrs;

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
  public
    constructor create(_tokType : TTokenType; _item : String; _line : integer; _col : integer); virtual;
    property TokType : TTokenType read FType;
    property item : string read FItem;
    property line : integer read FLine;
    property col : integer read FCol;
  end;
  TTokenItemArray = Array of TTokenItem;

  { TWordTokenizer }

  TWordTokenizer = class
  private
    function GetTokenCount: integer;
    function GetTokenItem(index : integer): TTokenItem;
  protected
    FItem : TObjectList;
  public
    KeySymbols : DyArrayOfChar;
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Tokenize(const _source : String); virtual;

    function getAsTokenItemArray : TTokenItemArray;

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

{ TWordTokenizer }

function TWordTokenizer.GetTokenCount: integer;
begin
  result := FItem.Count;
end;

function TWordTokenizer.GetTokenItem(index : integer): TTokenItem;
begin
  result := TTokenItem(FItem[index]);
end;

constructor TWordTokenizer.create;
begin
  FItem:= TObjectList.Create;
  SetStandartKeySymbols(KeySymbols);
end;

destructor TWordTokenizer.Destroy;
begin
  FreeAndNil(FItem);
end;

function TWordTokenizer.getAsTokenItemArray: TTokenItemArray;
var i : integer;
begin
  SetLength(result,FItem.Count);
  for i := 0 to FItem.Count-1 do
  begin
    Result[i] := TTokenItem(FItem[i]);
  end;
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

var code : String;
    _word : String;
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
  if isKeySymbols(code[i]) then
  begin
    wordProcess;
    if code[i] = #13 then
    begin
      FItem.add(TTokenItem.Create(TTokenType.ttLF,code[i],ln,lc));
      inc(ln);
      lc := 1;
    end
    else
    if code[i] = #10 then
    begin
      FItem.add(TTokenItem.Create(TTokenType.ttCR,code[i],ln,lc));
    end
    else
    begin
      FItem.add(TTokenItem.Create(TTokenType.ttsymbol,code[i],ln,lc));
      inc(lc);
    end;
  end
  else
  begin
    _word:= _word + code[i];
    inc(lc);
  end;
end;

begin
  code := _source;
  _word := '';
  lc := 1;
  ln := 1;
  for i:= 1 to length(code) do
  begin
    keySymbolProcess;
  end;
  wordProcess;
end;


end.

