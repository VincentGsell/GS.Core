unit GS.Language.Tokenizer.Pascal;


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
  TPascalTokenType = ( pttword,
                       pttsymbol,
                       pttnumberConstInt,
                       pttnumberConstFloat,
                       pttstringConst,
                       pttLF,
                       pttCR,
                       pttComment,
                       pttCompilationDirective
                       );

  //Sub symbol type : pttsymbol can have those subsymbol type. pttNA if non applicable.
  TPascalSymbolTokenType = (pttNA,pttComma,pttSemiColon,ptt2Point,pttPlus,pttMinus,pttMul,pttDiv,pttEqual,pttPointer);

  //langage specific symbol type.
  TPascalNativeKeyWord = ( kwUnknown, kwPprogram, kwProcedure, kwFunction, kwMethod, kwBegin, kwEnd, kwConst, kwVar, kwType);
  TPascalNativeType = ( ntUnknown, ntByte, ntInt, ntDouble, ntString, ntExtended);
  TPascalNativeTypeExt = ( nteUnknown, nteArray, nteRec, nteClass, nteExtendedBeyond);

Const
  CST_TPascalNativeKeyWord : array[TPascalNativeKeyWord] of string = ('unknown', 'program', 'procedure', 'function', 'method', 'begin', 'end', 'const', 'var', 'type');
  CST_TPascalNativeType : array[TPascalNativeType] of string = ( 'Unknown', 'byte', 'integer', 'double', 'string' , 'ExtendedType');
  CST_TPascalNativeTypeExt : array[TPascalNativeTypeExt] of string = ( 'Unknown', 'array', 'record', 'class', 'eExtendedBeyondType');

Type

  { TPascalTokenItem }

  TPascalTokenItem = class
  private
    FColEnd: integer;
    FColStart: integer;
    FItem: string;
    FLineEnd: integer;
    FLineStart: integer;
    FType: TPascalTokenType;
    FSymbolType: TPascalSymbolTokenType;
  protected
  public
    constructor create(_PascaltokType : TPascalTokenType; _item : String; _lineStart : integer; _colStart : integer; _lineEnd : integer; _colEnd : integer; const SymbolTokenType : TPascalSymbolTokenType = pttNA); virtual;
    property PascalTokenType : TPascalTokenType read FType;
    property PascalSymbolTokenType : TPascalSymbolTokenType read FSymbolType;
    property item : string read FItem;
    property lineStart : integer read FLineStart;
    property colStart : integer read FColStart;
    property lineEnd : integer read FLineEnd;
    property colEnd : integer read FColEnd;
  end;

  { TPascalTokenizer }
  TPascalTokenizer = class(TWordTokenizer)
  private
    function GetPascalTokenCount: integer;
    function GetPascalTokenItem(index : integer): TPascalTokenItem;
  protected
    FPascalTokens : TObjectList;
    FIndex : Integer;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Tokenize(const _source : String); override;

    function SetCursorTo(token: TPascalTokenItem) : TPascalTokenItem;

    function IsLast : boolean;
    function First : TPascalTokenItem;
    function Current : TPascalTokenItem;
    function Next : TPascalTokenItem;
    function Previous : TPascalTokenItem;



    property PascalTokens[index : integer] : TPascalTokenItem read GetPascalTokenItem;
    property PascalTokenCount : integer read GetPascalTokenCount;
  end;
  TPascalTokenItemArray = Array of  TPascalTokenItem;


procedure SetPascalStyleKeySymbols(var _symbols : DyArrayOfChar);

implementation

procedure SetPascalStyleKeySymbols(var _symbols: DyArrayOfChar);
begin
  _symbols := DyArrayOfChar.Create(' ',';',':','=','+','-','*','[',']','/','%','&','{','}',',','(',')','''','.',#13,#10,'^');
end;


{ TPascalTokenItem }

constructor TPascalTokenItem.create(_PascaltokType: TPascalTokenType;
  _item: String; _lineStart: integer; _colStart: integer; _lineEnd: integer;
  _colEnd: integer; const SymbolTokenType : TPascalSymbolTokenType);
begin
  inherited create;
  FColEnd := _colEnd;
  FColStart := _colStart;
  FItem := lowercase(_item);
  FLineEnd := _lineEnd;
  FLineStart := _lineStart;
  FType := _PascaltokType;
  FSymbolType := SymbolTokenType;
end;

{ TPascalTokenizer }

function TPascalTokenizer.GetPascalTokenCount: integer;
begin
  result := FPascalTokens.count;
end;

function TPascalTokenizer.GetPascalTokenItem(index : integer): TPascalTokenItem;
begin
  result := TPascalTokenItem(FPascalTokens[index]);
end;

function TPascalTokenizer.IsLast: boolean;
begin
  Result := FIndex = PascalTokenCount-1;
end;

constructor TPascalTokenizer.Create;
begin
  inherited Create;
  SetPascalStyleKeySymbols(KeySymbols);
  FPascalTokens := TObjectList.Create;
  FIndex := 0;
end;

function TPascalTokenizer.Current: TPascalTokenItem;
begin
  result := PascalTokens[FIndex];
end;

destructor TPascalTokenizer.Destroy;
begin
  FreeAndNil(FPascalTokens);
  inherited Destroy;
end;

procedure TPascalTokenizer.Tokenize(const _source: String);
var idx,j : integer;
    temps : string;
    tempc, templ : integer;
    parity : integer;
    pType : TPascalTokenType;

    procedure AddStdPascalToken(_ptokentype : TPascalTokenType; const _SymbolTokenType : TPascalSymbolTokenType = pttNA);
    begin
      FPascalTokens.Add(TPascalTokenItem.Create(
                                                 _pTokenType,
                                                 tokens[idx].item,
                                                 tokens[idx].line,
                                                 tokens[idx].col,
                                                 tokens[idx].line,
                                                 tokens[idx].col,
                                                 _SymbolTokenType)
                                                 );
    end;

begin
  inherited Tokenize(_source);
  idx := 0;
  while (idx<tokenCount) do
  begin
    case tokens[idx].TokType of

      TTokenType.ttsymbol:
      begin
        if tokens[idx].item = ';' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttSemiColon);
          inc(idx);
        end
        else
        if tokens[idx].item = ',' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttComma);
          inc(idx);
        end
        else
        if tokens[idx].item = ':' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.ptt2Point);
          inc(idx);
        end
        else
        if tokens[idx].item = '+' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttPlus);
          inc(idx);
        end
        else
        if tokens[idx].item = '-' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttMinus);
          inc(idx);
        end
        else
        if tokens[idx].item = '*' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttMul);
          inc(idx);
        end
        else
    // PLEASE : let this comment.
    //      if tokens[idx].item = '/' then
    //      begin
          //Not here : shared symbol with comment. See in comments.
    //      end
    //      else
        if tokens[idx].item = '=' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttEqual);
          inc(idx);
        end
        else
        if tokens[idx].item = '^' then
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttPointer);
          inc(idx);
        end
        else
        if tokens[idx].item = '/' then
        begin
          if Not(idx=tokenCount) and (tokens[idx+1].item ='/') then
          begin
            j := idx+1;
            temps := '';
            tempc := tokens[j].col;
            templ := tokens[j].line;
            while (tokens[j].TokType<>TTokenType.ttCR) or (j = tokenCount) do
            begin
              inc(j);
              temps := temps + tokens[j].item;
              tempc := tokens[j].col;
              templ := tokens[j].line;
            end;
            FPascalTokens.Add(TPascalTokenItem.Create(
                                                       TPascalTokenType.pttComment,
                                                       temps,
                                                       tokens[idx].line,
                                                       tokens[idx].col,
                                                       templ,
                                                       tempc)
                                                       );
            idx := j;
          end
          else
          begin
            AddStdPascalToken(TPascalTokenType.pttsymbol,TPascalSymbolTokenType.pttDiv);
            inc(idx);
          end;
        end
        else
        if tokens[idx].item = '{' then
        begin
          pType := TPascalTokenType.pttComment;
          if Not(idx=tokenCount) then
          begin
            j := idx+1;
            parity := 1;
            temps := '';
            tempc := tokens[j].col;
            templ := tokens[j].line;
            while (parity>0) and (j < tokenCount) do
            begin
              if tokens[j].item = '{' then
                inc(parity)
              else
              if tokens[j].item = '}' then
                dec(parity)
              else
              if (tokens[j].item = '$') and (j=idx+1) then
                pType := TPascalTokenType.pttComment
              else
              begin
                temps := temps + tokens[j].item;
                tempc := tokens[j].col;
                templ := tokens[j].line;
              end;
              inc(j);
            end;

            if j<tokenCount then
            begin
              FPascalTokens.Add(TPascalTokenItem.Create(
                                                       pType,
                                                       temps,
                                                       tokens[idx].line,
                                                       tokens[idx].col,
                                                       templ,
                                                       tempc)
                                                       );

            end;
            idx := j;
          end
          else
          begin
            AddStdPascalToken(TPascalTokenType.pttsymbol);
            inc(idx);
          end;
        end
        else
        if tokens[idx].item = '''' then
        begin
          if idx<tokenCount then
          begin
            j := idx+1;
            parity:= 1;
            temps := '';
            tempc := tokens[j].col;
            templ := tokens[j].line;
            while ((parity>0) and (j < tokenCount-1)) do
            begin
              if (tokens[j].item='''') And (tokens[j+1].item='''') then
              begin
                temps := temps + tokens[j].item;
                tempc := tokens[j].col;
                templ := tokens[j].line;
                j := j+1;
              end
              else
              if tokens[j].item='''' then
              begin
                parity := 0;
              end
              else
              begin
                temps := temps + tokens[j].item;
                tempc := tokens[j].col;
                templ := tokens[j].line;
              end;
              inc(j);
            end;

            FPascalTokens.Add(TPascalTokenItem.Create(
                                                     TPascalTokenType.pttstringConst,
                                                     temps,
                                                     tokens[idx].line,
                                                     tokens[idx].col,
                                                     templ,
                                                     tempc)
                                                     );

            idx := j;
          end
          else
          begin
            AddStdPascalToken(TPascalTokenType.pttsymbol);
            inc(idx);
          end;
        end
        else
        begin
          AddStdPascalToken(TPascalTokenType.pttsymbol);
          inc(idx);
        end;
      end;

      TTokenType.ttCR:
      begin
    //      AddStdPascalToken(TPascalTokenType.pttCR); Useless in pascal ?
        inc(idx);
      end;

      TTokenType.ttLF:
      begin
    //      AddStdPascalToken(TPascalTokenType.pttLF);
        inc(idx);
      end;

      TTokenType.ttnumber:
      begin
        //const float number writen as "x." (c-style)
        if (idx<tokenCount-1) and (tokens[idx+1].item ='.') and (tokens[idx+2].TokType<>TTokenType.ttnumber) then
          begin
           FPascalTokens.Add(TPascalTokenItem.Create(
                                                     TPascalTokenType.pttnumberConstFloat,
                                                     tokens[idx].item+tokens[idx+1].item+'0',
                                                     tokens[idx].line,
                                                     tokens[idx].col,
                                                     tokens[idx+1].line,
                                                     tokens[idx+1].col)
                                                     );
           idx :=idx+2
        end
        else
        //const float number writen as "x.y"
        if (idx<tokenCount-1) and (tokens[idx+1].item ='.') and (tokens[idx+2].TokType=TTokenType.ttnumber) then
          begin
           FPascalTokens.Add(TPascalTokenItem.Create(
                                                     TPascalTokenType.pttnumberConstFloat,
                                                     tokens[idx].item+tokens[idx+1].item+tokens[idx+2].item,
                                                     tokens[idx].line,
                                                     tokens[idx].col,
                                                     tokens[idx+2].line,
                                                     tokens[idx+2].col)
                                                     );
           idx :=idx+3
        end
        else
        begin  //no point, it is a const.
          AddStdPascalToken(TPascalTokenType.pttnumberConstInt);
          inc(idx);
        end;
      end;

      TTokenType.ttword:
      begin
        AddStdPascalToken(TPascalTokenType.pttword);
        inc(idx);
      end;

      else
      begin
        inc(idx);
      end;

    end;

  end;
end;

function TPascalTokenizer.First: TPascalTokenItem;
begin
  FIndex := -1;
  result := Next;
end;

function TPascalTokenizer.Next: TPascalTokenItem;
begin
  Assert(Findex<PascalTokenCount-1);
  inc(FIndex);
  While (FIndex<PascalTokenCount) and
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttCompilationDirective) or
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttComment) or
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttLF) or
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttCR) or
        (PascalTokens[FIndex].item = ' ') do
    inc(FIndex);
  result := PascalTokens[FIndex];
end;


function TPascalTokenizer.Previous: TPascalTokenItem;
begin
  dec(FIndex);
  While (FIndex>0) and
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttCompilationDirective) or
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttComment) or
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttLF) or
        (PascalTokens[FIndex].PascalTokenType = TPascalTokenType.pttCR) or
        (PascalTokens[FIndex].item = ' ') do
    Dec(FIndex);
  result := PascalTokens[FIndex];
end;

function TPascalTokenizer.SetCursorTo(token: TPascalTokenItem) : TPascalTokenItem;
begin
  assert(assigned(token));
  FIndex := FPascalTokens.IndexOf(token);
  result := token;
end;

end.

