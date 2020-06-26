//Twee3 parser. (twine.org)
unit GS.Language.Parser.Twee;

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}

interface

uses Classes, SysUtils, ContNrs,
     GS.Language.Tokenizer.Twee,
     GS.Language.Syntax.Twee,
     GS.Language.Semantic.Twee,
     GS.Language.Compiler;

type
  TGSCRootTwee = class(TGSCRoot)
  public
    Tokenizer : TTweeTokenizer; //simple tokenizer is enough.
    SyntaxCheck : TTweeSyntaxCheck;
    SemanticProcessor : TTweeSemanticProcessor;

    Constructor Create; override;
    Destructor Destroy; override;
  end;

  TTweeParserStat = record
    FirstPassTokenizerSymbolCount : Uint32;
    //TODO : Time metric
    SecondPassTokenizerSymbolCount : Uint32;
    //...
    procedure reset;
  end;

  //Group : Tokenizer, syntax checker, and semantic checker.
  TTweeParser = class
  private
    fRoot : TGSCRootTwee;
    fstats : TTweeParserStat;
    function GetLogs: TStringList;
    function getTokenizer: TTweeTokenizer;
  protected
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Parse(const code : string) : boolean;

    property logs : TStringList read GetLogs;
    property stats : TTweeParserStat read fstats;

    property Tokernizer : TTweeTokenizer read getTokenizer;
    property Root : TGSCRootTwee read fRoot;
  end;


implementation


{ TTweeParser }

constructor TTweeParser.Create;
begin
  inherited;
  froot := TGSCRootTwee.Create;
  fstats.reset;
end;

destructor TTweeParser.Destroy;
begin
  FreeAndNil(fRoot);
  inherited;
end;

function TTweeParser.GetLogs: TStringList;
begin
  result := froot.logs;
end;

function TTweeParser.getTokenizer: TTweeTokenizer;
begin
  result := fRoot.Tokenizer;
end;

function TTweeParser.Parse(const code: string): boolean;
begin
  result := true;
  FRoot.Tokenizer.Tokenize(code);
  fstats.FirstPassTokenizerSymbolCount := fRoot.Tokenizer.tokenCount;
  fstats.SecondPassTokenizerSymbolCount := fRoot.Tokenizer.TweeTokenCount;
  if FRoot.SyntaxCheck.Check then
  begin
    if FRoot.SemanticProcessor.Process then
    begin
      FRoot.SemanticProcessor.log('Semantic check success.');
    end
    else
    begin
      result := false;
      FRoot.SemanticProcessor.log('Semantic check unsuccess.');
    end;
  end
  else
  begin
    result := false;
    FRoot.log('Syntax check unsuccess.');
  end;
end;


{ TGSCRootTwee }

constructor TGSCRootTwee.Create;
begin
  inherited;
  Tokenizer := TTweeTokenizer.Create;
  SyntaxCheck := TTweeSyntaxCheck.Create(Self, Tokenizer);
  SemanticProcessor := TTweeSemanticProcessor.Create(Self);
end;

destructor TGSCRootTwee.Destroy;
begin
  FreeAndNil(Tokenizer);
  FreeAndNil(SyntaxCheck);
  FreeAndNil(SemanticProcessor);
  inherited;
end;



{ TTweeParserStat }

procedure TTweeParserStat.reset;
begin
  FirstPassTokenizerSymbolCount := 0;
  SecondPassTokenizerSymbolCount := 0;
end;

end.
