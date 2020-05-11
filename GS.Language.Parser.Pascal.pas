unit GS.Language.Parser.Pascal;


{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}

interface

uses
  Classes, SysUtils, ContNrs,
  GS.Language.WordTokenizer,
  GS.Language.Tokenizer.Pascal,
  GS.Language.Syntax.Pascal,
  GS.Language.Semantic.Pascal,
  GS.Language.Compiler;

type

  //Group : Tokenizer, syntax checker, and semantic checker.
  TPascalParser = class
  private
    function GetLogs: TStringList;
  protected
    FTokenizer : TPascalTokenizer;
    FSyntaxCheck : TPascalSyntaxCheck;
    FSemanticProcessor : TPascalSemanticProcessor;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Parse(const code : string) : boolean;

    property logs : TStringList read GetLogs;
  end;


implementation


{ TPascalParser }

constructor TPascalParser.Create;
begin
  inherited;
  FTokenizer := TPascalTokenizer.Create;
  FSyntaxCheck := TPascalSyntaxCheck.Create(FTokenizer);
  FSemanticProcessor := TPascalSemanticProcessor.Create(FSyntaxCheck.Root);
  FSyntaxCheck.Root.SemanticProcessor := FSemanticProcessor;
end;

destructor TPascalParser.Destroy;
begin
  FreeAndNil(FTokenizer);
  FreeAndNil(FSyntaxCheck);
  FreeAndNil(FSemanticProcessor);
  inherited;
end;

function TPascalParser.GetLogs: TStringList;
begin
  result := FSyntaxCheck.Log;
end;

function TPascalParser.Parse(const code: string): boolean;
begin
  result := true;
  FTokenizer.Tokenize(code);
  if FSyntaxCheck.Check then
  begin
    if FSemanticProcessor.Process then
    begin
      FSemanticProcessor.log('Semantic check success.');
    end
    else
    begin
      result := false;
      FSyntaxCheck.root.log('Semantic check unsuccess.');
    end;
  end
  else
  begin
    result := false;
    FSyntaxCheck.root.log('Syntax check unsuccess.');
  end;
end;


end.

