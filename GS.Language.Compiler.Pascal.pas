unit GS.Language.Compiler.Pascal;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}


uses
  Classes, SysUtils,
  //GS.VM,
  GS.Language.Parser.Pascal,
  GS.Language.Compiler;


type
  //...in another unit, link between parser and VM target.
  TPascalCompiler = class(TCompiler)
  protected
//    fProgram : TVMProgram;

//    procedure InternalGenerateCode(_prog : TVMProgram);
  public
    parser : TPascalParser;

    function Compile(const _code : String) : boolean; Override;

    constructor Create; virtual;
    destructor Destroy; override;

//    property CompiledProgram : TVMPRogram read FProgram;

  end;


implementation

{ TPascalCompiler }

function TPascalCompiler.Compile(const _code: String) : Boolean;
begin
  result := parser.Parse(_code);
  if result then
  begin
//    fProgram.reset;
//    InternalGenerateCode(fProgram);
  end;
end;

constructor TPascalCompiler.Create;
begin
  parser := TPascalParser.Create;
//  fProgram := TVMProgram.Create;
end;

destructor TPascalCompiler.Destroy;
begin
  freeAndNil(parser);
//  freeAndNil(fProgram);
  inherited;
end;

{
procedure TPascalCompiler.InternalGenerateCode(_prog : TVMProgram);
begin
  raise Exception.Create('TODO : EMIT :)');
end;
}
end.
