program ProjectPascalTest;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {Form34},
  GS.Language.AlgoProgram in '..\..\..\..\..\..\GS.Language.AlgoProgram.pas',
  GS.Language.AlgoProgram.Runner in '..\..\..\..\..\..\GS.Language.AlgoProgram.Runner.pas',
  GS.Language.Compiler in '..\..\..\..\..\..\GS.Language.Compiler.pas',
  GS.Language.Compiler.Pascal in '..\..\..\..\..\..\GS.Language.Compiler.Pascal.pas',
  GS.Language.Formula in '..\..\..\..\..\..\GS.Language.Formula.pas',
  GS.Language.Formula.Solver in '..\..\..\..\..\..\GS.Language.Formula.Solver.pas',
  GS.Language.Parser.Pascal in '..\..\..\..\..\..\GS.Language.Parser.Pascal.pas',
  GS.Language.Semantic.Pascal in '..\..\..\..\..\..\GS.Language.Semantic.Pascal.pas',
  GS.Language.Syntax.Pascal in '..\..\..\..\..\..\GS.Language.Syntax.Pascal.pas',
  GS.Language.Tokenizer.Pascal in '..\..\..\..\..\..\GS.Language.Tokenizer.Pascal.pas',
  GS.Language.VM in '..\..\..\..\..\..\GS.Language.VM.pas',
  GS.Language.WordTokenizer in '..\..\..\..\..\..\GS.Language.WordTokenizer.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
  {$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm34, Form34);
  Application.Run;
end.
