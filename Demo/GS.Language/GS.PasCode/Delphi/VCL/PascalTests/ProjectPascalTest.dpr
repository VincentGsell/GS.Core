program ProjectPascalTest;

uses
  Vcl.Forms,
  fmain in 'fmain.pas' {Form34},
  GS.Language.AlgoProgram in '..\..\..\..\..\..\GS.Core\GS.Language.AlgoProgram.pas',
  GS.Language.AlgoProgram.Runner in '..\..\..\..\..\..\GS.Core\GS.Language.AlgoProgram.Runner.pas',
  GS.Language.Compiler in '..\..\..\..\..\..\GS.Core\GS.Language.Compiler.pas',
  GS.Language.Compiler.Pascal in '..\..\..\..\..\..\GS.Core\GS.Language.Compiler.Pascal.pas',
  GS.Language.Formula in '..\..\..\..\..\..\GS.Core\GS.Language.Formula.pas',
  GS.Language.Formula.Solver in '..\..\..\..\..\..\GS.Core\GS.Language.Formula.Solver.pas',
  GS.Language.Parser.Pascal in '..\..\..\..\..\..\GS.Core\GS.Language.Parser.Pascal.pas',
  GS.Language.Semantic.Pascal in '..\..\..\..\..\..\GS.Core\GS.Language.Semantic.Pascal.pas',
  GS.Language.Syntax.Pascal in '..\..\..\..\..\..\GS.Core\GS.Language.Syntax.Pascal.pas',
  GS.Language.Tokenizer.Pascal in '..\..\..\..\..\..\GS.Core\GS.Language.Tokenizer.Pascal.pas',
  GS.Language.VM in '..\..\..\..\..\..\GS.Core\GS.Language.VM.pas',
  GS.Language.WordTokenizer in '..\..\..\..\..\..\GS.Core\GS.Language.WordTokenizer.pas';

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
