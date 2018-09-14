program BaseTestConsole;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  GS.Reference in '..\..\..\GS.Reference.pas';

var a : TofReferenceTest;
    lfr : String;
begin
  try
    a := TofReferenceTest.Create;

    if a.TestWriteSimple(lfr) then
    begin
      writeln('TestWriteSimple : Ok');
    end
    else
    begin
      Writeln('TestWriteSimple Fail : '+lfr);
    end;
    readln;

    if a.TestWriteStressSimple(1200,lfr) then
    begin
      writeln('TestWriteHuge : Ok');
    end
    else
    begin
      Writeln('TestWriteHuge Fail : '+lfr);
    end;
    readln;

    if a.TestreadForAllocationReconstitution(lfr) then
    begin
      writeln('TestreadForAllocationReconstitution : Ok');
    end
    else
    begin
      Writeln('TestreadForAllocationReconstitution Fail : '+lfr);
    end;
    Readln;

    if a.TestreadStressFileresult(lfr) then
    begin
      writeln('TestreadStressFileresult : Ok');
    end
    else
    begin
      Writeln('TestreadStressFileresult Fail : '+lfr);
    end;
    Readln;

    if a.TestreadAndRewrite(lfr) then
    begin
      a.TestreadForAllocationReconstitution(lfr);
      writeln('TestreadAndRewrite : Ok');
    end
    else
    begin
      Writeln('TestreadAndRewrite Fail : '+lfr);
    end;
    readln;

    if a.TestreadAndmassRewrite(lfr) then
    begin
      writeln('TestreadAndmassRewrite : Ok');
    end
    else
    begin
      Writeln('TestreadAndmassRewrite Fail : '+lfr);
    end;
    readln;


  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
