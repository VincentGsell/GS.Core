unit GS.Language.VM;

interface

{$IFDEF FPC}
{$mode delphi}{$H+}
{$ENDIF FPC}


uses sysUtils, classes,
      Generics.Collections,
      GS.Language.AlgoProgram,
      GS.Language.AlgoProgram.Runner;

type
  TVirtualMachine = class
  public
    procedure load(_program : TAlgoProgram); virtual; abstract;
    procedure run; virtual; abstract;

    procedure stdOutCallBack(data : string); virtual; abstract;
  end;


  //RAPI stand for : "Raw AlgoProgram Interpreter".
  //This machine run algoProgram raw from the code emmission,
  //Pure soft, no hardware or com'layer, without optimization.
  //Usage : "Load(algoProgram) will build internal code of the TalgoProgramRunner.
  //        then, run it without reloading, by changing variable value :
  //        It will be fast, because there are no recompilation process.
  TVirtualMachineRAPI = class(TVirtualMachine)
  protected
    fprogram : TAlgoProgram; //pointer;
    fProgramRunner : TAlgoProgramRunner;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure load(_program : TAlgoProgram); override;
    procedure run; override;

    function GetVarAsDouble(varname : string) : double;
    function GetVarAsInteger(varname : string) : Integer;
    function GetVarAsString(varname : string) : String;
  end;

implementation


{ TVirtualMachine_RawTest }


constructor TVirtualMachineRAPI.Create;
begin
  inherited create;
  fProgramRunner := TAlgoProgramRunner.Create;
end;

destructor TVirtualMachineRAPI.destroy;
begin
  freeAndNil(fProgramRunner);
  inherited;
end;

function TVirtualMachineRAPI.GetVarAsDouble(varname: string): double;
begin
  result := fProgramRunner.GetVar(varname).AsDouble;
end;

function TVirtualMachineRAPI.GetVarAsInteger(varname: string): Integer;
begin
  result := fProgramRunner.GetVar(varname).AsInteger;
end;

function TVirtualMachineRAPI.GetVarAsString(varname: string): String;
begin
  result := fProgramRunner.GetVar(varname).asString;
end;

procedure TVirtualMachineRAPI.load(_program: TAlgoProgram);
begin
  assert(Assigned(_program));
  fprogram := _program;
  fProgramRunner.build(fprogram);
end;

procedure TVirtualMachineRAPI.run;
var I: Integer;
begin
  Assert(assigned(fProgramRunner));
  for I := 0 to fProgramRunner.InstructionCodeCount-1 do
    fProgramRunner.InstructionCode[I].execute;
end;



end.
