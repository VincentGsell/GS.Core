unit GS.System.Processes;

interface

uses sysutils,
     classes,
     GS.Common;

Type

IGSProcesses = interface
  function processList : iGSStringList;
end;

IGSProcessLauncher = interface
  function processLaunch(aCommand : string; out aReturnCode : integer) : string;
end;


IGSCodeRunner = Interface
  function Run(aCode : string) : string;
End;

//Injection : Factory stuff

TCustomnGSCodeRunnerFactory = class
  class function RunnerName : String; virtual; abstract;
  class function GetInstance : IGSCodeRunner; virtual; abstract;
end;

TGSCodeRunnerFactoryClass = class of TCustomnGSCodeRunnerFactory;

TGSRegisteredRunnerFactory = class
protected
  class var FRunList : TArray<TGSCodeRunnerFactoryClass>;
public
  class function GetRunnerInstance(name : String) : IGSCodeRunner;
  class procedure RegisterRunnerFactory(factory : TGSCodeRunnerFactoryClass);
end;

implementation

{ TGSRegisteredRunner }

class function TGSRegisteredRunnerFactory.GetRunnerInstance(
  name: String): IGSCodeRunner;
var i : integer;
begin
  for i:= Low(FRunList) to High(FRunList) do
    if  FRunList[i].RunnerName.ToLower = name.ToLower then begin
      result := FRunList[i].GetInstance;
      Exit;
    end;

  raise Exception.Create('GetRunnerInstance "'+name+'" not found');
end;

class procedure TGSRegisteredRunnerFactory.RegisterRunnerFactory(factory: TGSCodeRunnerFactoryClass);
var i : integer;
begin
  for i:= Low(FRunList) to High(FRunList) do
    if  FRunList[i].RunnerName = factory.RunnerName then
       Exit;
  i := Length(FRunList);
  SetLength(FRunList,i+1);
  FRunList[i] := factory;
end;

end.
