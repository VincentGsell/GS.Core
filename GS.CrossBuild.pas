unit GS.CrossBuild;

interface

uses Classes, sysUtils;

type
TGSCProject = class
protected
public
  //Type lib, app, appCli, appGui
end;

TGSCCompilerMode =(ccmDebug, ccmRelease);
TGSCCompiler = Class
protected
  FMode : TGSCCompilerMode;
  FCompilerBinary : string;

  function GetVersion : string; virtual; abstract;
  function GetTargetCPU : string; virtual; abstract;
  function GetTargetOS : string; virtual; abstract;
public
  property Mode : TGSCCompilerMode read FMode write FMode;
  property CompilerBinary : string read FCompilerBinary Write FCompilerBinary;
  property Version : string read GetVersion;
  property TargetCPU : string read GetTargetCPU;
  property TargetOS : string read GetTargetOS;
End;

TGSCDirectories = Class
End;

TGSCDirectory = class
end;


TGSCEnv = class
protected
  function GetCompiler : TGSCCompiler; virtual; Abstract;
  function GetIncludeDirectories : TGSCDirectories; virtual; abstract;
  function GetOutDirectory : TGSCDirectory; virtual; abstract;
public
  property CompilerBackEnd : TGSCCompiler read GetCompiler;
  property IncludeDirectories : TGSCDirectories read GetIncludeDirectories;
  property OutDirectory : TGSCDirectory read GetOutDirectory;
end;

TGSCrossBuild = class
protected
  function GetProject : TGSCProject; virtual; abstract;
  function GetEnv : TGSCEnv; virtual; abstract;
public
  property Project : TGSCProject read GetProject;
  property Environments : TGSCEnv read GetEnv;

  procedure Clean;
  procedure StartBuild; virtual; abstract;
  procedure StartBuildAndRun; virtual; abstract;
  procedure StartRun; virtual; abstract;
end;

implementation

end.
