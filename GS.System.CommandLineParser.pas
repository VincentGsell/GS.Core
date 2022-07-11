unit GS.System.CommandLineParser;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

{$IFNDEF FPC}
{$IF CompilerVersion >= 25}
  {$LEGACYIFEND ON}
{$IFEND}
{$ENDIF}


uses
  {$IFNDEF FPC}
  {$IF CompilerVersion >= 23}
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.StrUtils,
  System.Generics.Collections;
  {$ELSE}
  Classes,
  Contnrs,
  SysUtils,
  StrUtils,
  Generics.Collections;
  {$IFEND}
  {$ELSE}
  Classes,
  Contnrs,
  SysUtils,
  StrUtils,
  Generics.Collections;
  {$ENDIF}


type
  EPArameterAlreadyExists = class(Exception);


  iGSCommandLine = Interface
    procedure registerParameter(paramId : String; ParamsShortcut : TArray<String>; DescriptionHelp : string);
    function hasParameter(paramName : string) : boolean;
    function getParameter(paramName : string) : string;
    function getHelp : string;
    function CurrentParameterCount : Integer;
    function checksyntax : boolean;
  End;

  TGSCmdLineParam = record
    paramId : string;
    shortcutList : string;
    Desc : string;
  end;

  TGSCmdLineManager = class(TInterfacedObject, iGSCommandLine)
    FParams : TDictionary<string,TGSCmdLineParam>;
    FCMdLine : TArray<string>;
    function GetCmdLineStrAsList : String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure registerParameter(paramId : String; ParamsShortcut : TArray<String>; DescriptionHelp : string);
    function hasParameter(paramName : string) : boolean;
    function getParameter(paramName : string) : string;
    function getHelp : string;
    function CurrentParameterCount : Integer;
    function checksyntax : boolean;

    function IsShortCut(aParamName : String; out aParamItem : TGSCmdLineParam) : Boolean;
    function IsShortCutInSameParam(aParam1, aParam2 : String; out aParamItem : TGSCmdLineParam) : Boolean;
  end;




implementation


{ TGSCmdLineManager }

function TGSCmdLineManager.checksyntax: boolean;
var lp : TGSCmdLineParam;
begin
  result := true;
  if length(FCMdLine)>0 then
    result := IsShortCut(FCMDLine[0],lp);
end;

constructor TGSCmdLineManager.Create;
begin
  inherited;
  FParams := TDictionary<string,TGSCmdLineParam>.create;
  FCmdLine := GetCmdLineStrAsList.split([' ']);
end;

function TGSCmdLineManager.CurrentParameterCount: Integer;
begin
  result := length(FCMdLine);
end;

destructor TGSCmdLineManager.Destroy;
begin
  FreeAndNil(FParams);
  inherited;
end;

function TGSCmdLineManager.GetCmdLineStrAsList: String;
var i: Integer;
begin
  for i := 1 to ParamCount do
    Result := Result + ParamStr(I) + ' ';
  Result := Result.Trim;
end;

function TGSCmdLineManager.getHelp: string;
var l : TPair<string,TGSCmdLineParam>;
    lsc : string;
    lscAlternate : string;
    lscl : TArray<string>;
    i : integer;
begin
  if FParams.Count=0 then
    exit;
  for l in FParams do begin
    lsc := l.Value.paramId;
    lscl := l.Value.shortcutList.Split([' ',',',';']);
    if length(lscl)>1 then
      for i := Low(lscl) to High(lscl) do
        lscAlternate := lscAlternate + '-'+lscl[i]+ ' --'+lscl[i]+' ';
      lscAlternate := lscAlternate.Trim;
    writeln(format('- %s [%s])',[lsc,lscAlternate]));
    writeln(format('    %s',[l.Value.Desc]));
    writeln;
    lscAlternate := '';
  end;
end;

function TGSCmdLineManager.getParameter(paramName: string): string;
var l : TArray<String>;
    i,j: Integer;
    lf : string;
    aParam : TGSCmdLineParam;
begin
  l := FCmdLine;
  for i := 0 to Length(l)-1 do
    if (l[i].StartsWith('-')) or (l[i].StartsWith('--')) then begin
      lf := l[i].Replace('-','');
      if (lf = paramName) or IsShortCutInSameParam(paramname,lf,aParam) then begin
        if i<length(l)-1 then
          for j := i+1 to length(l)-1 do begin
            if (l[j].StartsWith('-')) or (l[j].StartsWith('--')) then
              break;
            result := result +l[j]+' ';
          end;
      end;
    end;
  result := result.Trim;
end;

function TGSCmdLineManager.hasParameter(paramName: string): boolean;
var l : TArray<String>;
    i: Integer;
    lf : string;
    aParam : TGSCmdLineParam;
begin
  result := False;
  l := FCmdLine;
  for i := 0 to Length(l)-1 do
    if (l[i].StartsWith('-')) or (l[i].StartsWith('--')) then begin
      lf := l[i].Replace('-','');
      if (lf = paramName) or IsShortCutInSameParam(paramname,lf,aParam)  then begin //direct parameter Or registered one.
        result := true;
        break;
      end;
    end;
end;

function TGSCmdLineManager.IsShortCut(aParamName: String;
  out aParamItem: TGSCmdLineParam): Boolean;
var l : TPair<string,TGSCmdLineParam>;
    lsc : TArray<string>;
    lp : string;
    i : integer;
begin
  result := false;
  if aParamName.Trim='' then
    Exit;
  lp := aParamName.ToLower;
  lp := lp.Replace('-','');
  for l in FParams do begin
    lsc := l.Value.shortcutList.Split([' ',',',';']);
    if length(lsc)>0 then
      for i := Low(lsc) to High(lsc) do
        if lsc[i].ToLower = lp then begin
          result := true;
          aParamItem := l.Value;
          exit;
        end;
  end;
end;

function TGSCmdLineManager.IsShortCutInSameParam(aParam1, aParam2: String;
  out aParamItem: TGSCmdLineParam): Boolean;
var a,b : TGSCmdLineParam;
begin
  result := IsShortCut(aParam1,a) and IsShortCut(aParam2,b) and (a.paramId = b.paramId);
end;

procedure TGSCmdLineManager.registerParameter(paramId: String;
  ParamsShortcut: TArray<String>; DescriptionHelp: string);
var lp : TGSCmdLineParam;
    i : integer;
begin
  if FParams.TryGetValue(paramId,lp) then
    raise EPArameterAlreadyExists.Create(paramId);

  for i := Low(ParamsShortcut) to High(ParamsShortcut) do
    if (IsShortCut(ParamsShortcut[i],lp)) Or (ParamsShortcut[i].ToLower = paramId.ToLower) then
      raise Exception.Create(ParamsShortcut[i]+' is already shortcuts of '+paramId.ToLower);

  lp.paramId := paramId;
  for i := Low(ParamsShortcut) to High(ParamsShortcut) do
    lp.shortcutList := lp.shortcutList + ParamsShortcut[i]+' ';
  lp.shortcutList := lp.shortcutList + paramId;
  lp.shortcutList := lp.shortcutList.Trim;
  lp.Desc := DescriptionHelp;
  FParams.Add(paramId,lp);
end;

end.
