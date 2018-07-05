///-------------------------------------------------------------------------------
/// Title      : GS.Threads
/// Short Desc : Introduce protected value.
/// Source     : https://github.com/VincentGsell
/// Aim        : - Access to value within a thread or from another thread,
//                 pointing directly to the value, with no more care.
///              - GetThreadID FPC/Delphi Win and Linux.
///-------------------------------------------------------------------------------
unit GS.Threads;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  SyncObjs;
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.SyncObjs,
  System.Threading;
{$ENDIF}


Type
  //TProtectecValue : Basic type CS protected : To not use TMonitor.
  //USE IT CAREFULLY, because of ressource taken by CriticalSection.
  //Use it for your convenience only if you need
  //To access "easely" to an alien thread property.
  TProtectedValue<T> = Class
    FT : T;
    FPrivateCS : TCriticalSection;
  Protected
    function GetValue: T;
    procedure SetValue(const Value: T);
  Public
    constructor Create(aValue : T); Reintroduce;
    Destructor Destroy; Override;

    //Manual lock/Unlock. To protect as  "Lock; try [code] finally Unlock; end;.
    procedure Lock;
    procedure UnLock;

    Property Value : T read GetValue Write SetValue;
  End;

  TProtectedNativeUInt = Class(TProtectedValue<NativeUInt>)
  Public
    Procedure Inc;
    Procedure Add(aValue : NativeUInt);
  End;
  TProtectedInt64 = Class(TProtectedValue<Int64>)
  Public
    Procedure Inc;
    Procedure Dec;
    Procedure Add(aValue : NativeUInt);
  End;

  TProtectedString = Class(TProtectedValue<String>);
  TProtectedBoolean = Class(TProtectedValue<Boolean>);
  TProtectedDateTime = Class(TProtectedValue<TDateTime>);
  TProtectedSingle = Class(TProtectedValue<Single>);
  TProtectedDouble = Class(TProtectedValue<Double>);

  ///
  /// Threaded proof for object (and thus, Tlist, TObjectList...)
  ///
  TProtectedObject<T:class> = class
  protected
    FInstance : T;
    FLock : TCriticalSection;
  public
    constructor Create(aObject : T); Reintroduce;
    destructor Destroy; Override;

    Function Lock : T;
    Procedure Unlock;
  end;

  {$IF Defined(MSWINDOWS)}
    TOTThreadID = LongWord;
  {$ELSE}
    //LINUX}
    TOTThreadID = NativeUInt;
  {$ENDIF}
  Function GetThreadID : TOTThreadID;

implementation

  Function GetThreadID : TOTThreadID;
  begin
    {$IF Defined(MSWINDOWS)}
      {$IFDEF FPC}
    Result := TOTThreadID(ThreadID);
      {$ELSE}
    Result := TOTThreadID(System.Classes.TThread.CurrentThread.ThreadID);
      {$ENDIF}
    {$ELSE}
      {$IFDEF LINUX}
    Result := TOTThreadID(ThreadID);
      {$ENDIF}
    {$ENDIF}
  end;

{ TProtectedValue<T> }

constructor TProtectedValue<T>.Create(aValue: T);
begin
  FPrivateCS := TCriticalSection.Create;
  FT := aValue;
end;

destructor TProtectedValue<T>.Destroy;
begin
  FreeAndNil(FPrivateCS);
  inherited;
end;

function TProtectedValue<T>.GetValue: T;
begin
  FPrivateCS.Enter;
  try
    Result := FT;
  finally
    FPrivateCS.Leave;
  end;
end;


procedure TProtectedValue<T>.Lock;
begin
  FPrivateCS.Enter;
end;

procedure TProtectedValue<T>.SetValue(const Value: T);
begin
  FPrivateCS.Enter;
  try
    FT := Value;
  finally
    FPrivateCS.Leave;
  end;
end;

procedure TProtectedValue<T>.UnLock;
begin
  FPrivateCS.Leave;
end;

{ TProtectedNativeUInt }

procedure TProtectedNativeUInt.Add(aValue: NativeUInt);
begin
  //Value := Value + aValue; This is correct, but less efficient than the fellowing (1 lock only)
  FPrivateCS.Enter;
  try
    FT := FT + aValue;
  finally
    FPrivateCS.Leave;
  end;
end;

procedure TProtectedNativeUInt.Inc;
begin
  //Value := Value + 1; This is correct, but less efficient than the fellowing (1 lock only)
  FPrivateCS.Enter;
  try
    FT := FT + 1;
  finally
    FPrivateCS.Leave;
  end;
end;


{ TProtectedInt64 }

procedure TProtectedInt64.Add(aValue: NativeUInt);
begin
  //Same note than TPRotectedNativeUInt;
  FPrivateCS.Enter;
  try
    FT := FT + aValue;
  finally
    FPrivateCS.Leave;
  end;
end;

procedure TProtectedInt64.Dec;
begin
  //Same note than TPRotectedNativeUInt;
  FPrivateCS.Enter;
  try
    FT := FT - 1;
  finally
    FPrivateCS.Leave;
  end;
end;

procedure TProtectedInt64.Inc;
begin
  //Same note than TPRotectedNativeUInt;
  FPrivateCS.Enter;
  try
    FT := FT + 1;
  finally
    FPrivateCS.Leave;
  end;
end;


{ TProtectedObject<T> }

constructor TProtectedObject<T>.Create(aObject : T);
begin
  Inherited Create;
  Assert(Assigned(aObject));
  FLock :=  TCriticalSection.Create;
  FInstance := aObject;
end;

destructor TProtectedObject<T>.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FInstance);
  inherited;
end;

function TProtectedObject<T>.Lock: T;
begin
  FLock.Acquire;
  Result := FInstance;
end;

procedure TProtectedObject<T>.Unlock;
begin
  FLock.Release;
end;

end.

