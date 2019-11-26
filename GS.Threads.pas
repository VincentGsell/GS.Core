///-------------------------------------------------------------------------------
/// Title      : GS.Threads
/// Short Desc : Introduce protected value.
/// Source     : https://github.com/VincentGsell
/// Aim        : - Access to value within a thread or from another thread,
//                 pointing directly to the value, with no more care.
///              - GetThreadID FPC/Delphi Win and Linux.
///              - Since Multicore arch. aligned data is no longer guaranteed.
///                Use TGSProtectedItem descandant for protect the exposed data.
///-------------------------------------------------------------------------------
unit GS.Threads;

{$I GSCore.inc}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  SyncObjs,
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.SyncObjs,
  System.Threading,
{$ENDIF}
  GS.Common;

Type
  TGSProtectedItem = class
  protected
    FCriticalSection: TCriticalSection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Lock : TObject; Virtual;
    procedure Unlock;
  end;

  TGSProtectedObject = class(TGSProtectedItem)
  private
  protected
    FObject : TObject;
    FOwned : Boolean;
  public
    Constructor Create(aObject : TObject; const OwnedObject : Boolean = True); reintroduce;
    Destructor Destroy; Override;
    function Lock : TObject; Override;
    function tryLock(obj : TObject) : boolean;
  end;

  TGSProtectedBoolean = Class(TGSProtectedItem)
  private
    FBool : Boolean;
    function GetValue: Boolean;
    procedure SetValue(const Value: Boolean);
  public
    Constructor Create(aBool : Boolean); Reintroduce;

    Function Switch : Boolean;

    Property Value : Boolean read GetValue Write SetValue;
  End;

  TGSProtectedByte = Class(TGSProtectedItem)
  private
    FByte : Byte;
    function GetValue: Byte;
    procedure SetValue(const Value: Byte);
  public
    Constructor Create(aByte : Byte); Reintroduce;
    Property Value : Byte read GetValue Write SetValue;
  End;

  TGSProtectedUint32 = Class(TGSProtectedItem)
  private
    FValue : Uint32;
    function GetValue: Uint32;
    procedure SetValue(const Value: Uint32);
  public
    Constructor Create(aValue : Uint32); Reintroduce;

    Function Inc : Uint32;
    Function Dec : Uint32;
    Function Add(aValue : Uint32) : Uint32;

    Property Value : Uint32 read GetValue Write SetValue;
  End;

  TGSProtectedString = Class(TGSProtectedItem)
  private
    FString : String;
    function GetValue: String;
    procedure SetValue(const Value: String);
  public
    Constructor Create(Const aString : String); Reintroduce;
    Property Value : String read GetValue Write SetValue;
  End;

  TGSProtectedInt64 = Class(TGSProtectedItem)
  private
    FInt64 : Int64;
    function GetValue: Int64;
    procedure SetValue(const Value: Int64);
  public
    Constructor Create(aInt64 : Int64); Reintroduce;
    Function Inc : Int64;
    Function Dec : Int64;

    Property Value : Int64 read GetValue Write SetValue;
  End;

  TGSProtectedDateTime = Class(TGSProtectedItem)
  private
    FDateTime : TDateTime;
    function GetValue: TDateTime;
    procedure SetValue(const Value: TDateTime);
  public
    Constructor Create(aDateTime : TDateTime); Reintroduce;
    Property Value : TDateTime read GetValue Write SetValue;
  End;

  TGSProtectedDouble = Class(TGSProtectedItem)
  private
    FDouble : Double;
    function GetValue: Double;
    procedure SetValue(const Value: Double);
  public
    Constructor Create(aDouble : Double); Reintroduce;
    Property Value : Double read GetValue Write SetValue;
  End;


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
      {$IFDEF FPC}
    Result := TOTThreadID(ThreadID);
      {$ELSE}
    result := TTHread.CurrentThread.ThreadID; //10.3
      {$ENDIF}
    {$ENDIF}
    {$ENDIF}
  end;


{ TGSProtectedItem }

constructor TGSProtectedItem.Create;
begin
  Inherited;
  FCriticalSection := TCriticalSection.Create;
end;

destructor TGSProtectedItem.Destroy;
begin
  FreeAndNil(FCriticalSection);
  inherited;
end;

function TGSProtectedItem.Lock : TObject;
begin
  FCriticalSection.Enter;
  result := FCriticalSection;
end;

procedure TGSProtectedItem.Unlock;
begin
  FCriticalSection.Leave;
end;



{ TGSProtectedObject }

constructor TGSProtectedObject.Create(aObject: TObject; const OwnedObject : Boolean);
begin
  Inherited Create;
  FObject := aObject;
  FOwned := OwnedObject;
end;

destructor TGSProtectedObject.Destroy;
begin
  if FOwned then
    FObject.Free;
  inherited;
end;

function TGSProtectedObject.Lock: TObject;
begin
  Inherited Lock;
  Result := FObject;
end;

function TGSProtectedObject.tryLock(obj: TObject): boolean;
begin
  result := FCriticalSection.TryEnter;
  if result then
  begin
    obj := FObject;
  end;
end;

{ TGSProtectedByte }

constructor TGSProtectedByte.Create(aByte: Byte);
begin
  Inherited Create;
  FByte := aByte;
end;

function TGSProtectedByte.GetValue: Byte;
begin
  Lock;
  try
    result := FByte;
  finally
    Unlock;
  end;
end;

procedure TGSProtectedByte.SetValue(const Value: Byte);
begin
  Lock;
  try
    FByte := Value;
  finally
    Unlock;
  end;
end;

{ TGSProtectedUint32 }

function TGSProtectedUint32.Add(aValue: Uint32): Uint32;
begin
  Lock;
  try
    FValue := FValue + aValue;
    result := FValue;
  finally
    Unlock;
  end;
end;

constructor TGSProtectedUint32.Create(aValue: Uint32);
begin
  inherited Create;
  FValue := aValue;
end;

function TGSProtectedUint32.Dec: Uint32;
begin
  lock;
  try
    FValue := FValue - 1;
    result := FValue;
  finally
    Unlock;
  end;
end;

function TGSProtectedUint32.GetValue: Uint32;
begin
  lock;
  try
    result := FValue;
  finally
    Unlock;
  end;
end;

Function TGSProtectedUint32.Inc : UInt32;
begin
  lock;
  try
    FValue := FValue + 1;
    result := FValue;
  finally
    Unlock;
  end;
end;

procedure TGSProtectedUint32.SetValue(const Value: Uint32);
begin
  lock;
  try
    FValue := Value;
  finally
    Unlock;
  end;
end;

{ TGSProtectedBoolean }

constructor TGSProtectedBoolean.Create(aBool: Boolean);
begin
  inherited Create;
  FBool := aBool;
end;

function TGSProtectedBoolean.GetValue: Boolean;
begin
  lock;
  try
    result := FBool;
  finally
    UnLock;
  end;
end;

procedure TGSProtectedBoolean.SetValue(const Value: Boolean);
begin
  lock;
  try
    FBool := Value;
  finally
    UnLock;
  end;
end;

function TGSProtectedBoolean.Switch: Boolean;
begin
  lock;
  try
    FBool := Not(FBool);
    result := FBool;
  finally
    UnLock;
  end;
end;

{ TGSProtectedString }

constructor TGSProtectedString.Create(const aString: String);
begin
  Inherited Create;
  FString := aString;
end;

function TGSProtectedString.GetValue: String;
begin
  Lock;
  try
    result := FString;
  finally
    Unlock;
  end;
end;

procedure TGSProtectedString.SetValue(const Value: String);
begin
  Lock;
  try
    FString := Value;
  finally
    Unlock;
  end;
end;

{ TGSProtectedInt64 }

constructor TGSProtectedInt64.Create(aInt64: Int64);
begin
  Inherited Create;
  FInt64 := aInt64;
end;

function TGSProtectedInt64.Dec: Int64;
begin
  Lock;
  try
    FInt64 := FInt64 - 1;
    Result := FInt64;
  finally
    Unlock;
  end;
end;

function TGSProtectedInt64.GetValue: Int64;
begin
  Lock;
  try
    result := FInt64;
  finally
    Unlock;
  end;
end;

function TGSProtectedInt64.Inc: Int64;
begin
  Lock;
  try
    FInt64 := FInt64 + 1;
    Result := FInt64;
  finally
    Unlock;
  end;
end;

procedure TGSProtectedInt64.SetValue(const Value: Int64);
begin
  Lock;
  try
    FInt64 := Value;
  finally
    Unlock;
  end;
end;

{ TGSProtectedDateTime }

constructor TGSProtectedDateTime.Create(aDateTime: TDateTime);
begin
  Inherited Create;
  FDateTime := aDateTime;
end;

function TGSProtectedDateTime.GetValue: TDateTime;
begin
  Lock;
  try
    result := FDateTime;
  finally
    Unlock;
  end;
end;

procedure TGSProtectedDateTime.SetValue(const Value: TDateTime);
begin
  Lock;
  try
    FDateTime := Value;
  finally
    Unlock;
  end;
end;

{ TGSProtectedDouble }

constructor TGSProtectedDouble.Create(aDouble: Double);
begin
  Inherited Create;
  FDouble := aDouble;
end;

function TGSProtectedDouble.GetValue: Double;
begin
  Lock;
  try
    Result := FDouble;
  finally
    Unlock;
  end;
end;

procedure TGSProtectedDouble.SetValue(const Value: Double);
begin
  Lock;
  try
    FDouble := Value;
  finally
    Unlock;
  end;
end;

end.

