///-------------------------------------------------------------------------------
/// Title      : GS.BOList
/// Short Desc : Business Object list handler.
/// Source     : https://github.com/VincentGsell
/// Aim        : Get a reasonably fast list to access object by key (Generic)
///              "Object" Counterpart of BOListRec (For Record)
///-------------------------------------------------------------------------------
unit GS.BOList;

{$IFDEF FPC} {$mode delphi} {$H+} {$ENDIF}

interface

uses Classes, SysUtils,
{$IFDEF DELPHI}
     System.Generics.collections;
{$ELSE}
     Generics.collections;
{$ENDIF}

Type

TofBusinessObjectList<T> = Class
Private
  FBusinessObject : TDictionary<T,TObject>;    //Owned = False; (Not TObjectDictionnary.)
  FBusinessObjectB : TDictionary<TObject,T>;   //Optimization : Because run throught Dictionnary is slow.
  FBusinessObjectListShadow : TList<TObject>;  //Optimization : Access by Index.

  function GetBusinessObject(Key : T): TObject;
  procedure SetBusinessObject(Key : T; const Value: TObject);
  function GetBusinessObjectByIndex(Index: uInt64): TObject;
    function GetBusinessObjectByObject(aObject: TObject): T;
Public
  Constructor Create;
  Destructor Destroy; Override;

  Function ContainsKey(aKey : T) : Boolean;
  Function ContainsValue(aObject : TObject) : Boolean;

  Function Count : uInt64;

  Procedure Add(aKey : T; aValue : TObject);
  Procedure ChangeKey(NewOne: T; aObject: TObject);

  Procedure RemoveKey(aKey : T); Overload;
  Procedure Remove(aValue : TObject); Overload;

  Procedure Clear;

  Property ByIndex[Index : uInt64] : TObject read GetBusinessObjectByIndex;
  Property ByObject[aObject : TObject] : T read GetBusinessObjectByObject;
  Property ByValue[aValue : T] : TObject read GetBusinessObject Write SetBusinessObject; Default;
End;

implementation

{ TofBusinessObjectList<T> }

procedure TofBusinessObjectList<T>.Add(aKey: T; aValue: TObject);
begin
  if Not(FBusinessObject.ContainsKey(aKey)) then
  begin
    FBusinessObject.Add(aKey,aValue);
    FBusinessObjectB.Add(aValue,aKey);
    FBusinessObjectListShadow.Add(aValue);
  end
  else
  begin
    raise Exception.Create('TofBusinessObjectList<T>.Add : Key already exists.');
  end;
end;

procedure TofBusinessObjectList<T>.ChangeKey(NewOne: T; aObject: TObject);
var g : T;
begin
  if FBusinessObject.ContainsKey(NewOne) then
  begin
    raise Exception.Create('TofBusinessObjectList<T>.ChangeKey : Cannot change NewKey because it is already used.');
  end;

  if FBusinessObject.ContainsValue(aObject) then
  begin
    g := FBusinessObjectB[aObject];
    FBusinessObject.Remove(g);
    FBusinessObjectB.Remove(aObject);
    FBusinessObjectListShadow.Remove(aObject);

    FBusinessObject.Add(NewOne,aObject);
    FBusinessObjectB.Add(aObject,NewOne);
    FBusinessObjectListShadow.Add(aObject);
  end;
end;

procedure TofBusinessObjectList<T>.Clear;
begin
  FBusinessObject.Clear;
  FBusinessObjectB.Clear;
  FBusinessObjectListShadow.Clear;
end;

function TofBusinessObjectList<T>.ContainsKey(aKey: T): Boolean;
begin
  Result := FBusinessObject.ContainsKey(aKey);
end;

function TofBusinessObjectList<T>.ContainsValue(aObject: TObject): Boolean;
begin
  Result := FBusinessObject.ContainsValue(aObject);
end;

function TofBusinessObjectList<T>.Count: uInt64;
begin
  Result := FBusinessObject.Count;
end;

constructor TofBusinessObjectList<T>.Create;
begin
  Inherited;
  FBusinessObject := TDictionary<T,TObject>.Create;
  FBusinessObjectB := TDictionary<TObject,T>.Create;
  FBusinessObjectListShadow := TList<TObject>.Create;
end;

destructor TofBusinessObjectList<T>.Destroy;
begin
  FBusinessObject.Clear;
  FBusinessObjectB.Clear;
  FBusinessObjectListShadow.Clear;
  FreeAndNil(FBusinessObject);
  FreeAndNil(FBusinessObjectB);
  FreeAndNil(FBusinessObjectListShadow);
  inherited;
end;

function TofBusinessObjectList<T>.GetBusinessObject(Key: T): TObject;
begin
  try
    Result := FBusinessObject[Key];
  Except
    On E : Exception do
    begin
      raise Exception.Create('TofBusinessObjectList<T>.GetBusinessObject : '+E.Message);
    end;
  end;
end;

function TofBusinessObjectList<T>.GetBusinessObjectByIndex(
  Index: uInt64): TObject;
begin
  try
    Result := TObject(FBusinessObjectListShadow[Index]);
  Except
    On E : Exception do
    begin
      raise Exception.Create('TofBusinessObjectList<T>.GetBusinessObjectByIndex (ByIndex[uint64]) : '+E.Message);
    end;
  end;
end;

function TofBusinessObjectList<T>.GetBusinessObjectByObject(
  aObject: TObject): T;
begin
  Result := FBusinessObjectB[aObject];
end;

procedure TofBusinessObjectList<T>.RemoveKey(aKey: T);
var a :TObject;
begin
  a := FBusinessObject[aKey];
  if Assigned(a) then
  begin
    FBusinessObject.Remove(aKey);
    FBusinessObjectB.Remove(a);
    FBusinessObjectListShadow.Remove(a);
  end;
end;

procedure TofBusinessObjectList<T>.Remove(aValue: TObject);
var a : T;
    str : String;
begin
  a := FBusinessObjectB[aValue];
  if FBusinessObject.ContainsKey(a) then
  begin
    RemoveKey(a);
  end;
end;

procedure TofBusinessObjectList<T>.SetBusinessObject(Key: T; const Value: TObject);
var str : string;
begin
  if FBusinessObject.ContainsKey(Key) then
  begin
    if FBusinessObject[Key] <> Value then
      raise Exception.Create('TofBusinessObjectList<T>.SetBusinessObject : Key already present in List,  but object is different : Remove First.');
  end
  else
  begin
    FBusinessObject.Add(Key,Value);
    FBusinessObjectB.Add(Value,Key);
    FBusinessObjectListShadow.Add(Value);
  end;
end;


end.
