///-------------------------------------------------------------------------------
/// Title      : GS.BOListRec
/// Short Desc : Business Record list handler.
/// Source     : https://github.com/VincentGsell
/// Aim        : Get a reasonably fast list to access record by key (With Generic)
///              "Record" Counterpart of BOList (For Object)
///-------------------------------------------------------------------------------
/// History
/// 20180929 - VGS - Study to optimize a bit.
///                  Conclusion : better to let as is, becaus all optimization
///                  stuff are based to test in which type we are, and implement
///                  with native type.
///                  - Good plan should to introduce "optimizer" based on the type,
///                  and then change current system by dedicated one
///                  (exemple TofBusinessObjectListRec<String, String>
///                  ----> Decicated KeyValue engine. (dozen on the net.)
///                  - Another problem is the memory problem :
///                  current generic with optimization shadow list consume a lot
///                  of memory. Make a data ref, based on generic introduce
///                  new perforamce problem (Gneric comparer) :/
///-------------------------------------------------------------------------------

unit GS.BOList.Rec;

{$IFDEF FPC} {$mode delphi} {$H+} {$ENDIF}

interface

uses Classes, SysUtils,
{$IFDEF DELPHI}
     System.Generics.collections;
{$ELSE}
     Generics.collections;
{$ENDIF}

Type

TofBusinessObjectListRec<T,T2> = Class
Private
  FBusinessObject : TDictionary<T,T2>;    //Owned = False;
  FBusinessObjectListShadow : TList<T2>;      //Optimization : Access by Index.
  FBusinessObjectListShadowIndex : TList<T>;  //Optimization : for IndexOf().

  function GetBusinessObject(Key : T): T2;
  procedure SetBusinessObject(Key : T; const Value: T2);
  function GetBusinessObjectByIndex(Index: UInt64): T2;
  function GetBusinessObjectByObject(aObject: T2): T;
  function GetBusinessObjectKeyByIndex(Index: UInt64): T;
Public
  Constructor Create;
  Destructor Destroy; Override;

  Function ContainsKey(aKey : T) : Boolean;
  Function ContainsValue(aObject : T2) : Boolean;

  Function Count : Int64;

  Procedure Add(aKey : T; aValue : T2);

  Procedure RemoveKey(aKey : T); Overload;
  Procedure Remove(aValue : T2); Overload;

  Function IndexOf(Key : T) : Int64;

  Procedure Clear;

  Property KeyByIndex[Index : UInt64] : T read GetBusinessObjectKeyByIndex;
  Property ByIndex[Index : UInt64] : T2 read GetBusinessObjectByIndex;
  Property ByValue[aObject : T2] : T read GetBusinessObjectByObject; //Warning, could very long uin current impl. (Index of)
  Property ByKey[aValue : T] : T2 read GetBusinessObject Write SetBusinessObject; Default;
End;

implementation

{ TofBusinessObjectListRec<T,T2> }

procedure TofBusinessObjectListRec<T,T2>.Add(aKey: T; aValue: T2);
var i : UInt64;
begin
  if Not(FBusinessObject.ContainsKey(aKey)) then
  begin
    FBusinessObject.Add(aKey,aValue);
    FBusinessObjectListShadow.Add(aValue);
    FBusinessObjectListShadowIndex.Add(aKey);
  end
  else
  begin
    raise Exception.Create('TofBusinessObjectListRec<T,T2>.Add : Key already exists.');
  end;
end;

procedure TofBusinessObjectListRec<T,T2>.Clear;
begin
  FBusinessObject.Clear;
//  FBusinessObjectListShadow.Clear;
  FBusinessObjectListShadowIndex.Clear;
end;

function TofBusinessObjectListRec<T,T2>.ContainsKey(aKey: T): Boolean;
begin
  Result := FBusinessObject.ContainsKey(aKey);
end;

function TofBusinessObjectListRec<T,T2>.ContainsValue(aObject: T2): Boolean;
var i : Integer;
begin
  Result := FBusinessObject.ContainsValue(aObject);
end;

function TofBusinessObjectListRec<T,T2>.Count: Int64;
begin
  Result := FBusinessObject.Count;
end;

constructor TofBusinessObjectListRec<T,T2>.Create;
begin
  Inherited;
  FBusinessObject := TDictionary<T,T2>.Create;
  FBusinessObjectListShadow := TList<T2>.Create;
  FBusinessObjectListShadowIndex := TList<T>.Create;
end;

destructor TofBusinessObjectListRec<T,T2>.Destroy;
begin
  FBusinessObject.Clear;
  FBusinessObjectListShadow.Clear;
  FBusinessObjectListShadowIndex.Clear;
  FreeAndNil(FBusinessObject);
  FreeAndNil(FBusinessObjectListShadow);
  FreeAndNil(FBusinessObjectListShadowIndex);
  inherited;
end;

function TofBusinessObjectListRec<T,T2>.GetBusinessObject(Key: T): T2;
begin
  try
    Result := FBusinessObject[Key];
  Except
    On E : Exception do
    begin
      raise Exception.Create('TofBusinessObjectListRec<T,T2>.GetBusinessObject : '+E.Message);
    end;
  end;
end;

function TofBusinessObjectListRec<T,T2>.GetBusinessObjectByIndex(
  Index: UInt64): T2;
begin
  try
    Result := T2(FBusinessObjectListShadow[Index]);
  Except
    On E : Exception do
    begin
      raise Exception.Create('TofBusinessObjectListRec<T,T2>.GetBusinessObjectByIndex (ByIndex[Int64]) : '+E.Message);
    end;
  end;
end;

function TofBusinessObjectListRec<T,T2>.GetBusinessObjectByObject(
  aObject: T2): T;
var i : Int64;
begin
  i := FBusinessObjectListShadow.IndexOf(aObject);
  if i>-1 then
    Result := T(FBusinessObjectListShadowIndex[i])
  else
    raise Exception.Create(ClassName+' : Object not found');
end;

function TofBusinessObjectListRec<T, T2>.GetBusinessObjectKeyByIndex(
  Index: UInt64): T;
begin
  try
    Result := T(FBusinessObjectListShadowIndex[Index]);
  Except
    On E : Exception do
    begin
      raise Exception.Create('TofBusinessObjectListRec<T,T2>.GetBusinessObjectKeyByIndex (ByIndex[Int64]) : '+E.Message);
    end;
  end;

end;

function TofBusinessObjectListRec<T, T2>.IndexOf(Key: T): Int64;
begin
  Result := FBusinessObjectListShadowIndex.IndexOf(Key);
end;

procedure TofBusinessObjectListRec<T,T2>.RemoveKey(aKey: T);
var a :T2;
begin
  a := FBusinessObject[aKey];
  FBusinessObject.Remove(aKey);
  FBusinessObjectListShadow.Remove(a);
  FBusinessObjectListShadowIndex.Remove(aKey);
end;

procedure TofBusinessObjectListRec<T,T2>.Remove(aValue: T2);
var a : T;
    k : T2;
    i : int64;
begin
  i := FBusinessObjectListShadow.IndexOf(aValue);
  if i>-1 then
  begin
    a := FBusinessObjectListShadowIndex[i];
    RemoveKey(a);
  end;
end;

procedure TofBusinessObjectListRec<T,T2>.SetBusinessObject(Key: T; const Value: T2);
var str : string;
    i : Int64;
    ov : T2;
begin
  if FBusinessObject.ContainsKey(Key) then
  begin
    i := IndexOf(Key);
    ov := FBusinessObject[Key];
    FBusinessObject[Key] := Value;
    FBusinessObjectListShadow[i] := Value;
    FBusinessObjectListShadowIndex[i] := Key;
//    raise Exception.Create('TofBusinessObjectListRec<T,T2>.SetBusinessObject : Key already present in List : Delete it before.');
//    FBusinessObject[Key] := Value
//    if FBusinessObject[Key] <> Value then
//      raise Exception.Create('TofBusinessObjectListRec<T,T2>.SetBusinessObject : Key already present in List,  but object is different : Remove First.');
  end
  else
  begin
    FBusinessObject.Add(Key,Value);
    FBusinessObjectListShadow.Add(Value);
    FBusinessObjectListShadowIndex.Add(Key);
  end;
end;


end.

