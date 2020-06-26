unit GS.Common.RingList;

interface

{$I GSCore.inc}
{$DEFINE USE_GENERIC}

uses
  SysUtils
  {$IFDEF USE_GENERIC}
  ,System.Generics.Collections
  {$Else}
  ,ContNrs
  {$ENDIF}
  ;


Type
{$IFDEF USE_GENERIC}
 TRingListOnChangeEvent<T> = procedure(sender : Tobject; CurrentFrom, CurrentNow : T) of Object;
 TRingList<T: class> = class
 private
   fList : TObjectList<T>;
   fCurrentIndex : Integer;
   fInternalChangeEnabled : boolean;
   fOnChange: TRingListOnChangeEvent<T>;

   function GetCurrent: T;
   procedure SetCurrent(const Value: T);
 protected
   procedure DoChange(s,d : T); virtual;
 public
   Constructor Create; virtual;
   Destructor Destroy; Override;

   procedure Add(_item : T);

   function Previous : T;
   function First : T;
   function Last : T;
   function Next : T;

   function IndexOf(Item : T) : integer;
   function Count : Integer;

   property Current : T read GetCurrent Write SetCurrent;

   property OnChangeItem : TRingListOnChangeEvent<T> read FOnChange Write FOnChange;
end;

{$ELSE}

 TRingListOnChangeEvent = procedure(sender : Tobject; CurrentFrom, CurrentNow : TObject) of Object;
 TRingList = class
 private
   fList : TObjectList;
   fCurrentIndex : Integer;
   fInternalChangeEnabled : boolean;
   fOnChange: TRingListOnChangeEvent;

   function GetCurrent: TObject;
   procedure SetCurrent(const Value: TObject);
 protected
   procedure DoChange(s,d : TObject); virtual;
 public
   Constructor Create; virtual;
   Destructor Destroy; Override;

   procedure Add(_item : TObject);

   function Previous : TObject;
   function First : TObject;
   function Last : TObject;
   function Next : TObject;

   function IndexOf(Item : TObject) : integer;
   function Count : Integer;

   property Current : TObject read GetCurrent Write SetCurrent;

   property OnChangeItem : TRingListOnChangeEvent read FOnChange Write FOnChange;
end;
{$ENDIF}

Implementation

{ TRingList }

{$IFDEF USE_GENERIC}
procedure TRingList<T>.Add(_item: T);
begin
  fList.Add(TObject(_item));
  if fCurrentIndex=-1 then
    fCurrentIndex := fList.Count-1;
end;

function TRingList<T>.Count: Integer;
begin
  result := fList.Count;
end;

constructor TRingList<T>.Create;
begin
  inherited;
  fList := TObjectList<T>.Create;
  fCurrentIndex := -1;
  fInternalChangeEnabled := true;
end;

destructor TRingList<T>.Destroy;
begin
  FreeAndNil(flist);
  inherited;
end;

procedure TRingList<T>.DoChange(s, d: T);
begin
  if Assigned(FOnChange) then
    if fInternalChangeEnabled then
      fOnChange(Self,s,d);
end;



function TRingList<T>.GetCurrent: T;
begin
  result := nil;
  if (fCurrentIndex>-1) and (fCurrentIndex<Flist.Count) then
    result := fList[fCurrentIndex];
end;

function TRingList<T>.IndexOf(item : T): integer;
begin
  Result := fList.IndexOf(item);
end;



function TRingList<T>.First: T;
var r : T;
begin
  result := GetCurrent;
  if fList.Count>0 then
  begin
    fCurrentIndex := 0;
  end;

  if Result<>GetCurrent then
  begin
    r := Result;
    Result := GetCurrent;
    DoChange(r,Result);
  end;
end;

function TRingList<T>.Last: T;
var r : T;
begin
  result := GetCurrent;
  if fList.Count>0 then
  begin
    fCurrentIndex := fList.Count-1;
  end;

  if Result<>GetCurrent then
  begin
    r := result;
    Result := GetCurrent;
    DoChange(r,Result);
  end;
end;

function TRingList<T>.Next: T;
var l : T;
begin
  fInternalChangeEnabled  := false;
  result := GetCurrent;
  inc(fCurrentIndex);
  l := GetCurrent;
  if l = nil then
    l := first;
  if l = nil then
    fCurrentIndex := -1;
  fInternalChangeEnabled  := true;

  if Result<>l then
  begin
    result := GetCurrent;
    DoChange(l,result);
  end;
end;

function TRingList<T>.Previous: T;
var l : T;
begin
  fInternalChangeEnabled  := false;
  result := GetCurrent;
  dec(fCurrentIndex);
  l := GetCurrent;
  if l = nil then
    l := Last;
  if l = nil then
    fCurrentIndex := -1;
  fInternalChangeEnabled  := true;

  if Result<>l then
  begin
    result := GetCurrent;
    DoChange(l,result);
  end;
end;

procedure TRingList<T>.SetCurrent(const Value: T);
var l : integer;
    a : T;
begin
  l := IndexOf(value);
  if l>-1 then
  begin
    if fCurrentIndex<>l then
    begin
      a := GetCurrent;
      fCurrentIndex := l;
      DoChange(a,Value);
    end;
  end;
end;



{$ELSE}



procedure TRingList.Add(_item: TObject);
begin
  fList.Add(TObject(_item));
  if fCurrentIndex=-1 then
    fCurrentIndex := fList.Count-1;
end;

function TRingList.Count: Integer;
begin
  result := fList.Count;
end;

constructor TRingList.Create;
begin
  inherited;
  fList := TObjectList.Create;
  fCurrentIndex := -1;
  fInternalChangeEnabled := true;
end;

destructor TRingList.Destroy;
begin
  FreeAndNil(flist);
  inherited;
end;

procedure TRingList.DoChange(s, d: TObject);
begin
  if Assigned(FOnChange) then
    if fInternalChangeEnabled then
      fOnChange(Self,s,d);
end;



function TRingList.GetCurrent: TObject;
begin
  result := nil;
  if (fCurrentIndex>-1) and (fCurrentIndex<Flist.Count) then
    result := fList[fCurrentIndex];
end;

function TRingList.IndexOf(item : TObject): integer;
begin
  Result := fList.IndexOf(item);
end;

function TRingList.First: TObject;
var r : TObject;
begin
  result := GetCurrent;
  if fList.Count>0 then
  begin
    fCurrentIndex := 0;
  end;

  if Result<>GetCurrent then
  begin
    r := Result;
    Result := GetCurrent;
    DoChange(r,Result);
  end;
end;

function TRingList.Last: TObject;
var r : TObject;
begin
  result := GetCurrent;
  if fList.Count>0 then
  begin
    fCurrentIndex := fList.Count-1;
  end;

  if Result<>GetCurrent then
  begin
    r := result;
    Result := GetCurrent;
    DoChange(r,Result);
  end;
end;

function TRingList.Next: TObject;
var l : TObject;
begin
  fInternalChangeEnabled  := false;
  result := GetCurrent;
  inc(fCurrentIndex);
  l := GetCurrent;
  if l = nil then
    l := first;
  if l = nil then
    fCurrentIndex := -1;
  fInternalChangeEnabled  := true;

  if Result<>l then
  begin
    result := GetCurrent;
    DoChange(l,result);
  end;
end;

function TRingList.Previous: TObject;
var l : TObject;
begin
  fInternalChangeEnabled  := false;
  result := GetCurrent;
  dec(fCurrentIndex);
  l := GetCurrent;
  if l = nil then
    l := Last;
  if l = nil then
    fCurrentIndex := -1;
  fInternalChangeEnabled  := true;

  if Result<>l then
  begin
    result := GetCurrent;
    DoChange(l,result);
  end;
end;

procedure TRingList.SetCurrent(const Value: TObject);
var l : integer;
    a : TObject;
begin
  l := IndexOf(value);
  if l>-1 then
  begin
    if fCurrentIndex<>l then
    begin
      a := GetCurrent;
      fCurrentIndex := l;
      DoChange(a,Value);
    end;
  end;
end;

{$ENDIF}

end.
