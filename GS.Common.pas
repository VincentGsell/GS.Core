/// Title      : GS.Common
/// Short Desc : Common code, no dependancy.
/// Source     : https://github.com/VincentGsell
///
/// History
/// 20180101 - VGS - Create
/// 20190301 - VGS - Generic condition compilation (FPC Link problem).
unit GS.Common;

interface

{$I GSCore.inc}

Uses Classes, SysUtils,
    {$IFDEF USE_GENERIC}
    Generics.Collections,
   {$ENDIF}
    SyncObjs
    ;

Const
  {$I GSCoreConst.inc}
  CST_BUSTIMER = 250; //MilliSec.
  CST_DEFAULT_WILDCARD = '\';
  //Data repo feature extention,
  CST_DATAREPONAME_PREFIX = 'Sys.';
  CST_DATAREPONAME_SUFFIX = '.DataRepo';
  CST_ARRAY_INIT_QTE = 10;
  CST_DEFAULT_CYPH_KEY = 'efsdsadjpodjlkkassdsasrjhedfjhdbfsaiaweibw_WDcsew1212'+
                         'aweeosdnhehfalhgwmduzt1  .v 9^32e1cacjfalcurnclyaucal'+
                         '2384ehq b 8bz 2q*x"*c*&%t  has gf3qfauw xq33rt waza2q'+
                         'sugc gf8f2qr lbc8 29KZGZRNCQ43ZN2CAN.   eniwuz45835n4'+
                         'erraqaqi32zremmrkqncddqnkjdgfendshgflerkj<sjagfkudkzq'+
                         'zgfesibfnlir197g9332290 s 329em*%Refcq*Rfysnfcaw.3oiv'+
                         'lwsau4zreo nfli43emecue239brul.infawdhtnwufsu.nlfoiew'+
                         'me9854z842924986ttrvsger257vskjgiu*%e*asdwdjlwqkjlkej';

{$IFDEF FPC}
const
  INFINITE = Cardinal($FFFFFFFF);
{$ENDIF}


Type
  TKeyValueStringItem = Packed Record
    Key : UTF8String;
    Value : TObject;
  End;
  TStringItemArray = array of TKeyValueStringItem;

  TUTF8StringArray = array of UTF8String;

//Object list implementation for non generic usage.
TList_ObjectArray = class
private
protected
  FArray : Array of TObject;
  FIndex : UInt32;
  FInitialized : Boolean;
  FOwned : Boolean;
  Procedure ManagedAdd(aObject : TObject); Virtual;
  Procedure ManagedSet(Index : Uint32; aObject : TObject); Virtual;
  function GetCount: Uint32;
public
  constructor Create(Const Owned : Boolean = false); Reintroduce; Virtual;
  Destructor Destroy; Override;

  Procedure Clear; Virtual;
  Procedure Delete(Index : Uint32);
  Procedure Remove(aObject : TObject);

  Function IndexOf(aObject : TObject) : Int32;

  Property Owned : Boolean read FOwned Write FOwned;
  Property Count : Uint32 read GetCount;
end;

//Key Values (basic replacement for Generic Dictionnary key paired String/Object
TKeysValues_UTF8StringToObject = Class
private
protected
  FThreadProtector : TCriticalSection;
  FArray : TStringItemArray;
  FIndex : UInt32;
  FInitialized : Boolean;
  FSorted: Boolean;

  function GetItem(Index: UInt32): TKeyValueStringItem;
  procedure SetItem(Index: UInt32; const Value: TKeyValueStringItem);
  function GetCount: Uint32;
public
  Constructor Create; Virtual;
  Destructor Destroy; Override;

  Function TryGetValue(aKey : String; var aResult : TObject) : Boolean; virtual;
  Procedure Add(aKey : String; aObject : TObject); virtual;
  Procedure Remove(Index : UInt32; const FreeObject : Boolean = true); Overload;
  Procedure Remove(aKey : String; const FreeObject : Boolean = true); Overload;

  Procedure Clear; Virtual;

  procedure lock;
  procedure unlock;

  Property Count : Uint32 read GetCount;
  Property Items[Index : UInt32] : TKeyValueStringItem read GetItem Write SetItem; default;
end;


TList_UTF8String = Class
Private
  FArray : TUTF8StringArray;
  FIndex : UInt32;
  FInitialized : Boolean;
  FSorted: Boolean;
  function GetUTF8String(Index: Uint32): UTF8String;
  function GetUTF8StringCount: Uint32;
  procedure SetUTF8String(Index: Uint32; const Value: UTF8String);

Public
  constructor Create(const Sorted : Boolean = True); Reintroduce;
  Procedure Add(aString : UTF8String);
  Procedure Clear;
  Property Items[Index : Uint32] : UTF8String read GetUTF8String Write SetUTF8String; default;
  Property Count : Uint32 read GetUTF8StringCount;
  Property Sorted : Boolean read FSorted Write FSorted;
end;

{$IFDEF USE_GENERIC}
TObjectDictionnaryWithSequentialMode = class(TObjectDictionary<String,TObject>)
private
public
//  Property Items[Index : UInt32] : TObject read GetSeqItem Write SetSeqItem;
end;
TObjectDictionary_StringStream = TObjectDictionary<String,TStream>;
TObjectDictionary_StringObject = TObjectDictionary<String,TObject>;
{$ELSE}

TObjectDictionary_StringStream = Class(TKeysValues_UTF8StringToObject)
private
protected
public
  Function TryGetValue(aKey : String; var aResult : TStream) : Boolean; Reintroduce;
  Procedure Add(aString : String; aStream : TStream); Reintroduce;
End;

TObjectDictionary_StringObject = Class(TKeysValues_UTF8StringToObject)
private
protected
public
end;

{$ENDIF}

Procedure StreamEncrypt(aStreamToEncrypt : TMemoryStream);
Procedure StreamDecrypt(aStreamToDecrypt : TMemoryStream);
procedure QuickSort(var aArray: TStringItemArray; Start, Stop: Integer);
function BinSearch(aArray: TStringItemArray; aKey: UTF8String; Const Stop: Integer = -1): Integer;

var GLB_OTP_FILE : String; //One-time pad key file (https://en.wikipedia.org/wiki/One-time_pad) -> Should be localized in a secured dir/file.

implementation

function sencrypt_XorCipher(const Key, Source: TBytes): TBytes;
var
  I: Integer;
begin
  if Length(Key) = 0 then
    Exit(Source);
  SetLength(Result, Length(Source));
  for I := Low(Source) to High(Source) do
    Result[I] := Key[I mod Length(Key)] xor Source[I];
end;

procedure BetterThanNothingEncryptDecrypt(aStreamToEncrypt : TMemoryStream);
var b,c : TBytesStream;
    Key,r : TBytes;

    function GetOTPfile : TBytes;
    begin
      if FileExists(GLB_OTP_FILE) then
      begin
        c := TBytesStream.Create;
        try
          c.LoadFromFile(GLB_OTP_FILE);
          result := c.Bytes;
        finally
          FreeAndNil(c);
        end;
      end
      else
      begin
        Key:= BytesOf(CST_DEFAULT_CYPH_KEY); //-/
      end;
    end;
begin
  b := TBytesStream.Create;
  try
    b.LoadFromStream(aStreamToEncrypt);
    key := GetOTPFile;
    r := sencrypt_XorCipher(Key,b.Bytes);
    b.Clear;
    aStreamToEncrypt.Clear;
    c := TBytesStream.Create(r);
    try
      b.LoadFromStream(c);
    finally
      FreeAndNil(c);
    end;
    b.Position := 0;
    aStreamToEncrypt.LoadFromStream(b);
  finally
    FreeAndNil(b);
  end;
end;


{$IFDEF LIB_CIPHERS_ENABLED}
//uses ...; Here a nice ciphers lib.
{$ENDIF}

Procedure StreamEncrypt(aStreamToEncrypt : TMemoryStream);
{$IFDEF LIB_CIPHERS_ENABLED}
{$ENDIF}
begin
{$IFDEF LIB_CIPHERS_ENABLED}
  raise Exception.Create('To implement');
{$ELSE}
  BetterThanNothingEncryptDecrypt(aStreamToEncrypt);
{$ENDIF}
end;

Procedure StreamDecrypt(aStreamToDecrypt : TMemoryStream);
{$IFDEF LIB_CIPHERS_ENABLED}
{$ENDIF}
begin
{$IFDEF LIB_CIPHERS_ENABLED}
  raise Exception.Create('To implement');
{$ELSE}
  BetterThanNothingEncryptDecrypt(aStreamToDecrypt);
{$ENDIF}
end;


  {--------------------------------------------------------------------}
  //Start is the index of the first item of the array - usually 0
  //Stop is the index of the last item of the array
procedure QuickSort(var aArray: TStringItemArray; Start, Stop: Integer);
var
  Left: Integer;
  Right: Integer;
  Mid: Integer;
  Pivot: UTF8String;
  Temp:  TKeyValueStringItem;
begin
  Left  := Start;
  Right := Stop;
  Mid   := (Start + Stop) div 2;

  Pivot := aArray[mid].Key;
  repeat
    while aArray[Left].Key < Pivot do Inc(Left);
    while Pivot < aArray[Right].Key do Dec(Right);
    if Left <= Right then
    begin
      Temp           := aArray[Left];
      aArray[Left]  := aArray[Right]; // Swops the two Strings
      aArray[Right] := Temp;
      Inc(Left);
      Dec(Right);
    end;
  until Left > Right;

  if Start < Right then QuickSort(aArray, Start, Right); // Uses
  if Left < Stop then QuickSort(aArray, Left, Stop);     // Recursion
end;

procedure QuickSortStringArray(var aArray: TUTF8StringArray; Start, Stop: Integer);
var
  Left: Integer;
  Right: Integer;
  Mid: Integer;
  Pivot: UTF8String;
  Temp:  UTF8String;
begin
  Left  := Start;
  Right := Stop;
  Mid   := (Start + Stop) div 2;

  Pivot := aArray[mid];
  repeat
    while aArray[Left] < Pivot do Inc(Left);
    while Pivot < aArray[Right] do Dec(Right);
    if Left <= Right then
    begin
      Temp           := aArray[Left];
      aArray[Left]  := aArray[Right]; // Swops the two Strings
      aArray[Right] := Temp;
      Inc(Left);
      Dec(Right);
    end;
  until Left > Right;

  if Start < Right then QuickSortStringArray(aArray, Start, Right); // Uses
  if Left < Stop then QuickSortStringArray(aArray, Left, Stop);     // Recursion
end;
{--------------------------------------------------------------------}

function BinSearch(aArray: TStringItemArray; aKey: UTF8String; Const Stop: Integer = -1): Integer;
var
  First: Integer;
  Last: Integer;
  Pivot: Integer;
  Found: Boolean;
begin
  First  := Low(aArray); //Sets the first item of the range
  Last   := Stop; //Sets the last item of the range
  Found  := False; //Initializes the Found flag (Not found yet)
  Result := -1; //Initializes the Result

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (First <= Last) and (not Found) do
  begin
    //Gets the middle of the selected range
    Pivot := (First + Last) div 2;
    //Compares the String in the middle with the searched one
    if aArray[Pivot].Key = aKey then
    begin
      Found  := True;
      Result := Pivot;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if aArray[Pivot].Key > aKey then
      Last := Pivot - 1
        //else select the second half
    else
      First := Pivot + 1;
  end;
end;

{ TVisibilityThread }

{$IFDEF FPC}
function TVisibilityThread.GetStarted: Boolean;
begin
  result := not(Suspended); //Not same behaviour than DCC, but work in our case.
  //Normally, FStarted is set to true just before Execute.
end;
{$ENDIF}



{ TList_ObjectArray }

procedure TList_ObjectArray.Clear;
var i : Integer;
begin
  if FOwned then
  begin
    for I := 0 to Count-1 do
      TObject(FArray[i]).Free;
  end;
  FArray := Nil;
  SetLength(Farray,CST_ARRAY_INIT_QTE);
  FInitialized := False;
end;

constructor TList_ObjectArray.Create(const Owned: Boolean);
begin
  SetLength(Farray,CST_ARRAY_INIT_QTE);
  FInitialized := False;
  FOwned := Owned;
end;

procedure TList_ObjectArray.Delete(Index: Uint32);
var
  ALength: Uint32;
  i: Cardinal;
begin
  ALength := Length(FArray);
  Assert(ALength > 0);
  Assert(Index < ALength);
  if FOwned then
    FArray[Index].Free;
  for i := Index + 1 to ALength - 1 do
    FArray[i - 1] := FArray[i];
  SetLength(FArray, ALength - 1);
  Dec(FIndex);
end;

destructor TList_ObjectArray.Destroy;
begin
  Clear;
  inherited;
end;

function TList_ObjectArray.GetCount: Uint32;
begin
  result := FIndex;
end;

function TList_ObjectArray.IndexOf(aObject: TObject): Int32;
var i : Int32;
begin
  result := -1;
  for I := 0 to FIndex do
  begin
    if FArray[i] = aObject then
    begin
      result := i;
      break;
    end;
  end;
end;

procedure TList_ObjectArray.ManagedAdd(aObject: TObject);
begin
  Assert(Assigned(aObject));
  if FInitialized then
  begin
    if Int32(FIndex) = Length(FArray) then
    begin
      SetLength(FArray,(Length(FArray)+1)*2);
    end;
    FArray[FIndex] := aObject;
  end
  else
  begin
    FInitialized := true;
    FIndex := 0;
    FArray[FIndex] := aObject;
  end;
  Inc(FIndex);
end;


procedure TList_ObjectArray.ManagedSet(Index : Uint32; aObject: TObject);
var i : UInt32;
begin
  Assert(assigned(aObject));
  {$IFDEF DEBUG}
  for I := 0 to FIndex do
  begin
    if FArray[i] = aObject then
      raise Exception.Create('TList_ObjectArray : Duplicate item (Pos'+IntToStr(i)+')');
  end;
  {$ENDIF}
  FArray[Index] := aObject;
end;


procedure TList_ObjectArray.Remove(aObject : TObject);
var i : integer;
begin
  i := IndexOf(aObject);
  if i>-1 then
    Delete(i);
end;

{ TList_UTF8String }

procedure TList_UTF8String.Add(aString: UTF8String);
begin
  if FInitialized then
  begin
    if Int32(FIndex) = Length(FArray) then
    begin
      SetLength(FArray,Length(FArray)*2);
    end;
    FArray[FIndex] := aString;
  end
  else
  begin
    FInitialized := true;
    FIndex := 0;
    FArray[FIndex] := aString;
  end;
  Inc(FIndex);
  if FSorted then
    QuickSortStringArray(FArray,0,FIndex-1);
end;


procedure TList_UTF8String.Clear;
begin
  FArray := nil;
  SetLength(FArray,CST_ARRAY_INIT_QTE);
  FIndex := 0;
  FInitialized := False;
end;

constructor TList_UTF8String.Create(const Sorted : Boolean);
begin
  Inherited Create;
  FSorted := Sorted;
  Clear;
end;

function TList_UTF8String.GetUTF8String(Index: Uint32): UTF8String;
begin
  result := FArray[Index];
end;

function TList_UTF8String.GetUTF8StringCount: Uint32;
begin
  result := FIndex;
end;

procedure TList_UTF8String.SetUTF8String(Index: Uint32;
  const Value: UTF8String);
begin
  FArray[Index] := Value;
end;

{ TKeysValues_UTF8StringToObject }

procedure TKeysValues_UTF8StringToObject.Add(aKey: String; aObject: TObject);
begin
  if FInitialized then
  begin
    if Int32(FIndex) = Length(FArray) then
    begin
      SetLength(FArray,Length(FArray)*2);
    end;
    FArray[FIndex].Key := UTF8String(aKey);
    FArray[FIndex].Value := aObject;
  end
  else
  begin
    FInitialized := true;
    FIndex := 0;
    FArray[FIndex].Key := UTF8String(aKey);
    FArray[FIndex].Value := aObject;
  end;
  Inc(FIndex);
  QuickSort(FArray,0,FIndex-1);
end;

procedure TKeysValues_UTF8StringToObject.Clear;
begin
  FArray := nil;
  SetLength(FArray,CST_ARRAY_INIT_QTE);
  FIndex := 0;
  FInitialized := False;
end;

constructor TKeysValues_UTF8StringToObject.Create;
begin
  FThreadProtector := TCriticalSection.Create;
  Clear;
end;

destructor TKeysValues_UTF8StringToObject.Destroy;
begin
  FThreadProtector.Free;
  //Object Not Owned. Override if needed.
  inherited;
end;

function TKeysValues_UTF8StringToObject.GetCount: Uint32;
begin
  result := FIndex;
end;

function TKeysValues_UTF8StringToObject.GetItem(
  Index: UInt32): TKeyValueStringItem;
begin
  result := FArray[Index];
end;

procedure TKeysValues_UTF8StringToObject.lock;
begin
  FThreadProtector.Enter;
end;

procedure TKeysValues_UTF8StringToObject.Remove(aKey: String;
  const FreeObject: Boolean);
var l : int32;
begin
  l := BinSearch(FArray,UTF8String(aKey),FIndex);
  if l>-1 then
    Remove(l,FreeObject);
end;

procedure TKeysValues_UTF8StringToObject.Remove(Index: UInt32;
  const FreeObject: Boolean);
var llength : UInt32;
    i : integer;
begin
  llength := Length(FArray);
  if llength = 0 then
    exit;

  if Index<llength then
  begin
    if FreeObject then
      Items[Index].Value.Free;

    for i := Index + 1 to lLength - 1 do
      FArray[i - 1] := FArray[i];
    SetLength(FArray, lLength - 1);
    Dec(FIndex);
  end;
end;

procedure TKeysValues_UTF8StringToObject.SetItem(Index: UInt32;
  const Value: TKeyValueStringItem);
begin
  FArray[Index] := Value;
end;

function TKeysValues_UTF8StringToObject.TryGetValue(aKey: String;
  var aResult: TObject): Boolean;
var l : int32;
begin
  aResult := nil;
  l := BinSearch(FArray,UTF8String(aKey),FIndex-1);
  Result := l>-1;
  if result then
    aResult := FArray[l].Value;
end;

procedure TKeysValues_UTF8StringToObject.unlock;
begin
  FThreadProtector.Leave;
end;

{$IFNDEF USE_GENERIC}
{ TObjectDictionary_StringStream }

procedure TObjectDictionary_StringStream.Add(aString: String; aStream: TStream);
begin
  Assert(assigned(aStream));
  Inherited Add(aString, aStream);
end;

function TObjectDictionary_StringStream.TryGetValue(aKey: String;
  var aResult: TStream): Boolean;
begin
  Result := Inherited TryGetValue(aKey,TObject(aResult));
end;

{$ENDIF}

end.
