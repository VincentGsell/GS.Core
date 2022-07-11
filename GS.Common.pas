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
  function GetItem(Index: Uint32): TObject; virtual;
  procedure SetItem(Index: Uint32; const Value: TObject); virtual;
public
  constructor Create(Const Owned : Boolean = false); Reintroduce; Virtual;
  Destructor Destroy; Override;

  Procedure Clear; Virtual;
  Procedure Delete(Index : Uint32);
  Procedure Remove(aObject : TObject);
  procedure Add(aObject : TObject);

  Function IndexOf(aObject : TObject) : Int32;

  Property Owned : Boolean read FOwned Write FOwned;
  Property Count : Uint32 read GetCount;
  Property Items[Index : Uint32] : TObject read GetItem Write SetItem; default;
end;

//Key Values (basic replacement for Generic Dictionnary key paired String/Object, binSearch support.
TKeysValues_UTF8StringToObject = Class
private
protected
  FThreadProtector : TCriticalSection;
  FArray : TStringItemArray;
  FIndex : UInt32;
  FInitialized : Boolean;
  FSorted: Boolean;
  FOwned : Boolean;

  function GetItem(Index: UInt32): TKeyValueStringItem;
  procedure SetItem(Index: UInt32; const Value: TKeyValueStringItem);
  function GetCount: Uint32;

  procedure InternalClearObject;
public
  Constructor Create(Const Owned : Boolean = false); Reintroduce; Virtual;
  Destructor Destroy; Override;

  Function TryGetValue(aKey : String; var aResult : TObject) : Boolean; virtual;
  Procedure Add(aKey : String; aObject : TObject); virtual;
  Procedure Remove(Index : UInt32); Overload;
  Procedure Remove(aKey : String); Overload;

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

IGSStringList = interface
  procedure add(aString : String);
  function count : uint32;
  procedure clear;
  function lines(index : uint32) : string; overload;
  procedure lines(index  : uint32; value : string); overload;
  function text : string;
  procedure setText(aTxt : string);
  procedure saveToFile(afileName : string);
  procedure loadFromFile(aFileName : string);
  function ValueFromIndex(index : integer) : String;
  function Values(key : string) : String;
  function Names(index : integer) : String;
  function indexOf(astring : string) : Integer;
  procedure insert(index : integer; value : String);
  function ToStringArray : TArray<String>;
  procedure SetDelimiter(aDelimiter : char);
  procedure SetDelimitedText(aText : String);
  function DelimitedText : string;
  procedure setTrailinglineBreak(value : Boolean);
  procedure setEncoding(value : TEncoding); //Convert. Fix it on start. Default : UTF8.
end;

IGSThreadSafeList = interface
  procedure lock;
  procedure unlock;
end;

TGSStringList = class(TInterfacedObject,IGSStringList)
private
  FST : TStringList;
public
  //IGSStringList
  procedure add(aString : String);
  function count : uint32;
  procedure clear;
  function lines(index : uint32) : string; overload;
  procedure lines(index : uint32; value : string); overload;
  function text : string;
  procedure setText(aTxt : string);
  procedure saveToFile(afileName : string);
  procedure loadFromFile(aFileName : string);
  function ValueFromIndex(index : integer) : String;
  function Values(key : string) : String;
  function Names(index : integer) : String;
  function indexOf(astring : string) : Integer;
  procedure insert(index : integer; value : String);
  function ToStringArray : TArray<String>;
  procedure SetDelimiter(aDelimiter : char);
  procedure SetDelimitedText(aText : String);
  function DelimitedText : string;
  procedure setTrailinglineBreak(value : Boolean); //True by default in EMB StringList.
  procedure setEncoding(value : TEncoding);

  Constructor Create; virtual;
  Destructor Destroy; override;
End;

TGSStringListWithThreadAutoLock = class(TInterfacedObject,IGSStringList, IGSThreadSafeList)
private
  FCS : TCriticalSection;
  FST : TStringList;

  //IGSThreadSafeList
  procedure lock;
  procedure unlock;
Public
  //IGSStringList
  procedure add(aString : String);
  function count : uint32;
  procedure clear;
  function lines(index : uint32) : string; overload;
  procedure lines(index : uint32; value : string); overload;
  function text : string;
  procedure setText(aTxt : string);
  procedure saveToFile(afileName : string);
  procedure loadFromFile(aFileName : string);
  function Values(key : string) : String;
  function ValueFromIndex(index : integer) : String;
  function Names(index : integer) : String;
  function indexOf(astring : string) : Integer;
  procedure insert(index : integer; value : String);
  function ToStringArray : TArray<String>;
  procedure SetDelimiter(aDelimiter : char);
  procedure SetDelimitedText(aText : String);
  function DelimitedText : string;
  procedure setTrailinglineBreak(value : Boolean);
  procedure setEncoding(value : TEncoding);


  Constructor Create; virtual;
  Destructor Destroy; override;
end;

TGSFactory = Class
  Class function GetStringList_ThreadSafeAuto : IGSStringList;
  class function GetStringList : IGSStringList;
End;


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
var b : TMemoryStream;
    Key,r : TBytes;

    function GetOTPfile : TBytes;
    var c : TMemoryStream;
    begin
      if FileExists(GLB_OTP_FILE) then
      begin
        c := TMemoryStream.Create;
        try
          c.LoadFromFile(GLB_OTP_FILE);
          SetLEngth(result,c.Size);
          c.Read(result,c.Size);
        finally
          FreeAndNil(c);
        end;
      end
      else
      begin
        Result := BytesOf(CST_DEFAULT_CYPH_KEY); //-/
      end;
    end;

begin
  b := TMemoryStream.Create;
  try
    b.LoadFromStream(aStreamToEncrypt);
    key := GetOTPFile;
    SetLength(r,b.Size);
    b.Read(r,b.Size);
    r := sencrypt_XorCipher(Key,r);
    b.Clear;
    aStreamToEncrypt.Clear;
    aStreamToEncrypt.Write(r,length(r));
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

{ TList_ObjectArray }

procedure TList_ObjectArray.Add(aObject: TObject);
begin
  ManagedAdd(aObject);
end;

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

function TList_ObjectArray.GetItem(Index: Uint32): TObject;
begin
  assert(Index<Count);
  result := FArray[Index];
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

procedure TList_ObjectArray.SetItem(Index: Uint32; const Value: TObject);
begin
  ManagedSet(Index,Value);
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
  assert(Index<Count);
  result := FArray[Index];
end;

function TList_UTF8String.GetUTF8StringCount: Uint32;
begin
  result := FIndex;
end;

procedure TList_UTF8String.SetUTF8String(Index: Uint32;
  const Value: UTF8String);
begin
  assert(Index<Count);
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
  InternalClearObject;
  FArray := nil;
  SetLength(FArray,CST_ARRAY_INIT_QTE);
  FIndex := 0;
  FInitialized := False;
end;

constructor TKeysValues_UTF8StringToObject.Create(Const Owned : Boolean = false);
begin
  Inherited Create;
  FOwned := Owned;
  FThreadProtector := TCriticalSection.Create;
  Clear;
end;

destructor TKeysValues_UTF8StringToObject.Destroy;
begin
  InternalClearObject;
  FThreadProtector.Free;
  inherited;
end;

function TKeysValues_UTF8StringToObject.GetCount: Uint32;
begin
  result := FIndex;
end;

function TKeysValues_UTF8StringToObject.GetItem(
  Index: UInt32): TKeyValueStringItem;
begin
  assert(Index<Count);
  result := FArray[Index];
end;

procedure TKeysValues_UTF8StringToObject.InternalClearObject;
var i : integer;
begin
  if FOwned then
    if FIndex>0 then
      for i := FIndex-1 downto 0 do
        FArray[i].Value.Free;
end;

procedure TKeysValues_UTF8StringToObject.lock;
begin
  FThreadProtector.Enter;
end;

procedure TKeysValues_UTF8StringToObject.Remove(aKey: String);
var l : int32;
begin
  l := BinSearch(FArray,UTF8String(aKey),FIndex);
  if l>-1 then
    Remove(l);
end;

procedure TKeysValues_UTF8StringToObject.Remove(Index: UInt32);
var llength : UInt32;
    i : integer;
begin
  llength := Length(FArray);
  if llength = 0 then
    exit;

  if Index<llength then
  begin
    if FOwned then
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
  assert(Index<Count);
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

{ TGSStringListWithThreadAutoLock }

procedure TGSStringListWithThreadAutoLock.add(aString: String);
begin
  lock;
  try
    FST.Add(aString);
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.clear;
begin
  lock;
  try
    FST.Clear;
  finally
    unlock;
  end;
end;

function TGSStringListWithThreadAutoLock.count: uint32;
begin
  lock;
  try
    result := FST.Count;
  finally
    unlock;
  end;
end;

constructor TGSStringListWithThreadAutoLock.Create;
begin
  Inherited Create;
  FCS := TCriticalSection.Create;
  FST := TStringList.Create;
  setEncoding(TEncoding.UTF8);
end;

function TGSStringListWithThreadAutoLock.DelimitedText: string;
begin
  lock;
  try
    result := FST.DelimitedText;
  finally
    unlock;
  end;
end;

destructor TGSStringListWithThreadAutoLock.Destroy;
begin
  FreeAndNil(FCS);
  FreeAndNil(FST);
  inherited;
end;

function TGSStringListWithThreadAutoLock.indexOf(astring: string): Integer;
begin
  lock;
  try
    result := FST.IndexOf(astring);
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.insert(index: integer; value: String);
begin
  lock;
  try
    FST.Insert(index,value);
  finally
    unlock;
  end;
end;

function TGSStringListWithThreadAutoLock.lines(index: uint32): string;
begin
  lock;
  try
    result := FST[index];
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.lines(index: uint32; value: string);
begin
  lock;
  try
    FST[index] := value;
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.loadFromFile(aFileName: string);
begin
  lock;
  try
    FST.LoadFromFile(aFileName);
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.lock;
begin
  FCS.Acquire;
end;

function TGSStringListWithThreadAutoLock.Names(index: integer): String;
begin
  lock;
  try
    result := FST.Names[index];
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.saveToFile(afileName: string);
begin
  lock;
  try
    FST.SaveToFile(aFileName);
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.SetDelimitedText(aText: String);
begin
  lock;
  try
    FST.DelimitedText := aText;
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.SetDelimiter(aDelimiter: char);
begin
  lock;
  try
    FST.Delimiter := aDelimiter;
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.setEncoding(value: TEncoding);
begin
  lock;
  try
    FST.DefaultEncoding := value;
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.setText(aTxt: string);
begin
  lock;
  try
    FST.Text := aTxt;
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.setTrailinglineBreak(value: Boolean);
begin
  lock;
  try
    FST.TrailingLineBreak := value;
  finally
    unlock;
  end;
end;

function TGSStringListWithThreadAutoLock.text: string;
begin
  lock;
  try
    result := FST.Text;
  finally
    unlock;
  end;
end;

function TGSStringListWithThreadAutoLock.ToStringArray: TArray<String>;
var
  i: Integer;
begin
  lock;
  try
    begin
      SetLength(Result,FST.Count);
      for i := 0 to FST.Count-1 do
        result[i] := FST[i];
    end;
  finally
    unlock;
  end;
end;

procedure TGSStringListWithThreadAutoLock.unlock;
begin
  FCS.Leave;
end;

function TGSStringListWithThreadAutoLock.ValueFromIndex(index: integer): String;
begin
  lock;
  try
    result := FST.ValueFromIndex[index];
  finally
    unlock;
  end;
end;

function TGSStringListWithThreadAutoLock.Values(key: string): String;
begin
  lock;
  try
    result := FST.Values[key];
  finally
    unlock;
  end;
end;

{ TGSFactory }

class function TGSFactory.GetStringList: IGSStringList;
begin
  result := TGSStringList.Create;
end;

class function TGSFactory.GetStringList_ThreadSafeAuto: IGSStringList;
begin
  result := TGSStringListWithThreadAutoLock.Create;
end;

{ TGSStringList }

procedure TGSStringList.add(aString: String);
begin
  FST.Add(aString);
end;

procedure TGSStringList.clear;
begin
  FST.Clear;
end;

function TGSStringList.count: uint32;
begin
  result := FST.Count
end;

constructor TGSStringList.Create;
begin
  inherited;
  FST := TStringList.Create;
  setEncoding(TEncoding.UTF8);
end;

function TGSStringList.DelimitedText: string;
begin
  result := FST.DelimitedText;
end;

destructor TGSStringList.Destroy;
begin
  FreeAndNil(FST);
  inherited;
end;

function TGSStringList.indexOf(astring: string): Integer;
begin
  result := FST.IndexOf(astring);
end;

procedure TGSStringList.insert(index : integer; value: String);
begin
  FST.Insert(index,value);
end;

procedure TGSStringList.lines(index: uint32; value: string);
begin
  FST[Index] := value;
end;

procedure TGSStringList.loadFromFile(aFileName: string);
begin
  FST.LoadFromFile(aFileName);
end;

function TGSStringList.Names(index: integer): String;
begin
  result := FST.Names[index];
end;

procedure TGSStringList.saveToFile(afileName: string);
begin
  FST.SaveToFile(afileName);
end;

procedure TGSStringList.SetDelimitedText(aText: String);
begin
  FST.DelimitedText := aText;
end;

procedure TGSStringList.SetDelimiter(aDelimiter: char);
begin
  FSt.Delimiter := aDelimiter;
end;

procedure TGSStringList.setEncoding(value: TEncoding);
begin
  FST.DefaultEncoding := Value;
end;

procedure TGSStringList.setText(aTxt: string);
begin
  FST.Text := aTxt;
end;

procedure TGSStringList.setTrailinglineBreak(value: Boolean);
begin
  FST.TrailingLineBreak := value;
end;

function TGSStringList.lines(index: uint32): string;
begin
  result := FST[Index];
end;

function TGSStringList.text: string;
begin
  result := FST.Text;
end;

function TGSStringList.ToStringArray: TArray<String>;
var
  i: Integer;
begin
  SetLength(Result,count);
  for i := 0 to count-1 do
    result[i] := lines(i);
end;

function TGSStringList.Values(Key: string): String;
begin
  result := FST.Values[Key];
end;

function TGSStringList.ValueFromIndex(index: integer): String;
begin
  result := FST.ValueFromIndex[index];
end;

end.

