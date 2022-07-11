///-------------------------------------------------------------------------------
/// Title      : GS.Reference
/// Short Desc : Base classe for GS.LocalMemObject component.
/// Source     : https://github.com/VincentGsell
/// Aim        : Store in a file a Key/Value data. Design to be easy to use.
///-------------------------------------------------------------------------------
unit GS.Reference;
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  Generics.Collections,
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
{$ENDIF}
  GS.Stream,
  GS.Reference.Persister,
  GS.Reference.Persister.SingleFile;

Type

///
///  Reference : End user object.
///


TofReference = class
private
protected
  FPersister: TofReferencePersister;
  FWorkingStream : TMemoryStream;

  Procedure InternalWriteGenericData( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Virtual;
  Procedure InternalReadGenericDataPrepare( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Virtual;

  Procedure InternalDeleteGenericeData(Key : String); Virtual;

  Procedure InternalReadInfoDataByIndex(aIndex : UInt64; var aDataType : TofcontentType; var aKey : String); Virtual;
  Procedure InternalReadInfoDataByKey(aKey : string; var aDataType : TofcontentType; var Index : UInt64); Virtual;

  function GetEntryCount: Uint64; virtual;
  function GetPersisterClassName: String; Virtual;
  function GetPersisterDescription: string; Virtual;
  function GetPersisterVersion: String; Virtual;
public
  Constructor Create(aPersister : TofReferencePersister); Reintroduce; Virtual;
  Destructor Destroy; OVerride;

  Procedure WriteEntryString(Key : String; AString : String);
  Function GetEntryAsString(Key : String) : String;

  Procedure WriteEntryInteger(Key : String; Ainteger : Integer);
  Function GetEntryAsInteger(Key : String) : Integer;

  Procedure WriteEntryUInt32(Key : String; AUint32 : UInt32);
  Function GetEntryAsUInt32(Key : String) : UINT32;

  Procedure WriteEntryDouble(Key : String; ADouble : Double);
  Function GetEntryAsDouble(Key : String) : Double;

  //Use for user convenience only : Avoid TByteStream, which have a behaviour tricky between Size and capacity (under delphi).
  Procedure WriteEntryBytes(Key : String; ABytes : TBytes);
  Function GetEntryAsBytes(Key : String) : TBytes;

  //Use this preferably.
  Procedure WriteEntryStream(key : String; aStream : TMemoryStream);
  Function GetEntryAsStream(key : String) : TMemoryStream;

  function GetDataTypeByIndex(Index: UInt64): tofContentType;
  function GetDataType(aKey: String): tofcontentType;
  function GetKey(Index: UInt64): String;
  function GetIndex(Key : String) : Uint64;
  Function IsKeyExists(aKey : String) : Boolean;

  Procedure DeleteEntry(key : String);

  Procedure Open;
  Procedure Close;

  Property KeyByIndex[Index : UInt64] : String read GetKey;
  Property DataTypeByIndex[Index : UInt64] : tofContentType read GetDataTypeByIndex;
  property DataType[aKey : String] : tofcontentType read GetDataType;

  Property EntryCount : Uint64 read GetEntryCount;

  Property PersisterClassName : String read GetPersisterClassName;
  Property PersisterVersion : String read GetPersisterVersion;
  Property PersisterDescription : string read GetPersisterDescription;
end;

TofReferenceClass = class of TofReference;


//A TofReference with a "Single file" persister.
TofReferenceSingleFile = class(TofReference)
  private
    function GetInMemory: Boolean;
    function GetFileName: String;
    procedure SetFileName(const Value: String);
    function GetTypeConversionAllowedWriteTime: Boolean;
    procedure SetTypeConversionAllowedWriteTime(const Value: Boolean);
    function GetOnInitialLoading: TOnReferenceInitialLoad;
    procedure SetOnInitialLoading(const Value: TOnReferenceInitialLoad);
public
  Constructor Create; Reintroduce; Virtual;

  Property FileName : String read GetFileName write SetFileName;
  Property InMemory : Boolean read GetInMemory;
  Property TypeConversionAllowedWriteTime : Boolean read GetTypeConversionAllowedWriteTime Write SetTypeConversionAllowedWriteTime;

  Property OnInitialLoading : TOnReferenceInitialLoad read GetOnInitialLoading Write SetOnInitialLoading;
end;


TofReferenceTest = class(TObject)
private
  Procedure DisplayInMemoryState(ADB : TofReferencePersisterSingleFile);
public
  function TestWriteSimple(var FailureReason : string) : Boolean;
  function TestWriteStressSimple(ACount : Int64; var FailureReason : string) : Boolean;
  function TestreadForAllocationReconstitution(var FailureReason : string) : Boolean;
  function TestreadStressFileresult(var FailureReason : String) : Boolean;
  function TestreadAndRewrite(var FailureReason : String) : Boolean;
  function TestreadAndmassRewrite(var FailureReason : String) : Boolean;
end;


Implementation

{ TofReferenceTest }

procedure TofReferenceTest.DisplayInMemoryState(ADB : TofReferencePersisterSingleFile);
const lcspace = ' ';
var le : TofAllocationTableStruc;
    i : integer;
begin
  Writeln(EmptyStr);
  Writeln('Memory table dump');
  Writeln('Offset ,Previous, Next, BufferOffset, Key');
  for i := 0 to ADB.Allocation.Count-1 do
  begin
    le := TofAllocationTableStruc(ADB.Allocation.ByIndex[i]);
    Writeln( IntToStr(le.Offset)+lcspace+
             IntToStr(le.Previous)+lcspace+
             IntToStr(le.Next)+lcspace+
             IntToStr(le.BufferOffset)+lcspace+
             le.Key);
  end;
  Writeln(EmptyStr);
end;

function TofReferenceTest.TestreadAndmassRewrite(
  var FailureReason: String): Boolean;
var a : TofReference;
    p : TofReferencePersisterSingleFile;
    i : integer;
begin
  Writeln(EmptyStr);
  Writeln('Read and rewrite from file "test.data"');

  try
    try
      p := TofReferencePersisterSingleFile.Create('Test.data');
      a := TofReference.Create(p);
      a.Open;
      Writeln('Mass rewrite...');
      DisplayInMemoryState(p);
      Writeln(EmptyStr);
      Writeln('Write 10000 time in Double field...');
      for I := 1 to 10000 do
      begin
        a.WriteEntryDouble('6',1/i);
      end;

      Writeln('Write 10000 time in string field...');
      for I := 1 to 10000 do
      begin
        //This string is smaller than room in first time, and quiclky be bigger, so realloc.
        a.WriteEntryString('2', IntToStr(i) +' '+
                                IntToStr(i) +' '+
                                IntToStr(i) +' '+
                                IntToStr(i) +' '+
                                IntToStr(i) +' '+
                                IntToStr(i) +' '+
                                IntToStr(i));
      end;
      Writeln(a.GetEntryAsString('1'));
      Writeln(a.GetEntryAsString('2'));
      Writeln(a.GetEntryAsString('3'));
      Writeln(IntToStr(a.GetEntryAsInteger('4')));
      Writeln(IntToStr(a.GetEntryAsUInt32('5')));
      Writeln(FloatToStr(a.GetEntryAsDouble('6')));
      result := true;
    readln;
    Except
      On E : Exception do
      begin
        FailureReason := E.Message;
        result := False;
      end;
    end;
  finally
    FreeAndNil(a);
  end;
end;

function TofReferenceTest.TestreadAndRewrite(
  var FailureReason: String): Boolean;
var a : TofReference;
    p : TofReferencePersisterSingleFile;
begin
  Writeln(EmptyStr);
  Writeln('Read and rewrite from file "test.data"');

  try
    try
      p := TofReferencePersisterSingleFile.Create('Test.data');
      a := TofReference.Create(p);
      a.Open;
      DisplayInMemoryState(p);
      Writeln(EmptyStr);
      Writeln('Element get from reconstitued inMemory allocation table : ');
      Writeln(a.GetEntryAsString('1'));
      Writeln(a.GetEntryAsString('2'));
      Writeln(a.GetEntryAsString('3'));
      Writeln('Change element 2 (no realoc) : ');
      a.WriteEntryString('2','Test');
      Writeln('Element value is now : '+a.GetEntryAsString('2'));
      Writeln('Change element 1 (with realoc) : ');
      a.WriteEntryString('1','It''s a wonderfull, wonderfull world.');
      Writeln('Element value is now : '+a.GetEntryAsString('1'));
      Result := True;
    Except
      On E : Exception do
      begin
        FailureReason := E.Message;
        result := False;
      end;
    end;
  finally
    FreeAndNil(a);
  end;
end;

function TofReferenceTest.TestreadForAllocationReconstitution(
  var FailureReason: string): Boolean;
var a : TofReference;
    p : TofReferencePersisterSingleFile;
begin
  Writeln(EmptyStr);
  Writeln('Reconstitution from file "test.data"');

  try
    try
      p := TofReferencePersisterSingleFile.Create('Test.data');
      a := TofReference.Create(p);
      a.Open;
      DisplayInMemoryState(p);
      Writeln(EmptyStr);
      Writeln('Element get from reconstitued inMemory allocation table : ');
      Writeln(a.GetEntryAsString('1'));
      Writeln(a.GetEntryAsString('2'));
      Writeln(a.GetEntryAsString('3'));
      Writeln(IntToStr(a.GetEntryAsInteger('4')));
      Writeln(IntToStr(a.GetEntryAsUInt32('5')));
      Writeln(FloatToStr(a.GetEntryAsDouble('6')));
      Result := True;
    Except
      On E : Exception do
      begin
        FailureReason := E.Message;
        result := False;
      end;
    end;
  finally
    FreeAndNil(a);
  end;
end;

function TofReferenceTest.TestreadStressFileresult(
  var FailureReason: String): Boolean;
var a : TofReference;
    p : TofReferencePersisterSingleFile;
begin
  Writeln(EmptyStr);
  Writeln('Reconstitution from file "TestHuge.data"');

  try
    try
      p := TofReferencePersisterSingleFile.Create('TestHuge.data');
      a := TofReference.Create(p);
      a.Open;
      //DisplayInMemoryState(a);
      Writeln(EmptyStr);
      Writeln('Element get from reconstitued inMemory allocation table : ');
      Writeln(a.GetEntryAsString('1'));
      Writeln(a.GetEntryAsString('2'));
      Writeln(a.GetEntryAsString('3'));
      Writeln(a.GetEntryAsString('2000'));
      Result := a.GetEntryAsString('10000') = 'Hello guy "'+IntToStr(10000)+'" !';
    Except
      On E : Exception do
      begin
        FailureReason := E.Message;
        result := False;
      end;
    end;
  finally
    FreeAndNil(a);
  end;
end;

function TofReferenceTest.TestWriteSimple(var FailureReason: string): Boolean;
var a : TofReference;
    p : TofReferencePersisterSingleFile;
begin
  try
    try
      DeleteFile('Test.data');
      p := TofReferencePersisterSingleFile.Create('Test.data');
      a := TofReference.Create(p);
      a.Open;

      a.WriteEntryString('1','Hello guy !');
      a.WriteEntryString('2','Hello world !');
      a.WriteEntryString('3','All of that is the secret !');
      a.WriteEntryInteger('4',25);
      a.WriteEntryUInt32('5',1975);
      a.WriteEntryDouble('6',3.14);

      Writeln(EmptyStr);
      Writeln('Element get from inMemory allocation table : ');
      Writeln(a.GetEntryAsString('1'));
      Writeln(a.GetEntryAsString('2'));
      Writeln(a.GetEntryAsString('3'));
      Writeln(IntToStr(a.GetEntryAsInteger('4')));
      Writeln(IntToStr(a.GetEntryAsUInt32('5')));
      Writeln(FloatToStr(a.GetEntryAsDouble('6')));

      Result := True;

      DisplayInMemoryState(p);
    Except
      On E : Exception do
      begin
        FailureReason := E.Message;
        result := False;
      end;
    end;
  finally
    FreeAndNil(a);
  end;
end;

function TofReferenceTest.TestWriteStressSimple(ACount : Int64; var FailureReason: string): Boolean;
var a : TofReference;
    p : TofReferencePersisterSingleFile;
    i : integer;
begin
  try
    try
      DeleteFile('TestHuge.data');
      p := TofReferencePersisterSingleFile.Create('TestHuge.data');
      a := TofReference.Create(p);
      a.Open;
      for i := 0 to ACount-1 do
      begin
        a.WriteEntryString(IntToStr(i),'Hello guy "'+IntToStr(i)+'" !');
        if i mod 1000 = 0 then
        begin
          writeln(IntToStr(i)+'  '+IntToStr((i+1)*100 div ACount)+'%');
        end;
      end;
      writeln(IntToStr(i)+'  100%');
      Result := True;
    Except
      On E : Exception do
      begin
        FailureReason := E.Message;
        result := False;
      end;
    end;
  finally
    FreeAndNil(a);
  end;
end;



{ TofReference }

procedure TofReference.Close;
begin
  FPersister.Close;
end;

constructor TofReference.Create(aPersister: TofReferencePersister);
begin
  Assert(Assigned(aPersister));
  Inherited Create;
  FPersister := aPersister;
  FWorkingStream := TMemoryStream.Create;
end;

procedure TofReference.DeleteEntry(key: String);
begin
  InternalDeleteGenericeData(key);
end;

destructor TofReference.Destroy;
begin
  FreeAndNil(FPersister);
  FreeAndNil(FWorkingStream);
  inherited;
end;

function TofReference.GetDataType(aKey: String): tofcontentType;
var Dummy : Uint64;
begin
  InternalReadInfoDataByKey(aKey,Result,dummy);
end;

function TofReference.GetDataTypeByIndex(Index: UInt64): tofContentType;
var dummy : String;
begin
  InternalReadInfoDataByIndex(Index,Result,Dummy);
end;

function TofReference.GetEntryAsBytes(Key: String): TBytes;
//We cannot use TByteStream directly, because it will put at a wrong size.
var BB : TMemoryStream;
begin
  FWorkingStream.Clear;
  BB := TMEmoryStream.Create;
  try
    InternalReadGenericDataPrepare(Key,ctStream,FWorkingStream);
    ReadStream(FWorkingStream,TStream(BB));
    SetLength(Result,BB.Size);
    BB.Position  := 0;
    BB.Read(Result,BB.Size);
  finally
    FreeAndNil(BB);
  end;
end;

function TofReference.GetEntryAsDouble(Key: String): Double;
begin
  FWorkingStream.Clear;
  InternalReadGenericDataPrepare(Key,ctDouble,FWorkingStream);
  Result := ReadDouble(FWorkingStream)
end;

function TofReference.GetEntryAsInteger(Key: String): Integer;
begin
  FWorkingStream.Clear;
  InternalReadGenericDataPrepare(Key,ctInteger,FWorkingStream);
  Result := ReadInteger(FWorkingStream)
end;

function TofReference.GetEntryAsStream(key: String): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  FWorkingStream.Clear;
  InternalReadGenericDataPrepare(Key,ctStream,FWorkingStream);
  ReadStream(FWorkingStream,TStream(Result));
end;

function TofReference.GetEntryAsString(Key: String): String;
begin
  FWorkingStream.Clear;
  InternalReadGenericDataPrepare(Key,ctString,FWorkingStream);
  Result := ReadString(FWorkingStream);
end;

function TofReference.GetEntryAsUInt32(Key: String): UINT32;
begin
  FWorkingStream.Clear;
  InternalReadGenericDataPrepare(Key,ctUINT32,FWorkingStream);
  Result := ReadUINT32(FWorkingStream)
end;

function TofReference.GetEntryCount: Uint64;
begin
  Result := FPersister.EntryCount;
end;

function TofReference.GetIndex(Key: String): Uint64;
var dummy : tofContentType;
begin
  InternalReadInfoDataByKey(Key,dummy, Result);
end;

function TofReference.GetKey(Index: UInt64): String;
var dummy : tofContentType;
begin
  InternalReadInfoDataByIndex(Index,dummy, Result);
end;

function TofReference.GetPersisterClassName: String;
begin
  result := FPersister.ClassName;
end;

function TofReference.GetPersisterDescription: string;
begin
  result := FPersister.Description;
end;

function TofReference.GetPersisterVersion: String;
begin
  result := FPersister.Version;
end;

procedure TofReference.InternalDeleteGenericeData(Key: String);
begin
  FPersister.DeleteGenericData(Key);
end;

procedure TofReference.InternalReadGenericDataPrepare(Key: String;
  DataType: tofContentType; StreamData: TMemoryStream);
begin
  FPersister.ReadGenericDataPrepare(Key,DataType,StreamData);
end;

procedure TofReference.InternalReadInfoDataByIndex(aIndex: UInt64;
  var aDataType: TofcontentType; var aKey: String);
begin
  FPersister.ReadInfoDataByIndex(aIndex,aDataType,aKey);
end;

procedure TofReference.InternalReadInfoDataByKey(aKey: string;
  var aDataType: TofcontentType; var Index: UInt64);
begin
  FPersister.ReadInfoDataByKey(aKey,aDataType,Index);
end;

procedure TofReference.InternalWriteGenericData(Key: String;
  DataType: tofContentType; StreamData: TMemoryStream);
begin
  FPersister.WriteGenericData(Key,DataType,StreamData);
end;

function TofReference.IsKeyExists(aKey: String): Boolean;
begin
  Result := FPersister.IsKeyExists(aKey);
end;

procedure TofReference.Open;
begin
  FPersister.Open;
end;

procedure TofReference.WriteEntryBytes(Key: String; ABytes: TBytes);
var B : TBytesStream;
begin
  FWorkingStream.Clear;
  B := TBytesStream.Create(ABytes);
  B.Position := 0;
  try
    WriteStream(FWorkingStream,B);
    InternalWriteGenericData(Key,ctStream,FWorkingStream);
  finally
    FreeAndNil(B);
  end;
end;

procedure TofReference.WriteEntryDouble(Key: String; ADouble: Double);
begin
  FWorkingStream.Clear;
  WriteDouble(FWorkingStream,ADouble);
  InternalWriteGenericData(Key,ctDouble,FWorkingStream);
end;

procedure TofReference.WriteEntryInteger(Key: String; Ainteger: Integer);
begin
  FWorkingStream.Clear;
  WriteInteger(FWorkingStream,Ainteger);
  InternalWriteGenericData(Key,ctInteger,FWorkingStream);
end;

procedure TofReference.WriteEntryStream(key: String; aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  aStream.Position := 0;
  InternalWriteGenericData(Key,ctStream,aStream);
end;

procedure TofReference.WriteEntryString(Key, AString: String);
begin
  FWorkingStream.Clear;
  WriteString(FWorkingStream,AString);
  InternalWriteGenericData(Key,ctString,FWorkingStream);
end;

procedure TofReference.WriteEntryUInt32(Key: String; AUint32: UInt32);
begin
  FWorkingStream.Clear;
  WriteUInt32(FWorkingStream,AUint32);
  InternalWriteGenericData(Key,ctUINT32,FWorkingStream);
end;


{ TofReferenceSingleFile }

constructor TofReferenceSingleFile.Create;
begin
  Inherited Create(TofReferencePersisterSingleFile.Create);
end;

function TofReferenceSingleFile.GetFileName: String;
begin
  result := TofReferencePersisterSingleFile(FPersister).FileName;
end;

function TofReferenceSingleFile.GetInMemory: Boolean;
begin
  result := TofReferencePersisterSingleFile(FPersister).InMemoryOnly;
end;

function TofReferenceSingleFile.GetOnInitialLoading: TOnReferenceInitialLoad;
begin
  result := TofReferencePersisterSingleFile(FPersister).OnInitialLoading;
end;

function TofReferenceSingleFile.GetTypeConversionAllowedWriteTime: Boolean;
begin
  Result := TofReferencePersisterSingleFile(FPersister).TypeConversionAllowedWriteTime;
end;

procedure TofReferenceSingleFile.SetFileName(const Value: String);
begin
  TofReferencePersisterSingleFile(FPersister).FileName := Value;
end;

procedure TofReferenceSingleFile.SetOnInitialLoading(
  const Value: TOnReferenceInitialLoad);
begin
  TofReferencePersisterSingleFile(FPersister).OnInitialLoading := Value;
end;

procedure TofReferenceSingleFile.SetTypeConversionAllowedWriteTime(
  const Value: Boolean);
begin
  TofReferencePersisterSingleFile(FPersister).TypeConversionAllowedWriteTime := Value;
end;

end.
