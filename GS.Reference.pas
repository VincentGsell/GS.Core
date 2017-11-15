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
  GS.BOList.Rec;

Const CST_TEST_NORMAL_ITEM_COUNT      = 100000; //one hundred thousand.
      CST_TEST_BIG_ITEM_COUNT         = 1000000; //one million.
      CST_TEST_HUGE_ITEM_COUNT        = 10000000; //10 million.. Never tested.
      CST_CLUSTER_MEMORY              = 2;

Type
tofContentType = ( ctUnknow,
                   ctString,
                   ctStream,
                   ctInteger,
                   ctUINT32,
                   ctDouble);

Const
  Cst_ContentTypeStr : array[0..5] of string = ('Unknow',
                        'String',
                        'Stream',
                        'Integer',
                        'Uint32',
                        'Double');
Type

//Build On startup.
TofAllocationTableStruc = Record
  Offset        : int64;              //Offset of the allocation table.
  Previous      : Int64;              //Previous table entry. -1 if current is the first.
  Next          : Int64;              //Next table entry. -1 if current is the last.
  BufferOffset  : Int64;              //localization of real data (Value).
  Key           : String;             //Key of the pair Key/Value.
  BufferContentType : tofContentType; //Type of data value.
  BufferContentSize : Int64;          //Reals size, in bytes, of the data value.
  BufferRoom : Int64;                 //Room available in buffer : for example, for dynamic data, the room allocation will be *2 or more than the real size, in order to prevent too many realocation.
end;


TOnReferenceInitialLoad = Procedure(Sender : TObject; PercentProgress : Double) of Object;
TofReference = class(TObject)
private
  FFileName : string;
  FFileBuffer : TStream;
  FInMemoryAllocation : TofBusinessObjectListRec<String,TofAllocationTableStruc>;
  FTypeConversionAllowed: Boolean;
  FOnInitialLoading : TOnReferenceInitialLoad;
  FInMemoryOnly: Boolean;

  Procedure InternalStartUp;
  Procedure InternalInitialLoading(aProgress : Double);
  Procedure InternalCreateFileBuffer;

  Procedure InternalWriteGenericData( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream);
  Procedure InternalReadGenericDataPrepare( Key : String;
                                      DataType : tofContentType);

public
  Constructor Create(Const FileName: String = ''; Const OnInitLoadEvent : TOnReferenceInitialLoad = Nil); reintroduce; Virtual;
  destructor Destroy; Override;


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


  Property Allocation : TofBusinessObjectListRec<String,TofAllocationTableStruc> read FInMemoryAllocation;
  Property TypeConversionAllowedWriteTime : Boolean read FTypeConversionAllowed Write FTypeConversionAllowed;
  Property InMemoryOnly : Boolean read FInMemoryOnly;

  Property OnInitialLoading : TOnReferenceInitialLoad read FOnInitialLoading Write FOnInitialLoading;
end;

TofReferenceTest = class(TObject)
private
  Procedure DisplayInMemoryState(ADB : TofReference);
public
  function TestWriteSimple(var FailureReason : string) : Boolean;
  function TestWriteStressSimple(ACount : Int64; var FailureReason : string) : Boolean;
  function TestreadForAllocationReconstitution(var FailureReason : string) : Boolean;
  function TestreadStressFileresult(var FailureReason : String) : Boolean;
  function TestreadAndRewrite(var FailureReason : String) : Boolean;
  function TestreadAndmassRewrite(var FailureReason : String) : Boolean;
end;

Procedure WriteTofAllocation(TargetStream : TStream; var aAlloc : TofAllocationTableStruc);
Procedure ReadTofAllocation(TargetStream : TStream; Var aAlloc : TofAllocationTableStruc);

implementation

Procedure WriteTofAllocation(TargetStream : TStream; Var aAlloc : TofAllocationTableStruc);
begin
  WriteInt64(TargetStream,aAlloc.Offset);
  WriteInt64(TargetStream,aAlloc.Previous);
  WriteInt64(TargetStream,aAlloc.Next);
  WriteInt64(TargetStream,aAlloc.BufferOffset);
  WriteString(TargetStream,aAlloc.Key);
  WriteByte(TargetStream,Byte(aAlloc.BufferContentType));
  WriteInt64(TargetStream,aAlloc.BufferContentSize);
  WriteInt64(TargetStream,aAlloc.BufferRoom);
end;

Procedure ReadTofAllocation(TargetStream : TStream; var  aAlloc : TofAllocationTableStruc);
begin
  aAlloc.Offset := ReadInt64(TargetStream);
  aAlloc.Previous := ReadInt64(TargetStream);
  aAlloc.Next := ReadInt64(TargetStream);
  aAlloc.BufferOffset := ReadInt64(TargetStream);
  aAlloc.Key := ReadString(TargetStream);
  aAlloc.BufferContentType := tofContentType(ReadByte(TargetStream));
  aAlloc.BufferContentSize := ReadInt64(TargetStream);
  aAlloc.BufferRoom  := ReadInt64(TargetStream);
end;
{ TofReference }

constructor TofReference.Create(Const FileName: String; Const OnInitLoadEvent : TOnReferenceInitialLoad);
begin
  inherited Create;
  FFileName := FileName;
  FTypeConversionAllowed := True; //Allow to redefine type.
  FInMemoryAllocation := TofBusinessObjectListRec<String,TofAllocationTableStruc>.Create;
  InternalCreateFileBuffer;
  FOnInitialLoading := OnInitLoadEvent;
  InternalStartUp;
end;

destructor TofReference.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FFileBuffer);
  FreeAndNil(FInMemoryAllocation);
end;

function TofReference.GetEntryAsBytes(Key: String): TBytes;
//We cannot use TByteStream directly, because it will put at a wrong size.
var BB : TMemoryStream;
begin
  BB := TMEmoryStream.Create;
  try
    InternalReadGenericDataPrepare(Key,ctStream);
    ReadStream(FFileBuffer,BB);
    SetLength(Result,BB.Size);
    BB.Position  := 0;
    BB.Read(Result,BB.Size);
  finally
    FreeAndNil(BB);
  end;
end;

function TofReference.GetEntryAsDouble(Key: String): Double;
begin
  InternalReadGenericDataPrepare(Key,ctDouble);
  Result := ReadDouble(FFileBuffer)
end;

function TofReference.GetEntryAsInteger(Key: String): Integer;
begin
  InternalReadGenericDataPrepare(Key,ctInteger);
  Result := ReadInteger(FFileBuffer)
end;

function TofReference.GetEntryAsStream(key: String): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  InternalReadGenericDataPrepare(Key,ctStream);
  ReadStream(FFileBuffer,Result);
end;

function TofReference.GetEntryAsString(Key: String): String;
begin
  InternalReadGenericDataPrepare(Key,ctString);
  Result := ReadString(FFileBuffer)
end;

function TofReference.GetEntryAsUInt32(Key: String): UINT32;
begin
  InternalReadGenericDataPrepare(Key,ctUINT32);
  Result := ReadUINT32(FFileBuffer)
end;

procedure TofReference.InternalReadGenericDataPrepare(Key: String;
  DataType: tofContentType);
var lEntry : TofAllocationTableStruc;
begin
  if Not( FInMemoryAllocation.ContainsKey(Key) ) then
  begin
    raise Exception.Create('Key does not exists in DB');
  end;

  lEntry := TofAllocationTableStruc(FInMemoryAllocation[Key]);
  FFileBuffer.Position := lEntry.BufferOffset;

  if lEntry.BufferContentType <> DataType then
  begin
    raise Exception.Create('Key is not a '+IntToStr(Integer(DataType))+' type. Type found : '+IntToStr(Integer(lEntry.BufferContentType)));
  end;
  //the Code after this proc. must read the type :  FFileBuffer is in position.
end;

procedure TofReference.InternalCreateFileBuffer;
begin
  if FFileName = EmptyStr then
  begin
    FInMemoryOnly := True;
    FFileBuffer := TMemoryStream.Create;
  end
  else
  begin
    FInMemoryOnly := False;
    if Not(FileExists(FFileName)) then
    begin
      FFileBuffer := TFileStream.Create(FFileName,fmCreate);
    end
    else
    begin
      FFileBuffer := TFileStream.Create(FFileName,fmOpenReadWrite);
    end;
  end;
end;

procedure TofReference.InternalInitialLoading(aProgress: Double);
begin
  if Assigned(FOnInitialLoading) then
    FOnInitialLoading(Self,aProgress);
end;

procedure TofReference.InternalStartUp;
var lEntry : TofAllocationTableStruc;
    lPercent : Double;
begin
  FInMemoryAllocation.Clear;
  if FFileBuffer.Size>0 then
  begin
    FFileBuffer.Position := 0;

    InternalInitialLoading(0.0);

    while FFileBuffer.Position<FFileBuffer.Size do
    begin
      //lEntry := TofAllocationTableStruc.Create; Not an object this time. :)
      try
        ReadTofAllocation(FFileBuffer,lEntry);
      Except
        On E : Exception do
        begin
          raise Exception.Create(ClassName+'.InternalStartup fail : Error on read element index '+IntToStr(FInMemoryAllocation.Count+1)+' : '+E.Message);
        end;
      end;

      FInMemoryAllocation.Add(lEntry.Key,lEntry);
      lPercent := FFileBuffer.Position*100/FFileBuffer.Size;
      if lEntry.Next>-1 then
      begin
        InternalInitialLoading(lPercent);

        FFileBuffer.Position := lEntry.Next;
      end
      else
      begin
        InternalInitialLoading(100.00);
        Break;
      end;

    end;
  end;

//  FInMemoryAllocation.

end;

procedure TofReference.WriteEntryBytes(Key: String; ABytes: TBytes);
var content : TMemoryStream;
    B : TBytesStream;
begin
  B := TBytesStream.Create(ABytes);
  B.Position := 0;
  Content := TMemoryStream.Create;
  try
    WriteStream(Content,B);
    InternalWriteGenericData(Key,ctStream,content);
  finally
    FreeAndNil(Content);
    FreeAndNil(B);
  end;
end;

procedure TofReference.WriteEntryDouble(Key: String; ADouble: Double);
var content : TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    WriteDouble(Content,ADouble);
    InternalWriteGenericData(Key,ctDouble,content);
  finally
    FreeAndNil(Content);
  end;
end;

procedure TofReference.WriteEntryInteger(Key: String;
  Ainteger: Integer);
var content : TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    WriteInteger(Content,Ainteger);
    InternalWriteGenericData(Key,ctInteger,content);
  finally
    FreeAndNil(Content);
  end;
end;

Procedure TofReference.InternalWriteGenericData(Key : String; DataType : tofContentType; StreamData : TMemoryStream);
var lEntry, lPrev : TofAllocationTableStruc;
    lprevExists : Boolean;
    ls : TMemoryStream;
begin
  Assert(Assigned(StreamData));
  Assert(DataType <> ctUnknow);
  if FInMemoryAllocation.ContainsKey(Key) then
  begin
    //Old record we rewrite or realloc in case of dynamic data (String, stream)
    lEntry := TofAllocationTableStruc(FInMemoryAllocation[Key]);
    FFileBuffer.Position := lEntry.BufferOffset;

    if (lEntry.BufferContentType<>DataType) then
    begin
      If (FTypeConversionAllowed) then
      begin
        lEntry.BufferContentType := DataType;
      end
      else
      begin
        raise Exception.Create('Entry "'+Key+'" is not of "'+IntToStr(Integer(DataType))+'" type. (Actual type '+IntToStr(Integer(lEntry.BufferContentType))+')');
      end;
    end;

    StreamData.Position := 0;

    if StreamData.Size>lEntry.BufferRoom then
    begin
      //No enough room : Data realocation.
      FFileBuffer.Position := FFileBuffer.Size;
      lEntry.BufferOffset := FFileBuffer.Position;
      lEntry.BufferContentSize := StreamData.Size; //Update size.
      case DataType of
        ctUnknow: ;
        ctString, ctStream:
        begin
          lEntry.BufferRoom := StreamData.Size * CST_CLUSTER_MEMORY; //Prevent reserve for futher allocation.
          StreamData.SetSize(lEntry.BufferRoom);
        end;
        ctInteger,
        ctUINT32,
        ctDouble:
        begin
          lEntry.BufferRoom := StreamData.Size;
        end;
      end;

      FFileBuffer.CopyFrom(StreamData,StreamData.Size);
      //Update lEntry
      FFileBuffer.Position := lEntry.Offset;
      WriteTofAllocation(FFileBuffer,lEntry);
    end
    else
    begin
      //Enough room : Just rewrite and update.
      lEntry.BufferContentSize := StreamData.Size; //Update size.
      //Update lEntry
      FFileBuffer.Position := lEntry.Offset;
      WriteTofAllocation(FFileBuffer,lEntry);
      //Update data.
      FFileBuffer.Position := lEntry.BufferOffset;
      FFileBuffer.CopyFrom(StreamData,StreamData.Size);
    end;
    FInMemoryAllocation.ByValue[Key] := lEntry;
    //FInMemoryAllocation.RemoveKey(Key); //No ! Not change list sequence, because of inner bilateral ref. inside struct.
    //FInMemoryAllocation.Add(Key,lEntry);
  end
  else
  begin
    //New one : Got to end.
    FFileBuffer.Position := FFileBuffer.Size;

    //lEntry := TofAllocationTableStruc.Create;
    lEntry.Offset := FFileBuffer.Position;
    lEntry.Previous := 0;
    lEntry.Next := 0;
    lEntry.BufferOffset := 0;
    lEntry.BufferContentSize := 0;
    lEntry.BufferRoom := 0;

    lprevExists := false;

    //Previous link.
    if FInMemoryAllocation.Count>0 then
    begin
      lprevExists := true;
      lPrev := TofAllocationTableStruc(FInMemoryAllocation.ByIndex[FInMemoryAllocation.Count-1]);
      lPrev.Next := lEntry.Offset; //Data to update.
      lEntry.Previous := lPrev.Offset;
    end
    else
    begin
      lEntry.Previous := -1; //It the first.
    end;

    lEntry.Next := -1; //It is the last too.
    lEntry.Key := Key;

    //Buffer offset calculus.
    ls := TMemoryStream.Create;
    try
      WriteTofAllocation(ls,lEntry);
      lEntry.BufferOffset := FFileBuffer.Position + ls.Size;

      //Write buffer info.
      lEntry.BufferContentType := DataType;
      lEntry.BufferContentSize := StreamData.Size;
      lEntry.BufferRoom  := StreamData.Size; //Exact room reserverd (size) for first creation.

      ls.Clear;
      //rewrite allocation a last time : because lEntry is now really complete.
      WriteTofAllocation(ls,lEntry);

      //And juste after the record, you got data (Always). ref and data are STUCK
      //WriteStream(ls,content); //No, add an extra int64 (Size) which is not wanted.
      StreamData.Position := 0;
      ls.CopyFrom(StreamData,StreamData.Size);

      //Finally, write all in Buffer.
      //WriteStream(FFileBuffer,ls);  //No, add an extra int64 (Size) which is not wanted.
      ls.Position := 0;
      FFileBuffer.CopyFrom(ls,ls.Size);

      //rewrite lprev to reflec update data.
      if lprevExists then
      begin
        ls.Clear;
        WriteTofAllocation(ls,lPrev);
        FFileBuffer.Position := lPrev.Offset;
        //WriteStream(FFileBuffer,ls);  //No, add an extra int64 (Size) which is not wanted.
        ls.Position := 0;
        FFileBuffer.CopyFrom(ls,ls.Size);

        //Update lPrev in in-memory alloc (It is a record, not an object !).
        //FInMemoryAllocation[lPrev.Key] := lprev;
      end;

      //Finally, update in in-memory alloc.
      FInMemoryAllocation.Add(lEntry.Key,lEntry);
    finally
      FreeAndNil(ls);
    end;
  end;
end;

procedure TofReference.WriteEntryStream(key: String; aStream: TMemoryStream);
begin
  Assert(Assigned(aStream));
  aStream.Position := 0;
  InternalWriteGenericData(Key,ctStream,aStream);
end;

procedure TofReference.WriteEntryString(Key: String; AString: String);
var content : TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    WriteString(Content,AString);
    InternalWriteGenericData(Key,ctString,content);
  finally
    FreeAndNil(Content);
  end;
end;

procedure TofReference.WriteEntryUInt32(Key: String; AUint32: UInt32);
var content : TMemoryStream;
begin
  Content := TMemoryStream.Create;
  try
    WriteUInt32(Content,AUint32);
    InternalWriteGenericData(Key,ctUINT32,content);
  finally
    FreeAndNil(Content);
  end;
end;

{ TofReferenceTest }

procedure TofReferenceTest.DisplayInMemoryState(ADB : TofReference);
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
    i : integer;
begin
  Writeln(EmptyStr);
  Writeln('Read and rewrite from file "test.data"');

  try
    try
      a := TofReference.Create('Test.data');
      Writeln('Mass rewrite...');
      DisplayInMemoryState(a);
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
      DisplayInMemoryState(a);
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
begin
  Writeln(EmptyStr);
  Writeln('Read and rewrite from file "test.data"');

  try
    try
      a := TofReference.Create('Test.data');
      DisplayInMemoryState(a);
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
begin
  Writeln(EmptyStr);
  Writeln('Reconstitution from file "test.data"');

  try
    try
      a := TofReference.Create('Test.data');
      DisplayInMemoryState(a);
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
begin
  Writeln(EmptyStr);
  Writeln('Reconstitution from file "TestHuge.data"');

  try
    try
      a := TofReference.Create('TestHuge.data');
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
begin
  try
    try
      DeleteFile('Test.data');
      a := TofReference.Create('Test.data');
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

      DisplayInMemoryState(a);
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
    i : integer;
begin
  try
    try
      DeleteFile('TestHuge.data');
      a := TofReference.Create('TestHuge.data');
      for i := 0 to ACount-1 do
      begin
        a.WriteEntryString(IntToStr(i),'Hello guy "'+IntToStr(i)+'" !');
        if i mod 1000 = 0 then
        begin
          writeln(IntToStr(i)+'  '+IntToStr((i+1)*100 div ACount)+'%');
        end;
      end;
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



end.
