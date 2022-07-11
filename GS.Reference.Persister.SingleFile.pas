//Simple Single file persister for GS.Reference.
//It can act as a in memory file too.
unit GS.Reference.Persister.SingleFile;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses Classes, SysUtils,
     GS.Stream,
     GS.BOList.Rec,
     GS.Reference.Persister;

Type
TofAllocationTableStruc = Record
  Offset        : int64;              //Offset of the allocation table.
  Previous      : Int64;              //Previous table entry. -1 if current is the first.
  Next          : Int64;              //Next table entry. -1 if current is the last.
  BufferOffset  : Int64;              //localization of real data (Value).
  Deleted       : Boolean;            //true if element deleted.
  Key           : String;             //Key of the pair Key/Value.
  BufferContentType : tofContentType; //Type of data value.
  BufferContentSize : Int64;          //Reals size, in bytes, of the data value.
  BufferRoom : Int64;                 //Room available in buffer : for example, for dynamic data, the room allocation will be *2 or more than the real size, in order to prevent too many realocation.
end;

TofReferencePersisterSingleFile = class(TofReferencePersister)
private
protected
  FFileName : string;
  FFileBuffer : TStream;

  FInMemoryAllocation : TofBusinessObjectListRec<String,TofAllocationTableStruc>;

  FTypeConversionAllowed: Boolean;
  FOnInitialLoading : TOnReferenceInitialLoad;
  FInMemoryOnly: Boolean;

  Procedure InternalCheckFileBuffer;
  Procedure InternalStartUp;
  Procedure InternalInitialLoading(aProgress : Double);
  Procedure InternalCreateFileBuffer;

  procedure SetFileName(const Value: String);
public
  //If file neam is empty, file wiil be in memory only. So, you can use that for in memory database.
  Constructor Create(Const FileName: String = ''; Const OnInitLoadEvent : TOnReferenceInitialLoad = Nil); reintroduce; Virtual;
  Destructor Destroy; Override;

  Procedure WriteGenericData( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Override;
  Procedure ReadGenericDataPrepare( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Override;

  Procedure DeleteGenericData(Key : String); Override;

  Procedure ReadInfoDataByIndex(aIndex : UInt32; var aDataType : TofcontentType; var aKey : String); Override;
  Procedure ReadInfoDataByKey(aKey : string; var aDataType : TofcontentType; var Index : UInt64); Override;
  Function IsKeyExists(aKey : String) : Boolean; Override;

  Procedure Open; Override;
  Procedure Close; Override;

  Function EntryCount : UInt64; Override;
  Function Version : String; Override;
  Function Description : string; Override;

  Property Allocation : TofBusinessObjectListRec<String,TofAllocationTableStruc> read FInMemoryAllocation;
  Property TypeConversionAllowedWriteTime : Boolean read FTypeConversionAllowed Write FTypeConversionAllowed;
  Property InMemoryOnly : Boolean read FInMemoryOnly;

  Property FileName : String read FFileName Write SetFileName;

  Property OnInitialLoading : TOnReferenceInitialLoad read FOnInitialLoading Write FOnInitialLoading;
End;

Procedure WriteTofAllocation(TargetStream : TStream; var aAlloc : TofAllocationTableStruc);
Procedure ReadTofAllocation(TargetStream : TStream; Var aAlloc : TofAllocationTableStruc);

implementation

Procedure WriteTofAllocation(TargetStream : TStream; Var aAlloc : TofAllocationTableStruc);
begin
  WriteInt64(TargetStream,aAlloc.Offset);
  WriteInt64(TargetStream,aAlloc.Previous);
  WriteInt64(TargetStream,aAlloc.Next);
  WriteInt64(TargetStream,aAlloc.BufferOffset);
  WriteBoolean(TargetStream,aAlloc.Deleted);
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
  aAlloc.Deleted := ReadBoolean(TargetStream);
  aAlloc.Key := ReadString(TargetStream);
  aAlloc.BufferContentType := tofContentType(ReadByte(TargetStream));
  aAlloc.BufferContentSize := ReadInt64(TargetStream);
  aAlloc.BufferRoom  := ReadInt64(TargetStream);
end;

{ TofReferencePersisterSingleFile }

procedure TofReferencePersisterSingleFile.Close;
begin
  if Assigned(FFileBuffer) then
    FreeAndNil(FFileBuffer);
  FInMemoryAllocation.Clear;
end;

constructor TofReferencePersisterSingleFile.Create(const FileName: String;
  const OnInitLoadEvent: TOnReferenceInitialLoad);
begin
  inherited Create;
  FFileName := FileName;
  FTypeConversionAllowed := True; //Allow to redefine type.
  FInMemoryAllocation := TofBusinessObjectListRec<String,TofAllocationTableStruc>.Create;
  FOnInitialLoading := OnInitLoadEvent;
end;

procedure TofReferencePersisterSingleFile.DeleteGenericData(Key: String);
var lEntry : TofAllocationTableStruc;
begin
  InternalCheckFileBuffer;
  if FInMemoryAllocation.ContainsKey(Key) then
  begin
    lEntry := TofAllocationTableStruc(FInMemoryAllocation[Key]);
    lEntry.Deleted := True;
    WriteTofAllocation(FFileBuffer,lEntry);
  end;
end;

function TofReferencePersisterSingleFile.Description: string;
begin
  Result := 'Single file (raw) persister.'
end;

destructor TofReferencePersisterSingleFile.Destroy;
begin
  inherited Destroy;
  Close;
  FreeAndNil(FInMemoryAllocation);
end;

function TofReferencePersisterSingleFile.EntryCount: UInt64;
begin
  Result := FInMemoryAllocation.Count;
end;

procedure TofReferencePersisterSingleFile.InternalCheckFileBuffer;
begin
  Assert(Assigned(FFileBuffer),'Persister not opened.');
end;

procedure TofReferencePersisterSingleFile.InternalCreateFileBuffer;
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
      FFileBuffer := TBufferedFileStream.Create(FFileName,fmCreate);
    end
    else
    begin
      FFileBuffer := TBufferedFileStream.Create(FFileName,fmOpenReadWrite);
    end;
  end;
end;

procedure TofReferencePersisterSingleFile.InternalInitialLoading(
  aProgress: Double);
begin
  if Assigned(FOnInitialLoading) then
    FOnInitialLoading(Self,aProgress);
end;

procedure TofReferencePersisterSingleFile.InternalStartUp;
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
      if (lEntry.Next>-1) then
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
end;

function TofReferencePersisterSingleFile.IsKeyExists(aKey: String): Boolean;
begin
  InternalCheckFileBuffer;
  Result := FInMemoryAllocation.ContainsKey(aKey);
end;

procedure TofReferencePersisterSingleFile.Open;
begin
  InternalCreateFileBuffer;
  InternalStartUp;
end;

procedure TofReferencePersisterSingleFile.ReadGenericDataPrepare(Key: String;
  DataType: tofContentType; StreamData: TMemoryStream);
var lEntry : TofAllocationTableStruc;
begin
  InternalCheckFileBuffer;
  Assert(Assigned(StreamData));
  StreamData.Clear;
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

  case DataType of
    ctUnknow: ;
    ctString:
    begin
      WriteString(StreamData,ReadString(FFileBuffer));
    end;
    ctStream:
    begin
      ReadStream(FFileBuffer,TStream(StreamData));
    end;
    ctInteger: WriteInteger(StreamData,ReadInteger(FFileBuffer));
    ctUINT32: WriteUInt32(StreamData,ReadUINT32(FFileBuffer));
    ctDouble: WriteDouble(StreamData,ReadDouble(FFileBuffer));
  end;
  StreamData.Position := 0;
end;

procedure TofReferencePersisterSingleFile.ReadInfoDataByIndex(aIndex: UInt32;
  var aDataType: TofcontentType; var aKey: String);
var lEntry : TofAllocationTableStruc;
begin
  InternalCheckFileBuffer;
  lEntry := FInMemoryAllocation.ByIndex[aIndex];
  aDataType := lEntry.BufferContentType;
  aKey := lEntry.Key;
end;

procedure TofReferencePersisterSingleFile.ReadInfoDataByKey(aKey: string;
  var aDataType: TofcontentType; var Index: UInt64);
var lEntry : TofAllocationTableStruc;
begin
  InternalCheckFileBuffer;
  lEntry := FInMemoryAllocation.ByKey[aKey];
  aDataType := lEntry.BufferContentType;
  Index := FInMemoryAllocation.IndexOf(aKey);
end;

procedure TofReferencePersisterSingleFile.SetFileName(const Value: String);
begin
  if Assigned(FFileBuffer) then
    raise Exception.Create('Unable to change thename if database is open');
  FFileName := Value;
end;

function TofReferencePersisterSingleFile.Version: String;
begin
  Result := '0.9';
end;

procedure TofReferencePersisterSingleFile.WriteGenericData(Key: String;
  DataType: tofContentType; StreamData: TMemoryStream);
var lEntry, lPrev : TofAllocationTableStruc;
    lprevExists : Boolean;
    ls : TMemoryStream;
begin
  InternalCheckFileBuffer;
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
    FInMemoryAllocation.ByKey[Key] := lEntry;
    //FInMemoryAllocation.RemoveKey(Key); //No ! Not change list sequence, because of inner bilateral ref. inside struct.
    //FInMemoryAllocation.Add(Key,lEntry);
  end
  else
  begin
    //New one : Go to end.
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

  //Write immediately.
  if FFileBuffer is TBufferedFileStream then begin
    TBufferedFileStream(FFileBuffer).FlushBuffer;
  end;
end;


end.
