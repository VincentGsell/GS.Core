unit GS.FileOp.Glacify.Protocol.Default;

interface

uses
  Classes,
  SysUtils,
  Math,
  GS.FileOp,
  GS.Stream;

const
  CST_SIG_GLACIER = 'GSGLACIER';

type
  ///
  ///  Default FGlacier File structure is straighforwardly simple :
  ///  It conitnously write, as a diary (monitoring tail), and never
  ///  ever rewrite something.
  ///  In Each File block, we found whole FileBlockMap which is consistency with the block.
  ///  than we can easely, by scanning, rebuild whole file of version, or fellow all version file.
  ///

  TGlacierSigCode = (gscUnknown, gscHeader, gscFileBlock, gscDataBlock);

  TGlacierFileBlock = packed record
  public
    SigCode: TGlacierSigCode;
    BlockIndex: UINT32;
    FileId : UINT64;
    FileName: string;
    DateTag: TTimeStamp;
    OriginalFileSize : UINT64;
    DateLastUpdateTag: TTimeStamp;
    Version: UINT32;
    FileCompleteBlockMap: GS.Stream.TUint32Array; //Index of Block.
    compressed: boolean;
    encrypted: boolean;
    //CompressedAlg : string; //Later.
    //CryptographicAlg  : String;
    procedure SaveToStream(aTargetStream : TStream);
    procedure LoadFromStream(aSourceStream : TStream);
  end;

  TGlacierDataBlock = packed record
  public
    SigCode: TGlacierSigCode;
    BlockIndex: UINT32;
    CRC32: UINT32;
    DateTag: TTimeStamp;
    DataLengh: UINT32;
    OriginalDataLength : UINT32;
    compressRatio: single;
    ///
    ///  Here raw data
    ///
    procedure SaveToStream(aTargetStream : TStream);
    procedure LoadFromStream(aSourceStream : TStream);
  end;

  ///
  ///  Header of whole FGlacier File...
  ///
  TGlacierFileHeader = packed record
  public
    SigCode: TGlacierSigCode;
    Sig: string;
    Version: string;
    BlockCount: Uint32;

    procedure SaveToStream(aTargetStream : TStream);
    procedure LoadFromStream(aSourceStream : TStream);
  end;

  ///
  ///  Allocation table to fast retrieval.
  ///
  ///
  ///  How to do : Put a in memory one ? Then add BlockId.
  ///
  TGlacierAllocationTableItem = Packed Record
    SigCode : TGlacierSigCode;
    BlockIndex : UINT32;
    Offset : UINT64;

    procedure SaveToStream(aTargetStream : TStream);
    procedure LoadFromStream(aSourceStream : TStream);
  end;

  TGlacierAllocation = Record
    Allocations : array of TGlacierAllocationTableItem; //In memory only.
    AllocationIndex : UInt32;

    Procedure AddAllocationItem(aSigCode : TGlacierSigCode; aBlockIndex : UINT32; aOffset : UINT64);
  end;


  //This object must not exposed : Internal use only. It represent the whole glacier.
  TGlacierFileObject = class(TFileStream)
  protected
  public
    Header: TGlacierFileHeader;
    FileBlock: TGlacierFileBlock; //Current. (Available after a ReadFileBlock call)
    DataBlock: TGlacierDataBlock; //Current. (Available after a ReadDataBlock call)
    AllocationManager : TGlacierAllocation;

    constructor Create(var aGlacifyTask: TGlacifyProcessInfo); reintroduce;
    procedure WriteHeader;
    procedure WriteFileBlock(var al: TGlacierFileBlock);
    procedure WriteDataBlock(var al: TGlacierDataBlock);
    function ReadSignature: TGlacierSigCode;
    procedure ReadHeader;    //MUST call ReadSignature before.
    procedure ReadFileBlock; //MUST call ReadSignature before.
    procedure ReadDataBlock; //MUST call ReadSignature before.
    procedure BuildAllocationTable;

    //Tools
    function FlushGlacierFileStructure: UTF8String;

    //Utility
    function isFileExists(const aFileName: string): Boolean;
    function FileInformations(const aFileName: string; var aFileInformation: TFileInformationStructure): Boolean;
    function FilesList(var aFilesList : TFilesList): Boolean;
  end;

  TGlacifyProtocolDefault = class(TGlacifyProtocolItem)
  private
    procedure Internal_InitNewGlacierFileHeader(var agfh: TGlacierFileHeader);
    procedure Internal_InitNewGlacierFileBlock(var agfh: TGlacierFileBlock);
    procedure Internal_InitNewGlacierDataBlock(var agfh: TGlacierDataBlock);

    procedure ExtractBlockFromExternalFile( var aGlacifyTask: TGlacifyProcessInfo;
                                            const aBlockIndex: UInt32;
                                            var aDataBlock: TGlacierDataBlock;
                                            var aBlockBinary : TMemoryStream;
                                            Const Relocate : Boolean = True);
  protected
    FGlacier: TGlacierFileObject; //Glacier file stream.
    FCurrentFile: TFileStream;    //Current file outside the glacier.
    function PushWholeNewFileToGlacier(var aGlacifyTask: TGlacifyProcessInfo): boolean;
    function UpdateExistingFileToGlacier(var aGlacifyTask: TGlacifyProcessInfo): boolean;
  public
    procedure Init(var aGlacifyTask: TGlacifyProcessInfo); override;
    procedure Process(var aGlacifyTask: TGlacifyProcessInfo); override;
    procedure Finalize(var aGlacifyTask: TGlacifyProcessInfo); override;

    //Tools
    function Glacier_FlushStructure: UTF8String; override;

    //Utility
    function Glacier_IsFileExists(const aFileName: string): Boolean; override;
    function Glacier_FileInformations(const aFileName: string; var aFileInformation: TFileInformationStructure): Boolean; override;
    function Glacier_FileList(var aFlileList : TFilesList) : Boolean; Override;
    function Glacier_FileCompare( const FileNameFileSystem : String;
                                  const FileNameGlacier : String;
                                  out BlockCountDiffers : UInt32; //differs, or newer.
                                  var FileSystemInfo : TFileInformationStructure;
                                  var FileGlacierInfo : TFileInformationStructure) : Boolean; Override;
  end;

implementation


{ TGlacifyProtocolDefault }

function TGlacifyProtocolDefault.Glacier_FlushStructure: UTF8String;
begin
  Result := inherited;
  if Assigned(FGlacier) then //You have to call init() before use.
    result := FGlacier.FlushGlacierFileStructure;
end;

procedure TGlacifyProtocolDefault.Internal_InitNewGlacierDataBlock(var agfh: TGlacierDataBlock);
begin
  agfh.SigCode := gscDataBlock;
  agfh.BlockIndex := 0;
  agfh.CRC32 := 0;
  //DateTag :  TTimeStamp;
  agfh.DataLengh := 0;
  agfh.compressRatio := 0.0;
end;

procedure TGlacifyProtocolDefault.Internal_InitNewGlacierFileBlock(var agfh: TGlacierFileBlock);
begin
  agfh.SigCode := gscFileBlock;
  agfh.BlockIndex := 0;
  agfh.FileName := '';
//  agfh.DateTag := 0.0;
  agfh.OriginalFileSize := 0;
//  agfh.DateLastUpdateTag := 0.0;
  agfh.Version := 1;
  agfh.compressed := false;
  agfh.encrypted := false;
  agfh.FileCompleteBlockMap := Nil;
end;

procedure TGlacifyProtocolDefault.Internal_InitNewGlacierFileHeader(var agfh: TGlacierFileHeader);
begin
  agfh.SigCode := gscHeader;
  agfh.Sig := CST_SIG_GLACIER;
  agfh.Version := '1';
  agfh.BlockCount := 0;
end;

function TGlacifyProtocolDefault.Glacier_IsFileExists(const aFileName: string): Boolean;
begin
  Result := False;
  if Assigned(FGlacier) then
  begin
    Result := FGlacier.isFileExists(aFileName);
  end;
end;

function TGlacifyProtocolDefault.PushWholeNewFileToGlacier(var aGlacifyTask: TGlacifyProcessInfo): Boolean;
var
  lFileInfo : TFileInfo;
  lFileBlock: TGlacierFileBlock;
  lDataBlock: TGlacierDataBlock;
  i,j : integer;
  l :  TMemoryStream;
  lbl : UInt32;
begin
  Assert(Assigned(FCurrentFile));
  TGSBin.FileInfo(FCurrentFile.Handle,lFileInfo);
  Result := false;
  Internal_InitNewGlacierFileBlock(lFileBlock);
  lFileBlock.FileId := lFileInfo.FileId;
  lFileBlock.FileName := aGlacifyTask.inFilesource;
  lFileBlock.Version := 1;
  lFileBlock.compressed := gpoCompress in aGlacifyTask.InOptions;
  lFileBlock.encrypted := gpoEncrypt in aGlacifyTask.InOptions;
  if (lFileBlock.encrypted) and (Length(aGlacifyTask.InEncryptPassword) = 0) then
  begin
    aGlacifyTask.OutOperationCode := TGlacifyOpCode.gocErrorEncryptionPasswordEmpty;
    Exit;
  end;
  lFileBlock.DateTag := DateTimeToTimeStamp(Now);
  lFileBlock.DateLastUpdateTag := lFileInfo.LastWriteAccess;
  lFileBlock.OriginalFileSize := lFileInfo.FileSize;

  FGlacier.Header.BlockCount := FGlacier.Header.BlockCount + 1;
  lFileBlock.BlockIndex := FGlacier.Header.BlockCount;
  lbl := FCurrentFile.Size div CST_GLACIER_BLOCK;
  if FCurrentFile.Size mod CST_GLACIER_BLOCK > 0 then
    inc(lbl);
  SetLength(lFileBlock.FileCompleteBlockMap,lbl);
  for I := 0 to Length(lFileBlock.FileCompleteBlockMap) - 1 do
    lFileBlock.FileCompleteBlockMap[I] := FGlacier.Header.BlockCount + 1 + I;

  FGlacier.Position := FGlacier.Size;
  FGlacier.WriteFileBlock(lFileBlock);

  FCurrentFile.Position := 0;
  l := TMemoryStream.Create;
  try
    for I := 0 to Length(lFileBlock.FileCompleteBlockMap)- 1 do
    begin
      ExtractBlockFromExternalFile(aGlacifyTask, i, lDataBlock, l, False);
      FGlacier.WriteDataBlock(lDataBlock);
      FGlacier.CopyFrom(l, l.Size); //Data location is always immediately after the header.
    end;
  finally
    FreeAndNil(l);
  end;
  Assert(FCurrentFile.Position = FCurrentFile.Size); //If this assert is wrong, is means that the FileCompleteBlock is wrong.

  //Update Block counter.
  FGlacier.WriteHeader; //Update counter.
  result := true;
end;

function TGlacifyProtocolDefault.UpdateExistingFileToGlacier(var aGlacifyTask: TGlacifyProcessInfo): boolean;
Type
  TDual = Record
    ExternalFileIndex : UINT32;
    GlacierFileIndex : UINT32;
  end;
var lf, lexf : TFileInformationStructure;
    lbc : UINT32;
    i,j : integer;

    lnewFileBlock : TGlacierFileBlock;
    lnewDataBlock : TGlacierDataBlock;
    lExternalFileBlockIndexToBeHarvested : Array of TDual;
    lExtFile : TFileInfo;

    lCurrentBlockIndex : UINT32;
    l : TMemoryStream;

    FGHB : UINT32;


begin
  Assert(Assigned(FCurrentFile));
  result := Glacier_FileCompare(aGlacifyTask.inFilesource,aGlacifyTask.inFilesource,lbc,lexf,lf);
  if result and (lbc>0) then
  begin
    TGSBin.FileInfo(FCurrentFile.Handle,lExtFile);

    Internal_InitNewGlacierFileBlock(lnewFileblock);
    lnewFileBlock.FileId := lExtFile.FileId;
    lnewFileBlock.FileName := lexf.fileName;
    lnewFileBlock.Version := lf.FileVersion + 1;
    lnewFileBlock.FileCompleteBlockMap := Nil;
    lnewFileBlock.compressed := gpoCompress in aGlacifyTask.InOptions;
    lnewFileBlock.encrypted := gpoEncrypt in aGlacifyTask.InOptions;
    if (lnewFileBlock.encrypted) and (Length(aGlacifyTask.InEncryptPassword) = 0) then
    begin
      aGlacifyTask.OutOperationCode := TGlacifyOpCode.gocErrorEncryptionPasswordEmpty;
      Exit;
    end;
    lnewFileBlock.DateTag := DateTimeToTimeStamp(Now);
    lnewFileBlock.DateLastUpdateTag := lExtFile.LastWriteAccess;
    lnewFileBlock.OriginalFileSize := lExtFile.FileSize;

    lCurrentBlockIndex := FGlacier.Header.BlockCount;

    //File is not empty.
    inc(lCurrentBlockIndex);
    lnewFileBlock.BlockIndex := lCurrentBlockIndex;

    //Now, Fixing new lNewFileBlock.FileCompleteBlockMap
    j := Length(lf.fileBlock);
    Setlength(lnewFileBlock.FileCompleteBlockMap, Length(lexf.fileBlock));
    Setlength(lExternalFileBlockIndexToBeHarvested, Length(lexf.fileBlock));

    for i := 0 to Length(lexf.fileBlock)-1 do
    begin
      //Block are always sequential.
      //We pass throught the external file, and check of block consistancy.
      lExternalFileBlockIndexToBeHarvested[i].ExternalFileIndex := lexf.fileBlock[i].blockindex;
      inc(lCurrentBlockIndex);
      lExternalFileBlockIndexToBeHarvested[i].GlacierFileIndex := lCurrentBlockIndex; //Whole new one...

      if (i<j) and (lf.fileBlock[i].blockCRC32 = lexf.fileBlock[i].blockCRC32) then
      begin
        dec(lCurrentBlockIndex);
        lExternalFileBlockIndexToBeHarvested[i].GlacierFileIndex := lf.fileBlock[i].blockindex  //...But not if it already exits.
      end;
    end;

    for i := 0 to Length(lExternalFileBlockIndexToBeHarvested)-1 do
    begin
      lnewFileBlock.FileCompleteBlockMap[i] := lExternalFileBlockIndexToBeHarvested[i].GlacierFileIndex;
    end;

    //Writing sequence.
    FGlacier.Position := FGlacier.Size;
    FGlacier.WriteFileBlock(lnewFileBlock);
    l := TMemoryStream.Create;
    try
      for i := 0 to Length(lnewFileBlock.FileCompleteBlockMap)-1 do
      begin
        if lnewFileBlock.FileCompleteBlockMap[i] > FGlacier.Header.BlockCount then
        begin
          Internal_InitNewGlacierDataBlock(lnewDataBlock);
          ExtractBlockFromExternalFile( aGlacifyTask,
                                        lExternalFileBlockIndexToBeHarvested[i].ExternalFileIndex,
                                        lnewDataBlock,l);
          //Manage block index form Glacier.header. We have to fix that with our counter.
          lnewDataBlock.BlockIndex := lExternalFileBlockIndexToBeHarvested[i].GlacierFileIndex;
          FGlacier.WriteDataBlock(lnewDataBlock);
          FGlacier.CopyFrom(l, l.Size); //Data location is always immediately after the header.
        end;
      end;
    finally
      FreeAndNil(l);
    end;

    //Update Block counter.
    FGlacier.Header.BlockCount := lCurrentBlockIndex;
    FGlacier.WriteHeader; //Update counter.
    result := true;
  end;
end;

{ TGlacierFileObject }

procedure TGlacierFileObject.BuildAllocationTable;
var
  lSignature: TGlacierSigCode;
  lbIndex : UINT32;
  lLastOffset : UINT64;
begin
  //Step one : Gather file block.
  Position := 0;
  if ReadSignature = gscHeader then
    ReadHeader;

  AllocationManager.AddAllocationItem( TGlacierSigCode.gscHeader,0,0);
  while not (Position = Size) do
  begin
    lLastOffset := Position;
    case ReadSignature of
      gscUnknown, gscHeader:
        raise Exception.Create('Abnormal Signature found');
      gscFileBlock:
        begin
          ReadFileBlock;
          AllocationManager.AddAllocationItem( TGlacierSigCode.gscFileBlock,
                                               FileBlock.BlockIndex,
                                               lLastOffset);
        end;
      gscDataBlock:
        begin
          ReadDataBlock;
          Position := Position + DataBlock.DataLengh; //Jump pure data.
          AllocationManager.AddAllocationItem( TGlacierSigCode.gscDataBlock,
                                               DataBlock.BlockIndex,
                                               lLastOffset);
        end;
    end;
  end;
end;

constructor TGlacierFileObject.Create;

  function SourceFileExists: Integer;
  begin
    result := fmCreate;
    if FileExists(aGlacifyTask.InGlacierName) then
      result := fmOpenReadWrite;
  end;

begin
  inherited Create(aGlacifyTask.InGlacierName, SourceFileExists or fmExclusive);
  BuildAllocationTable;
end;

function TGlacierFileObject.FileInformations(const aFileName: string; var aFileInformation: TFileInformationStructure): Boolean;
var
  i,j : UINT32;
  lh  : Array of UINT32;
begin
  //Step one : Gather file block.
  InitFileInformationStructure(aFileName,aFileInformation);
  result := true;
  for I := 0 to AllocationManager.AllocationIndex do
  begin
    if AllocationManager.Allocations[i].SigCode = gscFileBlock then
    begin
      Position := AllocationManager.Allocations[i].Offset;
      Assert(ReadSignature = gscFileBlock);
      ReadFileBlock;
      if aFileName = FileBlock.FileName then
      begin
        aFileInformation.fileName := FileBlock.FileName;
        aFileInformation.fileversion := FileBlock.Version;
        aFileInformation.fileSize := FileBlock.OriginalFileSize;
        aFileInformation.fileLastModification := FileBlock.DateLastUpdateTag;
        SetLength(aFileInformation.fileBlock,Length(FileBlock.FileCompleteBlockMap));
        SetLength(lh,Length(FileBlock.FileCompleteBlockMap));
        for j := 0 to length(FileBlock.FileCompleteBlockMap)-1 do
          lh[j] := FileBlock.FileCompleteBlockMap[j];
      end;
    end;
  end;

  //Step 2 : Gather file block.
  if length(lh)=0 then
    Exit; //This version of the file is empty in glacier.

  for j := 0 to Length(lh)-1 do
    for I := 0 to AllocationManager.AllocationIndex do
    begin
      if (AllocationManager.Allocations[i].SigCode = gscDataBlock) and
         (AllocationManager.Allocations[i].BlockIndex = lh[j]) then
      begin
        Position := AllocationManager.Allocations[i].Offset;
        Assert(ReadSignature = gscDataBlock);
        ReadDataBlock;
        Assert(DataBlock.BlockIndex = lh[j]);
        aFileInformation.fileBlock[j].blockindex := DataBlock.BlockIndex;
        aFileInformation.fileBlock[j].blockCRC32 := DataBlock.CRC32;
        aFileInformation.fileBlock[j].blockSize := DataBlock.OriginalDataLength;
      end;
    end;
end;

function TGlacierFileObject.FilesList(var aFilesList: TFilesList): Boolean;
var
  i : UINT32;
  lbIndex :UINT32;
  lbfile : Integer;
begin
  //Step one : Gather file block.
  result := true;
  lbIndex := 0;
  aFilesList := nil;
  lbFile := 0;
  for I := 0 to AllocationManager.AllocationIndex do
  begin
    if AllocationManager.Allocations[i].SigCode = gscFileBlock then
    begin
      if (lbIndex = Length(aFilesList)-1) or (Length(aFilesList)=0) then
      begin
        SetLength(aFilesList,Length(aFilesList)+100);
      end;
      Position := AllocationManager.Allocations[i].Offset;
      Assert(ReadSignature = gscFileBlock);
      ReadFileBlock;
      aFilesList[lbIndex].BlockIndex := FileBlock.BlockIndex;
      aFilesList[lbIndex].FileName := FileBlock.FileName;
      aFilesList[lbIndex].DateTag := FileBlock.DateTag;
      aFilesList[lbIndex].OriginalFileSize := FileBlock.OriginalFileSize;
      aFilesList[lbIndex].DateLastUpdateTag := FileBlock.DateLastUpdateTag;
      aFilesList[lbIndex].Version := FileBlock.Version;
      aFilesList[lbIndex].FileBlockMapCount := Length(FileBlock.FileCompleteBlockMap);
      aFilesList[lbIndex].compressed := FileBlock.compressed;
      aFilesList[lbIndex].encrypted := FileBlock.encrypted;
      inc(lbIndex);
      inc(lbfile);
    end;
  end;
  SetLength(aFilesList,lbfile);
end;

function TGlacierFileObject.FlushGlacierFileStructure: UTF8String;

  procedure outputln(aText: UTF8String);
  begin
    result := result + #13#10 + aText;
  end;

  procedure outputlnMap(const al: array of Cardinal);
  var
    i: Integer;
    l: string;
  begin
    l := '';
    for i := 0 to Length(al) - 1 do
      l := l + ' ' + IntTostr(al[i]);
    result := result + #13#10 + l;
  end;

var
  i: integer;
  Signature: TGlacierSigCode;
begin
  outputln('FGlacier file flush --------------');
  Position := 0;
  try
    if ReadSignature = gscHeader then
      ReadHeader;
    outputln(Format('Header SigCode...."%d"', [Byte(Header.SigCode)]));
    outputln(Format('       Sig........"%s"', [Header.Sig]));
    outputln(Format('       SigCode...."%s"', [Header.Version]));
    outputln(Format('       BlockCount."%d"', [Header.BlockCount]));

    while not (Position = Size) do
    begin
      case ReadSignature of
        gscUnknown, gscHeader:
          raise Exception.Create('Abnormal Signature found');
        gscFileBlock:
          begin
            ReadFileBlock;
            outputln(' File Block detected');
            outputln(Format('  SigCode......."%d"', [Byte(FileBlock.SigCode)]));
            outputln(Format('  BlockIndex...."%d"', [FileBlock.BlockIndex]));
            outputln(Format('  FileName......"%s"', [FileBlock.FileName]));
            outputln(Format('  compressed...."%d"', [Integer(FileBlock.compressed)]));
            outputln(Format('  crypted......."%d"', [Integer(FileBlock.encrypted)]));
            outputln(Format('  FileBlockMap.."%d"', [Length(FileBlock.FileCompleteBlockMap)]));
            outputlnMap(FileBlock.FileCompleteBlockMap);
          end;
        gscDataBlock:
          begin
            ReadDataBlock;
            Position := Position + DataBlock.DataLengh; //Jump pure data.
            outputln(' Data Block detected');
            outputln(Format('  SigCode................"%d"', [Byte(DataBlock.SigCode)]));
            outputln(Format('  BlockIndex............."%d"', [DataBlock.BlockIndex]));
            outputln(Format('  CRC32.................."%u"', [UInt32(DataBlock.CRC32)]));
            outputln(Format('  DataLength............."%d"', [DataBlock.DataLengh]));
            outputln(Format('  OriginalDataLength....."%d"', [DataBlock.OriginalDataLength]));
            outputln(Format('  CompressRatio.........."%g"', [DataBlock.compressRatio]));
          end;
      end;

    end;

  finally

  end;
  outputln('FGlacier end of flush --------------');
end;

function TGlacierFileObject.isFileExists(const aFileName: string): Boolean;
var
  i : UINT32;
begin
  result := false;
  for I := 0 to AllocationManager.AllocationIndex do
  begin
    if AllocationManager.Allocations[i].SigCode = gscFileBlock then
    begin
      Position := AllocationManager.Allocations[i].Offset;
      Assert(ReadSignature = gscFileBlock);
      ReadFileBlock;
      result := aFileName = FileBlock.FileName;
      if result then
        Break;
    end;
  end;
end;

procedure TGlacierFileObject.ReadDataBlock;
begin
  DataBlock.LoadFromStream(Self);
end;

procedure TGlacierFileObject.ReadFileBlock;
begin
  FileBlock.LoadFromStream(Self);
end;

procedure TGlacierFileObject.ReadHeader;
begin
  Header.LoadFromStream(Self);
end;

function TGlacierFileObject.ReadSignature: TGlacierSigCode;
begin
  result := TGlacierSigCode(ReadByte(Self));
end;

procedure TGlacierFileObject.WriteDataBlock(var al: TGlacierDataBlock);
var signature: TGlacierSigCode;
begin
  signature := gscDataBlock;
  WriteByte(Self,Byte(Signature));
  al.SaveToStream(Self);
end;

procedure TGlacierFileObject.WriteFileBlock(var al: TGlacierFileBlock);
var signature: TGlacierSigCode;
begin
  signature := gscFileBlock;
  WriteByte(Self,Byte(Signature));
  al.SaveToStream(Self);
end;

procedure TGlacierFileObject.WriteHeader;
var signature: TGlacierSigCode;
begin
  Position := 0;
  signature := gscHeader;
  WriteByte(Self,Byte(Signature));
  Header.SaveToStream(Self);
end;


///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------

procedure TGlacifyProtocolDefault.Init(var aGlacifyTask: TGlacifyProcessInfo);

  procedure InitGlacierFile;
  begin
    Internal_InitNewGlacierFileHeader(FGlacier.Header);
    FGlacier.WriteHeader;
  end;

  function ReadGlacierHeader: Boolean;
  begin
    Result := false;
    if (FGlacier.ReadSignature = gscHeader) then
    begin
      FGlacier.ReadHeader;
      result := (FGlacier.Header.SigCode = gscHeader) and (FGlacier.Header.Sig = CST_SIG_GLACIER);
    end;
  end;

begin
  if aGlacifyTask.inFilesource <> '' then
  begin
    try
      FCurrentFile := TFileStream.Create(aGlacifyTask.InFileSource, fmOpenRead or fmShareDenyNone);
    except
      aGlacifyTask.OutOperationCode := gocErrorFileSourceUnavailable;
      raise
    end;
  end;

  try
    FGlacier := TGlacierFileObject.Create(aGlacifyTask);
  except
    aGlacifyTask.OutOperationCode := gocErrorGlacierRessourceUnavailable;
    raise
  end;

  if FGlacier.Size = 0 then
  begin
    InitGlacierFile;
  end
  else
  begin
    FGlacier.Position := 0;
    if not ReadGlacierHeader then
      aGlacifyTask.OutOperationCode := gocErrorGlacierRessourceUnreadable
  end;

  if aGlacifyTask.inFilesource <> '' then
    if not (Glacier_IsFileExists(aGlacifyTask.inFilesource)) then
    begin
      if PushWholeNewFileToGlacier(aGlacifyTask) then
        aGlacifyTask.OutOperationCode := gocSuccess;
    end
    else
    begin
      aGlacifyTask.OutOperationCode := gocProcess
    end;
end;

procedure TGlacifyProtocolDefault.Process(var aGlacifyTask: TGlacifyProcessInfo);
begin
  if not (aGlacifyTask.OutOperationCode = gocProcess) then
    Exit;
  if UpdateExistingFileToGlacier(aGlacifyTask) then
    aGlacifyTask.OutOperationCode := gocSuccess;
end;

function TGlacifyProtocolDefault.Glacier_FileCompare(const FileNameFileSystem,
  FileNameGlacier: String; out BlockCountDiffers: UInt32; var FileSystemInfo,
  FileGlacierInfo: TFileInformationStructure): Boolean;
var i : integer;
    lLittlest : Uint32;
begin
  Result := false;
  BlockCountDiffers := 0;

  result := TGSBin.GlacifyAnalytic_FileSystem_FileInformation(FileNameFileSystem,FileSystemInfo) And
            Glacier_FileInformations(FileNameGlacier,FileGlacierInfo);
  if result then
  begin
    lLittlest := Length(FileSystemInfo.fileBlock);
    BlockCountDiffers := Abs(lLittlest - Length(FileGlacierInfo.fileBlock));
    if lLittlest > Length(FileGlacierInfo.fileBlock) then
      lLittlest := Length(FileGlacierInfo.fileBlock);

    for I := 0 to lLittlest-1 do
    begin
      if Not ((FileSystemInfo.fileBlock[i].blockCRC32 = FileGlacierInfo.fileBlock[i].blockCRC32) And
              (FileSystemInfo.fileBlock[i].blockSize = FileGlacierInfo.fileBlock[i].blockSize)) then
      begin
        Inc(BlockCountDiffers);
      end;
    end;
  end;
end;

procedure TGlacifyProtocolDefault.ExtractBlockFromExternalFile( var aGlacifyTask: TGlacifyProcessInfo;
                                            const aBlockIndex: UInt32;
                                            var aDataBlock: TGlacierDataBlock;
                                            var aBlockBinary : TMemoryStream;
                                            Const Relocate : Boolean = True);
var
  lGlacierProgress: TGlacifyProgress;
  ls: UInt64;
  ldelta: Cardinal;
  lc: Cardinal;
  lratio: Single;
  SBuf: RawByteString;
  TBuf: RawByteString;
  l2 : TMemoryStream;
begin
  //Locate : Base 0 (File origin).
  if Relocate then
    if ((aBlockIndex-1) * CST_GLACIER_BLOCK) < FCurrentFile.Size then
      FCurrentFile.Position := ((aBlockIndex-1) * CST_GLACIER_BLOCK);

  if Assigned(aGlacifyTask.OutCallBackProc) then
  begin
    lGlacierProgress.Filesource := FCurrentFile.FileName;
    lGlacierProgress.FileNameInGlacier := aGlacifyTask.inFilesource;
    lGlacierProgress.GLacierName := aGlacifyTask.InGlacierName;
    lGlacierProgress.CurrentOperationCode := gocProcess;
    lGlacierProgress.PercentDone := FCurrentFile.Position * 100 / FCurrentFile.Size;
    lGlacierProgress.Info := '';
    aGlacifyTask.OutCallBackProc(lGlacierProgress);
  end;

  ls := CST_GLACIER_BLOCK;
  ldelta := FCurrentFile.Size - FCurrentFile.Position;
  if ldelta < CST_GLACIER_BLOCK then
    ls := ldelta;
  aBlockBinary.Clear;
  aBlockBinary.CopyFrom(FCurrentFile, ls);
  aBlockBinary.Position := 0;
  Internal_InitNewGlacierDataBlock(aDataBlock);
  FGlacier.Header.BlockCount := FGlacier.Header.BlockCount + 1;
  aDataBlock.BlockIndex := FGlacier.Header.BlockCount;
  TGSBin.CalcCRC32(TStream(aBlockBinary), lc);
  aDataBlock.CRC32 := lc;
  aDataBlock.DateTag := DateTimeToTimeStamp(Now);
  aBlockBinary.Position := 0;
  aDataBlock.OriginalDataLength := aBlockBinary.Size;
  //Crypto
  if gpoEncrypt in aGlacifyTask.InOptions then
  begin
    // if Assigned(onCrypto..) them FOnCrypto(aBlockBinary)
    //else
    // Default encryption. (xor here.)
    //TODO : Add compilation directive dor crypto system :
    //TODO : Add compilation directive for mORMot
    //TODO : Add compilation directive for ZCrypto
    //Default.
    l2 := TMemoryStream.Create;
    try
      begin
        TGSBin.Crypto_EncryptDecryptXor(TStream(aBlockBinary), TStream(l2), aGlacifyTask.InEncryptPassword);
        aBlockBinary.LoadFromStream(l2);
        l2.Clear;
        aBlockBinary.Position := 0;
      end;
    finally
      FreeAndNil(l2);
    end;
  end;
  //Compress
  lratio := 0.0;
  if gpoCompress in aGlacifyTask.InOptions then
  begin
    // if Assigned(onCompress..) them FOnCompress(aBlockBinary)
    //else
    // Default compression (rle here.)
    //l2.Size := aBlockBinary.Size;
    //TODO : Add compilation directive dor compress system :
    //TODO : Add https://github.com/zedalaye/d7zip.git
    //TODO : Add mORMot
    begin
      SetLength(SBuf, aBlockBinary.Size);
      aBlockBinary.ReadBuffer(Pointer(SBuf)^, aBlockBinary.Size);
      //TBuf := AlgoSynLZ.Compress(SBuf);
      //TBuf := AlgoDeflate.Compress(SBuf);
      TBuf := SBuf;
      if aBlockBinary.Size > 0 then
        lratio := Length(TBuf) / aBlockBinary.Size;
      aBlockBinary.Clear;
      aBlockBinary.Write(pointer(TBuf)^, length(TBuf));
      aBlockBinary.Position := 0;
    end;
  end;
  aDataBlock.DataLengh := aBlockBinary.Size;
  //Eventualy crypted and compressed.
  aDataBlock.compressRatio := 1-lratio;
end;

function TGlacifyProtocolDefault.Glacier_FileInformations(const aFileName: string; var aFileInformation: TFileInformationStructure): Boolean;
begin
  Result := False;
  if Assigned(FGlacier) then
  begin
    Result := FGlacier.FileInformations(aFileName, aFileInformation);
  end;
end;

function TGlacifyProtocolDefault.Glacier_FileList(
  var aFlileList: TFilesList): Boolean;
begin
  Result := False;
  if Assigned(FGlacier) then
  begin
    Result := FGlacier.FilesList(aFlileList);
  end;
end;

procedure TGlacifyProtocolDefault.Finalize(var aGlacifyTask: TGlacifyProcessInfo);
begin
  if Assigned(FCurrentFile) then
    FreeAndNil(FCurrentFile);
  if Assigned(FGlacier) then
    FreeAndNil(FGlacier);
end;

{ TGlacierAllocation }

procedure TGlacierAllocation.AddAllocationItem(aSigCode: TGlacierSigCode;
  aBlockIndex: UINT32; aOffset: UINT64);
begin
  if Length(Allocations) = 0 then
  begin
    SetLength(Allocations,100);
    AllocationIndex := 0;
  end;

  if AllocationIndex=Length(Allocations)-1 then
  begin
    SetLength(Allocations,Length(Allocations)*2);
  end;

  Allocations[AllocationIndex].SigCode := aSigCode;
  Allocations[AllocationIndex].BlockIndex := aBlockIndex;
  Allocations[AllocationIndex].Offset := aOffset;
  Inc(AllocationIndex);
end;

{ TGlacierFileHeader }

procedure TGlacierFileHeader.LoadFromStream(aSourceStream: TStream);
begin
  SigCode := TGlacierSigCode(GS.Stream.ReadByte(aSourceStream));
  Assert(SigCode = TGlacierSigCode.gscHeader);
  Sig := GS.Stream.ReadString(aSourceStream);
  Version := GS.Stream.ReadString(aSourceStream);
  BlockCount := GS.Stream.ReadUINT32(aSourceStream);
end;

procedure TGlacierFileHeader.SaveToStream(aTargetStream: TStream);
begin
  GS.Stream.WriteByte(aTargetStream, Byte(TGlacierSigCode.gscHeader));
  GS.Stream.WriteString(aTargetStream, Sig);
  GS.Stream.WriteString(aTargetStream, Version);
  GS.Stream.WriteUInt32(aTargetStream, BlockCount);
end;

{ TGlacierAllocationTableItem }

procedure TGlacierAllocationTableItem.LoadFromStream(aSourceStream: TStream);
begin
  SigCode := TGlacierSigCode(GS.Stream.ReadByte(aSourceStream));
  BlockIndex := GS.Stream.ReadUINT32(aSourceStream);
  Offset := GS.Stream.ReadUInt64(aSourceStream);
end;

procedure TGlacierAllocationTableItem.SaveToStream(aTargetStream: TStream);
begin
  GS.Stream.WriteByte(aTargetStream, Byte(TGlacierSigCode.gscUnknown));
  GS.Stream.WriteUInt32(aTargetStream, BlockIndex);
  GS.Stream.WriteUInt64(aTargetStream, Offset);
end;

{ TGlacierDataBlock }

procedure TGlacierDataBlock.LoadFromStream(aSourceStream: TStream);
begin
  SigCode := TGlacierSigCode(GS.Stream.ReadByte(aSourceStream));
  Assert(SigCode = TGlacierSigCode.gscDataBlock);
  BlockIndex := GS.Stream.ReadUINT32(aSourceStream);
  CRC32 := GS.Stream.ReadUINT32(aSourceStream);
  DateTag.Time := GS.Stream.ReadInteger(aSourceStream);
  DateTag.Date := GS.Stream.ReadInteger(aSourceStream);
  DataLengh := GS.Stream.ReadUINT32(aSourceStream);
  OriginalDataLength := GS.Stream.ReadUINT32(aSourceStream);
  compressRatio := GS.Stream.ReadSingle(aSourceStream);
end;

procedure TGlacierDataBlock.SaveToStream(aTargetStream: TStream);
begin
  GS.Stream.WriteByte(aTargetStream, Byte(TGlacierSigCode.gscDataBlock));
  GS.Stream.WriteUInt32(aTargetStream, BlockIndex);
  GS.Stream.WriteUInt32(aTargetStream, CRC32);
  GS.Stream.WriteInteger(aTargetStream, DateTag.Time);
  GS.Stream.WriteInteger(aTargetStream, DateTag.Date);
  GS.Stream.WriteUInt32(aTargetStream, DataLengh);
  GS.Stream.WriteUInt32(aTargetStream, OriginalDataLength);
  GS.Stream.WriteSingle(aTargetStream, compressRatio);
end;

{ TGlacierFileBlock }

procedure TGlacierFileBlock.LoadFromStream(aSourceStream: TStream);
begin
  SigCode := TGlacierSigCode(GS.Stream.ReadByte(aSourceStream));
  Assert(SigCode=TGlacierSigCode.gscFileBlock);
  BlockIndex := GS.Stream.ReadUINT32(aSourceStream);
  FileId := GS.Stream.ReadUINT64(aSourceStream);
  FileName := GS.Stream.ReadString(aSourceStream);
  DateTag.Time := GS.Stream.ReadInteger(aSourceStream);
  DateTag.Date := GS.Stream.ReadInteger(aSourceStream);
  OriginalFileSize := GS.Stream.ReadUINT64(aSourceStream);
  DateLastUpdateTag.Time := GS.Stream.ReadInteger(aSourceStream);
  DateLastUpdateTag.Date := GS.Stream.ReadInteger(aSourceStream);
  Version := GS.Stream.ReadUINT32(aSourceStream);
  FileCompleteBlockMap := GS.Stream.ReadArrayOfUINT32(aSourceStream);
  encrypted := GS.Stream.ReadBoolean(aSourceStream);
  compressed := GS.Stream.ReadBoolean(aSourceStream);
end;

procedure TGlacierFileBlock.SaveToStream(aTargetStream: TStream);
begin
  GS.Stream.WriteByte(aTargetStream, Byte(TGlacierSigCode.gscFileBlock));
  GS.Stream.WriteUInt32(aTargetStream, BlockIndex);
  GS.Stream.WriteUInt64(aTargetStream, FileId);
  GS.Stream.WriteString(aTargetStream,FileName);
  GS.Stream.WriteInteger(aTargetStream, DateTag.Time);
  GS.Stream.WriteInteger(aTargetStream, DateTag.Date);
  GS.Stream.WriteUInt64(aTargetStream, OriginalFileSize);
  GS.Stream.WriteInteger(aTargetStream, DateLastUpdateTag.Time);
  GS.Stream.WriteInteger(aTargetStream, DateLastUpdateTag.Date);
  GS.Stream.WriteUInt32(aTargetStream, Version);
  GS.Stream.WriteArrayOfUINT32(aTargetStream,FileCompleteBlockMap);
  GS.Stream.WriteBoolean(aTargetStream, encrypted);
  GS.Stream.WriteBoolean(aTargetStream, compressed);
end;

initialization
  GLB_GlacifyProtocols.RegisterProtocol(TGlacifyProtocolDefault);

end.

