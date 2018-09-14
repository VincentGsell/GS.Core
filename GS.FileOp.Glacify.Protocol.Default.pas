unit GS.FileOp.Glacify.Protocol.Default;

interface

uses Classes,
     SysUtils,
     GS.FileOp;


Const
  CST_SIG_GLACIER = 'GSGLACIER';
Type
  ///
  ///  Default FGlacier File structure is straighforwardly simple :
  ///  It conitnously write, as a diary (monitoring tail), and never
  ///  ever rewrite something.
  ///  In Each File block, we found whole FileBockMap which is consistency with the block.
  ///  than we can easely, by scanning, rebuild whole file of version, or fellow all verison file.
  ///

  TGlacierSigCode = (gscUnknown, gscHeader, gscFileBlock, gscDataBlock);
  TGlacierFileBlock  = packed Record
  Public
    SigCode : TGlacierSigCode;
    BlockIndex : UINT32;
    FileName : string;
    DateTag :  TTimeStamp;
    CRC32 : UINT32;
    Version : UINT32;
    FileCompleteBlockMap : Array of UINT32; //Index of Block.
    compressed : boolean;
    encrypted : boolean;
    versioned : boolean;
    //CompressedAlg : string; //Later.
    //CryptographicAlg  : String;
  End;
  TGlacierDataBlock  = packed Record
  Public
    SigCode : TGlacierSigCode;
    BlockIndex : UINT32;
    CRC32 : UINT32;
    DateTag :  TTimeStamp;
    DataLengh : UINT32;
    compressRatio : single;
    ///
    ///  Here raw data
    ///
  End;

  ///
  ///  Header of whole FGlacier File...
  ///
  TGlacierFileHeader = packed Record
  Public
    SigCode : TGlacierSigCode;
    Sig : String;
    Version : String;
    BlockCount : Uint32;
  End;

  //This object must not exposed : Internal use only.
  TGlacierFileObject = Class(TFileStream)
  protected
  public
    Header : TGlacierFileHeader;
    FileBlock : TGlacierFileBlock; //Current. (Available after a ReadFileBlock call)
    DataBlock : TGlacierDataBlock; //Current. (Available after a ReadDataBlock call)

    constructor Create(var aGlacifyTask : TGlacifyProcessInfo); Reintroduce;

    Procedure WriteHeader;
    Procedure WriteFileBlock(var al : TGlacierFileBlock);
    Procedure WriteDataBlock(var al : TGlacierDataBlock);

    Function ReadSignature : TGlacierSigCode;
    Procedure ReadHeader;    //MUST call ReadSignature before.
    Procedure ReadFileBlock; //MUST call ReadSignature before.
    Procedure ReadDataBlock; //MUST call ReadSignature before.


    Function FlushFileStructure : UTF8String;
  End;

  TGlacifyProtocolDefault = Class(TGlacifyProtocolItem)
  private
    Procedure Internal_InitNewGlacierFileHeader(var agfh : TGlacierFileHeader);
    Procedure Internal_InitNewGlacierFileBlock(var agfh : TGlacierFileBlock);
    Procedure Internal_InitNewGlacierDataBlock(var agfh : TGlacierDataBlock);
  protected
    FGlacier : TGlacierFileObject;
    function PushWholeNewFileToGlacier(var aGlacifyTask : TGlacifyProcessInfo) : boolean;
    Function IsFileExists(aFileName : String) : Boolean;

    Function FlushStructure : UTF8String; Override;
  public
    Procedure Init(var aGlacifyTask : TGlacifyProcessInfo); Override;
    Procedure Process(var aGlacifyTask : TGlacifyProcessInfo); Override;
    Procedure Finalize(var aGlacifyTask : TGlacifyProcessInfo); Override;
  end;

implementation

Uses uKBDynamic, mORMot, SynCommons;


{ TGlacifyProtocolDefault }

function TGlacifyProtocolDefault.FlushStructure: UTF8String;
begin
  Result := Inherited;
  if Assigned(FGlacier) then //You have to call init() before use.
    result := FGlacier.FlushFileStructure;
end;

procedure TGlacifyProtocolDefault.Internal_InitNewGlacierDataBlock(
  var agfh: TGlacierDataBlock);
begin
  agfh.SigCode := gscDataBlock;
  agfh.BlockIndex := 0;
  agfh.CRC32 := 0;
  //DateTag :  TTimeStamp;
  agfh.DataLengh := 0;
  agfh.compressRatio := 0.0;
end;

procedure TGlacifyProtocolDefault.Internal_InitNewGlacierFileBlock(
  var agfh: TGlacierFileBlock);
begin
  agfh.SigCode := gscFileBlock;
  agfh.BlockIndex := 0;
  agfh.FileName := '';
//  agfh.DateTag := 0.0;
  agfh.CRC32 := 0;
  agfh.Version := 1;
  agfh.compressed := false;
  agfh.encrypted := false;
  agfh.versioned := false;
  agfh.FileCompleteBlockMap := Nil;
end;

procedure TGlacifyProtocolDefault.Internal_InitNewGlacierFileHeader(
  var agfh: TGlacierFileHeader);
begin
  agfh.SigCode := gscHeader;
  agfh.Sig := CST_SIG_GLACIER;
  agfh.Version := '1';
  agfh.BlockCount := 0;
end;

function TGlacifyProtocolDefault.IsFileExists(aFileName: String): Boolean;
begin
  Result := False;
end;


Function TGlacifyProtocolDefault.PushWholeNewFileToGlacier(var aGlacifyTask: TGlacifyProcessInfo) : Boolean;
var lFileBlock : TGlacierFileBlock;
    lDataBlock : TGlacierDataBlock;
    lc : UInt32;
    lcb,I : integer;
    l,l2 : TMemoryStream;
    ls : UINT64;
    ldelta : UINT32;
    SBuf,TBuf : RawByteString;
    lratio : single;
begin
  Result := false;
  if IsFileExists(aGlacifyTask.inFilesource) then
    Exit;

  Internal_InitNewGlacierFileBlock(lFileBlock);
  lFileBlock.FileName := aGlacifyTask.inFilesource;
  lFileBlock.compressed := gpoCompress in aGlacifyTask.InOptions;
  lFileBlock.encrypted := gpoEncrypt in aGlacifyTask.InOptions;
  if (lFileBlock.encrypted) and (Length(aGlacifyTask.InEncryptPassword) = 0) then
  begin
    aGlacifyTask.OutOperationCode := TGlacifyOpCode.gocErrorEncryptionPasswordEmpty;
    Exit;
  end;
  lFileBlock.versioned := gpoVersioned in aGlacifyTask.InOptions;
  lFileBlock.DateTag := DateTimeToTimeStamp(Now);
  FGlacier.Header.BlockCount := FGlacier.Header.BlockCount + 1;
  lFileBlock.BlockIndex := FGlacier.Header.BlockCount;
  TGSBin.CalcCRC32(aGlacifyTask.inFilesource,lc,lcb);
  lFileBlock.CRC32 := lc;
  //File block sequence is previsible in this case.
  SetLength(lFileBlock.FileCompleteBlockMap, Round(aGlacifyTask.InternalFileSourceStreamObject.Size / CST_GLACIER_BLOCK));
  for I := 0 to Length(lFileBlock.FileCompleteBlockMap)-1 do
    lFileBlock.FileCompleteBlockMap[i] := FGlacier.Header.BlockCount + 1 + I;

  FGlacier.Position := FGlacier.Size;
  FGlacier.WriteFileBlock(lFileBlock);

  l := TMemoryStream.Create;
  l2 := TMemoryStream.Create;
  try
    aGlacifyTask.InternalFileSourceStreamObject.Position := 0;
    while aGlacifyTask.InternalFileSourceStreamObject.Position<aGlacifyTask.InternalFileSourceStreamObject.Size do
    begin
      ls := CST_GLACIER_BLOCK;
      lDelta := aGlacifyTask.InternalFileSourceStreamObject.Size - aGlacifyTask.InternalFileSourceStreamObject.Position;
      if lDelta<CST_GLACIER_BLOCK then
        ls := lDelta;

      l.Clear;
      l.CopyFrom(aGlacifyTask.InternalFileSourceStreamObject,ls);
      l.Position := 0;


      Internal_InitNewGlacierDataBlock(lDataBlock);
      FGlacier.Header.BlockCount := FGlacier.Header.BlockCount + 1;
      lDataBlock.BlockIndex := FGlacier.Header.BlockCount;
      TGSBin.CalcCRC32(TStream(l),lc);
      lDataBlock.CRC32 := lc;
      lDataBlock.DateTag := DateTimeToTimeStamp(Now);
      l.Position := 0;

      //Crypto
      if  gpoEncrypt in aGlacifyTask.InOptions then
      begin
        // if Assigned(onCrypto..) them FOnCrypto(l)
        //else
        // Default encryption. (xor here.)

        //TODO : Add compilation directive dor crypto system :
          //TODO : Add mORMot

        //Default.
        begin
          TGSBin.Crypto_EncryptDecryptXor(TStream(l),TStream(l2),aGlacifyTask.InEncryptPassword);
          l.LoadFromStream(l2);
          l2.Clear;
          l.Position := 0;
        end;
      end;

      //Compress
      lratio := 0.0;
      if  gpoCompress in aGlacifyTask.InOptions then
      begin
        // if Assigned(onCompress..) them FOnCompress(l)
        //else
        // Default compression (rle here.)
        //l2.Size := l.Size;

        //TODO : Add compilation directive dor compress system :
          //TODO : Add https://github.com/zedalaye/d7zip.git
          //TODO : Add mORMot



        begin
          SetLength(SBuf, l.Size);
          l.ReadBuffer(Pointer(SBuf)^,l.Size);
          //TBuf := AlgoSynLZ.Compress(SBuf);
          TBuf := AlgoDeflate.Compress(SBuf);
          if l.Size>0 then
            lratio := Length(TBuf) / l.Size;
          l.Clear;
          l.Write(pointer(TBuf)^,length(Tbuf));
          l.Position := 0;
        end;
      end;

      lDataBlock.DataLengh := l.Size; //Eventualy crypted and compressed.
      lDataBlock.compressRatio := lratio;


      FGlacier.WriteDataBlock(lDataBlock);
      FGlacier.CopyFrom(l,l.Size); //Data location is always immediately after the header.
    end;
  finally
    FreeAndNil(l);
    FreeAndNil(l2);
  end;

  //Update Block counter.
  FGlacier.WriteHeader; //Update counter.
  result := true;
end;

{ TGlacierFileObject }

constructor TGlacierFileObject.Create;
  Function SourceFileExists : Integer;
  begin
    result := fmCreate;
    if FileExists(aGlacifyTask.InGlacierName) then
      result := fmOpenReadWrite;
  end;

begin
  Inherited Create(aGlacifyTask.InGlacierName, SourceFileExists or fmExclusive);
end;

function TGlacierFileObject.FlushFileStructure: UTF8String;
  procedure outputln(aText : UTF8String);
  begin
    result := result+#13#10 + aText;
  end;

  var i : integer;
      Signature : TGlacierSigCode;
begin
  outputln('FGlacier file flush --------------');
  Position := 0;
  Try
    if ReadSignature = gscHeader then
      ReadHeader;
    outputln(Format('Header SigCode...."%d"',[Byte(Header.SigCode)]));
    outputln(Format('       Sig........"%s"',[Header.Sig]));
    outputln(Format('       SigCode...."%s"',[Header.Version]));
    outputln(Format('       BlockCount."%d"',[Header.BlockCount]));

    while not (Position=Size) do
    begin
      Case ReadSignature of
        gscUnknown, gscHeader : raise Exception.Create('Abnormal Signature found');
        gscFileBlock :
        begin
          ReadFileBlock;
          outputln(' File Block detected');
          outputln(Format('  SigCode......."%d"',[Byte(FileBlock.SigCode)]));
          outputln(Format('  BlockIndex...."%d"',[FileBlock.BlockIndex]));
          outputln(Format('  FileName......"%s"',[FileBlock.FileName]));
          outputln(Format('  compressed...."%d"',[Integer(FileBlock.compressed)]));
          outputln(Format('  crypted......."%d"',[Integer(FileBlock.encrypted)]));
          outputln(Format('  versioned....."%d"',[Integer(FileBlock.versioned)]));
          outputln(Format('  CRC32........."%d"',[FileBlock.CRC32]));
        end;
        gscDataBlock :
        begin
          ReadDataBlock;
          Position := Position + DataBlock.DataLengh; //Jump pure data.
          outputln(' Data Block detected');
          outputln(Format('  SigCode........"%d"',[Byte(DataBlock.SigCode)]));
          outputln(Format('  BlockIndex....."%d"',[DataBlock.BlockIndex]));
          outputln(Format('  CRC32.........."%d"',[DataBlock.CRC32]));
          outputln(Format('  DataLength....."%d"',[DataBlock.DataLengh]));
          outputln(Format('  CompressRatio.."%f"',[DataBlock.compressRatio]));
        end;
      End;

    end;

  Finally

  End;
  outputln('FGlacier end of flush --------------');
end;

procedure TGlacierFileObject.ReadDataBlock;
begin
  TKBDynamic.ReadFrom(Self,DataBlock,TypeInfo(TGlacierDataBlock))
end;

procedure TGlacierFileObject.ReadFileBlock;
begin
  TKBDynamic.ReadFrom(Self,FileBlock,TypeInfo(TGlacierFileBlock))
end;

procedure TGlacierFileObject.ReadHeader;
begin
  TKBDynamic.ReadFrom(Self,Header,TypeInfo(TGlacierFileHeader))
end;

function TGlacierFileObject.ReadSignature: TGlacierSigCode;
begin
  read(result,1);
end;

procedure TGlacierFileObject.WriteDataBlock(var al: TGlacierDataBlock);
var signature : TGlacierSigCode;
begin
  signature := gscDataBlock;
  Write(signature,1);
  TKBDynamic.WriteTo(Self,al,TypeInfo(TGlacierDataBlock));
end;

procedure TGlacierFileObject.WriteFileBlock(var al: TGlacierFileBlock);
var signature : TGlacierSigCode;
begin
  signature := gscFileBlock;
  Write(signature,1);
  TKBDynamic.WriteTo(Self,al,TypeInfo(TGlacierFileBlock));
end;

procedure TGlacierFileObject.WriteHeader;
var signature : TGlacierSigCode;
begin
  Position := 0;
  signature := gscHeader;
  Write(signature,1);
  TKBDynamic.WriteTo(Self,Header,TypeInfo(TGlacierFileHeader));
end;


///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------
///-----------------------------------------------------------------------------


Procedure TGlacifyProtocolDefault.Init(var aGlacifyTask: TGlacifyProcessInfo);

  Procedure InitGlacierFile;
  begin
    Internal_InitNewGlacierFileHeader(FGlacier.Header);
    FGlacier.WriteHeader;
  end;

  Function ReadGlacierHeader : Boolean;
  begin
    Result := false;
    if (FGlacier.ReadSignature = gscHeader) then
    begin
      FGlacier.ReadHeader;
      result := (FGlacier.Header.SigCode = gscHeader) And (FGlacier.Header.Sig = CST_SIG_GLACIER);
    end;
  end;

begin
  try
    FGlacier := TGlacierFileObject.Create(aGlacifyTask);
  Except
    aGlacifyTask.OutOperationCode := gocErrorFileSourceUnavailable;
  end;

  if FGlacier.Size = 0 then
  begin
    InitGlacierFile;
    if PushWholeNewFileToGlacier(aGlacifyTask) then
      aGlacifyTask.OutOperationCode := gocSuccess;
  end
  else
  begin
    FGlacier.Position := 0;
    if Not ReadGlacierHeader then
      aGlacifyTask.OutOperationCode := gocErrorGlacierRessourceUnreadable
    else
      aGlacifyTask.OutOperationCode := gocSuccess;
  end;
end;

Procedure TGlacifyProtocolDefault.Process(var aGlacifyTask: TGlacifyProcessInfo);
begin

end;

Procedure TGlacifyProtocolDefault.Finalize(var aGlacifyTask: TGlacifyProcessInfo);
begin
  if Assigned(FGlacier) then
    FreeAndNil(FGlacier);
end;




Initialization

  GLB_GlacifyProtocols.RegisterProtocol(TGlacifyProtocolDefault);

end.
