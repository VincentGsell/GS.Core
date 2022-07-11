unit GS.FileOp;

//#########################################################################
///
///
///
///
///
///
///
///
///
///


interface

Uses Classes,
     SysUtils,
     Math,
     Generics.Collections,
     GS.System.CPU
 {$IFDEF MSWINDOWS}
     ,Windows
 {$ENDIF}
     ;

Const
  CST_64K =  8192*16;
  CST_GLACIER_BLOCK = 1024; //*1024;
  CST_NO_STRUCT_SPECIFIED = 'No structure specified in this protocol.';


  CST_CRC32_TABLE: Array[0..255] of UInt32 =
     ($00000000, $77073096, $EE0E612C, $990951BA,
      $076DC419, $706AF48F, $E963A535, $9E6495A3,
      $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
      $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
      $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
      $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
      $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
      $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
      $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
      $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
      $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
      $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
      $26D930AC, $51DE003A, $C8D75180, $BFD06116,
      $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
      $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
      $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
      $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
      $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
      $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
      $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
      $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
      $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
      $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
      $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
      $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
      $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
      $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
      $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
      $5005713C, $270241AA, $BE0B1010, $C90C2086,
      $5768B525, $206F85B3, $B966D409, $CE61E49F,
      $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
      $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
      $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
      $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
      $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
      $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
      $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
      $F762575D, $806567CB, $196C3671, $6E6B06E7,
      $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
      $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
      $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
      $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
      $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
      $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
      $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
      $CC0C7795, $BB0B4703, $220216B9, $5505262F,
      $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
      $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
      $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
      $9C0906A9, $EB0E363F, $72076785, $05005713,
      $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
      $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
      $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
      $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
      $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
      $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
      $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
      $A7672661, $D06016F7, $4969474D, $3E6E77DB,
      $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
      $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
      $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
      $BAD03605, $CDD70693, $54DE5729, $23D967BF,
      $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
      $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);



Type
  TGSBinByteArray = array[1..CST_64K] of Byte;
  ///  -------------------------------------------------------------------------
  ///  File copy structures
  ///  -------------------------------------------------------------------------
  TCopyFileProcessOpCode = ( cfpInit,
                             cfpErrorFileSourceNotFound,
                             cfpErrorFileSourceUnavailable, //IOResult error.
                             cfpDestinationFileAlreadyExists,
                             cfpErrorFileDestinationUnabledToCreate,
                             cfpErrorExistingFileDestinationUnableToDelete,
                             cfpInProgress,
                             cfpSuccess,
                             cfpErrorCrc32NotMatch);

  TCopyFileProcessOption = (cfpoCheckCRC32, cfpoFailIfDestinationFileExists);
  TCopyFileProcessOptions = set of TCopyFileProcessOption;


  TCopyFileProcessInfo = Record
    InFileSource,
    InFileDest : String;
    InFileCopyBehaviour : TCopyFileProcessOptions;
    OutInFileDestWritten : UInt64;
    OutInFileSourceRead : UInt64;
    OutFileSourceSize : UInt64;
    OutFileSourceCRC32 : UInt32;
    OutFileDestCRC32 : UInt32;
    OutPercentDone : Single;
    OutOperationCode : TCopyFileProcessOpCode;
    OutStats_TimeTakenForOp : UInt64;

    InternalBuf: TGSBinByteArray;
    InternalFileSourceStreamObject, InternaFileDestStreamObject : TFileStream;
  End;


  ///  -------------------------------------------------------------------------
  ///  Glacify structures
  ///  -------------------------------------------------------------------------
  TGlacifyProtocolItem = Class;

  TGlacifyOpCode = ( gocInit,
                     gocProcess,
                     gocSuccess,
                     gocErrorFileSourceNotFound,
                     gocErrorFileSourceUnavailable,
                     gocErrorGlacierProtocolUnavailable,
                     gocErrorGlacierRessourceUnavailable,
                     gocErrorGlacierRessourceUnreadable,
                     gocErrorEncryptionPasswordEmpty
                   );
  TGlacifyProcessOption = (gpoEncrypt, gpoCompress);
  TGlacifyProcessOptions = Set of TGlacifyProcessOption;

  TGlacifyProgress = Record
    Filesource : UTF8String;
    FileNameInGlacier : UTF8String;
    GLacierName : UTF8String;
    CurrentOperationCode : TGlacifyOpCode;
    Info : UTF8String;
    PercentDone : Single;
  end;

  TGlacifyProcessInfoCallBack = Procedure(var aGlacifyTask : TGlacifyProgress);
  TGlacifyProcessInfo = Record
    inFilesource,
    InGlacierName : String;
    InOptions : TGlacifyProcessOptions;
    InEncryptPassword : String; //If encryption activate.

    OutOperationCode : TGlacifyOpCode;
    OutPercentDone : Single;
    OutFileSourceSize : UInt64;
    OutFileSourceCRC32 : UInt32;
    OutStats_TimeTakenForOp : UInt64;
    OutException : Boolean;
    OutExceptionString : String;
    //OutExceptionObj ? Mem management ?

    InternalProtocol : TGlacifyProtocolItem;
    OutCallBackProc : TGlacifyProcessInfoCallBack;
  end;

  ///
  ///  FileInformation API
  ///
  ///
  TBlockInformationStructure = packed record
    blockindex : Uint32;
    blockCRC32 : Uint32;
    blockSize : Uint32;
  end;
  TFileInformationStructure = packed record
    fileName : string;
    fileSize : UInt64;
    fileversion : UINT32; //Glacier only.
    fileLastModification : TTimeStamp;
    fileBlock : Array of TBlockInformationStructure;
  end;
  TFileInfo = Packed Record
    FileId: Uint64;
    FileSize: UInt64;
    LastWriteAccess: TTimeStamp;
    LastReadAccess: TTimeStamp;
    FileCreateDateTime: TTimeStamp;
  end;

  TFileItem = packed record //Mirror record of default glacify protocol. To see it it must be shared here.
    BlockIndex: UINT32;
    FileName: string;
    DateTag: TTimeStamp;
    OriginalFileSize : UINT64;
    DateLastUpdateTag: TTimeStamp;
    Version: UINT32;
    FileBlockMapCount : UInt32;
    compressed: boolean;
    encrypted: boolean;
  end;
  TFilesList = array of TFileItem;

  TFileDiff = packed record
    BinarySame : Boolean;
    FileBlockMapDiffersIndex : Array of UInt32;
  end;

  TGSBin = class
  public
    Class Function CalcCRC32(FileName: String; var CRC32: UInt32; Out aIOResult : integer) : boolean; Overload;
    Class Procedure CalcCRC32(aStream: TStream; var CRC32: UInt32); Overload;

    //To be called in a loop until result is True. True means copy process is terminated, even if copy file fail : Check TCopyFileProcessInfo to know more about copy status.
    //TCopyFileProcessInfo "out var, as "PercentDone" and "Operation", change on the fly and can be use for show progression.
    Class Function CopyFileProcess(var aCopyTask : TCopyFileProcessInfo) : Boolean;

    ///  Drive "CopyFileProcess" (synchrone procedure). WARNING : It delete destination file if it exists !
    ///  Result := true means copy is successfull.
    ///  This mnethods is above all an exemple of use : Please see in the nethods.
    class Function CopyFile(const aSourceFile, aDestFile : String) : Boolean;

    ///  Simple and easy to use Xor encryption/decyption : Huger password is, better it is.
    ///  Perhaps one of the best encryption if we consider the simplicity of use and number of dependancy ;)
    Class Procedure Crypto_EncryptDecryptXor(var aSourceStream: TStream; aTargetStream : TStream; const Password : String = '');

    ///  Make a block-versioned backup of a file.
    Class Function GlacifyFileProcess(var aGlacifyTask : TGlacifyProcessInfo) : Boolean;
    Class Function GlacifyFile( Const aSourceFile : String;
                                Const aGlacierName : String;
                                Const aCompressed : Boolean = True;
                                Const aEncrypted : Boolean = False;
                                aEncryptionPassword : String = '') : Boolean;
    ///
    ///  Return Glacier structure in plain text.
    class Function GlacifyAnalytic_flush(const aGlacierName : String) : String;
    ///
    ///  Return file list
    ///  Return True if a valid TfileList is returned in aFileList var.
    ///  This structure contain files currently owned by the glacier.
    class Function GlacifyAnalytic_FileList(const aGlacierName : String; Out aFileList : TFilesList) : Boolean;

    ///
    ///  Return True if a valid TfileInformationStruture is returned in aFileInformation var.
    ///  This structure contain a Glacier compatible structure from a file from the file system.
    class Function GlacifyAnalytic_FileSystem_FileInformation(const aFileName : String; Out aFileInformation : TFileInformationStructure) : Boolean;
    ///
    ///  Return True if a valid TfileInformationStruture is returned in aFileInformation var.
    ///  This structure contain a Glacier compatible structure from a file from the glacier.
    class Function GlacifyAnalytic_FileInformation(Const aGlacierName : String; const aFileName : String; Out aFileInformation : TFileInformationStructure) : Boolean;
    ///
    ///  Return True means if the 2 file are binary same.
    ///  the first file is the file in the glacier, and the second the file on file system host.
    ///  this entry use GlacifyAnalytic_[...]FileInformation API.
    class Function GlacifyAnalytic_Compare(const aGlacierName : String; const aFileNameInGalcier, aFileNameInFileSystem : string) : Boolean;


//    Class Function GlacierInfoAsCSV(const aglacierName : string; out aCSV : TStringList) : Boolean;

    class Function FileInfo(aFileHandle: THandle; Out aFileInfo : TFileInfo) : Boolean;
  end;

  TGlacifyProtocolItem = Class
  public
    Procedure Init(var aGlacifyTask : TGlacifyProcessInfo); Virtual; Abstract;
    Procedure Process(var aGlacifyTask : TGlacifyProcessInfo); Virtual; Abstract;
    Procedure Finalize(var aGlacifyTask : TGlacifyProcessInfo); Virtual; Abstract;

    //Too and admin op.
    Function Glacier_FlushStructure : UTF8String; Virtual;

    //Utility
    Function Glacier_IsFileExists(Const aFileName : String) : Boolean; Virtual; Abstract;
    Function Glacier_FileInformations(Const aFileName : String; var aFileInformation : TFileInformationStructure) : Boolean; virtual; abstract;
    Function Glacier_FileList(var aFlileList : TFilesList) : Boolean; virtual; abstract;
    function Glacier_FileCompare( const FileNameFileSystem : String;
                                  const FileNameGlacier : String;
                                  out BlockCountDiffers : UInt32; //differs, or newer.
                                  var FileSystemInfo : TFileInformationStructure;
                                  var FileGlacierInfo : TFileInformationStructure) : Boolean; Virtual; Abstract;
  End;

  TGlacifyProtocolItemClass = Class of TGlacifyProtocolItem;
  TGlacifyProtocol = Class(TList<TGlacifyProtocolItemClass>)
    Procedure RegisterProtocol(aProtocolClass : TGlacifyProtocolItemClass);
    Function DefaultProtocol : TGlacifyProtocolItemClass;
  End;


  procedure InitCopyFileProcessInfo(const aSourceFile, aDestFile : String; var aCopyFile : TCopyFileProcessInfo);
  procedure InitGlacifyProcessInfo(const aSourceFile, aGlacierName, aEncryptionPassword : String; var aGlacify : TGlacifyProcessInfo);
  procedure InitFileInformationStructure(const aSourceFile : String; var aStructure : TFileInformationStructure);


var
  GLB_GlacifyProtocols : TGlacifyProtocol;

implementation

Procedure InitCopyFileProcessInfo(const aSourceFile, aDestFile : String; var aCopyFile : TCopyFileProcessInfo);
begin
  aCopyFile.InFileSource := aSourceFile;
  aCopyFile.InFileDest := aDestFile;
  aCopyFile.InFileCopyBehaviour := [cfpoFailIfDestinationFileExists];
  aCopyFile.OutInFileDestWritten := 0;
  aCopyFile.OutInFileSourceRead := 0;
  aCopyFile.OutFileSourceSize := 0;
  aCopyFile.OutFileSourceCRC32 := 0;
  aCopyFile.OutFileDestCRC32 := 0;
  aCopyFile.OutPercentDone := 0.0;
  aCopyFile.OutOperationCode := cfpInit;
  aCopyFile.OutStats_TimeTakenForOp := 0;
end;

Procedure InitGlacifyProcessInfo(const aSourceFile, aGlacierName, aEncryptionPassword : String; var aGlacify : TGlacifyProcessInfo);
begin
  aGlacify.inFilesource := aSourceFile;
  aGlacify.InGlacierName := aGlacierName;
  aGlacify.InOptions := [];
  aGlacify.OutOperationCode := gocInit;
  aGlacify.OutPercentDone := 0.0;
  aGlacify.OutFileSourceSize := 0;
  aGlacify.OutStats_TimeTakenForOp := 0;
  aGlacify.InEncryptPassword := aEncryptionPassword;
  aGlacify.InternalProtocol := nil;
end;

procedure InitFileInformationStructure(const aSourceFile : String; var aStructure : TFileInformationStructure);
begin
  aStructure.fileName := aSourceFile;
  aStructure.fileSize := 0;
  aStructure.fileLastModification := DateTimeToTimeStamp(Now);
  aStructure.fileBlock := [];
end;



Class function TGSBin.CalcCRC32(FileName: String; var CRC32: UInt32; Out aIOResult : integer) : boolean;
var F: file;
    BytesRead: UInt32;
    Buffer: Array[1..65521] of byte;
    i: Word;
begin
  FileMode := 0;
  CRC32 := $ffffffff;
  {$I-}
  AssignFile(F, FileName);
  Reset(F, 1);
  aIOResult := IOResult;
  if aIOResult = 0 then
  begin
    repeat
      BlockRead(F, Buffer, SizeOf(Buffer), BytesRead);
      for i := 1 to BytesRead do
      begin
        CRC32 := (CRC32 shr 8) xor CST_CRC32_TABLE[Buffer[i] xor (CRC32 and $000000FF)];
      end;
    until BytesRead = 0;
  end;
  CloseFile(F);
  {$I+}
  CRC32 := not CRC32;
  Result := aIOResult = 0;
end;


class Procedure TGSBin.CalcCRC32(aStream: TStream; var CRC32: UInt32);
var i: Word;
    lBuffer: Array[1..65521] of byte;
    lBytesRead: UInt32;
begin
  Assert(assigned(aStream));
  CRC32 := $ffffffff;
  if aStream.Size=0 then
    Exit;
  repeat
    lBytesRead := 0;
    if aStream.Size>65521 then
      lBytesRead := 65521
    else
      lBytesRead := aStream.Size;

    aStream.Read(lBuffer,lBytesRead);
    for i := 1 to lBytesRead do
    begin
      CRC32 := (CRC32 shr 8) xor CST_CRC32_TABLE[lBuffer[i] xor (CRC32 and $000000FF)];
    end;
  until aStream.Position = aStream.Size;
  CRC32 := not CRC32;
end;

class function TGSBin.CopyFile(const aSourceFile, aDestFile: String): Boolean;
var lc : TCopyFileProcessInfo;
begin
  InitCopyFileProcessInfo(aSourceFile,aDestFile,lc); //Setup of Structure (Mandatory !)
  lc.InFileCopyBehaviour := [];                      //Delete destination file if exits, no CRC32 Check.
  while Not TGSBin.CopyFileProcess(lc) do            //Loop unit true (true = copy finished)
    //If you done our own, here you can update GUI or whatever. (lc.OutPercentdone is update)
  begin end;
  result := lc.OutOperationCode = cfpSuccess;
end;

Class Function TGSBin.CopyFileProcess(Var aCopyTask : TCopyFileProcessInfo) : Boolean;
var
  Numcpy: Integer;
  lIO : Integer;
  lfd : Boolean;

  Procedure Initcopy;
  begin
    lfd := FileExists(aCopyTask.InFileDest);
    if Not FileExists(aCopyTask.InFileSource) then
      aCopyTask.OutOperationCode := cfpErrorFileSourceNotFound;
    if (cfpoFailIfDestinationFileExists in aCopyTask.InFileCopyBehaviour) and (lfd) then
      aCopyTask.OutOperationCode := cfpDestinationFileAlreadyExists;

    if aCopyTask.outOperationCode <> cfpInit then
      Exit;

    if lfd then
      if not SysUtils.DeleteFile(aCopyTask.InFileDest) then
         aCopyTask.OutOperationCode := cfpErrorExistingFileDestinationUnableToDelete;

    if aCopyTask.outOperationCode <> cfpInit then
      Exit;

    //Determine file source CRC for check at end of copy.
    if cfpoCheckCRC32 in aCopyTask.InFileCopyBehaviour then
    begin
      If not TGSBin.CalcCRC32(aCopyTask.InFileSource,aCopyTask.OutFileSourceCRC32,lIo) then
        aCopyTask.OutOperationCode := cfpErrorFileSourceUnavailable;
    end;

    if aCopyTask.outOperationCode <> cfpInit then
      Exit;

    try
      aCopyTask.InternalFileSourceStreamObject := TFileStream.Create(aCopyTask.InFileSource, fmOpenRead or fmShareDenyNone);
      aCopyTask.OutFileSourceSize := aCopyTask.InternalFileSourceStreamObject.Size;

      aCopyTask.InternaFileDestStreamObject := TFileStream.Create(aCopyTask.InFileDest, fmCreate or fmOpenWrite or fmExclusive);

      if aCopyTask.OutFileSourceSize = 0 then
        aCopyTask.OutOperationCode := cfpSuccess //File empty copy.
      else
        aCopyTask.OutOperationCode := cfpInProgress;
    Except
     aCopyTask.OutOperationCode := cfpErrorFileDestinationUnabledToCreate;
    end;
  end;

  Procedure Progress;
  begin
    //Writeln('Copying ', FileSize(FromF), ' bytes...');
    //repeat
    numCpy := aCopyTask.InternalFileSourceStreamObject.Read(aCopyTask.InternalBuf,SizeOf(aCopyTask.InternalBuf));
    aCopyTask.InternaFileDestStreamObject.Write(aCopyTask.InternalBuf,Numcpy);

    aCopyTask.OutInFileSourceRead := aCopyTask.OutInFileSourceRead + Numcpy;
    aCopyTask.OutInFileDestWritten := aCopyTask.OutInFileDestWritten + Numcpy;

    aCopyTask.outPercentDone := aCopyTask.OutInFileDestWritten * 100 / aCopyTask.OutFileSourceSize;

    if (Numcpy = 0) or (aCopyTask.OutFileSourceSize = aCopyTask.OutInFileDestWritten) then
      aCopyTask.OutOperationCode := cfpSuccess;
    //Until (NumRead = 0) or (NumWritten <> NumRead).
  end;

  Procedure TerminateCopy;
  var lio : Integer;
  begin
    FreeAndNil(aCopyTask.InternalFileSourceStreamObject);
    FreeAndNil(aCopyTask.InternaFileDestStreamObject);

    //Check result (crc evaluate.) TODO : Perhaps compare size before ?
    if cfpoCheckCRC32 in aCopyTask.InFileCopyBehaviour then
    begin
      TGSBin.CalcCRC32(aCopyTask.InFileDest,aCopyTask.OutFileDestCRC32,lio); //not tested, if its fail the test bellow will fail.
      if aCopyTask.OutFileDestCRC32 <> aCopyTask.OutFileSourceCRC32 then
        aCopyTask.OutOperationCode := cfpErrorCrc32NotMatch;
    end;

    Result := True; //Process is TERMINATED, event copy is unsuccessfull.
  end;

var t : UInt64;
begin
  Result := false;
  t := gsGetTickCount;
  case aCopyTask.OutOperationCode of
    cfpInit :
    begin
      InitCopy;
    end;
    cfpInProgress :
    begin
      Progress;
    end
    else
    begin
      TerminateCopy;
    end;
  end;
  aCopyTask.OutStats_TimeTakenForOp := aCopyTask.OutStats_TimeTakenForOp + (gsGetTickCount - t);
end;

class procedure TGSBin.Crypto_EncryptDecryptXor(var aSourceStream: TStream; aTargetStream : TStream;
  const Password: String);
const
  CST_PWD = 'http://forum.lazarus.freepascal.org/index.php/topic,33013.msg213192.html#msg213192';
var
  i,l,len: integer;
  p : String;
  b : Byte;
  BufS,BufT : Array[1..CST_64K] of Byte;
  lBytesRead : Integer;
begin
  Assert(Assigned(aSourceStream));
  Assert(Assigned(aTargetStream));

  if  aSourceStream.Size=0 then
    Exit;

  p := trim(Password);
  if p = '' then
    p := CST_PWD;

  len := Length(p);

  aTargetStream.Size := aSourceStream.Size;
  aSourceStream.Position := 0;
  aTargetStream.Position := 0;
  repeat
    lBytesRead := aSourceStream.Read(BufS,CST_64K);
    l := 1;
    for i := 1 to lBytesRead do
    begin
      BufT[i] := BufS[i] xor Ord(p[l]);
      l := l + 1;
      if l>len then
        l := 0;
    end;
    aTargetStream.Write(Buft,lBytesRead);
  until aSourceStream.Position = aSourceStream.Size;
end;



class function TGSBin.FileInfo(aFileHandle: THandle; Out aFileInfo : TFileInfo) : boolean;

Type
  Int64Rec = packed record
      Lo, Hi: UInt32;
  end;
  PInt64Rec = ^Int64Rec;

var
 ld : _SYSTEMTIME;
 lFileSize : UInt64;
 lFileId : Uint64;
 {$IFDEF MSWINDOWS}
 lp : TByHandleFileInformation;
 {$ELSE}
 lp : stat;
 r : integer;
 {$ENDIF MSWINDOWS}
begin
 {$IFDEF MSWINDOWS}
 result := GetFileInformationByHandle(aFileHandle,lp);
 if result then begin
   FileTimeToSystemTime(lp.ftLastWriteTime, ld);
   aFileInfo.LastWriteAccess  :=  DateTimeToTimeStamp(SystemTimeToDateTime(ld));
   FileTimeToSystemTime(lp.ftCreationTime,ld);
   aFileInfo.FileCreateDateTime  := DateTimeToTimeStamp(SystemTimeToDateTime(ld));
   FileTimeToSystemTime(lp.ftLastAccessTime,ld);
   aFileInfo.LastReadAccess :=DateTimeToTimeStamp(SystemTimeToDateTime(ld));
   PInt64Rec(@lFileSize).lo := lp.nFileSizeLow;
   PInt64Rec(@lFileSize).hi := lp.nFileSizeHigh;
   PInt64Rec(@lFileId).lo := lp.nFileIndexLow;
   PInt64Rec(@lFileId).hi := lp.nFileIndexHigh;
   aFileInfo.FileId := lFileId;
   aFileInfo.FileSize := lFileSize;
 {$ELSE}
   r := FpFStat(aFileHandle, lp);
   result := r >= 0;
   if result then
   begin
     FileId := lp.st_ino;
     FileSize := lp.st_size;
     lastreadaccess := lp.st_atime * MSecsPerSec;
     LastWriteAccess := lp.st_mtime * MSecsPerSec;
     FileCreateDateTime := lp.st_ctime * MSecsPerSec;
 {$ENDIF MSWINDOWS}
 end;
end;

class function TGSBin.GlacifyAnalytic_Compare(const aGlacierName,
  aFileNameInGalcier, aFileNameInFileSystem: string): Boolean;
var l : TGlacifyProtocolItem;
    lfg : TGlacifyProcessInfo;
    lblockDiff : UInt32;
    la,lb :  TFileInformationStructure;
begin
  result := false;
  l := GLB_GlacifyProtocols.DefaultProtocol.Create;
  try
    InitGlacifyProcessInfo(EmptyStr, aGlacierName,EmptyStr,lfg);
    l.Init(lfg);
    if lfg.OutOperationCode = gocInit then
      if l.Glacier_FileCompare(aFileNameInFileSystem, aFileNameInGalcier, lblockDiff,la,lb) then
      begin
        result := lblockDiff = 0;
      end;
  finally
    l.Finalize(lfg);
    FreeAndNil(l);
  end;
end;

class function TGSBin.GlacifyAnalytic_FileInformation( const aGlacierName : String;
                                                       const aFileName: String;
                                                       out aFileInformation: TFileInformationStructure): Boolean;
var l : TGlacifyProtocolItem;
    lfg : TGlacifyProcessInfo;
begin
  result := false;
  l := GLB_GlacifyProtocols.DefaultProtocol.Create;
  try
    InitGlacifyProcessInfo(EmptyStr, aGlacierName,EmptyStr,lfg);
    l.Init(lfg);
    if lfg.OutOperationCode = gocInit then
      result := l.Glacier_FileInformations(aFileName,aFileInformation);
  finally
    l.Finalize(lfg);
    FreeAndNil(l);
  end;
end;

class function TGSBin.GlacifyAnalytic_FileSystem_FileInformation(
  const aFileName: String;
  out aFileInformation: TFileInformationStructure): Boolean;

var lfl : TFileStream;
    lDummyUint32 : UInt32;
    lfi : TFileInfo;
    ls: UINT64;
    ldelta: UINT32;
    lbIndex : UINT32;
    lc : UINT32;
    l : TMemoryStream;
    lbl : UINT32;
begin
  Result := false;
  InitFileInformationStructure(aFileName, aFileInformation);
  if not FileExists(aFileName) then
    Exit;
  lfl := TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  l := TMemoryStream.Create;
  try
    aFileInformation.fileSize := lfl.Size;
    aFileInformation.fileLastModification := DateTimeToTimeStamp(Now);
    aFileInformation.fileversion := 0; //No means here. (Only in glacier).

    if Not TGSBin.FileInfo(lfl.Handle, lfi) then
      Exit;

    aFileInformation.fileSize := lfi.FileSize;
    aFileInformation.fileLastModification := lfi.LastWriteAccess;

    lbl := lfl.Size div CST_GLACIER_BLOCK;
    if lfl.Size mod CST_GLACIER_BLOCK > 0 then
      inc(lbl);
    SetLength(aFileInformation.fileBlock, lbl);

    lfl.Position := 0;
    lbIndex := 0;
    while Not(lfl.Position = lfl.Size) do
    begin
      ls := CST_GLACIER_BLOCK;
        ldelta := lfl.Size - lfl.Position;
        if ldelta < CST_GLACIER_BLOCK then
          ls := ldelta;

      l.Clear;
      l.CopyFrom(lfl, ls);
      l.Position := 0;
      TGSBin.CalcCRC32(TStream(l), lc);

      aFileInformation.fileBlock[lbIndex].blockindex := lbIndex+1;
      aFileInformation.fileBlock[lbIndex].blockSize := ls;
      aFileInformation.fileBlock[lbIndex].blockCRC32 := lc;
      inc(lbIndex);
    end;
  finally
    FreeAndNil(l);
    FreeAndNil(lfl);
  end;
  Result := true;
end;

class function TGSBin.GlacifyAnalytic_flush(const aGlacierName: String): String;
var l : TGlacifyProtocolItem;
    lfg : TGlacifyProcessInfo;
begin
  l := GLB_GlacifyProtocols.DefaultProtocol.Create;
  try
    InitGlacifyProcessInfo(EmptyStr, aGlacierName,EmptyStr,lfg);
    l.Init(lfg);
    if lfg.OutOperationCode = gocInit then
      result := l.Glacier_FlushStructure
    else
      result := l.ClassName + ' - Structure analyze failure.';
  finally
    l.Finalize(lfg);
    FreeAndNil(l);
  end;
end;


class function TGSBin.GlacifyAnalytic_FileList(const aGlacierName: String;
  out aFileList: TFilesList): Boolean;
var l : TGlacifyProtocolItem;
    lfg : TGlacifyProcessInfo;
begin
  result := false;
  l := GLB_GlacifyProtocols.DefaultProtocol.Create;
  try
    InitGlacifyProcessInfo(EmptyStr, aGlacierName,EmptyStr,lfg);
    l.Init(lfg);
    if lfg.OutOperationCode = gocInit then
      result := l.Glacier_FileList(aFileList);
  finally
    l.Finalize(lfg);
    FreeAndNil(l);
  end;
end;

class function TGSBin.GlacifyFile( Const aSourceFile : String;
                                Const aGlacierName : String;
                                Const aCompressed : Boolean = True;
                                Const aEncrypted : Boolean = False;
                                aEncryptionPassword : String = ''): Boolean;

Procedure callback(var aGlacifyTask : TGlacifyProcessInfo);
begin
  //Just if you need gui update. Reproduce this one into the gui.
  //Use for that GlacifyFileProcess.
end;

var lc : TGlacifyProcessInfo;
begin
  InitGlacifyProcessInfo(aSourceFile,aGlacierName,aEncryptionPassword ,lc); //Setup of Structure (Mandatory !)
  lc.InOptions := [];
  lc.OutCallBackProc := @callback;
  if aCompressed then
    lc.InOptions := lc.InOptions + [gpoCompress];
  if aEncrypted then
    lc.InOptions := lc.InOptions + [gpoEncrypt];
  while Not TGSBin.GlacifyFileProcess(lc) do            //Loop unit true (true = glacification finished)
    //If you done our own, here you can update GUI or whatever. (lc.OutPercentdone is update)
  begin end;
  result := lc.OutOperationCode = gocSuccess;
end;

class function TGSBin.GlacifyFileProcess(
  var aGlacifyTask: TGlacifyProcessInfo): Boolean;
var t : UInt64;

  Procedure InitGlacify;
  begin
    if Not FileExists(aGlacifyTask.InFileSource) then
      aGlacifyTask.OutOperationCode := gocErrorFileSourceNotFound;
    if aGlacifyTask.outOperationCode <> gocInit then
      Exit;

    //Get Glacify protocol. Todo : Choose one by parameter ?
    aGlacifyTask.InternalProtocol := GLB_GlacifyProtocols.DefaultProtocol.Create; //For instance.

    if Not Assigned(aGlacifyTask.InternalProtocol) then
    begin
      aGlacifyTask.OutOperationCode := gocErrorGlacierProtocolUnavailable;
      raise Exception.Create('No protocol found');
    end;

    //Execute protocol.
    aGlacifyTask.InternalProtocol.Init(aGlacifyTask);
  end;

  Procedure ProgressGlacify;
  begin
    //Here build a File source map, pass it in aGlacifytask :
    // Map :
    // FileNameAndPath
    // Size
    // LastModifyDate
    // CRC32 (field not initilized, will be done by protocol if needed.)
    //
    aGlacifyTask.InternalProtocol.Process(aGlacifyTask);
  end;

  Procedure TerminateGlacify;
  begin
    if Assigned(aGlacifyTask.InternalProtocol) then
    begin
      aGlacifyTask.InternalProtocol.Finalize(aGlacifyTask);
      FreeAndNil(aGlacifyTask.InternalProtocol);
    end;
    Result := True;
  end;
begin
  Result := false;
  try
    try
      t := gsGetTickCount;
      case aGlacifyTask.OutOperationCode of
        gocInit :
        begin
          Initglacify;
        end;
        gocProcess :
        begin
          ProgressGlacify;
        end
        else
        begin
          TerminateGlacify;
        end;
      end;
    Except
      On E : Exception do
      begin
        if Assigned(aGlacifyTask.InternalProtocol) then
          TerminateGlacify;
        aGlacifyTask.OutException := True;
        aGlacifyTask.OutExceptionString := E.Message;
        //Add stack ? Add E ?
        Raise
      end;
    end;
  finally
    aGlacifyTask.OutStats_TimeTakenForOp := aGlacifyTask.OutStats_TimeTakenForOp + (gsGetTickCount - t);
  end;
end;




{ TGlacifyProtocol }

function TGlacifyProtocol.DefaultProtocol: TGlacifyProtocolItemClass;
begin
  Result := Nil;
  if Count>0 then
    Result := Items[0];
end;

procedure TGlacifyProtocol.RegisterProtocol(aProtocolClass : TGlacifyProtocolItemClass);
begin
  if IndexOf(aProtocolClass)<0 then
    Add(aProtocolClass);
end;

{ TGlacifyProtocolItem }

function TGlacifyProtocolItem.Glacier_FlushStructure: UTF8String;
begin
  Result := CST_NO_STRUCT_SPECIFIED;
end;

Initialization

GLB_GlacifyProtocols := TGlacifyProtocol.Create;

Finalization

FreeAndNil(GLB_GlacifyProtocols);

end.

