unit GS.fileFormat.Excel.TMSFlexCel;

interface

uses
  //Delphi
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  //Core
  GS.fileFormat.Excel,
  //TMS
  {$IF FMX.Types.FireMonkeyVersion >= 0} // if FireMonkey
  FMX.FlexCel.Core,
  {$ELSE} // its not FMX, so it must be VCL
  VCL.FlexCel.Core,
  {$ENDIF}
  FlexCel.Core,
  FlexCel.pdf,
  FlexCel.render,
  FlexCel.XlsAdapter;

type

  TExcelFileTMSFlexCel = class(TCustomExcelFile, IExcelFile)
  private
    function PrepareStringColTypeForXLS : TArray<TColumnImportType>;
    procedure internalPrepareAndConvertCSV(afileName : string);
  public
    XLS : TXlsFile;

    Constructor Create; Override;
    Destructor Destroy; override;

    //file
    procedure open(const fileName : String);

    //Sheets
    function sheetCount : integer;
    procedure sheetWorkOn(name : String); overload;
    procedure sheetWorkOn(tabIndex : uint32); overload;
    function sheetName :string;
    function cells(x,y : integer) : string;
    function colCount : integer;
    function rowCount : integer;

    //Export.
    function getExportEngine : IExcelExport;
  end;

  TExcelExportTMSFlexCel = class(TInterfacedObject, IExcelExport)
  private
    FInternal : TXLSFile;
  public
    Constructor Create(aFormat : TXlsFile);

    procedure exportAsCSV(xlsFile : string; sheetName :string; targetFileName : string);
    procedure exportAsPDF(xlsFile : string; sheetName :string; targetFileName : string);
  end;

implementation

{ TExcelFileTMSFlexCel }

function TExcelFileTMSFlexCel.cells(x, y : integer): string;
var a : TCellValue;
begin
  a := XLS.GetCellValue(y,x);
  if a.IsEmpty then
    result := ''
  else
    result := a.AsTValue.ToString;
end;

function TExcelFileTMSFlexCel.colCount: integer;
begin
  result := XLS.ColCount;
end;

constructor TExcelFileTMSFlexCel.create;
begin
  inherited Create;
  XLS := TXlsFile.Create;
end;

destructor TExcelFileTMSFlexCel.destroy;
begin
  FreeAndNil(XLS);
  inherited;
end;

function TExcelFileTMSFlexCel.getExportEngine: IExcelExport;
begin
  result := TExcelExportTMSFlexCel.Create(XLS);
end;

procedure TExcelFileTMSFlexCel.open(const fileName: String);
begin
  if ExtractFileExt(filename).ToLower = '.csv' then begin
    XLS.NewFile;
    //internalPrepareAndConvertCSV(fileName);
    //VGS : FlexCel is disapointed here : It is very difficult to make something simple.
    xls.Open(fileName, TFileFormats.Text,';', 1,1,nil,nil,TEncoding.Unicode,false);
  end
  else
    XLS.Open(filename);
end;

function GetEncodingFile(const filename : String;  var Size : integer) : TEncoding;
var
  Stream : TMemoryStream;
  Buffer: TBytes;
begin
  result := nil;
  Stream := TMemoryStream.Create;
  try
    Stream.LoadFromFile(filename);
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer, 0, Size);
    Size := TEncoding.GetBufferEncoding(Buffer, Result, TEncoding.Default);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TExcelFileTMSFlexCel.internalPrepareAndConvertCSV(afileName : string);
var ll, ls : TStringList;
    i, j, mCol, mRow: Integer;
    lt : String;
    lEncoding : TEncoding;
    lSize : integer;
begin
  assert(fileExists(afileName));
  lEncoding := GetEncodingFile(afileName, lSize);
  if lSize = 0 then begin //No BOM. Let search what is inside.
    //Test encoding
    try
      lt := TFile.ReadAllText(afileName,TEncoding.UTF8); //UTF8 no bom, the more probable.
    Except
      //TODO : Log a warning.
      //code page.
      lt := TFile.ReadAllText(afileName,lEncoding); //Assuming default encoding
      //That will work, but in case of wrong format, we Look if it is a no bomed-and-not-ansi enconded file (i.e. no encoging indicate)
      if (length(lt)>2) And (lt[2]=#0) then
      begin
        lt := TFile.ReadAllText(afileName,TEncoding.Unicode); //Process it as unicode, since UTF8 not worked : Last chance.
        //TODO : Log a warning.
      end;
    end;
  end
  else begin
    //BOM and encoding successfuly retrived.
    lt := TFile.ReadAllText(afileName,lEncoding); //Assuming detect encoding.
  end;

  ll := TStringList.Create;
  ls := TStringList.Create; ls.Delimiter := ';'; ls.StrictDelimiter := true;

  ll.Text := AnsiString(lt); //convert as ansi for faster processing.

  mrow := ll.Count;
  mcol := 0;
  //Inject into xls.
  for i := 0 to ll.Count-1 do begin
    ls.DelimitedText := ll[i];
    if ls.Count>mcol then
      mcol := ls.Count;
    for j := 0 to mcol-1 do
      if ls.Count>j then
        XLS.SetCellValue(i+1,j+1,ls[j]);
  end;
end;


function TExcelFileTMSFlexCel.PrepareStringColTypeForXLS: TArray<TColumnImportType>;
var
 ColTypes: TArray<TColumnImportType>;
 i: Int32;
begin
 SetLength(ColTypes, TFlxConsts.MaxColCount);
 for i := 0 to Length(ColTypes) - 1 do
   ColTypes[i] := TColumnImportType.Text;
end;

function TExcelFileTMSFlexCel.rowCount: integer;
begin
  result := XLS.RowCount;
end;

function TExcelFileTMSFlexCel.sheetName: string;
begin
  result := XLS.SheetName;
end;

function TExcelFileTMSFlexCel.sheetCount: integer;
begin
  result := XLS.SheetCount;
end;

procedure TExcelFileTMSFlexCel.sheetWorkOn(tabIndex: uint32);
begin
  XLS.ActiveSheet := tabIndex;
end;

procedure TExcelFileTMSFlexCel.sheetWorkOn(name: String);
var l : TStringList;
    i : integer;
begin
  l := TStringList.Create;
  try
    for i:= 0 to XLS.SheetCount-1 do begin
      XLS.ActiveSheet := i+1;
      l.add(XLS.SheetName);
    end;
    i := l.IndexOf(name);
    if (i>0) and (i<XLS.SheetCount) then
      XLS.ActiveSheet := i
    else
      raise Exception.Create(format('No sheets with "%s" name',[name]));
  finally
    FreeAndNil(l);
  end;
end;

{ TExcelExportTMSFlexCel }

constructor TExcelExportTMSFlexCel.Create(aFormat: TXlsFile);
begin
  FInternal := aFormat;
end;

procedure TExcelExportTMSFlexCel.exportAsCSV(xlsFile, sheetName,
  targetFileName: string);
begin
  //Export basically with tab seprator csv. If you want more customization, see https://support.tmssoftware.com/t/open-csv-text-file/3227/6
  FInternal.ActiveSheetByName := sheetName;
  FInternal.Save(targetFileName,TFileFormats.Text);
end;

procedure TExcelExportTMSFlexCel.exportAsPDF(xlsFile, sheetName,
  targetFileName: string);
var
  PdfStream: TFileStream;
  lpdf : TFlexCelPdfExport;
begin
  PdfStream := TFileStream.Create(targetFileName, fmCreate);
  lpdf := TFlexCelPdfExport.Create;
  try
    lpdf.Workbook := FInternal;
    lPdf.PageLayout := TPageLayout.Outlines;
    lpdf.Export(PdfStream);
  finally
    FreeAndNil(lpdf);
    FreeAndNil(PdfStream);
  end;
end;


initialization

registerImplementation(TExcelFileTMSFlexCel)

end.
