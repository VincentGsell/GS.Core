unit GS.fileFormat.Excel.SpreadSheet;
///
/// https://github.com/sergio-hcsoft/Delphi-SpreadSheets
///
///

interface

TO FINISH !!

uses
  //Delphi
  System.SysUtils,
  System.Classes,
  //GS Excel interface
  GS.fileFormat.Excel,
  //SpreadShieet
  UHojaCalc;

type

  TExcelFileSpreadSheet = class(TCustomExcelFile, IExcelFile)
  private
  public
    XLS : TZWorkBook;

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

  TExcelExportSpreadSheet = class(TInterfacedObject, IExcelExport)
  private
  public
    Constructor Create(aFormat : TZWorkBook);

    procedure exportAsCSV(xlsFile : string; sheetName :string; targetFileName : string);
  end;

implementation

{ TExcelFileTMSFlexCel }

function TExcelFileSpreadSheet.cells(x, y : integer): string;
var a : TZCellValue;
begin
  a := XLS.GetCellValue(y,x);
  if a.IsEmpty then
    result := ''
  else
    result := a.AsTValue.ToString;
end;

function TExcelFileSpreadSheet.colCount: integer;
begin
  result := XLS.ColCount;
end;

constructor TExcelFileSpreadSheet.create;
begin
  inherited Create;
  XLS := TXlsFile.Create;
end;

destructor TExcelFileSpreadSheet.destroy;
begin
  FreeAndNil(XLS);
  inherited;
end;

function TExcelFileSpreadSheet.getExportEngine: IExcelExport;
begin
  result := TExcelExportSpreadSheet.Create(XLS);
end;

procedure TExcelFileSpreadSheet.open(const fileName: String);
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

function TExcelFileSpreadSheet.rowCount: integer;
begin
  result := XLS.RowCount;
end;

function TExcelFileSpreadSheet.sheetName: string;
begin
  result := XLS.SheetName;
end;

function TExcelFileSpreadSheet.sheetCount: integer;
begin
  result := XLS.SheetCount;
end;

procedure TExcelFileSpreadSheet.sheetWorkOn(tabIndex: uint32);
begin
  XLS.ActiveSheet := tabIndex;
end;

procedure TExcelFileSpreadSheet.sheetWorkOn(name: String);
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

{ TExcelExportSpreadSheet }

constructor TExcelExportSpreadSheet.Create(aFormat: TXlsFile);
begin
  FInternal := aFormat;
end;

procedure TExcelExportSpreadSheet.exportAsCSV(xlsFile, sheetName,
  targetFileName: string);
begin
  //TODO
end;

initialization

registerImplementation(TExcelFileSpreadSheet)

end.
