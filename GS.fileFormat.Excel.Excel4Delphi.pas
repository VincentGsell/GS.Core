unit GS.fileFormat.Excel.Excel4Delphi;

interface

uses
  //Delphi
  System.SysUtils,
  System.Classes,
  //GS Excel interface
  GS.fileFormat.Excel,
  //Excel4Delphi
  Excel4Delphi,
  Excel4Delphi.Common,
  Excel4Delphi.Stream;

type

  TExcelFileExcel4Delphi = class(TCustomExcelFile, IExcelFile)
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

  TExcelExportExcel4Delphi = class(TInterfacedObject, IExcelExport)
  private
  public
    Constructor Create(aFormat : TZWorkBook);

    procedure exportAsCSV(xlsFile : string; sheetName :string; targetFileName : string);
  end;

implementation

{ TExcelFileTMSFlexCel }

function TExcelFileExcel4Delphi.cells(x, y : integer): string;
var a : TZCellValue;
begin
  a := XLS.GetCellValue(y,x);
  if a.IsEmpty then
    result := ''
  else
    result := a.AsTValue.ToString;
end;

function TExcelFileExcel4Delphi.colCount: integer;
begin
  result := XLS.ColCount;
end;

constructor TExcelFileExcel4Delphi.create;
begin
  inherited Create;
  XLS := TXlsFile.Create;
end;

destructor TExcelFileExcel4Delphi.destroy;
begin
  FreeAndNil(XLS);
  inherited;
end;

function TExcelFileExcel4Delphi.getExportEngine: IExcelExport;
begin
  result := TExcelExportExcel4Delphi.Create(XLS);
end;

procedure TExcelFileExcel4Delphi.open(const fileName: String);
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

function TExcelFileExcel4Delphi.rowCount: integer;
begin
  result := XLS.RowCount;
end;

function TExcelFileExcel4Delphi.sheetName: string;
begin
  result := XLS.SheetName;
end;

function TExcelFileExcel4Delphi.sheetCount: integer;
begin
  result := XLS.SheetCount;
end;

procedure TExcelFileExcel4Delphi.sheetWorkOn(tabIndex: uint32);
begin
  XLS.ActiveSheet := tabIndex;
end;

procedure TExcelFileExcel4Delphi.sheetWorkOn(name: String);
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

{ TExcelExportExcel4Delphi }

constructor TExcelExportExcel4Delphi.Create(aFormat: TXlsFile);
begin
  FInternal := aFormat;
end;

procedure TExcelExportExcel4Delphi.exportAsCSV(xlsFile, sheetName,
  targetFileName: string);
begin
  //TODO
end;

initialization

registerImplementation(TExcelFileExcel4Delphi)

end.
