//fpspreadsheet implementation
//https://github.com/tcs-ulli/fpspreadsheet
//FPC Only currently.
unit GS.fileFormat.Excel.fpspreadsheet;

interface

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, Classes, TypInfo,
  //Agroplus
  agr.fileFormat.Excel,
  //fpspreadsheet
  fpstypes, fpSpreadsheet, fpsutils;
  
type

  TExcelFileFpSpreadSheet = class(TCustomExcelFile, IExcelFile)
  private
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

    //Export.
    function getExportEngine : IExcelExport;
  end;

  TExcelFileFpSpreadSheet = class(TInterfacedObject, IExcelExport)
  private
    FInternal : TXLSFile;
  public
    Constructor Create(aFormat : TXlsFile);

    procedure exportAsCSV(xlsFile : string; sheetName :string; targetFileName : string);
  end;

implementation

{ TExcelFileFpSpreadSheet }

constructor TExcelFileFpSpreadSheet.create;
begin
  inherited Create;
  XLS := TXlsFile.Create;
end;

destructor TExcelFileFpSpreadSheet.destroy;
begin
  FreeAndNil(XLS);
  inherited;
end;

function TExcelFileFpSpreadSheet.getExportEngine: IExcelExport;
begin
  result := TExcelFileFpSpreadSheet.Create(XLS);
end;

procedure TExcelFileFpSpreadSheet.open(const fileName: String);
begin
  XLS.Open(filename);
end;

function TExcelFileFpSpreadSheet.sheetName: string;
begin
  result := XLS.SheetName;
end;

function TExcelFileFpSpreadSheet.sheetCount: integer;
begin
  result := XLS.SheetCount;
end;

procedure TExcelFileFpSpreadSheet.sheetWorkOn(tabIndex: uint32);
begin
  XLS.ActiveSheet := tabIndex;
end;

procedure TExcelFileFpSpreadSheet.sheetWorkOn(name: String);
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

{ TExcelFileFpSpreadSheet }

constructor TExcelFileFpSpreadSheet.Create(aFormat: TXlsFile);
begin
  FInternal := aFormat;
end;

procedure TExcelFileFpSpreadSheet.exportAsCSV(xlsFile, sheetName,
  targetFileName: string);
begin
  //Export basically with tab seprator csv. If you want more customization, see https://support.tmssoftware.com/t/open-csv-text-file/3227/6
  FInternal.ActiveSheetByName := sheetName;
  FInternal.Save(targetFileName,TFileFormats.Text);
end;

initialization

registerImplementation(TExcelFileFpSpreadSheet)

end.
