unit GS.fileFormat.Excel;

interface

uses
  System.SysUtils,
  System.Classes;

Type
  IExcelExport = Interface
    //export
    procedure exportAsCSV(xlsFile : string; sheetName :string; targetFileName : string);
    procedure exportAsPDF(xlsFile : string; sheetName :string; targetFileName : string);
  End;

  //Final interface.
  IExcelFile = Interface
  ['{B130A6D7-1958-4DD2-BD85-E961953D8B61}']
    //file
    procedure open(const fileName : String);

    //Sheets
    function sheetCount : integer;
    procedure sheetWorkOn(name : String); overload;
    procedure sheetWorkOn(tabIndex : uint32); overload;
    function sheetName :string;
    function cells(x,y : integer) : string; //return the cell located on x [1,n] and y [1,n] value as string.
    function colCount : integer; //Max colcount in sheet.
    function rowCount : integer; //Max rowcount in sheet.

    //Export.
    function getExportEngine : IExcelExport;
  End;

  TCustomExcelFile = class(TInterfacedObject)
    constructor Create; virtual;
  end;
  TCustomExcelFileClass = class of TCustomExcelFile;

  function GetImplementation : IExcelFile;
  procedure RegisterImplementation(objectClass : TCustomExcelFileClass);


implementation

  var Local_CurrentImpl : TCustomExcelFileClass;

  procedure RegisterImplementation(objectClass : TCustomExcelFileClass);
  begin
    if local_CurrentImpl <> nil then raise Exception.Create('Implementation already found');
    if Not Supports(objectClass,IExcelFile) then
      raise Exception.Create('object must support IExcellFormat interface');
    local_CurrentImpl := objectClass;
  end;

  function GetImplementation : IExcelFile;
  begin
    if local_CurrentImpl = nil then raise Exception.Create('No implementation found');
    result := local_CurrentImpl.Create as IExcelFile
  end;
{ TCustomExcelFile }

constructor TCustomExcelFile.Create;
begin
  //needed for injection.
end;

end.
