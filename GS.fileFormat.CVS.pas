///202110 - VGS - Added format to strange cvs encoding (no enconding mark at all, but enconding as wide (?))
///               Try to make this stuff tolerant.
unit GS.fileFormat.CVS;

interface

uses
  Classes,
  SysUtils,
  GS.Common,
  GS.Common.Log;

type
  TCSVDataSource = class(TObject)
    protected
      _columns: TStringList;
      _rows: TStringList;
      _delimiter: Char;
      _index: Integer;
      _feof: Boolean;
      _date_separator: Char;
    private
      function GetColumnIndex(Column: String): Integer;
    public
      constructor Create();
      procedure LoadFromFile(const FileName: String);
      procedure SetDelimiter(const Character: Char);
      procedure SetDateSeparator(const Separator: Char);
      procedure First;
      function GetTotal(): Integer;
      function GetCols: TArray<String>;
      function GetRowString(): String;
      function FieldByNameAsString(Column: String): String;
      function FieldByNameAsInteger(Column: String): Integer;
      function FieldByNameAsFloat(Column: String): Extended;
      function FieldByNameAsDate(Column: String): TDate;
      function FieldByNameAsTime(Column: String): TTime;
      function FieldByNameAsDateTime(Column: String): TDateTime;
      procedure Next;
      property Eof: Boolean read _feof;
      property Count: Integer read GetTotal;
  end;


Const
  cst_gsfileformatcvs_log_mark = 'GS.fileFormatCVS_log';
implementation

constructor TCSVDataSource.Create;
begin
  Inherited;
  _columns := TStringList.Create;
  _columns.DefaultEncoding := TEncoding.UTF8;
  _rows := TStringList.Create;
  _rows.DefaultEncoding := TEncoding.UTF8;
  _columns.Clear;
  _rows.Clear;
  _index := -1;
  _delimiter := ';';
  _date_separator := '-';
end;

procedure TCSVDataSource.LoadFromFile(const FileName: String);

  function LocalGetEncodingFile(var Size : integer) : TEncoding;
  var
    Stream : TMemoryStream;
    Buffer: TBytes;
  begin
    result := nil;
    Stream := TMemoryStream.Create;
    try
      Stream.LoadFromFile(FileName);
      Size := Stream.Size - Stream.Position;
      SetLength(Buffer, Size);
      Stream.Read(Buffer, 0, Size);
      Size := TEncoding.GetBufferEncoding(Buffer, Result, TEncoding.Default);
    finally
      FreeAndNil(Stream);
    end;
  end;

var
  loadedFile,Row: TStringList;
  I: Integer;
  lEncoding : TEncoding;
  lSize : integer;
begin
  Tlog.enter('TCSVDataSource.LoadFromFile');
  try
    assert(fileExists(FileName));
    loadedFile := TStringList.Create;
    lEncoding := LocalGetEncodingFile(lSize);
    if lSize = 0 then begin //No BOM. Let search what is inside.
      TLog.info('No bom file. Try to load with UTF8 enconding...',cst_gsfileformatcvs_log_mark);
      try
        loadedFile.LoadFromFile(FileName,TEncoding.UTF8); //UTF8 no bom, the more probable.
      Except
        TLog.warning('Failed to load file as UTF8 - try default encoding...',cst_gsfileformatcvs_log_mark);
        loadedFile.LoadFromFile(fileName,lEncoding); //Assuming default encoding
        //That should work, but in case of wrong format, we Look if it is a no bomed-and-not-ansi enconded file (i.e. no encoging indicate)
        if (length(loadedFile.Text)>2) And (loadedFile.Text[2]=#0) then
        begin
          TLog.warning('File seems to be bad encoded internaly : Try to load as unicode.',cst_gsfileformatcvs_log_mark);
          loadedFile.LoadFromFile(fileName,TEncoding.Unicode); //Process it as unicode, since UTF8 not worked : Last chance.
        end;
      end;
    end
    else begin
      //BOM and encoding successfuly retrived.
      loadedFile.LoadFromFile(fileName,lEncoding); //Assuming default encoding
    end;

    // load columns
    Row := TStringList.Create;
    Row.DefaultEncoding := TEncoding.UTF8;
    Row.StrictDelimiter := True;
    Row.Delimiter := Self._delimiter;
    Row.DelimitedText := loadedFile.Strings[0]; // first row is column names

    for I := 0 to Row.Count -1 do
    begin
      Self._columns.Add(Row.Strings[I]);
    end;

    // load rows
    for I := 1 to loadedFile.Count - 1 do
    begin
      Self._rows.Add(loadedFile.Strings[I]);
    end;

    Self._index := 0;
    Self._feof := False;
  finally
    Tlog.leave('TCSVDataSource.LoadFromFile');
  end;
end;

procedure TCSVDataSource.SetDelimiter(const Character: Char);
begin
  Self._delimiter := Character;
end;

procedure TCSVDataSource.SetDateSeparator(const Separator: Char);
begin
  Self._date_separator := Separator;
end;

procedure TCSVDataSource.First;
begin
  Self._index := 0;
end;

function TCSVDataSource.GetTotal: Integer;
begin
  Result := Self._rows.Count;
end;

function TCSVDataSource.GetRowString: String;
begin
  Result := Self._rows.Strings[Self._index];
end;

function TCSVDataSource.GetCols: TArray<String>;
var l : IGSStringList;
begin
  l := TGSStringList.Create;
  l.setText(_columns.Text);
  result := l.ToStringArray;
end;

function TCSVDataSource.GetColumnIndex(Column: String): Integer;
var
  ColumnIndex: Integer;
  i : integer;
begin
  ColumnIndex := Self._columns.IndexOf(Column);
  if ColumnIndex <> -1 then
    Result := ColumnIndex
  else
  begin
    Tlog.debug(_columns.Text);
    raise Exception.Create('Error: Column "' + Column + '" not found !');
  end;
end;

function TCSVDataSource.FieldByNameAsString(Column: String): String;
var
  ColumnIndex: Integer;
  Row: TStringList;
begin
  ColumnIndex := Self.GetColumnIndex(Column);
  Row := TStringList.Create;
  Row.DefaultEncoding := TEncoding.UTF8;
  Row.StrictDelimiter := True;
  Row.Delimiter := Self._delimiter;
  Row.DelimitedText := Self._rows.Strings[Self._index];
  Result := Row.Strings[ColumnIndex];
end;

function TCSVDataSource.FieldByNameAsInteger(Column: String): Integer;
var
  ColumnIndex: Integer;
  Row: TStringList;
begin
  ColumnIndex := Self.GetColumnIndex(Column);
  Row := TStringList.Create;
  Row.DefaultEncoding := TEncoding.UTF8;
  Row.StrictDelimiter := True;
  Row.Delimiter := Self._delimiter;
  Row.DelimitedText := Self._rows.Strings[Self._index];
  Result := StrToIntDef(Row.Strings[ColumnIndex], 0);
end;

function TCSVDataSource.FieldByNameAsFloat(Column: String): Extended;
var
  ColumnIndex: Integer;
  Row: TStringList;
begin
  ColumnIndex := Self.GetColumnIndex(Column);
  Row := TStringList.Create;
  Row.DefaultEncoding := TEncoding.UTF8;
  Row.StrictDelimiter := True;
  Row.Delimiter := Self._delimiter;
  Row.DelimitedText := Self._rows.Strings[Self._index];
  Result := StrToFloatDef(Row.Strings[ColumnIndex], 0);
end;

function TCSVDataSource.FieldByNameAsDate(Column: String): TDate;
var
  ColumnIndex: Integer;
  Row: TStringList;
  MySettings: TFormatSettings;
begin
  //GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, MySettings);
  MySettings.DateSeparator := Self._date_separator;

  ColumnIndex := Self.GetColumnIndex(Column);
  Row := TStringList.Create;
  Row.DefaultEncoding := TEncoding.UTF8;
  Row.StrictDelimiter := True;
  Row.Delimiter := Self._delimiter;
  Row.DelimitedText := Self._rows.Strings[Self._index];
  Result := StrToDate(Row.Strings[ColumnIndex], MySettings);
end;

function TCSVDataSource.FieldByNameAsTime(Column: String): TTime;
var
  ColumnIndex: Integer;
  Row: TStringList;
begin
  ColumnIndex := Self.GetColumnIndex(Column);
  Row := TStringList.Create;
  Row.DefaultEncoding := TEncoding.UTF8;
  Row.StrictDelimiter := True;
  Row.Delimiter := Self._delimiter;
  Row.DelimitedText := Self._rows.Strings[Self._index];
  Result := StrToTime(Row.Strings[ColumnIndex]);
end;

function TCSVDataSource.FieldByNameAsDateTime(Column: String): TDateTime;
var
  ColumnIndex: Integer;
  Row: TStringList;
  MySettings: TFormatSettings;
begin
//  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, MySettings);
  MySettings.DateSeparator := Self._date_separator;

  ColumnIndex := Self.GetColumnIndex(Column);
  Row := TStringList.Create;
  Row.StrictDelimiter := True;
  Row.Delimiter := Self._delimiter;
  Row.DelimitedText := Self._rows.Strings[Self._index];
  Result := StrToDateTime(Row.Strings[ColumnIndex], MySettings);
end;

procedure TCSVDataSource.Next;
begin
  Inc(Self._index);
  if Self._index = Self._rows.Count then
    Self._feof := True;
end;

end.
