program xlsSupport;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  GS.fileFormat.Excel in '..\..\..\GS.fileFormat.Excel.pas',
  GS.fileFormat.Excel.TMSFlexCel in '..\..\..\GS.fileFormat.Excel.TMSFlexCel.pas'
  {,GS.fileFormat.Excel.Excel4Delphi in '..\..\..\GS.fileFormat.Excel.Excel4Delphi.pas'}
  {,GS.fileFormat.Excel.fpspreadsheet in '..\..\..\GS.fileFormat.Excel.fpspreadsheet.pas'}
  ;

function ElapsedTime(const et, st: TDateTime): string;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(et - st, Hour, Min, Sec, MSec);
  Result :=  Format('%.2d:%.2d:%.2d', [Hour, Min, Sec]);
end;


var xls : IExcelFile;
    EndOpen, StartOpen : TDateTime;
    cdsSheetIndex : integer;
    i : integer;
    col, row : integer;
begin
  {$IFDEF DEBUG} ReportMemoryLeaksOnShutdown := true; {$ENDIF}
  try
    if ((ParamStr(1)<>'') and Not(fileExists(paramStr(1)))) then raise Exception.Create('source file does not exist');

    xls := GS.fileFormat.Excel.getImplementation; //Give an excel in depth impl (TMS, Excel4Delphi...)
    writeln(format('using "%s" implementation...',[TObject(xls).ClassName]));
    writeln(format('Loading "%s" ',[ExtractFileName(ParamStr(1))]));
    try
      StartOpen := now;
      xls.open(ParamStr(1));
      EndOpen := Now;
    except
      On e : exception do begin
        writeln(format(' Something wrong happen on opening "%s"',[ExtractFileName(ParamStr(1))]));
        writeln(format(' -> Details : %s',[e.Message]));
        Halt;
      end;
    end;

    writeln('file loading in: '+  ElapsedTime(EndOpen, StartOpen));
    writeln(format('Sheets count : %d',[xls.SheetCount]));
    for i:= 0 to xls.SheetCount-1 do begin
      xls.sheetWorkOn(i+1);
      Writeln(format('%d --> "%s"',[i,xls.SheetName]));
    end;

    {$IFDEF DEBUG}
    readln;
    {$ENDIF}
  except
    {$IFDEF DEBUG}
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
    {$ENDIF}
  end;
end.
