///
/// Reporting base abstrat api ressource.
///
unit GS.Common.Report;

interface

uses sysutils, classes,
     Generics.Collections,
     Datasnap.DBClient,
     Data.DB;

Type
IGSReportData = interface
  procedure addVar(name : String; value : string); overload;
  procedure addVar(name : String; value : TDateTime); overload;
  procedure addVar(name : String; value : TBytes); overload;
  procedure addVar(name : String; value : TClientDataset); overload;
  procedure DataBufferAdd(aDataBufferName : String);
  procedure DataBufferNewLine;
  procedure DataBufferAddFieldData(fieldName :string; Value : String);
end;

IGSReport = interface
  procedure Render(aTemplateFile : string; aTargetFile : string);
  function Data : IGSReportData;
end;

IGSReportExporter = interface
  procedure SaveToFile(afileName : String);
end;

IGSReportFactory = interface
  function getInstance : IGSReport;
end;

TGSReportFactory = class
private
  Class var FReportClasses : TDictionary<string,IGSReportFactory>;
  Class procedure internalcheck;
public
  class function GetInstansce(const _type : String = 'txt') : IGSReport;
  class procedure ReportRegister(_type : String; _Factory : IGSReportFactory);
  class procedure Terminate;
end;

implementation

{ TGSReportFactory }

class function TGSReportFactory.GetInstansce(const _type: String): IGSReport;
var lvo : IGSReportFactory;
begin
  internalcheck;
  if Not FReportClasses.TryGetValue(_type,lvo) then
    raise Exception.Create(className+'.GetInstance : report type "'+_type+'" not registered.');
  result := lvo.getInstance;
end;

class procedure TGSReportFactory.internalcheck;
begin
  if not Assigned(FReportClasses) then
    FReportClasses := TDictionary<string,IGSReportFactory>.create;
end;

class procedure TGSReportFactory.ReportRegister(_type : String; _Factory : IGSReportFactory);
var lv : IGSReportFactory;
begin
  internalcheck;
  if not FReportClasses.TryGetValue(_type,lv) then
    FReportClasses.Add(_type,_Factory);
end;

class procedure TGSReportFactory.Terminate;
begin
  if Assigned(FReportClasses) then
    FreeAndNil(FReportClasses);
end;

Initialization

Finalization

TGSReportFactory.Terminate;


end.
