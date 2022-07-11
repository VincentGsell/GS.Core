unit GS.Common.Report.Flexcel;

interface

uses SysUtils,
     Classes,
     IOUtils,
     GS.Common.Report,
     Generics.Collections,
     FlexCel.XlsAdapter,

     Datasnap.DBClient,
     Data.DB,

     FlexCel.Report,
     FlexCel.Render;

Type
TGSReportFlexcelFactory = class(TInterfacedObject,IGSReportFactory)
  function getInstance : IGSReport;
end;

TGSReportFlexCel = class(TInterfacedObject, IGSReport, IGSReportData)
private
  FReport : TFlexCelReport;
public
  constructor Create; virtual;
  destructor Destroy; override;

  //IGSReport
  procedure Render(aTemplateFile : string; aTargetFile : string);
  function Data : IGSReportData;

  //IGSReportData
  procedure addVar(name : String; value : string); overload;
  procedure addVar(name : String; value : TDateTime); overload;
  procedure addVar(name : String; value : TBytes); overload;
  procedure addVar(name : String; value : TClientDataset); overload;
  procedure DataBufferAdd(aDataBufferName : String);
  procedure DataBufferNewLine;
  procedure DataBufferAddFieldData(fieldName :string; Value : String);
end;

implementation



{ TGSReportFlexCel }

procedure TGSReportFlexCel.addVar(name, value: string);
begin
  FReport.SetValue(name,value);
end;

procedure TGSReportFlexCel.addVar(name: String; value: TBytes);
begin
  FReport.SetValue(name,value);
end;

procedure TGSReportFlexCel.addVar(name: String; value: TDateTime);
begin
  FReport.SetValue(name,value);
end;

procedure TGSReportFlexCel.addVar(name: String; value: TClientDataset);
begin
  FReport.AddTable(name,value);
end;

constructor TGSReportFlexCel.Create;
var i : integer;
begin
  Inherited;
  FReport := TFlexCelReport.Create(True);
end;

function TGSReportFlexCel.Data: IGSReportData;
begin
  result := IGSReportData(Self);
end;

procedure TGSReportFlexCel.DataBufferAdd(aDataBufferName: String);
begin
  ///..
end;

procedure TGSReportFlexCel.DataBufferAddFieldData(fieldName, Value: String);
begin
  ///..
end;

procedure TGSReportFlexCel.DataBufferNewLine;
begin
  ///..
end;

destructor TGSReportFlexCel.Destroy;
begin
  FreeAndNil(FReport);
  inherited;
end;

procedure TGSReportFlexCel.Render(aTemplateFile : string; aTargetFile : string);
begin
//  if ExtractFileExt(aTargetFile).ToLower = 'pdf'  then
//   FReport.
  FReport.Run(aTemplateFile,aTargetFile);

end;

{ TGSReportFlexcelFactory }

function TGSReportFlexcelFactory.getInstance: IGSReport;
begin
  result := TGSReportFlexCel.Create;
end;

initialization

TGSReportFactory.ReportRegister('flexcel',TGSReportFlexcelFactory.Create);
end.
