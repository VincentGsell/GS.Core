unit GS.Reference.Persister.SingleJsonFile;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses Classes, SysUtils,
     GS.Common,
     GS.Stream,
     GS.System.Files,
     GS.Reference.Persister;

Type

TofReferencePersisterSingleJsonFile = class(TofReferencePersister)
private
protected
  FFileName : string;
  FInMemoryAllocation : IGSStringList; //--> key,JsonString (JsonL (Json line file https://jsonlines.org/)

  FOnInitialLoading : TOnReferenceInitialLoad;
  FInMemoryOnly: Boolean;

  procedure SetFileName(const Value: String);
public
  //If file name is empty, file wiil be in memory only. So, you can use that for in memory database.
  Constructor Create(Const aFileName: String = ''; Const OnInitLoadEvent : TOnReferenceInitialLoad = Nil); reintroduce; Virtual;
  Destructor Destroy; Override;

  Procedure WriteGenericData( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Override;
  Procedure ReadGenericDataPrepare( Key : String;
                                      DataType : tofContentType;
                                      StreamData : TMemoryStream); Override;

  Procedure DeleteGenericData(Key : String); Override;

  Procedure ReadInfoDataByIndex(aIndex : UInt32; var aDataType : TofcontentType; var aKey : String); Override;
  Procedure ReadInfoDataByKey(aKey : string; var aDataType : TofcontentType; var Index : UInt64); Override;
  Function IsKeyExists(aKey : String) : Boolean; Override;

  Procedure Open; Override;
  Procedure Close; Override;

  Function EntryCount : UInt64; Override;
  Function Version : String; Override;
  Function Description : string; Override;

  Property InMemoryOnly : Boolean read FInMemoryOnly;
  Property FileName : String read FFileName Write SetFileName;

  Property OnInitialLoading : TOnReferenceInitialLoad read FOnInitialLoading Write FOnInitialLoading;
End;

implementation

{ TofReferencePersisterSingleJsonFile }

procedure TofReferencePersisterSingleJsonFile.Close;
begin
  inherited;
  if FileName.Trim.Length>0 then
    FInMemoryAllocation.SaveToFile(FileName);
end;

constructor TofReferencePersisterSingleJsonFile.Create(const aFileName: String;
  const OnInitLoadEvent: TOnReferenceInitialLoad);
begin
  Inherited Create;
  FInMemoryAllocation := TGSStringList.Create;
  FileName := aFileName; //Setter called
end;

procedure TofReferencePersisterSingleJsonFile.DeleteGenericData(Key: String);
var l : integer;
begin
  inherited;
  l := FInMemoryAllocation.indexOf(Key);
  if l>-1 then
    FInMemoryAllocation.lines(l,Key+'=');
end;

function TofReferencePersisterSingleJsonFile.Description: string;
begin
  result := '';
end;

destructor TofReferencePersisterSingleJsonFile.Destroy;
begin
  Close;
  inherited;
end;

function TofReferencePersisterSingleJsonFile.EntryCount: UInt64;
begin

end;

function TofReferencePersisterSingleJsonFile.IsKeyExists(aKey: String): Boolean;
begin

end;

procedure TofReferencePersisterSingleJsonFile.Open;
begin
  //None.
end;

procedure TofReferencePersisterSingleJsonFile.ReadGenericDataPrepare(
  Key: String; DataType: tofContentType; StreamData: TMemoryStream);
begin
  inherited;

end;

procedure TofReferencePersisterSingleJsonFile.ReadInfoDataByIndex(
  aIndex: UInt32; var aDataType: TofcontentType; var aKey: String);
begin
  inherited;

end;

procedure TofReferencePersisterSingleJsonFile.ReadInfoDataByKey(aKey: string;
  var aDataType: TofcontentType; var Index: UInt64);
begin
  inherited;

end;

procedure TofReferencePersisterSingleJsonFile.SetFileName(const Value: String);
begin
  FInMemoryOnly := Value.Trim.Length=0;
  if FInMemoryOnly then
    Exit;

  if TGSFileSys.FileExists(Value) then begin
    FInMemoryAllocation.loadFromFile(Value);
    Open;
    FFileName := Value;
  end;
end;

function TofReferencePersisterSingleJsonFile.Version: String;
begin
  result := 'GS JsonL simple persister v1.0';
end;

procedure TofReferencePersisterSingleJsonFile.WriteGenericData(Key: String;
  DataType: tofContentType; StreamData: TMemoryStream);
begin
  inherited;

end;

end.
