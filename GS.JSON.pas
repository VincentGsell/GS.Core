unit GS.JSON;

interface

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

Uses
  SysUtils,
  classes,
  Jsons,
  JsonsUtilsEx,
  {$IFDEF FPC}
  generics.collections;
  {$ELSE}
  System.Generics.Collections;
  {$ENDIF}

Type
  TGSJsonArrayPropertyItem = Class
  Public
    PropertyName : string;
    ItemArrayType :TClass;
  End;

  TGSJsonListOfProperty = class(TDictionary<String,TGSJsonArrayPropertyItem>)
    Procedure AddListProperty(anArrayPropertyName : String; aItemArrayType : TClass);
    Function GetPropertyConfiguration(anArrayPropertyName : String) : TGSJsonArrayPropertyItem;
    Procedure RegisterPropertyArrayTypes(Const PropertyName : String; ItemArrayType : TClass);
  end;

  TGSJson = class(TJson)
  Protected
    class var FListOfProperty : TGSJsonListOfProperty;
    class function CheckInstance : Boolean;
    class Procedure Init;
    class Procedure Release;
  Public
    class Procedure JsonToObject(Const aJSONString : String; var aObject : TObject);
    class Function ObjectToJson(aObject : TObject) : String;

    class Function Configuration : TGSJsonListOfProperty;
  end;

implementation


{ TGSJson }

class function TGSJson.CheckInstance: Boolean;
begin
  if Not Assigned(FListOfProperty) then
  begin
    FListOfProperty := TGSJsonListOfProperty.Create;
  end;
end;

class function TGSJson.Configuration: TGSJsonListOfProperty;
begin
  CheckInstance;
  Result := FListOfProperty;
end;

class procedure TGSJson.Init;
begin
  FListOfProperty := Nil;
end;

class Procedure TGSJson.JsonToObject(const aJSONString: String;
  var aObject: TObject);
begin
  JsonsUtilsEx.__JsonToObject(aJSONString, aObject);
end;

class function TGSJson.ObjectToJson(aObject: TObject): String;
begin
  Result := jsonsUtilsEx.__ObjectToJson(aObject);
end;

class procedure TGSJson.Release;
var aItem : TPair<String, TGSJsonArrayPropertyItem>;
    ab : TArray<TPair<String, TGSJsonArrayPropertyItem>>;
begin
  if Assigned(FListOfProperty) then
  begin
    ab := FListOfProperty.ToArray;
    for aItem in ab do
    begin
      aItem.Value.Free;
    end;
    FreeAndNil(FListOfProperty);
  end;
end;

{ TGSJsonListOfProperty }

procedure TGSJsonListOfProperty.RegisterPropertyArrayTypes(const PropertyName: String;
  ItemArrayType: TClass);
begin
  AddListProperty(PropertyName,ItemArrayType);
end;

procedure TGSJsonListOfProperty.AddListProperty(anArrayPropertyName: String;
  aItemArrayType: TClass);
var aItem : TGSJsonArrayPropertyItem;
begin
  Assert(length(anArrayPropertyName)>0);
  if not TryGetValue(anArrayPropertyName,aItem) then
  begin
    aItem := TGSJsonArrayPropertyItem.Create;
    Add(anArrayPropertyName,aItem);
    aItem.ItemArrayType := aItemArrayType;
  end;
end;

function TGSJsonListOfProperty.GetPropertyConfiguration(
  anArrayPropertyName: String): TGSJsonArrayPropertyItem;
var aItem : TGSJsonArrayPropertyItem;
begin
  Result := Nil;
  if TryGetValue(anArrayPropertyName,aItem) then
    result := aItem
  else
  begin
    raise Exception.Create('Error Message');
  end;
end;

Initialization

TGSJson.Init;

Finalization

TGSJson.Release;

end.
