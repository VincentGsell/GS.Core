unit GS.JSON;


{$I GSCore.inc}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  {$IFDEF USE_GENERIC}
  Generics.Collections, //Starting 3.1.1 only.
  {$ENDIF}
{$ELSE}
  System.Classes,
  System.SysUtils,
  {$IFDEF USE_GENERIC}
  System.Generics.Collections,
  {$ENDIF}
{$ENDIF}
  Jsons,
  JsonsUtilsEx,
  BcpBase64,
  GS.Common;

Type
  TGSJsonArrayPropertyItem = Class
  Public
    PropertyName : string;
    ItemArrayType :TClass;
  End;

  {$IFDEF USE_GENERIC}
  TGSJsonListOfProperty = class(TDictionary<String,TGSJsonArrayPropertyItem>)
    Procedure AddListProperty(anArrayPropertyName : String; aItemArrayType : TClass);
    Function GetPropertyConfiguration(anArrayPropertyName : String) : TGSJsonArrayPropertyItem;
    Procedure RegisterPropertyArrayTypes(Const PropertyName : String; ItemArrayType : TClass);
  end;
  {$ELSE}
  TGSJsonListOfProperty = class(TKeysValues_UTF8StringToObject)

    Function TryGetValue(aKey : String; var aResult : TGSJsonArrayPropertyItem) : Boolean; Reintroduce;
    Procedure Add(aString : String; aGSJsonArrayPropertyItem: TGSJsonArrayPropertyItem); Reintroduce;

    Procedure AddListProperty(anArrayPropertyName : String; aItemArrayType : TClass);
    Function GetPropertyConfiguration(anArrayPropertyName : String) : TGSJsonArrayPropertyItem;
    Procedure RegisterPropertyArrayTypes(Const PropertyName : String; ItemArrayType : TClass);
  end;
  {$ENDIF}

  TGSJson = class(TJson)
  Protected
    class var FListOfProperty : TGSJsonListOfProperty;
    class function CheckInstance : Boolean;
    class Procedure Init;
    class Procedure Release;
  Public
    class var base64 : TBase64;
    {$IFDEF DCC} //Currently, this compile on FPC work only for trunck. But Trunck is broken on ARM. :(
    class Procedure JsonToObject(Const aJSONString : String; var aObject : TObject);
    class Function ObjectToJson(aObject : TObject) : String;
    {$ENDIF DCC} //Currently, this compile on FPC work only for trunck. But Trunck is broken on ARM. :(

    class Function Configuration : TGSJsonListOfProperty;

    function Path(jsonPath : string) : TJsonValue;

    class Function Base64StringToBytes(Const aBase64String : String) : TBytes;
    class function BytesToBase64String(const aBytes : TBytes) : String;
  end;

  TGSJsonArray = TJsonArray;

implementation


{ TGSJson }

class function TGSJson.Base64StringToBytes(const aBase64String: String): TBytes;
begin
  result := base64.Decode(aBase64String);
end;

class function TGSJson.BytesToBase64String(const aBytes: TBytes): String;
begin
  result := base64.Encode(aBytes);
end;

class function TGSJson.CheckInstance: Boolean;
begin
  result :=true;
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
  base64 := TBase64.Create;
end;

{$IFDEF DCC} //Currently, this compile on FPC work only for trunk. But Trunk is broken on ARM. :(
class Procedure TGSJson.JsonToObject(const aJSONString: String;
  var aObject: TObject);
begin
  JsonsUtilsEx.__JsonToObject(aJSONString, aObject);
end;

class function TGSJson.ObjectToJson(aObject: TObject): String;
begin
  Result := jsonsUtilsEx.__ObjectToJson(aObject);
end;
{$ENDIF DCC} //Currently, this compile on FPC work only for trunk. But Trunk is broken on ARM. :(

function TGSJson.Path(jsonPath: string): TJsonValue;
var l : TstringList;
    ll : TJsonObject;
  I: Integer;
begin
  result := nil;
  l := TStringList.Create;
  try
    l.Delimiter := '.';
    l.DelimitedText := jsonPath;
    if l.Count>1 then
    begin
      ll := get(l[0]).AsObject;
      if l.Count=2 then
      begin
        result := ll.Values[l[1]];
      end
      else
      begin
        for i := 1 to l.Count-2 do
          ll := ll.Values[l[i]].AsObject;
        result := ll.Values[l[l.Count-1]];
      end;
    end
    else
      raise Exception.Create('Error Message');
  finally
    FreeAndNil(l);
  end;
end;


class procedure TGSJson.Release;
{$IFDEF USE_GENERIC}
var aItem : TPair<String, TGSJsonArrayPropertyItem>;
    ab : TArray<TPair<String, TGSJsonArrayPropertyItem>>;
{$ELSE}
var i : integer;
{$ENDIF}
begin
  if Assigned(FListOfProperty) then
  begin
  {$IFDEF USE_GENERIC}
    ab := FListOfProperty.ToArray;
    for aItem in ab do
    begin
      aItem.Value.Free;
    end;
    FreeAndNil(FListOfProperty);
  {$ELSE}
     for I := 0 to FListOfProperty.Count-1 do
     begin
       FListOfProperty.FArray[i].Value.Free;
     end;
  {$ENDIF}
  end;
  FreeAndNil(base64);
end;

{ TGSJsonListOfProperty }

procedure TGSJsonListOfProperty.RegisterPropertyArrayTypes(const PropertyName: String;
  ItemArrayType: TClass);
begin
  AddListProperty(PropertyName,ItemArrayType);
end;

procedure TGSJsonListOfProperty.Add(aString: String;
  aGSJsonArrayPropertyItem: TGSJsonArrayPropertyItem);
begin
  Assert(assigned(aGSJsonArrayPropertyItem));
  Inherited Add(aString, aGSJsonArrayPropertyItem);
end;

function TGSJsonListOfProperty.TryGetValue(aKey: String;
  var aResult: TGSJsonArrayPropertyItem): Boolean;
begin
  Result := Inherited TryGetValue(aKey,TObject(aResult));
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
