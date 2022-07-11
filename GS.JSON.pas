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
{$IFDEF BCPBASE64}
  BcpBase64,
{$ENDIF}
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

  TGSJsonArray = TJsonArray;
  TGSJsonObject = TJsonObject;

  TGSJson = class(TJson)
  private
    function GetJson: TJson;
  Protected
    class var FListOfProperty : TGSJsonListOfProperty; { TODO : TO REMOVE PLZ ! }
    class function CheckInstance : Boolean;
    class Procedure Init;
    class Procedure Release;
  Public
{$IFDEF BCPBASE64}
    class var base64 : TBase64;
    class Function Base64StringToBytes(Const aBase64String : String) : TBytes;
    class function BytesToBase64String(const aBytes : TBytes) : String;
{$ENDIF}
    {$IFDEF DCC} //Currently, this compile on FPC work only for trunck. But Trunck is broken on ARM. :(
    class Procedure JsonToObject(Const aJSONString : String; var aObject : TObject);
    class Function ObjectToJson(aObject : TObject) : String;
    {$ENDIF DCC} //Currently, this compile on FPC work only for trunck. But Trunck is broken on ARM. :(

    class function JSONDateToString(aDate : TDateTime) : String;
    class function JSONStringToDate(aDate : String) : TDateTime;

    class Function Configuration : TGSJsonListOfProperty;

    function Path(jsonPath : string) : TJsonValue;
  end;

  IGSJsonObject = interface;
  IGSJsonObject = interface
    function Put(const Name: String; const Value: Boolean): TJsonValue; overload;
    function Put(const Name: String; const Value: Integer): TJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TJsonValue; overload;
    function Put(const Name: String; const Value: String): TJsonValue; overload;
    function Stringify : string;
    procedure parser(aJsonString : String);
    function get(const name : String) : String;
    function getAsValue(const name : String) : TJsonValue;
    function getAsArray(const name : String) : TJsonArray;
    function exists(const name : string) : boolean;
  end;

  TGSJsonImpl = class(TInterfacedObject, IGSJsonObject)
  private
    FJson : TGSJSONObject;
  public
    constructor Create; virtual;
    destructor Destroy; Override;

    //IGSJsonObject
    function Put(const Name: String; const Value: Boolean): TJsonValue; overload;
    function Put(const Name: String; const Value: Integer): TJsonValue; overload;
    function Put(const Name: String; const Value: Extended): TJsonValue; overload;
    function Put(const Name: String; const Value: String): TJsonValue; overload;
    function Stringify : string;
    procedure parser(aJsonString : String);
    function get(const name : String) : String;
    function getAsValue(const name : String) : TJsonValue;
    function getAsArray(const name : String) : TGSJsonArray;
    function exists(const name : string) : boolean;
  end;

  TGSJsonTool = class
    class function JSONStrToDateTime(aJsonStr : string) : TDateTime;
    class function DataTimeToJSONStr(aDataTime : TDateTime) : String;
  end;

implementation


{ TGSJson }

{$IFDEF BCPBASE64}
class function TGSJson.Base64StringToBytes(const aBase64String: String): TBytes;
begin
  result := base64.Decode(aBase64String);
end;

class function TGSJson.BytesToBase64String(const aBytes: TBytes): String;
begin
  result := base64.Encode(aBytes);
end;
{$ENDIF}
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

function TGSJson.GetJson: TJson;
begin
  result := TJson(Self);
end;

class procedure TGSJson.Init;
begin
  FListOfProperty := Nil;
{$IFDEF BCPBASE64}
  base64 := TBase64.Create;
{$ENDIF}
end;

class function TGSJson.JSONDateToString(aDate: TDateTime): String;
begin
  result := jsonsUtilsEx.JSONDateToString(aDate);
end;

class function TGSJson.JSONStringToDate(aDate: String): TDateTime;
begin
  result := jsonsUtilsEx.JSONStringToDate(aDate);
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
{$IFDEF BCPBASE64}
  FreeAndNil(base64);
{$ENDIF}
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

{ TODO : Put a function to pretiffy, or dumb a json stqrting from the fellowing code (to refactor in a recursive manner)
var l : TStringStream;
    ljson : TJson;
    ljv : TJsonbase;
    i : integer;
    lpname : string;

    procedure InternalJsonValueProcess(aValue : TJSonValue; const aName : String = '');
    begin
      case aValue.ValueType of
        jvNone,jvNull : raise Exception.Create('todo');
        jvString :  FQuery.parameters.addStringValue(aValue.AsString,aName);
        jvArray : raise Exception.Create('todo');
        jvNumber :  FQuery.parameters.addIntegerValue(Round(aValue.AsNumber),aName);
        jvBoolean : FQuery.parameters.addBooleanValue(aValue.AsBoolean,aName);
        jvObject : raise Exception.Create('todo');
      end;
    end;

begin
  l :=  TStringStream.Create;
  try
    l.LoadFromStream(stream);
    ljson := TJson.Create;
    try
      ljson.Parse(l.DataString);

      case ljson.StructType of
        jsNone: ;
        jsArray: ;
        jsObject:
        begin
          decodeF
        end;
      end;

      for i := 0 to ljson.Count-1 do
      begin
        ljv := ljson.Get(i);
        if ljv is TJsonValue then
        begin
          InternalJsonValueProcess(TJSonValue(ljv));
        end
        else
        if ljv is TJsonPair then
        begin
          lpname :=  TJsonPair(ljv).Name;
          InternalJsonValueProcess(TJSonPair(ljv).Value,lpName);
        end
        else
        if ljv is TJsonArray then
        begin
          raise Exception.Create('Error Message');
        end
        else
        if ljv is TJsonObject then
        begin
          raise Exception.Create('Error Message');
        end;
     end;
    finally
      FreeAndNil(ljson);
    end;
  finally
    FreeAndNil(l);
  end;
end;
}

{ TGSJsonImpl }

constructor TGSJsonImpl.Create;
begin
  inherited;
  FJson := TGSJSONObject.Create;
end;

destructor TGSJsonImpl.Destroy;
begin
  FreeAndNil(FJson);
  inherited;
end;


function TGSJsonImpl.exists(const name: string): boolean;
begin
  result := FJson.Find(name)>-1;
end;

function TGSJsonImpl.get(const name: String): String;
begin
  result := FJson.Values[name].AsString;
end;

function TGSJsonImpl.getAsArray(const name: String): TGSJsonArray;
begin
  result := FJson.Values[name].AsArray;
end;

function TGSJsonImpl.getAsValue(const name: String): TJsonValue;
begin
  result := FJson.Values[name];
end;

procedure TGSJsonImpl.parser(aJsonString: String);
begin
  FJson.Parse(aJsonString);
end;

function TGSJsonImpl.Put(const Name: String; const Value: Boolean): TJsonValue;
begin
  result := FJson.Put(Name,Value);
end;

function TGSJsonImpl.Put(const Name: String; const Value: Integer): TJsonValue;
begin
  result := FJson.Put(Name,Value);
end;

function TGSJsonImpl.Put(const Name: String; const Value: Extended): TJsonValue;
begin
  result := FJson.Put(Name,Value);
end;

function TGSJsonImpl.Put(const Name, Value: String): TJsonValue;
begin
  result := FJson.Put(Name,Value);
end;

function TGSJsonImpl.Stringify: string;
begin
  result := FJson.Stringify;
end;

{ TGSJSonTool }

class function TGSJSonTool.DataTimeToJSONStr(aDataTime: TDateTime): String;
begin
  result := JSONDateToString(aDataTime);
end;

class function TGSJSonTool.JSONStrToDateTime(aJsonStr: string): TDateTime;
begin
  if aJsonStr.Trim.Length=0 then
    result := 0.0
  else
    result := JSONStringToDate(aJsonStr);
end;


Initialization

TGSJson.Init;

Finalization

TGSJson.Release;

end.
