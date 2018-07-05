program ProjectJSONDemo;

{$APPTYPE CONSOLE}

{.$DEFINE DELPHINATIVEJSON} //Activate if you want to use native REST.Json on delphi.
{$IFDEF FPC}
 {.$DEFINE DELPHINATIVEJSON} //By default
{$ENDIF}

{$R *.res}

uses
  {$IFNDEF FPC}
  FastMM4,
  {$ENDIF }
  SysUtils,
  Classes,
  {$IFDEF DELPHINATIVEJSON}
  Rest.Json,
  {$ELSE}
  GS.JSON,
  {$ENDIF }
  unElementobject in '..\..\data\unElementobject.pas';

var lo : TPeriodicTable;
    s :  TStringList;
    lTxt : string;
    lpath : string;
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}


  lpath := ExtractFilePath(ParamStr(0)) +'..\..\..\..' +'\data\PeriodicTableJSON.json.';

  try
    s :=  TStringList.Create;
    try
      s.LoadFromFile(lpath);
      lTxt := s.Text;
    finally
      freeAndNil(s);
    end;

   {$IFDEF DELPHINATIVEJSON}
    lo := Rest.Json.TJSON.JsonToObject<TPeriodicTable>(lTxt);
   {$ELSE}
    ///REGISTERING...
   //This register information for marshalling operation : JsonToObject.
   //It will be used to get information for answer to : "What class instance for this array list ?".
   //This for FPC, which has currently weaker rtti, but this Work in delphi too, but it isuseless in this case,
   //because delphi's RTTI reconizes Element item class.
   //We register item's class of array property "Elements"
   TGSJson.Configuration.RegisterPropertyArrayTypes('Elements',TElement);
    ///...End of REGISTERING.

    lo := TPeriodicTable.Create; //We create master object first....
    TGSJson.JsonToObject(lTxt,TObject(lo)); //Then try to convert. The conversion fellow object structure, not the json structure.
    {$ENDIF}

    Writeln(lo.ClassName+ ' loaded : '+lo.ClassName+' '+IntTostr(Length(lo.Elements))+' element(s)');

    //Operation : We put a *copy* of the first element "H" to the property "FirstElem"
    if Length(lo.Elements)>0 then
    begin
      lo.FirstElement := TElement.Create;
      lo.FirstElement.Name := lo.Elements[0].Name;
      //... and so on !

      //We update the date to
      lo.DateTest := Now;
    end;


    try
     {$IFDEF DELPHINATIVEJSON}
     ltxt := Rest.Json.TJson.ObjectToJsonString(lo);
     {$ELSE}
     ltxt := TGSJson.ObjectToJson(lo);
     {$ENDIF}
     Writeln(ltxt);

      s :=  TStringList.Create;
      try
        s.Text := lTxt;
       {$IFDEF DELPHINATIVEJSON}
        s.SaveToFile(lpath+'.DelphiNativeDemoSaved.json');
       {$ELSE}
         {$IFDEF FPC}
       s.SaveToFile(lpath+'.GSJsonFPCDemoSaved.json');
         {$ELSE}
       s.SaveToFile(lpath+'.GSJsonDELPHIDemoSaved.json');
         {$ENDIF}
       {$ENDIF}
      finally
        freeAndNil(s);
      end;


      readln;
    finally
      FreeAndNil(lo);
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      readln;
    end;
  end;
end.
