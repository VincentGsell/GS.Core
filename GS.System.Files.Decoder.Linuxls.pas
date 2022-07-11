unit GS.System.Files.Decoder.Linuxls;
//
/// This file decode and build a structured view for this kind of order :
///    ls -lart --full-time -R
/// with something like "ls -lart --full-time -R > ../../dts.txt"
/// you can decode dts.txt file with stuffs below.

interface

uses SysUtils,
     classes,
     system.Generics.Collections,
     GS.Common,
     GS.JSON;

Type

TidlsItemKind = (lsdirectory, lsfile);
TidlsItem = record
  Kind : TidlsItemKind;
  LastUpdate : TDateTime;
  gmt : string;
  Path : String;
  Name : String;
  size : Uint32;
end;

IGSLinuxlsDecoder = interface
  procedure process(const atxtresult : String; var aResult : TArray<TidlsItem>; var aErrorDesc : string);
  function toJson(const aArray : TArray<TidlsItem>) : string;
end;

TGSLinuxlsDecoder = class(TInterfacedObject, IGSLinuxlsDecoder)
  procedure process(const atxtresult : String; var aResult : TArray<TidlsItem>; var aErrorDesc : string);
  function toJson(const aArray : TArray<TidlsItem>) : string;
end;

implementation

{ TLinuxlsDecoder }

procedure TGSLinuxlsDecoder.process(const atxtresult: String;
  var aResult: TArray<TidlsItem>; var aErrorDesc: string);
var ll,llo : IGSStringList;
    i,j : integer;
    ls : string;
    li : TidlsItem;
    lindex : integer;
    lcurrentPath, lyear, ltime, lgmt : string;
    lfs: TFormatSettings;
begin
  {$IFDEF DEBUG}
  var lll : IGSStringList;
  lll := TGSStringList.Create;
  lll.setText(atxtresult);
  lll.saveToFile('TestcdsSysFileList_GrossStringData.txt');
  {$ENDIF}
  ll := TGSStringList.Create;
  llo := TGSStringList.Create;
  llo.SetDelimiter(' ');
  ll.setText(atxtresult);
  lcurrentPath := '';
  for i := 0 to ll.count-1 do begin
    ls := ll.lines(i);
    if ls.Trim.Length=0 then
      continue;
    llo.SetDelimitedText(ls);
    for j := 0 to llo.count-1 do
      llo.lines(j,llo.lines(j).Trim);
    ls := llo.lines(0);
    if ls.Trim.Length=0 then
      continue;
    if pos('data/datafiles',ls)>0 then begin
      lcurrentPath := ls.Split(['/'])[2].Replace(':','');
    end
    else begin
      if llo.count<9 then
        continue;
      if ((ls[1]='d') and (llo.lines(8)<>'..')) or (ls[1]='-') then begin
        li.Kind := TidlsItemKind.lsdirectory;
        if (ls[1]='-') then
          li.Kind := TidlsItemKind.lsfile;
        li.size := StrToIntDef(llo.lines(4),0);
        lyear := llo.lines(5);
        ltime := llo.lines(6).Split(['.'])[0];
        lgmt := llo.lines(7);
        lfs := FormatSettings;
        lfs.DateSeparator := '-';
        lfs.ShortDateFormat := 'yyyy-mm-dd';
        lfs.TimeSeparator := ':';
        lfs.ShortTimeFormat := 'hh:nn:zz';
        li.LastUpdate := StrToDate(lyear,lfs);
        li.LastUpdate := li.LastUpdate + StrToTime(ltime,lfs);
        li.gmt := lgmt;
        li.Name := llo.lines(8);
        li.Path := lcurrentPath;
        lindex := length(aResult);
        SetLength(aResult,lindex+1);
        aResult[lindex] := li;
      end;
    end;
  end;

end;

function TGSLinuxlsDecoder.toJson(const aArray : TArray<TidlsItem>): string;
var ljo : TGSJsonObject;
    lja : TGSJsonArray;
    li : TidlsItem;
begin
  lja := TGSJsonArray.Create;
  try
    if Length(aArray)=0 then begin
      result := lja.Stringify;
      Exit;
    end;

    for li in aArray do begin
      ljo := lja.Add.AsObject;
      ljo.put('kind',integer(li.Kind));
      ljo.put('lastupdate',single(li.LastUpdate));
      ljo.put('gmt',li.gmt);
      ljo.put('path',li.Path);
      ljo.put('name',li.Name);
      ljo.put('size',li.size);
    end;
    result := lja.Stringify;
  finally
    FreeAndNil(lja);
  end;

end;

end.
