///-------------------------------------------------------------------------------
/// Title      : GS.Stream
/// Short Desc : Lowlevel stream type write.
/// Source     : https://github.com/VincentGsell
/// Aim        : - Store type in stream in fast and easy way.
///-------------------------------------------------------------------------------
unit GS.Stream;

{$ifdef FPC}
{$mode delphi}
{$endif}

interface

{$ifdef FPC}
uses Sysutils, Classes;
{$else}
uses System.Sysutils, System.Classes;
{$endif}

procedure WriteDateTime(Stream: TStream; const Value: TDateTime);
procedure WriteLongInt(Stream: TStream; const Value: LongInt);
procedure WriteInteger(Stream: TStream; const Value: Integer);
procedure WriteInt32(Stream: TStream; const Value: Int32);
procedure WriteInt64(Stream: TStream; const Value: Int64);
procedure WriteUInt64(Stream: TStream; const Value: UInt64);
procedure WriteUInt32(Stream: TStream; const Value: UINT32);
procedure WriteByte(Stream: TStream; const Value: Byte);
procedure WriteBoolean(Stream: TStream; const Value: Boolean);
procedure WriteDouble(Stream: TStream; const Value: Double);
function ReadLongInt(Stream: TStream): LongInt;
function ReadInteger(Stream: TStream): Integer;
function ReadInt32(Stream: TStream): Int32;
function ReadInt64(Stream: TStream): Int64;
function ReadUInt64(Stream: TStream): UInt64;
function ReadUINT32(Stream: TStream): UInt32;
function ReadByte(Stream: TStream): Byte;
function ReadBoolean(Stream: TStream): Boolean;
function ReadDouble(Stream: TStream): Double;
function ReadDateTime(Stream: TStream): TDateTime;

procedure WriteString(Stream: TStream; Const Data: String);
function ReadString(Stream: TStream): String;

Procedure WriteStream(Stream : TStream; const SourceStream : TMemoryStream);
Procedure ReadStream(Stream : TStream; var DestinationStream : TMemoryStream);

implementation


procedure WriteDateTime(Stream: TStream; const Value: TDateTime);
begin
  Stream.Write(Value, SizeOf(TDateTime));
end;

procedure WriteLongInt(Stream: TStream; const Value: LongInt);
begin
  Stream.Write(Value, SizeOf(LongInt));
end;

procedure WriteInteger(Stream: TStream; const Value: Integer);
begin
  Stream.Write(Value, SizeOf(Integer));
end;
procedure WriteInt32(Stream: TStream; const Value: Int32);
begin
  Stream.Write(Value, SizeOf(Int32));
end;

procedure WriteInt64(Stream: TStream; const Value: Int64);
begin
  Stream.Write(Value, SizeOf(Int64));
end;

procedure WriteUInt64(Stream: TStream; const Value: UInt64);
begin
  Stream.Write(Value, SizeOf(UInt64));
end;

procedure WriteUINT32(Stream: TStream; const Value: UINT32);
begin
  Stream.Write(Value, SizeOf(UINT32));
end;

procedure WriteByte(Stream: TStream; const Value: Byte);
begin
  Stream.Write(Value, 1);
end;

procedure WriteBoolean(Stream: TStream; const Value: Boolean);
var b : Byte;
begin
  b := 0;
  if Value then
    b := 1;
  Stream.Write(b, 1);
end;

procedure WriteDouble(Stream: TStream; const Value: Double);
begin
  Stream.Write(Value, SizeOf(Double));
end;

function ReadLongInt(Stream: TStream): LongInt;
begin
  Stream.Read(Result, SizeOf(LongInt));
end;

function ReadInteger(Stream: TStream): Integer;
begin
  Stream.Read(Result, SizeOf(Integer));
end;
function ReadInt32(Stream: TStream): Int32;
begin
  Stream.Read(Result, SizeOf(Int32));
end;

function ReadInt64(Stream: TStream): Int64;
begin
  Stream.Read(Result, SizeOf(Int64));
end;

function ReadUInt64(Stream: TStream): UInt64;
begin
  Stream.Read(Result, SizeOf(UInt64));
end;

function ReadUINT32(Stream: TStream): UINT32;
begin
  Stream.Read(Result, SizeOf(UINT32));
end;

function ReadByte(Stream: TStream): Byte;
begin
  Stream.Read(Result, 1);
end;

function ReadBoolean(Stream: TStream): Boolean;
var b : Byte;
begin
  Stream.Read(b, 1);
  Result := b > 0;
end;

function ReadDouble(Stream: TStream): Double;
begin
  Stream.Read(Result, SizeOf(Double));
end;


function ReadDateTime(Stream: TStream): TDateTime;
begin
  Stream.Read(Result, SizeOf(TDateTime));
end;

procedure WriteString(Stream: TStream; Const Data: String);
var b : TStringStream;
    l : UINT32;
{$IFDEF NEXTGEN}
   sEnc : String;
{$ELSE}
   sEnc : UTF8String;
{$ENDIF}
begin
  {$IFDEF NEXTGEN}
   b := TStringStream.Create(Data,TEncoding.UTF8);
  {$ELSE}
   sEnc := UTF8String(Data);
   b := TStringStream.Create(sEnc);
  {$ENDIF}
  try
    l := b.Size;
    Stream.Write(l,SizeOf(UINT32));
    Stream.CopyFrom(b,b.Size);
  finally
    FreeAndNil(b);
  end;
end;

function ReadString(Stream: TStream): String;
var
  b : TStringStream;
  i : UINT32;
 {$IFDEF NEXTGEN}
 var sEnc : String;
 {$ELSE}
 var sEnc : UTF8String;
 {$ENDIF}
begin
  sEnc := ' '; //not empty ! : Force encoding.
  Stream.read(i,sizeOf(UINT32));
  if i= 0 then
  begin
    Result := EmptyStr;
  end
  else
  begin
    {$IFDEF NEXTGEN}
    b := TStringStream.Create(sEnc,TEncoding.UTF8);
    {$ELSE}
    b := TStringStream.Create(sEnc);
    {$ENDIF}
    try
      b.CopyFrom(Stream,i);
      Result := b.DataString;
    finally
      FreeAndNil(b);
    end;
  end;
end;


Procedure WriteStream(Stream : TStream; const SourceStream : TMemoryStream);
begin
  WriteInt64(Stream, SourceStream.Size-SourceStream.Position);
  Stream.CopyFrom(SourceStream,SourceStream.Size-SourceStream.Position);
end;

Procedure ReadStream(Stream : TStream; var DestinationStream : TMemoryStream);
var il : Int64;
begin
  il := ReadInt64(Stream);
  DestinationStream.CopyFrom(Stream,il);
end;



end.
