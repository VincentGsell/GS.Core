//Bridge between Pixel32 and Angus Johnson's Image32 backend service.
unit GS.Pixel32.Service.Image32;

interface

uses classes,
     sysutils,
     GS.Geometry,
     GS.Pixel32,
     GS.Pixel32.Types,
     GS.Pixel.Service,
     GS.Stream,
     GS.Pixel32.Service.Image32.Types;

Type

TPixel32CustomImage32Service = class(TPixelCustomService)
protected
  furi : string;
public
  Constructor Create; reintroduce;
end;


TPixel32Image32Service = Class(TPixel32CustomImage32Service)
protected
  procedure InternalDecodeGetPath(data : TStream; out response : TGetPathData);
public
  //RnD exemple....
  procedure StarsPath(InnerRadius, OuterRadius, Edge : Uint32);
  procedure Answer(content : TStream; success : boolean; report : TStream); override;
End;


implementation

{ TPixel32CustomImage32Service }

constructor TPixel32CustomImage32Service.Create;
begin
  inherited create(CST_IMAGE32_URI);
  furi := trim(lowercase(CST_IMAGE32_URI));
end;

{ TPixel32Image32Service }

procedure TPixel32Image32Service.InternalDecodeGetPath(data: TStream;
  out response: TGetPathData);
begin

end;

procedure TPixel32Image32Service.StarsPath(InnerRadius, OuterRadius, Edge : Uint32);
const CST_LINE_OPCODE : byte = 200;
var l : TMemoryStream;
begin
  l := TMemoryStream.Create;
  try
//    WriteBoolean(l,false); //retain mode, no bus return
    WriteByte(l,CST_GETPATH_OPCODE);
    WriteInteger(l,InnerRadius);
    WriteInteger(l,OuterRadius);
    WriteInteger(l,Edge);
    Ask(l);
  finally
    FreeAndNil(l);
  end;
end;

procedure TPixel32Image32Service.Answer(content: TStream;
  success: boolean; report: TStream);
var Path : TGetPathData;
begin
  if success then
  begin
    if content.Size>0 then
    begin
      //OpCode first
      case ReadUint32(content) of
      CST_GETPATH_OPCODE :
      begin
        InternalDecodeGetPath(content, path);
      end;
      end;
    end;
  end
  else
  begin
    raise Exception.Create('TODO : DECODE REPORT ');
  end;
end;



end.
