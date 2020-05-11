//Bridge between Pixel32 and Angus Johnson's Image32 backend service.
unit GS.Pixel32.Service.Image32;

interface

uses classes, sysutils, GS.Pixel32, GS.Pixel.Service, GS.Stream;

Type

TPixel32CustomPXLService = class(TPixelCustomService)
protected
  furi : string;
public
  Constructor Create(_uri : string); reintroduce;
end;


TPixel32PXLService = Class(TPixel32CustomPXLService)
protected
public
  //RnD exemple....
  procedure Line(x,y,xx,yy : integer; color : TP32);
End;


implementation

{ TPixel32CustomPXLService }

constructor TPixel32CustomPXLService.Create(_uri: string);
begin
  assert(length(trim(_uri))>0);
  inherited create(_uri);
  furi := trim(lowercase(_uri));
end;

{ TPixel32PXLService }

procedure TPixel32PXLService.Line(x, y, xx, yy: integer; color: TP32);
const CST_LINE_OPCODE : byte = 200;
var l : TMemoryStream;
begin
  l := TMemoryStream.Create;
  try
    WriteBoolean(l,false); //retain mode, no bus return
    WriteByte(l,CST_LINE_OPCODE);
    WriteInteger(l,x);
    WriteInteger(l,y);
    WriteInteger(l,xx);
    WriteInteger(l,yy);
    WriteInteger(l,Color);
    Ask(l);
  finally
    FreeAndNil(l);
  end;
end;

end.
