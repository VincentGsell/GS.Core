unit GS.Pixel32.PixelShader;

interface

uses sysutils, classes,
     GS.Pixel, GS.Pixel32;

Type
TP32UVMap = record
  u : single;
  v : Single;
end;

TPixel32GouraudShader = class(TPixel32ColorShader)
protected
  fva,fvb,fvc : TP32Vertex;
  fca,fcb,fcc : TP32;
public
  procedure setVertexAndColor(VertexA,VertexB,VertexC : TP32Vertex; ca,cb,cc : TP32);
  procedure process; override;
end;


TPixel32SquaredMotif = class(TPixel32ColorShader)
protected
  ff, fm : TP32;
  fl : Integer;
public
  Constructor create; virtual;
  procedure SetDataColor(_colorFill, _ColorMotif : TP32; const SquareLen : Integer = 5);
  procedure process; override;
end;


implementation

{ TPixel32GouraudShader }

procedure TPixel32GouraudShader.process;
var colorcoef1,colorcoef2,colorcoef3 : single;
    c : TP32;
begin
  colorcoef1 := (abs(fx - fva.x)/fva.x);
  colorcoef2 := abs(fx - fvb.x)/fvb.x;
  colorcoef3 := abs(fx - fvc.x)/fvc.x;

  c := Trunc((fca * colorcoef1) + (fcb * colorcoef2) + (fcc * colorcoef3)) div 3;

  Color := c;

  inherited;
end;

procedure TPixel32GouraudShader.setVertexAndColor(VertexA, VertexB,
  VertexC: TP32Vertex; ca, cb, cc: TP32);
begin
  fva := VertexA;
  fvb := VertexB;
  fvc := VertexC;
  fca := ca;
  fcb := cb;
  fcc := cc;
end;

{ TPixel32SquaredMotif }

constructor TPixel32SquaredMotif.create;
begin
  inherited;
  fl := 5;
end;

procedure TPixel32SquaredMotif.process;
begin
  Color := ff;
  if fx mod fl = 1 then
    Color := fm
  else
  if fy mod fl = 1 then
    Color := fm;

  inherited;
end;

procedure TPixel32SquaredMotif.SetDataColor(_colorFill, _ColorMotif: TP32;
  const SquareLen: Integer);
begin
  ff := _colorFill;
  fm := _ColorMotif;
  if SquareLen<3 then
    fl := 3
  else
    fl := SquareLen;
end;

end.
