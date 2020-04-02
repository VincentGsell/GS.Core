{$I GSCore.Inc}

unit GS.Pixel32.PixelShader;

interface

uses sysutils, classes,
     GS.Pixel, GS.Pixel32;

Type
TP32UVMap = record
  u : single;
  v : Single;
end;

TPixel32ShaderSquaredMotif = class(TPixel32ColorShader)
protected
  ff, fm : TP32;
  fl : Integer;
public
  constructor create(surface : iPixSurface); reintroduce;
  procedure SetDataColor(_colorFill, _ColorMotif : TP32; const SquareLen : Integer = 5);
  procedure process; override;
end;


implementation

{ TPixel32ShaderSquaredMotif }

constructor TPixel32ShaderSquaredMotif.create(surface : iPixSurface);
begin
  inherited create(surface);
  fl := 5;
  SetDataColor(gspWhite,gspBlue,fl);
end;

procedure TPixel32ShaderSquaredMotif.process;
begin
  Color := ff;
  if fx mod fl = 1 then
    Color := fm
  else
  if fy mod fl = 1 then
    Color := fm;

  inherited;
end;

procedure TPixel32ShaderSquaredMotif.SetDataColor(_colorFill, _ColorMotif: TP32;
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
