{$I GSCore.Inc}

unit GS.Pixel32.PixelShader;

interface

uses sysutils, classes, Windows, Math,
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

TPixel32ShaderWithGlobalAlphaBlanding = Class(TPixel32ColorShader)
private
protected
  fAlpha: byte;
public
  Constructor Create(const alphavalue : byte = 255); Reintroduce;
  property AlphaChannel : byte read fAlpha write fAlpha;
End;


TPixel32ShaderRandomizer = class(TPixel32ShaderWithGlobalAlphaBlanding)
protected
public
  procedure process; override;
end;

TPixel32ShaderColorTest = Class(TPixel32ShaderWithGlobalAlphaBlanding)
private
protected
public
  procedure process; override;
End;

TPixel32ShaderPlasma = class(TPixel32ShaderWithGlobalAlphaBlanding)
public
  procedure process; override;
end;



implementation

{ TPixel32ShaderSquaredMotif }

constructor TPixel32ShaderSquaredMotif.create(surface : iPixSurface);
begin
  inherited create;
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


{ TPixel32ShaderWithGlobalAlphaBlanding }

constructor TPixel32ShaderWithGlobalAlphaBlanding.Create(
  const alphavalue: byte);
begin
  inherited create;
  fAlpha := alphavalue;
end;

{ TPixel32ShaderRandomizer }

procedure TPixel32ShaderRandomizer.process;
begin
//  color := TPixel32(fsurface).colorP32Rec(Random(255),Random(255),Random(255),255).Color;
  color := TPixel32(fsurface).colorP32Rec(100+Random(155),0,0,fAlpha).Color;
  inherited;
end;

{ TPixel32ShaderColorTest }

//Inspired from fantastic delphi port of opencl shader :
//https://github.com/WouterVanNifterick/delphi-shader
procedure TPixel32ShaderColorTest.process;
Type
  TVec2 = record x,y : single end;

function dot(a,b : single) : single;
begin
  result := a*b;
end;

function mix(x,y,a :single) : single;
begin
  result := x * (1 - a) + y * a;
end;

function sqrts(const a: single): single;
begin
  if a<0 then result := -1 else result := System.sqrt(a);
end;

var
  uv :TVec2;
  col:TP32Rec;

begin
	uv.x  := fx / fsurface.width;
	uv.y  := fy / fsurface.height;

	col.Red    := trunc( (sqrts(mix(-0.20,2,dot(uv.x*2,uv.y*0.5)))+0.2) * 255);
	col.green  := trunc( (sqrts(mix(-0.20,2,dot(1-uv.y,1-uv.x)))+0.2) * 255);
	col.blue   := trunc( (sqrts(mix(-0.20,2,dot(1-uv.x,uv.y)))+0.2) * 255);
  col.alphachannel := fAlpha;

  color := col.Color;

  inherited;
end;

{ TPixel32ShaderPlasma }

procedure TPixel32ShaderPlasma.process;

function sinLarge(const x: single): single;
begin
  {$IFDEF CPUX64}
  Result := System.sin(Math.FMod(x,2*pi));
  {$ELSE}
  Result := System.sin(x);
  {$ENDIF}
end;

function cosLarge(const x: single): Single;
begin
  {$IFDEF CPUX64}
  Result := System.cos(Math.FMod(x,2*pi));
  {$ELSE}
  Result := System.cos(x);
  {$ENDIF}
end;



var
  mov0, mov1, mov2, c1, c2, c3: Single;
  iGlobalTime : double;
  col : TP32Rec;
begin
  iGlobalTime :=  GetTickcount/10000;
  mov0   := fx + fy + System.cos(sinLarge(iGlobalTime) * 2) * 100. + System.sin(fx / 100) * 1000;
  mov1   := fy / fsurface.height / 0.2 + iGlobalTime;
  mov2   := fx / fsurface.width / 0.2;
  c1     := System.abs(sinLarge(mov1 + iGlobalTime) / 2. + mov2 / 2. - mov1 - mov2 + iGlobalTime);
  c2     := System.abs(System.sin(c1 + sinLarge(mov0 / 1000 + iGlobalTime) + System.sin(fy / 40 + iGlobalTime) + System.sin((fx + fy) / 100) * 3));
  c3     := System.abs(System.sin(c2 + cosLarge(mov1 + mov2 + c2) + System.cos(mov2) + System.sin(fx / 1000)));
  col.Red := trunc(c1*255);
  col.Green := trunc(c2*255);
  col.Blue := trunc(c3*255);
  col.AlphaChannel := fAlpha;

  Color := col.Color;

  inherited;
end;


end.
