{$I GSCore.Inc}

unit GS.Pixel32.Fragments;

interface

uses sysutils, classes, Windows, Math,
     GS.Geometry,
     GS.Pixel,
     GS.Pixel32.types,
     GS.Pixel32;

Type
TPixel32FragmentShaderVerticeColor = Class(TPixel32FragmentShader)
public
  procedure process; override;
End;

TPixel32FragmentShaderTextureColor = class(TPixel32FragmentShader)
protected
  fTexture: TPixel32;
public
  property Texture : TPixel32 read fTexture write fTexture;
  procedure process; override;
end;

TPixel32ShaderSquaredMotif = class(TPixel32FragmentShaderFlatColor)
private
protected
  ff, fm : TP32;
  fl : Integer;
  procedure setSquareLen(const Value: integer);
public
  constructor create; reintroduce;
  procedure process; override;

  property colorFill : TP32 read ff write ff;
  property colorMotif : TP32 read fm write fm;
  property Squarelen : integer read fl write setSquareLen;
end;

TPixel32ShaderWithGlobalAlphaBlending = Class(TPixel32FragmentShaderFlatColor)
private
protected
  fAlpha: byte;
public
  Constructor Create(const alphavalue : byte = 255); Reintroduce; virtual;
  property AlphaChannel : byte read fAlpha write fAlpha;
End;

TPixel32FragmentShaderCheckerBoard = class(TPixel32ShaderWithGlobalAlphaBlending)
protected
public
  procedure process; override;
end;

TPixel32ShaderRandomizer = class(TPixel32ShaderWithGlobalAlphaBlending)
protected
public
  procedure process; override;
end;

TPixel32ShaderColorTest = Class(TPixel32ShaderWithGlobalAlphaBlending)
private
protected
public
  procedure process; override;
End;

TPixel32ShaderPlasma = class(TPixel32ShaderWithGlobalAlphaBlending)
public
  procedure process; override;
end;

TPixel32ShaderStaticTexture = class(TPixel32ShaderWithGlobalAlphaBlending)
private
protected
  FTileTexture: TPixel32;
public
  Constructor Create(TileTexture : TPixel32; const alphavalue : byte = 255); Reintroduce;
  property Texture : TPixel32 read FTileTexture Write FTileTexture;
end;

TPixel32ShaderStaticTextureStretchOnSurface = class(TPixel32ShaderStaticTexture)
public
  procedure process; override;
end;

TPixel32ShaderStaticTextureTiledOnSurface = class(TPixel32ShaderStaticTexture)
public
  procedure process; override;
end;



implementation

{ TPixel32FragmentShaderTextureColor }

procedure TPixel32FragmentShaderTextureColor.process;
var sSurfaceColor : TP32Rec; //Current surface's target color.
    fcolorAsRec : TP32Rec;
    tb : pTP32;
    u,v : single;
    ui,vi : integer;
begin
  assert(assigned(fTexture));
  u := interpolationVerticeA * VerticeA^.u + interpolationVerticeB * VerticeB^.u + interpolationVerticeC * VerticeC^.u;
  v := interpolationVerticeA * VerticeA^.v + interpolationVerticeB * VerticeB^.v + interpolationVerticeC * VerticeC^.v;

  if zProc <>0 then
  begin
//    u := u * zProc; //TODO Perspective correction.
//    v := v * zProc;
  end;

  ui := Trunc(u * fTexture.width);
  vi := Trunc(v * fTexture.height);


  tb := fTexture.getSurfacePtr;
  inc(tb,vi*fTexture.width+ui);


  if TP32rec(tb^).AlphaChannel<255 then
  begin
    fcolorAsRec.Color := tb^;
    sSurfaceColor.Color := sourceSurfaceBits^;
    _processedColor.red:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Red - sSurfaceColor.Red) shr 8) + (sSurfaceColor.Red);
    _processedColor.Blue:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Blue - sSurfaceColor.Blue) shr 8) + (sSurfaceColor.Blue);
    _processedColor.Green:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Green - sSurfaceColor.Green) shr 8) + (sSurfaceColor.Green);
  end
  else
    _processedColor.Color := tb^;
end;

{ TPixel32FragmentShaderVerticeColor }

procedure TPixel32FragmentShaderVerticeColor.process;
var sSurfaceColor : TP32Rec; //Current surface's target color.
    fcolorAsRec : TP32Rec;
    r,g,b,a : single;
begin
  r := interpolationVerticeA * VerticeA^.rgba.r + interpolationVerticeB * VerticeB^.rgba.r + interpolationVerticeC * VerticeC^.rgba.r;
  g := interpolationVerticeA * VerticeA^.rgba.g + interpolationVerticeB * VerticeB^.rgba.g + interpolationVerticeC * VerticeC^.rgba.g;
  b := interpolationVerticeA * VerticeA^.rgba.b + interpolationVerticeB * VerticeB^.rgba.b + interpolationVerticeC * VerticeC^.rgba.b;
  a := interpolationVerticeA * VerticeA^.rgba.a + interpolationVerticeB * VerticeB^.rgba.a + interpolationVerticeC * VerticeC^.rgba.a;

  //Perspective correctoi
  if zProc > 0 then
  begin
    r := r*_z;
    g := g*_z;
    b := b*_z;
    a := a*_z;
  end;

  fcolorAsRec.Color := TPixel32(sourceSurface).colorP32Rec(trunc(r*255),trunc(g*255),trunc(b*255),trunc(a*255)).Color;
  sSurfaceColor.Color := sourceSurfaceBits^;

  _processedColor.red:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Red - sSurfaceColor.Red) shr 8) + (sSurfaceColor.Red);
  _processedColor.Blue:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Blue - sSurfaceColor.Blue) shr 8) + (sSurfaceColor.Blue);
  _processedColor.Green:=(fcolorAsRec.AlphaChannel * (fcolorAsRec.Green - sSurfaceColor.Green) shr 8) + (sSurfaceColor.Green);
end;

{ TPixel32FragmentShaderCheckerBoard }

procedure TPixel32FragmentShaderCheckerBoard.process;
const M : Integer = 10;
var s,t,p : single;
    st0, st1, st2 : Vec2;
begin
  st2.create(0,0);
  st1.create(1,0);
  st0.create(1,1);

  s := interpolationVerticeA * st0.x + interpolationVerticeB * st1.x + interpolationVerticeC * st2.x;
  t := interpolationVerticeA * st0.y + interpolationVerticeB * st1.y + interpolationVerticeC * st2.y;

  //Perspective Correction.
  if zProc > 0 then
  begin
    s := s*_z;
    t := t*_z;
  end;

  p := _pow(integer(_fmods(s*M,1.0)>0.5),integer(_fmods(t*M,1.0)<0.5));

  fColor := TPixel32(sourceSurface).colorP32Rec(round(p*255),round(p*255),round(p*255),100).Color;

  inherited;
end;


{ TPixel32ShaderSquaredMotif }

constructor TPixel32ShaderSquaredMotif.create;
begin
  inherited create;
  fl := 5;
  ff := gspWhite;
  fm := gspBlue;
end;

procedure TPixel32ShaderSquaredMotif.process;
begin
  _processedColor.Color := ff;
  if x mod fl = 1 then
    _processedColor.Color := fm
  else
  if y mod fl = 1 then
    _processedColor.Color := fm;

  fColor := _processedColor.Color; //for inherited call.
  inherited;
end;



procedure TPixel32ShaderSquaredMotif.setSquareLen(const Value: integer);
begin
  fl := Value;
  if fl<3 then
    fl := 3;
end;

{ TPixel32ShaderWithGlobalAlphaBlending }

constructor TPixel32ShaderWithGlobalAlphaBlending.Create(
  const alphavalue: byte);
begin
  inherited create;
  fAlpha := alphavalue;
end;

{ TPixel32ShaderRandomizer }

procedure TPixel32ShaderRandomizer.process;
begin
  fColor := (sourceSurface as TPixel32).colorP32Rec(10+Random(245),10+Random(245),10+Random(245),fAlpha).Color;
  inherited;
end;

{ TPixel32ShaderColorTest }

//Inspired from fantastic delphi port of opencl shader :
//https://github.com/WouterVanNifterick/delphi-shader
procedure TPixel32ShaderColorTest.process;
var
  uv :Vec2;
  col:TP32Rec;

begin
	uv.x  := x / sourcesurface.width;
	uv.y  := y / sourcesurface.height;

	col.Red    := trunc( (_sqrts(_mix(-0.20,2,_dot(uv.x*2,uv.y*0.5)))+0.2) * 255);
	col.green  := trunc( (_sqrts(_mix(-0.20,2,_dot(1-uv.y,1-uv.x)))+0.2) * 255);
	col.blue   := trunc( (_sqrts(_mix(-0.20,2,_dot(1-uv.x,uv.y)))+0.2) * 255);
  col.alphachannel := fAlpha;

  fColor:= col.Color;

  inherited;
end;

{ TPixel32ShaderPlasma }

procedure TPixel32ShaderPlasma.process;

function sinLarge(const x: single): single;
begin
  {$IFDEF CPUX64}
  Result := System.sin(_Mod(x,2*pi));
  {$ELSE}
  Result := System.sin(x);
  {$ENDIF}
end;

function cosLarge(const x: single): Single;
begin
  {$IFDEF CPUX64}
  Result := System.cos(_Mod(x,2*pi));
  {$ELSE}
  Result := System.cos(x);
  {$ENDIF}
end;



var
  mov0, mov1, mov2, c1, c2, c3: Single;
  iGlobalTime : double;
  col : TP32Rec;
begin
  iGlobalTime :=  GetTickcount/1000;
  mov0   := x + y + System.cos(sinLarge(iGlobalTime) * 2) * 100. + System.sin(x / 100) * 1000;
  mov1   := y / sourcesurface.height / 0.2 + iGlobalTime;
  mov2   := x / sourcesurface.width / 0.2;
  c1     := System.abs(sinLarge(mov1 + iGlobalTime) / 2. + mov2 / 2. - mov1 - mov2 + iGlobalTime);
  c2     := System.abs(System.sin(c1 + sinLarge(mov0 / 1000 + iGlobalTime) + System.sin(y / 40 + iGlobalTime) + System.sin((x + y) / 100) * 3));
  c3     := System.abs(System.sin(c2 + cosLarge(mov1 + mov2 + c2) + System.cos(mov2) + System.sin(x / 1000)));
  col.Red := trunc(c1*255);
  col.Green := trunc(c2*255);
  col.Blue := trunc(c3*255);
  col.AlphaChannel := fAlpha;

  fColor := col.Color;

  inherited;
end;

{ TPixel32ShaderStaticTexture }

constructor TPixel32ShaderStaticTexture.Create(TileTexture: TPixel32;
  const alphavalue: byte);
begin
  assert(assigned(TileTexture));
  assert((TileTexture.width>1) and (TileTexture.height>1));
  inherited Create(alphavalue);
  FTileTexture :=  TileTexture;
end;


{ TPixel32ShaderStaticTextureStretchOnSurface }

procedure TPixel32ShaderStaticTextureStretchOnSurface.process;
var b : pTP32;
    tx,ty : integer;
    xscale,yscale : single;
begin
  b := FTileTexture.getSurfacePtr;
  //Texture is like stretching in surface size.
  xscale := FTileTexture.width/sourcesurface.width;
  yscale := FTileTexture.height/sourcesurface.height;
  tx := trunc(x*xscale);
  ty := trunc(y*yscale);

  inc(b,ty*FTileTexture.width+tx);
  fColor := b^;
  TP32Rec(fColor).AlphaChannel := fAlpha;
  inherited;
end;


{ TPixel32ShaderStaticTextureTiledOnSurface }

procedure TPixel32ShaderStaticTextureTiledOnSurface.process;
var b : pTP32;
    tx,ty : integer;
begin
  b := FTileTexture.getSurfacePtr;
  //Texture is untouched in size, and display tiled on surface size.
  tx := x mod FTileTexture.width;
  ty := y mod FTileTexture.height;

  inc(b,ty*FTileTexture.width+tx);
  fColor := b^;
  TP32Rec(fColor).AlphaChannel := fAlpha;
  inherited;
end;



end.
