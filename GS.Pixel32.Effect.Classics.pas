{ -----------------------------------------------------------------------------
    This program is free software: Under statement of join file README - LGPL.txt
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-----------------------------------------------------------------------------
 Unit Name : GS.Pixel32.Effect.Classics
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : Pixel Effect class.
 Date:     : 20200324
 History   :
 20200301 - Creating unit.

Credits :
//*Heavely* inspired by Anthony Walter's Pixels.pas unit.
//From project https://github.com/sysrpl/ImageShop

Description :
-----------------------------------------------------------------------------}
{$I GSCore.Inc}


unit GS.Pixel32.Effect.Classics;

interface

uses classes,
     sysutils,
     GS.Pixel32,
     GS.Pixel32.Types,
     GS.Pixel32.Effect,
     Pixels;

type
TCustomPixel32SurfaceEffectClassic = class(TCustomPixel32SurfaceEffect)
protected
  FLevel: Single;
public
  Constructor Create; virtual;

  property Level : Single read FLevel write FLevel;
end;

TPixelUnitOp = ( RedOperation,
                 GreenOperation,
                 BlueOperation,
                 SaturationOperation,
                 AlphaOperation,
                 BlackOrWhiteOperation,
                 BrightenOperation,
                 ContrastOperation,
                 DarkenOperation,
                 InvertOperation,
                 HueOperation
                );

TPixelUnitBlendOp = ( OpacityBlend,
                      DisolveBlend,
                      MultiplyBlend,
                      AdditionBlend,
                      SubtractionBlend,
                      WipeBlend,
                      WipeVBlend,
                      WipeDoubleBlend,
                      CircleBlend,
                      BlockBlend,
                      ScreenBlend,
                      OverlayBlend,
                      BurnBlend,
                      DodgeBlend
                    );

Const

  cPixelUnitOpEN : array[TPixelUnitOp] of string = ( 'red channel',
                                                     'green channel',
                                                     'blue channel',
                                                     'saturation',
                                                     'alpha channel',
                                                     'black or white',
                                                     'brighten',
                                                     'contrast',
                                                     'darken',
                                                     'invert',
                                                     'hue');

  cPixelUnitBlendOpEN : array[TPixelUnitBlendOp] of string = ( 'opacity',
                                                               'disolve',
                                                               'multiply',
                                                               'addition',
                                                               'subtraction',
                                                               'wipe',
                                                               'wipev',
                                                               'wipedouble',
                                                               'circle',
                                                               'blocks',
                                                               'screen',
                                                               'overlay',
                                                               'burn',
                                                               'dodge');


Type

TPixel32SurfaceEffectClassicOperations = class(TCustomPixel32SurfaceEffectClassic)
private
protected
  FOp : array of Pixels.TPixelOperation;
  FCurrentOp : TPixelUnitOp;
  procedure addInternalPixelUnitOperation(const Name: string; Proc: TPixelOperation);
Public
  Constructor Create; virtual;
  procedure setOpParameter(askLevel : single; op : TPixelUnitOp);
  procedure process; override;
end;

TPixel32SurfaceEffectClassicBlendOps = class(TCustomPixel32SurfaceEffectClassic)
private
protected
  FSurfaceDest : TPixel32;
  FOp : array of Pixels.TPixelBlend;
  FCurrentOp : TPixelUnitBlendOp;
  procedure addInternalPixelUnitBlendOperation(const Name: string; Proc: TPixelBlend);
Public
  Constructor Create; virtual;
  procedure setOpParameter(askLevel : single; op : TPixelUnitBlendOp; surfaceDest : TPixel32);
  procedure process; override;
end;


function opNameToOp(name : String; var opType : TPixelUnitOp) : boolean;
function opNameToBlendOp(name : String; var opType : TPixelUnitBlendOp) : boolean;

implementation

function opNameToOp(name : String; var opType : TPixelUnitOp) : boolean;
var l,m : string;
    i : integer;
begin
  result := false;
  m := trim(LowerCase(name));
  i := 0;
  for l in cPixelUnitOpEN do
  begin
    if l = m  then
    begin
      opType := TPixelUnitOp(i);
      result := true;
      break;
    end;
    inc(i);
  end;
end;

function opNameToBlendOp(name : String; var opType : TPixelUnitBlendOp) : boolean;
var l,m : string;
    i : integer;
begin
  result := false;
  m := trim(LowerCase(name));
  i := 0;
  for l in cPixelUnitBlendOpEN do
  begin
    if l = m  then
    begin
      opType := TPixelUnitBlendOp(i);
      result := true;
      break;
    end;
    inc(i);
  end;
end;


{ TPixel32SurfaceEffectClassicOperations }

procedure TPixel32SurfaceEffectClassicOperations.addInternalPixelUnitOperation(const Name: string; Proc: TPixelOperation);
var id : integer;
begin
  id := length(FOp);
  SetLEngth(Fop,id+1);;
  Fop[id] := Proc;
end;

constructor TPixel32SurfaceEffectClassicOperations.Create;
begin
  inherited Create;
  InitializeOperations(addInternalPixelUnitOperation);
  FCurrentOp := RedOperation; //First one.
end;

procedure TPixel32SurfaceEffectClassicOperations.process;
var bits : pTP32;
    i,j : integer;
    c : TP32;
    pixelInPixelUnitFormat : TPixel;
begin

  for j := 0 to fsurface.height-1 do
  begin
    bits := fSurface.getSurfaceScanLinePtr(j);
    for i := 0 to fsurface.width-1 do
    begin
      TP32(pixelInPixelUnitFormat) := bits^;
      Fop[Ord(FCurrentOp)](pixelInPixelUnitFormat,i,j,FLevel);
      Bits^ := TP32(pixelInPixelUnitFormat);
      inc(Bits);

      //In a speedy world... :)
      //fsurface.color_pen := TP32(pixelInPixelUnitFormat);
      //fsurface.pixel(i,j);
    end;
  end;
end;

procedure TPixel32SurfaceEffectClassicOperations.setOpParameter(
  askLevel: single; op: TPixelUnitOp);
begin
  FLevel := askLevel;
  FCurrentOp := op;
end;

{ TCustomPixel32SurfaceEffectClassic }

constructor TCustomPixel32SurfaceEffectClassic.Create;
begin
  inherited;
  FLevel := 0.0;
end;

{ TPixel32SurfaceEffectClassicBlendOps }

procedure TPixel32SurfaceEffectClassicBlendOps.addInternalPixelUnitBlendOperation(
  const Name: string; Proc: TPixelBlend);
var id : integer;
begin
  id := length(FOp);
  SetLEngth(Fop,id+1);;
  Fop[id] := Proc;
end;

constructor TPixel32SurfaceEffectClassicBlendOps.Create;
begin
  inherited Create;
  InitializeBlends(addInternalPixelUnitBlendOperation);
  FCurrentOp := OpacityBlend; //First one.
end;

procedure TPixel32SurfaceEffectClassicBlendOps.setOpParameter(askLevel: single;
  op: TPixelUnitBlendOp; surfaceDest: TPixel32);
begin
  assert(assigned(surfaceDest));
  Assert(fsurface.width = surfaceDest.width);
  Assert(fsurface.height = surfaceDest.height);
  FSurfaceDest := surfaceDest;
  FLevel := askLevel;
  FCurrentOp := op;
end;

procedure TPixel32SurfaceEffectClassicBlendOps.process;
var bits,bitsT : pTP32;
    i,j : integer;
    c : TP32;
    source_pixelInPixelUnitFormat : TPixel;
    dest_pixelInPixelUnitFormat : TPixel;
    result_pixelInPixelUnitFormat : TPixel;
begin
  ImageWidth := fsurface.width;
  ImageHeight := fsurface.height;
  for j := 0 to fsurface.height-1 do
  begin
    bits := fSurface.getSurfaceScanLinePtr(j);
    bitsT := FSurfaceDest.getSurfaceScanLinePtr(j);
    for i := 0 to fsurface.width-1 do
    begin
      TP32(source_pixelInPixelUnitFormat) := bits^;
      TP32(dest_pixelInPixelUnitFormat) := bitsT^;
      Fop[Ord(FCurrentOp)]( source_pixelInPixelUnitFormat,
                            dest_pixelInPixelUnitFormat,
                            result_pixelInPixelUnitFormat,i,j,FLevel);
      Bits^ := TP32(result_pixelInPixelUnitFormat);
      inc(Bits);
      inc(Bitst);
    end;
  end;
end;


end.
