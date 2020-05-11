unit GS.Pixel32.Types;

interface

uses
  GS.Geometry;

Type

  TP32 = int32;
  pTP32 = ^TP32;
  TP32Array = array of TP32;

  TP32Vertice = record
    x,y : TVecType; //Coord.
    rgba : vec4;      //vertice color.
    u,v : TVecType;   //UV Map (texture, ref., etc.)
  end;
  pTP32Vertice = ^TP32Vertice;
  TP32triVertices = Array of TP32Vertice;


  TP32QuadVertices = Array[0..3] of TP32Vertice;

  TP32Rec = packed record
  case int32 of
    0: (Blue: Byte; Green: Byte; Red: Byte; AlphaChannel: Byte);
    1: (Color: TP32);
    2: (Values: packed array[0..3] of Byte);
    3: (Valuef : single);
  end;
  pTP32Rec = ^TP32Rec;

  TSoftwareRasterizeOption = (roDirectMode, robasic, roBackBuffer);

const
  gspColorNone : TP32 = $00000000;
  gspWhite : TP32 = $FFFFFFFF;
  gspBlack : TP32 = $FF000000;
  gspBlue : TP32 = $FF0000FF;
  gspGreen : TP32 = $FF008000;
  gspRed : TP32 = $FFFF0000;
  gspOrange : TP32 = $FFFF7F00;
  gspAqua :  TP32= $FF00FFFF;
  gspNavy  : TP32 = $FF000080;
  gspOlive : TP32 = $FF7F7F00;
  gspYellow : TP32 = $FFFFFF00;


function P32Vertice( const x : single = 0;
                    const y : single = 0;
                    u : single =0;
                    v : single =0;
                    r : single = 1;
                    g : single = 1;
                    b : single = 1;
                    a : single = 1) : TP32Vertice;
implementation



function P32Vertice( const x : single = 0;
                    const y : single = 0;
                    u : single =0;
                    v : single =0;
                    r : single = 1;
                    g : single = 1;
                    b : single = 1;
                    a : single = 1) : TP32Vertice;
begin
  result.x := x;
  result.y := y;
  result.u := u;
  result.v := v;
  result.rgba.r := r;
  result.rgba.g := g;
  result.rgba.b := b;
  result.rgba.a := a;
end;

end.
