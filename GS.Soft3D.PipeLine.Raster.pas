unit GS.Soft3D.PipeLine.Raster;

interface

uses Classes,
     SysUtils,
     Math,
     GS.Geometry,
     GS.Common.Monitoring,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.Types.InternalFormat,
     GS.Soft3d.PipeLine.GeometryShader,
     GS.Soft3d.PipeLine.Tesselation;

const
  CST_DEFAULT_LAYER_COUNT = 4;
Type

  TS3DBackBuffeCode = array of byte;

  TS3DBackBufferItem = packed record //(!) pixel level.
    barrier : Uint16;
    z : single;
    ObjFound : Boolean;
    ObjIndex : Uint32;
    ObjFaceIndex : Uint32;
    //r,g,b,a : byte;
  end;
  pTS3DBackBufferItem = ^TS3DBackBufferItem;
  TS3DBackBufferArray = array of TS3DBackBufferItem;

  TS3DBackBuffer = class
  private
  protected
    fMemory : UInt64;
    fwidth: Uint32;
    fheight: Uint32;
    fv : Uint32;
  public
    Buffer : TS3DBackBufferArray;

    Constructor Create(width, height : Uint32); reintroduce;

    procedure resize(_w,_h : Uint32);

    procedure cycle;

    function BackBufferMemory : pTS3DBackBufferItem;
    function BackBufferScanLine(LineIndex : Uint32) : pTS3DBackBufferItem;


    property MemorySize : Uint64 read fMemory;
    property width : Uint32 read fwidth;
    property height : Uint32 read fheight;

    property BackBufferValue : Uint32 read fv;

  end;

  TS3DRasterAndInterpolationControl = class(TS3DPipeLineStep)
  public
    BackMem : TS3DBackBuffer;
    Constructor Create(Owner : TObject; _in : TS3DInputData3D; _wo : TS3DPipeLineData); override;
    Destructor Destroy; Override;

    function Run : Boolean; override;

    function QueryingBuffer(x, y: Uint32; out objIndex, FaceIndex: Uint32): Boolean;
  end;


implementation

{ TS3DBackBuffer }

function TS3DBackBuffer.BackBufferMemory: pTS3DBackBufferItem;
begin
  result := @(Buffer[0]);
end;

function TS3DBackBuffer.BackBufferScanLine(
  LineIndex: Uint32): pTS3DBackBufferItem;
begin
  assert(lineIndex<height);
  result := BackBufferMemory;
  inc(Result,LineIndex*width);
end;

constructor TS3DBackBuffer.Create(width, height: Uint32);
var i : integer;
begin
  fv := 0;
  assert(width>0);
  assert(height>0);

  resize(width,height);
end;

procedure TS3DBackBuffer.cycle;
begin
  inc(fv); //Alternative : reset all backbuffer barrier array.
end;

procedure TS3DBackBuffer.resize(_w, _h: Uint32);
begin
  SetLength(Buffer,_w *_h);
  FMemory := SizeOf(Buffer) * _w *_h;
  fwidth := _w;
  fheight := _h;
end;

{ TS3DRasterAndInterpolationControl }

constructor TS3DRasterAndInterpolationControl.Create(Owner : TObject; _in : TS3DInputData3D; _wo : TS3DPipeLineData);
begin
  inherited Create(Owner,_in,_wo);
  BackMem := TS3DBackBuffer.Create(InputData.Resolution.width,InputData.Resolution.height);
end;

destructor TS3DRasterAndInterpolationControl.Destroy;
begin
  FreeAndNil(BackMem);
  inherited;
end;

function TS3DRasterAndInterpolationControl.QueryingBuffer(x, y: Uint32;
  out objIndex, FaceIndex: Uint32): Boolean;
var bb : pTS3DBackBufferItem;
begin
  result := false;
  objIndex := 0;
  FaceIndex := 0;
  if (y<BackMem.height) and (x<BackMem.width) then
  begin
    bb := BackMem.BackBufferScanLine(y);
    inc(bb,x);
    if bb.barrier = BackMem.BackBufferValue then
    begin
      result := bb.ObjFound;
      objIndex := bb.Objindex;
      FaceIndex := bb.ObjFaceIndex;
    end;
  end;
end;

function edgeFunctionLocal(var a,b,c : vec3) : TVecType; {$IFNDEF DEBUG} inline; {$ENDIF}
begin
  result := (a.x - c.x) * (b.y - a.y) - (a.y - c.y) * (b.x - a.x);
end;

function TS3DRasterAndInterpolationControl.Run : boolean;
var s : TS3PLObject;
    area, w0,w1,w2 : TVecType;
    p, sv : vec3;
    oi,k,w,h : UInt32;
    v : array[0..2] of vec3;
    minx, maxx, miny, maxy : integer;
    vi0x,vi0y,vi1x,vi1y,vi2x,vi2y : integer;


    procedure ZBufferBuild;
    var i,j : Integer;
        holyZ : TVecType;
        ZCol,ZDef : single;
        bb : pTS3DBackBufferItem;
    begin
      for j := miny to maxy do
      begin
        bb := BackMem.BackBufferScanLine(j);
        inc(bb,minx);
        for i := minx to maxx do
        begin
          p.x := i; p.y := j;
          w0 := edgeFunctionLocal(v[1], v[2], p);
          w1 := edgeFunctionLocal(v[2], v[0], p);
          w2 := edgeFunctionLocal(v[0], v[1], p);

          if (w0>=0) and (w1>=0) and (w2>=0) then
          begin
            w0 := w0 / area;
            w1 := w1 / area;
            w2 := w2 / area;

            holyZ := (w0*v[0].z + w1*v[1].z + w2*v[2].z);
            ZDef := 0;
            if holyZ<>0 then
              ZDef := abs(1 / holyZ);
            ZCol := _clamp(ZDef,0,1);

            if bb.barrier <> BackMem.BackBufferValue then
            begin
              bb.barrier := BackMem.BackBufferValue;
              bb.z := ZCol;
              bb.Objindex := oi;
              bb.ObjFaceIndex := k;
              bb.ObjFound := true;
            end
            else
            begin
              if ZDef>bb.z then
              begin
                { TODO : Here Introduce layer, to keep object overlay ? }
                bb.z := ZCol;
                bb.Objindex := oi;
                bb.ObjFaceIndex := k;
              end;
            end;
          end;
          inc(bb);
        end;
      end;
    end;

begin
  //ZBuffer build.
//  TMonitoring.enter('TS3DRasterAndInterpolationControl.Run');
  try
    Result := WorkingData.Transformed.Count>0;
    w := InputData.Resolution.width;
    h := InputData.Resolution.height;
    BackMem.cycle;
    oi := 0;
    for s in WorkingData.Transformed do
    begin
      for k := 0 to length(s.Faces)-1 do
      begin
        v[0] := s.Faces[k].v[0];
        v[1] := s.Faces[k].v[1];
        v[2] := s.Faces[k].v[2];

        area := edgeFunctionLocal(v[0],
                                  v[1],
                                  v[2]);
        if area<=0 then
        begin
          //swap (rev. triangle.)
          sv := v[0];
          v[0] := v[2];
          v[2] := sv;
          area := edgeFunctionLocal(v[0],v[1],v[2]);
          if area<=0 then
          begin
            Exit;
          end;
        end;

        vi0x := round(v[0].x);
        vi0y := round(v[0].y);
        vi1x := round(v[1].x);
        vi1y := round(v[1].y);
        vi2x := round(v[2].x);
        vi2y := round(v[2].y);

        minx:=max(min(min(vi0x,vi1x),vi2x),0);
        miny:=max(min(min(vi0y,vi1y),vi2y),0);

        maxx:=min(max(max(vi0x,vi1x),vi2x),w-1);
        maxy:=min(max(max(vi0y,vi1y),vi2y),h-1);

        { TODO : replace by clamp }
        if maxx>InputData.Resolution.x then
          maxx := InputData.Resolution.x;
        if maxx<0 then
          maxx := 0;
        if maxy>InputData.Resolution.y then
          maxy := InputData.Resolution.y;
        if maxy<0 then
          maxy := 0;

        if minx>InputData.Resolution.x then
          minx := InputData.Resolution.x;
        if minx<0 then
          minx := 0;
        if miny>InputData.Resolution.y then
          miny := InputData.Resolution.y;
        if miny<0 then
          miny := 0;

        //avoid enter in Zbffer processing with incongrous value.
        if maxx-minx=0 then break;
        if maxy-miny=0 then break;

        ZBufferBuild;
      end;
      Inc(oi); //Object index in transformed list.
    end;
  finally
//    TMonitoring.exit('TS3DRasterAndInterpolationControl.Run');
  end;
end;

end.
