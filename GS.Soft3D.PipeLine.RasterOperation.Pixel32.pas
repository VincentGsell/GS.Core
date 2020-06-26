unit GS.Soft3D.PipeLine.RasterOperation.Pixel32;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.PipeLine.Types.InternalFormat,
     GS.Soft3d.PipeLine.RasterOperation,
     GS.Pixel32.Types,
     GS.Pixel32;

Type

  TS3DRasterOperationPixel32 = class(TS3DRasterOperation)
  protected
  public
    PixelSurface : TPixel32;
    function Run : boolean; Override;
    procedure Resize(_width, _height : Uint32); Override;
  end;


implementation

uses GS.Soft3d.PipeLine;

{ TS3DRasterOperationPixel32 }

procedure TS3DRasterOperationPixel32.Resize(_width, _height: Uint32);
begin
  inherited;
  PixelSurface.resize(_width,_height);
end;

function TS3DRasterOperationPixel32.Run : boolean;
var l : pTP32;
    i,j : integer;
    ZCol : single;
    ob : TS3PLObject;
begin
  if TS3DPipeline(PipelineMaster).WorkData.Transformed.Count=0 then
    exit;

  PixelSurface.clear;

  l := PixelSurface.getSurfaceScanLinePtr(0);
  for j := 0 to PixelSurface.height-1 do
  begin
    for i := 0 to PixelSurface.width-1 do
    begin
      if TS3DPipeline(PipelineMaster).RasterControl.BackMem.Buffer[j*PixelSurface.width+i].barrier =  TS3DPipeline(PipelineMaster).RasterControl.BackMem.BackBufferValue then
      begin
        ZCol := TS3DPipeline(PipelineMaster).RasterControl.BackMem.Buffer[j*PixelSurface.width+i].Z;
        if ZCol<>0 then
          l^ := PixelSurface.colorP32Rec(trunc(ZCol*255),trunc(ZCol*255),trunc(ZCol*255),255).Color
      end;
      inc(l);
    end;
    l := PixelSurface.getSurfaceScanLinePtr(j);
  end;

{

  for ob in WorkingData.Transformed do
  for i := 0 to length(ob.faces)-1 do
  begin
    PixelSurface.moveTo(round(ob.Faces[i].v[0].X),
                        round(ob.Faces[i].v[0].Y));
    PixelSurface.LineTo(round(ob.Faces[i].v[1].X),
                        round(ob.Faces[i].v[1].Y));
    PixelSurface.LineTo(round(ob.Faces[i].v[2].X),
                        round(ob.Faces[i].v[2].Y));
    PixelSurface.LineTo(round(ob.Faces[i].v[0].X),
                        round(ob.Faces[i].v[0].Y));
  end;
}

end;



end.
