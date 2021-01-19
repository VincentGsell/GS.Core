unit GS.Soft3D.PipeLine.RasterOperation.Pixel32;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.PipeLine.Types.InternalFormat,
     GS.Soft3d.PipeLine.RasterOperation,
     GS.Soft3d.PipeLine.FragmentShader,
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
    ob : TS3PLObject;
    align,backbuffervalue : NativeInt;
begin
  if TS3DPipeline(PipelineMaster).WorkData.Transformed.Count=0 then
    exit;

  if EnableClearRaster then
    PixelSurface.clear;

  if EnableRasterEngine then
  begin
    backbuffervalue := TS3DPipeline(PipelineMaster).RasterControl.BackMem.BackBufferValue;
    for j := 0 to PixelSurface.height-1 do
    begin
      align := j*PixelSurface.width;
      l := PixelSurface.getSurfaceScanLinePtr(j);
      for i := 0 to PixelSurface.width-1 do
      begin
        With TS3DPipeline(PipelineMaster).RasterControl.BackMem.Buffer[align] do
        if barrier = backbuffervalue  then
        begin
          FragControl.x := i;
          FragControl.y := j;
          FragControl.Z := Z;
          FragControl.ObjIndex := Objindex;
          FragControl.FaceIndex := ObjFaceIndex;

          if Z>0 then
          begin
            FragControl.Run; //running all shader for the current parameter.
            l^ := PixelSurface.colorP32Rec( trunc(FragControl.result_rgba.r*255),
                                            trunc(FragControl.result_rgba.g*255),
                                            trunc(FragControl.result_rgba.b*255),
                                            trunc(FragControl.result_rgba.a*255)).Color;
          end;
        end;
        inc(l);
        inc(align);
      end;
    end;
  end;

  PixelSurface.rasterMode := TSoftwareRasterizeOption.roDirectMode;
  if EnableFullWireFrame then
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


end;



end.
