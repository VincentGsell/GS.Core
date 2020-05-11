unit GS.Soft3D.PipeLine.Raster;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.GeometryShader,
     GS.Soft3d.PipeLine.Tesselation;

const
  CST_DEFAULT_LAYER_COUNT = 4;
Type

  TS3DBackBuffeCode = array of byte;

  TS3DBackBufferItem = packed record //(!) pixel level.
    z : single;
    r,g,b,a : byte;
    //detection info ?
    //rec and object id ?
  end;
  pTS3DBackBufferItem = ^TS3DBackBufferItem;

  TS3DBackBufferArray = array of TS3DBackBufferItem;
  TS3DBackBufferLayer = array of TS3DBackBufferArray;

  TS3DBackBuffer = class
  private
  protected
    BarrierCode : TS3DBackBuffeCode;
    Buffer : TS3DBackBufferLayer;
    fMemory : UInt64;
    fwidth: Uint32;
    fLayerCount: byte;
    fheight: Uint32;
  public
    Constructor Create(layerCounter : byte; width, height : Uint32); reintroduce;

    property MemorySize : Uint64 read fMemory;
    property layerCount : byte read fLayerCount;
    property width : Uint32 read fwidth;
    property height : Uint32 read fheight;
  end;

  TS3DRasterAndInterpolationControl = class(TS3DPipeLineStep)
  public
    Geometry : TS3DGeometryShaderControler;
    Buffer : TS3DBackBuffer;
    Constructor Create(s3dGeom : TS3DGeometryShaderControler; width, height : Uint32); reintroduce;
    Destructor Destroy; override;

    procedure Run; override;
  end;


implementation

{ TS3DBackBuffer }

constructor TS3DBackBuffer.Create(layerCounter: byte; width, height: Uint32);
begin
  assert(layerCounter>0);
  assert(width>0);
  assert(height>0);

  SetLength(BarrierCode,width*height);
  SetLength(Buffer,width*height);

  FMemory := SizeOf(Buffer) + SizeOf(BarrierCode);
end;

{ TS3DRasterAndInterpolationControl }

constructor TS3DRasterAndInterpolationControl.Create(s3dGeom : TS3DGeometryShaderControler; width, height: Uint32);
begin
  assert(assigned(s3dGeom));
  InputData := s3dGeom.InputData;
  Geometry := s3dGeom;
  Buffer := TS3DBackBuffer.Create(CST_DEFAULT_LAYER_COUNT,width,height);
end;

destructor TS3DRasterAndInterpolationControl.Destroy;
begin
  FreeAndNil(Buffer);
  inherited;
end;

procedure TS3DRasterAndInterpolationControl.Run;
var i,j : Integer;
    t : TBaseTriangle3D;
    ltri : TS3DTesselletionTri;
begin

///
///  Triangle transformation for preparing raster fragement op.

  for I := 0 to Geometry.TesselationControl.MeshObjectCount-1 do
  begin
    Geometry.TesselationControl.MeshIndex := i;
    ltri :=  TS3DTesselletionTri(Geometry.TesselationControl.TesselationMethod);

    for j := 0 to ltri.triangleCount-1 do
    begin
      t := ltri.triangles3D(j);

      //Applying Z.
      t.VertexA.X := t.VertexA.X / t.VertexA.Z;
      t.VertexA.Y := t.VertexA.Y / t.VertexA.Z;
      t.VertexB.X := t.VertexB.X / t.VertexB.Z;
      t.VertexB.Y := t.VertexB.Y / t.VertexB.Z;
      t.VertexC.X := t.VertexC.X / t.VertexC.Z;
      t.VertexC.Y := t.VertexC.Y / t.VertexC.Z;

      //https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/perspective-correct-interpolation-vertex-attributes
      //https://www.khronos.org/opengl/wiki/Vertex_Transformation
      //viewport = (0,0,Width, Height) | Formula = windowCoordinate[0] = (x * 0.5 + 0.5) * viewport[2] + viewport[0];

      //Denormalization.
      t.VertexA.X := (1 + t.VertexA.X) * 0.5 * InputData.Resolution.width;
      t.VertexA.Y := (1 + t.VertexA.Y) * 0.5 * InputData.Resolution.height;
      t.VertexB.X := (1 + t.VertexB.X) * 0.5 * InputData.Resolution.width;
      t.VertexB.Y := (1 + t.VertexB.Y) * 0.5 * InputData.Resolution.height;
      t.VertexC.X := (1 + t.VertexC.X) * 0.5 * InputData.Resolution.width;
      t.VertexC.Y := (1 + t.VertexC.Y) * 0.5 * InputData.Resolution.height;

      //Denormalization Equivalence : (for info - vector dir display inverse.)
      //t.VertexA.x := (1-t.VertexA.x) * Width/2;
      //t.VertexA.y := (1-t.VertexA.y) * Height/2;
      //t.VertexB.x := (1-t.VertexB.x) * Width/2;
      //t.VertexB.y := (1-t.VertexB.y) * Height/2;
      //t.VertexC.x := (1-t.VertexC.x) * Width/2;
      //t.VertexC.y := (1-t.VertexC.y) * Height/2;

    end;
{
//      TargetCanvas.beginDraw;
      if Frasterframe then
      begin

        TargetCanvas.setVerticeXYZ(0,t.VertexA.X,t.VertexA.Y,t.VertexA.Z);
        TargetCanvas.setVerticeUV(0,t.uvA.u,t.uvA.v);
        TargetCanvas.setVerticeXYZ(1,t.VertexB.X,t.VertexB.Y,t.VertexB.Z);
        TargetCanvas.setVerticeUV(1,t.uvB.u,t.uvB.v);
        TargetCanvas.setVerticeXYZ(2,t.VertexC.X,t.VertexC.Y,t.VertexC.Z);
        TargetCanvas.setVerticeUV(2,t.uvC.u,t.uvC.v);
        TargetCanvas.rasterize;

      end;

      if wireFrame then //TODO 3d line !?
      begin

        TargetCanvas.moveTo(round(t.VertexA.x),round(t.VertexA.y));
        TargetCanvas.LineTo(round(t.VertexB.x),round(t.VertexB.y));
        TargetCanvas.lineTo(round(t.VertexC.x),round(t.VertexC.y));
        TargetCanvas.lineTo(round(t.VertexA.x),round(t.VertexA.y));
}
//      TargetCanvas.endDraw;
//    end;
//    TargetCanvas.endDraw;
end;
end;

end.
