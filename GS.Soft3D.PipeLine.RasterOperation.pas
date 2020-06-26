unit GS.Soft3D.PipeLine.RasterOperation;

interface


uses Classes,
     SysUtils,
     GS.Common.Monitoring,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.FragmentShader;
Type

  TS3DRasterOperation = class(TS3DPipeLineStep)
  public
    //Must be implemented into surface enabled techno descendant (such as Pixel32)
    function Run : boolean; virtual; abstract;
    //Resisze pixel buffer.
    procedure Resize(_width, _height : Uint32); Virtual; abstract;
  end;


implementation

end.
