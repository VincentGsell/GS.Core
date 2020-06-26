unit GS.Soft3D.PipeLine.GeometryShader;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.Tesselation;

Type
  TS3DGeometryShader = class(TS3DObject)
        { TODO :
    As Vertex Shader.
    See if it relevant, at this stage. }
  end;

  TS3DGeometryShaderControler = class(TS3DPipeLineStep)
  protected
  public
    function Run : Boolean; override;
  end;


implementation


function TS3DGeometryShaderControler.Run : Boolean;
begin
  //....
end;

end.
