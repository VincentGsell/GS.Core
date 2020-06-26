unit GS.Soft3D.PipeLine.Tesselation;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3D.Types,
     GS.Soft3D.PipeLine.Types,
     GS.Soft3D.PipeLine.Types.InternalFormat,
     GS.Soft3D.PipeLine.VertexShader;

Type
  { TODO : In thi state, useless mesh occultation (mesh culling), other triangle based optimization. }
  TS3DTesselletionControl = class(TS3DPipeLineStep)
  private
  protected
  public
    function Run: Boolean; override;
  end;

implementation

function TS3DTesselletionControl.Run : boolean;
var k : TS3PLObject;
    i,j: Integer;
begin
  for k in WorkingData.Transformed do
  begin
    for i := 0 to Length(k.faces)-1 do
    begin
      for j := 0 to 2 do
        if k.faces[i].v[j].Z<>0 then
        begin
//          k.faces[i].v[j].Z := k.faces[i].v[j].Z * 0.2;
          //Applying Z.

          k.faces[i].v[j].X := k.faces[i].v[j].X/k.faces[i].v[j].Z;
          k.faces[i].v[j].Y := k.faces[i].v[j].Y/k.faces[i].v[j].Z;
        end;

        for j := 0 to 2 do
        begin
          //Denormalization.
          k.faces[i].v[j].X := (1 + k.faces[i].v[j].X) * 0.5 * InputData.Resolution.width;
          k.faces[i].v[j].Y := (1 + k.faces[i].v[j].Y) * 0.5 * InputData.Resolution.height;
        end;
    end;

    //https://www.scratchapixel.com/lessons/3d-basic-rendering/rasterization-practical-implementation/perspective-correct-interpolation-vertex-attributes
    //https://www.khronos.org/opengl/wiki/Vertex_Transformation
    //viewport = (0,0,Width, Height) | Formula = windowCoordinate[0] = (x * 0.5 + 0.5) * viewport[2] + viewport[0];

    //Denormalization Equivalence : (for info - vector dir display inverse.)
    //VA.x := (1-VA.x) * Width/2;
    //VA.y := (1-VA.y) * Height/2;
    //VB.x := (1-VB.x) * Width/2;
    //VB.y := (1-VB.y) * Height/2;
    //VC.x := (1-VC.x) * Width/2;
    //VC.y := (1-VC.y) * Height/2;
  end;
end;



end.
