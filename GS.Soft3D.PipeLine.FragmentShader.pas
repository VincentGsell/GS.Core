unit GS.Soft3D.PipeLine.FragmentShader;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.Raster;

Type

  TS3DFragmentShaderControl = class(TS3DPipeLineStep)
    function Run : Boolean; Override;
  end;
    TS3DFragmentShader = class
    end;
    TS3DFragmentShader_Color = class
    end;

implementation


{ TS3DFragmentShaderControl }


function TS3DFragmentShaderControl.Run : boolean;
begin
  //...
  result := true;
end;

end.
