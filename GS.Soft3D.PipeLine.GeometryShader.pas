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

  TS3DGeometryShaderControler = class;
  TS3DGeometryShader = class(TS3DObject)
    controler : TS3DGeometryShaderControler;
    procedure process(obj : TS3DTesselletionMethod); virtual; abstract;
  end;
    TS3DGeometryShader_STDShader = class(TS3DGeometryShader)
    public
      procedure process(obj : TS3DTesselletionMethod); override;
    end;

  TS3DGeometryShaderControler = class(TS3DPipeLineStep)
  protected
  public
    TesselationControl : TS3DTesselletionControl;
    GeomShader : TS3DGeometryShader;

    Constructor Create(Tessel : TS3DTesselletionControl); reintroduce;
    procedure Run; override;
  end;


implementation


{ TS3DGeometryShader_STDShader }

procedure TS3DGeometryShader_STDShader.process(obj: TS3DTesselletionMethod);
begin
  //Todo.
  //Aim : adding basic geometry stuffs, starting from entry data.
end;



{ TS3DGeometryShaderControler }

constructor TS3DGeometryShaderControler.Create(Tessel: TS3DTesselletionControl);
begin
  assert(assigned(Tessel));
  InputData := Tessel.InputData;
  TesselationControl := Tessel;
  GeomShader := TS3DGeometryShader_STDShader.Create;
  GeomShader.Controler := self;
end;

procedure TS3DGeometryShaderControler.Run;
var i,j : integer;
begin
  //WIP...
  for i := 0 to TesselationControl.MeshObjectCount-1 do
  begin
    TesselationControl.MeshIndex := i;
    GeomShader.process(TesselationControl.TesselationMethod);
  end;
end;

end.
