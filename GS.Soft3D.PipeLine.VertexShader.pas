unit GS.Soft3D.PipeLine.VertexShader;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types;

Type
  TS3DVertexShaderControl = class;
  TS3DVertexShader = class(TS3DObject)
  public
    controler : TS3DVertexShaderControl;
    procedure process(obj : TMesh3D; var _matrix : mat4); virtual; abstract;
  end;
    TS3DVertexShader_STDShader = class(TS3DVertexShader)
       procedure process(obj : TMesh3D; var _matrix : mat4); override;
    end;

  TS3DVertexShaderControl = class(TS3DPipeLineStep)
  protected
    TransformationMatrix : Mat4;
    Shader : TS3DVertexShader;
    procedure transform;
  public
    TransformedMeshes : TS3DMeshList;

    procedure Run; override;

    constructor Create(input : TS3DInputData3D); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TS3DVertexShaderControl }

constructor TS3DVertexShaderControl.Create(input: TS3DInputData3D);
begin
  assert(assigned(input));
  inherited Create;
  InputData := input;
  Shader := TS3DVertexShader_STDShader.Create;
  Shader.controler := self;
end;

destructor TS3DVertexShaderControl.Destroy;
begin
  freeAndNil(Shader);
end;

procedure TS3DVertexShaderControl.Run;
var s,l : TMesh3D;
begin
  for s in InputData.Meshes do
  begin
    l := TMesh3D.Create;
    TMesh3DTool.copy(s,l);
    TransformedMeshes.AddMesh(l);
  end;
  transform;
end;

procedure TS3DVertexShaderControl.transform;
var i : integer;
begin
  for i:= 0 to TransformedMeshes.Count-1 do
  begin
    Shader.process(TransformedMeshes[i],TransformationMatrix);
  end;
end;

{ TS3DVertexShader_STDShader }

procedure TS3DVertexShader_STDShader.process(obj: TMesh3D; var _matrix: mat4);
var i : integer;
begin
  for i := 0 to obj.meshData.verticesArraySize-1 do
    obj.meshData.Vertices[i] := obj.meshData.Vertices[i] * _matrix;
end;


end.
