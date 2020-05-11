unit GS.Soft3D.PipeLine.Tesselation;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3D.Types,
     GS.Soft3D.PipeLine.Types,
     GS.Soft3D.PipeLine.VertexShader;

Type

  TS3DTesselletionControl = class;
  TS3DTesselletionMethod = class(TS3DObject)
  public
    Controler : TS3DTesselletionControl;
    Mesh: TMesh3D;

    function TesselationUnitCount : Uint32; virtual; abstract;
  end;
    //Only triangle tesselation for instance.
    TS3DTesselletionTri = class(TS3DTesselletionMethod)
    private
    public
      function triangleCount : Integer;
      function triangles(TriIndice : integer) : TBaseTriangle;
      function triangles3D(TriIndice : integer) : TBaseTriangle3D;

      function TesselationUnitCount : Uint32; override;
    end;
  TS3DTesselletionControl = class(TS3DPipeLineStep)
  private
  protected
    fMeshIndex: Uint32;
    function GetMeshObjectCount: Uint32;
    procedure SetMeshIndex(const Value: Uint32);
  public
    VertexShaderControler : TS3DVertexShaderControl; //pointer.
    TesselationMethod : TS3DTesselletionMethod;

    constructor create(input : TS3DVertexShaderControl); reintroduce;

    procedure Run; override;

    property MeshObjectCount : Uint32 read GetMeshObjectCount;
    property MeshIndex : Uint32 read fMeshIndex write SetMeshIndex;
  end;


implementation


{ TS3DTesselletionTri }

function TS3DTesselletionTri.TesselationUnitCount: Uint32;
begin
  result := triangleCount;
end;

function TS3DTesselletionTri.triangleCount: Integer;
begin
  Assert(assigned(Mesh));
  result := Mesh.meshData.indicesArraySize div 3;
end;

function TS3DTesselletionTri.triangles(TriIndice: integer): TBaseTriangle;
var l : Uint32;
begin
  Assert(assigned(Mesh));
  l := TriIndice * 3;
  Assert(l<Mesh.meshData.indicesArraySize-2,'Triangles indice overflow');
  result.VertexIndiceA := Mesh.meshData.indices[l];
  result.VertexIndiceB := Mesh.meshData.indices[l+1];
  result.VertexIndiceC := Mesh.meshData.indices[l+2];
  Result.uvA := Mesh.meshData.UVMap[l];
  Result.uvB := Mesh.meshData.UVMap[l+1];
  Result.uvC := Mesh.meshData.UVMap[l+2];
end;

function TS3DTesselletionTri.triangles3D(TriIndice: integer): TBaseTriangle3D;
var l : TBaseTriangle;
begin
  Assert(assigned(Mesh));
  l := triangles(TriIndice);

  result.VertexA.x := Mesh.meshData.vertices[l.VertexIndiceA].X;
  result.VertexA.y := Mesh.meshData.vertices[l.VertexIndiceA].Y;
  result.VertexA.z := Mesh.meshData.vertices[l.VertexIndiceA].Z;

  result.VertexB.x := Mesh.meshData.vertices[l.VertexIndiceB].X;
  result.VertexB.y := Mesh.meshData.vertices[l.VertexIndiceB].Y;
  result.VertexB.z := Mesh.meshData.vertices[l.VertexIndiceB].Z;

  result.VertexC.x := Mesh.meshData.vertices[l.VertexIndiceC].X;
  result.VertexC.y := Mesh.meshData.vertices[l.VertexIndiceC].Y;
  result.VertexC.z := Mesh.meshData.vertices[l.VertexIndiceC].Z;

  Result.uvA := l.uvA; //  meshData.UVMap[l.VertexIndiceA];
  Result.uvB := l.uvB; //  meshData.UVMap[l.VertexIndiceB];
  Result.uvC := l.uvC; //  meshData.UVMap[l.VertexIndiceC];
end;

{ TS3DTesselletionControl }

constructor TS3DTesselletionControl.create(input: TS3DVertexShaderControl);
begin
  inherited Create;
  assert(assigned(input));
  assert(input.TransformedMeshes.Count>0);
  InputData := input.InputData;
  VertexShaderControler := input;
  TesselationMethod := TS3DTesselletionTri.Create;
  MeshIndex := 0;
end;

function TS3DTesselletionControl.GetMeshObjectCount: Uint32;
begin
  result := VertexShaderControler.TransformedMeshes.Count;
end;

procedure TS3DTesselletionControl.Run;
begin
  //None here
end;


procedure TS3DTesselletionControl.SetMeshIndex(const Value: Uint32);
begin
  assert(value<VertexShaderControler.TransformedMeshes.Count);
  fMeshIndex := Value;
  TesselationMethod.Mesh := VertexShaderControler.TransformedMeshes[fMeshIndex];
end;

end.
