unit GS.Soft3D.PipeLine.VertexShader;

interface

uses Classes,
     SysUtils,
     GS.Common.Monitoring,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.Types.InternalFormat;

Type
  TS3DVertexShaderControl = class;
  TS3DVertexShader = class(TS3DObject)
  public
    { TODO : Not used yet.
    To define here access, and methodology, to overriy shader transformation.
    See in Run, the call nest. }
  end;

  TS3DVertexShaderControl = class(TS3DPipeLineStep)
  protected
  public
    function Run : Boolean; override;

    Constructor Create(Owner : TObject; _in : TS3DInputData3D; _wo : TS3DPipeLineData); override;

    destructor Destroy; override;
  end;

implementation

{ TS3DVertexShaderControl }


constructor TS3DVertexShaderControl.Create(Owner : TObject; _in : TS3DInputData3D; _wo : TS3DPipeLineData);
begin
  inherited Create(Owner,_in,_wo);
end;

destructor TS3DVertexShaderControl.Destroy;
begin
end;

Function TS3DVertexShaderControl.Run : boolean;
var s,l : TS3DObject;
    f : TMesh3D;
    t : TS3PLObject;
    tl,mi,i : integer;
    VertexIndiceA,VertexIndiceB,VertexIndiceC : Uint16;
    transformationMatrix : Mat4;

begin
//  TMonitoring.enter('S3DVertexShaderControl.Run');
  try
    //HERE : Frustrum culling, sorting, simplying etc. etc.
    //In order to give an "lighten" transformed mesh.
    result := InputData.Objects.Count>0;
    if not(result) then
      Exit;

    WorkingData.Transformed.Clear;
    mi := 0;
    for s in InputData.Objects do
    begin
      Assert(Assigned(s.MeshAsset),'TS3DVertexShaderControl.Run / PreTesslization state : Object has no mesh assigned');
      f := TMesh3D.Create;
      try
        transformationMatrix := s.TransformationMatrix * InputData.ViewMatrix;
        TMesh3DTool.transform(s.MeshAsset,f,transformationMatrix);

        t := TS3PLObject.Create;
        t.meshIndex := mi;
        Assert(f.meshData.indicesArraySize mod 3 = 0,'TS3DVertexShaderControl.Run / PreTesslization state : Indice must be multiple of 3 (triangulation)');
        Assert(f.meshData.indicesArraySize-2>0,'TS3DVertexShaderControl.Run / Triangles indice overflow');

        SetLength(t.faces,f.meshData.indicesArraySize div 3);
        for i := 0 to Length(t.faces)-1 do
        begin
          tl := i * 3;

          VertexIndiceA := f.meshData.indices[tl];
          VertexIndiceB := f.meshData.indices[tl+1];
          VertexIndiceC := f.meshData.indices[tl+2];

          SetLength(t.faces[i].v,3); //triangle.

          t.faces[i].v[0] := f.meshData.vertices[VertexIndiceA];
          t.faces[i].v[1] := f.meshData.vertices[VertexIndiceB];
          t.faces[i].v[2] := f.meshData.vertices[VertexIndiceC];

          { TODO : UV, normals... }
        end;
        WorkingData.Transformed.AddObj(t);
      finally
        FreeAndNil(f);
      end;

      //Here, vertices is matrix transformed, but not raster projected. (made in tesselization)
      result := WorkingData.Transformed.Count>0;
      for t in WorkingData.Transformed do
      begin
          //{ TODO : Put here External Vertex Shader call ! }
      end;
    end;
  finally
//    TMonitoring.exit('S3DVertexShaderControl.Run');
  end;
end;


end.
