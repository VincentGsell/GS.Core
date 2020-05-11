unit GS.Soft3D.MeshTools;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3D.Types;

Type

  TMesh3DTool = Class
  public
    class procedure copy(source,destination : TMesh3D);
    class procedure transform(source,destination : TMesh3D; var _matrix : mat4);
  end;

implementation


{ TMesh3DTool }

class procedure TMesh3DTool.copy(source, destination: TMesh3D);
var i : integer;
begin
  assert(assigned(source));
  Assert(assigned(destination));
  if source.meshData.verticesArraySize=0 then
    exit;
  source.meshData.copy(destination.meshData);
end;

class procedure TMesh3DTool.transform(source, destination: TMesh3D;
  var _matrix: mat4);
var i : integer;
begin
  assert(assigned(source));
  Assert(assigned(destination));
  if source.meshData.verticesArraySize=0 then
    exit;
  copy(source,destination);

  for i := 0 to destination.meshData.verticesArraySize-1 do
    destination.meshData.Vertices[i] := destination.meshData.Vertices[i] * _matrix;
end;



end.
