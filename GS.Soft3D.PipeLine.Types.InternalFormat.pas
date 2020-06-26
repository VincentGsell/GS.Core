//Internal post tesselation format.
unit GS.Soft3D.PipeLine.Types.InternalFormat;

interface


uses Classes,
     SysUtils,
     contnrs,
     GS.Geometry;

type

TS3PLFace = record
  v : Array of vec3;
end;

TS3PLFaces = class
  faces : Array of TS3PLFace;
end;

TS3PLObject = Class
  meshIndex : Uint32;
  faces : Array of TS3PLFace;
End;

TS3PLObjectList = Class(TObjectList)
private
    function GetObj(Index: Uint32): TS3PLObject;
    procedure SetObj(Index: Uint32; const Value: TS3PLObject);
public
  procedure AddObj(obj : TS3PLObject);
  property Mesh[Index : Uint32] : TS3PLObject read GetObj write SetObj; default;
End;


implementation

{ TS3PLObjectList }

procedure TS3PLObjectList.AddObj(obj: TS3PLObject);
begin
  Add(obj);
end;

function TS3PLObjectList.GetObj(Index: Uint32): TS3PLObject;
begin
  result := TS3PLObject(Items[index]);
end;

procedure TS3PLObjectList.SetObj(Index: Uint32; const Value: TS3PLObject);
begin
  Items[Index] := Value;
end;

end.
