unit GS.Pixel32.Service.Image32.Types;

interface

uses classes,
     sysutils,
     GS.Geometry;

const
  CST_IMAGE32_URI = 'IMG32SVC';
  CST_GETPATH_OPCODE = 20;

Type

TGetPathData = packed record
  cloudPoint : array of Vec2;
  indice : array of Uint32;
end;


implementation

end.
