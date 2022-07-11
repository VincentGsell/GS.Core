unit GS.Data.GIS;

interface

uses sysutils
     ,classes
     ,types;

Type

IGSDataGIS = Interface
  function getAreaGeometryByStringQueryAsJson(aQuery :string) : string;
End;

implementation

end.
