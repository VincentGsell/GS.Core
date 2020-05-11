(*************************************************************************)
(*                                                                       *)
(*                             FASTGEO                                   *)
(*                                                                       *)
(*                2D/3D Computational Geometry Algorithms                *)
(*                        Release Version 5.0.1                          *)
(*                                                                       *)
(* Author: Arash Partow 1997-2006                                        *)
(* URL: http://fastgeo.partow.net                                        *)
(*      http://www.partow.net/projects/fastgeo/index.html                *)
(*                                                                       *)
(* Copyright notice:                                                     *)
(* Free use of the FastGEO computational geometry library is permitted   *)
(* under the guidelines and in accordance with the most current version  *)
(* of the Common Public License.                                         *)
(* http://www.opensource.org/licenses/cpl.php                            *)
(*                                                                       *)
(*************************************************************************)


unit FastGEO;

interface


const VersionInformation = 'FastGEO Version 5.0.1';
const AuthorInformation  = 'Arash Partow (1997-2006)';
const EpochInformation   = 'Lambda-Phi';
const RecentUpdate       = '10-02-2006';
const LCID               = '$10-02-2006:FEDEDB4632780C2FAE$';


{$IFNDEF FASTGEO}
  {$DEFINE FASTGEO}
{$ENDIF}


{$DEFINE FASTGEO_DOUBLE_PRECISION}


{$IFDEF FASTGEO_SINGLE_PRECISION}
 type TFloat = Single;
{$ENDIF}

{$IFDEF FASTGEO_DOUBLE_PRECISION}
 type TFloat = Double;
{$ENDIF}

{$IFDEF FASTGEO_EXTENDED_PRECISION}
 type TFloat = Extended;
{$ENDIF}




(****************************************************************************)
(********************[ Basic Geometric Structure Types ]*********************)
(****************************************************************************)

(**************[  Vertex type   ]***************)
type TPoint2DPtr    = ^TPoint2D;
     TPoint2D       = record x,y:TFloat; end;
     TPoint2DArray  = array of TPoint2D;


(**************[ 3D Vertex type ]***************)
type TPoint3DPtr    = ^TPoint3D;
     TPoint3D       = record x,y,z:TFloat; end;
     TPoint3DArray  = array of TPoint3D;


(**************[  Quadix type   ]***************)
type TQuadix2DPtr   = ^TQuadix2D;
     TQuadix2D      = array [1..4] of TPoint2D;
     TQuadix2DArray = array of TQuadix2D;

type TQuadix3DPtr   = ^TQuadix3D;
     TQuadix3D      = array [1..4] of TPoint3D;
     TQuadix3DArray = array of TQuadix3D;


(**************[ Rectangle type ]***************)
type TRectanglePtr   = ^TRectangle;
     TRectangle      = array [1..2] of TPoint2D;
     TRectangleArray = array of TRectangle;


(**************[ Triangle type  ]***************)
type TTriangle2DPtr   = ^TTriangle2D;
     TTriangle2D      = array [1..3] of TPoint2D;
     TTriangle2DArray = array of TTriangle2D;

type TTriangle3DPtr   = ^TTriangle3D;
     TTriangle3D      = array [1..3] of TPoint3D;
     TTriangle3DArray = array of TTriangle3D;


(**************[  Segment type  ]***************)
type TSegment2DPtr   = ^TSegment2D;
     TSegment2D      = array [1..2] of TPoint2D;
     TSegment2DArray = array of TSegment2D;

type TSegment3DPtr   = ^TSegment3D;
     TSegment3D      = array [1..2] of TPoint3D;
     TSegment3DArray = array of TSegment3D;


(**************[  Line type  ]***************)
type TLine2DPtr     = ^TLine2D;
     TLine2D        = array [1..2] of TPoint2D;
     TLine2DArray   = array of TLine2D;

type TLine3DPtr     = ^TLine3D;
     TLine3D        = array [1..2] of TPoint3D;
     TLine3DArray   = array of TLine3D;


(**************[  Circle type   ]***************)
type TCirclePtr     = ^TCircle;
     TCircle        = record x,y,Radius : TFloat; end;
     TCircleArray   = array of TCircle;


(**************[  Sphere type   ]***************)
type TSpherePtr     = ^TSphere;
     TSphere        = record x,y,z,Radius : TFloat; end;
     TSphereArray   = array of TSphere;


(**************[  Arc type   ]***************)
type TCircularArc2DPtr = ^TCircularArc2D;
     TCircularArc2D    = record
                           x1,y1,x2,y2,cx,cy,px,py : TFloat;
                           angle1, angle2          : TFloat;
                           Orientation             : Integer;
                         end;
     TCircularArc2DArray = array of TCircularArc2D;


(************[  Bezier type   ]*************)
type TQuadraticBezier2DPtr   = ^TQuadraticBezier2D;
     TQuadraticBezier2D      = array [0..2] of TPoint2D;
     TQuadraticBezier2DArray = array of TQuadraticBezier2D;

     TQuadraticBezier3DPtr   = ^TQuadraticBezier3D;
     TQuadraticBezier3D      = array [0..2] of TPoint3D;
     TQuadraticBezier3DArray = array of TQuadraticBezier3D;

     TCubicBezier2DPtr       = ^TCubicBezier2D;
     TCubicBezier2D          = array [0..3] of TPoint2D;
     TCubicBezier2DArray     = array of TCubicBezier2D;

     TCubicBezier3DPtr       = ^TCubicBezier3D;
     TCubicBezier3D          = array [0..3] of TPoint3D;
     TCubicBezier3DArray     = array of TCubicBezier3D;

     TCurvePoint2D           = record x,y,t   : TFloat; end;
     TCurvePoint3D           = record x,y,z,t : TFloat; end;

     TCurvePoint2DArray      = array of TCurvePoint2D;
     TCurvePoint3DArray      = array of TCurvePoint3D;


(**************[ 2D Vector type ]***************)
type TVector2D      = record x,y : TFloat; end;
type TVector2DPtr   = ^TVector2D;
type TVector2DArray = array of TVector2D;


(**************[ 3D Vector type ]***************)
type TVector3D      = record x,y,z : TFloat; end;
type TVector3DPtr   = ^TVector3D;
type TVector3DArray = array of TVector3D;


(**********[ Polygon Vertex type  ]************)
type TPolygon2D     = array of TPoint2D;
type TPolygon2DPtr  = ^TPolygon2D;

type TPolyLine2D     = array of TPoint2D;
type TPolyLine2DPtr  = ^TPolyLine2D;

type TPolygon3D     = array of TPoint3D;
type TPolygon3DPtr  = ^TPolygon3D;

type TPolyhedron    = array of TPolygon3D;
type TPolyhedronPtr = ^TPolyhedron;


(**************[ Plane type ]******************)

type TPlane2D       = record a,b,c,d : TFloat; end;
type TPlane2DPtr    = ^TPlane2D;


(**********[ Barycentric Coordinates]***********)

type TBarycentricUnit       = record x1,y1,x2,y2,x3,y3,delta : TFloat; end;
type TBarycentricUnitPtr    = ^TBarycentricUnit;

type TBarycentricTriplet    = record u,v,w : TFloat; end;
type TBarycentricTripletPtr = ^TBarycentricTriplet;


type TInclusion    = (
                      eFully,
                      ePartially,
                      eOutside,
                      eUnknown
                     );

type eTriangletype = (
                      etEquilateral,
                      etIsosceles,
                      etRight,
                      etScalene,
                      etObtuse,
                      etUnknown
                     );



(********[ Universal Geometric Variable ]********)

type eGeometricObjectTypes = (
                              geoPoint2D,
                              geoPoint3D,
                              geoLine2D,
                              geoLine3D,
                              geoSegment2D,
                              geoSegment3D,
                              geoQuadix2D,
                              geoQuadix3D,
                              geoTriangle2D,
                              geoTriangle3D,
                              geoRectangle,
                              geoCircle,
                              geoSphere,
                              geoPolygon2D,
                              geoPolygon3D,
                              geoQuadraticBezier2D,
                              geoQuadraticBezier3D,
                              geoCubicBezier2D,
                              geoCubicBezier3D,
                              geoPolyhedron
                             );



type TGeometricObjectPtr = ^TGeometricObject;
     TGeometricObject = record
       case ObjectType : eGeometricObjectTypes of
         geoPoint2D           : (Point2D    : TPoint2D          );
         geoPoint3D           : (Point3D    : TPoint3D          );
         geoLine2D            : (Line2D     : TLine2D           );
         geoLine3D            : (Line3D     : TLine3D           );
         geoSegment2D         : (Segment2D  : TSegment2D        );
         geoSegment3D         : (Segment3D  : TSegment3D        );
         geoTriangle2D        : (Triangle2D : TTriangle2D       );
         geoTriangle3D        : (Triangle3D : TTriangle3D       );
         geoQuadix2D          : (Quadix2D   : TQuadix2D         );
         geoQuadix3D          : (Quadix3D   : TQuadix3D         );
         geoRectangle         : (Rectangle  : TRectangle        );
         geoCircle            : (Circle     : TCircle           );
         geoSphere            : (Sphere     : TSphere           );
         geoPolygon2D         : (Polygon2D  : TPolygon2DPtr     );
         geoPolygon3D         : (Polygon3D  : TPolygon3DPtr     );
         geoQuadraticBezier2D : (QBezier2D  : TQuadraticBezier2D);
         geoQuadraticBezier3D : (QBezier3D  : TQuadraticBezier3D);
         geoCubicBezier2D     : (CBezier2D  : TCubicBezier2D    );
         geoCubicBezier3D     : (CBezier3D  : TCubicBezier3D    );
         geoPolyhedron        : (Polyhedron : TPolyhedronPtr    );
       end;



type TBooleanArray = array of Boolean;


type TNumericPrecisionResult = record
       EEResult      : Boolean;  // Epsilon equivelence result
       ZEResult      : Boolean;  // Zero equivelence result;
       EFPResult     : Boolean;  // Extended floating point test result
       SystemEpsilon : TFloat;
end;


(**********[ Orientation constants ]**********)

const RightHandSide        = -1;
const LeftHandSide         = +1;
const Clockwise            = -1;
const CounterClockwise     = +1;
const CollinearOrientation =  0;
const AboveOriention       = +1;
const BelowOrientation     = -1;
const CoplanarOrientation  =  0;



(************[ Epsilon constants ]*************)

const Epsilon_High      = 1.0E-16;
const Epsilon_Medium    = 1.0E-12;
const Epsilon_Low       = 1.0E-08;
const Epsilon           = Epsilon_Medium;


{$IFDEF FASTGEO_SINGLE_PRECISION}
 const Infinity          = 1e+30;
{$ENDIF}

{$IFDEF FASTGEO_DOUBLE_PRECISION}
 const Infinity          = 1e+300;
{$ENDIF}

{$IFDEF FASTGEO_EXTENDED_PRECISION}
 const Infinity          = 1e+4924;
{$ENDIF}


(*******[ Random resolution constants ]********)

const RandomResolutionInt = 1000000000;
const RandomResolutionFlt = RandomResolutionInt * 1.0;



function Orientation(const x1,y1,x2,y2,Px,Py:TFloat):Integer;                                                                  overload;
function Orientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TFloat):Integer;                                                overload;

function RobustOrientation(const x1,y1,x2,y2,Px,Py:TFloat):Integer;                                                            overload;
function RobustOrientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TFloat):Integer;                                          overload;

function Orientation(const Point1,Point2:TPoint2D; const Px,Py:TFloat):Integer;                                                overload;
function Orientation(const Point1,Point2,Point3:TPoint2D):Integer;                                                             overload;
function Orientation(const Line:TLine2D; const Point:TPoint2D):Integer;                                                        overload;
function Orientation(const Segment:TSegment2D; const Point:TPoint2D):Integer;                                                  overload;

function Orientation(const Point1,Point2,Point3:TPoint3D; const Px,Py,Pz:TFloat):Integer;                                      overload;
function Orientation(const Point1,Point2,Point3,Point4:TPoint3D):Integer;                                                      overload;
function Orientation(const Triangle:TTriangle3D; const Point:TPoint3D):Integer;                                                overload;

function Signed(const x1,y1,x2,y2,Px,Py:TFloat):TFloat;                                                                        overload;
function Signed(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TFloat):TFloat;                                                      overload;

function Signed(const Point1,Point2:TPoint2D; const Px,Py:TFloat):TFloat;                                                      overload;
function Signed(const Point1,Point2,Point3:TPoint2D):TFloat;                                                                   overload;
function Signed(const Line:TLine2D; const Point:TPoint2D):TFloat;                                                              overload;
function Signed(const Segment:TSegment2D; const Point:TPoint2D):TFloat;                                                        overload;

function Signed(const Point1,Point2,Point3:TPoint3D; const Px,Py,Pz:TFloat):TFloat;                                            overload;
function Signed(const Point1,Point2,Point3,Point4:TPoint3D):TFloat;                                                            overload;
function Signed(const Triangle:TTriangle3D; const Point:TPoint3D):TFloat;                                                      overload;

function Collinear(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                                    overload;
function Collinear(const x1,y1,x2,y2,x3,y3,Epsilon:TFloat):Boolean;                                                            overload;
function Collinear(const PointA,PointB,PointC:TPoint2D):Boolean;                                                               overload;
function Collinear(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;                                                           overload;
function Collinear(const PointA,PointB,PointC:TPoint3D):Boolean;                                                               overload;

function RobustCollinear(const x1,y1,x2,y2,x3,y3:TFloat; const Epsilon : TFloat = Epsilon_High):Boolean;                       overload;
function RobustCollinear(const PointA,PointB,PointC:TPoint2D;  const Epsilon : TFloat = Epsilon_High):Boolean;                 overload;

function Coplanar(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):Boolean;                                                   overload;
function Coplanar(const PointA,PointB,PointC,PointD:TPoint3D):Boolean;                                                         overload;

function IsPointCollinear(const x1,y1,x2,y2,Px,Py:TFloat;                   const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const PointA,PointB,PointC:TPoint2D;              const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const PointA,PointB:TPoint2D; const Px,Py:TFloat; const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const Segment:TSegment2D; const PointC:TPoint2D;  const Robust : Boolean = False):Boolean;           overload;
function IsPointCollinear(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat):Boolean;                                                    overload;
function IsPointCollinear(const PointA,PointB,PointC:TPoint3D):Boolean;                                                        overload;
function IsPointCollinear(const Segment:TSegment3D; const PointC:TPoint3D):Boolean;                                            overload;

function IsPointOnRightSide(const Px,Py,x1,y1,x2,y2:TFloat):Boolean;                                                           overload;
function IsPointOnRightSide(const x,y:TFloat; const Segment:TSegment2D):Boolean;                                               overload;
function IsPointOnRightSide(const Point:TPoint2D; const Segment:TSegment2D):Boolean;                                           overload;

function IsPointOnRightSide(const x,y:TFloat; const Line:TLine2D):Boolean;                                                     overload;
function IsPointOnRightSide(const Point:TPoint2D; const Line:TLine2D):Boolean;                                                 overload;

function IsPointOnLeftSide(const Px,Py,x1,y1,x2,y2:TFloat):Boolean;                                                            overload;
function IsPointOnLeftSide(const x,y:TFloat; const Segment:TSegment2D):Boolean;                                                overload;
function IsPointOnLeftSide(const Point:TPoint2D; const Segment:TSegment2D):Boolean;                                            overload;

function IsPointOnLeftSide(const x,y:TFloat; const Line:TLine2D):Boolean;                                                      overload;
function IsPointOnLeftSide(const Point:TPoint2D; const Line:TLine2D):Boolean;                                                  overload;

function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                                              overload;
function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; out ix, iy :TFloat):Boolean;                                          overload;
function Intersect(const Point1,Point2,Point3,Point4:TPoint2D):Boolean;                                                        overload;
function Intersect(const Segment1,Segment2:TSegment2D):Boolean;                                                                overload;
function Intersect(const Segment1,Segment2:TSegment2D; out ix, iy : TFloat):Boolean;                                           overload;

function Intersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const fuzzy:TFloat = 0.0):Boolean;                        overload;
function Intersect(const P1,P2,P3,P4:TPoint3D; const fuzzy:TFloat = 0.0):Boolean;                                              overload;
function Intersect(const Segment1,Segment2:TSegment3D; const fuzzy:TFloat = 0.0):Boolean;                                      overload;

function Intersect(const Segment:TSegment2D;   const Rectangle : TRectangle):Boolean;                                          overload;
function Intersect(const Segment:TSegment2D;   const Triangle  : TTriangle2D):Boolean;                                         overload;
function Intersect(const Segment:TSegment2D;   const Quadix    : TQuadix2D):Boolean;                                           overload;
function Intersect(const Segment:TSegment2D;   const Line      : TLine2D):Boolean;                                             overload;
function Intersect(const Segment:TSegment2D;   const Circle    : TCircle):Boolean;                                             overload;
function Intersect(const Segment:TSegment3D;   const Sphere    : TSphere):Boolean;                                             overload;
function Intersect(const Line:TLine2D;         const Triangle  : TTriangle2D):Boolean;                                         overload;
function Intersect(const Line:TLine2D;         const Quadix    : TQuadix2D):Boolean;                                           overload;
function Intersect(const Line:TLine2D;         const Circle    : TCircle):Boolean;                                             overload;
function Intersect(const Line:TLine3D;         const Triangle  : TTriangle3D; out IPoint: TPoint3D):Boolean;                   overload;
function Intersect(const Triangle:TTriangle2D; const Circle    : TCircle):Boolean;                                             overload;
function Intersect(const Triangle:TTriangle2D; const Rectangle : TRectangle):Boolean;                                          overload;
function Intersect(const Rectangle1,Rectangle2:TRectangle):Boolean;                                                            overload;
function Intersect(const Triangle1,Triangle2:TTriangle2D):Boolean;                                                             overload;
function Intersect(const Rectangle:TRectangle; const Circle:TCircle):Boolean;                                                  overload;
function Intersect(const Circle1,Circle2:TCircle):Boolean;                                                                     overload;
function Intersect(const Sphere1,Sphere2:TSphere):Boolean;                                                                     overload;
function Intersect(const Poly1,Poly2:TPolygon2D):Boolean;                                                                      overload;
function Intersect(const Obj1,Obj2:TGeometricObject):Boolean;                                                                  overload;

function SimpleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                                        overload;
function SimpleIntersect(const Point1,Point2,Point3,Point4:TPoint2D):Boolean;                                                  overload;
function SimpleIntersect(const Segment1,Segment2:TSegment2D):Boolean;                                                          overload;

function ThickSegmentIntersect(const x1,y1,x2,y2,x3,y3,x4,y4,Thickness:TFloat):Boolean;                                        overload;
function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TPoint2D; const Thickness:TFloat):Boolean;                    overload;
function ThickSegmentIntersect(const Segment1,Segment2:TSegment2D;         const Thickness:TFloat):Boolean;                    overload;

function ThickSegmentIntersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,        Thickness:TFloat):Boolean;                    overload;
function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TPoint3D; const Thickness:TFloat):Boolean;                    overload;
function ThickSegmentIntersect(const Segment1,Segment2:TSegment3D;         const Thickness:TFloat):Boolean;                    overload;

procedure IntersectionPoint(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; out Nx,Ny:TFloat);                                           overload;
procedure IntersectionPoint(const P1,P2,P3,P4:TPoint2D; out Nx,Ny:TFloat);                                                     overload;
function  IntersectionPoint(const P1,P2,P3,P4:TPoint2D):TPoint2D;                                                              overload;
function  IntersectionPoint(const Segment1,Segment2:TSegment2D):TPoint2D;                                                      overload;
function  IntersectionPoint(const Line1,Line2:TLine2D):TPoint2D;                                                               overload;
procedure IntersectionPoint(const Circle1,Circle2:TCircle; out Point1,Point2:TPoint2D);                                        overload;
procedure IntersectionPoint(const Segment:TSegment2D; const Triangle:TTriangle2D; out ICnt:Integer; out I1,I2:TPoint2D);       overload;

procedure IntersectionPoint(const Line:TLine3D; const Triangle:TTriangle3D; out IPoint:TPoint3D);                              overload;
procedure IntersectionPoint(const x1,y1,x2,y2,Cx,Cy,Radius:TFloat; out ICnt:Integer; out Ix1,Iy1,Ix2,Iy2:TFloat);              overload;
procedure IntersectionPoint(const Segment:TSegment2D; const Circle:TCircle; out ICnt:Integer; out I1,I2:TPoint2D);             overload;

function  NormalizeAngle  (const Angle : TFloat) : TFloat;
function  VerticalMirror  (const Angle : TFloat) : TFloat;
function  HorizontalMirror(const Angle : TFloat) : TFloat;

function Quadrant(const Angle : TFloat  ):Integer;                                                                             overload;
function Quadrant(const x,y   : TFloat  ):Integer;                                                                             overload;
function Quadrant(const Point : TPoint2D):Integer;                                                                             overload;

function VertexAngle(x1,y1,x2,y2,x3,y3:TFloat):TFloat;                                                                         overload;
function VertexAngle(const Point1,Point2,Point3:TPoint2D):TFloat;                                                              overload;
function VertexAngle(x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):TFloat;                                                                overload;
function VertexAngle(const Point1,Point2,Point3:TPoint3D):TFloat;                                                              overload;

function OrientedVertexAngle(const x1,y1,x2,y2,x3,y3:TFloat;      const Orient : Integer = Clockwise):TFloat;                  overload;
function OrientedVertexAngle(const Point1,Point2,Point3:TPoint2D; const Orient : Integer = Clockwise):TFloat;                  overload;

function CartesianAngle(const x,y   : TFloat  ):TFloat;                                                                        overload;
function CartesianAngle(const Point : TPoint2D):TFloat;                                                                        overload;

function RobustCartesianAngle(const x,y   : TFloat;  const Epsilon : TFloat = Epsilon_High):TFloat;                            overload;
function RobustCartesianAngle(const Point : TPoint2D;const Epsilon : TFloat = Epsilon_High):TFloat;                            overload;

function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TPoint2D):TFloat;                                             overload;
function SegmentIntersectAngle(const Segment1,Segment2:TSegment2D):TFloat;                                                     overload;
function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TPoint3D):TFloat;                                             overload;
function SegmentIntersectAngle(const Segment1,Segment2:TSegment3D):TFloat;                                                     overload;

function InPortal(const P:TPoint2D):Boolean;                                                                                   overload;
function InPortal(const P:TPoint3D):Boolean;                                                                                   overload;

function HighestPoint(const Polygon  : TPolygon2D):TPoint2D;                                                                   overload;
function HighestPoint(const Point    : array of TPoint2D):TPoint2D;                                                            overload;
function HighestPoint(const Triangle : TTriangle2D):TPoint2D;                                                                  overload;
function HighestPoint(const Triangle : TTriangle3D):TPoint3D;                                                                  overload;
function HighestPoint(const Quadix   : TQuadix2D):TPoint2D;                                                                    overload;
function HighestPoint(const Quadix   : TQuadix3D):TPoint3D;                                                                    overload;

function LowestPoint(const Polygon   : TPolygon2D):TPoint2D;                                                                   overload;
function LowestPoint(const Point     : array of TPoint2D):TPoint2D;                                                            overload;
function LowestPoint(const Triangle  : TTriangle2D):TPoint2D;                                                                  overload;
function LowestPoint(const Triangle  : TTriangle3D):TPoint3D;                                                                  overload;
function LowestPoint(const Quadix    : TQuadix2D):TPoint2D;                                                                    overload;
function LowestPoint(const Quadix    : TQuadix3D):TPoint3D;                                                                    overload;

function MostLeftPoint(const Polygon: TPolygon2D):TPoint2D;                                                                    overload;
function MostLeftPoint(const Point: array of TPoint2D):TPoint2D;                                                               overload;

function MostRightPoint(const Polygon: TPolygon2D):TPoint2D;                                                                   overload;
function MostRightPoint(const Point: array of TPoint2D):TPoint2D;                                                              overload;

function MostUpperRight(const Polygon: TPolygon2D):TPoint2D;                                                                   overload;
function MostUpperRight(const Point: array of TPoint2D):TPoint2D;                                                              overload;

function MostUpperLeft(const Polygon: TPolygon2D):TPoint2D;                                                                    overload;
function MostUpperLeft(const Point: array of TPoint2D):TPoint2D;                                                               overload;

function MostLowerRight(const Polygon: TPolygon2D):TPoint2D;                                                                   overload;
function MostLowerRight(const Point: array of TPoint2D):TPoint2D;                                                              overload;

function MostLowerLeft(const Polygon: TPolygon2D):TPoint2D;                                                                    overload;
function MostLowerLeft(const Point: array of TPoint2D):TPoint2D;                                                               overload;

function Min(const Point1,Point2:TPoint2D):TPoint2D;                                                                           overload;
function Min(const Point1,Point2:TPoint3D):TPoint3D;                                                                           overload;
function Max(const Point1,Point2:TPoint2D):TPoint2D;                                                                           overload;
function Max(const Point1,Point2:TPoint3D):TPoint3D;                                                                           overload;

function Coincident(const Point1,Point2:TPoint2D):Boolean;                                                                     overload;
function Coincident(const Point1,Point2:TPoint3D):Boolean;                                                                     overload;
function Coincident(const Segment1,Segment2:TSegment2D):Boolean;                                                               overload;
function Coincident(const Segment1,Segment2:TSegment3D):Boolean;                                                               overload;
function Coincident(const Triangle1,Triangle2:TTriangle2D):Boolean;                                                            overload;
function Coincident(const Triangle1,Triangle2:TTriangle3D):Boolean;                                                            overload;
function Coincident(const Rect1,Rect2:TRectangle):Boolean;                                                                     overload;
function Coincident(const Quad1,Quad2:TQuadix2D):Boolean;                                                                      overload;
function Coincident(const Quad1,Quad2:TQuadix3D):Boolean;                                                                      overload;
function Coincident(const Circle1,Circle2:TCircle):Boolean;                                                                    overload;
function Coincident(const Sphr1,Sphr2:TSphere):Boolean;                                                                        overload;
function Coincident(const Obj1,Obj2:TGeometricObject):Boolean;                                                                 overload;

function Parallel(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat;       const Epsilon:TFloat = Epsilon_Medium):Boolean;                  overload;
function Parallel(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;                  overload;
function Parallel(const Segment1,Segment2:TSegment2D;         const Epsilon:TFloat = Epsilon_Medium):Boolean;                  overload;
function Parallel(const Line1,Line2:TLine2D;                  const Epsilon:TFloat = Epsilon_Medium):Boolean;                  overload;

function Parallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;
function Parallel(const Point1,Point2,Point3,Point4:TPoint3D;       const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;
function Parallel(const Segment1,Segment2:TSegment3D;               const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;
function Parallel(const Line1,Line2:TLine3D;                        const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;

function RobustParallel(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat;       const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;
function RobustParallel(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;
function RobustParallel(const Segment1,Segment2:TSegment2D;         const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;
function RobustParallel(const Line1,Line2:TLine2D;                  const Epsilon:TFloat = Epsilon_Medium):Boolean;            overload;

function RobustParallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat;const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function RobustParallel(const Point1,Point2,Point3,Point4:TPoint3D;      const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function RobustParallel(const Segment1,Segment2:TSegment3D;              const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function RobustParallel(const Line1,Line2:TLine3D;                       const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;

function Perpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat;       const Epsilon:TFloat = Epsilon_Medium):Boolean;             overload;
function Perpendicular(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;             overload;
function Perpendicular(const Segment1,Segment2:TSegment2D;         const Epsilon:TFloat = Epsilon_Medium):Boolean;             overload;
function Perpendicular(const Line1,Line2:TLine2D;                  const Epsilon:TFloat = Epsilon_Medium):Boolean;             overload;

function Perpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function Perpendicular(const Point1,Point2,Point3,Point4:TPoint3D;       const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function Perpendicular(const Segment1,Segment2:TSegment3D;               const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function Perpendicular(const Line1,Line2:TLine3D;                        const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;

function RobustPerpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat;       const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function RobustPerpendicular(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function RobustPerpendicular(const Segment1,Segment2:TSegment2D;         const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;
function RobustPerpendicular(const Line1,Line2:TLine2D;                  const Epsilon:TFloat = Epsilon_Medium):Boolean;       overload;

function RobustPerpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const Epsilon:TFloat = Epsilon_Medium):Boolean; overload;
function RobustPerpendicular(const Point1,Point2,Point3,Point4:TPoint3D;       const Epsilon:TFloat = Epsilon_Medium):Boolean; overload;
function RobustPerpendicular(const Segment1,Segment2:TSegment3D;               const Epsilon:TFloat = Epsilon_Medium):Boolean; overload;
function RobustPerpendicular(const Line1,Line2:TLine3D;                        const Epsilon:TFloat = Epsilon_Medium):Boolean; overload;

function LineToLineIntersect(x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                                          overload;
function LineToLineIntersect(Line1,Line2:TLine2D):Boolean;                                                                     overload;

function RectangleToRectangleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                          overload;
function RectangleToRectangleIntersect(const Rectangle1,Rectangle2:TRectangle):Boolean;                                        overload;

function RectangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                               overload;
function RectangleWithinRectangle(const Rectangle1,Rectangle2:TRectangle):Boolean;                                             overload;

function CircleWithinRectangle(const x,y,Radius,x1,y1,x2,y2:TFloat):Boolean;                                                   overload;
function CircleWithinRectangle(const Circle:TCircle; const Rectangle:TRectangle):Boolean;                                      overload;

function TriangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4,x5,y5:TFloat):Boolean;                                          overload;
function TriangleWithinRectangle(const Triangle:TTriangle2D; const Rectangle:TRectangle):Boolean;                              overload;

function SegmentWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                                 overload;
function SegmentWithinRectangle(const Segment:TSegment2D; const Rectangle:TRectangle):Boolean;                                 overload;

function CircleInCircle(const Circle1,Circle2:TCircle):Boolean;
function IsTangent(const Segment:TSegment2D; const Circle:TCircle):Boolean;
function PointOfReflection(const Sx1,Sy1,Sx2,Sy2,P1x,P1y,P2x,P2y:TFloat; out RPx,RPy:TFloat):Boolean;                          overload;
function PointOfReflection(const Segment:TSegment2D; const P1,P2:tPoint2D; out RP:TPoint2D):Boolean;                           overload;

procedure Mirror(const Px,Py,x1,y1,x2,y2:TFloat;     out   Nx,Ny:TFloat);                                                      overload;
function  Mirror(const Point     : TPoint2D;         const Line:TLine2D):TPoint2D;                                             overload;
function  Mirror(const Segment   : TSegment2D;       const Line:TLine2D):TSegment2D;                                           overload;
function  Mirror(const Rectangle : TRectangle;       const Line:TLine2D):TRectangle;                                           overload;
function  Mirror(const Triangle  : TTriangle2D;      const Line:TLine2D):TTriangle2D;                                          overload;
function  Mirror(const Quadix    : TQuadix2D;        const Line:TLine2D):TQuadix2D;                                            overload;
function  Mirror(const Circle    : TCircle;          const Line:TLine2D):TCircle;                                              overload;
function  Mirror(const Obj       : TGeometricObject; const Line:TLine2D):TGeometricObject;                                     overload;

procedure NonSymmetricMirror(const Px,Py,x1,y1,x2,y2:TFloat; const Ratio:TFloat; out   Nx,Ny:TFloat);                          overload;
function  NonSymmetricMirror(const Point:TPoint2D;           const Ratio:TFloat; const Line:TLine2D):TPoint2D;                 overload;
function  NonSymmetricMirror(const Segment:TSegment2D;       const Ratio:TFloat; const Line:TLine2D):TSegment2D;               overload;
function  NonSymmetricMirror(const Rectangle:TRectangle;     const Ratio:TFloat; const Line:TLine2D):TRectangle;               overload;
function  NonSymmetricMirror(const Triangle:TTriangle2D;     const Ratio:TFloat; const Line:TLine2D):TTriangle2D;              overload;
function  NonSymmetricMirror(const Quadix:TQuadix2D;         const Ratio:TFloat; const Line:TLine2D):TQuadix2D;                overload;
function  NonSymmetricMirror(const Circle:TCircle;           const Ratio:TFloat; const Line:TLine2D):TCircle;                  overload;
function  NonSymmetricMirror(const Obj:TGeometricObject;     const Ratio:TFloat; const Line:TLine2D):TGeometricObject;         overload;

function Distance(const x1,y1,x2,y2:TFloat):TFloat;                                                                            overload;
function Distance(const Point1,Point2:TPoint2D):TFloat;                                                                        overload;
function Distance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                                                      overload;
function Distance(const Point1,Point2:TPoint3D):TFloat;                                                                        overload;
function Distance(const Point:TPoint2D; const Segment:TSegment2D):TFloat;                                                      overload;
function Distance(const Point:TPoint2D; const Rectangle:TRectangle):TFloat;                                                    overload;
function Distance(const Point:TPoint2D; const Triangle:TTriangle2D):TFloat;                                                    overload;
function Distance(const Point:TPoint2D; const Quadix:TQuadix2D):TFloat;                                                        overload;
function Distance(const Line1,Line2:TLine2D):TFloat;                                                                           overload;
function Distance(const Line1,Line2:TLine3D):TFloat;                                                                           overload;
function Distance(const Segment1,Segment2:TSegment2D):TFloat;                                                                  overload;
function Distance(const Segment1,Segment2:TSegment3D):TFloat;                                                                  overload;
function Distance(const Segment:TSegment2D):TFloat;                                                                            overload;
function Distance(const Segment:TSegment3D):TFloat;                                                                            overload;
function Distance(const Segment:TSegment2D; const Triangle:TTriangle2D):TFloat;                                                overload;
function Distance(const Segment:TSegment3D; const Triangle:TTriangle3D):TFloat;                                                overload;
function Distance(const Segment:TSegment2D; const Rectangle:TRectangle):TFloat;                                                overload;
function Distance(const Segment:TSegment2D; const Circle:TCircle):TFloat;                                                      overload;

function Distance(const Triangle1,Triangle2:TTriangle2D):TFloat;                                                               overload;
function Distance(const Triangle:TTriangle2D; const Rectangle:TRectangle):TFloat;                                              overload;
function Distance(const Rectangle1,Rectangle2:TRectangle):TFloat;                                                              overload;
function Distance(const Triangle:TTriangle2D; const Circle:TCircle):TFloat;                                                    overload;
function Distance(const Rectangle:TRectangle; const Circle:TCircle):TFloat;                                                    overload;
function Distance(const Point : TPoint2D; const Circle:TCircle):TFloat;                                                        overload;
function Distance(const Circle1,Circle2:TCircle):TFloat;                                                                       overload;
function Distance(const Sphere1,Sphere2:TSphere):TFloat;                                                                       overload;
function Distance(const Obj1,Obj2:TGeometricObject):TFloat;                                                                    overload;

function LayDistance(const x1,y1,x2,y2:TFloat):TFloat;                                                                         overload;
function LayDistance(const Point1,Point2:TPoint2D):TFloat;                                                                     overload;
function LayDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                                                   overload;
function LayDistance(const Point1,Point2:TPoint3D):TFloat;                                                                     overload;
function LayDistance(const Point:TPoint2D; const Triangle:TTriangle2D):TFloat;                                                 overload;
function LayDistance(const Point:TPoint2D; const Quadix:TQuadix2D):TFloat;                                                     overload;
function LayDistance(const Segment1,Segment2:TSegment2D):TFloat;                                                               overload;
function LayDistance(const Segment1,Segment2:TSegment3D):TFloat;                                                               overload;
function LayDistance(const Line1,Line2:TLine2D):TFloat;                                                                        overload;
function LayDistance(const Line1,Line2:TLine3D):TFloat;                                                                        overload;
function LayDistance(const Segment:TSegment2D):TFloat;                                                                         overload;
function LayDistance(const Segment:TSegment3D):TFloat;                                                                         overload;
function LayDistance(const Segment:TSegment2D; const Triangle:TTriangle2D):TFloat;                                             overload;
function LayDistance(const Segment:TSegment3D; const Triangle:TTriangle3D):TFloat;                                             overload;

function ManhattanDistance(const x1,y1,x2,y2:TFloat):TFloat;                                                                   overload;
function ManhattanDistance(const Point1,Point2:TPoint2D):TFloat;                                                               overload;
function ManhattanDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                                             overload;
function ManhattanDistance(const Point1,Point2:TPoint3D):TFloat;                                                               overload;
function ManhattanDistance(const Segment:TSegment2D):TFloat;                                                                   overload;
function ManhattanDistance(const Segment:TSegment3D):TFloat;                                                                   overload;
function ManhattanDistance(const Circle1,Circle2:TCircle):TFloat;                                                              overload;

function VectorSumDistance(const x1,y1,x2,y2:TFloat):TFloat;                                                                   overload;
function VectorSumDistance(const Point1,Point2:TPoint2D):TFloat;                                                               overload;
function VectorSumDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                                             overload;
function VectorSumDistance(const Point1,Point2:TPoint3D):TFloat;                                                               overload;
function VectorSumDistance(const Segment:TSegment2D):TFloat;                                                                   overload;
function VectorSumDistance(const Segment:TSegment3D):TFloat;                                                                   overload;
function VectorSumDistance(const Circle1,Circle2:TCircle):TFloat;                                                              overload;
function VectorSumDistance(const Obj1,Obj2:TGeometricObject):TFloat;                                                           overload;

function ChebyshevDistance(const x1,y1,x2,y2:TFloat):TFloat;                                                                   overload;
function ChebyshevDistance(const Point1,Point2:TPoint2D):TFloat;                                                               overload;
function ChebyshevDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                                             overload;
function ChebyshevDistance(const Point1,Point2:TPoint3D):TFloat;                                                               overload;
function ChebyshevDistance(const Segment:TSegment2D):TFloat;                                                                   overload;
function ChebyshevDistance(const Segment:TSegment3D):TFloat;                                                                   overload;
function ChebyshevDistance(const Circle1,Circle2:TCircle):TFloat;                                                              overload;

function InverseChebyshevDistance(const x1,y1,x2,y2:TFloat):TFloat;                                                            overload;
function InverseChebyshevDistance(const Point1,Point2:TPoint2D):TFloat;                                                        overload;
function InverseChebyshevDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                                      overload;
function InverseChebyshevDistance(const Point1,Point2:TPoint3D):TFloat;                                                        overload;
function InverseChebyshevDistance(const Segment:TSegment2D):TFloat;                                                            overload;
function InverseChebyshevDistance(const Segment:TSegment3D):TFloat;                                                            overload;
function InverseChebyshevDistance(const Circle1,Circle2:TCircle):TFloat;                                                       overload;

function DistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TFloat;                                                overload;
function DistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TFloat;                                    overload;

function LayDistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TFloat;                                             overload;
function LayDistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TFloat;                                 overload;

function DistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TFloat;                                                      overload;
function DistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TFloat;                                          overload;

function LayDistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TFloat;                                                   overload;
function LayDistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TFloat;                                       overload;

function TriangleType(const x1,y1,x2,y2,x3,y3:TFloat):eTriangletype;                                                           overload;
function TriangleType(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):eTriangletype;                                                  overload;
function TriangleType(const Point1,Point2,Point3:TPoint2D):eTriangletype;                                                      overload;
function TriangleType(const Point1,Point2,Point3:TPoint3D):eTriangletype;                                                      overload;
function TriangleType(const Triangle:TTriangle2D):eTriangletype;                                                               overload;
function TriangleType(const Triangle:TTriangle3D):eTriangletype;                                                               overload;

function IsEquilateralTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                        overload;
function IsEquilateralTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;                                               overload;
function IsEquilateralTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;                                                   overload;
function IsEquilateralTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;                                                   overload;
function IsEquilateralTriangle(const Triangle:TTriangle2D):Boolean;                                                            overload;
function IsEquilateralTriangle(const Triangle:TTriangle3D):Boolean;                                                            overload;

function IsIsoscelesTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                          overload;
function IsIsoscelesTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;                                                 overload;
function IsIsoscelesTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;                                                     overload;
function IsIsoscelesTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;                                                     overload;
function IsIsoscelesTriangle(const Triangle:TTriangle2D):Boolean;                                                              overload;
function IsIsoscelesTriangle(const Triangle:TTriangle3D):Boolean;                                                              overload;

function IsRightTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                              overload;
function IsRightTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;                                                     overload;
function IsRightTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;                                                         overload;
function IsRightTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;                                                         overload;
function IsRightTriangle(const Triangle:TTriangle2D):Boolean;                                                                  overload;
function IsRightTriangle(const Triangle:TTriangle3D):Boolean;                                                                  overload;

function IsScaleneTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                            overload;
function IsScaleneTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;                                                   overload;
function IsScaleneTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;                                                       overload;
function IsScaleneTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;                                                       overload;
function IsScaleneTriangle(const Triangle:TTriangle2D):Boolean;                                                                overload;
function IsScaleneTriangle(const Triangle:TTriangle3D):Boolean;                                                                overload;

function IsObtuseTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                             overload;
function IsObtuseTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;                                                    overload;
function IsObtuseTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;                                                        overload;
function IsObtuseTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;                                                        overload;
function IsObtuseTriangle(const Triangle:TTriangle2D):Boolean;                                                                 overload;
function IsObtuseTriangle(const Triangle:TTriangle3D):Boolean;                                                                 overload;

function TriangleEdge(const Triangle:TTriangle2D; const Edge:Integer):TSegment2D;                                              overload;
function TriangleEdge(const Triangle:TTriangle3D; const Edge:Integer):TSegment3D;                                              overload;

function RectangleEdge(const Rectangle:TRectangle; const Edge:Integer):TSegment2D;

function PointInTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                        overload;
function PointInTriangle(const x,y:TFloat; const Triangle:TTriangle2D):Boolean;                                                overload;
function PointInTriangle(const Point:TPoint2D; const Triangle:TTriangle2D):Boolean;                                            overload;

function PointInCircle(const Px,Py,Cx,Cy,Radius:TFloat):Boolean;                                                               overload;
function PointInCircle(const Px,Py:TFloat; const Circle:TCircle):Boolean;                                                      overload;
function PointInCircle(const Point:TPoint2D; const Circle:TCircle):Boolean;                                                    overload;

function PointOnCircle(const Px,Py:TFloat; const Circle:TCircle):Boolean;                                                      overload;
function PointOnCircle(const Point:TPoint2D; const Circle:TCircle):Boolean;                                                    overload;

function TriangleInCircle         (const Triangle:TTriangle2D; const Circle:TCircle):Boolean;
function TriangleOutsideCircle    (const Triangle:TTriangle2D; const Circle:TCircle):Boolean;
function TriangleEncompassesCircle(const Triangle:TTriangle2D; const Circle:TCircle):Boolean;
function RectangleInCircle        (const Rectangle:TRectangle; const Circle:TCircle):Boolean;
function RectangleOutsideCircle   (const Rectangle:TRectangle; const Circle:TCircle):Boolean;
function QuadixInCircle           (const Quadix:TQuadix2D;     const Circle:TCircle):Boolean;
function QuadixOutsideCircle      (const Quadix:TQuadix2D;     const Circle:TCircle):Boolean;

function PointInThreePointCircle(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat):Boolean;                                                overload;
function PointInThreePointCircle(const Point,Point1,Point2,Point3:TPoint2D):Boolean;                                           overload;
function PointInThreePointCircle(const Point:TPoint2D; const Triangle:TTriangle2D):Boolean;                                    overload;

function PointInRectangle(const Px,Py:TFloat;   const x1,y1,x2,y2:TFloat):Boolean;                                             overload;
function PointInRectangle(const Point:TPoint2D; const x1,y1,x2,y2:TFloat):Boolean;                                             overload;
function PointInRectangle(const Px,Py:TFloat;   const Rectangle:TRectangle):Boolean;                                           overload;
function PointInRectangle(const Point:TPoint2D; const Rectangle:TRectangle):Boolean;                                           overload;
function TriangleInRectangle(const Triangle:TTriangle2D; const Rectangle:TRectangle):Boolean;
function TriangleOutsideRectangle(const Triangle:TTriangle2D; const Rectangle:TRectangle):Boolean;
function QuadixInRectangle(const Quadix:TQuadix2D; const Rectangle:TRectangle):Boolean;
function QuadixOutsideRectangle(const Quadix:TQuadix2D; const Rectangle:TRectangle):Boolean;

function PointInQuadix(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                                    overload;
function PointInQuadix(const Point,Point1,Point2,Point3,Point4:TPoint2D):Boolean;                                              overload;
function PointInQuadix(const x,y:TFloat; const Quadix:TQuadix2D):Boolean;                                                      overload;
function PointInQuadix(const Point:TPoint2D; const Quadix:TQuadix2D):Boolean;                                                  overload;
function TriangleInQuadix(const Triangle:TTriangle2D; const Quadix:TQuadix2D):Boolean;
function TriangleOutsideQuadix(const Triangle:TTriangle2D; const Quadix:TQuadix2D):Boolean;

function PointInSphere(const x,y,z:TFloat; const Sphere:TSphere):Boolean;                                                      overload;
function PointInSphere(const Point3D:TPoint3D; const Sphere:TSphere):Boolean;                                                  overload;
function PointOnSphere(const Point3D:TPoint3D; const Sphere:TSphere):Boolean;                                                  overload;
function PolyhedronInSphere(const Polygon:TPolyhedron; const Sphere:TSphere):TInclusion;

function PointOnPerimeter(const Px,Py,x1,y1,x2,y2:TFloat):Boolean;                                                             overload;
function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat; Robust:Boolean = False):Boolean;                               overload;
function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;                                                 overload;

function PointOnPerimeter(const Point:TPoint2D; const Rectangle : TRectangle):Boolean;                                         overload;
function PointOnPerimeter(const Point:TPoint2D; const Triangle  : TTriangle2D):Boolean;                                        overload;
function PointOnPerimeter(const Point:TPoint2D; const Quadix    : TQuadix2D):Boolean;                                          overload;
function PointOnPerimeter(const Point:TPoint2D; const Circle    : TCircle):Boolean;                                            overload;
function PointOnPerimeter(const Point:TPoint3D; const Sphere    : TSphere):Boolean;                                            overload;
function PointOnPerimeter(const Point:TPoint2D; const Polygon   : TPolygon2D):Boolean;                                         overload;
function PointOnPerimeter(const Point:TPoint2D; const Obj       : TGeometricObject):Boolean;                                   overload;
function PointOnPerimeter(const Point:TPoint3D; const Obj       : TGeometricObject):Boolean;                                   overload;

function PointInObject(const Point:TPoint2D; const Segment   : TSegment2D ):Boolean;                                           overload;
function PointInObject(const Point:TPoint2D; const Line      : TLine2D    ):Boolean;                                           overload;
function PointInObject(const Point:TPoint2D; const Rectangle : TRectangle ):Boolean;                                           overload;
function PointInObject(const Point:TPoint2D; const Triangle  : TTriangle2D):Boolean;                                           overload;
function PointInObject(const Point:TPoint2D; const Quadix    : TQuadix2D  ):Boolean;                                           overload;
function PointInObject(const Point:TPoint2D; const Circle    : TCircle    ):Boolean;                                           overload;
function PointInObject(const Point:TPoint2D; const Polygon   : TPolygon2D ):Boolean;                                           overload;
function PointInObject(const Point:TPoint2D; const Obj       : TGeometricObject):Boolean;                                      overload;

function GeometricSpan(const Point: array of TPoint2D):TFloat;                                                                 overload;
function GeometricSpan(const Point: array of TPoint3D):TFloat;                                                                 overload;

procedure CreateEquilateralTriangle(x1,y1,x2,y2:TFloat; out x3,y3:TFloat);                                                     overload;
procedure CreateEquilateralTriangle(const Point1,Point2:TPoint2D; out Point3:TPoint2D);                                        overload;
function  CreateEquilateralTriangle(const x1,y1,x2,y2:TFloat):TTriangle2D;                                                     overload;
function  CreateEquilateralTriangle(const Point1,Point2:TPoint2D):TTriangle2D;                                                 overload;
function  CreateEquilateralTriangle(const Cx,Cy,SideLength : TFloat) : TTriangle2D;                                            overload;
function  CreateEquilateralTriangle(const CenterPoint : TPoint2D; const SideLength : TFloat) : TTriangle2D;                    overload;

procedure TorricelliPoint(const x1,y1,x2,y2,x3,y3:TFloat; out Px,Py:TFloat);                                                   overload;
function  TorricelliPoint(const Point1,Point2,Point3:TPoint2D):TPoint2D;                                                       overload;
function  TorricelliPoint(const Triangle:TTriangle2D):TPoint2D;                                                                overload;

procedure Incenter(const x1,y1,x2,y2,x3,y3:TFloat; out Px,Py:TFloat);                                                          overload;
procedure Incenter(const Triangle:TTriangle2D; out Px,Py:TFloat);                                                              overload;
function  Incenter(const Point1,Point2,Point3:TPoint2D):TPoint2D;                                                              overload;
function  Incenter(const Triangle:TTriangle2D):TPoint2D;                                                                       overload;

procedure Circumcenter(const x1,y1,x2,y2,x3,y3:TFloat; out Px,Py:TFloat);                                                      overload;
function  Circumcenter(const Point1,Point2,Point3:TPoint2D):TPoint2D;                                                          overload;
function  Circumcenter(const Triangle:TTriangle2D):TPoint2D;                                                                   overload;

function Circumcircle(const P1,P2,P3:TPoint2D):TCircle;                                                                        overload;
function Circumcircle(const Triangle:TTriangle2D):TCircle;                                                                     overload;
function InscribedCircle(const x1,y1,x2,y2,x3,y3:TFloat):TCircle;                                                              overload;
function InscribedCircle(const P1,P2,P3:TPoint2D):TCircle;                                                                     overload;
function InscribedCircle(const Triangle:TTriangle2D):TCircle;                                                                  overload;

procedure ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TFloat; out Nx,Ny:TFloat);                                    overload;
procedure ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat; out Nx,Ny,Nz:TFloat);                        overload;
procedure ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TFloat; out Nx,Ny:TFloat);                                       overload;
procedure ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat; out Nx,Ny,Nz:TFloat);                           overload;

function ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TFloat):TPoint2D;                                              overload;
function ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat):TPoint3D;                                     overload;

function ClosestPointOnSegmentFromPoint(const Segment:TSegment2D; const Point:TPoint2D):TPoint2D;                              overload;
function ClosestPointOnSegmentFromPoint(const Segment:TSegment3D; const Point:TPoint3D):TPoint3D;                              overload;

function ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TFloat):TPoint2D;                                                 overload;
function ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat):TPoint3D;                                        overload;

function ClosestPointOnLineFromPoint(const Line:TLine2D; const Point:TPoint2D):TPoint2D;                                       overload;
function ClosestPointOnLineFromPoint(const Line:TLine3D; const Point:TPoint3D):TPoint3D;                                       overload;

procedure ClosestPointOnTriangleFromPoint(const x1,y1,x2,y2,x3,y3,Px,Py:TFloat; out Nx,Ny:TFloat);                             overload;
function  ClosestPointOnTriangleFromPoint(const Triangle:TTriangle2D;  const Px,Py:TFloat  ):TPoint2D;                         overload;
function  ClosestPointOnTriangleFromPoint(const Triangle:TTriangle2D;  const Point:TPoint2D):TPoint2D;                         overload;
function  ClosestPointOnTriangleFromPoint(const Triangle:TTriangle3D;  const Point:TPoint3D):TPoint3D;                         overload;

procedure ClosestPointOnRectangleFromPoint(const x1,y1,x2,y2,Px,Py:TFloat; out Nx,Ny:TFloat);                                  overload;
function  ClosestPointOnRectangleFromPoint(const Rectangle:TRectangle;  const Px,Py:TFloat  ):TPoint2D;                        overload;
function  ClosestPointOnRectangleFromPoint(const Rectangle:TRectangle;  const Point:TPoint2D):TPoint2D;                        overload;

function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix2D;      const Point:TPoint2D):TPoint2D;                            overload;
function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix3D;      const Point:TPoint3D):TPoint3D;                            overload;
function ClosestPointOnCircleFromPoint(const Circle:TCircle;        const Point:TPoint2D):TPoint2D;
function ClosestPointOnSphereFromPoint(const Sphere:TSphere;        const Point:TPoint3D):TPoint3D;
function ClosestPointOnAABBFromPoint  (const Rectangle: TRectangle; const Point:TPoint2D):TPoint2D;

function ClosestPointOnCircleFromSegment(const Circle:TCircle; Segment:TSegment2D):TPoint2D;
function ClosestPointOnSphereFromSegment(const Sphere:TSphere; Segment:TSegment3D):TPoint3D;

function MinimumDistanceFromPointToSegment(const Px,Py,x1,y1,x2,y2:TFloat):TFloat;                                             overload;
function MinimumDistanceFromPointToSegment(const Point:TPoint2D; const Segment:TSegment2D):TFloat;                             overload;
function MinimumDistanceFromPointToSegment(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                    overload;
function MinimumDistanceFromPointToSegment(const Point:TPoint3D; const Segment:TSegment3D):TFloat;                             overload;

function MinimumDistanceFromPointToLine(const Px,Py,x1,y1,x2,y2:TFloat):TFloat;                                                overload;
function MinimumDistanceFromPointToLine(const Point:TPoint2D; const Line:TLine2D):TFloat;                                      overload;
function MinimumDistanceFromPointToLine(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TFloat):TFloat;                                       overload;
function MinimumDistanceFromPointToLine(const Point:TPoint3D; const Line:TLine3D):TFloat;                                      overload;

function MinimumDistanceFromPointToTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat):TFloat;                                      overload;
function MinimumDistanceFromPointToTriangle(const Point:TPoint2D; const Triangle:TTriangle2D):TFloat;                          overload;

function MinimumDistanceFromPointToRectangle(const Px,Py,x1,y1,x2,y2:TFloat):TFloat;                                           overload;
function MinimumDistanceFromPointToRectangle(const Point:TPoint2D; const Rectangle:TRectangle):TFloat;                         overload;

function MinimumDistanceFromPointToPolygon(const Point:TPoint2D; const Polygon:TPolygon2D): TFloat                             overload;

procedure SegmentMidPoint(const x1,y1,x2,y2:TFloat; out midx,midy:TFloat);                                                     overload;
procedure SegmentMidPoint(const Segment:TSegment2D; out midx,midy:TFloat);                                                     overload;
function  SegmentMidPoint(const P1,P2:TPoint2D):TPoint2D;                                                                      overload;
function  SegmentMidPoint(const Segment:TSegment2D):TPoint2D;                                                                  overload;

procedure SegmentMidPoint(const x1,y1,z1,x2,y2,z2:TFloat; out midx,midy,midz:TFloat);                                          overload;
function  SegmentMidPoint(const P1,P2:TPoint3D):TPoint3D;                                                                      overload;
function  SegmentMidPoint(const Segment:TSegment3D):TPoint3D;                                                                  overload;

procedure Centroid(const x1,y1,x2,y2:TFloat; out x,y:TFloat);                                                                  overload;
function  Centroid(const P1,P2:TPoint2D):TPoint2D;                                                                             overload;
function  Centroid(const Segment:TSegment2D):TPoint2D;                                                                         overload;

procedure Centroid(const x1,y1,x2,y2,x3,y3:TFloat; out x,y:TFloat);                                                            overload;
procedure Centroid(const Triangle:TTriangle2D; out x,y:TFloat);                                                                overload;
procedure Centroid(const Rectangle:TRectangle; out x,y:TFloat);                                                                overload;
function  Centroid(const P1,P2,P3:TPoint2D):TPoint2D;                                                                          overload;
function  Centroid(const Triangle:TTriangle2D):TPoint2D;                                                                       overload;
function  Centroid(const Rectangle:TRectangle):TPoint2D;                                                                       overload;

procedure Centroid(const Polygon:TPolygon2D; out x,y:TFloat);                                                                  overload;
function  Centroid(const Polygon :TPolygon2D):TPoint2D;                                                                        overload;
function  Centroid(const Polygon : array of TPoint3D):TPoint3D;                                                                overload;

function PolygonSegmentIntersect(const Segment:TSegment2D; const Polygon: TPolygon2D):Boolean;
function PolygonInPolygon(const Poly1,Poly2: TPolygon2D):Boolean;

function PointInConvexPolygon(const Px,Py:TFloat; const Polygon:TPolygon2D):Boolean;                                           overload;
function PointInConvexPolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;                                         overload;

function PointInConcavePolygon(const Px,Py:TFloat; const Polygon:TPolygon2D):Boolean;                                          overload;
function PointInConcavePolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;                                        overload;

function PointOnPolygon(const Px,Py:TFloat;   const Polygon:TPolygon2D):Boolean;                                               overload;
function PointOnPolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;                                               overload;

function PointInPolygon(const Px,Py:TFloat;             const Polygon:TPolygon2D):Boolean;                                     overload;
function PointInPolygon(const Point:TPoint2D;           const Polygon:TPolygon2D):Boolean;                                     overload;
function PointInPolygon(const Point: array of TPoint2D; const Polygon:TPolygon2D): TBooleanArray;                              overload;

function ConvexQuadix(const Quadix:TQuadix2D):Boolean;
function ComplexPolygon(const Polygon:TPolygon2D):Boolean;
function SimplePolygon(const Polygon:TPolygon2D):Boolean;
function ConvexPolygon(const Polygon:TPolygon2D):Boolean;                                                                      overload;
function ConvexPolygon(const Polygon: array of TPoint2D):Boolean;                                                              overload;
function ConcavePolygon(const Polygon:TPolygon2D):Boolean;
function ConvexPolygonOrientation(const Polygon:TPolygon2D):Integer;
function SimplePolygonOrientation(const Polygon:TPolygon2D):Integer;
function SelfIntersectingPolygon(const Polygon:TPolygon2D):Boolean;

function RectangularHull(const Point: array of TPoint2D):TRectangle;                                                           overload;
function RectangularHull(const Polygon:TPolygon2D):TRectangle;                                                                 overload;
function CircularHull(const Polygon:TPolygon2D):TCircle;
function SphereHull(const Polygon:array of TPoint3D):TSphere;

function CalculateBarycentricBase(const x1,y1,x2,y2,x3,y3:TFloat):TFloat;
function CreateBarycentricUnit(const x1,y1,x2,y2,x3,y3:TFloat):TBarycentricUnit;                                               overload;
function CreateBarycentricUnit(const Triangle : TTriangle2D):TBarycentricUnit;                                                 overload;

procedure ConvertCartesianToBarycentric(const x1,y1,x2,y2,x3,y3,px,py:TFloat; out u,v,w: TFloat);                              overload;
procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; Px,Py:TFloat; out u,v,w: TFloat);                           overload;
procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TPoint2D; out BCrd: TBarycentricTriplet);       overload;
function  ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TPoint2D): TBarycentricTriplet;                 overload;

procedure ConvertBarycentricToCartesian(const u,v,w,x1,y1,x2,y2,x3,y3:TFloat; out x,y:TFloat);                                 overload;
procedure ConvertBarycentricToCartesian(const u,v,w:TFloat; BU:TBarycentricUnit; out x,y:TFloat);                              overload;
procedure ConvertBarycentricToCartesian(const u,v,w:TFloat; BU:TBarycentricUnit; out Point:TPoint2D);                          overload;
function  ConvertBarycentricToCartesian(const u,v,w:TFloat; BU:TBarycentricUnit):TPoint2D;                                     overload;

function Clip(const Segment:TSegment2D; const Rect     : TRectangle;       out CSegment:TSegment2D):Boolean;                   overload;
function Clip(const Segment:TSegment2D; const Triangle : TTriangle2D;      out CSegment:TSegment2D):Boolean;                   overload;
function Clip(const Segment:TSegment2D; const Quadix   : TQuadix2D;        out CSegment:TSegment2D):Boolean;                   overload;
function Clip(const Segment:TSegment2D; const Circle   : TCircle;          out CSegment:TSegment2D):Boolean;                   overload;
function Clip(const Segment:TSegment2D; const Obj      : TGeometricObject; out CSegment:TSegment2D):Boolean;                   overload;

function Area(const Point1,Point2,Point3:TPoint2D):TFloat;                                                                     overload;
function Area(const Point1,Point2,Point3:TPoint3D):TFloat;                                                                     overload;
function Area(const Triangle:TTriangle2D):TFloat;                                                                              overload;
function Area(const Triangle:TTriangle3D):TFloat;                                                                              overload;
function Area(const Quadix:TQuadix2D):TFloat;                                                                                  overload;
function Area(const Quadix:TQuadix3D):TFloat;                                                                                  overload;
function Area(const Rectangle:TRectangle):TFloat;                                                                              overload;
function Area(const Circle:TCircle):TFloat;                                                                                    overload;
function Area(const Polygon:TPolygon2D):TFloat;                                                                                overload;
function Area(const Obj:TGeometricObject):TFloat;                                                                              overload;

function Perimeter(const Triangle  : TTriangle2D):TFloat;                                                                      overload;
function Perimeter(const Triangle  : TTriangle3D):TFloat;                                                                      overload;
function Perimeter(const Quadix    : TQuadix2D):TFloat;                                                                        overload;
function Perimeter(const Quadix    : TQuadix3D):TFloat;                                                                        overload;
function Perimeter(const Rectangle : TRectangle):TFloat;                                                                       overload;
function Perimeter(const Circle    : TCircle):TFloat;                                                                          overload;
function Perimeter(const Polygon   : TPolygon2D):TFloat;                                                                       overload;
function Perimeter(const Obj       : TGeometricObject):TFloat;                                                                 overload;

function SemiPerimeter(const Triangle : TTriangle2D):TFloat;                                                                   overload;
function SemiPerimeter(const Triangle : TTriangle3D):TFloat;                                                                   overload;

procedure Rotate(RotAng:TFloat; const x,y:TFloat; out Nx,Ny:TFloat);                                                           overload;
procedure Rotate(const RotAng:TFloat; const x,y,ox,oy:TFloat; out Nx,Ny:TFloat);                                               overload;

function Rotate(const RotAng:TFloat; const Point:TPoint2D):TPoint2D;                                                           overload;
function Rotate(const RotAng:TFloat; const Point,OPoint:TPoint2D):TPoint2D;                                                    overload;
function Rotate(const RotAng:TFloat; const Segment  : TSegment2D):TSegment2D;                                                  overload;
function Rotate(const RotAng:TFloat; const Segment  : TSegment2D; const OPoint: TPoint2D):TSegment2D;                          overload;
function Rotate(const RotAng:TFloat; const Triangle : TTriangle2D):TTriangle2D;                                                overload;
function Rotate(const RotAng:TFloat; const Triangle : TTriangle2D; const OPoint:TPoint2D):TTriangle2D;                         overload;
function Rotate(const RotAng:TFloat; const Quadix   : TQuadix2D):TQuadix2D;                                                    overload;
function Rotate(const RotAng:TFloat; const Quadix   : TQuadix2D; const OPoint:TPoint2D):TQuadix2D;                             overload;
function Rotate(const RotAng:TFloat; const Polygon  : TPolygon2D):TPolygon2D;                                                  overload;
function Rotate(const RotAng:TFloat; const Polygon  : TPolygon2D; const OPoint:TPoint2D):TPolygon2D;                           overload;
function Rotate(const RotAng:TFloat; const Obj      : TGeometricObject):TGeometricObject;                                      overload;
function Rotate(const RotAng:TFloat; const Obj      : TGeometricObject; const OPoint: TPoint2D):TGeometricObject;              overload;

procedure Rotate(const Rx,Ry,Rz:TFloat; const x,y,z:TFloat; out Nx,Ny,Nz:TFloat);                                              overload;
procedure Rotate(const Rx,Ry,Rz:TFloat; const x,y,z,ox,oy,oz:TFloat; out Nx,Ny,Nz:TFloat);                                     overload;

function Rotate(const Rx,Ry,Rz:TFloat; const Point:TPoint3D):TPoint3D;                                                         overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Point,OPoint:TPoint3D):TPoint3D;                                                  overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Segment  : TSegment3D):TSegment3D;                                                overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Segment  : TSegment3D; const OPoint: TPoint3D):TSegment3D;                        overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Triangle : TTriangle3D):TTriangle3D;                                              overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Triangle : TTriangle3D; const OPoint:TPoint3D):TTriangle3D;                       overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Quadix   : TQuadix3D):TQuadix3D;                                                  overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Quadix   : TQuadix3D; const OPoint:TPoint3D):TQuadix3D;                           overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Polygon  : TPolygon3D):TPolygon3D;                                                overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Polygon  : TPolygon3D; const OPoint:TPoint3D):TPolygon3D;                         overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Obj      : TGeometricObject):TGeometricObject;                                    overload;
function Rotate(const Rx,Ry,Rz:TFloat; const Obj      : TGeometricObject; const OPoint: TPoint3D):TGeometricObject;            overload;

procedure FastRotate(RotAng:Integer; const x,y:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure FastRotate(RotAng:Integer; x,y,ox,oy:TFloat; out Nx,Ny:TFloat);                                                      overload;

function FastRotate(const RotAng:Integer; const Point:TPoint2D):TPoint2D;                                                      overload;
function FastRotate(const RotAng:Integer; const Point,OPoint:TPoint2D):TPoint2D;                                               overload;
function FastRotate(const RotAng:Integer; const Segment  : TSegment2D):TSegment2D;                                             overload;
function FastRotate(const RotAng:Integer; const Segment  : TSegment2D; const OPoint: TPoint2D):TSegment2D;                     overload;
function FastRotate(const RotAng:Integer; const Triangle : TTriangle2D):TTriangle2D;                                           overload;
function FastRotate(const RotAng:Integer; const Triangle : TTriangle2D; const OPoint:TPoint2D):TTriangle2D;                    overload;
function FastRotate(const RotAng:Integer; const Quadix   : TQuadix2D):TQuadix2D;                                               overload;
function FastRotate(const RotAng:Integer; const Quadix   : TQuadix2D; const OPoint:TPoint2D):TQuadix2D;                        overload;
function FastRotate(const RotAng:Integer; const Polygon  : TPolygon2D):TPolygon2D;                                             overload;
function FastRotate(const RotAng:Integer; const Polygon  : TPolygon2D; const OPoint:TPoint2D):TPolygon2D;                      overload;
function FastRotate(const RotAng:Integer; const Obj      : TGeometricObject):TGeometricObject;                                 overload;
function FastRotate(const RotAng:Integer; const Obj      : TGeometricObject; const OPoint: TPoint2D):TGeometricObject;         overload;

procedure FastRotate(Rx,Ry,Rz:Integer; const x,y,z:TFloat; out Nx,Ny,Nz:TFloat);                                               overload;
procedure FastRotate(const Rx,Ry,Rz:Integer; const x,y,z,ox,oy,oz:TFloat; out Nx,Ny,Nz:TFloat);                                overload;

function FastRotate(const Rx,Ry,Rz:Integer; const Point:TPoint3D):TPoint3D;                                                    overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Point,OPoint:TPoint3D):TPoint3D;                                             overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Segment  : TSegment3D):TSegment3D;                                           overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Segment  : TSegment3D; const OPoint: TPoint3D):TSegment3D;                   overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Triangle : TTriangle3D):TTriangle3D;                                         overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Triangle : TTriangle3D; const OPoint:TPoint3D):TTriangle3D;                  overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Quadix   : TQuadix3D):TQuadix3D;                                             overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Quadix   : TQuadix3D; const OPoint:TPoint3D):TQuadix3D;                      overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Polygon  : TPolygon3D):TPolygon3D;                                           overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Polygon  : TPolygon3D; const OPoint:TPoint3D):TPolygon3D;                    overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Obj      : TGeometricObject):TGeometricObject;                               overload;
function FastRotate(const Rx,Ry,Rz:Integer; const Obj      : TGeometricObject; const OPoint: TPoint3D):TGeometricObject;       overload;

function Translate(const Dx,Dy:TFloat;   const Point     : TPoint2D):TPoint2D;                                                 overload;
function Translate(const Dx,Dy:TFloat;   const Line      : TLine2D):TLine2D;                                                   overload;
function Translate(const Dx,Dy:TFloat;   const Segment   : TSegment2D):TSegment2D;                                             overload;
function Translate(const Dx,Dy:TFloat;   const Triangle  : TTriangle2D):TTriangle2D;                                           overload;
function Translate(const Dx,Dy:TFloat;   const Quadix    : TQuadix2D):TQuadix2D;                                               overload;
function Translate(const Dx,Dy:TFloat;   const Rectangle : TRectangle):TRectangle;                                             overload;
function Translate(const Dx,Dy:TFloat;   const Circle    : TCircle):TCircle;                                                   overload;
function Translate(const Dx,Dy:TFloat;   const Polygon   : TPolygon2D):TPolygon2D;                                             overload;
function Translate(const Point:TPoint2D; const Polygon   : TPolygon2D):TPolygon2D;                                             overload;
function Translate(const Dx,Dy:TFloat;   const Obj       : TGeometricObject):TGeometricObject;                                 overload;

function Translate(const Delta:TFloat;   const Point     : TPoint2D):TPoint2D;                                                 overload;
function Translate(const Delta:TFloat;   const Line      : TLine2D):TLine2D;                                                   overload;
function Translate(const Delta:TFloat;   const Segment   : TSegment2D):TSegment2D;                                             overload;
function Translate(const Delta:TFloat;   const Triangle  : TTriangle2D):TTriangle2D;                                           overload;
function Translate(const Delta:TFloat;   const Quadix    : TQuadix2D):TQuadix2D;                                               overload;
function Translate(const Delta:TFloat;   const Rectangle : TRectangle):TRectangle;                                             overload;
function Translate(const Delta:TFloat;   const Circle    : TCircle):TCircle;                                                   overload;
function Translate(const Delta:TFloat;   const Polygon   : TPolygon2D):TPolygon2D;                                             overload;
function Translate(const Delta:TFloat;   const Obj       : TGeometricObject):TGeometricObject;                                 overload;

function Translate(const Dx,Dy,Dz:TFloat; const Point    : TPoint3D):TPoint3D;                                                 overload;
function Translate(const Dx,Dy,Dz:TFloat; const Line     : TLine3D):TLine3D;                                                   overload;
function Translate(const Dx,Dy,Dz:TFloat; const Segment  : TSegment3D):TSegment3D;                                             overload;
function Translate(const Dx,Dy,Dz:TFloat; const Triangle : TTriangle3D):TTriangle3D;                                           overload;
function Translate(const Dx,Dy,Dz:TFloat; const Quadix   : TQuadix3D):TQuadix3D;                                               overload;
function Translate(const Dx,Dy,Dz:TFloat; const Sphere   : TSphere):TSphere;                                                   overload;
function Translate(const Dx,Dy,Dz:TFloat; const Polygon  : TPolygon3D):TPolygon3D;                                             overload;
function Translate(const Point:TPoint3D;  const Polygon  : TPolygon3D):TPolygon3D;                                             overload;
function Translate(const Dx,Dy,Dz:TFloat; const Obj      : TGeometricObject):TGeometricObject;                                 overload;

function Scale(const Dx,Dy:TFloat; const Point     : TPoint2D):TPoint2D;                                                       overload;
function Scale(const Dx,Dy:TFloat; const Line      : TLine2D):TLine2D;                                                         overload;
function Scale(const Dx,Dy:TFloat; const Segment   : TSegment2D):TSegment2D;                                                   overload;
function Scale(const Dx,Dy:TFloat; const Triangle  : TTriangle2D):TTriangle2D;                                                 overload;
function Scale(const Dx,Dy:TFloat; const Quadix    : TQuadix2D):TQuadix2D;                                                     overload;
function Scale(const Dx,Dy:TFloat; const Rectangle : TRectangle):TRectangle;                                                   overload;
function Scale(const Dr   :TFloat; const Circle    : TCircle):TCircle;                                                         overload;
function Scale(const Dx,Dy:TFloat; const Polygon   : TPolygon2D):TPolygon2D;                                                   overload;
function Scale(const Dx,Dy:TFloat; const Obj       : TGeometricObject):TGeometricObject;                                       overload;

function Scale(const Dx,Dy,Dz:TFloat; const Point    : TPoint3D):TPoint3D;                                                     overload;
function Scale(const Dx,Dy,Dz:TFloat; const Line     : TLine3D):TLine3D;                                                       overload;
function Scale(const Dx,Dy,Dz:TFloat; const Segment  : TSegment3D):TSegment3D;                                                 overload;
function Scale(const Dx,Dy,Dz:TFloat; const Triangle : TTriangle3D):TTriangle3D;                                               overload;
function Scale(const Dx,Dy,Dz:TFloat; const Quadix   : TQuadix3D):TQuadix3D;                                                   overload;
function Scale(const Dr      :TFloat; const Sphere   : TSphere):TSphere;                                                       overload;
function Scale(const Dx,Dy,Dz:TFloat; const Polygon  : TPolygon3D):TPolygon3D;                                                 overload;
function Scale(const Dx,Dy,Dz:TFloat; const Obj      : TGeometricObject):TGeometricObject;                                     overload;

procedure ShearXAxis(const Shear,x,y:TFloat; out Nx,Ny:TFloat);                                                                overload;
function ShearXAxis(const Shear:TFloat; const Point    : TPoint2D):TPoint2D;                                                   overload;
function ShearXAxis(const Shear:TFloat; const Segment  : TSegment2D):TSegment2D;                                               overload;
function ShearXAxis(const Shear:TFloat; const Triangle : TTriangle2D):TTriangle2D;                                             overload;
function ShearXAxis(const Shear:TFloat; const Quadix   : TQuadix2D):TQuadix2D;                                                 overload;
function ShearXAxis(const Shear:TFloat; const Polygon  : TPolygon2D):TPolygon2D;                                               overload;
function ShearXAxis(const Shear:TFloat; const Obj      : TGeometricObject):TGeometricObject;                                   overload;

procedure ShearYAxis(const Shear,x,y:TFloat; out Nx,Ny:TFloat);                                                                overload;
function ShearYAxis(const Shear:TFloat; const Point    : TPoint2D):TPoint2D;                                                   overload;
function ShearYAxis(const Shear:TFloat; const Segment  : TSegment2D):TSegment2D;                                               overload;
function ShearYAxis(const Shear:TFloat; const Triangle : TTriangle2D):TTriangle2D;                                             overload;
function ShearYAxis(const Shear:TFloat; const Quadix   : TQuadix2D):TQuadix2D;                                                 overload;
function ShearYAxis(const Shear:TFloat; const Polygon  : TPolygon2D):TPolygon2D;                                               overload;
function ShearYAxis(const Shear:TFloat; const Obj      : TGeometricObject):TGeometricObject;                                   overload;

function CenterAtLocation(const Point:TPoint2D;       const x,y:TFloat):TPoint2D;                                              overload;
function CenterAtLocation(const Segment:TSegment2D;   const x,y:TFloat):TSegment2D;                                            overload;
function CenterAtLocation(const Triangle:TTriangle2D; const x,y:TFloat):TTriangle2D;                                           overload;
function CenterAtLocation(const Rectangle:TRectangle; const x,y:TFloat):TRectangle;                                            overload;
function CenterAtLocation(const Quadix:TQuadix2D;     const x,y:TFloat):TQuadix2D;                                             overload;
function CenterAtLocation(const Circle:TCircle;       const x,y:TFloat):TCircle;                                               overload;
function CenterAtLocation(const Polygon:TPolygon2D;   const x,y:TFloat):TPolygon2D;                                            overload;

function CenterAtLocation(const Point,                      CPoint:TPoint2D):TPoint2D;                                         overload;
function CenterAtLocation(const Segment:TSegment2D;   const CPoint:TPoint2D):TSegment2D;                                       overload;
function CenterAtLocation(const Triangle:TTriangle2D; const CPoint:TPoint2D):TTriangle2D;                                      overload;
function CenterAtLocation(const Rectangle:TRectangle; const CPoint:TPoint2D):TRectangle;                                       overload;
function CenterAtLocation(const Quadix:TQuadix2D;     const CPoint:TPoint2D):TQuadix2D;                                        overload;
function CenterAtLocation(const Circle:TCircle;       const CPoint:TPoint2D):TCircle;                                          overload;
function CenterAtLocation(const Polygon:TPolygon2D;   const CPoint:TPoint2D):TPolygon2D;                                       overload;

function AABB(const Segment   : TSegment2D   ):TRectangle;                                                                     overload;
function AABB(const Triangle  : TTriangle2D  ):TRectangle;                                                                     overload;
function AABB(const Rectangle : TRectangle   ):TRectangle;                                                                     overload;
function AABB(const Quadix    : TQuadix2D    ):TRectangle;                                                                     overload;
function AABB(const Circle    : TCircle      ):TRectangle;                                                                     overload;
function AABB(const Polygon   : TPolygon2D   ):TRectangle;                                                                     overload;
function AABB(const Curve     : TPoint2DArray):TRectangle;                                                                     overload;

procedure AABB(const Segment   : TSegment2D;    out x1,y1,x2,y2:TFloat);                                                       overload;
procedure AABB(const Triangle  : TTriangle2D;   out x1,y1,x2,y2:TFloat);                                                       overload;
procedure AABB(const Rectangle : TRectangle;    out x1,y1,x2,y2:TFloat);                                                       overload;
procedure AABB(const Quadix    : TQuadix2D;     out x1,y1,x2,y2:TFloat);                                                       overload;
procedure AABB(const Circle    : TCircle;       out x1,y1,x2,y2:TFloat);                                                       overload;
procedure AABB(const Polygon   : TPolygon2D;    out x1,y1,x2,y2:TFloat);                                                       overload;
procedure AABB(const Curve     : TPoint2DArray; out x1,y1,x2,y2:TFloat);                                                       overload;

procedure ProjectPoint(const Srcx,Srcy,Dstx,Dsty,Dist:TFloat; out Nx,Ny:TFloat);                                               overload;
procedure ProjectPoint(const Srcx,Srcy,Srcz,Dstx,Dsty,Dstz,Dist:TFloat; out Nx,Ny,Nz:TFloat);                                  overload;

procedure ProjectPoint(const Px,Py,Angle,Distance:TFloat; out Nx,Ny:TFloat);                                                   overload;
procedure ProjectPoint0  (const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure ProjectPoint45 (const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure ProjectPoint90 (const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure ProjectPoint135(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure ProjectPoint180(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure ProjectPoint225(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure ProjectPoint270(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;
procedure ProjectPoint315(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);                                                      overload;

function ProjectPoint(const SrcPoint,DstPoint:TPoint2D; const Dist:TFloat):TPoint2D;                                           overload;
function ProjectPoint(const SrcPoint,DstPoint:TPoint3D; const Dist:TFloat):TPoint3D;                                           overload;

function ProjectPoint(const Point:TPoint2D; Angle,Distance:TFloat):TPoint2D;                                                   overload;

function ProjectPoint0  (const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;
function ProjectPoint45 (const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;
function ProjectPoint90 (const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;
function ProjectPoint135(const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;
function ProjectPoint180(const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;
function ProjectPoint225(const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;
function ProjectPoint270(const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;
function ProjectPoint315(const Point:TPoint2D; Distance:TFloat):TPoint2D;                                                      overload;

function ProjectObject(const Point    : TPoint2D;         Angle,Distance:TFloat):TPoint2D;                                     overload;
function ProjectObject(const Segment  : TSegment2D;       Angle,Distance:TFloat):TSegment2D;                                   overload;
function ProjectObject(const Triangle : TTriangle2D;      Angle,Distance:TFloat):TTriangle2D;                                  overload;
function ProjectObject(const Quadix   : TQuadix2D;        Angle,Distance:TFloat):TQuadix2D;                                    overload;
function ProjectObject(const Circle   : TCircle;          Angle,Distance:TFloat):TCircle;                                      overload;
function ProjectObject(const Polygon  : TPolygon2D;       Angle,Distance:TFloat):TPolygon2D;                                   overload;
function ProjectObject(const GeoObj   : TGeometricObject; Angle,Distance:TFloat):TGeometricObject;                             overload;

procedure CalculateBezierCoefficients(const Bezier : TQuadraticBezier2D; out ax,bx,ay,by:TFloat);                              overload;
procedure CalculateBezierCoefficients(const Bezier : TQuadraticBezier3D; out ax,bx,ay,by,az,bz:TFloat);                        overload;

procedure CalculateBezierCoefficients(const Bezier : TCubicBezier2D; out ax,bx,cx,ay,by,cy:TFloat);                            overload;
procedure CalculateBezierCoefficients(const Bezier : TCubicBezier3D; out ax,bx,cx,ay,by,cy,az,bz,cz:TFloat);                   overload;

function  PointOnBezier(const StartPoint:TPoint2D; const ax,bx,ay,by,T:TFloat):TPoint2D;                                       overload;
function  PointOnBezier(const StartPoint:TPoint3D; const ax,bx,ay,by,az,bz,T:TFloat):TPoint3D;                                 overload;

function  PointOnBezier(const StartPoint:TPoint2D; const ax,bx,cx,ay,by,cy,T:TFloat):TPoint2D;                                 overload;
function  PointOnBezier(const StartPoint:TPoint3D; const ax,bx,cx,ay,by,cy,az,bz,cz,T:TFloat):TPoint3D;                        overload;

function  CreateBezier(const Bezier:TQuadraticBezier2D; const PointCount:Integer):TPoint2DArray;                               overload;
function  CreateBezier(const Bezier:TQuadraticBezier3D; const PointCount:Integer):TPoint3DArray;                               overload;

function  CreateBezier(const Bezier:TCubicBezier2D; const PointCount:Integer):TPoint2DArray;                                   overload;
function  CreateBezier(const Bezier:TCubicBezier3D; const PointCount:Integer):TPoint3DArray;                                   overload;

function  CreateCurvePointBezier(const Bezier:TQuadraticBezier2D; const PointCount:Integer):TCurvePoint2DArray;                overload;
function  CreateCurvePointBezier(const Bezier:TQuadraticBezier3D; const PointCount:Integer):TCurvePoint3DArray;                overload;

function  CreateCurvePointBezier(const Bezier:TCubicBezier2D; const PointCount:Integer):TCurvePoint2DArray;                    overload;
function  CreateCurvePointBezier(const Bezier:TCubicBezier3D; const PointCount:Integer):TCurvePoint3DArray;                    overload;

function CurveLength(const Bezier:TQuadraticBezier2D; const PointCount:Integer):TFloat;                                        overload;
function CurveLength(const Bezier:TQuadraticBezier3D; const PointCount:Integer):TFloat;                                        overload;

function CurveLength(const Bezier:TCubicBezier2D; const PointCount:Integer):TFloat;                                            overload;
function CurveLength(const Bezier:TCubicBezier3D; const PointCount:Integer):TFloat;                                            overload;

procedure ShortenSegment(const Amount:TFloat; out x1,y1,x2,y2:TFloat);                                                         overload;
procedure ShortenSegment(const Amount:TFloat; out x1,y1,z1,x2,y2,z2:TFloat);                                                   overload;
function  ShortenSegment(const Segment:TSegment2D; const Amount:TFloat):TSegment2D;                                            overload;
function  ShortenSegment(const Segment:TSegment3D; const Amount:TFloat):TSegment3D;                                            overload;

procedure LengthenSegment(const Amount:TFloat; out x1,y1,x2,y2:TFloat);                                                        overload;
procedure LengthenSegment(const Amount:TFloat; out x1,y1,z1,x2,y2,z2:TFloat);                                                  overload;
function  LengthenSegment(const Segment:TSegment2D; const Amount:TFloat):TSegment2D;                                           overload;
function  LengthenSegment(const Segment:TSegment3D; const Amount:TFloat):TSegment3D;                                           overload;

function EquatePoint(const x,y:TFloat):TPoint2D;                                                                               overload;
function EquatePoint(const x,y,z:TFloat):TPoint3D;                                                                             overload;
function EquatePointPtr(const x,y:TFloat):TPoint2DPtr;                                                                         overload;
function EquatePointPtr(const x,y,z:TFloat):TPoint3DPtr;                                                                       overload;

procedure EquatePoint(const x,y:TFloat; out Point:TPoint2D);                                                                   overload;
procedure EquatePoint(const x,y,z:TFloat; out Point:TPoint3D);                                                                 overload;
procedure EquatePointPtr(const x,y:TFloat; out Point:TPoint2DPtr);                                                             overload;
procedure EquatePointPtr(const x,y,z:TFloat; out Point:TPoint3DPtr);                                                           overload;

function EquateCurvePoint(x,y,t:TFloat            ):TCurvePoint2D;                                                             overload;
function EquateCurvePoint(x,y,z,t:TFloat          ):TCurvePoint3D;                                                             overload;
function EquateCurvePoint(Point:TPoint2D; t:TFloat):TCurvePoint2D;                                                             overload;
function EquateCurvePoint(Point:TPoint3D; t:TFloat):TCurvePoint3D;                                                             overload;

function EquateSegment(const x1,y1,x2,y2:TFloat):TSegment2D;                                                                   overload;
function EquateSegment(const x1,y1,z1,x2,y2,z2:TFloat):TSegment3D;                                                             overload;

function EquateSegment(const Point1,Point2:TPoint2D):TSegment2D;                                                               overload;
function EquateSegment(const Point1,Point2:TPoint3D):TSegment3D;                                                               overload;

procedure EquateSegment(const x1,y1,x2,y2:TFloat; out Segment:TSegment2D);                                                     overload;
procedure EquateSegment(const x1,y1,z1,x2,y2,z2:TFloat; out Segment:TSegment3D);                                               overload;

function EquateLine(const x1,y1,x2,y2:TFloat):TLine2D;                                                                         overload;
function EquateLine(const x1,y1,z1,x2,y2,z2:TFloat):TLine3D;                                                                   overload;

function EquateLine(const Point1,Point2:TPoint2D):TLine2D;                                                                     overload;
function EquateLine(const Point1,Point2:TPoint3D):TLine3D;                                                                     overload;

procedure EquateLine(const x1,y1,x2,y2:TFloat; out Line:TLine2D);                                                              overload;
procedure EquateLine(const x1,y1,z1,x2,y2,z2:TFloat; out Line:TLine3D);                                                        overload;

function EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TQuadix2D;                                                         overload;
function EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TQuadix3D;                                             overload;

function EquateQuadix(const Point1,Point2,Point3,Point4:TPoint2D):TQuadix2D;                                                   overload;
function EquateQuadix(const Point1,Point2,Point3,Point4:TPoint3D):TQuadix3D;                                                   overload;

procedure EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat;             out Quadix:TQuadix2D);                                overload;
procedure EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; out Quadix:TQuadix3D);                                overload;

function  EquateRectangle(const x1,y1,x2,y2:TFloat):TRectangle;                                                                overload;
function  EquateRectangle(const Point1,Point2:TPoint2D):TRectangle;                                                            overload;
procedure EquateRectangle(const x1,y1,x2,y2:TFloat; out Rect:TRectangle);                                                      overload;
procedure EquateRectangle(const Point1,Point2:TPoint2D; out Rect:TRectangle);                                                  overload;

function EquateTriangle(const x1,y1,x2,y2,x3,y3:TFloat):TTriangle2D;                                                           overload;
function EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):TTriangle3D;                                                  overload;
function EquateTriangle(const Point1,Point2,Point3:TPoint2D):TTriangle2D;                                                      overload;
function EquateTriangle(const Point1,Point2,Point3:TPoint3D):TTriangle3D;                                                      overload;

procedure EquateTriangle(const x1,y1,x2,y2,x3,y3:TFloat;          out Triangle:TTriangle2D);                                   overload;
procedure EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat; out Triangle:TTriangle3D);                                   overload;
procedure EquateTriangle(const Point1,Point2,Point3: TPoint2D;    out Triangle:TTriangle2D);                                   overload;
procedure EquateTriangle(const Point1,Point2,Point3: TPoint3D;    out Triangle:TTriangle3D);                                   overload;

function  EquateCircle(const x,y,r:TFloat):TCircle;                                                                            overload;
function  EquateCircle(const Point:TPoint2D; Radius:TFloat):TCircle;                                                           overload;
procedure EquateCircle(const x,y,r:TFloat; out Circle:TCircle);                                                                overload;

function  EquateSphere(const x,y,z,r:TFloat):TSphere;                                                                          overload;
procedure EquateSphere(const x,y,z,r:TFloat; out Sphere:TSphere);                                                              overload;

function EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):TPlane2D;                                                        overload;
function EquatePlane(const Point1,Point2,Point3:TPoint3D):TPlane2D;                                                            overload;

procedure EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat; out Plane:TPlane2D);                                            overload;
procedure EquatePlane(const Point1,Point2,Point3:TPoint3D;     out Plane:TPlane2D);                                            overload;

procedure EquateBezier(const x1,y1,x2,y2,x3,y3:TFloat;          out Bezier:TQuadraticBezier2D);                                overload;
procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat; out Bezier:TQuadraticBezier3D);                                overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3:TPoint2D):TQuadraticBezier2D;                                                      overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3:TPoint3D):TQuadraticBezier3D;                                                      overload;

procedure EquateBezier(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat;             out Bezier:TCubicBezier2D);                           overload;
procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; out Bezier:TCubicBezier3D);                           overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TPoint2D):TCubicBezier2D;                                                     overload;
function  EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TPoint3D):TCubicBezier3D;                                                     overload;

function  RectangleToQuadix(x1,y1,x2,y2:TFloat    ):TQuadix2D;                                                                 overload;
function  RectangleToQuadix(Point1,Point2:TPoint2D):TQuadix2D;                                                                 overload;
function  RectangleToQuadix(Rectangle:TRectangle  ):TQuadix2D;                                                                 overload;

function  TriangleToPolygon(x1,y1,x2,y2,x3,y3:TFloat):TPolygon2D;                                                              overload;
function  TriangleToPolygon(Triangle:TTriangle2D):TPolygon2D;                                                                  overload;
function  QuadixToPolygon(x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TPolygon2D;                                                          overload;
function  QuadixToPolygon(Quadix:TQuadix2D):TPolygon2D;                                                                        overload;
function  CircleToPolygon(const Cx,Cy,Radius:TFloat; const PointCount:Integer):TPolygon2D;                                     overload;
function  CircleToPolygon(const Circle:TCircle;      const PointCount:Integer):TPolygon2D;                                     overload;

procedure SetGeometricObject(const Primitive:TPoint2D;    out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TPoint3D;    out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TLine2D;     out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TLine3D;     out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TSegment2D;  out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TSegment3D;  out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TTriangle2D; out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TTriangle3D; out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TQuadix2D;   out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TQuadix3D;   out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TRectangle;  out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TCircle;     out GeoObj:TGeometricObject);                                        overload;
procedure SetGeometricObject(const Primitive:TSphere;     out GeoObj:TGeometricObject);                                        overload;

procedure GenerateRandomPoints(const Bx1,By1,Bx2,By2:TFloat; var PointList:array of TPoint2D);                                 overload;
procedure GenerateRandomPoints(const Rectangle:TRectangle;   var PointList:array of TPoint2D);                                 overload;
procedure GenerateRandomPoints(const Segment:TSegment2D;     var PointList:array of TPoint2D);                                 overload;
procedure GenerateRandomPoints(const Segment:TSegment3D;     var PointList:array of TPoint3D);                                 overload;
procedure GenerateRandomPoints(const Triangle:TTriangle2D;   var PointList:array of TPoint2D);                                 overload;
procedure GenerateRandomPoints(const Circle:TCircle;         var PointList:array of TPoint2D);                                 overload;
procedure GenerateRandomPoints(const Quadix:TQuadix2D;       var PointList:array of TPoint2D);                                 overload;

procedure GenerateRandomPointsOnConvexPentagon(const Pentagon : TPolygon2D; var PointList : array of TPoint2D);
procedure GenerateRandomPointsOnConvexHexagon (const Hexagon  : TPolygon2D; var PointList : array of TPoint2D);
procedure GenerateRandomPointsOnConvexHeptagon(const Heptagon : TPolygon2D; var PointList : array of TPoint2D);
procedure GenerateRandomPointsOnConvexOctagon (const Octagon  : TPolygon2D; var PointList : array of TPoint2D);

procedure GenerateRandomTriangle(const Bx1,By1,Bx2,By2:TFloat; out Triangle : TTriangle2D);
procedure GenerateRandomQuadix  (const Bx1,By1,Bx2,By2:TFloat; out Quadix   : TQuadix2D);
procedure GenerateRandomCircle  (const Bx1,By1,Bx2,By2:TFloat; out Circle   : TCircle);

function Add(const Vec1,Vec2:TVector2D):TVector2D;                                                                             overload;
function Add(const Vec1,Vec2:TVector3D):TVector3D;                                                                             overload;

function Add(const Vec:TVector2DArray):TVector2D;                                                                              overload;
function Add(const Vec:TVector3DArray):TVector3D;                                                                              overload;
function Add(const Vec1,Vec2:TVector2DArray):TVector2DArray;                                                                   overload;
function Add(const Vec1,Vec2:TVector3DArray):TVector3DArray;                                                                   overload;

function Sub(const Vec1,Vec2:TVector2D):TVector2D;                                                                             overload;
function Sub(const Vec1,Vec2:TVector3D):TVector3D;                                                                             overload;
function Sub(const Vec1,Vec2:TVector2DArray):TVector2DArray;                                                                   overload;
function Sub(const Vec1,Vec2:TVector3DArray):TVector3DArray;                                                                   overload;

function Mul(const Vec1,Vec2:TVector2D):TVector3D;                                                                             overload;
function Mul(const Vec1,Vec2:TVector3D):TVector3D;                                                                             overload;
function Mul(const Vec1,Vec2:TVector3DArray):TVector3DArray;                                                                   overload;

function UnitVector(const Vec:TVector2D):TVector2D;                                                                            overload;
function UnitVector(const Vec:TVector3D):TVector3D;                                                                            overload;

function Magnitude(const Vec:TVector2D):TFloat;                                                                                overload;
function Magnitude(const Vec:TVector3D):TFloat;                                                                                overload;

function DotProduct(const Vec1,Vec2:TVector2D):TFloat;                                                                         overload;
function DotProduct(const Vec1,Vec2:TVector3D):TFloat;                                                                         overload;

function Scale(const Vec:TVector2D; const Factor:TFloat):TVector2D;                                                            overload;
function Scale(const Vec:TVector3D; const Factor:TFloat):TVector3D;                                                            overload;

function Scale(const Vec:TVector2DArray; const Factor:TFloat):TVector2DArray;                                                  overload;
function Scale(const Vec:TVector3DArray; const Factor:TFloat):TVector3DArray;                                                  overload;

function Negate(const Vec:TVector2D):TVector2D;                                                                                overload;
function Negate(const Vec:TVector3D):TVector3D;                                                                                overload;

function Negate(Vec:TVector2DArray):TVector2DArray;                                                                            overload;
function Negate(Vec:TVector3DArray):TVector3DArray;                                                                            overload;

function IsEqual(const Val1,Val2,Epsilon:TFloat):Boolean;                                                                      overload;
function IsEqual(const Point1,Point2:TPoint2D; const Epsilon:TFloat):Boolean;                                                  overload;
function IsEqual(const Point1,Point2:TPoint3D; const Epsilon:TFloat):Boolean;                                                  overload;

function IsEqual(const Val1,Val2:TFloat):Boolean;                                                                              overload;
function IsEqual(const Point1,Point2:TPoint2D):Boolean;                                                                        overload;
function IsEqual(const Point1,Point2:TPoint3D):Boolean;                                                                        overload;

function NotEqual(const Val1,Val2,Epsilon:TFloat):Boolean;                                                                     overload;
function NotEqual(const Point1,Point2:TPoint2D; const Epsilon:TFloat):Boolean;                                                 overload;
function NotEqual(const Point1,Point2:TPoint3D; const Epsilon:TFloat):Boolean;                                                 overload;

function NotEqual(const Val1,Val2:TFloat):Boolean;                                                                             overload;
function NotEqual(const Point1,Point2:TPoint2D):Boolean;                                                                       overload;
function NotEqual(const Point1,Point2:TPoint3D):Boolean;                                                                       overload;

function LessThanOrEqual(const Val1,Val2,Epsilon:TFloat):Boolean;                                                              overload;
function LessThanOrEqual(const Val1,Val2:TFloat):Boolean;                                                                      overload;
function GreaterThanOrEqual(const Val1,Val2,Epsilon:TFloat):Boolean;                                                           overload;
function GreaterThanOrEqual(const Val1,Val2:TFloat):Boolean;                                                                   overload;

function IsEqualZero(const Val,Epsilon:TFloat):Boolean;                                                                        overload;
function IsEqualZero(const Val:TFloat):Boolean;                                                                                overload;

function IsDegenerate(const x1,y1,x2,y2:TFloat):Boolean;                                                                       overload;
function IsDegenerate(const Segment:TSegment2D):Boolean;                                                                       overload;
function IsDegenerate(const Line:TLine2D):Boolean;                                                                             overload;

function IsDegenerate(const x1,y1,z1,x2,y2,z2:TFloat):Boolean;                                                                 overload;
function IsDegenerate(const Segment:TSegment3D):Boolean;                                                                       overload;
function IsDegenerate(const Line:TLine3D):Boolean;                                                                             overload;

function IsDegenerate(const Triangle:TTriangle2D):Boolean;                                                                     overload;
function IsDegenerate(const Triangle:TTriangle3D):Boolean;                                                                     overload;

function IsDegenerate(const Quadix:TQuadix2D):Boolean;                                                                         overload;
function IsDegenerate(const Quadix:TQuadix3D):Boolean;                                                                         overload;

function IsDegenerate(const Rect:TRectangle):Boolean;                                                                          overload;

function IsDegenerate(const Circle:TCircle):Boolean;                                                                           overload;
function IsDegenerate(const Sphere:TSphere):Boolean;                                                                           overload;

function IsDegenerate(const Arc:TCircularArc2D):Boolean;                                                                       overload;

function IsDegenerate(const Obj:TGeometricObject):Boolean;                                                                     overload;

procedure Swap(var val1,val2           :TFloat);                                                                               overload;
procedure Swap(var val1,val2           :Integer);                                                                              overload;
procedure Swap(var Point1,Point2       :TPoint2D);                                                                             overload;
procedure Swap(var Point1,Point2       :TPoint3D);                                                                             overload;
procedure Swap(var Segment1,Segment2   :TSegment2D);                                                                           overload;
procedure Swap(var Segment1,Segment2   :TSegment3D);                                                                           overload;
procedure Swap(var Line1,Line2         :TLine2D);                                                                              overload;
procedure Swap(var Triangle1,Triangle2 :TTriangle2D);                                                                          overload;
procedure Swap(var Triangle1,Triangle2 :TTriangle3D);                                                                          overload;
procedure Swap(var Quadix1,Quadix2     :TQuadix2D);                                                                            overload;
procedure Swap(var Quadix1,Quadix2     :TQuadix3D);                                                                            overload;
procedure Swap(var Circle1,Circle2     :TCircle);                                                                              overload;
procedure Swap(var Sphere1,Sphere2     :TSphere);                                                                              overload;
procedure Swap(var Arc1,Arc2           :TCircularArc2D);                                                                       overload;

function CalculateSystemEpsilon:TFloat;
function ZeroEquivalency:Boolean;
function ExtendedFloatingPointTest:Boolean;
function ExecuteTests:TNumericPrecisionResult;



const PI2       =  6.283185307179586476925286766559000;
const PIDiv180  =  0.017453292519943295769236907684886;
const _180DivPI = 57.295779513082320876798154814105000;

var

 SystemEpsilon : TFloat;

 (* 2D/3D Portal Definition *)
 MaximumX : TFloat;
 MinimumX : TFloat;
 MaximumY : TFloat;
 MinimumY : TFloat;
 MaximumZ : TFloat;
 MinimumZ : TFloat;


 SinTable : array of TFloat;
 CosTable : array of TFloat;
 TanTable : array of TFloat;

procedure InitialiseTrigonometryTables;
function  RandomValue(ResInt : Integer = RandomResolutionInt; ResFlt : TFloat = RandomResolutionFlt) : TFloat;

implementation

uses
   Math;


function Orientation(const x1,y1,x2,y2,Px,Py:TFloat):Integer;
var
  Orin : TFloat;
begin
  (* Determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  if Orin > 0.0 then
    Result := LeftHandSide          (* Orientaion is to the left-hand side  *)
  else if Orin < 0.0 then
    Result := RightHandSide         (* Orientaion is to the right-hand side *)
  else
    Result := CollinearOrientation; (* Orientaion is neutral aka collinear  *)
end;
(* End of Orientation *)


function Orientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TFloat):Integer;
var
  Px1  : TFloat;
  Px2  : TFloat;
  Px3  : TFloat;
  Py1  : TFloat;
  Py2  : TFloat;
  Py3  : TFloat;
  Pz1  : TFloat;
  Pz2  : TFloat;
  Pz3  : TFloat;
  Orin : TFloat;
begin
  Px1 := x1 - px;
  Px2 := x2 - px;
  Px3 := x3 - px;

  Py1 := y1 - py;
  Py2 := y2 - py;
  Py3 := y3 - py;

  Pz1 := z1 - pz;
  Pz2 := z2 - pz;
  Pz3 := z3 - pz;

  Orin  := Px1 * (Py2 * Pz3 - Pz2 * Py3) +
           Px2 * (Py3 * Pz1 - Pz3 * Py1) +
           Px3 * (Py1 * Pz2 - Pz1 * Py2);

  if Orin < 0.0  then
    Result := -1           (* Orientaion is below plane                      *)
  else if Orin > 0.0 then
    Result := +1           (* Orientaion is above plane                      *)
  else
    Result := 0;           (* Orientaion is coplanar to plane if Result is 0 *)
end;
(* End of Orientation *)


function RobustOrientation(const x1,y1,x2,y2,Px,Py:TFloat):Integer;
var
  Orin : TFloat;
begin
  (* Linear determinant of the 3 points *)
  Orin := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);

  (*
    Calculation Policy:
    if |Orin - Orin`| < Epsilon then Orin` is assumed to be equal to zero.
    Where:
     Orin : is the "real" mathematically precise orientation value, using infinite
            precision arithmetic (hypothetical)
     Orin`: is the calculated imprecise orientation value, using finite precision arithmetic
  *)

  if IsEqual(Orin,0.0) then
    Result := 0                (* Orientaion is neutral aka collinear  *)
  else if Orin < 0.0 then
    Result := RightHandSide    (* Orientaion is to the right-hand side *)
  else
    Result := LeftHandSide;    (* Orientaion is to the left-hand side  *)
end;
(* End of Robust Orientation *)


function RobustOrientation(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TFloat):Integer;
var
  Px1  : TFloat;
  Px2  : TFloat;
  Px3  : TFloat;
  Py1  : TFloat;
  Py2  : TFloat;
  Py3  : TFloat;
  Pz1  : TFloat;
  Pz2  : TFloat;
  Pz3  : TFloat;
  Orin : TFloat;
begin
  Px1 := x1 - px;
  Px2 := x2 - px;
  Px3 := x3 - px;

  Py1 := y1 - py;
  Py2 := y2 - py;
  Py3 := y3 - py;

  Pz1 := z1 - pz;
  Pz2 := z2 - pz;
  Pz3 := z3 - pz;

  Orin  := Px1 * (Py2 * Pz3 - Pz2 * Py3) +
           Px2 * (Py3 * Pz1 - Pz3 * Py1) +
           Px3 * (Py1 * Pz2 - Pz1 * Py2);

 if IsEqual(Orin,0.0) then
   Result :=  0             (* Orientaion is coplanar to plane if Result is 0 *)
 else if Orin < 0.0 then
   Result := -1             (* Orientaion is below plane                      *)
 else
   Result := +1;            (* Orientaion is above plane                      *)
end;
(* End of Robust Orientation *)


function Orientation(const Point1,Point2:TPoint2D; const Px,Py:TFloat):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point2.x,Point2.y,Px,Py);
end;
(* End of Orientation *)


function Orientation(const Point1,Point2,Point3:TPoint2D):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Orientation *)


function Orientation(const Line:TLine2D; const Point:TPoint2D):Integer;
begin
  Result := Orientation(Line[1].x,Line[1].y,Line[2].x,Line[2].y,Point.x,Point.y);
end;
(* End of Orientation *)


function Orientation(const Segment:TSegment2D; const Point:TPoint2D):Integer;
begin
  Result := Orientation(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Point.x,Point.y);
end;
(* End of Orientation *)


function Orientation(const Point1,Point2,Point3:TPoint3D; const Px,Py,Pz:TFloat):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Px,Py,Pz);
end;
(* End of Orientation *)


function Orientation(const Point1,Point2,Point3,Point4:TPoint3D):Integer;
begin
  Result := Orientation(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z);
end;
(* End of Orientation *)


function Orientation(const Triangle:TTriangle3D; const Point:TPoint3D):Integer;
begin
  Result := Orientation(Triangle[1],Triangle[2],Triangle[3],Point);
end;
(* End of Orientation *)


function Signed(const x1,y1,x2,y2,Px,Py:TFloat):TFloat;
begin
  Result := (x2 - x1) * (py - y1) - (px - x1) * (y2 - y1);
end;
(* End of Signed *)


function Signed(const x1,y1,z1,x2,y2,z2,x3,y3,z3,Px,Py,Pz:TFloat):TFloat;
var
  Px1 : TFloat;
  Px2 : TFloat;
  Px3 : TFloat;
  Py1 : TFloat;
  Py2 : TFloat;
  Py3 : TFloat;
  Pz1 : TFloat;
  Pz2 : TFloat;
  Pz3 : TFloat;
begin
  Px1 := x1 - px;
  Px2 := x2 - px;
  Px3 := x3 - px;

  Py1 := y1 - py;
  Py2 := y2 - py;
  Py3 := y3 - py;

  Pz1 := z1 - pz;
  Pz2 := z2 - pz;
  Pz3 := z3 - pz;

  Result:= Px1 * (Py2 * Pz3 - Pz2 * Py3) +
           Px2 * (Py3 * Pz1 - Pz3 * Py1) +
           Px3 * (Py1 * Pz2 - Pz1 * Py2);
end;
(* End of Signed *)


function Signed(const Point1,Point2:TPoint2D; const Px,Py:TFloat):TFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point2.x,Point2.y,Px,Py);
end;
(* End of Signed *)


function Signed(const Point1,Point2,Point3:TPoint2D):TFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Signed *)


function Signed(const Line:TLine2D; const Point:TPoint2D):TFloat;
begin
  Result := Signed(Line[1].x,Line[1].y,Line[2].x,Line[2].y,Point.x,Point.y);
end;
(* End of Signed *)


function Signed(const Segment:TSegment2D; const Point:TPoint2D):TFloat;
begin
  Result := Signed(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Point.x,Point.y);
end;
(* End of Signed *)


function Signed(const Point1,Point2,Point3:TPoint3D; const Px,Py,Pz:TFloat):TFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Px,Py,Pz);
end;
(* End of Signed *)


function Signed(const Point1,Point2,Point3,Point4:TPoint3D):TFloat;
begin
  Result := Signed(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z);
end;
(* End of Signed *)


function Signed(const Triangle:TTriangle3D; const Point:TPoint3D):TFloat;
begin
  Result := Signed(Triangle[1],Triangle[2],Triangle[3],Point);
end;
(* End of Signed *)


function Collinear(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;
begin
  Result := IsEqual((x2 - x1) * (y3 - y1) ,(x3 - x1) * (y2 - y1));
end;
(* End of Collinear *)


function Collinear(const x1,y1,x2,y2,x3,y3,Epsilon:TFloat):Boolean;
begin
  Result := IsEqual((x2 - x1) * (y3 - y1),(x3 - x1) * (y2 - y1),Epsilon);
end;
(* End of Collinear *)


function Collinear(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;
var
  Dx1  : TFloat;
  Dx2  : TFloat;
  Dy1  : TFloat;
  Dy2  : TFloat;
  Dz1  : TFloat;
  Dz2  : TFloat;
  Cx   : TFloat;
  Cy   : TFloat;
  Cz   : TFloat;
begin
  (* Find the difference between the 2 points P2 and P3 to P1 *)
  Dx1 := x2 - x1;
  Dy1 := y2 - y1;
  Dz1 := z2 - z1;

  Dx2 := x3 - x1;
  Dy2 := y3 - y1;
  Dz2 := z3 - z1;

  (* Perform a 3d cross product *)
  Cx  := (Dy1 * Dz2) - (Dy2 * Dz1);
  Cy  := (Dx2 * Dz1) - (Dx1 * Dz2);
  Cz  := (Dx1 * Dy2) - (Dx2 * Dy1);

  Result := IsEqual(Cx * Cx + Cy * Cy + Cz * Cz,0.0);
end;
(* End of Collinear *)


function Collinear(const PointA,PointB,PointC:TPoint2D):Boolean;
begin
  Result := Collinear(PointA.x,PointA.y,PointB.x,PointB.y,PointC.x,PointC.y);
end;
(* End of Collinear *)


function Collinear(const PointA,PointB,PointC:TPoint3D):Boolean;
begin
  Result := Collinear(PointA.x,PointA.y,PointA.z,PointB.x,PointB.y,PointB.z,PointC.x,PointC.y,PointC.z);
end;
(* End of Collinear *)


function RobustCollinear(const x1,y1,x2,y2,x3,y3:TFloat; const Epsilon : TFloat = Epsilon_High):Boolean;
var
  LeyDist1 : TFloat;
  LeyDist2 : TFloat;
  LeyDist3 : TFloat;
begin
  LeyDist1 := LayDistance(x1,y1,x2,y2);
  LeyDist2 := LayDistance(x2,y2,x3,y3);
  LeyDist3 := LayDistance(x3,y3,x1,y1);

  if LeyDist1 >= LeyDist2 then
    if LeyDist1 >= LeyDist3 then
      Result := IsEqual(MinimumDistanceFromPointToLine(x3,y3,x1,y1,x2,y2),0.0,Epsilon)
    else
      Result := IsEqual(MinimumDistanceFromPointToLine(x2,y2,x3,y3,x1,y1),0.0,Epsilon)
  else if LeyDist2 >= LeyDist3 then
    Result := IsEqual(MinimumDistanceFromPointToLine(x1,y1,x2,y2,x3,y3),0.0,Epsilon)
  else
    Result := IsEqual(MinimumDistanceFromPointToLine(x2,y2,x3,y3,x1,y1),0.0,Epsilon);
end;
(* End of Robust Collinear *)


function RobustCollinear(const PointA,PointB,PointC:TPoint2D; const Epsilon : TFloat = Epsilon_High):Boolean;
begin
  Result := RobustCollinear(PointA.x,PointA.y,PointB.x,PointB.y,PointC.x,PointC.y,Epsilon);
end;
(* End of Robust Collinear *)


function Coplanar(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):Boolean;
begin
  Result := (Orientation(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4) = CoplanarOrientation);
end;
(* End of Coplanar *)


function Coplanar(const PointA,PointB,PointC,PointD:TPoint3D):Boolean;
begin
  Result := (Orientation(PointA,PointB,PointC,PointD) = CoplanarOrientation);
end;
(* End of Coplanar *)


function IsPointCollinear(const x1,y1,x2,y2,Px,Py:TFloat; const Robust : Boolean = False):Boolean;
begin
  (*
    This method will return true iff the point (px,py) is collinear
    to points (x1,y1) and (x2,y2) and exists on the segment A(x1,y1)->B(x2,y2)
  *)
  if (((x1 <= px) and (px <= x2)) or ((x2 <= px) and (px <= x1))) and
     (((y1 <= py) and (py <= y2)) or ((y2 <= py) and (py <= y1))) then
  begin
    if Robust then
      Result := RobustCollinear(x1,y1,x2,y2,Px,Py)
    else
      Result := Collinear(x1,y1,x2,y2,Px,Py);
  end
  else
    Result := False;
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const PointA,PointB,PointC:TPoint2D; const Robust : Boolean = False):Boolean;
begin
 (*
   This method will return true iff the pointC is collinear
   to points A and B and exists on the segment A->B or B->A
 *)
 Result := IsPointCollinear(PointA.x,PointA.y,PointB.x,PointB.y,PointC.x,PointC.y,Robust);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const PointA,PointB:TPoint2D; const Px,Py:TFloat; const Robust : Boolean = False):Boolean;
begin
  Result := IsPointCollinear(PointA.x,PointA.y,PointB.x,PointB.y,Px,Py,Robust);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const Segment:TSegment2D; const PointC:TPoint2D; const Robust : Boolean = False):Boolean;
begin
  Result := IsPointCollinear(Segment[1],Segment[2],PointC,Robust);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat):Boolean;
begin
 (*
   This method will return true iff the point (px,py,pz) is collinear
   to points (x1,y1,z1) and (x2,y2,z2) and exists on the segment A(x1,y1,z1)->B(x2,y2,z2)
 *)
 if (((x1 <= px) and (px <= x2)) or ((x2 <= px) and (px <= x1))) and
    (((y1 <= py) and (py <= y2)) or ((y2 <= py) and (py <= y1))) and
    (((z1 <= pz) and (pz <= z2)) or ((z2 <= pz) and (pz <= z1))) then
 begin
   Result := Collinear(x1,y1,z1,x2,y2,z2,Px,Py,Pz);
 end
 else
   Result := False;
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const PointA,PointB,PointC:TPoint3D):Boolean;
begin
  (*
    This method will return true iff the pointC is collinear
    to points A and B and exists on the segment A->B or B->A
  *)
  Result := IsPointCollinear(PointA.x,PointA.y,PointA.z,PointB.x,PointB.y,PointB.z,PointC.x,PointC.y,PointC.z);
end;
(* End of IsPointCollinear *)


function IsPointCollinear(const Segment:TSegment3D; const PointC:TPoint3D):Boolean;
begin
  Result := IsPointCollinear(Segment[1],Segment[2],PointC);
end;
(* End of IsPointCollinear *)


function IsPointOnRightSide(const Px,Py,x1,y1,x2,y2:TFloat):Boolean;
begin
  Result := (((x2 - x1) * (py - y1)) < ((px - x1) * (y2 - y1)));
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const x,y:TFloat; const Segment:TSegment2D):Boolean;
begin
  Result := IsPointOnRightSide(x,y,Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const Point:TPoint2D; const Segment:TSegment2D):Boolean;
begin
  Result := IsPointOnRightSide(Point.x,Point.y,Segment);
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const x,y:TFloat; const Line:TLine2D):Boolean;
begin
  Result := IsPointOnRightSide(x,y,Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of IsPointOnRightSide *)


function IsPointOnRightSide(const Point:TPoint2D; const Line:TLine2D):Boolean;
begin
  Result := IsPointOnRightSide(Point.x,Point.y,Line);
end;
(* End of IsPointOnRightSide *)


function IsPointOnLeftSide(const Px,Py,x1,y1,x2,y2:TFloat):Boolean;
begin
  Result := ((x2 - x1) * (py - y1) > (px - x1) * (y2 - y1));
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const x,y:TFloat; const Segment:TSegment2D):Boolean;
begin
  Result := IsPointOnLeftSide(x,y,Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const Point:TPoint2D; const Segment:TSegment2D):Boolean;
begin
  Result := IsPointOnLeftSide(Point.x,Point.y,Segment);
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const x,y:TFloat; const Line:TLine2D):Boolean;
begin
  Result := IsPointOnLeftSide(x,y,Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of IsPointOnLeftSide *)


function IsPointOnLeftSide(const Point:TPoint2D; const Line:TLine2D):Boolean;
begin
  Result := IsPointOnLeftSide(Point.x,Point.y,Line);
end;
(* End of IsPointOnLeftSide *)


function RotationCCW(const x1,y1,x2,y2,Px,Py : TFloat):Integer;
begin
  if ((px - x1) * (y2 - y1)) > ((x2 - x1) * (py - y1)) then
    Result := 1
  else
    Result := -1;
end;
(* End of RotationCCW *)


function RotationCW(const x1,y1,x2,y2,Px,Py : TFloat):Integer;
begin
  if ((x2 - x1) * (py - y1)) > ((px - x1) * (y2 - y1)) then
    Result := +1
  else
    Result := -1;
end;
(* End of RotationCW *)


function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
var
  UpperX : TFloat;
  UpperY : TFloat;
  LowerX : TFloat;
  LowerY : TFloat;
  Ax     : TFloat;
  Bx     : TFloat;
  Cx     : TFloat;
  Ay     : TFloat;
  By     : TFloat;
  Cy     : TFloat;
  D      : TFloat;
  F      : TFloat;
  E      : TFloat;
begin
  Result := false;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < 0.0 then
  begin
    LowerX := x2;
    UpperX := x1;
  end
  else
  begin
    UpperX := x2;
    LowerX := x1;
  end;

  if Bx > 0.0 then
  begin
    if (UpperX < x4) or (x3 < LowerX) then
     Exit;
  end
  else if (Upperx < x3) or (x4 < LowerX) then
    Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < 0.0 then
  begin
    LowerY := y2;
    UpperY := y1;
  end
  else
  begin
    UpperY := y2;
    LowerY := y1;
  end;

  if By > 0.0 then
  begin
    if (UpperY < y4) or (y3 < LowerY) then
      Exit;
  end
  else if (UpperY < y3) or (y4 < LowerY) then
    Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d  := (By * Cx) - (Bx * Cy);
  f  := (Ay * Bx) - (Ax * By);

  if f > 0.0 then
  begin
    if (d < 0.0) or (d > f) then
     Exit;
  end
  else if (d > 0.0) or  (d < f) then
    Exit;

  e := (Ax * Cy) - (Ay * Cx);

  if f > 0.0 then
  begin
    if (e < 0.0) or (e > f) then
      Exit;
  end
  else if(e > 0.0) or (e < f) then
    Exit;

  Result := true;

  (*

  Simple method, yet not so accurate for certain situations and a little more
  inefficient (roughly 19.5%).
  Result := (
             ((Orientation(x1,y1, x2,y2, x3,y3) * Orientation(x1,y1, x2,y2, x4,y4)) <= 0) and
             ((Orientation(x3,y3, x4,y4, x1,y1) * Orientation(x3,y3, x4,y4, x2,y2)) <= 0)
            );
  *)

end;
(* End of SegmentIntersect *)


function Intersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; out ix,iy:TFloat):Boolean;
var
  UpperX    : TFloat;
  UpperY    : TFloat;
  LowerX    : TFloat;
  LowerY    : TFloat;
  Ax        : TFloat;
  Bx        : TFloat;
  Cx        : TFloat;
  Ay        : TFloat;
  By        : TFloat;
  Cy        : TFloat;
  D         : TFloat;
  F         : TFloat;
  E         : TFloat;
  Ratio     : TFloat;
begin
  Result := false;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < 0.0 then
  begin
    LowerX := x2;
    UpperX := x1;
  end
  else
  begin
    UpperX := x2;
    LowerX := x1;
  end;

  if Bx > 0.0 then
  begin
    if (UpperX < x4) or (x3 < LowerX) then
      Exit;
  end
  else if (Upperx < x3) or (x4 < LowerX) then
    Exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < 0.0 then
  begin
    LowerY := y2;
    UpperY := y1;
  end
  else
  begin
    UpperY := y2;
    LowerY := y1;
  end;

  if By > 0.0 then
  begin
  if (UpperY < y4) or (y3 < LowerY) then
    Exit;
  end
  else if (UpperY < y3) or (y4 < LowerY) then
    Exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d  := (By * Cx) - (Bx * Cy);
  f  := (Ay * Bx) - (Ax * By);

  if f > 0.0 then
  begin
    if (d < 0.0) or (d > f) then
      Exit;
  end
  else if (d > 0.0) or  (d < f) then
    Exit;

  e := (Ax * Cy) - (Ay * Cx);

  if f > 0.0 then
  begin
    if (e < 0.0) or (e > f) then
      Exit;
  end
  else if (e > 0.0) or (e < f) then
    Exit;

  Result := true;

  (*

    From IntersectionPoint Routine

    dx1 := x2 - x1; ->  Ax
    dx2 := x4 - x3; -> -Bx
    dx3 := x1 - x3; ->  Cx

    dy1 := y2 - y1; ->  Ay
    dy2 := y1 - y3; ->  Cy
    dy3 := y4 - y3; -> -By

  *)

  Ratio := (Ax * -By) - (Ay * -Bx);

  if NotEqual(Ratio,0.0) then
  begin
    Ratio := ((Cy * -Bx) - (Cx * -By)) / Ratio;
    ix    := x1 + (Ratio * Ax);
    iy    := y1 + (Ratio * Ay);
  end
  else
  begin
    //if Collinear(x1,y1,x2,y2,x3,y3) then
    if IsEqual((Ax * -Cy),(-Cx * Ay)) then
    begin
      ix := x3;
      iy := y3;
    end
    else
    begin
      ix := x4;
      iy := y4;
    end;
  end;
end;
(* End of SegmentIntersect *)


function Intersect(const Point1,Point2,Point3,Point4:TPoint2D):Boolean;
begin
  Result := Intersect(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y);
end;
(* End of Intersect *)


function Intersect(const Segment1,Segment2:TSegment2D):Boolean;
begin
  Result := Intersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2]);
end;
(* End of Intersect *)


function Intersect(const Segment1,Segment2:TSegment2D; out ix, iy : TFloat):Boolean;
begin
  Result := Intersect(Segment1[1].x,Segment1[1].y,Segment1[2].x,Segment1[2].y,Segment2[1].x,Segment2[1].y,Segment2[2].x,Segment2[2].y,ix,iy);
end;
(* End of Intersect *)


function Intersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const fuzzy : TFloat = 0.0):Boolean;
begin
  Result := (IsEqual(LayDistanceSegmentToSegment(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4),fuzzy));
end;
(* End of Intersect *)


function Intersect(const P1,P2,P3,P4:TPoint3D; const fuzzy:TFloat = 0.0):Boolean;
begin
  Result := Intersect(P1.x,P1.y,P1.z,P2.x,P2.y,P2.z,P3.x,P3.y,P3.z,P4.x,P4.y,P4.z,fuzzy);
end;
(* End of Intersect *)


function Intersect(const Segment1,Segment2:TSegment3D; const fuzzy:TFloat = 0.0):Boolean;
begin
  Result := Intersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2],fuzzy);
end;
(* End of Intersect *)


function Intersect(const Segment:TSegment2D; const Rectangle:TRectangle):Boolean;
begin
  Result := RectangleToRectangleIntersect(Rectangle,EquateRectangle(Segment[1],Segment[2]));
end;
(* End of Intersect *)


function Intersect(const Segment:TSegment2D; const Triangle:TTriangle2D):Boolean;
begin
  Result := Intersect(Segment,TriangleEdge(Triangle,1)) or
            Intersect(Segment,TriangleEdge(Triangle,2)) or
            Intersect(Segment,TriangleEdge(Triangle,3)) or
            PointInTriangle(Segment[1],Triangle)        or
            PointInTriangle(Segment[2],Triangle);
end;
(* End of Intersect *)


function Intersect(const Segment:TSegment2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := Intersect(Segment,EquateTriangle(Quadix[1],Quadix[2],Quadix[3])) or
            Intersect(Segment,EquateTriangle(Quadix[1],Quadix[3],Quadix[4]));
end;
(* End of Intersect *)


function Intersect(const Segment:TSegment2D; const Line:TLine2D):Boolean;
begin
  Result := (Orientation(Line,Segment[1]) * Orientation(Line,Segment[2]) <= 0);
end;
(* End of Intersect *)


function Intersect(const Segment:TSegment2D; const Circle:TCircle):Boolean;
var
  Px : TFloat;
  Py : TFloat;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Circle.x,Circle.y,Px,Py);
  Result := (LayDistance(Px,Py,Circle.x,Circle.y) <= (Circle.Radius * Circle.Radius));
end;
(* End of Intersect *)


function Intersect(const Segment:TSegment3D; const Sphere:TSphere):Boolean;
var
  A : TFloat;
  B : TFloat;
  C : TFloat;
begin
  A := LayDistance(Segment);
  B := 2 * ((Segment[2].x - Segment[1].x) * (Segment[1].x - Sphere.x) + (Segment[2].y - Segment[1].y) * (Segment[1].y - Sphere.y) + (Segment[2].z - Segment[1].z) * (Segment[1].z  - Sphere.z));
  C := Sqr(Sphere.x) + Sqr(Sphere.y) + Sqr(Sphere.z) + Sqr(Segment[1].x) + Sqr(Segment[1].y) + Sqr(Segment[1].z) - 2 * (Sphere.x * Segment[1].x + Sphere.y * Segment[1].y + Sphere.z * Segment[1].z) - Sqr(Sphere.Radius);
  //Result:=((B * B - 4 * A * C) >= 0)
  Result := GreaterThanOrEqual((B * B - 4 * A * C),0.0)
end;
(* End of Intersect *)


function Intersect(const Line:TLine2D; const Triangle:TTriangle2D):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
begin
  Result := True;

  Or1 := Orientation (Line[1], Line[2], Triangle[1]);
  if Or1 = 0 then Exit;
  Or2 := Orientation (Line[1], Line[2], Triangle[2]);

  if (Or2 <> Or1) then Exit;
  Or2 := Orientation (Line[1], Line[2], Triangle[3]);

  Result := Or2 <> Or1;
end;
(* End of Intersect *)


function Intersect(const Line:TLine2D; const Quadix:TQuadix2D):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
begin
  Result := True;

  Or1 := Orientation (Line[1], Line[2], Quadix[1]);
  if Or1 = 0 then Exit;
  Or2 := Orientation (Line[1], Line[2], Quadix[2]);

  if (Or2 <> Or1) then Exit;
  Or2 := Orientation (Line[1], Line[2], Quadix[3]);

  if (Or2 <> Or1) then Exit;
  Or2 := Orientation (Line[1], Line[2], Quadix[4]);

  Result := Or2 <> Or1;
end;
(* End of Intersect *)


function Intersect(const Line:TLine2D; const Circle:TCircle):Boolean;
var
  x1 : TFloat;
  y1 : TFloat;
  x2 : TFloat;
  y2 : TFloat;
begin
  (*
    It is assumed that an intersection of a circle by a line
    is either a full intersection (2 points), partial intersection
    (1 point), or tangential.
    Anything else will Result in a false output.
  *)
  x1 := Line[1].x - Circle.x;
  y1 := Line[1].y - Circle.y;
  x2 := Line[2].x - Circle.x;
  y2 := Line[2].y - Circle.y;
  Result := GreaterThanOrEqual(((Circle.Radius * Circle.Radius) * LayDistance(x1,y1,x2,y2) - Sqr(x1 * y2 - x2 * y1)),0.0);
end;
(* End of Intersect *)


function Intersect(const Line:TLine3D; const Triangle:TTriangle3D; out IPoint: TPoint3D):Boolean;
var
  u   : TVector3D;
  v   : TVector3D;
  n   : TVector3D;
  dir : TVector3D;
  w0  : TVector3D;
  w   : TVector3D;
  a   : TFloat;
  b   : TFloat;
  r   : TFloat;
  uu  : TFloat;
  uv  : TFloat;
  vv  : TFloat;
  wu  : TFloat;
  wv  : TFloat;
  d   : TFloat;
  s   : TFloat;
  t   : TFloat;
begin
  Result := False;

  u.x := Triangle[2].x  - Triangle[1].x;
  u.y := Triangle[2].y  - Triangle[1].y;
  u.z := Triangle[2].z  - Triangle[1].z;

  v.x := Triangle[3].x  - Triangle[1].x;
  v.y := Triangle[3].y  - Triangle[1].y;
  v.z := Triangle[3].z  - Triangle[1].z;

  n   := Mul(u,v);

  if IsEqual(DotProduct(u,n),0.0) then
  begin
   (*
      The Triangle is degenerate, ie: vertices are all
      collinear to each other and/or not unique.
   *)
   Exit;
  end;

  dir.x := Line[2].x - Line[1].x;
  dir.y := Line[2].y - Line[1].y;
  dir.z := Line[2].z - Line[1].z;

  w0.x  := Line[1].x - Triangle[1].x;
  w0.y  := Line[1].y - Triangle[1].y;
  w0.z  := Line[1].z - Triangle[1].z;

  a := dotProduct(n,w0) * -1.0;
  b := dotProduct(n,dir);

  if IsEqual(Abs(b),0.0) then
  begin
    Exit;
    (*
       A further test can be done to determine if the
       ray is coplanar to the Triangle.
       In any case the test for unique point intersection
       has failed.
       if IsEqual(a,0.0) then ray is coplanar to Triangle
    *)
  end;

  r := a / b;

  if IsEqual(r,0.0) then
  begin
    Exit;
  end;

  IPoint.x := Line[1].x + (r * dir.x);
  IPoint.y := Line[1].y + (r * dir.y);
  IPoint.z := Line[1].z + (r * dir.z);

  w.x := IPoint.x - Triangle[1].x;
  w.y := IPoint.y - Triangle[1].y;
  w.z := IPoint.z - Triangle[1].z;

  uu := dotProduct(u,u);
  uv := dotProduct(u,v);
  vv := dotProduct(v,v);
  wu := dotProduct(w,u);
  wv := dotProduct(w,v);

  d  := uv * uv - uu * vv;

  // get and test parametric coords
  s := ((uv * wv) - (vv * wu)) / d;

  if (s < 0.0) or (s > 1.0) then
  begin
    (* Intersection is outside of Triangle *)
    Exit;
  end;

  t := ((uv * wu) - (uu * wv)) / d;

  if (t < 0.0) or ((s + t) > 1.0) then
  begin
    (* Intersection is outside of Triangle *)
    Exit;
  end;

  Result := True;

end;
(* End of Intersect *)


function Intersect(const Triangle:TTriangle2D; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(ClosestPointOnTriangleFromPoint(Triangle,Circle.x,Circle.y),Circle);
end;
(* End of Intersect *)


function Intersect(const Triangle:TTriangle2D; const Rectangle : TRectangle):Boolean;
begin
  Result := Intersect(EquateSegment(Triangle[1],Triangle[2]),Rectangle) or
            Intersect(EquateSegment(Triangle[2],Triangle[3]),Rectangle) or
            Intersect(EquateSegment(Triangle[3],Triangle[1]),Rectangle);
end;
(* End of Intersect *)


function Intersect(const Rectangle1,Rectangle2:TRectangle):Boolean;
begin
  Result := RectangleToRectangleIntersect(Rectangle1,Rectangle2);
end;
(* End of Intersect *)


function Intersect(const Triangle1,Triangle2:TTriangle2D):Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 1 to 3 do
  begin
    if IsEqual(MinimumDistanceFromPointToTriangle(Triangle1[i],Triangle2),0.0) or
       IsEqual(MinimumDistanceFromPointToTriangle(Triangle2[i],Triangle1),0.0) then
    begin
      Result := True;
      Exit;
    end;
  end
end;
(* End of Intersect *)


function Intersect(const Rectangle:TRectangle; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(ClosestPointOnRectangleFromPoint(Rectangle,Circle.x,Circle.y),Circle);
end;
(* End of Intersect *)


function Intersect(const Circle1,Circle2:TCircle):Boolean;
begin
  Result := (LayDistance(Circle1.x,Circle1.y,Circle2.x,Circle2.y) <= ((Circle1.Radius + Circle2.Radius) * (Circle1.Radius + Circle2.Radius)));
end;
(* End of Intersect *)


function Intersect(const Sphere1,Sphere2:TSphere):Boolean;
begin
  Result := (LayDistance(Sphere1.x,Sphere1.y,Sphere1.z,Sphere2.x,Sphere2.y,Sphere2.z) <= ((Sphere1.Radius + Sphere2.Radius) * (Sphere1.Radius + Sphere2.Radius)));
end;
(* End of Intersect *)


function Intersect(const Poly1,Poly2: TPolygon2D):Boolean;
var
  I            : Integer;
  J            : Integer;
  Poly1Trailer : Integer;
  Poly2Trailer : Integer;
begin
  Result := False;
  if (Length(Poly1) < 3) or (Length(Poly2) < 3) then exit;
  Poly1Trailer := Length(Poly1) - 1;
  for i := 0 to Length(Poly1) - 1 do
  begin
    Poly2Trailer := Length(Poly2) - 1;
    for j := 0 to Length(Poly2) - 1 do
    begin
      if Intersect(Poly1[i],Poly1[Poly1Trailer],Poly2[j],Poly2[Poly2Trailer]) then
      begin
        Result := True;
        Exit;
      end;
      Poly2Trailer := j;
    end;
    Poly1Trailer := i;
  end;
end;
(* End of Intersect *)


function Intersect(const Obj1,Obj2:TGeometricObject):Boolean;
begin
  Result := False;
  if Obj1.ObjectType = geoSegment2D then
  begin
    case Obj2.ObjectType of
      geoSegment2D : Result := Intersect(Obj1.Segment2D, Obj2.Segment2D );
      geoRectangle : Result := Intersect(Obj1.Segment2D, Obj2.Rectangle );
      geoTriangle2D: Result := Intersect(Obj1.Segment2D, Obj2.Triangle2D);
      geoQuadix2D  : Result := Intersect(Obj1.Segment2D, Obj2.Quadix2D  );
      geoCircle    : Result := Intersect(Obj1.Segment2D, Obj2.Circle    );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoSegment2D then
  begin
    case Obj1.ObjectType of
      geoSegment2D : Result := Intersect(Obj2.Segment2D, Obj1.Segment2D );
      geoRectangle : Result := Intersect(Obj2.Segment2D, Obj1.Rectangle );
      geoTriangle2D: Result := Intersect(Obj2.Segment2D, Obj1.Triangle2D);
      geoQuadix2D  : Result := Intersect(Obj2.Segment2D, Obj1.Quadix2D  );
      geoCircle    : Result := Intersect(Obj2.Segment2D, Obj1.Circle    );
    end;
    Exit;
  end
  else if Obj1.ObjectType = geoSegment3D then
  begin
    case Obj2.ObjectType of
      geoSegment3D : Result := Intersect(Obj1.Segment3D, Obj2.Segment3D);
      geoSphere    : Result := Intersect(Obj1.Segment3D, Obj2.Sphere   );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoSegment3D then
  begin
    case Obj1.ObjectType of
      geoSegment3D : Result := Intersect(Obj2.Segment3D, Obj1.Segment3D);
      geoSphere    : Result := Intersect(Obj2.Segment3D, Obj1.Sphere   );
    end;
    Exit;
  end
  else if Obj1.ObjectType = geoTriangle2D then
  begin
    case Obj2.ObjectType of
      geoRectangle : Result := Intersect(Obj1.Triangle2D, Obj2.Rectangle );
      geoTriangle2D: Result := Intersect(Obj1.Triangle2D, Obj2.Triangle2D);
      geoCircle    : Result := Intersect(Obj1.Triangle2D, Obj2.Circle    );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoTriangle2D then
  begin
    case Obj1.ObjectType of
      geoRectangle : Result := Intersect(Obj2.Triangle2D, Obj1.Rectangle );
      geoTriangle2D: Result := Intersect(Obj2.Triangle2D, Obj1.Triangle2D);
      geoCircle    : Result := Intersect(Obj2.Triangle2D, Obj1.Circle    );
    end;
    Exit;
  end
  else if Obj1.ObjectType = geoRectangle then
  begin
    case Obj2.ObjectType of
      geoRectangle : Result := Intersect(Obj1.Rectangle,  Obj2.Rectangle);
      geoTriangle2D: Result := Intersect(Obj2.Triangle2D, Obj1.Rectangle);
      geoCircle    : Result := Intersect(Obj1.Rectangle,  Obj2.Circle   );
    end;
    Exit;
  end
  else if Obj2.ObjectType = geoRectangle then
  begin
    case Obj1.ObjectType of
      geoRectangle : Result := Intersect(Obj2.Rectangle,  Obj1.Rectangle);
      geoTriangle2D: Result := Intersect(Obj1.Triangle2D, Obj2.Rectangle);
      geoCircle    : Result := Intersect(Obj2.Rectangle,  Obj1.Circle   );
    end;
    Exit;
  end
  else if (Obj1.ObjectType = geoLine2D) and (Obj2.ObjectType = geoCircle) then
    Result := Intersect(Obj1.Line2D,Obj2.Circle)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoLine2D) then
    Result := Intersect(Obj2.Line2D,Obj1.Circle)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Intersect(Obj1.Circle,Obj2.Circle)
end;
(* End of Intersect *)


function SimpleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
begin
  Result := (
             ((Orientation(x1,y1,x2,y2,x3,y3) * Orientation(x1,y1,x2,y2,x4,y4)) <= 0) and
             ((Orientation(x3,y3,x4,y4,x1,y1) * Orientation(x3,y3,x4,y4,x2,y2)) <= 0)
            );
end;
(* End of SimpleIntersect *)


function SimpleIntersect(const Point1,Point2,Point3,Point4:TPoint2D):Boolean;
begin
  Result := SimpleIntersect(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y);
end;
(* End of SimpleIntersect *)


function SimpleIntersect(const Segment1,Segment2:TSegment2D):Boolean;
begin
  Result := Simpleintersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2]);
end;
(* End of SimpleIntersect *)


function ThickSegmentIntersect(const x1,y1,x2,y2,x3,y3,x4,y4,Thickness:TFloat):Boolean;
begin
  Result := LessThanOrEqual(Thickness * Thickness, LayDistanceSegmentToSegment(x1,y1,x2,y2,x3,y3,x4,y4));
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TPoint2D; const Thickness:TFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Thickness);
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Segment1,Segment2:TSegment2D; const Thickness:TFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Thickness);
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,Thickness:TFloat):Boolean;
begin
  Result := LessThanOrEqual(Thickness * Thickness, LayDistanceSegmentToSegment(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4));
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Point1,Point2,Point3,Point4:TPoint3D; const Thickness:TFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z,Thickness);
end;
(* End of ThickSegmentIntersect *)


function ThickSegmentIntersect(const Segment1,Segment2:TSegment3D; const Thickness:TFloat):Boolean;
begin
  Result := ThickSegmentIntersect(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Thickness);
end;
(* End of ThickSegmentIntersect *)


procedure IntersectionPoint(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; out Nx,Ny:TFloat);
var
  Ratio : TFloat;
  dx1   : TFloat;
  dx2   : TFloat;
  dx3   : TFloat;
  dy1   : TFloat;
  dy2   : TFloat;
  dy3   : TFloat;
begin
  dx1 := x2 - x1;
  dx2 := x4 - x3;
  dx3 := x1 - x3;

  dy1 := y2 - y1;
  dy2 := y1 - y3;
  dy3 := y4 - y3;

  Ratio := dx1 * dy3 - dy1 * dx2;

  if NotEqual(Ratio,0.0) then
  begin
    Ratio := (dy2 * dx2 - dx3 * dy3) / Ratio;
    Nx    := x1 + Ratio * dx1;
    Ny    := y1 + Ratio * dy1;
  end
  else
  begin
    //if Collinear(x1,y1,x2,y2,x3,y3) then
    if IsEqual((dx1 * -dy2),(-dx3 * dy1)) then
    begin
      Nx := x3;
      Ny := y3;
    end
    else
    begin
      Nx := x4;
      Ny := y4;
    end;
  end;
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const P1,P2,P3,P4:TPoint2D; out Nx,Ny:TFloat);
begin
  IntersectionPoint(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,P4.x,P4.y,Nx,Ny);
end;
(* End of IntersectionPoint *)


function IntersectionPoint(const P1,P2,P3,P4:TPoint2D):TPoint2D;
begin
  IntersectionPoint(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,P4.x,P4.y,Result.x,Result.y);
end;
(* End of IntersectionPoint *)


function IntersectionPoint(const Segment1,Segment2:TSegment2D):TPoint2D;
begin
  Result := IntersectionPoint(Segment1[1],Segment1[2],Segment2[1],Segment2[2]);
end;
(* End of IntersectionPoint *)


function IntersectionPoint(const Line1,Line2:TLine2D):TPoint2D;
var
  dx1   : TFloat;
  dx2   : TFloat;
  dx3   : TFloat;
  dy1   : TFloat;
  dy2   : TFloat;
  dy3   : TFloat;
  det   : TFloat;
  ratio : TFloat;
begin
  dx1 := Line1[1].x - Line1[2].x;
  dx2 := Line2[1].x - Line2[2].x;
  dx3 := Line2[2].x - Line1[2].x;
  dy1 := Line1[1].y - Line1[2].y;
  dy2 := Line2[1].y - Line2[2].y;
  dy3 := Line2[2].y - Line1[2].y;

  det := (dx2 * dy1) - (dy2 * dx1);

  if IsEqual(det,0.0) then
  begin
    if IsEqual((dx2 * dy3),(dy2 * dx3)) then
    begin
      Result.x := Line2[1].x;
      Result.y := Line2[1].y;
      Exit;
    end
  else
    Exit;
  end;

  ratio := ((dx1 * dy3) - (dy1 * dx3)) / det;

  Result.x := (ratio * dx2) + Line2[2].x;
  Result.y := (ratio * dy2) + Line2[2].y;

end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const Circle1,Circle2:TCircle; out Point1,Point2:TPoint2D);
var
  Dist   : TFloat;
  A      : TFloat;
  H      : TFloat;
  RatioA : TFloat;
  RatioH : TFloat;
  Dx     : TFloat;
  Dy     : TFloat;
  Phi    : TPoint2D;
  r1Sqr  : TFloat;
  r2Sqr  : TFloat;
  dstSqr : TFloat;
begin
  Dist   := Distance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);

  dstSqr := Dist * Dist;
  r1Sqr  := Circle1.Radius * Circle1.Radius;
  r2Sqr  := Circle2.Radius * Circle2.Radius;

  A      := (dstSqr - r2sqr + r1sqr) / (2 * Dist);
  H      := Sqrt(r1sqr - (A*A));

  RatioA := A / Dist;
  RatioH := H / Dist;

  Dx     := Circle2.x - Circle1.x;
  Dy     := Circle2.y - Circle1.y;

  Phi.x  := Circle1.x + (RatioA * Dx);
  Phi.y  := Circle1.y + (RatioA * Dy);

  Dx     := Dx * RatioH;
  Dy     := Dy * RatioH;

  Point1.x := Phi.x + Dy;
  Point1.y := Phi.y - Dx;

  Point2.x := Phi.x - Dy;
  Point2.y := Phi.y + Dx;
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const Segment:TSegment2D; const Triangle:TTriangle2D; out ICnt:Integer; out I1,I2:TPoint2D);
var
  Ix : TFloat;
  Iy : TFloat;
begin
  ICnt := 0;
  if Intersect(Segment,TriangleEdge(Triangle,1),Ix,Iy) then
  begin
    I1.x := Ix;
    I1.y := Iy;
    Inc(ICnt);
  end;
  if Intersect(Segment,TriangleEdge(Triangle,2),Ix,Iy) then
  begin
    if Icnt = 1 then
    begin
      I2.x := Ix;
      I2.y := Iy;
      Inc(ICnt);
      Exit;
    end;
    I1.x := Ix;
    I1.y := Iy;
    Inc(ICnt);
  end;
  if Intersect(Segment,TriangleEdge(Triangle,3),Ix,Iy) then
  begin
    if Icnt = 1 then
    begin
      I2.x := Ix;
      I2.y := Iy;
      Inc(ICnt);
      Exit;
    end;
    I1.x := Ix;
    I1.y := Iy;
    Inc(ICnt);
  end;
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const Line:TLine3D; const Triangle:TTriangle3D; out IPoint: TPoint3D);
var
  u   : TVector3D;
  v   : TVector3D;
  n   : TVector3D;
  dir : TVector3D;
  w0  : TVector3D;
  a   : TFloat;
  b   : TFloat;
  r   : TFloat;
begin
  u.x := Triangle[2].x  - Triangle[1].x;
  u.y := Triangle[2].y  - Triangle[1].y;
  u.z := Triangle[2].z  - Triangle[1].z;

  v.x := Triangle[3].x  - Triangle[1].x;
  v.y := Triangle[3].y  - Triangle[1].y;
  v.z := Triangle[3].z  - Triangle[1].z;

  n   := Mul(u,v);

  dir.x := Line[2].x - Line[1].x;
  dir.y := Line[2].y - Line[1].y;
  dir.z := Line[2].z - Line[1].z;

  w0.x  := Line[1].x - Triangle[1].x;
  w0.y  := Line[1].y - Triangle[1].y;
  w0.z  := Line[1].z - Triangle[1].z;

  a := dotProduct(n,w0) * -1.0;
  b := dotProduct(n,dir);
  r := a / b;

  IPoint.x := Line[1].x + (r * dir.x);
  IPoint.y := Line[1].y + (r * dir.y);
  IPoint.z := Line[1].z + (r * dir.z);
end;
(* End of IntersectionPoint *)


procedure IntersectionPoint(const x1,y1,x2,y2,Cx,Cy,Radius:TFloat; out ICnt:Integer; out Ix1,Iy1,Ix2,Iy2:TFloat);
var
  Px   : TFloat;
  Py   : TFloat;
  S1In : Boolean;
  s2In : Boolean;
  h    : TFloat;
  a    : TFloat;
begin
  ICnt := 0;

  S1In := PointInCircle(x1,y1,Cx,Cy,Radius);
  S2In := PointInCircle(x2,y2,Cx,Cy,Radius);

  if S1In and S2In then
  begin
    ICnt := 2;
    Ix1  := x1;
    Iy1  := y1;
    Ix2  := x2;
    Iy2  := y2;
    Exit;
  end;

  if S1In Or S2In then
  begin
    ICnt := 2;
    ClosestPointOnLineFromPoint(x1,y1,x2,y2,Cx,Cy,Px,Py);
    if S1In then
    begin
      h   := Distance(Px,Py,Cx,Cy);
      a   := sqrt((Radius * Radius) - (h * h));
      Ix1 := x1;
      Iy1 := y1;
      ProjectPoint(Px,Py,x2,y2,a,Ix2,Iy2);
    end
    else if S2In then
    begin
      h   := Distance(Px,Py,Cx,Cy);
      a   := sqrt((Radius * Radius) - (h * h));
      Ix1 := x2;
      Iy1 := y2;
      ProjectPoint(Px,Py,x1,y1,a,Ix2,Iy2);
    end;
    Exit;
  end;

  ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Cx,Cy,Px,Py);

  if (IsEqual(x1,Px) and IsEqual(y1,Py)) or
     (IsEqual(x2,Px) and IsEqual(y2,Py)) then
    exit
  else
  begin
    h := Distance(Px,Py,Cx,Cy);
    if h > Radius then
       Exit
    else if IsEqual(h,Radius) then
    begin
      ICnt := 1;
      Ix1  := Px;
      Iy1  := Py;
      Exit;
    end
    else if IsEqual(h,0.0) then
    begin
      ICnt := 2;
      ProjectPoint(Cx,Cy,x1,y1,Radius,Ix1,Iy1);
      ProjectPoint(Cx,Cy,x2,y2,Radius,Ix2,Iy2);
      Exit;
    end
    else
    begin
      ICnt := 2;
      a    := sqrt((Radius * Radius) - (h * h));
      ProjectPoint(Px,Py,x1,y1,a,Ix1,Iy1);
      ProjectPoint(Px,py,x2,y2,a,Ix2,Iy2);
      Exit;
    end;
  end;
end;
(* End Of IntersectionPoint *)


procedure IntersectionPoint(const Segment:TSegment2D; const Circle:TCircle; out ICnt:Integer; out I1,I2:TPoint2D);
begin
  IntersectionPoint(
                    Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,
                    Circle.x,Circle.y,Circle.Radius,
                    ICnt,I1.x,I1.y,I2.x,I2.y
                   );
end;
(* End of IntersectionPoint *)


function NormalizeAngle(const Angle : TFloat) : TFloat;
begin
  Result := Angle;
  if Result > 360.0 then
    Result := Result - (Trunc(Result / 360.0) * 360.0)
  else if Result < 0 then
  begin
    while Result < 0.0 Do Result := Result + 360.0;
  end;
end;
(* Normalize Angle *)


function VerticalMirror(const Angle : TFloat) : TFloat;
begin
  Result := Angle;
  if IsEqual(Angle,  0.0) or
     IsEqual(Angle,180.0) or
     IsEqual(Angle,360.0) then Exit;
  Result := 360 - Result;
end;
(* Vertical Mirror *)


function HorizontalMirror(const Angle : TFloat) : TFloat;
begin
  Result := Angle;
  if Result <= 180 then
    Result := 180 - Result
  else
    Result := 540 - Result;
end;
(* Vertical Mirror *)


function Quadrant(const Angle : TFloat  ):Integer;
begin
    Result := 0;
         if (Angle >=   0) and (Angle <  90) then Result := 1
    else if (Angle >=  90) and (Angle < 180) then Result := 2
    else if (Angle >= 180) and (Angle < 270) then Result := 3
    else if (Angle >= 270) and (Angle < 360) then Result := 4
    else if Angle = 360                      then Result := 1;
end;
(* End of Quadrant *)


function Quadrant(const x,y:TFloat):Integer;
begin
  if      (x >  0.0) and (y >= 0.0) then Result := 1
  else if (x <= 0.0) and (y >  0.0) then Result := 2
  else if (x <  0.0) and (y <= 0.0) then Result := 3
  else if (x >= 0.0) and (y <  0.0) then Result := 4
  else
    Result := 0;
end;
(* End of Quadrant *)


function Quadrant(const Point : TPoint2D):Integer;
begin
  Result := Quadrant(Point.x,Point.y);
end;
(* End of Quadrant *)


function VertexAngle(x1,y1,x2,y2,x3,y3:TFloat):TFloat;
var
  Dist      : TFloat;
  InputTerm : TFloat;
begin
 (*
    Using the cosine identity:
    cosA = (b^2 + c^2 - a^2) / (2*b*c)
    A    = Cos'((b^2 + c^2 - a^2) / (2*b*c))

    Where:

    a,b and c : are edges in the triangle
    A         : is the angle at the vertex opposite edge 'a'
                aka the edge defined by the vertex <x1y1-x2y2-x3y3>

 *)
  (* Quantify coordinates *)
  x1   := x1 - x2;
  x3   := x3 - x2;
  y1   := y1 - y2;
  y3   := y3 - y2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1) * (x3 * x3 + y3 * y3);

  if IsEqual(Dist,0.0) then
    Result := 0.0
  else
  begin
    InputTerm := (x1 * x3 + y1 * y3) / sqrt(Dist);
    if IsEqual(InputTerm,1.0) then
      Result := 0.0
    else if IsEqual(InputTerm,-1.0) then
      Result := 180.0
    else
      Result := ArcCos(InputTerm) * _180DivPI
  end;
end;
(* End of Vertex Angle *)


function VertexAngle(const Point1,Point2,Point3:TPoint2D):TFloat;
begin
  Result := VertexAngle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Vertex Angle *)


function VertexAngle(x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):TFloat;
var
  Dist : TFloat;
begin
 (*
    Method is the same as the one described in
    the above routine.
 *)

  (* Quantify coordinates *)
  x1 := x1 - x2;
  x3 := x3 - x2;
  y1 := y1 - y2;
  y3 := y3 - y2;
  z1 := z1 - z2;
  z3 := z3 - z2;

  (* Calculate Ley Distance *)
  Dist := (x1 * x1 + y1 * y1 + z1 * z1) * (x3 * x3 + y3 * y3 + z3 * z3);

  if IsEqual(Dist,0.0) then
    Result := 0.0
  else
    Result := ArcCos((x1 * x3+ y1 * y3 + z1 * z3) / sqrt(Dist)) * _180DivPI;
end;
(* End of Vertex Angle *)


function VertexAngle(const Point1,Point2,Point3:TPoint3D):TFloat;
begin
  Result := VertexAngle(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of Vertex Angle *)


function OrientedVertexAngle(const x1,y1,x2,y2,x3,y3:TFloat; const Orient : Integer = Clockwise):TFloat;
begin
  Result := VertexAngle(x1,y1,x2,y2,x3,y3);
  if Orientation(x1,y1,x2,y2,x3,y3) <> Orient then
    Result := 360.0 - Result;
end;
(* End of Oriented Vertex Angle *)


function OrientedVertexAngle(const Point1,Point2,Point3:TPoint2D; const Orient : Integer = Clockwise):TFloat;
begin
  Result := OrientedVertexAngle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Orient);
end;
(* End of Oriented Vertex Angle *)


function CartesianAngle(const x,y:TFloat):TFloat;
begin
  if      (x >  0.0) and (y >  0.0) then Result := (ArcTan( y / x) * _180DivPI)
  else if (x <  0.0) and (y >  0.0) then Result := (ArcTan(-x / y) * _180DivPI) +  90.0
  else if (x <  0.0) and (y <  0.0) then Result := (ArcTan( y / x) * _180DivPI) + 180.0
  else if (x >  0.0) and (y <  0.0) then Result := (ArcTan(-x / y) * _180DivPI) + 270.0
  else if (x  = 0.0) and (y >  0.0) then Result :=  90.0
  else if (x <  0.0) and (y =  0.0) then Result := 180.0
  else if (x  = 0.0) and (y <  0.0) then Result := 270.0
  else
    Result := 0.0;
end;
(* End of Cartesian Angle *)


function CartesianAngle(const Point : TPoint2D):TFloat;
begin
  Result := CartesianAngle(Point.x,Point.y);
end;
(* End of Cartesian Angle *)


function RobustCartesianAngle(const x,y:TFloat; const Epsilon : TFloat = Epsilon_High):TFloat;
begin
  if             (x >  0.0)         and        (y >  0.0)         then Result := (ArcTan( y / x) * _180DivPI)
  else if        (x <  0.0)         and        (y >  0.0)         then Result := (ArcTan(-x / y) * _180DivPI) +  90.0
  else if        (x <  0.0)         and        (y <  0.0)         then Result := (ArcTan( y / x) * _180DivPI) + 180.0
  else if        (x >  0.0)         and        (y <  0.0)         then Result := (ArcTan(-x / y) * _180DivPI) + 270.0
  else if IsEqual(x,   0.0,Epsilon) and        (y >  0.0)         then Result :=  90.0
  else if        (x <  0.0)         and IsEqual(y,   0.0,Epsilon) then Result := 180.0
  else if IsEqual(x,   0.0)         and        (y <  0.0)         then Result := 270.0
  else
    Result := 0.0;
end;
(* End of Robust Cartesian Angle *)


function RobustCartesianAngle(const Point : TPoint2D; const Epsilon : TFloat = Epsilon_High):TFloat;
begin
  Result := RobustCartesianAngle(Point.x,Point.y,Epsilon);
end;
(* End of Robust Cartesian Angle *)


function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TPoint2D):TFloat;
var
  TempPoint : TPoint2D;
begin
  Result := -1;
  if Intersect(Point1,Point2,Point3,Point4) then
  begin
    TempPoint := IntersectionPoint(Point1,Point2,Point3,Point4);
    Result    := VertexAngle(Point1,TempPoint,Point4);
  end;
end;
(* End of SegmentIntersectAngle *)


function SegmentIntersectAngle(const Segment1,Segment2:TSegment2D):TFloat;
var
  TempPoint : TPoint2D;
begin
  Result := -1;
  if Intersect(Segment1,Segment2) then
  begin
    TempPoint := IntersectionPoint(Segment1,Segment2);
    Result    := VertexAngle(Segment1[1],TempPoint,Segment2[1]);
  end;
end;
(* End of SegmentIntersectAngle *)


function SegmentIntersectAngle(const Point1,Point2,Point3,Point4:TPoint3D):TFloat;
begin
 {
  This section can be completed once line intersection in 3D is complete
 }
  Result := 0.0;
end;
(* End of SegmentIntersectAngle *)


function SegmentIntersectAngle(const Segment1,Segment2:TSegment3D):TFloat;
begin
 {
  This section can be completed once line intersection in 3D is complete
 }
  Result := 0.0;
end;
(* End of SegmentIntersectAngle *)


function InPortal(const P:TPoint2D):Boolean;
begin
  Result := PointInRectangle(P,MinimumX,MinimumY,MaximumX,MaximumY);
end;
(* End of InPortal *)


function InPortal(const P:TPoint3D):Boolean;
begin
  Result := LessThanOrEqual(MinimumX,P.x) and LessThanOrEqual(MaximumZ,P.x) and
            LessThanOrEqual(MinimumY,P.y) and LessThanOrEqual(MaximumY,P.y) and
            LessThanOrEqual(MinimumZ,P.y) and LessThanOrEqual(MaximumZ,P.y);
end;
(* End of InPortal *)


function HighestPoint(const Polygon:TPolygon2D):TPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].y > Result.y then
    Result := Polygon[i];
end;
(* End of HighestPoint *)


function HighestPoint(const Point: array of TPoint2D):TPoint2D;
var
  i : Integer;
begin
  if Length(Point) = 0 then Exit;
  Result := Point[0];
  for i := 1 to Length(Point) - 1 do
    if Point[i].y > Result.y then
      Result := Point[i];
end;
(* End of HighestPoint *)


function HighestPoint(const Triangle:TTriangle2D):TPoint2D;
begin
  Result := Triangle[1];
  if Triangle[2].y > Result.y then
    Result := Triangle[2];
  if Triangle[3].y > Result.y then
    Result := Triangle[3];
end;
(* End of HighestPoint *)


function HighestPoint(const Triangle:TTriangle3D):TPoint3D;
begin
  Result := Triangle[1];
  if Triangle[2].y > Result.y then
    Result := Triangle[2];
  if Triangle[3].y > Result.y then
    Result := Triangle[3];
end;
(* End of HighestPoint *)


function HighestPoint(const Quadix:TQuadix2D):TPoint2D;
begin
  Result := Quadix[1];
  if Quadix[2].y > Result.y then
    Result := Quadix[2];
  if Quadix[3].y > Result.y then
    Result := Quadix[3];
  if Quadix[4].y > Result.y then
    Result := Quadix[4];
end;
(* End of HighestPoint *)


function HighestPoint(const Quadix:TQuadix3D):TPoint3D;
begin
  Result := Quadix[1];
  if Quadix[2].y > Result.y then
    Result := Quadix[2];
  if Quadix[3].y > Result.y then
    Result := Quadix[3];
  if Quadix[4].y > Result.y then
    Result := Quadix[4];
end;
(* End of HighestPoint *)


function LowestPoint(const Polygon: TPolygon2D):TPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].y < Result.y then
      Result := Polygon[i];
end;
(* End of LowestPoint *)


function LowestPoint(const Point: array of TPoint2D):TPoint2D;
var
  i : Integer;
begin
  if Length(Point) = 0 then Exit;
  Result := Point[0];
  for i := 1 to length(Point) - 1 do
   if Point[i].y < Result.y then
     Result := Point[i];
end;
(* End of LowestPoint *)


function LowestPoint(const Triangle:TTriangle2D):TPoint2D;
begin
  Result := Triangle[1];
  if Triangle[2].y < Result.y then
     Result := Triangle[2];
  if Triangle[3].y < Result.y then
     Result := Triangle[3];
end;
(* End of LowestPoint *)


function LowestPoint(const Triangle:TTriangle3D):TPoint3D;
begin
  Result := Triangle[1];
  if Triangle[2].y < Result.y then
     Result := Triangle[2];
  if Triangle[3].y < Result.y then
     Result := Triangle[3];
end;
(* End of LowestPoint *)


function LowestPoint(const Quadix:TQuadix2D):TPoint2D;
begin
  Result := Quadix[1];
  if Quadix[2].y < Result.y then
     Result := Quadix[2];
  if Quadix[3].y < Result.y then
     Result := Quadix[3];
  if Quadix[4].y < Result.y then
     Result := Quadix[4];
end;
(* End of LowestPoint *)


function LowestPoint(const Quadix:TQuadix3D):TPoint3D;
begin
  Result := Quadix[1];
  if Quadix[2].y < Result.y then
     Result := Quadix[2];
  if Quadix[3].y < Result.y then
     Result := Quadix[3];
  if Quadix[4].y < Result.y then
     Result := Quadix[4];
end;
(* End of LowestPoint *)


function MostLeftPoint(const Polygon: TPolygon2D):TPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
   if Polygon[i].x < Result.x then
     Result := Polygon[i];
end;
(* End of Most Left Point *)


function MostLeftPoint(const Point: array of TPoint2D):TPoint2D;
begin
  Result := MostLeftPoint(TPolygon2D(@Point));
end;
(* End of Most Left Point *)


function MostRightPoint(const Polygon: TPolygon2D):TPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].x < Result.x then
      Result := Polygon[i];
end;
(* End of Most Right Point *)


function MostRightPoint(const Point: array of TPoint2D):TPoint2D;
begin
  Result := MostRightPoint(TPolygon2D(@Point));
end;
(* End of Most Right Point *)


function MostUpperRight(const Polygon: TPolygon2D):TPoint2D;
var
  i : Integer;
begin
   if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
    if Polygon[i].y > Result.y then
      Result := Polygon[i]
    else if Polygon[i].y = Result.y then
      if Polygon[i].x > Result.x then
        Result := Polygon[i];
end;
(* End of Most Upper Right *)


function MostUpperRight(const Point: array of TPoint2D):TPoint2D;
begin
  Result := MostUpperRight(TPolygon2D(@Point));
end;
(* End of Most Upper Right *)


function MostUpperLeft(const Polygon: TPolygon2D):TPoint2D;
var
  i : Integer;
begin
  if Length(Polygon) = 0 then Exit;
  Result := Polygon[0];
  for i := 1 to Length(Polygon) - 1 do
   if Polygon[i].y > Result.y then
     Result := Polygon[i]
   else if Polygon[i].y = Result.y then
    if Polygon[i].x < Result.x then
      Result := Polygon[i];
end;
(* End of Most Upper Left *)


function MostUpperLeft(const Point: array of TPoint2D):TPoint2D;
begin
 Result := MostUpperLeft(TPolygon2D(@Point));
end;
(* End of Most Upper Left *)


function MostLowerRight(const Polygon: TPolygon2D):TPoint2D;
var
  i : Integer;
begin
 if Length(Polygon) = 0 then Exit;
 Result := Polygon[0];
 for i := 1 to Length(Polygon) - 1 do
  if Polygon[i].y < Result.y then
    Result := Polygon[i]
  else if Polygon[i].y = Result.y then
   if Polygon[i].x > Result.x then
     Result := Polygon[i];
end;
(* End of Most Lower Right *)


function MostLowerRight(const Point: array of TPoint2D):TPoint2D;
begin
 Result := MostLowerRight(TPolygon2D(@Point));
end;
(* End of Most Lower Right *)


function MostLowerLeft(const Polygon: TPolygon2D):TPoint2D;
var
  i : Integer;
begin
 if Length(Polygon) = 0 then Exit;
 Result := Polygon[0];
 for i := 1 to Length(Polygon) - 1 do
  if Polygon[i].y < Result.y then
    Result := Polygon[i]
  else if Polygon[i].y = Result.y then
   if Polygon[i].x < Result.x then
     Result := Polygon[i];
end;
(* End of Most Lower Left *)


function MostLowerLeft(const Point: array of TPoint2D):TPoint2D;
begin
  Result := MostLowerLeft(TPolygon2D(@Point));
end;
(* End of Most Lower Left *)


function Min(const Point1,Point2:TPoint2D):TPoint2D;
begin
       if Point1.x < Point2.x then Result := Point1
  else if Point2.x < Point1.x then Result := Point2
  else if Point1.y < Point2.y then Result := Point1
  else                             Result := Point2;
end;
(* End of Minimum Between 2 Points *)


function Min(const Point1,Point2:TPoint3D):TPoint3D;
begin
       if Point1.x < Point2.x then Result := Point1
  else if Point2.x < Point1.x then Result := Point2
  else if Point1.y < Point2.y then Result := Point1
  else if Point2.y < Point1.y then Result := Point2
  else if Point1.z < Point2.z then Result := Point1
  else                             Result := Point2;
end;
(* End of Minimum Between 2 Points *)


function Max(const Point1,Point2:TPoint2D):TPoint2D;
begin
       if Point1.x > Point2.x then Result := Point1
  else if Point2.x > Point1.x then Result := Point2
  else if Point1.y > Point2.y then Result := Point1
  else                             Result := Point2;
end;
(* End of Maximum Between 2 Points *)


function Max(const Point1,Point2:TPoint3D):TPoint3D;
begin
       if Point1.x > Point2.x then Result := Point1
  else if Point2.x > Point1.x then Result := Point2
  else if Point1.y > Point2.y then Result := Point1
  else if Point2.y > Point1.y then Result := Point2
  else if Point1.z > Point2.z then Result := Point1
  else                             Result := Point2;
end;
(* End of Maximum Between 2 Points *)


function Coincident(const Point1,Point2:TPoint2D):Boolean;
begin
  Result := IsEqual(Point1,Point2);
end;
(* End of Coincident - 2D Points *)


function Coincident(const Point1,Point2:TPoint3D):Boolean;
begin
  Result := IsEqual(Point1,Point2);
end;
(* End of Coincident - 3D Points *)


function Coincident(const Segment1,Segment2:TSegment2D):Boolean;
begin
  Result := (Coincident(Segment1[1],Segment2[1]) and Coincident(Segment1[2],Segment2[2])) or
            (Coincident(Segment1[1],Segment2[2]) and Coincident(Segment1[2],Segment2[1]));
end;
(* End of Coincident - 2D Segments *)


function Coincident(const Segment1,Segment2:TSegment3D):Boolean;
begin
  Result := (Coincident(Segment1[1],Segment2[1]) and  Coincident(Segment1[2],Segment2[2])) or
            (Coincident(Segment1[1],Segment2[2]) and  Coincident(Segment1[2],Segment2[1]));
end;
(* End of Coincident - 3D Segments *)


function Coincident(const Triangle1,Triangle2:TTriangle2D):Boolean;
var
  Flag  : array [1..3] of Boolean;
  Count : Integer;
  i     : Integer;
  j     : Integer;
begin
  Count := 0;
  for i := 1 to 3 do Flag[i] := False;
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    if not Flag[i] then
      if Coincident(Triangle1[i],Triangle2[j]) then
      begin
        Inc(Count);
        Flag[i] := True;
        Break;
      end;
  end;
  Result := (Count = 3);
end;
(* End of Coincident - 2D Triangles *)


function Coincident(const Triangle1,Triangle2:TTriangle3D):Boolean;
var
  Flag  : array [1..3] of Boolean;
  Count : Integer;
  i     : Integer;
  j     : Integer;
begin
  Count := 0;
  for i := 1 to 3 do Flag[i] := False;
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
    if not Flag[i] then
      if Coincident(Triangle1[i],Triangle2[j]) then
      begin
        Inc(Count);
        Flag[i] := True;
        Break;
      end;
  end;
  Result := (Count = 3);
end;
(* End of Coincident - 3D Triangles *)


function Coincident(const Rect1,Rect2:TRectangle):Boolean;
begin
  Result := Coincident(Rect1[1],Rect2[1]) and
            Coincident(Rect1[2],Rect2[2]);
end;
(* End of Coincident - Rectangles *)


function Coincident(const Quad1,Quad2:TQuadix2D):Boolean;
var
  Flag  : array [1..4] of Boolean;
  Count : Integer;
  i     : Integer;
  j     : Integer;
begin
  Result := False;
  if ConvexQuadix(Quad1) <> ConvexQuadix(Quad2) then Exit;
  Count := 0;
  for i := 1 to 4 do Flag[I] := False;
  for i := 1 to 4 do
  begin
    for j := 1 to 4 do
    if not Flag[i] then
      if Coincident(Quad1[i],Quad2[j]) then
      begin
        Inc(Count);
        Flag[i] := True;
        Break;
      end;
  end;
  Result := (Count = 4);
end;
(* End of Coincident - 2D Quadii *)


function Coincident(const Quad1,Quad2:TQuadix3D):Boolean;
begin
  (* to be implemented at a later date *)
  Result := False;
end;
(* End of Coincident - 3D Quadii *)


function Coincident(const Circle1,Circle2:TCircle):Boolean;
begin
 Result := IsEqual(Circle1.x      , Circle2.x) and
           IsEqual(Circle1.y      , Circle2.y) and
           IsEqual(Circle1.Radius , Circle2.Radius);
end;
(* End of Coincident - Circles *)


function Coincident(const Sphr1,Sphr2:TSphere):Boolean;
begin
  Result := IsEqual(Sphr1.x      , Sphr2.x) and
            IsEqual(Sphr1.y      , Sphr2.y) and
            IsEqual(Sphr1.z      , Sphr2.z) and
            IsEqual(Sphr1.Radius , Sphr2.Radius);
end;
(* End of Coincident - Spheres *)


function Coincident(const Obj1,Obj2:TGeometricObject):Boolean;
begin
  if (Obj1.ObjectType = geoPoint2D) and (Obj2.ObjectType = geoPoint2D) then
    Result := Coincident(Obj1.Point2D,Obj2.Point2D)
  else if (Obj1.ObjectType = geoPoint3D) and (Obj2.ObjectType = geoPoint3D) then
    Result := Coincident(Obj1.Point3D,Obj2.Point3D)
  else if (Obj1.ObjectType = geoSegment2D) and (Obj2.ObjectType = geoSegment2D) then
    Result := Coincident(Obj1.Segment2D,Obj2.Segment2D)
  else if (Obj1.ObjectType = geoSegment3D) and (Obj2.ObjectType = geoSegment3D) then
    Result := Coincident(Obj1.Segment3D,Obj2.Segment3D)
  else if (Obj1.ObjectType = geoTriangle2D) and (Obj2.ObjectType = geoTriangle2D) then
    Result := Coincident(Obj1.Triangle2D,Obj2.Triangle2D)
  else if (Obj1.ObjectType = geoTriangle3D) and (Obj2.ObjectType = geoTriangle3D) then
    Result := Coincident(Obj1.Triangle3D,Obj2.Triangle3D)
  else if (Obj1.ObjectType = geoQuadix2D) and (Obj2.ObjectType = geoQuadix2D) then
    Result := Coincident(Obj1.Quadix2D,Obj2.Quadix2D)
  else if (Obj1.ObjectType = geoQuadix3D) and (Obj2.ObjectType = geoQuadix3D) then
    Result := Coincident(Obj1.Quadix3D,Obj2.Quadix3D)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Coincident(Obj1.Circle,Obj2.Circle)
  else if (Obj1.ObjectType = geoSphere) and (Obj2.ObjectType = geoSphere) then
    Result := Coincident(Obj1.Sphere,Obj2.Sphere)
  else
    Result := False;
end;
(* End of Coincident - Geometric Object *)


function Parallel(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := IsEqual(((y1 - y2) * (x3 - x4)),((y3 - y4) * (x1 - x2)),Epsilon);
end;
(* End of Parallel *)


function Parallel(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := Parallel(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Parallel *)


function Parallel(const Segment1,Segment2:TSegment2D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := Parallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Parallel *)


function Parallel(const Line1,Line2:TLine2D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := Parallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Parallel *)


function Parallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const Epsilon : TFloat = Epsilon_Medium):Boolean;
var
  Dx1 : TFloat;
  Dx2 : TFloat;
  Dy1 : TFloat;
  Dy2 : TFloat;
  Dz1 : TFloat;
  Dz2 : TFloat;
begin
 (*
    Theory:
    if the gradients in the following planes x-y, y-z, z-x are equal then
    it can be said that the segments are parallel in 3D - ~ i think ~
    Worst case scenario: 6 floating point multiplications and 9 floating
    point subtractions
 *)

  Result := False;

  Dx1 := x1 - x2;
  Dx2 := x3 - x4;

  Dy1 := y1 - y2;
  Dy2 := y3 - y4;

  Dz1 := z1 - z2;
  Dz2 := z3 - z4;

  if NotEqual((Dy1 * Dx2),(Dy2 * Dx1),Epsilon) then Exit;
  if NotEqual((Dz1 * Dy2),(Dz2 * Dy1),Epsilon) then Exit;
  if NotEqual((Dx1 * Dz2),(Dx2 * Dz1),Epsilon) then Exit;

  Result := True;
end;
(* End of Parallel *)


function Parallel(const Point1,Point2,Point3,Point4:TPoint3D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result:= Parallel(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z,Epsilon)
end;
(* End of Parallel *)


function Parallel(const Segment1,Segment2:TSegment3D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result:= Parallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Parallel *)


function Parallel(const Line1,Line2:TLine3D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result:= Parallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Parallel *)


function RobustParallel(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; const Epsilon:TFloat = Epsilon_Medium):Boolean;
var
   Px1 : TFloat;
   Py1 : TFloat;
   Px2 : TFloat;
   Py2 : TFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x3,y3,Px1,Py1);
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x4,y4,Px2,Py2);
  Result := IsEqual(Distance(x3,y3,Px1,Py1),Distance(x4,y4,Px2,Py2),Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Segment1,Segment2:TSegment2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Line1,Line2:TLine2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat;const Epsilon:TFloat = Epsilon_Medium):Boolean;
var
  Px1 : TFloat;
  Py1 : TFloat;
  Pz1 : TFloat;
  Px2 : TFloat;
  Py2 : TFloat;
  Pz2 : TFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x3,y3,z3,Px1,Py1,Pz1);
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x4,y4,z4,Px2,Py2,Pz2);
  Result := IsEqual(Distance(x3,y3,z3,Px1,Py1,Pz1),Distance(x4,y4,z4,Px2,Py2,Pz2),Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Point1,Point2,Point3,Point4:TPoint3D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point3.z,Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Segment1,Segment2:TSegment3D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Parallel *)


function RobustParallel(const Line1,Line2:TLine3D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustParallel(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Parallel *)


function Perpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result:= IsEqual(-1.0 * (y2 - y1) * (y4 - y3),(x4 - x3) * (x2 - x1), Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Segment1,Segment2:TSegment2D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result:= Perpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Line1,Line2:TLine2D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result:= Perpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const Epsilon : TFloat = Epsilon_Medium):Boolean;
var
  Dx1 : TFloat;
  Dx2 : TFloat;
  Dy1 : TFloat;
  Dy2 : TFloat;
  Dz1 : TFloat;
  Dz2 : TFloat;
begin
 (*
    The dot product of the vector forms of the segments will
    be 0 if the segments are perpendicular
 *)

  Dx1 := x1 - x2;
  Dx2 := x3 - x4;

  Dy1 := y1 - y2;
  Dy2 := y3 - y4;

  Dz1 := z1 - z2;
  Dz2 := z3 - z4;

  Result := IsEqual((Dx1 * Dx2) + (Dy1 * Dy2) + (Dz1 * Dz2),0.0,Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Point1,Point2,Point3,Point4:TPoint3D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z);
end;
(* End of Perpendicular *)


function Perpendicular(const Segment1,Segment2:TSegment3D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Perpendicular *)


function Perpendicular(const Line1,Line2:TLine3D; const Epsilon : TFloat = Epsilon_Medium):Boolean;
begin
  Result := Perpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Perpendicular *)


function RobustPerpendicular(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; const Epsilon:TFloat = Epsilon_Medium):Boolean;
var
  P1x : TFloat;
  P1y : TFloat;
  P2x : TFloat;
  P2y : TFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x3,y3,P1x,P1y);
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,x4,y4,P2x,P2y);
  Result := IsEqual(Distance(P1x,P1y,P2x,P2y),0.0,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Point1,Point2,Point3,Point4:TPoint2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Segment1,Segment2:TSegment2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Line1,Line2:TLine2D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; const Epsilon:TFloat = Epsilon_Medium):Boolean;
var
  P1x : TFloat;
  P1y : TFloat;
  P1z : TFloat;
  P2x : TFloat;
  P2y : TFloat;
  P2z : TFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x3,y3,z3,P1x,P1y,P1z);
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,x4,y4,z3,P2x,P2y,P2z);
  Result := IsEqual(Distance(P1x,P1y,P1z,P2x,P2y,P2z),0.0,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Point1,Point2,Point3,Point4:TPoint3D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Point4.x,Point4.y,Point4.z,Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Segment1,Segment2:TSegment3D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Segment1[1],Segment1[2],Segment2[1],Segment2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function RobustPerpendicular(const Line1,Line2:TLine3D; const Epsilon:TFloat = Epsilon_Medium):Boolean;
begin
  Result := RobustPerpendicular(Line1[1],Line1[2],Line2[1],Line2[2],Epsilon);
end;
(* End of Robust Perpendicular *)


function LineToLineIntersect(x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
begin
  Result := False;
  if NotEqual((x1 - x2) * (y3 - y4),(y1 - y2) * (x3 - x4)) then
    Result := True
  else if Collinear(x1,y1,x2,y2,x3,y3) then
    Result := True;
end;
(* End of LineToLineIntersect *)


function LineToLineIntersect(Line1,Line2:TLine2D):Boolean;
begin
  Result := LineToLineIntersect(Line1[1].x,Line1[1].y,Line1[2].x,Line1[2].y,Line2[1].x,Line2[1].y,Line2[2].x,Line2[2].y);
end;
(* End of LineToLineIntersect *)


function RectangleToRectangleIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
begin
  (*
    Assumes that:  x1 < x2, y1 < y2, x3 < x4, y3 < y4
  *)
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;
(* End of Rectangle To Rectangle Intersect *)


function RectangleToRectangleIntersect(const Rectangle1,Rectangle2:TRectangle):Boolean;
begin
  Result := RectangleToRectangleIntersect(
                                          Rectangle1[1].x,Rectangle1[1].y,Rectangle1[2].x,Rectangle1[2].y,
                                          Rectangle2[1].x,Rectangle2[1].y,Rectangle2[2].x,Rectangle2[2].y
                                         );
end;
(* End of Rectangle To Rectangle Intersect *)


function RectangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
begin
  Result := PointInRectangle(x1,y1,x3,y3,x4,y4) and PointInRectangle(x2,y2,x3,y3,x4,y4);
end;
(* End of Rectangle Within Rectangle Intersect *)


function RectangleWithinRectangle(const Rectangle1,Rectangle2:TRectangle):Boolean;
begin
  Result := RectangleWithinRectangle(
                                     Rectangle1[1].x,Rectangle1[1].y,Rectangle1[2].x,Rectangle1[2].y,
                                     Rectangle2[1].x,Rectangle2[1].y,Rectangle2[2].x,Rectangle2[2].y
                                    );
end;
(* End of Rectangle Within Rectangle Intersect *)


function CircleWithinRectangle(const x,y,Radius,x1,y1,x2,y2:TFloat):Boolean;
begin
  Result := RectangleWithinRectangle(AABB(EquateCircle(x,y,Radius)),EquateRectangle(x1,y1,x2,y2));
end;
(* End of Circle Within Rectangle Intersect *)


function CircleWithinRectangle(const Circle:TCircle; const Rectangle:TRectangle):Boolean;
begin
  Result := RectangleWithinRectangle(AABB(Circle),Rectangle);
end;
(* End of Circle Within Rectangle Intersect *)


function TriangleWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4,x5,y5:TFloat):Boolean;
begin
  Result := PointInRectangle(x1,y1,x4,y4,x5,y5) and
            PointInRectangle(x2,y2,x4,y4,x5,y5) and
            PointInRectangle(x3,y3,x4,y4,x5,y5);
end;
(* End of Triangle Within Rectangle *)


function TriangleWithinRectangle(const Triangle:TTriangle2D; const Rectangle:TRectangle):Boolean;
begin
  Result := PointInRectangle(Triangle[1],Rectangle) and
            PointInRectangle(Triangle[2],Rectangle) and
            PointInRectangle(Triangle[3],Rectangle);
end;
(* End of Triangle Within Rectangle *)


function SegmentWithinRectangle(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
begin
  Result := PointInRectangle(x1,y1,x3,y3,x4,y4) and
            PointInRectangle(x2,y2,x3,y3,x4,y4);
end;
(* End of Segment Within Rectangle *)


function SegmentWithinRectangle(const Segment:TSegment2D; const Rectangle:TRectangle):Boolean;
begin
  Result := SegmentWithinRectangle(  Segment[1].x,  Segment[1].y,  Segment[2].x,  Segment[2].y,
                                   Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of Segment Within Rectangle *)


function CircleInCircle(const Circle1,Circle2:TCircle):Boolean;
begin
  Result := (PointInCircle(Circle1.x,Circle1.y,Circle2) and (Circle1.Radius < Circle2.Radius));
end;
(* End of CircleInCircle *)


function IsTangent(const Segment:TSegment2D; const Circle:TCircle):Boolean;
var
  rSqr        : TFloat;
  drSqr       : TFloat;
  dSqr        : TFloat;
  TempSegment : TSegment2D;
begin
  TempSegment := Translate(-Circle.x,-Circle.y,Segment);
  rSqr        := Circle.Radius * Circle.Radius;
  drSqr       := LayDistance(TempSegment);
  dSqr        := Sqr(TempSegment[1].x * TempSegment[2].y - TempSegment[2].x * TempSegment[1].y);
  Result      := IsEqual((rSqr * drSqr - dSqr),0.0);
end;
(* End of IsTangent *)


function PointOfReflection(const Sx1,Sy1,Sx2,Sy2,P1x,P1y,P2x,P2y:TFloat; out RPx,RPy:TFloat):Boolean;
var
  Ix   : TFloat;
  Iy   : TFloat;
  P1Px : TFloat;
  P1Py : TFloat;
  P2Px : TFloat;
  P2Py : TFloat;
begin
  Result := False;
  if (not Collinear(Sx1,Sy1,Sx2,Sy2,P1x,P1y)) and
     (not Collinear(Sx1,Sy1,Sx2,Sy2,P2x,P2y)) then
  begin
    ClosestPointOnLineFromPoint(Sx1,Sy1,Sx2,Sy2,P1x,P1y,P1Px,P1Py);
    ClosestPointOnLineFromPoint(Sx1,Sy1,Sx2,Sy2,P2x,P2y,P2Px,P2Py);
    Intersect(P1x,P1y,P2Px,P2Py,P2x,P2y,P1Px,P1Py,Ix,Iy);
    ClosestPointOnLineFromPoint(Sx1,Sy1,Sx2,Sy2,Ix,Iy,RPx,RPy);
    if IsPointCollinear(Sx1,Sy1,Sx2,Sy2,RPx,RPy) then
    begin
      Result := True
    end;
  end;
end;
(* End of PointOfReflection *)


function PointOfReflection(const Segment:TSegment2D; const P1,P2:tPoint2D; out RP:TPoint2D):Boolean;
begin
  Result := PointOfReflection(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,P1.x,P1.y,P2.x,P2.y,RP.x,RP.y);
end;
(* End of PointOfReflection *)


procedure Mirror(const Px,Py,x1,y1,x2,y2:TFloat; out Nx,Ny:TFloat);
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Nx := Px + 2 * (Nx - Px);
  Ny := Py + 2 * (Ny - Py);
end;
(* End of Mirror *)


function Mirror(const Point:TPoint2D; const Line:TLine2D):TPoint2D;
begin
  Mirror(Point.x,Point.y,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Mirror *)


function Mirror(const Segment:TSegment2D; const Line:TLine2D):TSegment2D;
begin
  Result[1] := Mirror(Segment[1],Line);
  Result[2] := Mirror(Segment[2],Line);
end;
(* End of Mirror *)


function Mirror(const Rectangle:TRectangle; const Line:TLine2D):TRectangle;
begin
  Result[1] := Mirror(Rectangle[1],Line);
  Result[2] := Mirror(Rectangle[2],Line);
end;
(* End of Mirror *)


function Mirror(const Triangle:TTriangle2D; const Line:TLine2D):TTriangle2D;
begin
  Result[1] := Mirror(Triangle[1],Line);
  Result[2] := Mirror(Triangle[2],Line);
  Result[3] := Mirror(Triangle[3],Line);
end;
(* End of Mirror *)


function Mirror(const Quadix:TQuadix2D; const Line:TLine2D):TQuadix2D;
begin
  Result[1] := Mirror(Quadix[1],Line);
  Result[2] := Mirror(Quadix[2],Line);
  Result[3] := Mirror(Quadix[3],Line);
end;
(* End of Mirror *)


function Mirror(const Circle:TCircle; const Line:TLine2D):TCircle;
begin
  Result.Radius := Circle.Radius;
  Mirror(Circle.x,Circle.y,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Mirror *)


function Mirror(const Obj:TGeometricObject; const Line:TLine2D):TGeometricObject;
begin
  Result.ObjectType := Obj.ObjectType;
  case Obj.ObjectType of
    geoSegment2D  : Result.Segment2D  := Mirror(Obj.Segment2D ,Line);
    geoTriangle2D : Result.Triangle2D := Mirror(Obj.Triangle2D,Line);
    geoRectangle  : Result.Rectangle  := Mirror(Obj.Rectangle ,Line);
    geoQuadix2D   : Result.Quadix2D   := Mirror(Obj.Quadix2D  ,Line);
    geoCircle     : Result.Circle     := Mirror(Obj.Circle    ,Line);
  end;
end;
(* End of Mirror *)


procedure NonSymmetricMirror(const Px,Py,x1,y1,x2,y2:TFloat; const Ratio:TFloat; out Nx,Ny:TFloat);
var
  GeneralRatio : TFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  GeneralRatio := 2 * Ratio;
  Nx := Px + GeneralRatio * (Nx - Px);
  Ny := Py + GeneralRatio * (Ny - Py);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Point:TPoint2D; const Ratio:TFloat; const Line:TLine2D):TPoint2D;
begin
  NonSymmetricMirror(Point.x,Point.y,Ratio,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Segment:TSegment2D; const Ratio:TFloat; const Line:TLine2D):TSegment2D;
begin
  Result[1] := NonSymmetricMirror(Segment[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Segment[2],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Rectangle:TRectangle; const Ratio:TFloat; const Line:TLine2D):TRectangle;
begin
  Result[1] := NonSymmetricMirror(Rectangle[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Rectangle[2],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Triangle:TTriangle2D; const Ratio:TFloat; const Line:TLine2D):TTriangle2D;
begin
  Result[1] := NonSymmetricMirror(Triangle[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Triangle[2],Ratio,Line);
  Result[3] := NonSymmetricMirror(Triangle[3],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Quadix:TQuadix2D; const Ratio:TFloat; const Line:TLine2D):TQuadix2D;
begin
  Result[1] := NonSymmetricMirror(Quadix[1],Ratio,Line);
  Result[2] := NonSymmetricMirror(Quadix[2],Ratio,Line);
  Result[3] := NonSymmetricMirror(Quadix[3],Ratio,Line);
  Result[4] := NonSymmetricMirror(Quadix[4],Ratio,Line);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Circle:TCircle; const Ratio:TFloat; const Line:TLine2D):TCircle;
begin
  Result.Radius := Circle.Radius;
  NonSymmetricMirror(Circle.x,Circle.y,Ratio,Line[1].x,Line[1].y,Line[2].x,Line[2].y,Result.x,Result.y);
end;
(* End of Non-symmetric Mirror *)


function NonSymmetricMirror(const Obj:TGeometricObject; const Ratio:TFloat; const Line:TLine2D):TGeometricObject;
begin
  Result.ObjectType := Obj.ObjectType;
  case Obj.ObjectType of
    geoSegment2D  : Result.Segment2D  := NonSymmetricMirror(Obj.Segment2D ,Ratio,Line);
    geoTriangle2D : Result.Triangle2D := NonSymmetricMirror(Obj.Triangle2D,Ratio,Line);
    geoRectangle  : Result.Rectangle  := NonSymmetricMirror(Obj.Rectangle ,Ratio,Line);
    geoQuadix2D   : Result.Quadix2D   := NonSymmetricMirror(Obj.Quadix2D  ,Ratio,Line);
    geoCircle     : Result.Circle     := NonSymmetricMirror(Obj.Circle    ,Ratio,Line);
  end;
end;
(* End of Non-symmetric Mirror *)


function Distance(const x1,y1,x2,y2:TFloat):TFloat;
var
  dx : TFloat;
  dy : TFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  Result := Sqrt(dx * dx + dy * dy);
end;
(* End of Distance *)


function Distance(const Point1,Point2:TPoint2D):TFloat;
begin
  Result := Distance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Distance *)


function Distance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;
var
  dx : TFloat;
  dy : TFloat;
  dz : TFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  dz := z2 - z1;
  Result := Sqrt(dx * dx + dy * dy + dz * dz);
end;
(* End of Distance *)


function Distance(const Point1,Point2:TPoint3D):TFloat;
begin
  Result := Distance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of Distance *)


function Distance(const Point:TPoint2D; const Segment:TSegment2D):TFloat;
begin
  Result := MinimumDistanceFromPointToSegment(Point,Segment);
end;
(* End of Distance *)


function Distance(const Point:TPoint2D; const Rectangle:TRectangle):TFloat;
begin
  Result := MinimumDistanceFromPointToRectangle(Point,Rectangle);
end;
(* End of Distance *)


function Distance(const Point:TPoint2D; const Triangle:TTriangle2D):TFloat;
begin
  Result := MinimumDistanceFromPointToTriangle(Point,Triangle);
end;
(* End of Distance *)


function Distance(const Point:TPoint2D; const Quadix:TQuadix2D):TFloat;
begin
  Result := sqrt(LayDistance(Point,Quadix));
end;
(* End of Distance *)


function Distance(const Line1,Line2:TLine2D):TFloat;
begin
  Result := DistanceLineToLine(
                               Line1[1].x,Line1[1].y,Line1[2].x,Line1[2].y,
                               Line2[1].x,Line2[1].y,Line2[2].x,Line2[2].y
                              );
end;
(* End of Distance *)


function Distance(const Line1,Line2:TLine3D):TFloat;
begin
  Result := DistanceLineToLine(
                               Line1[1].x,Line1[1].y,Line1[1].z,Line1[2].x,Line1[2].y,Line1[2].z,
                               Line2[1].x,Line2[1].y,Line2[1].z,Line2[2].x,Line2[2].y,Line2[2].z
                              );
end;
(* End of Distance *)


function Distance(const Segment1,Segment2:TSegment2D):TFloat;
begin
  Result := DistanceSegmentToSegment(
                                     Segment1[1].x,Segment1[1].y,Segment1[2].x,Segment1[2].y,
                                     Segment2[1].x,Segment2[1].y,Segment2[2].x,Segment2[2].y
                                    );
end;
(* End of Distance *)


function Distance(const Segment1,Segment2:TSegment3D):TFloat;
begin
  Result := DistanceSegmentToSegment(
                                     Segment1[1].x,Segment1[1].y,Segment1[1].z,Segment1[2].x,Segment1[2].y,Segment1[2].z,
                                     Segment2[1].x,Segment2[1].y,Segment2[1].z,Segment2[2].x,Segment2[2].y,Segment2[2].z
                                    );
end;
(* End of Distance *)


function Distance(const Segment:TSegment2D):TFloat;
begin
  Result := Distance(Segment[1],Segment[2]);
end;
(* End of Distance *)


function Distance(const Segment:TSegment3D):TFloat;
begin
  Result := Distance(Segment[1],Segment[2]);
end;
(* End of Distance *)


function Distance(const Segment:TSegment2D; const Triangle:TTriangle2D):TFloat;
begin
  Result := sqrt(LayDistance(Segment,Triangle));
end;
(* End of Distance *)


function Distance(const Segment:TSegment3D; const Triangle:TTriangle3D):TFloat;
begin
  Result := sqrt(LayDistance(Segment,Triangle));
end;
(* End of Distance *)


function Distance(const Triangle1,Triangle2:TTriangle2D):TFloat;
var
  i : Integer;
begin
  Result := Min(
                MinimumDistanceFromPointToTriangle(Triangle1[1],Triangle2),
                MinimumDistanceFromPointToTriangle(Triangle2[1],Triangle1)
               );

  for i := 2 to 3 do
  begin
    if IsEqual(Result,0.0) then Exit;
    Result := Min(
                  Result,
                  Min(
                      MinimumDistanceFromPointToTriangle(Triangle1[i],Triangle2),
                      MinimumDistanceFromPointToTriangle(Triangle2[i],Triangle1)
                     )
                 );
  end
end;
(* End of Distance *)


function Distance(const Triangle:TTriangle2D; const Rectangle:TRectangle):TFloat;
begin
  if Intersect(Triangle,Rectangle) then
    Result := 0.0
  else
    Result := Min(
                  Min(
                      Distance(RectangleEdge(Rectangle,1),Triangle),
                      Distance(RectangleEdge(Rectangle,2),Triangle)
                     ),
                  Min(
                      Distance(RectangleEdge(Rectangle,3),Triangle),
                      Distance(RectangleEdge(Rectangle,4),Triangle)
                     )
                 );
end;
(* End of Distance *)


function Distance(const Rectangle1,Rectangle2:TRectangle):TFloat;
var
  Rec1 : TRectangle;
  Rec2 : TRectangle;
begin
  if Intersect(Rectangle1,Rectangle2) then
    Result := 0.0
  else
  begin
    Rec1 := AABB(Rectangle1);
    Rec2 := AABB(Rectangle2);

    if Rec1[2].y < Rec2[1].y then
      Result := Distance(EquateSegment(Rec1[1].x,Rec1[2].y,Rec1[2].x,Rec1[2].y),EquateSegment(Rec2[1].x,Rec2[1].y,Rec2[2].x,Rec2[1].y))
    else if Rec1[1].y > Rec2[2].y then
      Result := Distance(EquateSegment(Rec1[1].x,Rec1[1].y,Rec1[2].x,Rec1[1].y),EquateSegment(Rec2[1].x,Rec2[2].y,Rec2[2].x,Rec2[2].y))
    else if Rec1[2].x < Rec2[1].x then
      Result := Distance(EquateSegment(Rec1[2].x,Rec1[1].y,Rec1[2].x,Rec1[2].y),EquateSegment(Rec2[1].x,Rec2[1].y,Rec2[1].x,Rec2[2].y))
    else if Rec1[1].x > Rec2[2].x then
      Result := Distance(EquateSegment(Rec1[1].x,Rec1[1].y,Rec1[1].x,Rec1[2].y),EquateSegment(Rec2[2].x,Rec2[1].y,Rec2[2].x,Rec2[2].y))
    else
      Result := 0.0;
  end;
end;
(* End of Distance *)


function Distance(const Segment:TSegment2D; const Rectangle:TRectangle):TFloat;
begin
  Result := Min(
                Min(Distance(Segment,RectangleEdge(Rectangle,1)),Distance(Segment,RectangleEdge(Rectangle,2))),
                Min(Distance(Segment,RectangleEdge(Rectangle,3)),Distance(Segment,RectangleEdge(Rectangle,4)))
               );
end;
(* End of Distance *)


function Distance(const Segment:TSegment2D; const Circle:TCircle):TFloat;
begin
   Result := Distance(ClosestPointOnCircleFromSegment(Circle,Segment),Segment);
end;
(* End of Distance *)


function Distance(const Triangle : TTriangle2D; const Circle:TCircle):TFloat;
var
  Point1 : TPoint2D;
  Point2 : TPoint2D;
begin
  if Intersect(Triangle,Circle) then
    Result := 0.0
  else
  begin
    Point1 := ClosestPointOnTriangleFromPoint(Triangle,Circle.x,Circle.y);
    Point2 := ClosestPointOnCircleFromPoint(Circle,Point1);
    Result := Distance(Point1,Point2);
  end;
end;
(* End of Distance *)


function Distance(const Rectangle : TRectangle; const Circle:TCircle):TFloat;
var
  Point1 : TPoint2D;
  Point2 : TPoint2D;
begin
  if Intersect(Rectangle,Circle) then
    Result := 0.0
  else
  begin
    Point1 := ClosestPointOnRectangleFromPoint(Rectangle,Circle.x,Circle.y);
    Point2 := ClosestPointOnCircleFromPoint(Circle,Point1);
    Result := Distance(Point1,Point2);
  end;
end;
(* End of Distance *)


function Distance(const Point : TPoint2D; const Circle:TCircle):TFloat;
begin
  if PointIncircle(Point,Circle) then
    Result := 0.0
  else
    Result := Distance(Point,ClosestPointOnCircleFromPoint(Circle,Point));
end;
(* End of Distance *)


function Distance(const Circle1,Circle2:TCircle):TFloat;
var
  Dist : TFloat;
begin
  Dist := Distance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);
  if Dist > Circle1.Radius + Circle2.Radius then
    Result := Dist - (Circle1.Radius + Circle2.Radius)
  else
    Result := 0.0;
end;
(* End of Distance *)


function Distance(const Sphere1,Sphere2:TSphere):TFloat;
var
  Dist : TFloat;
begin
  Dist := Distance(Sphere1.x,Sphere1.y,Sphere1.z,Sphere2.x,Sphere2.y,Sphere2.z);
  if Dist > Sphere1.Radius + Sphere2.Radius then
    Result := Dist - (Sphere1.Radius + Sphere2.Radius)
  else
    Result := 0.0;
end;
(* End of Distance *)


function Distance(const Obj1,Obj2:TGeometricObject):TFloat;
begin
  Result := 0.0;
  if (Obj1.ObjectType = geoPoint2D) and (Obj2.ObjectType = geoPoint2D) then
    Result := Distance(Obj1.Point2D,Obj2.Point2D)
  else if (Obj1.ObjectType = geoPoint3D) and (Obj2.ObjectType = geoPoint3D) then
    Result := Distance(Obj1.Point3D,Obj2.Point3D)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Distance(Obj1.Circle,Obj2.Circle)
  else if (Obj1.ObjectType = geoSegment3D) and (Obj2.ObjectType = geoSegment3D) then
    Result := Distance(Obj1.Segment3D,Obj2.Segment3D);
end;
(* End of Distance *)


function LayDistance(const x1,y1,x2,y2:TFloat):TFloat;
var
  dx : TFloat;
  dy : TFloat;
begin
  dx := (x2 - x1);
  dy := (y2 - y1);
  Result := dx * dx + dy * dy;
end;
(* End of LayDistance *)


function LayDistance(const Point1,Point2:TPoint2D):TFloat;
begin
  Result := LayDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of LayDistance *)


function LayDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;
var
  dx : TFloat;
  dy : TFloat;
  dz : TFloat;
begin
  dx := x2 - x1;
  dy := y2 - y1;
  dz := z2 - z1;
  Result := dx * dx + dy * dy + dz * dz;
end;
(* End of LayDistance *)


function LayDistance(const Point1,Point2:TPoint3D):TFloat;
begin
  Result := LayDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of LayDistance *)


function LayDistance(const Point:TPoint2D; const Triangle:TTriangle2D):TFloat;
begin
  Result := LayDistance(Point,ClosestPointOnTriangleFromPoint(Triangle,Point));
end;
(* End of LayDistance *)


function LayDistance(const Point:TPoint2D; const Quadix:TQuadix2D):TFloat;
begin
  Result := LayDistance(Point,ClosestPointOnQuadixFromPoint(Quadix,Point));
end;
(* End of LayDistance *)


function LayDistance(const Segment1,Segment2:TSegment2D):TFloat;
begin
  Result := LayDistanceSegmentToSegment(
                                        Segment1[1].x,Segment1[1].y,Segment1[2].x,Segment1[2].y,
                                        Segment2[1].x,Segment2[1].y,Segment2[2].x,Segment2[2].y
                                        );
end;
(* End of LayDistance *)


function LayDistance(const Segment1,Segment2:TSegment3D):TFloat;
begin
  Result := LayDistanceSegmentToSegment(
                                        Segment1[1].x,Segment1[1].y,Segment1[1].z,Segment1[2].x,Segment1[2].y,Segment1[2].z,
                                        Segment2[1].x,Segment2[1].y,Segment2[1].z,Segment2[2].x,Segment2[2].y,Segment2[2].z
                                        );
end;
(* End of LayDistance *)


function LayDistance(const Line1,Line2:TLine2D):TFloat;
begin
  Result := LayDistanceLineToLine(
                                  Line1[1].x,Line1[1].y,Line1[2].x,Line1[2].y,
                                  Line2[1].x,Line2[1].y,Line2[2].x,Line2[2].y
                                 );
end;
(* End of Distance *)


function LayDistance(const Line1,Line2:TLine3D):TFloat;
begin
  Result := LayDistanceLineToLine(
                                  Line1[1].x,Line1[1].y,Line1[1].z,Line1[2].x,Line1[2].y,Line1[2].z,
                                  Line2[1].x,Line2[1].y,Line2[1].z,Line2[2].x,Line2[2].y,Line2[2].z
                                 );
end;
(* End of Distance *)


function LayDistance(const Segment:TSegment2D):TFloat;
begin
  Result := LayDistance(Segment[1],Segment[2]);
end;
(* End of LayDistance *)


function LayDistance(const Segment:TSegment3D):TFloat;
begin
  Result := LayDistance(Segment[1],Segment[2]);
end;
(* End of LayDistance *)


function LayDistance(const Segment:TSegment2D; const Triangle:TTriangle2D):TFloat;
begin
  Result := Min(
                Min(
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                                Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y
                                               ),
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                                Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y
                                               )
                   ),
                LayDistanceSegmentToSegment(
                                            Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                            Triangle[3].x,Triangle[3].y,Triangle[1].x,Triangle[1].y
                                           )
            );
end;
(* End of LayDistance *)


function LayDistance(const Segment:TSegment3D; const Triangle:TTriangle3D):TFloat;
begin
  Result := Min(
                Min(
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[1].z, Segment[2].x, Segment[2].y, Segment[2].z,
                                                Triangle[1].x,Triangle[1].y,Triangle[1].z,Triangle[2].x,Triangle[2].y,Triangle[2].z
                                               ),
                    LayDistanceSegmentToSegment(
                                                Segment[1].x, Segment[1].y, Segment[1].z, Segment[2].x, Segment[2].y, Segment[2].z,
                                                Triangle[2].x,Triangle[2].y,Triangle[2].z,Triangle[3].x,Triangle[3].y,Triangle[3].z
                                               )
                   ),
                LayDistanceSegmentToSegment(
                                            Segment[1].x, Segment[1].y, Segment[2].x, Segment[2].y,
                                            Triangle[3].x,Triangle[3].y,Triangle[1].x,Triangle[1].y
                                           )
            );
end;


function ManhattanDistance(const x1,y1,x2,y2:TFloat):TFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Point1,Point2:TPoint2D):TFloat;
begin
  Result := ManhattanDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1) + Abs(z2 - z1);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Point1,Point2:TPoint3D):TFloat;
begin
  Result := ManhattanDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Segment:TSegment2D):TFloat;
begin
  Result := ManhattanDistance(Segment[1],Segment[2]);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Segment:TSegment3D):TFloat;
begin
  Result := ManhattanDistance(Segment[1],Segment[2]);
end;
(* End of ManhattanDistance *)


function ManhattanDistance(const Circle1,Circle2:TCircle):TFloat;
begin
  Result := ManhattanDistance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);
end;
(* End of ManhattanDistance *)


function VectorSumDistance(const x1,y1,x2,y2:TFloat):TFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Point1,Point2:TPoint2D):TFloat;
begin
  Result := VectorSumDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;
begin
  Result := Abs(x2 - x1) + Abs(y2 - y1) + Abs(z2 - z1);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Point1,Point2:TPoint3D):TFloat;
begin
  Result := VectorSumDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Segment:TSegment2D):TFloat;
begin
  Result := VectorSumDistance(Segment[1],Segment[2]);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Segment:TSegment3D):TFloat;
begin
  Result := VectorSumDistance(Segment[1],Segment[2]);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Circle1,Circle2:TCircle):TFloat;
begin
  Result := VectorSumDistance(Circle1.x,Circle1.y,Circle2.x,Circle2.y);
end;
(* End of VectorSumDistance *)


function VectorSumDistance(const Obj1,Obj2:TGeometricObject):TFloat;
begin
  Result := 0.0;
  if (Obj1.ObjectType = geoPoint2D) and (Obj2.ObjectType = geoPoint2D) then
    Result := Distance(Obj1.Point2D,Obj2.Point2D)
  else if (Obj1.ObjectType = geoPoint3D) and (Obj2.ObjectType = geoPoint3D) then
    Result := Distance(Obj1.Point3D,Obj2.Point3D)
  else if (Obj1.ObjectType = geoCircle) and (Obj2.ObjectType = geoCircle) then
    Result := Distance(Obj1.Circle,Obj2.Circle)
end;
(* End of VectorSumDistance *)


function ChebyshevDistance(const x1,y1,x2,y2:TFloat):TFloat;
begin
  Result := Max(abs(x1-x2),abs(y1-y2));
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Point1,Point2:TPoint2D):TFloat;
begin
  Result := ChebyshevDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;
begin
  Result := Max(Max(abs(x1-x2),abs(y1-y2)),abs(z1-z2));
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Point1,Point2:TPoint3D):TFloat;
begin
  Result := ChebyshevDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Segment:TSegment2D):TFloat;
begin
  Result := ChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Segment:TSegment3D):TFloat;
begin
  Result := ChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of ChebyshevDistance *)


function ChebyshevDistance(const Circle1,Circle2:TCircle):TFloat;
begin
  Result := Max(abs(Circle1.x - Circle2.x),abs(Circle1.y - Circle2.y));
end;
(* End of ChebyshevDistance *)


function InverseChebyshevDistance(const x1,y1,x2,y2:TFloat):TFloat;
begin
  Result := Min(abs(x1 - x2),abs(y1 - y2));
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Point1,Point2:TPoint2D):TFloat;
begin
  Result := InverseChebyshevDistance(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const x1,y1,z1,x2,y2,z2:TFloat):TFloat;
begin
  Result := Min(Min(abs(x1 - x2),abs(y1 - y2)),abs(z1 - z2));
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Point1,Point2:TPoint3D):TFloat;
begin
  Result := InverseChebyshevDistance(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Segment:TSegment2D):TFloat;
begin
  Result := InverseChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Segment:TSegment3D):TFloat;
begin
  Result := InverseChebyshevDistance(Segment[1],Segment[2]);
end;
(* End of InverseChebyshevDistance *)


function InverseChebyshevDistance(const Circle1,Circle2:TCircle):TFloat;
begin
  Result := Min(abs(Circle1.x - Circle2.x),abs(Circle1.y - Circle2.y));
end;
(* End of InverseChebyshevDistance *)


function DistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TFloat;
begin
  Result := sqrt(LayDistanceSegmentToSegment(x1,y1,x2,y2,x3,y3,x4,y4));
end;
(* End of DistanceSegmentToSegment *)


function DistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TFloat;
begin
  Result := sqrt(LayDistanceSegmentToSegment(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4));
end;
(* End of DistanceSegmentToSegment *)


function LayDistanceSegmentToSegment(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TFloat;
var
  ux : TFloat;
  uy : TFloat;
  vx : TFloat;
  vy : TFloat;
  wx : TFloat;
  wy : TFloat;
  a  : TFloat;
  b  : TFloat;
  c  : TFloat;
  d  : TFloat;
  e  : TFloat;
  Dt : TFloat;
  sc : TFloat;
  sN : TFloat;
  sD : TFloat;
  tc : TFloat;
  tN : TFloat;
  tD : TFloat;
  dx : TFloat;
  dy : TFloat;
begin
  ux := x2 - x1;
  uy := y2 - y1;

  vx := x4 - x3;
  vy := y4 - y3;

  wx := x1 - x3;
  wy := y1 - y3;

  a  := (ux * ux + uy * uy);
  b  := (ux * vx + uy * vy);
  c  := (vx * vx + vy * vy);
  d  := (ux * wx + uy * wy);
  e  := (vx * wx + vy * wy);
  Dt := a * c - b * b;

  sD := Dt;
  tD := Dt;

  if IsEqual(Dt,0.0) then
  begin
    sN := 0.0;
    sD := 1.0;
    tN := e;
    tD := c;
  end
  else
  begin
    sN := (b * e - c * d);
    tN := (a * e - b * d);
    if sN < 0.0 then
    begin
      sN := 0.0;
      tN := e;
      tD := c;
    end
    else if sN > sD then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if tN < 0.0 then
  begin
    tN := 0.0;
    if -d < 0.0 then
      sN := 0.0
    else if -d > a then
      sN := sD
    else
    begin
      sN := -d;
      sD := a;
    end;
  end
  else if tN > tD  then
  begin
    tN := tD;
    if (-d + b) < 0.0 then
      sN := 0
    else if (-d + b) > a then
      sN := sD
    else
    begin
      sN := (-d + b);
      sD := a;
    end;
  end;

  if IsEqual(sN,0.0) then
    sc := 0.0
  else
    sc := sN / sD;

  if IsEqual(tN,0.0) then
    tc := 0.0
  else
    tc := tN / tD;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  Result := dx * dx + dy * dy;
end;
(* End of LayDistanceSegmentToSegment *)


function LayDistanceSegmentToSegment(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TFloat;
var
  ux : TFloat;
  uy : TFloat;
  uz : TFloat;
  vx : TFloat;
  vy : TFloat;
  vz : TFloat;
  wx : TFloat;
  wy : TFloat;
  wz : TFloat;
  a  : TFloat;
  b  : TFloat;
  c  : TFloat;
  d  : TFloat;
  e  : TFloat;
  Dt : TFloat;
  sc : TFloat;
  sN : TFloat;
  sD : TFloat;
  tc : TFloat;
  tN : TFloat;
  tD : TFloat;
  dx : TFloat;
  dy : TFloat;
  dz : TFloat;
begin
  ux := x2 - x1;
  uy := y2 - y1;
  uz := z2 - z1;

  vx := x4 - x3;
  vy := y4 - y3;
  vz := z4 - z3;

  wx := x1 - x3;
  wy := y1 - y3;
  wz := z1 - z3;

  a  := (ux * ux + uy * uy + uz * uz);
  b  := (ux * vx + uy * vy + uz * vz);
  c  := (vx * vx + vy * vy + vz * vz);
  d  := (ux * wx + uy * wy + uz * wz);
  e  := (vx * wx + vy * wy + vz * wz);
  Dt := a * c - b * b;

  sD := Dt;
  tD := Dt;

  if IsEqual(Dt,0.0) then
  begin
    sN := 0.0;
    sD := 1.0;
    tN := e;
    tD := c;
  end
  else
  begin
    sN := (b * e - c * d);
    tN := (a * e - b * d);
    if sN < 0.0 then
    begin
      sN := 0.0;
      tN := e;
      tD := c;
    end
    else if sN > sD then
    begin
      sN := sD;
      tN := e + b;
      tD := c;
    end;
  end;

  if tN < 0.0 then
  begin
    tN := 0.0;
    if -d < 0.0 then
      sN := 0.0
    else if -d > a then
      sN := sD
    else
    begin
      sN := -d;
      sD := a;
    end;
  end
  else if tN > tD  then
  begin
    tN := tD;
    if (-d + b) < 0.0 then
      sN := 0
    else if (-d + b) > a then
      sN := sD
    else
    begin
      sN := (-d + b);
      sD := a;
    end;
  end;

  if IsEqual(sN,0.0) then
    sc := 0.0
  else
    sc := sN / sD;

  if IsEqual(tN,0.0) then
    tc := 0.0
  else
    tc := tN / tD;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  dz := wz + (sc * uz) - (tc * vz);
  Result := dx * dx + dy * dy + dz * dz;
end;
(* End of LayDistanceSegmentToSegment *)


function DistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4: TFloat):TFloat;
begin
  Result := sqrt(LayDistanceLineToLine(x1,y1,x2,y2,x3,y3,x4,y4));
end;
(* End of DistanceLineToLine *)


function DistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4: TFloat):TFloat;
begin
  Result := sqrt(LayDistanceLineToLine(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4));
end;
(* End of DistanceLineToLine *)


function LayDistanceLineToLine(const x1,y1,x2,y2,x3,y3,x4,y4: TFloat):TFloat;
var
  ux : TFloat;
  uy : TFloat;
  vx : TFloat;
  vy : TFloat;
  wx : TFloat;
  wy : TFloat;
  a  : TFloat;
  b  : TFloat;
  c  : TFloat;
  d  : TFloat;
  e  : TFloat;
  Dt : TFloat;
  sc : TFloat;
  tc : TFloat;
  dx : TFloat;
  dy : TFloat;

begin
  ux := x2 - x1;
  uy := y2 - y1;

  vx := x4 - x3;
  vy := y4 - y3;

  if NotEqual(ux * vy,uy * vx) then
  begin
    Result := 0.0;
    Exit;
  end;

  wx := x1 - x3;
  wy := y1 - y3;

  a  := (ux * ux + uy * uy);
  b  := (ux * vx + uy * vy);
  c  := (vx * vx + vy * vy);
  d  := (ux * wx + uy * wy);
  e  := (vx * wx + vy * wy);
  Dt := a * c - b * b;

  if IsEqual(Dt,0.0) then
  begin
    sc := 0.0;
    if b > c then
      tc := d / b
    else
      tc  := e /c;
  end
  else
  begin
    sc := (b * e - c * d) / Dt;
    tc := (a * e - b * d) / Dt;
  end;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  Result := dx * dx + dy * dy;
end;
(* End of LayDistanceLineToLine *)


function LayDistanceLineToLine(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4: TFloat):TFloat;
var
  ux : TFloat;
  uy : TFloat;
  uz : TFloat;
  vx : TFloat;
  vy : TFloat;
  vz : TFloat;
  wx : TFloat;
  wy : TFloat;
  wz : TFloat;
  a  : TFloat;
  b  : TFloat;
  c  : TFloat;
  d  : TFloat;
  e  : TFloat;
  Dt : TFloat;
  sc : TFloat;
  tc : TFloat;
  dx : TFloat;
  dy : TFloat;
  dz : TFloat;
begin
  ux := x2 - x1;
  uy := y2 - y1;
  uz := z2 - z1;

  vx := x4 - x3;
  vy := y4 - y3;
  vz := z4 - z3;

  wx := x1 - x3;
  wy := y1 - y3;
  wz := z1 - z3;

  a  := (ux * ux + uy * uy + uz * uz);
  b  := (ux * vx + uy * vy + uz * vz);
  c  := (vx * vx + vy * vy + vz * vz);
  d  := (ux * wx + uy * wy + uz * wz);
  e  := (vx * wx + vy * wy + vz * wz);
  Dt := a * c - b * b;

  if IsEqual(Dt,0.0) then
  begin
    sc := 0.0;
    if b > c then
      tc := d / b
    else
      tc  := e /c;
  end
  else
  begin
    sc := (b * e - c * d) / Dt;
    tc := (a * e - b * d) / Dt;
  end;

  dx := wx + (sc * ux) - (tc * vx);
  dy := wy + (sc * uy) - (tc * vy);
  dz := wz + (sc * uz) - (tc * vz);
  Result := dx * dx + dy * dy + dz * dz;
end;
(* End of LayDistanceLineToLine *)


function TriangleType(const x1,y1,x2,y2,x3,y3:TFloat):eTriangletype;
begin
  if IsEquilateralTriangle(x1,y1,x2,y2,x3,y3)    then Result := etEquilateral
   else
    if IsIsoscelesTriangle(x1,y1,x2,y2,x3,y3)    then Result := etIsosceles
     else
      if IsRightTriangle(x1,y1,x2,y2,x3,y3)      then Result := etRight
       else
        if IsScaleneTriangle(x1,y1,x2,y2,x3,y3)  then Result := etScalene
         else
          if IsObtuseTriangle(x1,y1,x2,y2,x3,y3) then Result := etObtuse
           else
             Result := etUnknown;
end;
(* End of Triangletype *)


function TriangleType(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):eTriangletype;
begin
  if IsEquilateralTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)    then Result := etEquilateral
   else
    if IsIsoscelesTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)    then Result := etIsosceles
     else
      if IsRightTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)      then Result := etRight
       else
        if IsScaleneTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3)  then Result := etScalene
         else
          if IsObtuseTriangle(x1,y1,z1,x2,y2,z2,x3,y3,z3) then Result := etObtuse
           else
             Result:= etUnknown;
end;
(* End of Triangletype *)


function TriangleType(const Point1,Point2,Point3:TPoint2D):eTriangletype;
begin
  Result := TriangleType(Point1.x,Point1.y,
                         Point2.x,Point2.y,
                         Point3.x,Point3.y);
end;
(* End of Triangletype *)


function TriangleType(const Point1,Point2,Point3:TPoint3D):eTriangletype;
begin
  Result := TriangleType(Point1.x,Point1.y,Point1.z,
                         Point2.x,Point2.y,Point2.z,
                         Point3.x,Point3.y,Point3.z);
end;
(* End of Triangletype *)


function TriangleType(const Triangle:TTriangle2D):eTriangletype;
begin
  Result := TriangleType(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Triangletype *)


function TriangleType(const Triangle:TTriangle3D):eTriangletype;
begin
  Result := TriangleType(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Triangletype *)


function IsEquilateralTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);
  Result := (IsEqual(d1,d2) and IsEqual(d2,d3));
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);
  Result := (IsEqual(d1,d2) and IsEqual(d2,d3));
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;
begin
  Result := IsEquilateralTriangle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;
begin
  Result := IsEquilateralTriangle(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Triangle:TTriangle2D):Boolean;
begin
  Result := IsEquilateralTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsEquilateralTriangle *)


function IsEquilateralTriangle(const Triangle:TTriangle3D):Boolean;
begin
  Result := IsEquilateralTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsEquilateralTriangle *)


function IsIsoscelesTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);
  Result :=((IsEqual(d1,d2) or  IsEqual(d1,d3)) and NotEqual(d2,d3)) or
            (IsEqual(d2,d3) and NotEqual(d2,d1));
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);
  Result := (
             (IsEqual(d1,d2) or  IsEqual(d1,d3)) and NotEqual(d2,d3)) or
             (IsEqual(d2,d3) and NotEqual(d2,d1)
            );
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;
begin
  Result := IsIsoscelesTriangle(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y);
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;
begin
  Result := IsIsoscelesTriangle(Point1.x, Point1.y,Point1.z, Point2.x, Point2.y,Point2.z, Point3.x, Point3.y, Point3.z);
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Triangle:TTriangle2D):Boolean;
begin
  Result := IsIsoscelesTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsIsoscelesTriangle *)


function IsIsoscelesTriangle(const Triangle:TTriangle3D):Boolean;
begin
  Result := IsIsoscelesTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of *)


function IsRightTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);

  Result := (
             IsEqual(d1 + d2,d3) or
             IsEqual(d1 + d3,d2) or
             IsEqual(d3 + d2,d1)
            );
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);

  Result := (
             IsEqual(d1 + d2,d3) or
             IsEqual(d1 + d3,d2) or
             IsEqual(d3 + d2,d1)
            );
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;
begin
  Result := IsRightTriangle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;
begin
  Result := IsRightTriangle(Point1.x,Point1.y, Point1.z,Point2.x,Point2.y, Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Triangle:TTriangle2D):Boolean;
begin
  Result := IsRightTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsRightTriangle *)


function IsRightTriangle(const Triangle:TTriangle3D):Boolean;
begin
  Result := IsRightTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsRightTriangle *)


function IsScaleneTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,x2,y2);
  d2 := LayDistance(x2,y2,x3,y3);
  d3 := LayDistance(x3,y3,x1,y1);
  Result := NotEqual(d1,d2) and NotEqual(d2,d3) and NotEqual(d3,d1);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;
var
  d1 : TFloat;
  d2 : TFloat;
  d3 : TFloat;
begin
  d1 := LayDistance(x1,y1,z1,x2,y2,z2);
  d2 := LayDistance(x2,y2,z2,x3,y3,z3);
  d3 := LayDistance(x3,y3,z3,x1,y1,z1);
  Result := NotEqual(d1,d2) and NotEqual(d2,d3) and NotEqual(d3,d1);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;
begin
  Result := IsScaleneTriangle(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;
begin
  Result := IsScaleneTriangle(Point1.x, Point1.y, Point1.z, Point2.x, Point2.y, Point2.z, Point3.x, Point3.y, Point3.z);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Triangle:TTriangle2D):Boolean;
begin
  Result := IsScaleneTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsScaleneTriangle *)


function IsScaleneTriangle(const Triangle:TTriangle3D):Boolean;
begin
 Result := IsScaleneTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsScaleneTriangle *)


function IsObtuseTriangle(const x1,y1,x2,y2,x3,y3:TFloat):Boolean;
var
  a1 : TFloat;
  a2 : TFloat;
  a3 : TFloat;
begin
  a1 := VertexAngle(x1,y1,x2,y2,x3,y3);
  a2 := VertexAngle(x3,y3,x1,y1,x2,y2);
  a3 := VertexAngle(x2,y2,x3,y3,x1,y1);
  Result := (a1 > 90.0) or (a2 > 90.0) or (a3 > 90.0);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):Boolean;
var
  a1 : TFloat;
  a2 : TFloat;
  a3 : TFloat;
begin
  a1 := VertexAngle(x1,y1,z1,x2,y2,z2,x3,y3,z3);
  a2 := VertexAngle(x3,y3,z3,x1,y1,z1,x2,y2,z2);
  a3 := VertexAngle(x2,y2,z2,x3,y3,z3,x1,y1,z1);
  Result := (a1 > 90.0) or (a2 > 90.0) or (a3 > 90.0);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Point1,Point2,Point3:TPoint2D):Boolean;
begin
  Result := IsObtuseTriangle(Point1.x, Point1.y, Point2.x, Point2.y, Point3.x, Point3.y);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Point1,Point2,Point3:TPoint3D):Boolean;
begin
  Result := IsObtuseTriangle(Point1.x, Point1.y, Point1.z, Point2.x, Point2.y, Point2.z, Point3.x, Point3.y ,Point3.z);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Triangle:TTriangle2D):Boolean;
begin
  Result := IsObtuseTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsObtuseTriangle *)


function IsObtuseTriangle(const Triangle:TTriangle3D):Boolean;
begin
  Result := IsObtuseTriangle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of IsObtuseTriangle *)


function TriangleEdge(const Triangle:TTriangle2D; const Edge:Integer):TSegment2D;
begin
  case Edge of
    1: Result := EquateSegment(Triangle[1],Triangle[2]);
    2: Result := EquateSegment(Triangle[2],Triangle[3]);
    3: Result := EquateSegment(Triangle[3],Triangle[1]);
  end;
end;
(* Triangle Edge *)


function TriangleEdge(const Triangle:TTriangle3D; const Edge:Integer):TSegment3D;
begin
  case Edge of
    1: Result := EquateSegment(Triangle[1],Triangle[2]);
    2: Result := EquateSegment(Triangle[2],Triangle[3]);
    3: Result := EquateSegment(Triangle[3],Triangle[1]);
  end;
end;
(* Triangle Edge *)


function RectangleEdge(const Rectangle:TRectangle; const Edge:Integer):TSegment2D;
begin
  case Edge of
    1: Result := EquateSegment(Rectangle[1].x,Rectangle[1].y,Rectangle[1].x,Rectangle[1].y);
    2: Result := EquateSegment(Rectangle[2].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
    3: Result := EquateSegment(Rectangle[2].x,Rectangle[2].y,Rectangle[1].x,Rectangle[2].y);
    4: Result := EquateSegment(Rectangle[1].x,Rectangle[2].y,Rectangle[1].x,Rectangle[1].y);
  end;
end;
(* Rectangle Edge*)


function PointInTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
  Or3 : Integer;
begin
  Or1 := Orientation(x1,y1,x2,y2,Px,Py);
  Or2 := Orientation(x2,y2,x3,y3,Px,Py);

  if (Or1 * Or2) = -1 then
    Result := False
  else
  begin
    Or3 := Orientation(x3,y3,x1,y1,Px,Py);
    if (Or1 = Or3) or (Or3 = 0) then
      Result := True
    else if Or1 = 0 then
      Result := (Or2 * Or3) >= 0
    else if Or2 = 0 then
      Result := (Or1 * Or3) >= 0
    else
      Result := False;
  end;

  (*
    Note: The following code does the same as above,
         but is less efficient time-wise.
    Or1 := Orientation(x1,y1,x2,y2,Px,Py);
    Or2 := Orientation(x2,y2,x3,y3,Px,Py);
    Or3 := Orientation(x3,y3,x1,y1,Px,Py);

    if (Or1 = Or2) and (Or2 = Or3) then
     Result := True
    else if Or1 = 0 then
     Result := (Or2 * Or3) >= 0
    else if Or2 = 0 then
     Result := (Or1 * Or3) >= 0
    else if Or3 = 0 then
     Result := (Or1 * Or2) >= 0
    else
     Result := False;
  *)
end;
(* End of PointInTriangle *)


function PointInTriangle(const x,y:TFloat; const Triangle:TTriangle2D):Boolean;
begin
  Result := PointInTriangle(x,y,Triangle[1].x,Triangle[1].y,
                                Triangle[2].x,Triangle[2].y,
                                Triangle[3].x,Triangle[3].y);
end;
(* End of PointInTriangle *)


function PointInTriangle(const Point:TPoint2D; const Triangle:TTriangle2D):Boolean;
begin
  Result := PointInTriangle(Point.x,Point.y,Triangle[1].x,Triangle[1].y,
                                            Triangle[2].x,Triangle[2].y,
                                            Triangle[3].x,Triangle[3].y);
end;
(* End of PointInTriangle *)


function PointInCircle(const Px,Py,Cx,Cy,Radius:TFloat):Boolean;
begin
  Result := (LayDistance(Px,Py,Cx,Cy) <= (Radius * Radius));
end;
(* End of PointInCircle *)


function PointInCircle(const Px,Py:TFloat; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(Px,Py,Circle.x,Circle.y,Circle.Radius);
end;
(* End of PointInCircle *)


function PointInCircle(const Point:TPoint2D; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(Point.x,Point.y,Circle);
end;
(* End of PointInCircle *)


function PointOnCircle(const Px,Py:TFloat; const Circle:TCircle):Boolean;
begin
  Result := IsEqual(LayDistance(Px,Py,Circle.x,Circle.y),(Circle.Radius * Circle.Radius));
end;
(* End of PointInCircle *)


function PointOnCircle(const Point:TPoint2D; const Circle:TCircle):Boolean;
begin
  Result := PointOnCircle(Point.x,Point.y,Circle);
end;
(* End of PointInCircle *)


function TriangleInCircle(const Triangle:TTriangle2D; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(Triangle[1],Circle) and
            PointInCircle(Triangle[2],Circle) and
            PointInCircle(Triangle[3],Circle);
end;
(* End of TriangleInCircle *)


function TriangleOutsideCircle(const Triangle:TTriangle2D; const Circle:TCircle):Boolean;
begin
  Result := (not PointInCircle(Triangle[1],Circle)) and
            (not PointInCircle(Triangle[2],Circle)) and
            (not PointInCircle(Triangle[3],Circle));
end;
(* End of TriangleOutsideCircle *)


function TriangleEncompassesCircle(const Triangle:TTriangle2D; const Circle:TCircle):Boolean;
begin
  Result := TriangleOutsideCircle(Triangle,Circle) and
            PointInCircle(ClosestPointOnTriangleFromPoint(Triangle,Circle.x,Circle.y),Circle);
end;
(* End of TriangleEncompassesCircle *)


function RectangleInCircle(const Rectangle:TRectangle; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(Rectangle[1].x,Rectangle[1].y,Circle) and
            PointInCircle(Rectangle[2].x,Rectangle[2].y,Circle) and
            PointInCircle(Rectangle[1].x,Rectangle[2].y,Circle) and
            PointInCircle(Rectangle[2].x,Rectangle[1].y,Circle);
end;
(* End of RectangleInCircle *)


function RectangleOutsideCircle(const Rectangle:TRectangle; const Circle:TCircle):Boolean;
begin
  Result := (not PointInCircle(Rectangle[1].x,Rectangle[1].y,Circle)) and
            (not PointInCircle(Rectangle[2].x,Rectangle[2].y,Circle)) and
            (not PointInCircle(Rectangle[1].x,Rectangle[2].y,Circle)) and
            (not PointInCircle(Rectangle[2].x,Rectangle[1].y,Circle));
end;
(* End of RectangleInCircle *)


function QuadixInCircle(const Quadix:TQuadix2D; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(Quadix[1],Circle) and
            PointInCircle(Quadix[2],Circle) and
            PointInCircle(Quadix[3],Circle) and
            PointInCircle(Quadix[4],Circle);
end;
(* End of QuadixInCircle *)


function QuadixOutsideCircle(const Quadix:TQuadix2D; const Circle:TCircle):Boolean;
begin
  Result := (not PointInCircle(Quadix[1],Circle)) and
            (not PointInCircle(Quadix[2],Circle)) and
            (not PointInCircle(Quadix[3],Circle)) and
            (not PointInCircle(Quadix[4],Circle));
end;
(* End of QuadixInCircle *)


function PointInThreePointCircle(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat):Boolean;
var
  a11 : TFloat;
  a12 : TFloat;
  a21 : TFloat;
  a22 : TFloat;
  Dx1 : TFloat;
  Dx2 : TFloat;
  Dx3 : TFloat;
  Dy3 : TFloat;
  Dy1 : TFloat;
  Dy2 : TFloat;
begin
  Dx1 := x1 - Px;
  Dx2 := x2 - Px;
  Dx3 := x3 - Px;
  Dy1 := y2 - Py;
  Dy2 := y3 - Py;
  Dy3 := y1 - Py;

  a11 := Dx3 * Dy1 - Dx2 * Dy2;
  a12 := Dx3 * Dy3 - Dx1 * Dy2;
  a21 := Dx2 * (x2 - x3) + Dy1 * (y2 - y3);
  a22 := Dx1 * (x1 - x3) + Dy3 * (y1 - y3);

  Result := ((a11 * a22 - a21 * a12) <= 0);
end;
(* End of Point In Three Point Circle *)


function PointInThreePointCircle(const Point,Point1,Point2,Point3:TPoint2D):Boolean;
begin
  Result := PointInThreePointCircle(Point.x, Point.y,
                                    Point1.x,Point1.y,
                                    Point2.x,Point2.y,
                                    Point3.x,Point3.y);
end;
(* End of Point In Three Point Circle *)


function PointInThreePointCircle(const Point:TPoint2D; const Triangle:TTriangle2D):Boolean;
begin
  Result := PointInThreePointCircle(Point,Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Point In Three Point Circle *)


function PointInRectangle(const Px,Py:TFloat; const x1,y1,x2,y2:TFloat):Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or
            ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;
(* End of PointInRectangle *)


function PointInRectangle(const Point:TPoint2D; const x1,y1,x2,y2:TFloat):Boolean;
begin
  Result := PointInRectangle(Point.x,Point.y,x1,y1,x2,y2);
end;
(* End of PointInRectangle *)


function PointInRectangle(const Px,Py:TFloat; const Rectangle:TRectangle):Boolean;
begin
  Result := PointInRectangle(Px,Py,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of PointInRectangle *)


function PointInRectangle(const Point:TPoint2D; const Rectangle:TRectangle):Boolean;
begin
  Result := PointInRectangle(Point.x,Point.y,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of PointInRectangle *)


function TriangleInRectangle(const Triangle:TTriangle2D; const Rectangle:TRectangle):Boolean;
begin
  Result := PointInRectangle(Triangle[1],Rectangle) and
            PointInRectangle(Triangle[2],Rectangle) and
            PointInRectangle(Triangle[3],Rectangle);
end;
(* End of TriangleInRectangle *)

function TriangleOutsideRectangle(const Triangle:TTriangle2D; const Rectangle:TRectangle):Boolean;
begin
 Result := (not PointInRectangle(Triangle[1],Rectangle)) and
           (not PointInRectangle(Triangle[2],Rectangle)) and
           (not PointInRectangle(Triangle[3],Rectangle));
end;
(* End of TriangleInRectangle *)


function QuadixInRectangle(const Quadix:TQuadix2D; const Rectangle:TRectangle):Boolean;
begin
 Result := PointInRectangle(Quadix[1],Rectangle) and
           PointInRectangle(Quadix[2],Rectangle) and
           PointInRectangle(Quadix[3],Rectangle) and
           PointInRectangle(Quadix[4],Rectangle);
end;
(* End of QuadixInRectangle *)


function QuadixOutsideRectangle(const Quadix:TQuadix2D; const Rectangle:TRectangle):Boolean;
begin
 Result := (not PointInRectangle(Quadix[1],Rectangle)) and
           (not PointInRectangle(Quadix[2],Rectangle)) and
           (not PointInRectangle(Quadix[3],Rectangle)) and
           (not PointInRectangle(Quadix[4],Rectangle));
end;
(* End of QuadixOutsideRectangle *)


function PointInQuadix(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
var
  Or1 : Integer;
  Or2 : Integer;
  Or3 : Integer;
  Or4 : Integer;
begin
  Or1 := Orientation(x1,y1,x2,y2,Px,Py);
  Or2 := Orientation(x2,y2,x3,y3,Px,Py);
  Or3 := Orientation(x3,y3,x4,y4,Px,Py);
  Or4 := Orientation(x4,y4,x1,y1,Px,Py);

  if (Or1 = Or2) and (Or2 = Or3) and (Or3 = Or4) then
    Result := True
  else if Or1 = 0 then
    Result := (Or2 * Or4) = 0
  else if Or2 = 0 then
    Result := (Or1 * Or3) = 0
  else if Or3 = 0 then
    Result := (Or2 * Or4) = 0
  else if Or4 = 0 then
    Result := (Or1 * Or3) = 0
  else
    Result := False;
end;
(* End of PointInQuadix *)


function PointInQuadix(const Point,Point1,Point2,Point3,Point4:TPoint2D):Boolean;
begin
  Result := PointInQuadix(Point.x,Point.y,Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Point4.x,Point4.y);
end;
(* End of PointInQuadix *)


function PointInQuadix(const x,y:TFloat; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(x,y,Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y);
end;
(* End of PointInQuadix *)


function PointInQuadix(const Point:TPoint2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(Point,Quadix[1],Quadix[2],Quadix[3],Quadix[4]);
end;
(* End of PointInQuadix *)


function TriangleInQuadix(const Triangle:TTriangle2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(Triangle[1],Quadix) and
            PointInQuadix(Triangle[2],Quadix) and
            PointInQuadix(Triangle[3],Quadix);
end;
(* End of TriangleInQuadix *)


function TriangleOutsideQuadix(const Triangle:TTriangle2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := (not PointInQuadix(Triangle[1],Quadix)) and
            (not PointInQuadix(Triangle[2],Quadix)) and
            (not PointInQuadix(Triangle[3],Quadix));
end;
(* End of TriangleInQuadix *)


function PointInSphere(const x,y,z:TFloat; const Sphere:TSphere):Boolean;
begin
  Result := (LayDistance(x,y,z,Sphere.z,Sphere.y,Sphere.z) <= (Sphere.Radius * Sphere.Radius));
end;
(* End of PointInSphere *)


function PointInSphere(const Point3D:TPoint3D; const Sphere:TSphere):Boolean;
begin
  Result := PointInSphere(Point3D.x,Point3D.y,Point3D.z,Sphere);
end;
(* End of PointInSphere *)


function PointOnSphere(const Point3D:TPoint3D; const Sphere:TSphere):Boolean;
begin
  Result := IsEqual(LayDistance(Point3D.x,Point3D.y,Point3D.z,Sphere.z,Sphere.y,Sphere.z),(Sphere.Radius * Sphere.Radius));
end;
(* End of PointOnSphere *)


function PolyhedronInSphere(const Polygon:TPolyhedron; const Sphere:TSphere):TInclusion;
var
  i         : Integer;
  j         : Integer;
  Count     : Integer;
  RealCount : Integer;
begin
  RealCount := 0;
  Count     := 0;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Inc(RealCount,Length(Polygon[i]));
    for j := 0 to Length(Polygon[i]) - 1 do
    if PointInSphere(Polygon[i][j],Sphere) then
      Inc(Count);
  end;
  Result := ePartially;
  if Count = 0 then
    Result := eOutside
  else if Count = RealCount then
    Result:= eFully;
end;
(* End of PolyhedronInSphere *)


function PointOnPerimeter(const Px,Py,x1,y1,x2,y2:TFloat):Boolean;
begin
 //Result := (((Px = x1) or (Px = x2)) and ((Py = y1) or (Py = y2)));
  Result := (
             ((IsEqual(Px,x1) or IsEqual(Px,x2)) and ((Py >= y1) and (Py <= y2))) or
             ((IsEqual(Py,y1) or IsEqual(Py,y2)) and ((Px >= x1) and (Px <= x2)))
            );
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat; Robust:Boolean = false):Boolean;
begin
  Result := (
             IsPointCollinear(x1,y1,x2,y2,Px,Py,Robust) or
             IsPointCollinear(x2,y2,x3,y3,Px,Py,Robust) or
             IsPointCollinear(x3,y3,x1,y1,Px,Py,Robust)
            );
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Px,Py,x1,y1,x2,y2,x3,y3,x4,y4:TFloat):Boolean;
begin
  Result := (
             IsPointCollinear(x1,y1,x2,y2,Px,Py) or
             IsPointCollinear(x2,y2,x3,y3,Px,Py) or
             IsPointCollinear(x3,y3,x4,y4,Px,Py) or
             IsPointCollinear(x4,y4,x1,y1,Px,Py)
            );
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TPoint2D; const Rectangle:TRectangle):Boolean;
begin
  Result := PointOnPerimeter(Point.x,Point.y,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TPoint2D; const Triangle:TTriangle2D):Boolean;
begin
  Result := PointOnPerimeter(Point.x,Point.y,Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TPoint2D; const Quadix: TQuadix2D):Boolean;
begin
  Result := PointOnPerimeter(Point.x,Point.y,Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TPoint2D; const Circle: TCircle):Boolean;
begin
  //Result := (LayDistance(Point.x,Point.y,Circle.x,Circle.y) = (Circle.Radius * Circle.Radius));
  Result := IsEqual(LayDistance(Point.x,Point.y,Circle.x,Circle.y),(Circle.Radius * Circle.Radius));
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TPoint3D; const Sphere: TSphere):Boolean;
begin
  //Result := (LayDistance(Point.x,Point.y,Point.z,Sphr.x,Sphr.y,Sphr.z) = (Sphr.Radius * Sphr.Radius));
  Result := IsEqual(LayDistance(Point.x,Point.y,Point.z,Sphere.x,Sphere.y,Sphere.z),(Sphere.Radius * Sphere.Radius));
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point:TPoint2D; const Polygon : TPolygon2D):Boolean;
begin
  Result := PointOnPolygon(Point.x,Point.y,Polygon);
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TPoint2D; const Obj:TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoRectangle  : Result := PointOnPerimeter(Point,Obj.Rectangle );
    geoTriangle2D : Result := PointOnPerimeter(Point,Obj.Triangle2D);
    geoQuadix2D   : Result := PointOnPerimeter(Point,Obj.Quadix2D  );
    geoCircle     : Result := PointOnPerimeter(Point,Obj.Circle    );
  else
    Result := False;
  end;
end;
(* End of PointOnPerimeter *)


function PointOnPerimeter(const Point: TPoint3D; const Obj:TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoSphere  : Result := PointOnPerimeter(Point,Obj.Sphere);
  else
    Result := False;
  end;
end;
(* End of PointOnPerimeter *)


function PointInObject(const Point:TPoint2D; const Segment:TSegment2D):Boolean;
begin
  Result := IsPointCollinear(Segment,Point);
end;
(* End of Point In Object *)


function PointInObject(const Point:TPoint2D; const Line:TLine2D):Boolean;
begin
  Result := Collinear(Line[1],Line[2],Point);
end;
(* End of Point In Object *)


function PointInObject(const Point:TPoint2D; const Rectangle:TRectangle):Boolean;
begin
  Result := PointInRectangle(Point,Rectangle);
end;
(* End of Point In Object *)


function PointInObject(const Point:TPoint2D; const Triangle:TTriangle2D):Boolean;
begin
  Result := PointInTriangle(Point,Triangle);
end;
(* End of Point In Object *)


function PointInObject(const Point:TPoint2D; const Quadix:TQuadix2D):Boolean;
begin
  Result := PointInQuadix(Point,Quadix);
end;
(* End of Point In Object *)


function PointInObject(const Point:TPoint2D; const Circle:TCircle):Boolean;
begin
  Result := PointInCircle(Point,Circle);
end;
(* End of Point In Object *)


function PointInObject(const Point:TPoint2D; const Polygon:TPolygon2D ):Boolean;
begin
  Result := PointInPolygon(Point,Polygon);
end;
(* End of Point In Object *)


function PointInObject(const Point:TPoint2D; const Obj : TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoSegment2D  : Result := PointInObject(Point,Obj.Segment2D);
    geoLine2D     : Result := PointInObject(Point,Obj.Line2D);
    geoRectangle  : Result := PointInObject(Point,Obj.Rectangle);
    geoTriangle2D : Result := PointInObject(Point,Obj.Triangle2D);
    geoQuadix2D   : Result := PointInObject(Point,Obj.Quadix2D);
    geoCircle     : Result := PointInObject(Point,Obj.Circle);
    geoPolygon2D  : Result := PointInObject(Point,Obj.Polygon2D^);
  else
    Result := False;
  end;
end;
(* End of Point In Object *)


function GeometricSpan(const Point: array of TPoint2D):TFloat;
var
  TempDistance : TFloat;
  i            : Integer;
  j            : Integer;
begin
  Result := -1;
  for i := 0 to Length(Point) - 2 do
  begin
    for j:= (i + 1) to Length(Point) - 1 do
    begin
      TempDistance := LayDistance(Point[i],Point[j]);
      if TempDistance > Result then
        Result := TempDistance;
    end;
  end;
  Result := Sqrt(Result);
end;
(* End of 2D Geometric Span *)


function GeometricSpan(const Point: array of TPoint3D):TFloat;
var
  TempDistance : TFloat;
  i            : Integer;
  j            : Integer;
begin
  Result := -1;
  if Length(Point) < 2 then Exit;
  for i := 0 to Length(Point) - 2 do
  begin
    for j := (i + 1) to Length(Point) - 1 do
    begin
      TempDistance := LayDistance(Point[I],Point[J]);
      if TempDistance > Result then
        Result := TempDistance;
    end;
  end;
  Result := Sqrt(Result);
end;
(* End of 3D Geometric Span *)


procedure CreateEquilateralTriangle(x1,y1,x2,y2:TFloat; out x3,y3:TFloat);
const Sin60 : TFloat = 0.86602540378443864676372317075294;
const Cos60 : TFloat = 0.50000000000000000000000000000000;
begin
  (* Translate for x1,y1 to be origin *)
  x2 := x2 - x1;
  y2 := y2 - y1;
  (* Rotate 60 degrees and translate back *)
  x3 := ((x2 * Cos60) - (y2 * Sin60)) + x1;
  y3 := ((y2 * Cos60) + (x2 * Sin60)) + y1;
end;
(* End of Create Equilateral Triangle *)


procedure CreateEquilateralTriangle(const Point1,Point2:TPoint2D; out Point3:TPoint2D);
begin
  CreateEquilateralTriangle(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const x1,y1,x2,y2:TFloat):TTriangle2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
  CreateEquilateralTriangle(x1,y1,x2,y2,Result[3].x,Result[3].y);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const Point1,Point2:TPoint2D):TTriangle2D;
begin
  Result[1] := Point1;
  Result[2] := Point2;
  CreateEquilateralTriangle(Result[1],Result[2],Result[3]);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const Cx,Cy,SideLength : TFloat) : TTriangle2D;
begin
  Result := CenterAtLocation(CreateEquilateralTriangle(-SideLength * 0.5 , 0.0, SideLength * 0.5, 0.0),Cx,Cy);
end;
(* End of Create Equilateral Triangle *)


function CreateEquilateralTriangle(const CenterPoint : TPoint2D; const SideLength : TFloat) : TTriangle2D;
begin
  Result := CreateEquilateralTriangle(CenterPoint.x, CenterPoint.y, SideLength);
end;
(* End of Create Equilateral Triangle *)


procedure TorricelliPoint(const x1,y1,x2,y2,x3,y3:TFloat; out Px,Py:TFloat);
var
  OETx1 : TFloat;
  OETy1 : TFloat;
  OETx2 : TFloat;
  OETy2 : TFloat;
begin
  (*
    Proven by Cavalieri in this book "Exercitationes geometricae" 1647.
    The theory goes, if the triangle has an angle of 120 degrees or more
    the toricelli point lies at the vertex of the large angle. Otherwise
    the point a which the Simpson lines intersect is said to be the optimal
    solution.
    to find an intersection in 2D, all that is needed is 2 lines (segments),
    hence not all three of the Simpson lines are calculated.
  *)
  //if VertexAngle(x1,y1,x2,y2,x3,y3) >= 120.0 then
  if GreaterThanOrEqual(VertexAngle(x1,y1,x2,y2,x3,y3),120.0) then
  begin
    Px := x2;
    Py := y2;
    Exit;
  end
  //else if VertexAngle(x3,y3,x1,y1,x2,y2) >= 120.0 then
  else if GreaterThanOrEqual(VertexAngle(x3,y3,x1,y1,x2,y2),120.0) then
  begin
    Px := x1;
    Py := y1;
    Exit;
  end
  //else if VertexAngle(x2,y2,x3,y3,x1,y1) >= 120.0 then
  else if GreaterThanOrEqual(VertexAngle(x2,y2,x3,y3,x1,y1),120.0) then
  begin
    Px := x3;
    Py := y3;
    Exit;
  end
  else
  begin
    if Orientation(x1,y1,x2,y2,x3,y3) = RightHandSide then
    begin
      CreateEquilateralTriangle(x1,y1,x2,y2,OETx1,OETy1);
      CreateEquilateralTriangle(x2,y2,x3,y3,OETx2,OETy2);
    end
    else
    begin
      CreateEquilateralTriangle(x2,y2,x1,y1,OETx1,OETy1);
      CreateEquilateralTriangle(x3,y3,x2,y2,OETx2,OETy2);
    end;
    IntersectionPoint(OETx1,OETy1,x3,y3,OETx2,OETy2,x1,y1,Px,Py);
  end;
end;
(* End of Create Torricelli Point *)


function TorricelliPoint(const Point1,Point2,Point3:TPoint2D):TPoint2D;
begin
  TorricelliPoint(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Result.x,Result.y);
end;
(* End of Create Torricelli Point *)


function TorricelliPoint(const Triangle:TTriangle2D):TPoint2D;
begin
  Result := TorricelliPoint(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Create Torricelli Point *)


procedure Incenter(const x1,y1,x2,y2,x3,y3:TFloat; out Px,Py:TFloat);
var
  Perim  : TFloat;
  Side12 : TFloat;
  Side23 : TFloat;
  Side31 : TFloat;
begin
  Side12 := Distance(x1,y1,x2,y2);
  Side23 := Distance(x2,y2,x3,y3);
  Side31 := Distance(x3,y3,x1,y1);

  (* Using Heron's S=UR *)
  Perim  := 1.0 / (Side12 + Side23 + Side31);
  Px     := (Side23 * x1 + Side31 * x2 + Side12 * x3) * Perim;
  Py     := (Side23 * y1 + Side31 * y2 + Side12 * y3) * Perim;
end;
(* End of Incenter *)


procedure Incenter(const Triangle:TTriangle2D; out Px,Py:TFloat);
begin
  Incenter(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Px,Py);
end;
(* End of Incenter *)


function Incenter(const Point1,Point2,Point3:TPoint2D):TPoint2D;
begin
  Incenter(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Result.x,Result.y);
end;
(* End of Incenter *)


function Incenter(const Triangle:TTriangle2D):TPoint2D;
begin
  Incenter(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Result.x,Result.y);
end;
(* End of Incenter *)


procedure Circumcenter(const x1,y1,x2,y2,x3,y3:TFloat; out Px,Py:TFloat);
var
  A : TFloat;
  C : TFloat;
  B : TFloat;
  D : TFloat;
  E : TFloat;
  F : TFloat;
  G : TFloat;
begin
  A := x2 - x1;
  B := y2 - y1;
  C := x3 - x1;
  D := y3 - y1;
  E := A * (x1 + x2) + B * (y1 + y2);
  F := C * (x1 + x3) + D * (y1 + y3);
  G := 2.0 * (A * (y3 - y2) - B * (x3 - x2));
  if IsEqual(G,0.0) then Exit;
  Px := (D * E - B * F) / G;
  Py := (A * F - C * E) / G;
end;
(* End of Circumcenter *)


function Circumcenter(const Point1,Point2,Point3:TPoint2D):TPoint2D;
begin
  Circumcenter(Point1.x,Point1.y,Point2.x,Point2.y,Point3.x,Point3.y,Result.x,Result.y);
end;
(* End of Circumcenter *)


function Circumcenter(const Triangle:TTriangle2D):TPoint2D;
begin
  Circumcenter(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Result.x,Result.y);
end;
(* End of Circumcenter *)


function Circumcircle(const P1,P2,P3:TPoint2D):TCircle;
begin
  Circumcenter(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,Result.x,Result.y);
  Result.Radius := Distance(P1.x,P1.y,Result.x,Result.y);
end;
(* End of TriangleCircumCircle *)


function Circumcircle(const Triangle:TTriangle2D):TCircle;
begin
  Result := CircumCircle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of TriangleCircumCircle *)


function InscribedCircle(const x1,y1,x2,y2,x3,y3:TFloat):TCircle;
var
  Perimeter : TFloat;
  Side12    : TFloat;
  Side23    : TFloat;
  Side31    : TFloat;
begin
  Side12 := Distance(x1,y1,x2,y2);
  Side23 := Distance(x2,y2,x3,y3);
  Side31 := Distance(x3,y3,x1,y1);

  (* Using Heron's S = UR *)
  Perimeter     := 1.0 / (Side12 + Side23 + Side31);
  Result.x      := (Side23 * x1 + Side31 * x2 + Side12 * x3) * Perimeter;
  Result.y      := (Side23 * y1 + Side31 * y2 + Side12 * y3) * Perimeter;
  Result.Radius := 0.5 * sqrt((-Side12 + Side23 + Side31) * (Side12 - Side23 + Side31) * (Side12 + Side23 - Side31) * Perimeter);
end;
(* End of InscribedCircle *)


function InscribedCircle(const P1,P2,P3:TPoint2D):TCircle;
begin
  Result := InscribedCircle(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y);
end;
(* End of InscribedCircle *)


function InscribedCircle(const Triangle:TTriangle2D):TCircle;
begin
  Result := InscribedCircle(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of InscribedCircle *)


procedure ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TFloat; out Nx,Ny:TFloat);
var
  Vx    : TFloat;
  Vy    : TFloat;
  Wx    : TFloat;
  Wy    : TFloat;
  c1    : TFloat;
  c2    : TFloat;
  Ratio : TFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;

  if c1 <= 0 then
  begin
    Nx := x1;
    Ny := y1;
    Exit;
  end;

  c2 := Vx * Vx + Vy * Vy;

  if c2 <= c1  then
  begin
    Nx := x2;
    Ny := y2;
    Exit;
  end;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;
(* End of ClosestPointOnSegmentFromPoint *)


procedure ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat; out Nx,Ny,Nz:TFloat);
var
  Vx    : TFloat;
  Vy    : TFloat;
  Vz    : TFloat;
  Wx    : TFloat;
  Wy    : TFloat;
  wz    : TFloat;
  c1    : TFloat;
  c2    : TFloat;
  Ratio : TFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Vz := z2 - z1;
  Wx := Px - x1;
  Wy := Py - y1;
  wz := Pz - z1;

  c1 := Vx * Wx + Vy * Wy + Vz * Wz;

  if c1 <= 0 then
  begin
    Nx := x1;
    Ny := y1;
    Nz := z1;
    Exit;
  end;

  c2 := Vx * Vx + Vy * Vy + Vz * Vz;

  if c2 <= c1  then
  begin
    Nx := x2;
    Ny := y2;
    Nz := z2;
    Exit;
  end;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
  Nz := z1 + Ratio * Vz;
end;
(* End of ClosestPointOnSegmentFromPoint *)


procedure ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TFloat; out Nx,Ny:TFloat);
var
  Vx    : TFloat;
  Vy    : TFloat;
  Wx    : TFloat;
  Wy    : TFloat;
  c1    : TFloat;
  c2    : TFloat;
  Ratio : TFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;
  c2 := Vx * Vx + Vy * Vy;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;
(* End of ClosestPointOnLineFromPoint *)


procedure ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat; out Nx,Ny,Nz:TFloat);
var
  Vx    : TFloat;
  Vy    : TFloat;
  Vz    : TFloat;
  Wx    : TFloat;
  Wy    : TFloat;
  wz    : TFloat;
  c1    : TFloat;
  c2    : TFloat;
  Ratio : TFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Vz := z2 - z1;
  Wx := Px - x1;
  Wy := Py - y1;
  wz := Pz - z1;

  c1 := Vx * Wx + Vy * Wy + Vz * Wz;
  c2 := Vx * Vx + Vy * Vy + Vz * Vz;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
  Nz := z1 + Ratio * Vz;
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnSegmentFromPoint(const x1,y1,x2,y2,Px,Py:TFloat):TPoint2D;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Px,Py,Result.x,Result.y);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnSegmentFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat):TPoint3D;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Result.x,Result.y,Result.z);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnSegmentFromPoint(const Segment:TSegment2D; const Point:TPoint2D):TPoint2D;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Point.x,Point.y,Result.x,Result.y);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnSegmentFromPoint(const Segment:TSegment3D; const Point:TPoint3D):TPoint3D;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z,Point.x,Point.y,Point.z,Result.x,Result.y,Result.Z);
end;
(* End of ClosestPointOnSegmentFromPoint *)


function ClosestPointOnLineFromPoint(const x1,y1,x2,y2,Px,Py:TFloat):TPoint2D;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Result.x,Result.y)
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnLineFromPoint(const x1,y1,z1,x2,y2,z2,Px,Py,Pz:TFloat):TPoint3D;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Result.x,Result.y,Result.z);
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnLineFromPoint(const Line:TLine2D; const Point:TPoint2D):TPoint2D;
begin
  ClosestPointOnLineFromPoint(Line[1].x,Line[1].y,Line[2].x,Line[2].y,Point.x,Point.y,Result.x,Result.y);
end;
(* End of ClosestPointOnLineFromPoint *)


function ClosestPointOnLineFromPoint(const Line:TLine3D; const Point:TPoint3D):TPoint3D;
begin
  ClosestPointOnLineFromPoint(Line[1].x,Line[1].y,Line[1].z,Line[2].x,Line[2].y,Line[2].z,Point.x,Point.y,Point.z,Result.x,Result.y,Result.Z);
end;
(* End of ClosestPointOnLineFromPoint *)


procedure ClosestPointOnTriangleFromPoint(const x1,y1,x2,y2,x3,y3,Px,Py:TFloat; out Nx,Ny : TFloat);
begin
  if Orientation(x1,y1,x2,y2,Px,Py) <> Orientation(x1,y1,x2,y2,x3,y3) then
  begin
    ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
    Exit;
  end;

  if Orientation(x2,y2,x3,y3,Px,Py) <> Orientation(x2,y2,x3,y3,x1,y1) then
  begin
    ClosestPointOnSegmentFromPoint(x2,y2,x3,y3,Px,Py,Nx,Ny);
    Exit;
  end;

  if Orientation(x3,y3,x1,y1,Px,Py) <> Orientation(x3,y3,x1,y1,x2,y2) then
  begin
    ClosestPointOnSegmentFromPoint(x3,y3,x1,y1,Px,Py,Nx,Ny);
    Exit;
  end;
  Nx := Px;
  Ny := Py;
end;
(* Closest Point On Triangle 2D From Point *)


function ClosestPointOnTriangleFromPoint(const Triangle:TTriangle2D; const Px,Py:TFloat):TPoint2D;
begin
  ClosestPointOnTriangleFromPoint(Triangle[1].x, Triangle[1].y,
                                  Triangle[2].x, Triangle[2].y,
                                  Triangle[3].x, Triangle[3].y,
                                  Px           , Py           ,
                                  Result.x     , Result.y
                                 );
end;
(* Closest Point On Triangle 2D From Point *)


function ClosestPointOnTriangleFromPoint(const Triangle:TTriangle2D; const Point:TPoint2D):TPoint2D;
begin
  Result := ClosestPointOnTriangleFromPoint(Triangle,Point.x,Point.y);
end;
(* Closest Point On Triangle 2D From Point *)


function ClosestPointOnTriangleFromPoint(const Triangle:TTriangle3D; const Point:TPoint3D):TPoint3D;
var
  Leydist1 : TFloat;
  Leydist2 : TFloat;
  Leydist3 : TFloat;
begin
  Leydist1 := LayDistance(Triangle[1],Point);
  Leydist2 := LayDistance(Triangle[2],Point);
  Leydist3 := LayDistance(Triangle[3],Point);

  (* Edge 3 is the longest *)
  if GreaterThanOrEqual(Leydist3,Leydist1) and GreaterThanOrEqual(Leydist3,Leydist2) then
  begin
    ClosestPointOnSegmentFromPoint(Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Point.x,Point.y,Result.x,Result.y);
    Exit;
  end;

  (* Edge 2 is the longest *)
  if GreaterThanOrEqual(Leydist2,Leydist1) and GreaterThanOrEqual(Leydist2,Leydist3) then
  begin
    ClosestPointOnSegmentFromPoint(Triangle[1].x,Triangle[1].y,Triangle[3].x,Triangle[3].y,Point.x,Point.y,Result.x,Result.y);
    Exit;
  end;

  (* Edge 1 is the longest *)
  if GreaterThanOrEqual(Leydist1,Leydist2) and GreaterThanOrEqual(Leydist1,Leydist3) then
  begin
    ClosestPointOnSegmentFromPoint(Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,Point.x,Point.y,Result.x,Result.y);
    Exit;
  end;

end;
(* Closest Point On Triangle 3D From Point *)


procedure ClosestPointOnRectangleFromPoint(const x1,y1,x2,y2,Px,Py:TFloat; out Nx,Ny:TFloat);
begin
  if (Px < Min(x1,x2)) then
    Nx := Min(x1,x2)
  else if (Px > Max(x1,x2)) then
    Nx := Max(x1,x2)
  else
    Nx := Px;

  if (Py < Min(y1,y2)) then
    Ny := Min(y1,y2)
  else if (Py > Max(y1,y2)) then
    Ny := Max(y1,y2)
  else
    Ny := Py;
end;
(* Closest Point On Rectangle From Point *)


function ClosestPointOnRectangleFromPoint(const Rectangle:TRectangle; const Px,Py:TFloat):TPoint2D;
begin
  ClosestPointOnRectangleFromPoint(Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y,Px,Py,Result.x,Result.y);
end;
(* Closest Point On Rectangle From Point *)


function ClosestPointOnRectangleFromPoint(const Rectangle:TRectangle; const Point:TPoint2D):TPoint2D;
begin
  Result := ClosestPointOnRectangleFromPoint(Rectangle,Point.x,Point.y);
end;
(* Closest Point On Rectangle From Point *)


function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix2D; const Point:TPoint2D):TPoint2D;
var
  MinDist   : TFloat;
  TempDist  : TFloat;
  TempPoint : TPoint2D;
begin
  if PointInQuadix(Point,Quadix) then
  begin
    Result := Point;
    Exit;
  end;

  ClosestPointOnSegmentFromPoint(Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,Point.x,Point.y,Result.x,Result.y);
  MinDist := Distance(Result,Point);

  ClosestPointOnSegmentFromPoint(Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,Point.x,Point.y,TempPoint.x,TempPoint.y);
  TempDist := Distance(TempPoint,Point);

  if MinDist > TempDist then
  begin
    MinDist := TempDist;
    Result  := TempPoint;
  end;

  ClosestPointOnSegmentFromPoint(Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y,Point.x,Point.y,TempPoint.x,TempPoint.y);
  TempDist := Distance(TempPoint,Point);

  if MinDist > TempDist then
  begin
    MinDist := TempDist;
    Result  := TempPoint;
  end;

  ClosestPointOnSegmentFromPoint(Quadix[4].x,Quadix[4].y,Quadix[1].x,Quadix[1].y,Point.x,Point.y,TempPoint.x,TempPoint.y);
  TempDist := Distance(TempPoint,Point);

  if MinDist > TempDist then
  begin
    Result  := TempPoint;
  end;
end;
(* Closest Point On Quadix 2D From Point *)


function ClosestPointOnQuadixFromPoint(const Quadix:TQuadix3D; const Point:TPoint3D):TPoint3D;
(*
var
  Leydist1 : TFloat;
  Leydist2 : TFloat;
  Leydist3 : TFloat;
  Leydist4 : TFloat;
*)
begin
 (*
  Leydist1 := LayDistance(Quadix[1],Point);
  Leydist2 := LayDistance(Quadix[2],Point);
  Leydist3 := LayDistance(Quadix[3],Point);
  Leydist4 := LayDistance(Quadix[4],Point);
 *)
end;
(* Closest Point On Quadix 3D From Point *)


function ClosestPointOnCircleFromPoint(const Circle:TCircle; const Point:TPoint2D):TPoint2D;
var
  Ratio : TFloat;
  dx    : TFloat;
  dy    : TFloat;
begin
  dx       := Point.x - Circle.x;
  dy       := Point.y - Circle.y;
  Ratio    := Circle.Radius / sqrt(dx * dx + dy * dy);
  Result.x := Circle.x + Ratio * dx;
  Result.y := Circle.y + Ratio * dy;
end;
(* Closest Point On Circle From Point *)


function ClosestPointOnSphereFromPoint(const Sphere:TSphere; const Point:TPoint3D):TPoint3D;
var
  Ratio : TFloat;
  dx    : TFloat;
  dy    : TFloat;
  dz    : TFloat;
begin
  dx       := Point.x - Sphere.x;
  dy       := Point.y - Sphere.y;
  dz       := Point.z - Sphere.z;
  Ratio    := Sphere.Radius / sqrt(dx * dx + dy * dy + dz * dz);
  Result.x := Sphere.x + Ratio * dx;
  Result.y := Sphere.y + Ratio * dy;
  Result.z := Sphere.z + Ratio * dz;
end;
(* Closest Point On Sphere From Point *)


function ClosestPointOnAABBFromPoint(const Rectangle: TRectangle; const Point:TPoint2D):TPoint2D;
begin
  Result := Point;
  if Point.x <= Rectangle[1].x then
    Result.x := Rectangle[1].x
  else if Point.x >= Rectangle[2].x then
    Result.x := Rectangle[2].x;

  if Point.y <= Rectangle[1].y then
    Result.y := Rectangle[1].y
  else if Point.y >= Rectangle[2].y then
    Result.y := Rectangle[2].y;
end;
(* Closest Point On AABB From Point *)


function ClosestPointOnCircleFromSegment(const Circle:TCircle; Segment:TSegment2D):TPoint2D;
var
  Nx    : TFloat;
  Ny    : TFloat;
  Ratio : TFloat;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Circle.x,Circle.y,Nx,Ny);
  Ratio    := Circle.Radius / Distance(Circle.x,Circle.y,Nx,Ny);
  Result.x := Circle.x + Ratio * (Nx - Circle.x);
  Result.y := Circle.y + Ratio * (Ny - Circle.y);
end;
(* End of ClosestPointOnCircle *)


function ClosestPointOnSphereFromSegment(const Sphere:TSphere; Segment:TSegment3D):TPoint3D;
var
  Nx    : TFloat;
  Ny    : TFloat;
  Nz    : TFloat;
  Ratio : TFloat;
begin
  ClosestPointOnSegmentFromPoint(Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z,Sphere.x,Sphere.y,Sphere.z,Nx,Ny,Nz);
  Ratio    := Sphere.Radius / Distance(Sphere.x,Sphere.y,Sphere.z,Nx,Ny,Nz);
  Result.x := Sphere.x + Ratio * (Nx - Sphere.x);
  Result.y := Sphere.y + Ratio * (Ny - Sphere.y);
  Result.z := Sphere.z + Ratio * (Nz - Sphere.z);
end;
(* End of ClosestPointOnCircle *)


function MinimumDistanceFromPointToSegment(const Px,Py,x1,y1,x2,y2:TFloat):TFloat;
var
  Nx    : TFloat;
  Ny    : TFloat;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToSegment(const Point:TPoint2D; const Segment:TSegment2D):TFloat;
begin
  Result := MinimumDistanceFromPointToSegment(Point.x,Point.y,Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToSegment(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TFloat):TFloat;
var
  Nx    : TFloat;
  Ny    : TFloat;
  Nz    : TFloat;
begin
  ClosestPointOnSegmentFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Nx,Ny,Nz);
  Result := Distance(Px,Py,Pz,Nx,Ny,Nz);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToSegment(const Point:TPoint3D; const Segment:TSegment3D):TFloat;
begin
  Result := MinimumDistanceFromPointToSegment(Point.x,Point.y,Point.z,Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z);
end;
(* End of Minimum Distance From Point to Segment *)


function MinimumDistanceFromPointToLine(const Px,Py,x1,y1,x2,y2:TFloat):TFloat;
var
  Nx : TFloat;
  Ny : TFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToLine(const Point:TPoint2D; const Line:TLine2D):TFloat;
begin
  Result := MinimumDistanceFromPointToLine(Point.x,Point.y, Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToLine(const Px,Py,Pz,x1,y1,z1,x2,y2,z2:TFloat):TFloat;
var
  Nx : TFloat;
  Ny : TFloat;
  Nz : TFloat;
begin
  ClosestPointOnLineFromPoint(x1,y1,z1,x2,y2,z2,Px,Py,Pz,Nx,Ny,Nz);
  Result := Distance(Px,Py,Pz,Nx,Ny,Nz);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToLine(const Point:TPoint3D; const Line:TLine3D):TFloat;
begin
  Result := MinimumDistanceFromPointToLine(Point.x,Point.y,Point.z,Line[1].x,Line[1].y,Line[1].z,Line[2].x,Line[2].y,Line[2].z);
end;
(* End of Minimum Distance From Point to Line *)


function MinimumDistanceFromPointToTriangle(const Px,Py,x1,y1,x2,y2,x3,y3:TFloat):TFloat;
var
  Nx : TFloat;
  Ny : TFloat;
begin
  ClosestPointOnTriangleFromPoint(x1,y1,x2,y2,x3,y3,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Triangle *)


function MinimumDistanceFromPointToTriangle(const Point:TPoint2D; const Triangle:TTriangle2D):TFloat;
begin
  Result := MinimumDistanceFromPointToTriangle(Point.x,Point.y, Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y);
end;
(* End of Minimum Distance From Point to Triangle *)


function MinimumDistanceFromPointToRectangle(const Px,Py,x1,y1,x2,y2:TFloat):TFloat;
var
  Nx : TFloat;
  Ny : TFloat;
begin
  ClosestPointOnRectangleFromPoint(x1,y1,x2,y2,Px,Py,Nx,Ny);
  Result := Distance(Px,Py,Nx,Ny);
end;
(* End of Minimum Distance From Point to Rectangle *)


function MinimumDistanceFromPointToRectangle(const Point:TPoint2D; const Rectangle:TRectangle):TFloat;
begin
  Result := MinimumDistanceFromPointToRectangle(Point.x,Point.y,Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y);
end;
(* End of Minimum Distance From Point to Rectangle *)


function MinimumDistanceFromPointToPolygon(const Point:TPoint2D; const Polygon:TPolygon2D):TFloat;
var
  i        : Integer;
  j        : Integer;
  TempDist : TFloat;
begin
  Result := 0.0;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  i := 0;
  Result := MinimumDistanceFromPointToSegment(Point.x,Point.y,Polygon[i].x,Polygon[i].y,Polygon[j].x,Polygon[j].y);
  j := 0;
  for i := 1 to Length(Polygon) - 1 do
  begin
    TempDist := MinimumDistanceFromPointToSegment(Point.x,Point.y,Polygon[i].x,Polygon[i].y,Polygon[j].x,Polygon[j].y);
    if TempDist < Result then
      Result := TempDist;
    j := i;
  end;
end;
(* End of Minimum Distance From Point to Line *)


procedure SegmentMidPoint(const x1,y1,x2,y2:TFloat; out midx, midy:TFloat);
begin
  midx := (x1 + x2) * 0.5;
  midy := (y1 + y2) * 0.5;
end;
(* End of SegmentMidPoint *)


procedure SegmentMidPoint(const Segment:TSegment2D; out midx, midy:TFloat);
begin
  SegmentMidPoint(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,midx,midy);
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const P1,P2:TPoint2D):TPoint2D;
begin
  SegmentMidPoint(P1.x,P1.y,P2.x,P2.y,Result.x,Result.y);
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const Segment:TSegment2D):TPoint2D;
begin
  Result := SegmentMidPoint(Segment[1],Segment[2]);
end;
(* End of SegmentMidPoint *)


procedure SegmentMidPoint(const x1,y1,z1,x2,y2,z2:TFloat; out midx, midy ,midz:TFloat);
begin
  midx := (x1 + x2) * 0.5;
  midy := (y1 + y2) * 0.5;
  midz := (z1 + z2) * 0.5;
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const P1,P2:TPoint3D):TPoint3D;
begin
  SegmentMidpoint(P1.x,P1.y,P1.z,P2.x,P2.y,P2.z,Result.x,Result.y,Result.z);
end;
(* End of SegmentMidPoint *)


function SegmentMidPoint(const Segment:TSegment3D):TPoint3D;
begin
 Result := SegmentMidPoint(Segment[1],Segment[2]);
end;
(* End of SegmentMidPoint *)


procedure Centroid(const x1,y1,x2,y2:TFloat; out x,y:TFloat);
begin
  x := (x1 + x2) * 0.5;
  y := (y1 + y2) * 0.5;
end;
(* End of Centroid *)


function Centroid(const P1,P2:TPoint2D):TPoint2D;
begin
  Centroid(P1.x,P1.y,P2.x,P2.y,Result.x,Result.y);
end;
(* End of Centroid *)


function Centroid(const Segment:TSegment2D):TPoint2D;
begin
  Result := Centroid(Segment[1],Segment[2]);
end;
(* End of Centroid *)


procedure Centroid(const x1,y1,x2,y2,x3,y3:TFloat; out x,y:TFloat);
var
  midx1 : TFloat;
  midy1 : TFloat;
  midx2 : TFloat;
  midy2 : TFloat;
begin
  SegmentMidPoint(x2,y2,x3,y3,midx1,midy1);
  SegmentMidPoint(x1,y1,x3,y3,midx2,midy2);
  Intersect(x1,y1,midx1,midy1,x2,y2,midx2,midy2,x,y);
end;
(* End of Centroid *)


procedure Centroid(const Triangle:TTriangle2D; out x,y:TFloat);
begin
  Centroid(Triangle[1].x,Triangle[1].y,
           Triangle[2].x,Triangle[2].y,
           Triangle[3].x,Triangle[3].y,x,y);
end;
(* End of Centroid *)


procedure Centroid(const Rectangle:TRectangle; out x,y:TFloat);
begin
  x := (Rectangle[1].x + Rectangle[2].x) * 0.5;
  x := (Rectangle[1].y + Rectangle[2].y) * 0.5;
end;
(* End of Centroid *)


function Centroid(const P1,P2,P3:TPoint2D):TPoint2D;
begin
  Centroid(P1.x,P1.y,P2.x,P2.y,P3.x,P3.y,Result.x,Result.y);
end;
(* End of Centroid *)


function Centroid(const Triangle:TTriangle2D):TPoint2D;
begin
  Result := Centroid(Triangle[1],Triangle[2],Triangle[3]);
end;
(* End of Centroid *)


function Centroid(const Rectangle:TRectangle):TPoint2D;
begin
  Centroid(Rectangle,Result.x,Result.y);
end;
(* End of Centroid *)


procedure Centroid(const Polygon:TPolygon2D; out x,y:TFloat);
var
  i    : Integer;
  j    : Integer;
  asum : TFloat;
  term : TFloat;
begin
  x := 0.0;
  y := 0.0;

  if Length(Polygon) < 3 then Exit;

  asum := 0.0;
  j    := Length(Polygon) - 1;

  for i := 0 to Length(Polygon) - 1 do
  begin
    term := ((Polygon[j].x * Polygon[i].y) - (Polygon[j].y * Polygon[i].x));
    asum := asum + term;
    x := x + (Polygon[j].x + Polygon[i].x) * term;
    y := y + (Polygon[j].y + Polygon[i].y) * term;
    j := i;
  end;

  if NotEqual(asum,0.0) then
  begin
    x := x / (3.0 * asum);
    y := y / (3.0 * asum);
  end;
end;
(* End of PolygonCentroid *)


function Centroid(const Polygon:TPolygon2D): TPoint2D;
begin
  Centroid(Polygon,Result.x,Result.y);
end;
(* End of PolygonCentroid *)


function Centroid(const Polygon : array of TPoint3D):TPoint3D;
var
  TotalArea : TFloat;
  TempArea  : TFloat;
  i         : Integer;
  Len       : Integer;
begin
  Result.x := 0.0;
  Result.y := 0.0;
  Result.z := 0.0;

  if Length(Polygon) < 3 then Exit;

  TotalArea := 0.0;
  Len       := Length(Polygon);

  for i := 0 to  Len - 2 do
  begin
    TempArea := Area(Polygon[i],Polygon[i + 1],Polygon[Len - 1]);

    TotalArea := TotalArea + TempArea;

    Result.x := Result.x + TempArea * (Polygon[i].x + Polygon[i + 1].x + Polygon[Len - 1].x ) / 3.0;
    Result.y := Result.y + TempArea * (Polygon[i].y + Polygon[i + 1].y + Polygon[Len - 1].y ) / 3.0;
    Result.z := Result.z + TempArea * (Polygon[i].z + Polygon[i + 1].z + Polygon[Len - 1].z ) / 3.0;
  end;

  Result.x := Result.x / TotalArea;
  Result.y := Result.y / TotalArea;
  Result.z := Result.z / TotalArea;
end;
(* End of PolygonCentroid *)


function PolygonSegmentIntersect(const Segment:TSegment2D; const Polygon:TPolygon2D):Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    if Intersect(Segment[1],Segment[2],Polygon[i],Polygon[j]) then
    begin
      Result := True;
      Break;
    end;
    j := i;
  end;
end;
(* End of PolygonSegmentIntersect *)


function PolygonInPolygon(const Poly1,Poly2: TPolygon2D):Boolean;
begin
  (* to be implemented at a later date *)
  Result := False;
end;
(* End of PolygonInPolygon *)


function PointInConvexPolygon(const Px,Py:TFloat; const Polygon:TPolygon2D):Boolean;
var
  i                  : Integer;
  j                  : Integer;
  InitialOrientation : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  Result             := True;
  InitialOrientation := Orientation(Polygon[0],Polygon[Length(Polygon) - 1],Px,Py);
  j                  := 0;
  if InitialOrientation <> 0 then
  for i := 1 to Length(Polygon) - 1 do
  begin
    if InitialOrientation <> Orientation(Polygon[i],Polygon[j],Px,Py) then
    begin
      Result := False;
      Exit;
    end;
    j := i;
  end;
end;
(* End of PointInConvexPolygon *)


function PointInConvexPolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;
begin
  Result := PointInConvexPolygon(Point.x,Point.y,Polygon);
end;
(* End of PointInConvexPolygon *)


function PointInConcavePolygon(const Px,Py:TFloat; const Polygon:TPolygon2D):Boolean;
begin
 (* to be implemented at a later date *)
  Result := False;
end;
(* End of PointInConcavePolygon *)


function PointInConcavePolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;
begin
  Result := PointInConcavePolygon(Point.x,Point.y,Polygon);
end;
(* End of PointInConcavePolygon *)


function PointOnPolygon(const Px,Py:TFloat; const Polygon:TPolygon2D):Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    if ((Polygon[i].y <= Py) and (Py < Polygon[j].y)) or
       ((Polygon[j].y <= Py) and (Py < Polygon[i].y)) then
    begin
      if IsPointCollinear(Polygon[i],Polygon[j],Px,Py) then
      begin
        Result := True;
        Exit;
      end;
    end;
    j := i;
  end;
end;
(* End of PointOnPolygon *)


function PointOnPolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;
begin
  Result := PointOnPolygon(Point.x,Point.y,Polygon);
end;
(* End of PointOnPolygon *)


function PointInPolygon(const Px,Py:TFloat; const Polygon:TPolygon2D):Boolean;
var
  i : Integer;
  j : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    if ((Polygon[i].y <= Py) and (Py < Polygon[j].y)) or    // an upward crossing
       ((Polygon[j].y <= Py) and (Py < Polygon[i].y)) then  // a downward crossing
    begin
      (* compute the edge-ray intersect @ the x-coordinate *)
      if (Px - Polygon[i].x < ((Polygon[j].x - Polygon[i].x) * (Py - Polygon[i].y) / (Polygon[j].y - Polygon[i].y))) then
        Result := not Result;
    end;
    j := i;
  end;
end;
(* End PointInPolygon *)


function PointInPolygon(const Point:TPoint2D; const Polygon:TPolygon2D):Boolean;
begin
  Result := PointInPolygon(Point.x,Point.y,Polygon);
end;
(* End PointInPolygon *)


function PointInPolygon(const Point: array of TPoint2D; const Polygon:TPolygon2D): TBooleanArray;
var
  i               : Integer;
  j               : Integer;
  k               : Integer;
  Ratio           : TFloat;
  RatioCalculated : Boolean;
begin
  Ratio           := 0;
  RatioCalculated := False;

  if (Length(Polygon) < 3) or (Length(Point) = 0) then Exit;
  j := Length(Polygon) - 1;

  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Result) - 1 do Result[i] := False;

  for i := 0 to Length(Polygon) - 1 do
  begin
    for k := 0 to length(Point) - 1 do
    if ((Polygon[i].y <= Point[k].y) and (Point[k].y < Polygon[j].y)) or   // an upward crossing
       ((Polygon[j].y <= Point[k].y) and (Point[k].y < Polygon[i].y)) then // a downward crossing
    begin
      if not RatioCalculated then
      begin
        Ratio           := (Polygon[j].x - Polygon[i].x) / (Polygon[j].y - Polygon[i].y);
        RatioCalculated := True;
      end;
      if (Point[k].x - Polygon[i].x < ((Point[k].y - Polygon[i].y) * Ratio)) then
      Result[k] := not Result[k];
    end;
    RatioCalculated := False;
    j := i;
  end;
end;
(* End PointInPolygon *)


function ConvexQuadix(const Quadix:TQuadix2D):Boolean;
var
  Orin : TFloat;
begin
  Result := False;
  Orin   := Orientation(Quadix[1],Quadix[3],Quadix[2]);
  if Orin <> Orientation(Quadix[2],Quadix[4],Quadix[3]) then Exit;
  if Orin <> Orientation(Quadix[3],Quadix[1],Quadix[4]) then Exit;
  if Orin <> Orientation(Quadix[4],Quadix[2],Quadix[1]) then Exit;
  Result:= True;
end;
(* End of ConvexQuadix *)


function ComplexPolygon(const Polygon:TPolygon2D):Boolean;
begin
  (*
    Complex polygon definition
    A polygon that can have:
    1.) Self intersecting edges
    2.) Holes
    3.) Unclosed area
  *)
  Result := SelfIntersectingPolygon(Polygon);
end;
(* End of ComplexPolygon *)


function SimplePolygon(const Polygon:TPolygon2D):Boolean;
begin
  (*
    Simple polygon definition
    A polygon that can have:
    1.) inner and outter verticies
    2.) closed area
    3.) no self intersecting edges
  *)
  Result := not SelfIntersectingPolygon(Polygon);
end;
(* End of SimplePolygon *)


function ConvexPolygon(const Polygon:TPolygon2D):Boolean;
var
  i                  : Integer;
  j                  : Integer;
  k                  : Integer;
  InitialOrientation : Integer;
  CurrentOrientation : Integer;
  FirstTime          : Boolean;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  FirstTime := True;
  InitialOrientation  := Orientation(Polygon[Length(Polygon) - 2],Polygon[Length(Polygon) - 1],Polygon[0].x,Polygon[0].y);
  j                   := 0;
  k                   := Length(Polygon) - 1;
  for i := 1 to Length(Polygon) - 1 do
  begin
    CurrentOrientation := Orientation(Polygon[k],Polygon[j],Polygon[i].x,Polygon[i].y);
    if (InitialOrientation = CollinearOrientation) and (InitialOrientation <> CurrentOrientation) and FirstTime then
    begin
      InitialOrientation := CurrentOrientation;
      FirstTime          := False;
    end
    else if (InitialOrientation <> CurrentOrientation) and (CurrentOrientation <> CollinearOrientation) then
      Exit;
    k := j;
    j := i;
  end;
  Result:= True;
end;
(* End of ConvexPolygon *)


function ConvexPolygon(const Polygon:array of TPoint2D):Boolean;
var
  i                  : Integer;
  j                  : Integer;
  k                  : Integer;
  InitialOrientation : Integer;
  CurrentOrientation : Integer;
begin
  Result := False;
  if Length(Polygon) < 3 then Exit;
  InitialOrientation  := Orientation(Polygon[Length(Polygon) - 2],Polygon[Length(Polygon) - 1],Polygon[0].x,Polygon[0].y);
  j                   := 0;
  k                   := Length(Polygon) - 1;
  for i := 1 to Length(Polygon) - 1 do
  begin
    CurrentOrientation := Orientation(Polygon[k],Polygon[j],Polygon[i].x,Polygon[i].y);
    if (InitialOrientation = CollinearOrientation) and (InitialOrientation <> CurrentOrientation) then
      InitialOrientation := CurrentOrientation
    else if (InitialOrientation <> CurrentOrientation) and (CurrentOrientation <> CollinearOrientation) then
      Exit;
    k := j;
    j := i;
  end;
  Result:= True;
end;
(* End of ConvexPolygon *)


function ConcavePolygon(const Polygon:TPolygon2D):Boolean;
begin
  Result := not ConvexPolygon(Polygon);
end;
(* End of ConcavePolygon *)


function ConvexPolygonOrientation(const Polygon:TPolygon2D):Integer;
begin
  if Length(Polygon) < 3 then
  Result := 0
  else
  Result := Orientation(Polygon[0],Polygon[1],Polygon[2]);
end;
(* End of ConvexPolygonOrientation *)


function SimplePolygonOrientation(const Polygon:TPolygon2D):Integer;
var
  I       : Integer;
  anchor  : Integer;
  prevpos : Integer;
  postpos : Integer;
begin
  Result := 0;
  if Length(Polygon) < 3 then Exit;
    anchor := 0;
  for i := 1 to Length(Polygon) - 1 do
  begin
    if Polygon[i].x > Polygon[anchor].x then
      anchor := i
    else if (Polygon[i].x = Polygon[anchor].x) and (Polygon[i].y  < Polygon[anchor].y) then
      anchor := i;
  end;
  postpos := (anchor + 1) mod Length(Polygon);
  prevpos := anchor - 1;
  if prevpos < 0 then
    prevpos := Length(Polygon) - prevpos;
  Result := Orientation(Polygon[prevpos],Polygon[postpos],Polygon[anchor]);
end;
(* End of SimplePolygonOrientation *)


function SelfIntersectingPolygon(const Polygon:TPolygon2D):Boolean;
var
  I            : Integer;
  J            : Integer;
  Poly1Trailer : Integer;
  Poly2Trailer : Integer;
begin
  Result := False;
  if (Length(Polygon) < 3) or (Length(Polygon) < 3) then exit;
  Poly1Trailer := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Poly2Trailer := i + 1;
    for j := i + 2 to Length(Polygon) - 2 do
    begin
      if (i <> j) and (Poly1Trailer <> Poly2Trailer) then
      begin
        if Intersect(Polygon[i],Polygon[Poly1Trailer],Polygon[j],Polygon[Poly2Trailer]) then
        begin
          Result := True;
          Exit;
        end;
      end;
      Poly2Trailer := j;
    end;
    Poly1Trailer := i;
  end;
end;
(* End of SelfIntersectingPolygon *)


function RectangularHull(const Point:array of TPoint2D):TRectangle;
var
  MaxX : TFloat;
  MaxY : TFloat;
  MinX : TFloat;
  MinY : TFloat;
  I    : Integer;
begin
  if Length(Point) < 2 then Exit;
  MinX := Point[0].x;
  MaxX := Point[0].x;
  MinY := Point[0].y;
  MaxY := Point[0].y;
  for i := 1 to Length(Point) - 1 do
  begin
    if Point[i].x < MinX then
      MinX := Point[i].x
    else if Point[i].x > MaxX then
      MaxX := Point[i].x;

    if Point[i].y < MinY then
      MinY := Point[i].y
    else if Point[i].y > MaxY then
      MaxY := Point[i].y;
  end;
  Result := EquateRectangle(MinX,MinY,MaxX,MaxY);
end;
(* End of RectangularHull *)


function RectangularHull(const Polygon:TPolygon2D):TRectangle;
var
  MaxX : TFloat;
  MaxY : TFloat;
  MinX : TFloat;
  MinY : TFloat;
  I    : Integer;
begin
  if Length(Polygon) < 2 then Exit;
  MinX := Polygon[0].x;
  MaxX := Polygon[0].x;
  MinY := Polygon[0].y;
  MaxY := Polygon[0].y;
  for i := 1 to Length(Polygon) - 1 do
  begin
    if Polygon[i].x < MinX then
      MinX := Polygon[i].x
    else if Polygon[i].x > MaxX then
      MaxX := Polygon[i].x;

    if Polygon[i].y < MinY then
      MinY := Polygon[i].y
    else if Polygon[i].y > MaxY then
      MaxY := Polygon[i].y;
  end;
  Result := EquateRectangle(MinX,MinY,MaxX,MaxY);
end;
(* End of RectangularHull *)


function CircularHull(const Polygon:TPolygon2D):TCircle;
var
  I       : Integer;
  Cen     : TPoint2D;
  LayLen  : TFloat;
  LayDist : TFloat;
begin
  if Length(Polygon) < 3 then Exit;
    LayLen := -1;
  Cen := Centroid(Polygon);
  for i := 0 to Length(Polygon) - 1 do
  begin
    LayDist:= LayDistance(Cen,Polygon[i]);
    if LayDist > LayLen then
      LayLen := LayDist;
  end;
  Result.x      := Cen.x;
  Result.y      := Cen.y;
  Result.Radius := sqrt(LayLen);
end;
(* End of CircularHull *)


function SphereHull(const Polygon:array of TPoint3D):TSphere;
var
  I       : Integer;
  Cen     : TPoint3D;
  LayLen  : TFloat;
  LayDist : TFloat;
begin
  if Length(Polygon) < 2 then Exit;
  LayLen := -1;
  Cen := Centroid(Polygon);
  for i := 0 to Length(Polygon) - 1 do
  begin
    LayDist := LayDistance(Cen,Polygon[i]);
    if LayDist > LayLen then
      LayLen := LayDist;
  end;
  Result.x      := Cen.x;
  Result.y      := Cen.y;
  Result.z      := Cen.z;
  Result.Radius := sqrt(LayLen);
end;
(* End of SphereHull *)


function CalculateBarycentricBase(const x1,y1,x2,y2,x3,y3:TFloat):TFloat;
begin
 Result := Signed(x1,y1,x2,y2,x3,y3);
end;
(* End Of Calculate Barycentric Unit *)


function CreateBarycentricUnit(const x1,y1,x2,y2,x3,y3:TFloat):TBarycentricUnit;
begin
  Result.x1    := x1;
  Result.y1    := y1;
  Result.x2    := x2;
  Result.y2    := y2;
  Result.x3    := x3;
  Result.y3    := y3;
  Result.delta := CalculateBarycentricBase(x1,y1,x2,y2,x3,y3);
end;
(* End of Create Barycentric Unit *)


function CreateBarycentricUnit(const Triangle : TTriangle2D):TBarycentricUnit;
begin
  Result := CreateBarycentricUnit(Triangle[1].x,Triangle[1].y,
                                  Triangle[2].x,Triangle[2].y,
                                  Triangle[3].x,Triangle[3].y);
end;
(* End of Create Barycentric Unit *)


procedure ConvertCartesianToBarycentric(const x1,y1,x2,y2,x3,y3,px,py:TFloat; out u,v,w: TFloat);
var
  BarycentricBase : TFloat;
begin
  BarycentricBase := 1 / CalculateBarycentricBase(x1,y1,x2,y2,x3,y3);
  u := CalculateBarycentricBase(Px,Py,x2,y2,x3,y3) * BarycentricBase;
  v := CalculateBarycentricBase(x1,y1,Px,Py,x3,y3) * BarycentricBase;
  w := CalculateBarycentricBase(x1,y1,x2,y2,Px,Py) * BarycentricBase;
end;
(* End Of Convert Cartesian to Barycentric *)


procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; Px,Py:TFloat; out u,v,w: TFloat);
var
  BarycentricBase : TFloat;
begin
  BarycentricBase := 1 / BU.delta;
  u := CalculateBarycentricBase(Px,Py,BU.x2,BU.y2,BU.x3,BU.y3) * BarycentricBase;
  v := CalculateBarycentricBase(BU.x1,BU.y1,Px,Py,BU.x3,BU.y3) * BarycentricBase;
  w := CalculateBarycentricBase(BU.x1,BU.y1,BU.x2,BU.y2,Px,Py) * BarycentricBase;
end;
(* End Of Convert Cartesian to Barycentric *)


procedure ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TPoint2D; out BCrd: TBarycentricTriplet);
begin
  ConvertCartesianToBarycentric(BU,Point.x,Point.y,BCrd.u,BCrd.v,BCrd.w);
end;
(* End Of Convert Cartesian to Barycentric *)


function ConvertCartesianToBarycentric(const BU:TBarycentricUnit; const Point:TPoint2D): TBarycentricTriplet;
begin
  ConvertCartesianToBarycentric(BU,Point,Result);
end;
(* End Of Convert Cartesian to Barycentric *)


procedure ConvertBarycentricToCartesian(const u,v,w,x1,y1,x2,y2,x3,y3:TFloat; out x,y:TFloat);
begin
  x := u * x1 + v * x2 + w * x3;
  y := u * y1 + v * y2 + w * y3;
end;
(* End Of Convert Barycentric to Cartesian *)


procedure ConvertBarycentricToCartesian(const u,v,w:TFloat; BU:TBarycentricUnit; out x,y:TFloat);
begin
  ConvertBarycentricToCartesian(u,v,w,BU.x1,BU.y1,BU.x2,BU.y2,BU.x3,BU.y3,x,y);
end;
(* End Of Convert Barycentric to Cartesian *)


procedure ConvertBarycentricToCartesian(const u,v,w:TFloat; BU:TBarycentricUnit; out Point:TPoint2D);
begin
  ConvertBarycentricToCartesian(u,v,w,BU.x1,BU.y1,BU.x2,BU.y2,BU.x3,BU.y3,Point.x,Point.y);
end;
(* End Of Convert Barycentric to Cartesian *)


function ConvertBarycentricToCartesian(const u,v,w:TFloat; BU:TBarycentricUnit):TPoint2D;
begin
  ConvertBarycentricToCartesian(u,v,w,BU,Result);
end;
(* End Of Convert Barycentric to Cartesian *)


function Clip(const Segment:TSegment2D; const Rect:TRectangle; out CSegment:TSegment2D):Boolean;
 (* Cohen-Sutherland Clipping Algorithm *)

 const CLIP_BOTTOM = 1;
 const CLIP_TOP    = 2;
 const CLIP_LEFT   = 4;
 const CLIP_RIGHT  = 8;

  function OutCode(const x,y:TFloat):Integer;
  begin
    Result := 0;

    if y < Rect[1].y then
      Result := Result or CLIP_TOP
    else if y > Rect[2].y then
      Result := Result or CLIP_BOTTOM;

    if x < Rect[1].x then
      Result := Result or CLIP_LEFT
    else if x > Rect[2].x then
      Result := Result or CLIP_RIGHT;
  end;

var
  outcode0   : Integer;
  outcode1   : Integer;
  outcodeout : Integer;
  x          : TFloat;
  y          : TFloat;
  Dx         : TFloat;
  Dy         : TFloat;
begin
  Result   := False;
  CSegment := Segment;
  x        := 0.0;
  y        := 0.0;

  OutCode0 := OutCode(CSegment[1].x,CSegment[1].y);
  OutCode1 := OutCode(CSegment[2].x,CSegment[2].y);

  while (OutCode0 <> 0) or (OutCode1 <> 0) do
  begin
    if (OutCode0 and OutCode1) <> 0 then
    Exit
    else
    begin
      if Outcode0 <> 0 then
        OutCodeout := OutCode0
      else
        OutCodeout := OutCode1;

      Dx := (CSegment[2].x - CSegment[1].x);
      Dy := (CSegment[2].y - CSegment[1].y);

      if ((OutCodeout and CLIP_TOP) = CLIP_TOP) then
      begin
        x := CSegment[1].x + Dx * (Rect[1].y - CSegment[1].y) / Dy;
        y := Rect[1].y;
      end
      else if ((OutCodeout and CLIP_BOTTOM) = CLIP_BOTTOM) then
      begin
        x := CSegment[1].x + Dx * (Rect[2].y - CSegment[1].y) / Dy;
        y := Rect[2].y;
      end
      else if ((OutCodeout and CLIP_RIGHT) = CLIP_RIGHT) then
      begin
        y := CSegment[1].y + Dy * (Rect[2].x - CSegment[1].x) / Dx;
        x := Rect[2].x;
      end
      else if ((OutCodeout and CLIP_LEFT) = CLIP_LEFT) then
      begin
        y := CSegment[1].y + Dy * (Rect[1].x - CSegment[1].x) / Dx;
        x := Rect[1].x;
      end;

      if (OutCodeout = outcode0) then
      begin
        CSegment[1].x := x;
        CSegment[1].y := y;
        OutCode0  := OutCode(CSegment[1].x,CSegment[1].y)
      end
      else
      begin
        CSegment[2].x := x;
        CSegment[2].y := y;
        OutCode1  := OutCode(CSegment[2].x,CSegment[2].y);
      end
    end;
  end;
  Result := true;
end;
(* End of Clip *)


function Clip(const Segment:TSegment2D; const Triangle:TTriangle2D; out CSegment:TSegment2D):Boolean;
var
  Pos : Integer;
begin
  Pos := 1;

  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Triangle[1].x,Triangle[1].y,Triangle[2].x,Triangle[2].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Triangle[2].x,Triangle[2].y,Triangle[3].x,Triangle[3].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if (Pos < 3) then
  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Triangle[3].x,Triangle[3].y,Triangle[1].x,Triangle[1].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Pos = 2 then
  begin
    if PointInTriangle(Segment[1],Triangle) then
      CSegment[Pos] := Segment[1]
    else
      CSegment[Pos] := Segment[2];
  end;
  Result := (Pos > 1);
end;
(* End of Clip *)


function Clip(const Segment:TSegment2D; const Quadix:TQuadix2D; out CSegment:TSegment2D):Boolean;
var
  Pos : Integer;
begin
  Pos := 1;

  if Intersect(Segment[1].x,Segment[1].y, Segment[2].x,Segment[2].y,Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Intersect(Segment[1].x,Segment[1].y, Segment[2].x,Segment[2].y,Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y,CSegment[Pos].x,CSegment[Pos].y) then
  begin
    CSegment[Pos]:= IntersectionPoint(Segment[1],Segment[2],Quadix[2],Quadix[3]);
    Inc(Pos);
  end;

  if (Pos < 3) then
  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if (Pos < 3) then
  if Intersect(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y,Quadix[4].x,Quadix[4].y,Quadix[1].x,Quadix[1].y,CSegment[Pos].x,CSegment[Pos].y) then
    Inc(Pos);

  if Pos = 2 then
  begin
    if PointInQuadix(Segment[1],Quadix) then
    CSegment[Pos]:= Segment[1]
    else
    CSegment[Pos]:= Segment[2];
  end;
  Result := (Pos > 1);
end;
(* End of Clip *)


function Clip(const Segment:TSegment2D; const Circle:TCircle; out CSegment:TSegment2D):Boolean;
var
  Cnt : Integer;
  I1  : TPoint2D;
  I2  : TPoint2D;
begin
  Result := False;
  IntersectionPoint(Segment,Circle,Cnt,I1,I2);
  if Cnt = 2 then
  begin
    CSegment[1] := I1;
    CSegment[2] := I2;
    Result      := True;
  end
end;
(* End of Clip *)


function Clip(const Segment:TSegment2D; const Obj : TGeometricObject; out CSegment:TSegment2D):Boolean;
begin
  Result := False;
  case Obj.ObjectType of
    geoRectangle  : Result := Clip(Segment,Obj.Rectangle ,CSegment);
    geoTriangle2D : Result := Clip(Segment,Obj.Triangle2D,CSegment);
    geoQuadix2D   : Result := Clip(Segment,Obj.Quadix2D  ,CSegment);
    geoCircle     : Result := Clip(Segment,Obj.Circle    ,CSegment);
  end;
end;
(* End of Clip *)


function Area(const Point1,Point2,Point3:TPoint2D):TFloat;
begin
  Result := 0.5 *
                  (
                    (Point1.x * (Point2.y - Point3.y)) +
                    (Point2.x * (Point3.y - Point1.y)) +
                    (Point3.x * (Point1.y - Point2.y))
                  );
end;
(* End of Area 3-2D Points *)


function Area(const Point1,Point2,Point3:TPoint3D):TFloat;
var
  Dx1 : TFloat;
  Dx2 : TFloat;
  Dy1 : TFloat;
  Dy2 : TFloat;
  Dz1 : TFloat;
  Dz2 : TFloat;
  Cx  : TFloat;
  Cy  : TFloat;
  Cz  : TFloat;
begin
  Dx1 := Point2.x - Point1.x;
  Dy1 := Point2.y - Point1.y;
  Dz1 := Point2.z - Point1.z;

  Dx2 := Point3.x - Point1.x;
  Dy2 := Point3.y - Point1.y;
  Dz2 := Point3.z - Point1.z;

  Cx  := Dy1 * Dz2 - Dy2 * Dz1;
  Cy  := Dx2 * Dz1 - Dx1 * Dz2;
  Cz  := Dx1 * Dy2 - Dx2 * Dy1;

  Result := (sqrt(Cx * Cx + Cy * Cy + Cz * Cz) * 0.5);
end;
(* End of Area 3-3D Points *)


function Area(const Triangle:TTriangle2D):TFloat;
begin
  Result := 0.5 *
                 (
                   (Triangle[1].x * (Triangle[2].y - Triangle[3].y)) +
                   (Triangle[2].x * (Triangle[3].y - Triangle[1].y)) +
                   (Triangle[3].x * (Triangle[1].y - Triangle[2].y))
                 );
end;
(* End of Area 2D Triangle *)


function Area(const Triangle:TTriangle3D):TFloat;
var
  Dx1 : TFloat;
  Dx2 : TFloat;
  Dy1 : TFloat;
  Dy2 : TFloat;
  Dz1 : TFloat;
  Dz2 : TFloat;
  Cx  : TFloat;
  Cy  : TFloat;
  Cz  : TFloat;
begin
  Dx1 := Triangle[2].x - Triangle[1].x;
  Dy1 := Triangle[2].y - Triangle[1].y;
  Dz1 := Triangle[2].z - Triangle[1].z;

  Dx2 := Triangle[3].x - Triangle[1].x;
  Dy2 := Triangle[3].y - Triangle[1].y;
  Dz2 := Triangle[3].z - Triangle[1].z;

  Cx  := Dy1 * Dz2 - Dy2 * Dz1;
  Cy  := Dx2 * Dz1 - Dx1 * Dz2;
  Cz  := Dx1 * Dy2 - Dx2 * Dy1;

  Result := (sqrt(Cx * Cx + Cy * Cy + Cz * Cz) * 0.5);
end;
(* End of Area 3D Triangle *)


function Area(const Quadix:TQuadix2D):TFloat;
begin
  Result := 0.5 *
                 (
                  (Quadix[1].x * (Quadix[2].y - Quadix[4].y))+
                  (Quadix[2].x * (Quadix[3].y - Quadix[1].y))+
                  (Quadix[3].x * (Quadix[4].y - Quadix[2].y))+
                  (Quadix[4].x * (Quadix[1].y - Quadix[3].y))
                 );
end;
(* End of Area 2D Qudix *)


function Area(const Quadix:TQuadix3D):TFloat;
begin
  Result := (
             Area(EquateTriangle(Quadix[1],Quadix[2],Quadix[3]))+
             Area(EquateTriangle(Quadix[3],Quadix[4],Quadix[1]))
            );
end;
(* End of Area 3D Quadix *)


function Area(const Rectangle:TRectangle):TFloat;
begin
  Result := abs(Rectangle[2].x - Rectangle[1].x) * abs(Rectangle[2].y - Rectangle[1].y);
end;
(* End of Area *)


function Area(const Circle:TCircle):TFloat;
begin
  Result := PI2 * Circle.Radius * Circle.Radius;
end;
(* End of Area *)


function Area(const Polygon:TPolygon2D):TFloat;
var
  i : Integer;
  j : Integer;
begin
 (*
 Old implementation uses mod to wrap around - not very efficient...
 Result := 0.0;
 if Length(Polygon) < 3 then Exit;
 for i := 0 to Length(Polygon) - 1 do
  begin
   Result := Result + (
                       (Polygon[i].x * Polygon[(i + 1) mod Length(Polygon)].y)-
                       (Polygon[i].y * Polygon[(i + 1) mod Length(Polygon)].x)
                      );
  end;
 Result := Result * 0.5;
 *)

  Result := 0.0;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result := Result + ((Polygon[j].x * Polygon[i].y) - (Polygon[j].y * Polygon[i].x));
    j := i;
  end;
  Result := Result * 0.5;
end;
(* End of Area *)


function Area(const Obj:TGeometricObject):TFloat;
begin
  case Obj.ObjectType of
    geoTriangle2D: Result := Area(Obj.Triangle2D);
    geoTriangle3D: Result := Area(Obj.Triangle3D);
    geoQuadix2D  : Result := Area(Obj.Quadix2D  );
    geoQuadix3D  : Result := Area(Obj.Quadix3D  );
    geoRectangle : Result := Area(Obj.Rectangle );
    geoCircle    : Result := Area(Obj.Circle    );
  else
    Result := 0.0;
  end;
end;
(* End of Area *)


function Perimeter(const Triangle:TTriangle2D):TFloat;
begin
  Result:= Distance(Triangle[1],Triangle[2]) +
           Distance(Triangle[2],Triangle[3]) +
           Distance(Triangle[3],Triangle[1]);
end;
(* End of Perimeter *)


function Perimeter(const Triangle:TTriangle3D):TFloat;
begin
  Result:= Distance(Triangle[1],Triangle[2]) +
           Distance(Triangle[2],Triangle[3]) +
           Distance(Triangle[3],Triangle[1]);
end;
(* End of Perimeter *)


function Perimeter(const Quadix:TQuadix2D):TFloat;
begin
  Result:= Distance(Quadix[1],Quadix[2]) +
           Distance(Quadix[2],Quadix[3]) +
           Distance(Quadix[3],Quadix[4]) +
           Distance(Quadix[4],Quadix[1]);
end;
(* End of Perimeter *)


function Perimeter(const Quadix:TQuadix3D):TFloat;
begin
  Result:= Distance(Quadix[1],Quadix[2]) +
           Distance(Quadix[2],Quadix[3]) +
           Distance(Quadix[3],Quadix[4]) +
           Distance(Quadix[4],Quadix[1]);
end;
(* End of Perimeter *)


function Perimeter(const Rectangle:TRectangle):TFloat;
begin
  Result:= 2 * (abs(Rectangle[2].x - Rectangle[1].x) + abs(Rectangle[2].y - Rectangle[1].y));
end;
(* End of Perimeter *)


function Perimeter(const Circle:TCircle):TFloat;
begin
  Result:= 2 * Pi * Circle.Radius;
end;
(* End of Perimeter *)


function Perimeter(const Polygon:TPolygon2D):TFloat;
var
  i : Integer;
  j : Integer;
begin
  Result := 0.0;
  if Length(Polygon) < 3 then Exit;
  j := Length(Polygon) - 1;
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result := Result + Distance(Polygon[i], Polygon[j]);
    j := i;
  end;
end;
(* End of Perimeter *)


function Perimeter(const Obj:TGeometricObject):TFloat;
begin
  case Obj.ObjectType of
    geoTriangle2D : Result := Perimeter(Obj.Triangle2D);
    geoTriangle3D : Result := Perimeter(Obj.Triangle3D);
    geoQuadix2D   : Result := Perimeter(Obj.Quadix2D  );
    geoQuadix3D   : Result := Perimeter(Obj.Quadix3D  );
    geoRectangle  : Result := Perimeter(Obj.Rectangle );
    geoCircle     : Result := Perimeter(Obj.Circle    );
  else
    Result := 0.0;
  end;
end;
(* End of Perimeter *)


function SemiPerimeter(const Triangle : TTriangle2D):TFloat;
begin
  Result := Perimeter(Triangle) * 0.5;
end;
(* End of Perimeter *)


function SemiPerimeter(const Triangle : TTriangle3D):TFloat;
begin
  Result := Perimeter(Triangle) * 0.5;
end;
(* End of Perimeter *)


procedure Rotate(RotAng:TFloat; const x,y:TFloat; out Nx,Ny:TFloat);
var
  SinVal : TFloat;
  CosVal : TFloat;
begin
  RotAng := RotAng * PIDiv180;
  SinVal := Sin(RotAng);
  CosVal := Cos(RotAng);
  Nx     := (x * CosVal) - (y * SinVal);
  Ny     := (y * CosVal) + (x * SinVal);
end;
(* End of Rotate Cartesian Point *)


procedure Rotate(const RotAng:TFloat; const x,y,ox,oy:TFloat; out Nx,Ny:TFloat);
begin
  Rotate(RotAng,x - ox,y - oy,Nx,Ny);
  Nx := Nx + ox;
  Ny := Ny + oy;
end;
(* End of Rotate Cartesian Point About Origin *)


function Rotate(const RotAng:TFloat; const Point:TPoint2D):TPoint2D;
begin
  Rotate(RotAng,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Rotate Point *)


function Rotate(const RotAng:TFloat; const Point,OPoint:TPoint2D):TPoint2D;
begin
  Rotate(RotAng,Point.x,Point.y,OPoint.x,OPoint.y,Result.x,Result.y);
end;
(* End of Rotate Point About Origin *)


function Rotate(const RotAng:TFloat; const Segment:TSegment2D):TSegment2D;
begin
  Result[1] := Rotate(RotAng,Segment[1]);
  Result[2] := Rotate(RotAng,Segment[2]);
end;
(* End of Rotate Segment*)


function Rotate(const RotAng:TFloat; const Segment:TSegment2D; const OPoint:TPoint2D):TSegment2D;
begin
  Result[1] := Rotate(RotAng,Segment[1],OPoint);
  Result[2] := Rotate(RotAng,Segment[2],OPoint);
end;
(* End of Rotate Segment About Origin *)


function Rotate(const RotAng:TFloat; const Triangle:TTriangle2D):TTriangle2D;
begin
  Result[1] := Rotate(RotAng,Triangle[1]);
  Result[2] := Rotate(RotAng,Triangle[2]);
  Result[3] := Rotate(RotAng,Triangle[3]);
end;
(* End of Rotate 2D Triangle*)


function Rotate(const RotAng:TFloat; const Triangle:TTriangle2D; const OPoint:TPoint2D):TTriangle2D;
begin
  Result[1] := Rotate(RotAng,Triangle[1],OPoint);
  Result[2] := Rotate(RotAng,Triangle[2],OPoint);
  Result[3] := Rotate(RotAng,Triangle[3],OPoint);
end;
(* End of Rotate 2D Triangle About Origin *)


function Rotate(const RotAng:TFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := Rotate(RotAng,Quadix[1]);
  Result[2] := Rotate(RotAng,Quadix[2]);
  Result[3] := Rotate(RotAng,Quadix[3]);
  Result[4] := Rotate(RotAng,Quadix[4]);
end;
(* End of Rotate 2D Quadix*)


function Rotate(const RotAng:TFloat; const Quadix:TQuadix2D; const OPoint:TPoint2D):TQuadix2D;
begin
  Result[1] := Rotate(RotAng,Quadix[1],OPoint);
  Result[2] := Rotate(RotAng,Quadix[2],OPoint);
  Result[3] := Rotate(RotAng,Quadix[3],OPoint);
  Result[4] := Rotate(RotAng,Quadix[4],OPoint);
end;
(* End of Rotate 2D Quadix About Origin *)


function Rotate(const RotAng:TFloat; const Polygon:TPolygon2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i]);
  end;
end;
(* End of Rotate 2D Polygon *)


function Rotate(const RotAng:TFloat; const Polygon:TPolygon2D; const OPoint:TPoint2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i],OPoint);
  end;
end;
(* End of Rotate 2D Polygon About Origin *)


function Rotate(const RotAng:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Rotate(RotAng,Obj.Point2D);
    geoSegment2D : Result.Segment2D  := Rotate(RotAng,Obj.Segment2D);
    geoTriangle2D: Result.Triangle2D := Rotate(RotAng,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := Rotate(RotAng,Obj.Quadix2D);
  else
    Result := Obj;
  end;
end;
(* End of Rotate Geometric Object *)


function Rotate(const RotAng:TFloat; const Obj:TGeometricObject; const OPoint: TPoint2D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Rotate(RotAng,Obj.Point2D,OPoint);
    geoSegment2D : Result.Segment2D  := Rotate(RotAng,Obj.Segment2D,OPoint);
    geoTriangle2D: Result.Triangle2D := Rotate(RotAng,Obj.Triangle2D,OPoint);
    geoQuadix2D  : Result.Quadix2D   := Rotate(RotAng,Obj.Quadix2D,OPoint);
  else
    Result := Obj;
  end;
end;
(* End of Rotate Geometric Object About Origin *)


procedure Rotate(const Rx,Ry,Rz:TFloat; const x,y,z:TFloat; out Nx,Ny,Nz:TFloat);
var
  TempX   : TFloat;
  TempY   : TFloat;
  TempZ   : TFloat;
  SinX    : TFloat;
  SinY    : TFloat;
  SinZ    : TFloat;
  CosX    : TFloat;
  CosY    : TFloat;
  CosZ    : TFloat;
  XRadAng : TFloat;
  YRadAng : TFloat;
  ZRadAng : TFloat;
begin
  XRadAng := Rx * PIDiv180;
  YRadAng := Ry * PIDiv180;
  ZRadAng := Rz * PIDiv180;

  SinX    := Sin(XRadAng);
  SinY    := Sin(YRadAng);
  SinZ    := Sin(ZRadAng);

  CosX    := Cos(XRadAng);
  CosY    := Cos(YRadAng);
  CosZ    := Cos(ZRadAng);

  Tempy   := y * CosY -     z * SinY;
  Tempz   := y * SinY +     z * CosY;
  Tempx   := x * CosX - Tempz * SinX;

  Nz      :=     x * SinX + Tempz * CosX;
  Nx      := Tempx * CosZ - TempY * SinZ;
  Ny      := Tempx * SinZ + TempY * CosZ;
end;
(* End of Rotate 3D Point *)


procedure Rotate(const Rx,Ry,Rz:TFloat; const x,y,z,ox,oy,oz:TFloat; out Nx,Ny,Nz:TFloat);
begin
  Rotate(Rx,Ry,Rz,x - ox,y - oy,z - oz,Nx,Ny,Nz);
  Nx := Nx + ox;
  Ny := Ny + oy;
  Nz := Nz + oz;
end;
(* End of Rotate 3D Point About Origin Point *)


function Rotate(const Rx,Ry,Rz:TFloat; const Point:TPoint3D):TPoint3D;
begin
  Rotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,Result.x,Result.y,Result.z);
end;
(* End of Rotate 3D Point *)


function Rotate(const Rx,Ry,Rz:TFloat; const Point,OPoint:TPoint3D):TPoint3D;
begin
  Rotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,OPoint.x,OPoint.y,OPoint.z,Result.x,Result.y,Result.z);
end;
(* End of Rotate 3D Point About Origin Point *)


function Rotate(const Rx,Ry,Rz:TFloat; const Segment:TSegment3D):TSegment3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Segment[1]);
  Result[2] := Rotate(Rx,Ry,Rz,Segment[2]);
end;
(* End of Rotate 3D Segment *)


function Rotate(const Rx,Ry,Rz:TFloat; const Segment:TSegment3D; const OPoint:TPoint3D):TSegment3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Segment[1],OPoint);
  Result[2] := Rotate(Rx,Ry,Rz,Segment[2],OPoint);
end;
(* End of Rotate 3D Segment About Origin Point *)


function Rotate(const Rx,Ry,Rz:TFloat; const Triangle:TTriangle3D):TTriangle3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Triangle[1]);
  Result[2] := Rotate(Rx,Ry,Rz,Triangle[2]);
  Result[3] := Rotate(Rx,Ry,Rz,Triangle[3]);
end;
(* End of Rotate 3D Triangle *)


function Rotate(const Rx,Ry,Rz:TFloat; const Triangle:TTriangle3D; const OPoint:TPoint3D):TTriangle3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Triangle[1],OPoint);
  Result[2] := Rotate(Rx,Ry,Rz,Triangle[2],OPoint);
  Result[3] := Rotate(Rx,Ry,Rz,Triangle[3],OPoint);
end;
(* End of Rotate 3D Triangle About Origin Point *)


function Rotate(const Rx,Ry,Rz:TFloat; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Quadix[1]);
  Result[2] := Rotate(Rx,Ry,Rz,Quadix[2]);
  Result[3] := Rotate(Rx,Ry,Rz,Quadix[3]);
  Result[4] := Rotate(Rx,Ry,Rz,Quadix[4]);
end;
(* End of Rotate 3D Quadix *)


function Rotate(const Rx,Ry,Rz:TFloat; const Quadix:TQuadix3D; const OPoint:TPoint3D):TQuadix3D;
begin
  Result[1] := Rotate(Rx,Ry,Rz,Quadix[1],OPoint);
  Result[2] := Rotate(Rx,Ry,Rz,Quadix[2],OPoint);
  Result[3] := Rotate(Rx,Ry,Rz,Quadix[3],OPoint);
  Result[4] := Rotate(Rx,Ry,Rz,Quadix[4],OPoint);
end;
(* End of Rotate 3D Quadix About Origin Point *)


function Rotate(const Rx,Ry,Rz:TFloat; const Polygon:TPolygon3D):TPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(Rx,Ry,Rz,Polygon[i]);
  end;
end;
(* End of Rotate 3D Polygon *)


function Rotate(const Rx,Ry,Rz:TFloat; const Polygon:TPolygon3D; const OPoint:TPoint3D):TPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(Rx,Ry,Rz,Polygon[i],OPoint);
  end;
end;
(* End of Rotate 3D Polygon About Origin Point *)


function Rotate(const Rx,Ry,Rz:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Rotate(Rx,Ry,Rz,Obj.Point3D   );
    geoSegment3D : Result.Segment3D  := Rotate(Rx,Ry,Rz,Obj.Segment3D );
    geoTriangle3D: Result.Triangle3D := Rotate(Rx,Ry,Rz,Obj.Triangle3D);
    geoQuadix3D  : Result.Quadix3D   := Rotate(Rx,Ry,Rz,Obj.Quadix3D  );
  else
    Result := Obj;
  end;
end;
(* End of Rotate *)


function Rotate(const Rx,Ry,Rz:TFloat; const Obj:TGeometricObject; const OPoint: TPoint3D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Rotate(Rx,Ry,Rz,Obj.Point3D,OPoint   );
    geoSegment3D : Result.Segment3D  := Rotate(Rx,Ry,Rz,Obj.Segment3D,OPoint );
    geoTriangle3D: Result.Triangle3D := Rotate(Rx,Ry,Rz,Obj.Triangle3D,OPoint);
    geoQuadix3D  : Result.Quadix3D   := Rotate(Rx,Ry,Rz,Obj.Quadix3D,OPoint  );
  else
    Result := Obj;
  end;
end;
(* End of Rotate About Origin Point *)


procedure FastRotate(RotAng:Integer; const x,y:TFloat; out Nx,Ny:TFloat);
var
  SinVal : TFloat;
  CosVal : TFloat;
begin
  RotAng := RotAng mod 360;
  if RotAng < 0 then RotAng := 360 + RotAng;
  SinVal := SinTable[RotAng];
  CosVal := CosTable[RotAng];
  Nx     := x * CosVal - y * SinVal;
  Ny     := y * CosVal + x * SinVal;
end;
(* End of Fast Rotation *)


procedure FastRotate(RotAng:Integer; x,y,ox,oy:TFloat; out Nx,Ny:TFloat);
var
  SinVal : TFloat;
  CosVal : TFloat;
begin
  RotAng := RotAng mod 360;
  SinVal := SinTable[RotAng];
  CosVal := CosTable[RotAng];
  x      := x - ox;
  y      := y - oy;
  Nx     := (x * CosVal - y * SinVal) + ox;
  Ny     := (y * CosVal + x * SinVal) + oy;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Point:TPoint2D):TPoint2D;
begin
  FastRotate(RotAng,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Point,OPoint:TPoint2D):TPoint2D;
begin
  FastRotate(RotAng,Point.x,Point.y,OPoint.x,OPoint.y,Result.x,Result.y);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Segment:TSegment2D):TSegment2D;
begin
  Result[1] := FastRotate(RotAng,Segment[1]);
  Result[2] := FastRotate(RotAng,Segment[2]);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Segment:TSegment2D; const OPoint:TPoint2D):TSegment2D;
begin
  Result[1] := FastRotate(RotAng,Segment[1],OPoint);
  Result[2] := FastRotate(RotAng,Segment[2],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Triangle:TTriangle2D):TTriangle2D;
begin
  Result[1] := FastRotate(RotAng,Triangle[1]);
  Result[2] := FastRotate(RotAng,Triangle[2]);
  Result[3] := FastRotate(RotAng,Triangle[3]);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Triangle:TTriangle2D; const OPoint:TPoint2D):TTriangle2D;
begin
  Result[1] := FastRotate(RotAng,Triangle[1],OPoint);
  Result[2] := FastRotate(RotAng,Triangle[2],OPoint);
  Result[3] := FastRotate(RotAng,Triangle[3],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := FastRotate(RotAng,Quadix[1]);
  Result[2] := FastRotate(RotAng,Quadix[2]);
  Result[3] := FastRotate(RotAng,Quadix[3]);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Quadix:TQuadix2D; const OPoint:TPoint2D):TQuadix2D;
begin
  Result[1] := FastRotate(RotAng,Quadix[1],OPoint);
  Result[2] := FastRotate(RotAng,Quadix[2],OPoint);
  Result[3] := FastRotate(RotAng,Quadix[3],OPoint);
  Result[4] := FastRotate(RotAng,Quadix[4],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Polygon:TPolygon2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i]);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Polygon:TPolygon2D; const OPoint:TPoint2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := Rotate(RotAng,Polygon[i],OPoint);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := FastRotate(RotAng,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := FastRotate(RotAng,Obj.Segment2D );
    geoTriangle2D: Result.Triangle2D := FastRotate(RotAng,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := FastRotate(RotAng,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const RotAng:Integer; const Obj:TGeometricObject; const OPoint: TPoint2D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := FastRotate(RotAng,Obj.Point2D,OPoint   );
    geoSegment2D : Result.Segment2D  := FastRotate(RotAng,Obj.Segment2D,OPoint );
    geoTriangle2D: Result.Triangle2D := FastRotate(RotAng,Obj.Triangle2D,OPoint);
    geoQuadix2D  : Result.Quadix2D   := FastRotate(RotAng,Obj.Quadix2D,OPoint  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


procedure FastRotate(Rx,Ry,Rz:Integer; const x,y,z:TFloat; out Nx,Ny,Nz:TFloat);
var
  TempX : TFloat;
  TempY : TFloat;
  TempZ : TFloat;
  SinX  : TFloat;
  SinY  : TFloat;
  SinZ  : TFloat;
  CosX  : TFloat;
  CosY  : TFloat;
  CosZ  : TFloat;
begin
  Rx := Rx mod 360;
  Ry := Ry mod 360;
  Rz := Rz mod 360;

  if Rx < 0 then Rx := 360 + Rx;
  if Ry < 0 then Ry := 360 + Ry;
  if Rz < 0 then Rz := 360 + Rz;

  SinX := SinTable[Rx];
  SinY := SinTable[Ry];
  SinZ := SinTable[Rz];

  CosX := CosTable[Rx];
  CosY := CosTable[Ry];
  CosZ := CosTable[Rz];

  Tempy := y * CosY - z * SinY;
  Tempz := y * SinY + z * CosY;
  Tempx := x * CosX - Tempz * SinX;

  Nz    :=     x * SinX + Tempz * CosX;
  Nx    := Tempx * CosZ - TempY * SinZ;
  Ny    := Tempx * SinZ + TempY * CosZ;
end;
(* End of Fast Rotation *)


procedure FastRotate(const Rx,Ry,Rz:Integer; const x,y,z,ox,oy,oz:TFloat; out Nx,Ny,Nz:TFloat);
begin
  FastRotate(Rx,Ry,Rz,x - ox,y - oy,z - oz,Nx,Ny,Nz);
  Nx := Nx + ox;
  Ny := Ny + oy;
  Nz := Nz + oz;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Point:TPoint3D):TPoint3D;
begin
  FastRotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,Result.x,Result.y,Result.z);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Point,OPoint:TPoint3D):TPoint3D;
begin
  FastRotate(Rx,Ry,Rz,Point.x,Point.y,Point.z,OPoint.x,OPoint.y,OPoint.z,Result.x,Result.y,Result.z);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Segment:TSegment3D):TSegment3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Segment[1]);
  Result[2] := FastRotate(Rx,Ry,Rz,Segment[2]);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Segment:TSegment3D; const OPoint:TPoint3D):TSegment3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Segment[1],OPoint);
  Result[2] := FastRotate(Rx,Ry,Rz,Segment[2],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Triangle:TTriangle3D):TTriangle3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Triangle[1]);
  Result[2] := FastRotate(Rx,Ry,Rz,Triangle[2]);
  Result[3] := FastRotate(Rx,Ry,Rz,Triangle[3]);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Triangle:TTriangle3D; const OPoint:TPoint3D):TTriangle3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Triangle[1],OPoint);
  Result[2] := FastRotate(Rx,Ry,Rz,Triangle[2],OPoint);
  Result[3] := FastRotate(Rx,Ry,Rz,Triangle[3],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Quadix[1]);
  Result[2] := FastRotate(Rx,Ry,Rz,Quadix[2]);
  Result[3] := FastRotate(Rx,Ry,Rz,Quadix[3]);
  Result[4] := FastRotate(Rx,Ry,Rz,Quadix[4]);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Quadix:TQuadix3D; const OPoint:TPoint3D):TQuadix3D;
begin
  Result[1] := FastRotate(Rx,Ry,Rz,Quadix[1],OPoint);
  Result[2] := FastRotate(Rx,Ry,Rz,Quadix[2],OPoint);
  Result[3] := FastRotate(Rx,Ry,Rz,Quadix[3],OPoint);
  Result[4] := FastRotate(Rx,Ry,Rz,Quadix[4],OPoint);
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Polygon:TPolygon3D):TPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := FastRotate(Rx,Ry,Rz,Polygon[i]);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Polygon:TPolygon3D; const OPoint:TPoint3D):TPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := FastRotate(Rx,Ry,Rz,Polygon[i],OPoint);
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point3D    := FastRotate(Rx,Ry,Rz,Obj.Point3D   );
    geoSegment2D : Result.Segment3D  := FastRotate(Rx,Ry,Rz,Obj.Segment3D );
    geoTriangle2D: Result.Triangle3D := FastRotate(Rx,Ry,Rz,Obj.Triangle3D);
    geoQuadix2D  : Result.Quadix3D   := FastRotate(Rx,Ry,Rz,Obj.Quadix3D  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


function FastRotate(const Rx,Ry,Rz:Integer; const Obj:TGeometricObject; const OPoint: TPoint3D):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point3D    := FastRotate(Rx,Ry,Rz,Obj.Point3D,OPoint   );
    geoSegment2D : Result.Segment3D  := FastRotate(Rx,Ry,Rz,Obj.Segment3D,OPoint );
    geoTriangle2D: Result.Triangle3D := FastRotate(Rx,Ry,Rz,Obj.Triangle3D,OPoint);
    geoQuadix2D  : Result.Quadix3D   := FastRotate(Rx,Ry,Rz,Obj.Quadix3D,OPoint  );
  else
    Result := Obj;
  end;
end;
(* End of Fast Rotation *)


function Translate(const Dx,Dy:TFloat; const Point:TPoint2D):TPoint2D;
begin
  Result.x := Point.x + Dx;
  Result.y := Point.y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Line:TLine2D):TLine2D;
begin
  Result[1].x := Line[1].x + Dx;
  Result[1].y := Line[1].y + Dy;
  Result[2].x := Line[2].x + Dx;
  Result[2].y := Line[2].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Segment:TSegment2D):TSegment2D;
begin
  Result[1].x := Segment[1].x + Dx;
  Result[1].y := Segment[1].y + Dy;
  Result[2].x := Segment[2].x + Dx;
  Result[2].y := Segment[2].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Triangle:TTriangle2D):TTriangle2D;
begin
  Result[1].x := Triangle[1].x + Dx;
  Result[1].y := Triangle[1].y + Dy;
  Result[2].x := Triangle[2].x + Dx;
  Result[2].y := Triangle[2].y + Dy;
  Result[3].x := Triangle[3].x + Dx;
  Result[3].y := Triangle[3].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1].x := Quadix[1].x + Dx;
  Result[1].y := Quadix[1].y + Dy;
  Result[2].x := Quadix[2].x + Dx;
  Result[2].y := Quadix[2].y + Dy;
  Result[3].x := Quadix[3].x + Dx;
  Result[3].y := Quadix[3].y + Dy;
  Result[4].x := Quadix[4].x + Dx;
  Result[4].y := Quadix[4].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Rectangle:TRectangle):TRectangle;
begin
  Result[1].x := Rectangle[1].x + Dx;
  Result[1].y := Rectangle[1].y + Dy;
  Result[2].x := Rectangle[2].x + Dx;
  Result[2].y := Rectangle[2].y + Dy;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Circle:TCircle):TCircle;
begin
  Result.x      := Circle.x + Dx;
  Result.y      := Circle.y + Dy;
  Result.Radius := Circle.Radius;
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Polygon:TPolygon2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x + Dx;
    Result[i].y := Polygon[i].y + Dy;
  end;
end;
(* End of Translate *)


function Translate(const Point:TPoint2D; const Polygon:TPolygon2D):TPolygon2D;
begin
  Result:= Translate(Point.x,Point.y,Polygon);
end;
(* End of Translate *)


function Translate(const Dx,Dy:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Translate(Dx,Dy,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := Translate(Dx,Dy,Obj.Segment2D );
    geoLine2D    : Result.Line2D     := Translate(Dx,Dy,Obj.Line2D    );
    geoRectangle : Result.Rectangle  := Translate(Dx,Dy,Obj.Rectangle );
    geoTriangle2D: Result.Triangle2D := Translate(Dx,Dy,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := Translate(Dx,Dy,Obj.Quadix2D  );
    geoCircle    : Result.Circle     := Translate(Dx,Dy,Obj.Circle    );
  else
    Result := Obj;
  end;
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Point:TPoint2D):TPoint2D;
begin
  Result := Translate(Delta,Delta,Point);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Line:TLine2D):TLine2D;
begin
  Result := Translate(Delta,Delta,Line);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Segment:TSegment2D):TSegment2D;
begin
  Result := Translate(Delta,Delta,Segment);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Triangle:TTriangle2D):TTriangle2D;
begin
  Result := Translate(Delta,Delta,Triangle);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result := Translate(Delta,Delta,Quadix);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Rectangle:TRectangle):TRectangle;
begin
  Result := Translate(Delta,Delta,Rectangle);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Circle:TCircle):TCircle;
begin
  Result := Translate(Delta,Delta,Circle);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Polygon:TPolygon2D):TPolygon2D;
begin
  Result := Translate(Delta,Delta,Polygon);
end;
(* End of Translate *)


function Translate(const Delta:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  Result := Translate(Delta,Delta,Obj);
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Point:TPoint3D):TPoint3D;
begin
  Result.x := Point.x + Dx;
  Result.y := Point.y + Dy;
  Result.z := Point.z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Line:TLine3D):TLine3D;
begin
  Result[1].x := Line[1].x + Dx;
  Result[1].y := Line[1].y + Dy;
  Result[1].z := Line[1].z + Dz;
  Result[2].x := Line[2].x + Dx;
  Result[2].y := Line[2].y + Dy;
  Result[2].z := Line[2].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Segment:TSegment3D):TSegment3D;
begin
  Result[1].x := Segment[1].x + Dx;
  Result[1].y := Segment[1].y + Dy;
  Result[1].z := Segment[1].z + Dz;
  Result[2].x := Segment[2].x + Dx;
  Result[2].y := Segment[2].y + Dy;
  Result[2].z := Segment[2].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Triangle:TTriangle3D):TTriangle3D;
begin
  Result[1].x := Triangle[1].x + Dx;
  Result[1].y := Triangle[1].y + Dy;
  Result[1].z := Triangle[1].z + Dz;
  Result[2].x := Triangle[2].x + Dx;
  Result[2].y := Triangle[2].y + Dy;
  Result[2].z := Triangle[2].z + Dz;
  Result[3].x := Triangle[3].x + Dx;
  Result[3].y := Triangle[3].y + Dy;
  Result[3].z := Triangle[3].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1].x := Quadix[1].x + Dx;
  Result[1].y := Quadix[1].y + Dy;
  Result[1].z := Quadix[1].z + Dz;
  Result[2].x := Quadix[2].x + Dx;
  Result[2].y := Quadix[2].y + Dy;
  Result[2].z := Quadix[2].z + Dz;
  Result[3].x := Quadix[3].x + Dx;
  Result[3].y := Quadix[3].y + Dy;
  Result[3].z := Quadix[3].z + Dz;
  Result[4].x := Quadix[4].x + Dx;
  Result[4].y := Quadix[4].y + Dy;
  Result[4].z := Quadix[4].z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Sphere:TSphere):TSphere;
begin
  Result.x := Sphere.x + Dx;
  Result.y := Sphere.y + Dy;
  Result.z := Sphere.z + Dz;
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Polygon:TPolygon3D):TPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x + Dx;
    Result[i].y := Polygon[i].y + Dy;
    Result[i].z := Polygon[i].z + Dz;
  end;
end;
(* End of Translate *)


function Translate(const Point:TPoint3D; const Polygon:TPolygon3D):TPolygon3D;
begin
  Result := Translate(Point.x,Point.y,Point.z,Polygon);
end;
(* End of Translate *)


function Translate(const Dx,Dy,Dz:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Translate(Dx,Dy,Dz,Obj.Point3D   );
    geoSegment3D : Result.Segment3D  := Translate(Dx,Dy,Dz,Obj.Segment3D );
    geoLine3D    : Result.Line3D     := Translate(Dx,Dy,Dz,Obj.Line3D    );
    geoTriangle3D: Result.Triangle3D := Translate(Dx,Dy,Dz,Obj.Triangle3D);
    geoQuadix3D  : Result.Quadix3D   := Translate(Dx,Dy,Dz,Obj.Quadix3D  );
    geoSphere    : Result.Sphere     := Translate(Dx,Dy,Dz,Obj.Sphere    );
  else
    Result := Obj;
  end;
end;
(* End of Translate *)


function Scale(const Dx,Dy:TFloat; const Point:TPoint2D):TPoint2D;
begin
  Result.x := Point.x * Dx;
  Result.y := Point.y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TFloat; const Line:TLine2D):TLine2D;
begin
  Result[1].x := Line[1].x * Dx;
  Result[1].y := Line[1].y * Dy;
  Result[2].x := Line[2].x * Dx;
  Result[2].y := Line[2].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TFloat; const Segment:TSegment2D):TSegment2D;
begin
  Result[1].x := Segment[1].x * Dx;
  Result[1].y := Segment[1].y * Dy;
  Result[2].x := Segment[1].x * Dx;
  Result[2].y := Segment[2].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TFloat; const Triangle:TTriangle2D):TTriangle2D;
begin
  Result[1].x := Triangle[1].x * Dx;
  Result[1].y := Triangle[1].y * Dy;
  Result[2].x := Triangle[2].x * Dx;
  Result[2].y := Triangle[2].y * Dy;
  Result[3].x := Triangle[3].x * Dx;
  Result[3].y := Triangle[3].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1].x := Quadix[1].x * Dx;
  Result[1].y := Quadix[1].y * Dy;
  Result[2].x := Quadix[2].x * Dx;
  Result[2].y := Quadix[2].y * Dy;
  Result[3].x := Quadix[3].x * Dx;
  Result[3].y := Quadix[3].y * Dy;
  Result[4].x := Quadix[4].x * Dx;
  Result[4].y := Quadix[4].y * Dy;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TFloat; const Rectangle:TRectangle):TRectangle;
begin
  Result[1].x := Rectangle[1].x * Dx;
  Result[1].y := Rectangle[1].y * Dy;
  Result[2].x := Rectangle[2].x * Dx;
  Result[2].y := Rectangle[2].y * Dy;
end;
(* End of Scale*)


function Scale(const Dr:TFloat; const Circle:TCircle):TCircle;
begin
  Result.x      := Circle.x;
  Result.y      := Circle.y;
  Result.Radius := Circle.Radius * Dr;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TFloat; const Polygon:TPolygon2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x * Dx;
    Result[i].y := Polygon[i].y * Dy;
  end;
end;
(* End of Scale *)


function Scale(const Dx,Dy:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := Scale(Dx,Dy,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := Scale(Dx,Dy,Obj.Segment2D );
    geoLine2D    : Result.Line2D     := Scale(Dx,Dy,Obj.Line2D    );
    geoTriangle2D: Result.Triangle2D := Scale(Dx,Dy,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := Scale(Dx,Dy,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TFloat; const Point:TPoint3D):TPoint3D;
begin
  Result.x := Point.x * Dx;
  Result.y := Point.y * Dy;
  Result.z := Point.z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TFloat; const Line:TLine3D):TLine3D;
begin
  Result[1].x := Line[1].x * Dx;
  Result[1].y := Line[1].y * Dy;
  Result[1].z := Line[1].z * Dz;
  Result[2].x := Line[2].x * Dx;
  Result[2].y := Line[2].y * Dy;
  Result[2].z := Line[2].z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TFloat; const Segment:TSegment3D):TSegment3D;
begin
  Result[1].x := Segment[1].x * Dx;
  Result[1].y := Segment[1].y * Dy;
  Result[1].z := Segment[1].z * Dz;
  Result[2].x := Segment[2].x * Dx;
  Result[2].y := Segment[2].y * Dy;
  Result[2].z := Segment[2].z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TFloat; const Triangle:TTriangle3D):TTriangle3D;
begin
  Result[1].x := Triangle[1].x * Dx;
  Result[1].y := Triangle[1].y * Dy;
  Result[1].z := Triangle[1].z * Dz;
  Result[2].x := Triangle[2].x * Dx;
  Result[2].y := Triangle[2].y * Dy;
  Result[2].z := Triangle[2].z * Dz;
  Result[3].x := Triangle[3].x * Dx;
  Result[3].y := Triangle[3].y * Dy;
  Result[3].z := Triangle[3].z * Dz;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TFloat; const Quadix:TQuadix3D):TQuadix3D;
begin
  Result[1].x := Quadix[1].x * Dx;
  Result[1].y := Quadix[1].y * Dy;
  Result[1].z := Quadix[1].z * Dz;
  Result[2].x := Quadix[2].x * Dx;
  Result[2].y := Quadix[2].y * Dy;
  Result[2].z := Quadix[2].z * Dz;
  Result[3].x := Quadix[3].x * Dx;
  Result[3].y := Quadix[3].y * Dy;
  Result[3].z := Quadix[3].z * Dz;
  Result[4].x := Quadix[4].x * Dx;
  Result[4].y := Quadix[4].y * Dy;
  Result[4].z := Quadix[4].z * Dz;
end;
(* End of Scale *)


function Scale(const Dr:TFloat; const Sphere:TSphere):TSphere;
begin
  Result.x      := Sphere.x;
  Result.y      := Sphere.y;
  Result.z      := Sphere.z;
  Result.Radius := Sphere.Radius*Dr;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TFloat; const Polygon:TPolygon3D):TPolygon3D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i].x := Polygon[i].x * Dx;
    Result[i].y := Polygon[i].y * Dy;
  end;
end;
(* End of Scale *)


function Scale(const Dx,Dy,Dz:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint3D   : Result.Point3D    := Scale(Dx,Dy,Dz,Obj.Point3D   );
    geoSegment3D : Result.Segment3D  := Scale(Dx,Dy,Dz,Obj.Segment3D );
    geoLine3D    : Result.Line3D     := Scale(Dx,Dy,Dz,Obj.Line3D    );
    geoTriangle3D: Result.Triangle3D := Scale(Dx,Dy,Dz,Obj.Triangle3D);
    geoQuadix3D  : Result.Quadix3D   := Scale(Dx,Dy,Dz,Obj.Quadix3D  );
  else
    Result := Obj;
  end;
end;
(* End of Scale *)


procedure ShearXAxis(const Shear,x,y:TFloat; out Nx,Ny:TFloat);
begin
  Nx := x + Shear * y;
  Ny := y;
end;
(* End of Shear Cartesian Coordiante Along X-Axis *)


function ShearXAxis(const Shear:TFloat; const Point:TPoint2D):TPoint2D;
begin
  ShearXAxis(Shear,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Shear 2D Point Along X-Axis *)


function ShearXAxis(const Shear:TFloat; const Segment:TSegment2D):TSegment2D;
begin
  Result[1] := ShearXAxis(Shear,Segment[1]);
  Result[2] := ShearXAxis(Shear,Segment[2]);
end;
(* End of Shear 2D Segment Along X-Axis *)


function ShearXAxis(const Shear:TFloat; const Triangle:TTriangle2D):TTriangle2D;
begin
  Result[1] := ShearXAxis(Shear,Triangle[1]);
  Result[2] := ShearXAxis(Shear,Triangle[2]);
  Result[3] := ShearXAxis(Shear,Triangle[2]);
end;
(* End of Shear 2D Triangle Along X-Axis *)


function ShearXAxis(const Shear:TFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := ShearXAxis(Shear,Quadix[1]);
  Result[2] := ShearXAxis(Shear,Quadix[2]);
  Result[3] := ShearXAxis(Shear,Quadix[2]);
  Result[3] := ShearXAxis(Shear,Quadix[2]);
end;
(* End of Shear 2D Quadix Along X-Axis *)


function ShearXAxis(const Shear:TFloat; const Polygon:TPolygon2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := ShearXAxis(Shear,Polygon[i]);
  end;
end;
(* End of Shear 2D Polygon Along X-Axis *)


function ShearXAxis(const Shear:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := ShearXAxis(Shear,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := ShearXAxis(Shear,Obj.Segment2D );
    geoTriangle2D: Result.Triangle2D := ShearXAxis(Shear,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := ShearXAxis(Shear,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Shear 2D Along X-Axis *)


procedure ShearYAxis(const Shear,x,y:TFloat; out Nx,Ny:TFloat);
begin
  Nx := x;
  Ny := x * Shear + y;
end;
(* End of Shear Cartesian Coordiante Along Y-Axis *)


function ShearYAxis(const Shear:TFloat; const Point:TPoint2D):TPoint2D;
begin
  ShearYAxis(Shear,Point.x,Point.y,Result.x,Result.y);
end;
(* End of Shear 2D Point Along Y-Axis *)


function ShearYAxis(const Shear:TFloat; const Segment:TSegment2D):TSegment2D;
begin
  Result[1] := ShearYAxis(Shear,Segment[1]);
  Result[2] := ShearYAxis(Shear,Segment[2]);
end;
(* End of Shear 2D Segment Along Y-Axis *)


function ShearYAxis(const Shear:TFloat; const Triangle:TTriangle2D):TTriangle2D;
begin
  Result[1] := ShearYAxis(Shear,Triangle[1]);
  Result[2] := ShearYAxis(Shear,Triangle[2]);
  Result[3] := ShearYAxis(Shear,Triangle[2]);
end;
(* End of Shear 2D Triangle Along Y-Axis *)


function ShearYAxis(const Shear:TFloat; const Quadix:TQuadix2D):TQuadix2D;
begin
  Result[1] := ShearYAxis(Shear,Quadix[1]);
  Result[2] := ShearYAxis(Shear,Quadix[2]);
  Result[3] := ShearYAxis(Shear,Quadix[2]);
  Result[3] := ShearYAxis(Shear,Quadix[2]);
end;
(* End of Shear 2D Quadix Along X-Axis *)


function ShearYAxis(const Shear:TFloat; const Polygon:TPolygon2D):TPolygon2D;
var
  i : Integer;
begin
  SetLength(Result,Length(Polygon));
  for i := 0 to Length(Polygon) - 1 do
  begin
    Result[i] := ShearYAxis(Shear,Polygon[i]);
  end;
end;
(* End of Shear 2D Polygon Along Y-Axis *)


function ShearYAxis(const Shear:TFloat; const Obj:TGeometricObject):TGeometricObject;
begin
  case Obj.ObjectType of
    geoPoint2D   : Result.Point2D    := ShearYAxis(Shear,Obj.Point2D   );
    geoSegment2D : Result.Segment2D  := ShearYAxis(Shear,Obj.Segment2D );
    geoTriangle2D: Result.Triangle2D := ShearYAxis(Shear,Obj.Triangle2D);
    geoQuadix2D  : Result.Quadix2D   := ShearYAxis(Shear,Obj.Quadix2D  );
  else
    Result := Obj;
  end;
end;
(* End of Shear 2D Along Y-Axis *)


function CenterAtLocation(const Point:TPoint2D; const x,y:TFloat):TPoint2D;
begin
  Result.x := x;
  Result.y := y;
end;
(* End Of Center At Location *)


function CenterAtLocation(const Segment:TSegment2D; const x,y:TFloat):TSegment2D;
var
  Cx : TFloat;
  Cy : TFloat;
begin
  SegmentMidPoint(Segment,Cx,Cy);
  Result := Translate(x - Cx,y - Cy,Segment);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Triangle:TTriangle2D; const x,y:TFloat):TTriangle2D;
var
  Cx : TFloat;
  Cy : TFloat;
begin
  Centroid(Triangle,Cx,Cy);
  Result := Translate(x - Cx,y - Cy,Triangle);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Rectangle:TRectangle; const x,y:TFloat):TRectangle;
var
  Cx : TFloat;
  Cy : TFloat;
begin
  Cx     := abs(Rectangle[1].x - Rectangle[2].x) * 0.5;
  Cy     := abs(Rectangle[1].y - Rectangle[2].y) * 0.5;
  Result := Translate(x - Cx,y - Cy,Rectangle);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Quadix:TQuadix2D; const x,y:TFloat):TQuadix2D;
begin
 (* to Be Completed *)
end;
(* End Of Center At Location *)


function CenterAtLocation(const Circle:TCircle; const x,y:TFloat):TCircle;
begin
  Result.x := x;
  Result.y := y;
end;
(* End Of Center At Location *)


function CenterAtLocation(const Polygon:TPolygon2D; const x,y:TFloat):TPolygon2D;
var
  Cx : TFloat;
  Cy : TFloat;
begin
  Centroid(Polygon,Cx,Cy);
  Result := Translate(x - Cx,y - Cy,Polygon);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Point, CPoint:TPoint2D):TPoint2D;
begin
  Result := CenterAtLocation(Point,CPoint.x,Point.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Segment:TSegment2D; const CPoint:TPoint2D):TSegment2D;
begin
  Result := CenterAtLocation(Segment,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Triangle:TTriangle2D; const CPoint:TPoint2D):TTriangle2D;
begin
  Result := CenterAtLocation(Triangle,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Rectangle:TRectangle; const CPoint:TPoint2D):TRectangle;
begin
  Result := CenterAtLocation(Rectangle,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Quadix:TQuadix2D; const CPoint:TPoint2D):TQuadix2D;
begin
  Result := CenterAtLocation(Quadix,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Circle:TCircle; const CPoint:TPoint2D):TCircle;
begin
  Result := CenterAtLocation(Circle,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function CenterAtLocation(const Polygon:TPolygon2D; const CPoint:TPoint2D):TPolygon2D;
begin
  Result := CenterAtLocation(Polygon,CPoint.x,CPoint.y);
end;
(* End Of Center At Location *)


function AABB(const Segment : TSegment2D):TRectangle;
begin
  if Segment[1].x < Segment[2].x then
  begin
    Result[1].x := Segment[1].x;
    Result[2].x := Segment[2].x;
  end
  else
  begin
    Result[1].x := Segment[2].x;
    Result[2].x := Segment[1].x;
  end;

  if Segment[1].y < Segment[2].y then
  begin
    Result[1].y := Segment[1].y;
    Result[2].y := Segment[2].y;
  end
  else
  begin
    Result[1].y := Segment[2].y;
    Result[2].y := Segment[1].y;
  end;
end;
(* End Of AABB *)


function AABB(const Triangle : TTriangle2D):TRectangle;
var
  i  : Integer;
begin
  Result[1].x := Triangle[1].x;
  Result[1].y := Triangle[1].y;
  Result[2].x := Triangle[1].x;
  Result[2].y := Triangle[1].y;
  for i := 2 to 3 do
  begin
    if Triangle[i].x < Result[1].x then
      Result[1].x := Triangle[i].x
    else if Triangle[i].x > Result[2].x then
      Result[2].x := Triangle[i].x;
    if Triangle[i].y < Result[1].y then
      Result[1].y := Triangle[i].y
    else if Triangle[i].y > Result[2].y then
      Result[2].y := Triangle[i].y;
  end;
end;
(* End Of AABB *)


function AABB(const Rectangle : TRectangle):TRectangle;
begin
  Result[1].x := Min(Rectangle[1].x,Rectangle[2].x);
  Result[1].y := Min(Rectangle[1].y,Rectangle[2].y);
  Result[2].x := Max(Rectangle[1].x,Rectangle[2].x);
  Result[2].y := Max(Rectangle[1].y,Rectangle[2].y);
end;
(* End Of AABB *)


function AABB(const Quadix : TQuadix2D):TRectangle;
var
  i  : Integer;
begin
  Result[1].x := Quadix[1].x;
  Result[1].y := Quadix[1].y;
  Result[2].x := Quadix[1].x;
  Result[2].y := Quadix[1].y;
  for i := 2 to 4 do
  begin
    if Quadix[i].x < Result[1].x then
      Result[1].x := Quadix[i].x
    else if Quadix[i].x > Result[2].x then
      Result[2].x := Quadix[i].x;
    if Quadix[i].y < Result[1].y then
      Result[1].y := Quadix[i].y
    else if Quadix[i].y > Result[2].y then
      Result[2].y := Quadix[i].y;
  end;
end;
(* End Of AABB *)


function AABB(const Circle : TCircle):TRectangle;
begin
  Result[1].x := Circle.x - Circle.Radius;
  Result[1].y := Circle.y - Circle.Radius;
  Result[2].x := Circle.x + Circle.Radius;
  Result[2].y := Circle.y + Circle.Radius;
end;
(* End Of AABB *)


function AABB(const Polygon : TPolygon2D):TRectangle;
var
  i  : Integer;
begin
  Result[1].x := Polygon[0].x;
  Result[1].y := Polygon[0].y;
  Result[2].x := Polygon[0].x;
  Result[2].y := Polygon[0].y;
  for i := 1 to Length(Polygon) - 1 do
  begin
    if Polygon[i].x < Result[1].x then
      Result[1].x := Polygon[i].x
    else if Polygon[i].x > Result[2].x then
      Result[2].x := Polygon[i].x;
    if Polygon[i].y < Result[1].y then
      Result[1].y := Polygon[i].y
    else if Polygon[i].y > Result[2].y then
      Result[2].y := Polygon[i].y;
  end;
end;
(* End Of AABB *)


function AABB(const Curve : TPoint2DArray):TRectangle;
var
  i  : Integer;
begin
  Result[1].x := Curve[0].x;
  Result[1].y := Curve[0].y;
  Result[2].x := Curve[0].x;
  Result[2].y := Curve[0].y;
  for i := 1 to Length(Curve) - 1 do
  begin
    if Curve[i].x < Result[1].x then
      Result[1].x := Curve[i].x
    else if Curve[i].x > Result[2].x then
      Result[2].x := Curve[i].x;
    if Curve[i].y < Result[1].y then
      Result[1].y := Curve[i].y
    else if Curve[i].y > Result[2].y then
      Result[2].y := Curve[i].y;
  end;
end;
(* End Of AABB *)


procedure AABB(const Segment : TSegment2D; out x1,y1,x2,y2:TFloat);
var
   Rectangle : TRectangle;
begin
  Rectangle := AABB(Segment);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Triangle : TTriangle2D; out x1,y1,x2,y2:TFloat);
var
   Rectangle : TRectangle;
begin
  Rectangle := AABB(Triangle);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)

procedure AABB(const Rectangle : TRectangle;  out x1,y1,x2,y2:TFloat);
var
   _Rectangle : TRectangle;
begin
  _Rectangle := AABB(Rectangle);
  x1 := _Rectangle[1].x;
  y1 := _Rectangle[1].y;
  x2 := _Rectangle[2].x;
  y2 := _Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Quadix : TQuadix2D; out x1,y1,x2,y2:TFloat);
var
   Rectangle : TRectangle;
begin
  Rectangle := AABB(Quadix);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Circle : TCircle; out x1,y1,x2,y2:TFloat);
var
   Rectangle : TRectangle;
begin
  Rectangle := AABB(Circle);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Polygon : TPolygon2D; out x1,y1,x2,y2:TFloat);
var
   Rectangle : TRectangle;
begin
  Rectangle := AABB(Polygon);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure AABB(const Curve : TPoint2DArray; out x1,y1,x2,y2:TFloat);
var
   Rectangle : TRectangle;
begin
  Rectangle := AABB(Curve);
  x1 := Rectangle[1].x;
  y1 := Rectangle[1].y;
  x2 := Rectangle[2].x;
  y2 := Rectangle[2].y;
end;
(* End Of AABB *)


procedure ProjectPoint(const Srcx,Srcy,Dstx,Dsty,Dist:TFloat; out Nx,Ny:TFloat);
var
  DistRatio : TFloat;
begin
  DistRatio := Dist / Distance(Srcx,Srcy,Dstx,Dsty);
  Nx := Srcx + DistRatio * (Dstx - SrcX);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
end;
(* End of Project Point 2D *)


procedure ProjectPoint(const Srcx,Srcy,Srcz,Dstx,Dsty,Dstz,Dist:TFloat; out Nx,Ny,Nz:TFloat);
var
  DistRatio : TFloat;
begin
  DistRatio := Dist / Distance(Srcx,Srcy,Srcz,Dstx,Dsty,Dstz);
  Nx := Srcx + DistRatio * (Dstx - SrcX);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
  Nz := Srcz + DistRatio * (Dstz - Srcz);
end;
(* End of Project Point 3D *)


procedure ProjectPoint(const Px,Py,Angle,Distance:TFloat; out Nx,Ny:TFloat);
var
  Dx : TFloat;
  Dy : TFloat;
begin
  Dx := 0.0;
  Dy := 0.0;
  case Quadrant(Angle) of
    1 : begin
          Dx := Cos(Angle * PIDiv180) * Distance;
          Dy := Sin(Angle * PIDiv180) * Distance;
        end;
    2 : begin
          Dx := Sin((Angle - 90.0) * PIDiv180) * Distance * -1.0;
          Dy := Cos((Angle - 90.0) * PIDiv180) * Distance;
        end;
    3 : begin
          Dx := Cos((Angle - 180.0) * PIDiv180) * Distance * -1.0;
          Dy := Sin((Angle - 180.0) * PIDiv180) * Distance * -1.0;
        end;
    4 : begin
          Dx := Sin((Angle - 270.0) * PIDiv180) * Distance;
          Dy := Cos((Angle - 270.0) * PIDiv180) * Distance * -1.0;
        end;
   end;
   Nx := Px + Dx;
   Ny := Py + Dy;
end;
(* End of Project Point 2D *)


procedure ProjectPoint0  (const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px + Distance;
  Ny := Py;
end;
(* End of Project Point 2D *)


procedure ProjectPoint45(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px + 0.70710678118654752440084436210485 * Distance;
  Ny := Py + 0.70710678118654752440084436210485 * Distance;
end;
(* End of Project Point 2D *)


procedure ProjectPoint90(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px;
  Ny := Py + Distance;
end;
(* End of Project Point 2D *)


procedure ProjectPoint135(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px - 0.70710678118654752440084436210485 * Distance;
  Ny := Py + 0.70710678118654752440084436210485 * Distance;
end;
(* End of Project Point 2D *)


procedure ProjectPoint180(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px - Distance;
  Ny := Py;
end;
(* End of Project Point 2D *)


procedure ProjectPoint225(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px - 0.70710678118654752440084436210485 * Distance;
  Ny := Py - 0.70710678118654752440084436210485 * Distance;
end;
(* End of Project Point 2D *)


procedure ProjectPoint270(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px;
  Ny := Py - Distance;
end;
(* End of Project Point 2D *)


procedure ProjectPoint315(const Px,Py,Distance:TFloat; out Nx,Ny:TFloat);
begin
  Nx := Px + 0.70710678118654752440084436210485 * Distance;
  Ny := Py - 0.70710678118654752440084436210485 * Distance;
end;
(* End of Project Point 2D *)


function ProjectPoint(const SrcPoint,DstPoint:TPoint2D; const Dist:TFloat):TPoint2D;
begin
  ProjectPoint(SrcPoint.x,SrcPoint.y,DstPoint.x,DstPoint.y,Dist,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint(const SrcPoint,DstPoint:TPoint3D; const Dist:TFloat):TPoint3D;
begin
  ProjectPoint(SrcPoint.x,SrcPoint.y,SrcPoint.z,DstPoint.x,DstPoint.y,DstPoint.z,Dist,Result.x,Result.y,Result.z);
end;
(* End of Project Point 3D *)


function ProjectPoint(const Point:TPoint2D; Angle,Distance:TFloat):TPoint2D;
begin
  ProjectPoint(Point.x,Point.y,Angle,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint0(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint0(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint45(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint45(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint90(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint90(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint135(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint135(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint180(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint180(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint225(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint225(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint270(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint270(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectPoint315(const Point:TPoint2D; Distance:TFloat):TPoint2D;
begin
  ProjectPoint315(Point.x,Point.y,Distance,Result.x,Result.y);
end;
(* End of Project Point 2D *)


function ProjectObject(const Point:TPoint2D; Angle,Distance:TFloat):TPoint2D;
begin
  Result := ProjectPoint(Point,Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Segment:TSegment2D; Angle,Distance:TFloat):TSegment2D;
begin
  Result[1] := ProjectPoint(Segment[1],Angle,Distance);
  Result[2] := ProjectPoint(Segment[2],Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Triangle:TTriangle2D; Angle,Distance:TFloat):TTriangle2D;
begin
  Result[1] := ProjectPoint(Triangle[1],Angle,Distance);
  Result[2] := ProjectPoint(Triangle[2],Angle,Distance);
  Result[3] := ProjectPoint(Triangle[3],Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Quadix:TQuadix2D; Angle,Distance:TFloat):TQuadix2D;
begin
  Result[1] := ProjectPoint(Quadix[1],Angle,Distance);
  Result[2] := ProjectPoint(Quadix[2],Angle,Distance);
  Result[3] := ProjectPoint(Quadix[3],Angle,Distance);
  Result[4] := ProjectPoint(Quadix[4],Angle,Distance);
end;
(* End of Project Object *)


function ProjectObject(const Circle:TCircle; Angle,Distance:TFloat):TCircle;
begin
  ProjectPoint(Circle.x,Circle.y,Angle,Distance,Result.x,Result.y);
end;
(* End of Project Object *)


function ProjectObject(const Polygon : TPolygon2D; Angle,Distance:TFloat):TPolygon2D;
var
  i : Integer;
begin
  Setlength(Result,Length(Polygon));
  for i := 0 to Length(Result) -  1 do
  begin
    Result[i] := ProjectObject(Polygon[i],Angle,Distance);
  end;
end;
(* End of Project Object *)


function ProjectObject(const GeoObj : TGeometricObject; Angle,Distance:TFloat):TGeometricObject;
begin
  case GeoObj.ObjectType of
    geoPoint2D   : Result.Point2D    := ProjectObject(GeoObj.Point2D   ,Angle,Distance);
    geoSegment2D : Result.Segment2D  := ProjectObject(GeoObj.Segment2D ,Angle,Distance);
    geoTriangle2D: Result.Triangle2D := ProjectObject(GeoObj.Triangle2D,Angle,Distance);
    geoQuadix2D  : Result.Quadix2D   := ProjectObject(GeoObj.Quadix2D  ,Angle,Distance);
    geoCircle    : Result.Circle     := ProjectObject(GeoObj.Circle    ,Angle,Distance);
  else
    Result := GeoObj;
  end;
end;
(* End of Project Object *)


procedure CalculateBezierCoefficients(const Bezier:TQuadraticBezier2D; out ax,bx,ay,by:TFloat);
begin
  bx := 2.0 * (Bezier[1].x - Bezier[0].x);
  by := 2.0 * (Bezier[1].y - Bezier[0].y);
  ax := Bezier[2].x - Bezier[0].x - bx;
  ay := Bezier[2].y - Bezier[0].y - by;
end;
(* End of Calculate Bezier Values *)


procedure CalculateBezierCoefficients(const Bezier:TQuadraticBezier3D; out ax,bx,ay,by,az,bz:TFloat);
begin
  bx := 2.0 * (Bezier[1].x - Bezier[0].x);
  by := 2.0 * (Bezier[1].y - Bezier[0].y);
  bz := 2.0 * (Bezier[1].z - Bezier[0].z);
  ax := Bezier[2].x - Bezier[0].x - bx;
  ay := Bezier[2].y - Bezier[0].y - by;
  az := Bezier[2].z - Bezier[0].z - bz;
end;
(* End of Calculate Bezier Values *)


procedure CalculateBezierCoefficients(const Bezier:TCubicBezier2D; out ax,bx,cx,ay,by,cy:TFloat);
begin
  cx := 3.0 * (Bezier[1].x - Bezier[0].x);
  cy := 3.0 * (Bezier[1].y - Bezier[0].y);
  bx := 3.0 * (Bezier[2].x - Bezier[1].x) - cx;
  by := 3.0 * (Bezier[2].y - Bezier[1].y) - cy;
  ax := Bezier[3].x - Bezier[0].x - cx - bx;
  ay := Bezier[3].y - Bezier[0].y - cy - by;
end;
(* End of Calculate Bezier Values *)


procedure CalculateBezierCoefficients(const Bezier:TCubicBezier3D; out ax,bx,cx,ay,by,cy,az,bz,cz:TFloat);
begin
  cx := 3.0 * (Bezier[1].x - Bezier[0].x);
  cy := 3.0 * (Bezier[1].y - Bezier[0].y);
  cz := 3.0 * (Bezier[1].z - Bezier[0].z);
  bx := 3.0 * (Bezier[2].x - Bezier[1].x) - cx;
  by := 3.0 * (Bezier[2].y - Bezier[1].y) - cy;
  bz := 3.0 * (Bezier[2].z - Bezier[1].z) - cz;
  ax := Bezier[3].x - Bezier[0].x - cx - bx;
  ay := Bezier[3].y - Bezier[0].y - cy - by;
  az := Bezier[3].z - Bezier[0].z - cz - bz;
end;
(* End of Calculate Bezier Values *)


function PointOnBezier(const StartPoint:TPoint2D; const ax,bx,ay,by,T:TFloat):TPoint2D;
var
  tSqr  : TFloat;
begin
  tSqr     := t * t;
  Result.x := (ax * tSqr) + (bx * t) + StartPoint.x;
  Result.y := (ay * tSqr) + (by * t) + StartPoint.y;
end;
(* End of Point On Bezier *)


function PointOnBezier(const StartPoint:TPoint3D; const ax,bx,ay,by,az,bz,T:TFloat):TPoint3D;
var
  tSqr  : TFloat;
begin
  tSqr     := t * t;
  Result.x := (ax * tSqr) + (bx * t) + StartPoint.x;
  Result.y := (ay * tSqr) + (by * t) + StartPoint.y;
  Result.z := (az * tSqr) + (bz * t) + StartPoint.z;
end;
(* End of Point On Bezier *)


function PointOnBezier(const StartPoint:TPoint2D; const ax,bx,cx,ay,by,cy,T:TFloat):TPoint2D;
var
  tSqr  : TFloat;
  tCube : TFloat;
begin
  tSqr     := t * t;
  tCube    := tSqr * t;
  Result.x := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.x;
  Result.y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.y;
end;
(* End of Point On Bezier *)


function PointOnBezier(const StartPoint:TPoint3D; const ax,bx,cx,ay,by,cy,az,bz,cz,T:TFloat):TPoint3D;
var
  tSqr  : TFloat;
  tCube : TFloat;
begin
  tSqr     := t * t;
  tCube    := tSqr * t;
  Result.x := (ax * tCube) + (bx * tSqr) + (cx * t) + StartPoint.x;
  Result.y := (ay * tCube) + (by * tSqr) + (cy * t) + StartPoint.y;
  Result.z := (az * tCube) + (bz * tSqr) + (cz * t) + StartPoint.z;
end;
(* End of Point On Bezier *)


function CreateBezier(const Bezier:TQuadraticBezier2D; const PointCount:Integer) : TPoint2DArray;
var
  ax : TFloat;
  ay : TFloat;
  bx : TFloat;
  by : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,ay,by,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateBezier(const Bezier:TQuadraticBezier3D; const PointCount:Integer) : TPoint3DArray;
var
  ax : TFloat;
  ay : TFloat;
  az : TFloat;
  bx : TFloat;
  by : TFloat;
  bz : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by,az,bz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,ay,by,az,bz,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateBezier(const Bezier:TCubicBezier2D; const PointCount:Integer) : TPoint2DArray;
var
  ax : TFloat;
  bx : TFloat;
  cx : TFloat;
  ay : TFloat;
  by : TFloat;
  cy : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateBezier(const Bezier:TCubicBezier3D; const PointCount:Integer) : TPoint3DArray;
var
  ax : TFloat;
  bx : TFloat;
  cx : TFloat;
  ay : TFloat;
  by : TFloat;
  cy : TFloat;
  az : TFloat;
  bz : TFloat;
  cz : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy,az,bz,cz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,az,bz,cz,T);
    T := T + dT;
  end;
end;
(* End of Create Bezier *)


function CreateCurvePointBezier(const Bezier:TQuadraticBezier2D; const PointCount:Integer):TCurvePoint2DArray;
var
  ax : TFloat;
  ay : TFloat;
  bx : TFloat;
  by : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,ay,by,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CreateCurvePointBezier(const Bezier:TQuadraticBezier3D; const PointCount:Integer):TCurvePoint3DArray;
var
  ax : TFloat;
  ay : TFloat;
  az : TFloat;
  bx : TFloat;
  by : TFloat;
  bz : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,ay,by,az,bz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,ay,by,az,bz,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CreateCurvePointBezier(const Bezier:TCubicBezier2D; const PointCount:Integer):TCurvePoint2DArray;
var
  ax : TFloat;
  bx : TFloat;
  cx : TFloat;
  ay : TFloat;
  by : TFloat;
  cy : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CreateCurvePointBezier(const Bezier:TCubicBezier3D; const PointCount:Integer):TCurvePoint3DArray;
var
  ax : TFloat;
  bx : TFloat;
  cx : TFloat;
  ay : TFloat;
  by : TFloat;
  cy : TFloat;
  az : TFloat;
  bz : TFloat;
  cz : TFloat;
  dT : TFloat;
  T  : TFloat;
  i  : Integer;
begin
  if PointCount = 0 then exit;
  dT := 1.0 / (1.0 * PointCount - 1.0);
  T  := 0.0;
  SetLength(Result,PointCount);
  CalculateBezierCoefficients(Bezier,ax,bx,cx,ay,by,cy,az,bz,cz);
  for i := 0 to PointCount - 1 do
  begin
    Result[i] := EquateCurvePoint(PointOnBezier(Bezier[0],ax,bx,cx,ay,by,cy,az,bz,cz,T),T);
    T := T + dT;
  end;
end;
(* End of Create Curve Point Bezier *)


function CurveLength(const Bezier:TQuadraticBezier2D; const PointCount:Integer):TFloat;
var
   i     : Integer;
   Curve : TPoint2DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


function CurveLength(const Bezier:TQuadraticBezier3D; const PointCount:Integer):TFloat;
var
   i     : Integer;
   Curve : TPoint3DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


function CurveLength(const Bezier:TCubicBezier2D; const PointCount:Integer):TFloat;
var
   i     : Integer;
   Curve : TPoint2DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


function CurveLength(const Bezier:TCubicBezier3D; const PointCount:Integer):TFloat;
var
   i     : Integer;
   Curve : TPoint3DArray;
begin
  Result := 0;
  Curve  := CreateBezier(Bezier,PointCount);
  for i := 0 To Length(Curve) - 2 do
  begin
    Result := Result + Distance(Curve[i],Curve[i + 1]);
  end;
  Finalize(Curve);
end;
(* End of Curve Length *)


procedure ShortenSegment(const Amount:TFloat; out x1,y1,x2,y2:TFloat);
var
  SegmentLength : TFloat;
  DistRatio     : TFloat;
  Dx            : TFloat;
  Dy            : TFloat;
begin
  SegmentLength := Distance(x1,y1,x2,y2);

  if SegmentLength < Amount then
  begin
    SegmentMidPoint(x1,y1,x2,y2,x1,y1);
    x2 := x1;
    y2 := y1;
    Exit;
  end;

  DistRatio := Amount / (2 * SegmentLength);
  Dx        := x2 - x1;
  Dy        := y2 - y1;

  x1 := x1 + DistRatio * Dx;
  y1 := y1 + DistRatio * Dy;
  x2 := x2 - DistRatio * Dx;
  y2 := y2 - DistRatio * Dy;
end;
(* End of Shorten Segment *)


procedure ShortenSegment(const Amount:TFloat; out x1,y1,z1,x2,y2,z2:TFloat);
var
  SegmentLength : TFloat;
  DistRatio     : TFloat;
  Dx            : TFloat;
  Dy            : TFloat;
  Dz            : TFloat;
begin
  SegmentLength := Distance(x1,y1,z1,x2,y2,z2);

  if SegmentLength <= Amount then
  begin
    SegmentMidPoint(x1,y1,z1,x2,y2,z2,x1,y1,z1);
    x2 := x1;
    y2 := y1;
    z2 := z1;
    Exit;
  end;

  DistRatio := Amount / (2 * SegmentLength);
  Dx        := x2 - x1;
  Dy        := y2 - y1;
  Dz        := z2 - z1;

  x1 := x1 + DistRatio * Dx;
  y1 := y1 + DistRatio * Dy;
  z1 := z1 + DistRatio * Dz;
  x2 := x2 - DistRatio * Dx;
  y2 := y2 - DistRatio * Dy;
  z2 := z2 - DistRatio * Dz;
end;
(* End of Shorten Segment *)


function ShortenSegment(const Segment:TSegment2D; const Amount : TFloat):TSegment2D;
begin
  Result := Segment;
  ShortenSegment(Amount,Result[1].x,Result[1].y,Result[2].x,Result[2].y);
end;
(* End of Shorten Segment *)


function ShortenSegment(const Segment:TSegment3D; const Amount : TFloat):TSegment3D;
begin
  Result := Segment;
  ShortenSegment(Amount,Result[1].x,Result[1].y,Result[1].z,Result[2].x,Result[2].y,Result[2].z);
end;
(* End of Shorten Segment *)


procedure LengthenSegment(const Amount:TFloat; out x1,y1,x2,y2:TFloat);
var
  SegmentLength : TFloat;
  DistRatio     : TFloat;
  Dx            : TFloat;
  Dy            : TFloat;
begin
  SegmentLength := Distance(x1,y1,x2,y2);

  DistRatio := Amount / (2 * SegmentLength);
  Dx        := x2 - x1;
  Dy        := y2 - y1;

  x1 := x1 - DistRatio * Dx;
  y1 := y1 - DistRatio * Dy;
  x2 := x2 + DistRatio * Dx;
  y2 := y2 + DistRatio * Dy;
end;
(* End of Lengthen Segment *)


procedure LengthenSegment(const Amount:TFloat; out x1,y1,z1,x2,y2,z2:TFloat);
var
  SegmentLength : TFloat;
  DistRatio     : TFloat;
  Dx            : TFloat;
  Dy            : TFloat;
  Dz            : TFloat;
begin
  SegmentLength := Distance(x1,y1,z1,x2,y2,z2);

  DistRatio := Amount / (2 * SegmentLength);
  Dx        := x2 - x1;
  Dy        := y2 - y1;
  Dz        := z2 - z1;

  x1 := x1 - DistRatio * Dx;
  y1 := y1 - DistRatio * Dy;
  z1 := z1 - DistRatio * Dz;
  x2 := x2 + DistRatio * Dx;
  y2 := y2 + DistRatio * Dy;
  z2 := z2 + DistRatio * Dz;
end;
(* End of Lengthen Segment *)


function LengthenSegment(const Segment:TSegment2D; const Amount:TFloat):TSegment2D;
begin
  Result := Segment;
  LengthenSegment(Amount,Result[1].x,Result[1].y,Result[2].x,Result[2].y);
end;
(* End of Lengthen Segment *)


function LengthenSegment(const Segment:TSegment3D; const Amount:TFloat):TSegment3D;
begin
  Result := Segment;
  LengthenSegment(Amount,Result[1].x,Result[1].y,Result[1].z,Result[2].x,Result[2].y,Result[2].z);
end;
(* End of Lengthen Segment *)


function EquatePoint(const x,y:TFloat):TPoint2D;
begin
  Result.x := x;
  Result.y := y;
end;
(* End of Equate Point *)


function EquatePoint(const x,y,z:TFloat):TPoint3D;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;
(* End of Equate Point *)


function EquatePointPtr(const x,y:TFloat):TPoint2DPtr;
begin
  New(Result);
  Result^ := EquatePoint(x,y);
end;
(* End of Equate Point *)


function EquatePointPtr(const x,y,z:TFloat):TPoint3DPtr;
begin
  New(Result);
  Result^ := EquatePoint(x,y,z);
end;
(* End of Equate Point *)


procedure EquatePoint(const x,y:TFloat; out Point:TPoint2D);
begin
  Point.x := x;
  Point.y := y;
end;
(* End of Equate Point *)


procedure EquatePoint(const x,y,z:TFloat; out Point:TPoint3D);
begin
  Point.x := x;
  Point.y := y;
  Point.z := z;
end;
(* End of Equate Point *)


procedure EquatePointPtr(const x,y:TFloat; out Point:TPoint2DPtr);
begin
 Point := EquatePointPtr(x,y);
end;
(* End of Equate Point *)


procedure EquatePointPtr(const x,y,z:TFloat; out Point:TPoint3DPtr);
begin
 Point := EquatePointPtr(x,y,z);
end;
(* End of Equate Point *)


function EquateCurvePoint(x,y,t:TFloat):TCurvePoint2D;
begin
  Result.x := x;
  Result.y := y;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateCurvePoint(x,y,z,t:TFloat):TCurvePoint3D;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateCurvePoint(Point:TPoint2D; t:TFloat):TCurvePoint2D;
begin
  Result.x := Point.x;
  Result.y := Point.y;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateCurvePoint(Point:TPoint3D; t:TFloat):TCurvePoint3D;
begin
  Result.x := Point.x;
  Result.y := Point.y;
  Result.z := Point.z;
  Result.t := t;
end;
(* End of Equate Curve Point *)


function EquateSegment(const x1,y1,x2,y2:TFloat):TSegment2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
end;
(* End of Equate Segment *)


function EquateSegment(const Point1,Point2:TPoint2D):TSegment2D;
begin
  Result := EquateSegment(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Equate Segment *)


function EquateSegment(const Point1,Point2:TPoint3D):TSegment3D;
begin
  Result := EquateSegment(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z);
end;
(* End of Equate Segment *)


function EquateSegment(const x1,y1,z1,x2,y2,z2:TFloat):TSegment3D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[1].z := z1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[2].z := z2;
end;
(* End of EquateSegment *)


procedure EquateSegment(const x1,y1,x2,y2:TFloat; out Segment:TSegment2D);
begin
  Segment[1].x := x1;
  Segment[1].y := y1;
  Segment[2].x := x2;
  Segment[2].y := y2;
end;
(* End of Equate Segment *)


procedure EquateSegment(const x1,y1,z1,x2,y2,z2:TFloat; out Segment:TSegment3D);
begin
  Segment[1].x := x1;
  Segment[1].y := y1;
  Segment[1].z := z1;
  Segment[2].x := x2;
  Segment[2].y := y2;
  Segment[2].z := z2;
end;
(* End of Equate Segment *)


function EquateLine(const x1,y1,x2,y2:TFloat):TLine2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
end;
(* End of Equate Segment *)


function EquateLine(const x1,y1,z1,x2,y2,z2:TFloat):TLine3D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[1].z := z1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[2].z := z2;
end;
(* End of Equate Segment *)


function EquateLine(const Point1,Point2:TPoint2D):TLine2D;
begin
  Result := EquateLine(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Equate Segment *)


function EquateLine(const Point1,Point2:TPoint3D):TLine3D;
begin
  Result := EquateLine(Point1.x,Point1.y,Point1.z,Point1.x,Point2.y,Point2.z);
end;
(* End of Equate Segment *)


procedure EquateLine(const x1,y1,x2,y2:TFloat; out Line:TLine2D);
begin
  Line[1].x := x1;
  Line[1].y := y1;
  Line[2].x := x2;
  Line[2].y := y2;
end;
(* End of Equate Segment *)


procedure EquateLine(const x1,y1,z1,x2,y2,z2:TFloat; out Line:TLine3D);
begin
  Line[1].x := x1;
  Line[1].y := y1;
  Line[1].z := z1;
  Line[2].x := x2;
  Line[2].y := y2;
  Line[2].z := z2;
end;
(* End of Equate Segment *)


function EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TQuadix2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[3].x := x3;
  Result[3].y := y3;
  Result[4].x := x4;
  Result[4].y := y4;
end;
(* End of Equate Quadix *)


function EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat):TQuadix3D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[1].z := z1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[2].z := z2;
  Result[3].x := x3;
  Result[3].y := y3;
  Result[3].z := z3;
  Result[4].x := x4;
  Result[4].y := y4;
  Result[4].z := z4;
end;
(* End of Equate Quadix *)


function EquateQuadix(const Point1,Point2,Point3,Point4:TPoint2D):TQuadix2D;
begin
  Result := EquateQuadix(
                         Point1.x,Point1.y,
                         Point2.x,Point2.y,
                         Point3.x,Point3.y,
                         Point4.x,Point4.y
                        );
end;
(* End of Equate Quadix *)


function EquateQuadix(const Point1,Point2,Point3,Point4:TPoint3D):TQuadix3D;
begin
  Result := EquateQuadix(
                         Point1.x,Point1.y,Point1.z,
                         Point2.x,Point2.y,Point1.z,
                         Point3.x,Point3.y,Point1.z,
                         Point4.x,Point4.y,Point1.z
                        );
end;
(* End of Equate Quadix *)


procedure EquateQuadix(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; out Quadix:TQuadix2D);
begin
  Quadix[1].x := x1;
  Quadix[1].y := y1;
  Quadix[2].x := x2;
  Quadix[2].y := y2;
  Quadix[3].x := x3;
  Quadix[3].y := y3;
  Quadix[4].x := x4;
  Quadix[4].y := y4;
end;
(* End of Equate Quadix *)


procedure EquateQuadix(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; out Quadix:TQuadix3D);
begin
  Quadix[1].x := x1;
  Quadix[1].y := y1;
  Quadix[1].z := z1;
  Quadix[2].x := x2;
  Quadix[2].y := y2;
  Quadix[2].z := z2;
  Quadix[3].x := x3;
  Quadix[3].y := y3;
  Quadix[3].z := z3;
  Quadix[4].x := x4;
  Quadix[4].y := y4;
  Quadix[4].z := z4;
end;
(* End of Equate Quadix *)


function EquateRectangle(const x1,y1,x2,y2:TFloat):TRectangle;
begin
  if x1 <= x2 then
  begin
    Result[1].x := x1;
    Result[2].x := x2;
  end
  else
  begin
    Result[1].x := x2;
    Result[2].x := x1;
  end;

  if y1 <= y2 then
  begin
    Result[1].y := y1;
    Result[2].y := y2;
  end
  else
  begin
    Result[1].y := y2;
    Result[2].y := y1;
  end;
end;
(* End of Equate Rectangle *)


function EquateRectangle(const Point1,Point2:TPoint2D):TRectangle;
begin
  Result := EquateRectangle(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of Equate Rectangle *)


procedure EquateRectangle(const x1,y1,x2,y2:TFloat; out Rect:TRectangle);
begin
  if x1 <= x2 then
  begin
    Rect[1].x := x1;
    Rect[2].x := x2;
  end
  else
  begin
    Rect[1].x := x2;
    Rect[2].x := x1;
  end;

  if y1 <= y2 then
  begin
    Rect[1].y := y1;
    Rect[2].y := y2;
  end
  else
  begin
    Rect[1].y := y2;
    Rect[2].y := y1;
  end;
end;
(* End of Equate Rectangle *)


procedure EquateRectangle(const Point1,Point2:TPoint2D; out Rect:TRectangle);
begin
  EquateRectangle(Point1.x,Point1.y,Point2.x,Point2.y,Rect);
end;
(* End of Equate Rectangle *)


function EquateTriangle(const x1,y1,x2,y2,x3,y3:TFloat):TTriangle2D;
begin
  Result[1].x := x1;
  Result[1].y := y1;
  Result[2].x := x2;
  Result[2].y := y2;
  Result[3].x := x3;
  Result[3].y := y3;
end;
(* End of Equate Triangle *)


function EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):TTriangle3D;
begin
  Result[1].x := x1;
  Result[2].x := x2;
  Result[3].x := x3;
  Result[1].y := y1;
  Result[2].y := y2;
  Result[3].y := y3;
  Result[1].z := z1;
  Result[2].z := z2;
  Result[3].z := z3;
end;
(* End of Equate Triangle *)

function EquateTriangle(const Point1,Point2,Point3: TPoint2D):TTriangle2D;
begin
  Result[1] := Point1;
  Result[2] := Point2;
  Result[3] := Point3;
end;
(* End of Equate Triangle *)


function EquateTriangle(const Point1,Point2,Point3: TPoint3D):TTriangle3D;
begin
 Result[1] := Point1;
 Result[2] := Point2;
 Result[3] := Point3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const x1,y1,x2,y2,x3,y3:TFloat; out Triangle:TTriangle2D);
begin
 Triangle[1].x := x1;
 Triangle[2].x := x2;
 Triangle[3].x := x3;
 Triangle[1].y := y1;
 Triangle[2].y := y2;
 Triangle[3].y := y3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat; out Triangle:TTriangle3D);
begin
  Triangle[1].x := x1;
  Triangle[2].x := x2;
  Triangle[3].x := x3;
  Triangle[1].y := y1;
  Triangle[2].y := y2;
  Triangle[3].y := y3;
  Triangle[1].z := z1;
  Triangle[2].z := z2;
  Triangle[3].z := z3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const Point1,Point2,Point3:TPoint2D; out Triangle:TTriangle2D);
begin
  Triangle[1] := Point1;
  Triangle[2] := Point2;
  Triangle[3] := Point3;
end;
(* End of Equate Triangle *)


procedure EquateTriangle(const Point1,Point2,Point3:TPoint3D; out Triangle:TTriangle3D);
begin
  Triangle[1] := Point1;
  Triangle[2] := Point2;
  Triangle[3] := Point3;
end;
(* End of Equate Triangle *)


function EquateCircle(const x,y,r:TFloat):TCircle;
begin
  Result.x      := x;
  Result.y      := y;
  Result.Radius := r;
end;
(* End of Equate Circle *)


function EquateCircle(const Point:TPoint2D; Radius:TFloat):TCircle;
begin
  Result := EquateCircle(Point.x,Point.y,Radius);
end;
(* End of Equate Circle *)


procedure EquateCircle(const x,y,r:TFloat; out Circle:TCircle);
begin
  Circle.x      := x;
  Circle.y      := y;
  Circle.Radius := r;
end;
(* End of Equate Circle *)


function EquateSphere(const x,y,z,r:TFloat):TSphere;
begin
  Result.x      := x;
  Result.y      := y;
  Result.z      := z;
  Result.Radius := r;
end;
(* End of Equate Sphere *)


procedure EquateSphere(const x,y,z,r:TFloat; out Sphere:TSphere);
begin
  Sphere.x      := x;
  Sphere.y      := y;
  Sphere.z      := z;
  Sphere.Radius := r;
end;
(* End of Equate Sphere *)


function EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat):TPlane2D;
begin
  Result.a := y1 * (z2 - z3) + y2 * (z3 - z1) + y3 * (z1 - z2);
  Result.b := z1 * (x2 - x3) + z2 * (x3 - x1) + z3 * (x1 - x2);
  Result.c := x1 * (y2 - y3) + x2 * (y3 - y1) + x3 * (y1 - y2);
  Result.d := -(x1 * (y2 * z3 - y3 * z2) + x2 * (y3 * z1 - y1 * z3) + x3 * (y1 * z2 - y2 * z1));
end;
(* End of Equate Plane *)


function EquatePlane(const Point1,Point2,Point3:TPoint3D):TPlane2D;
begin
  Result := EquatePlane(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z);
end;
(* End of Equate Plane *)


procedure EquatePlane(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat; out Plane:TPlane2D);
begin
  Plane := EquatePlane(x1,y1,z1,x2,y2,z2,x3,y3,z3);
end;
(* End of Equate Plane *)


procedure EquatePlane(const Point1,Point2,Point3:TPoint3D; out Plane:TPlane2D);
begin
  EquatePlane(Point1.x,Point1.y,Point1.z,Point2.x,Point2.y,Point2.z,Point3.x,Point3.y,Point3.z,Plane);
end;
(* End of Equate Plane *)


procedure EquateBezier(const x1,y1,x2,y2,x3,y3:TFloat; out Bezier:TQuadraticBezier2D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
end;
(* End of Equate Bezier *)


procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3:TFloat; out Bezier:TQuadraticBezier3D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[0].z := z1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[1].z := z2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
  Bezier[2].z := z3;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3:TPoint2D):TQuadraticBezier2D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3:TPoint3D):TQuadraticBezier3D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
end;
(* End of Equate Bezier *)


procedure EquateBezier(const x1,y1,x2,y2,x3,y3,x4,y4:TFloat; out Bezier:TCubicBezier2D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
  Bezier[3].x := x4;
  Bezier[3].y := y4;
end;
(* End of Equate Bezier *)


procedure EquateBezier(const x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4:TFloat; out Bezier:TCubicBezier3D);
begin
  Bezier[0].x := x1;
  Bezier[0].y := y1;
  Bezier[0].z := z1;
  Bezier[1].x := x2;
  Bezier[1].y := y2;
  Bezier[1].z := z2;
  Bezier[2].x := x3;
  Bezier[2].y := y3;
  Bezier[2].z := z3;
  Bezier[3].x := x4;
  Bezier[3].y := y4;
  Bezier[3].z := z4;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TPoint2D):TCubicBezier2D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
  Result[3] := Pnt4;
end;
(* End of Equate Bezier *)


function EquateBezier(const Pnt1,Pnt2,Pnt3,Pnt4:TPoint3D):TCubicBezier3D;
begin
  Result[0] := Pnt1;
  Result[1] := Pnt2;
  Result[2] := Pnt3;
  Result[3] := Pnt4;
end;
(* End of Equate Bezier *)


function RectangleToQuadix(x1,y1,x2,y2:TFloat):TQuadix2D;
begin
  Result[1] := EquatePoint(x1,y1);
  Result[2] := EquatePoint(x2,y1);
  Result[3] := EquatePoint(x2,y2);
  Result[4] := EquatePoint(x1,y2);
end;
(* End of RectangleToQuadix *)


function RectangleToQuadix(Point1,Point2:TPoint2D):TQuadix2D;
begin
  Result := RectangleToQuadix(Point1.x,Point1.y,Point2.x,Point2.y);
end;
(* End of RectangleToQuadix *)


function RectangleToQuadix(Rectangle:TRectangle):TQuadix2D;
begin
  Result := RectangleToQuadix(Rectangle[1],Rectangle[2]);
end;
(* End of RectangleToQuadix *)


function TriangleToPolygon(x1,y1,x2,y2,x3,y3:TFloat):TPolygon2D;
begin
  SetLength(Result,3);
  Result[0] := EquatePoint(x1,y1);
  Result[1] := EquatePoint(x2,y2);
  Result[2] := EquatePoint(x3,y3);
end;
(* End of TriangleToPolygon *)


function TriangleToPolygon(Triangle:TTriangle2D):TPolygon2D;
begin
  Result := TriangleToPolygon(Triangle[1].x,Triangle[1].y,
                              Triangle[2].x,Triangle[2].y,
                              Triangle[3].x,Triangle[3].y);
end;
(* End of TriangleToPolygon *)


function QuadixToPolygon(x1,y1,x2,y2,x3,y3,x4,y4:TFloat):TPolygon2D;
begin
  SetLength(Result,4);
  Result[0] := EquatePoint(x1,y1);
  Result[1] := EquatePoint(x2,y2);
  Result[2] := EquatePoint(x3,y3);
  Result[3] := EquatePoint(x4,y4);
end;
(* QuadixToPolygon *)


function QuadixToPolygon(Quadix:TQuadix2D):TPolygon2D;
begin
  Result := QuadixToPolygon(Quadix[1].x,Quadix[1].y,
                            Quadix[2].x,Quadix[2].y,
                            Quadix[3].x,Quadix[3].y,
                            Quadix[4].x,Quadix[4].y);
end;
(* End of QuadixToPolygon *)


function CircleToPolygon(const Cx,Cy,Radius:TFloat; const PointCount:Integer):TPolygon2D;
var
  i     : Integer;
  Angle : TFloat;
begin
  SetLength(Result,PointCount);
  Angle := 360.0 / (1.0 * PointCount);
  for i := 0 to PointCount - 1 do
  begin
    Rotate(Angle * i,Cx + Radius, Cy,Cx,Cy,Result[i].x,Result[i].y);
  end
end;
(* End of Circle To Polygon *)


function CircleToPolygon(const Circle:TCircle; const PointCount:Integer):TPolygon2D;
begin
  Result := CircleToPolygon(Circle.x,Circle.y,Circle.Radius,PointCount);
end;
(* End of Circle To Polygon *)


procedure SetGeometricObject(const Primitive:TPoint2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoPoint2D;
  GeoObj.Point2D    := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TPoint3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoPoint3D;
  GeoObj.Point3D    := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TLine2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoLine2D;
  GeoObj.Line2D     := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TLine3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoLine3D;
  GeoObj.Line3D     := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TSegment2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoSegment2D;
  GeoObj.Segment2D  := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TSegment3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoSegment3D;
  GeoObj.Segment3D  := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TTriangle2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoTriangle2D;
  GeoObj.Triangle2D := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TTriangle3D; out GeoObj:TGeometricObject);
begin
 GeoObj.ObjectType := geoTriangle3D;
 GeoObj.Triangle3D := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TQuadix2D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoQuadix2D;
  GeoObj.Quadix2D   := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TQuadix3D; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoQuadix3D;
  GeoObj.Quadix3D   := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TRectangle; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoRectangle;
  GeoObj.Rectangle  := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TCircle; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoCircle;
  GeoObj.Circle     := Primitive;
end;
(* End of Set Geometric Object *)


procedure SetGeometricObject(const Primitive:TSphere; out GeoObj:TGeometricObject);
begin
  GeoObj.ObjectType := geoSphere;
  GeoObj.Sphere     := Primitive;
end;
(* End of Set Geometric Object *)


function GenerateRandomValue(Range : TFloat; Resolution : TFloat = 10000000.0) : TFloat;
begin
  (* { Result e R : 0 <= Result < Range } *)
  if Range > 1.0 then
    Result := (1.0 * Random(Round(Range) - 1)) + ((1.0 * Random(Round(Resolution))) / Resolution)
  else
    Result := ((1.0 * Random(Round(Resolution))) / Resolution);
end;
(* End of Generate Random Value *)


procedure GenerateRandomPoints(const Bx1,By1,Bx2,By2:TFloat; var PointList: array of TPoint2D);
var
  i  : Integer;
  Dx : TFloat;
  Dy : TFloat;
begin
  Randomize;
  Dx  := Abs(Bx2 - Bx1);
  Dy  := Abs(By2 - By1);
  for i := 0 to Length(PointList) - 1 do
  begin
    PointList[i].x := Bx1 + GenerateRandomValue(Dx);
    PointList[i].y := By1 + GenerateRandomValue(Dy);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Rectangle:TRectangle; var PointList: array of TPoint2D);
begin
  GenerateRandomPoints(Rectangle[1].x,Rectangle[1].y,Rectangle[2].x,Rectangle[2].y,PointList);
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Segment:TSegment2D; var PointList:array of TPoint2D);
var
  i     : Integer;
  Dist  : TFloat;
  Ratio : TFloat;
begin
  Dist := Distance(Segment);
  for i := 0 to Length(PointList) - 1 do
  begin
    Ratio := GenerateRandomValue(Dist) / Dist;
    PointList[i].x := Segment[1].x + Ratio * (Segment[2].x - Segment[1].x);
    PointList[i].y := Segment[1].y + Ratio * (Segment[2].y - Segment[1].y);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Segment:TSegment3D; var PointList:array of TPoint3D);
var
  i     : Integer;
  Dist  : TFloat;
  Ratio : TFloat;
begin
  Dist := Distance(Segment);
  for i := 0 to Length(PointList) - 1 do
  begin
    Ratio := GenerateRandomValue(Dist) / Dist;
    PointList[i].x := Segment[1].x + Ratio * (Segment[2].x - Segment[1].x);
    PointList[i].y := Segment[1].y + Ratio * (Segment[2].y - Segment[1].y);
    PointList[i].z := Segment[1].z + Ratio * (Segment[2].z - Segment[1].z);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Triangle:TTriangle2D; var PointList:array of TPoint2D);
var
  a : TFloat;
  b : TFloat;
  c : TFloat;
  i : Integer;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    a := RandomValue;
    b := RandomValue;
    if (a + b) > 1 then
    begin
      a := 1 - a;
      b := 1 - b;
    end;
    c := (1 - a - b);
    PointList[i].x := (Triangle[1].x * a) + (Triangle[2].x * b) + (Triangle[3].x * c);
    PointList[i].y := (Triangle[1].y * a) + (Triangle[2].y * b) + (Triangle[3].y * c);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Circle:TCircle; var PointList:array of TPoint2D);
var
  i           : Integer;
  RandomAngle : TFloat;
  CPoint      : TPoint2D;
begin
  CPoint := EquatePoint(Circle.x, Circle.y);
  for i := 0 to Length(PointList) - 1 do
  begin
    RandomAngle := GenerateRandomValue(360);
    PointList[i].x  := Circle.x + Circle.Radius * Sqrt(RandomValue);
    PointList[i].y  := Circle.y;
    PointList[i]    := Rotate(RandomAngle, PointList[i], CPoint);
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPoints(const Quadix:TQuadix2D; var PointList : array of TPoint2D);
(*  Note: It is assumed the input Quadix is convex. *)
var
  i  : Integer;
  a  : TFloat;
  b  : TFloat;
  a1 : TFloat;
  b1 : TFloat;
  a2 : TFloat;
  b2 : TFloat;
  r1 : TFloat;
  r2 : TFloat;
  r3 : TFloat;
  r4 : TFloat;
begin
  for i := 0 to Length(PointList) - 1 do
  begin
    a := (2 * RandomValue) - 1;
    b := (2 * RandomValue) - 1;

    a1 := 1 - a;
    a2 := 1 + a;

    b1 := 1 - b;
    b2 := 1 + b;

    r1 := a1 * b1;
    r2 := a2 * b1;
    r3 := a2 * b2;
    r4 := a1 * b2;

    PointList[i].x := ((r1 * Quadix[1].x) + (r2 * Quadix[2].x) + (r3 * Quadix[3].x) + (r4 * Quadix[4].x)) * 0.25;
    PointList[i].y := ((r1 * Quadix[1].y) + (r2 * Quadix[2].y) + (r3 * Quadix[3].y) + (r4 * Quadix[4].y)) * 0.25;
  end;
end;
(* End of Generate Random Points *)


procedure GenerateRandomPointsOnConvexPentagon(const Pentagon : TPolygon2D; var PointList : array of TPoint2D);
var
  i            : Integer;
  Triangle     : TTriangle2D;
  Quadix       : TQuadix2D;
  QRndPoint    : array of TPoint2D;
  TRndPoint    : array of TPoint2D;
  QRndPntCnt   : Integer;
  TRndPntCnt   : Integer;
begin
  if Length(Pentagon) <> 5       then Exit;
  if not ConvexPolygon(Pentagon) then Exit;
  Quadix     := EquateQuadix(Pentagon[0],Pentagon[1],Pentagon[2],Pentagon[3]);
  Triangle   := EquateTriangle(Pentagon[3],Pentagon[4],Pentagon[0]);
  QRndPntCnt := Round(1.0 * Length(PointList) * (Area(Quadix) / Area(Pentagon)));
  TRndPntCnt := Length(PointList) - QRndPntCnt;
  SetLength(QRndPoint,QRndPntCnt);
  SetLength(TRndPoint,TRndPntCnt);
  GenerateRandomPoints(Quadix,QRndPoint);
  GenerateRandomPoints(Triangle,TRndPoint);
  for i := 0 to Length(QRndPoint) - 1 do PointList[i                    ] := QRndPoint[i];
  for i := 0 to Length(TRndPoint) - 1 do PointList[i + Length(QRndPoint)] := TRndPoint[i];
  finalize(QRndPoint);
  finalize(TRndPoint);
end;
(* End of Generate Random Points On Convex Pentagon *)


procedure GenerateRandomPointsOnConvexHexagon(const Hexagon : TPolygon2D; var PointList : array of TPoint2D);
var
  i           : Integer;
  Quadix1     : TQuadix2D;
  Quadix2     : TQuadix2D;
  Q1RndPoint  : array of TPoint2D;
  Q2RndPoint  : array of TPoint2D;
  Q1RndPntCnt : Integer;
  Q2RndPntCnt : Integer;
begin
  if Length(Hexagon) <> 6       then Exit;
  if not ConvexPolygon(Hexagon) then Exit;
  Quadix1      := EquateQuadix(Hexagon[0],Hexagon[1],Hexagon[2],Hexagon[5]);
  Quadix2      := EquateQuadix(Hexagon[2],Hexagon[3],Hexagon[4],Hexagon[5]);
  Q1RndPntCnt  := Round(1.0 * Length(PointList) * (Area(Quadix1) / Area(Hexagon)));
  Q2RndPntCnt  := Length(PointList) - Q1RndPntCnt;
  SetLength(Q1RndPoint,Q1RndPntCnt);
  SetLength(Q2RndPoint,Q2RndPntCnt);
  GenerateRandomPoints(Quadix1,Q1RndPoint);
  GenerateRandomPoints(Quadix2,Q2RndPoint);
  for i := 0 to Length(Q1RndPoint) - 1 do PointList[i                     ] := Q1RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint)] := Q2RndPoint[i];
  finalize(Q1RndPoint);
  finalize(Q2RndPoint);
end;
(* End of Generate Random Points On Convex Hexagon *)


procedure GenerateRandomPointsOnConvexHeptagon(const Heptagon : TPolygon2D; var PointList : array of TPoint2D);
var
  i           : Integer;
  Quadix1     : TQuadix2D;
  Quadix2     : TQuadix2D;
  Triangle    : TTriangle2D;
  Q1RndPoint  : array of TPoint2D;
  Q2RndPoint  : array of TPoint2D;
  TRndPoint   : array of TPoint2D;
  Q1RndPntCnt : Integer;
  Q2RndPntCnt : Integer;
  TRndPntCnt  : Integer;
begin
  if Length(Heptagon) <> 7       then Exit;
  if not ConvexPolygon(Heptagon) then Exit;
  Quadix1     := EquateQuadix(Heptagon[0],Heptagon[1],Heptagon[2],Heptagon[3]);
  Quadix2     := EquateQuadix(Heptagon[3],Heptagon[4],Heptagon[5],Heptagon[6]);
  Triangle    := EquateTriangle(Heptagon[6],Heptagon[0],Heptagon[3]);
  Q1RndPntCnt := Round(1.0 * Length(PointList) * (Area(Quadix1) / Area(Heptagon)));
  Q2RndPntCnt := Round(1.0 * (Length(PointList) - Q1RndPntCnt) * (Area(Quadix2) / (Area(Quadix2) + Area(Triangle))));
  TRndPntCnt  := Length(PointList) - Q1RndPntCnt - Q2RndPntCnt;
  SetLength(Q1RndPoint,Q1RndPntCnt);
  SetLength(Q2RndPoint,Q2RndPntCnt);
  SetLength(TRndPoint ,TRndPntCnt );
  GenerateRandomPoints(Quadix1,Q1RndPoint);
  GenerateRandomPoints(Quadix2,Q2RndPoint);
  GenerateRandomPoints(Triangle,TRndPoint);
  for i := 0 to Length(Q1RndPoint) - 1 do PointList[i                                          ] := Q1RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint)                     ] := Q2RndPoint[i];
  for i := 0 to Length(TRndPoint ) - 1 do PointList[i + Length(Q1RndPoint) + Length(Q2RndPoint)] := TRndPoint [i];
  finalize(Q1RndPoint);
  finalize(Q2RndPoint);
  finalize(TRndPoint);
end;
(* End of Generate Random Points On Convex Heptagon *)


procedure GenerateRandomPointsOnConvexOctagon(const Octagon : TPolygon2D; var PointList : array of TPoint2D);
var
  i           : Integer;
  Quadix1     : TQuadix2D;
  Quadix2     : TQuadix2D;
  Quadix3     : TQuadix2D;
  Q1RndPoint  : array of TPoint2D;
  Q2RndPoint  : array of TPoint2D;
  Q3RndPoint  : array of TPoint2D;
  Q1RndPntCnt : Integer;
  Q2RndPntCnt : Integer;
  Q3RndPntCnt : Integer;
begin
  if Length(Octagon) <> 8       then Exit;
  if not ConvexPolygon(Octagon) then Exit;
  Quadix1     := EquateQuadix(Octagon[0],Octagon[1],Octagon[2],Octagon[3]);
  Quadix2     := EquateQuadix(Octagon[3],Octagon[4],Octagon[5],Octagon[6]);
  Quadix3     := EquateQuadix(Octagon[0],Octagon[3],Octagon[6],Octagon[7]);
  Q1RndPntCnt := Round(1.0 * Length(PointList) * (Area(Quadix1) / Area(Octagon)));
  Q2RndPntCnt := Round(1.0 * (Length(PointList) - Q1RndPntCnt) * (Area(Quadix2) / (Area(Quadix2) + Area(Quadix3))));
  Q3RndPntCnt := Length(PointList) - Q1RndPntCnt - Q2RndPntCnt;
  SetLength(Q1RndPoint,Q1RndPntCnt);
  SetLength(Q2RndPoint,Q2RndPntCnt);
  SetLength(Q3RndPoint,Q3RndPntCnt);
  GenerateRandomPoints(Quadix1,Q1RndPoint);
  GenerateRandomPoints(Quadix2,Q2RndPoint);
  GenerateRandomPoints(Quadix3,Q3RndPoint);
  for i := 0 to Length(Q1RndPoint) - 1 do PointList[i                                          ] := Q1RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint)                     ] := Q2RndPoint[i];
  for i := 0 to Length(Q2RndPoint) - 1 do PointList[i + Length(Q1RndPoint) + Length(Q2RndPoint)] := Q3RndPoint[i];
  finalize(Q1RndPoint);
  finalize(Q2RndPoint);
  finalize(Q3RndPoint);
end;
(* End of Generate Random Points On Convex Octagon *)


procedure GenerateRandomTriangle(const Bx1,By1,Bx2,By2:TFloat; out Triangle : TTriangle2D);
var
  Dx : TFloat;
  Dy : TFloat;
begin
  Dx := Abs(Bx2 - Bx1);
  Dy := Abs(By2 - By1);
  repeat
    Triangle[1].x := Bx1 + GenerateRandomValue(Dx);
    Triangle[1].y := By1 + GenerateRandomValue(Dy);

    Triangle[2].x := Bx1 + GenerateRandomValue(Dx);
    Triangle[2].y := By1 + GenerateRandomValue(Dy);

    Triangle[3].x := Bx1 + GenerateRandomValue(Dx);
    Triangle[3].y := By1 + GenerateRandomValue(Dy);
  until not IsDegenerate(Triangle);
end;
(* End of Generate Random Triangle *)


procedure GenerateRandomQuadix(const Bx1,By1,Bx2,By2:TFloat; out Quadix : TQuadix2D);
var
  Dx : Integer;
  Dy : Integer;
begin
  Dx := Round(Abs(Bx2 - Bx1) - 1.0);
  Dy := Round(Abs(By2 - By1) - 1.0);
  repeat
    Quadix[1].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[1].y := By1 + Random(Dy) + RandomValue;

    Quadix[2].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[2].y := By1 + Random(Dy) + RandomValue;

    Quadix[3].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[3].y := By1 + Random(Dy) + RandomValue;

    Quadix[4].x := Bx1 + Random(Dx) + RandomValue;
    Quadix[4].y := By1 + Random(Dy) + RandomValue;
  until (not IsDegenerate(Quadix)) and ConvexQuadix(Quadix);
end;
(* End of Generate Random Quadix *)


procedure GenerateRandomCircle(const Bx1,By1,Bx2,By2:TFloat; out Circle : TCircle);
var
  Dx : TFloat;
  Dy : TFloat;
begin
  Dx := Abs(Bx2 - Bx1) - 1.0;
  Dy := Abs(By2 - By1) - 1.0;
  Circle.Radius := Random(Round(Min(Dx,Dy) * 0.5)) + RandomValue;
  Circle.x      := Bx1 + Circle.Radius + Random(Round(Dx - (2 * Circle.Radius))) + RandomValue;
  Circle.y      := By1 + Circle.Radius + Random(Round(Dy - (2 * Circle.Radius))) + RandomValue;
end;
(* End of Generate Random Circle *)


function Add(const Vec1,Vec2:TVector2D):TVector2D;
begin
  Result.x := Vec1.x + Vec2.x;
  Result.y := Vec1.y + Vec2.y;
end;
(* End of Add *)


function Add(const Vec1,Vec2:TVector3D):TVector3D;
begin
  Result.x := Vec1.x + Vec2.x;
  Result.y := Vec1.y + Vec2.y;
  Result.z := Vec1.z + Vec2.z;
end;
(* End of Add *)


function Add(const Vec:TVector2DArray):TVector2D;
var
  i : Integer;
begin
  Result.x := 0.0;
  Result.y := 0.0;
  for i := 0 to Length(Vec) - 1 do
  begin
    Result.x := Result.x + Vec[i].x;
    Result.y := Result.y + Vec[i].y;
  end;
end;
(* End of Add *)


function Add(const Vec:TVector3DArray):TVector3D;
var
  i : Integer;
begin
  Result.x := 0.0;
  Result.y := 0.0;
  Result.z := 0.0;
  for i := 0 to Length(Vec) - 1 do
  begin
    Result.x := Result.x + Vec[i].x;
    Result.y := Result.y + Vec[i].y;
    Result.z := Result.z + Vec[i].z;
  end;
end;
(* End of Add *)


function Add(const Vec1,Vec2:TVector2DArray):TVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Add(Vec1[i],Vec2[i]);
  end;
end;
(* End of Add *)


function Add(const Vec1,Vec2:TVector3DArray):TVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Add(Vec1[i],Vec2[i]);
  end;
end;
(* End of Add *)


function Sub(const Vec1,Vec2:TVector2D):TVector2D;
begin
  Result.x := Vec1.x - Vec2.x;
  Result.y := Vec1.y - Vec2.y;
end;
(* End of Sub *)


function Sub(const Vec1,Vec2:TVector3D):TVector3D;
begin
  Result.x := Vec1.x - Vec2.x;
  Result.y := Vec1.y - Vec2.y;
  Result.z := Vec1.z - Vec2.z;
end;
(* End of Sub *)


function Sub(const Vec1,Vec2:TVector2DArray):TVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Sub(Vec1[i],Vec2[i]);
  end;
end;
(* End of Sub *)


function Sub(const Vec1,Vec2:TVector3DArray):TVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Sub(Vec1[i],Vec2[i]);
  end;
end;
(* End of Sub *)


function Mul(const Vec1,Vec2:TVector2D):TVector3D;
begin
end;
(* End of *)


function Mul(const Vec1,Vec2:TVector3D):TVector3D;
begin
  Result.x := Vec1.y * Vec2.z - Vec1.z * Vec2.y;
  Result.y := Vec1.z * Vec2.x - Vec1.x * Vec2.z;
  Result.z := Vec1.x * Vec2.y - Vec1.y * Vec2.x;
end;
(* End of Multiply (cross-product) *)


function Mul(const Vec1,Vec2:TVector3DArray):TVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Min(Length(Vec1),Length(Vec2)));
  for i := 0 to Min(Length(Vec1),Length(Vec2)) - 1 do
  begin
    Result[i] := Mul(Vec1[i],Vec2[i]);
  end;
end;
(* End of Multiply (cross-product) *)


function UnitVector(const Vec:TVector2D):TVector2D;
var
  Mag : TFloat;
begin
  Mag := Magnitude(Vec);
  if Mag > 0.0 then
  begin
    Result.x := Vec.x / Mag;
    Result.y := Vec.y / Mag;
  end
  else
  begin
    Result.x := 0.0;
    Result.y := 0.0;
  end;
end;
(* End of UnitVector *)


function UnitVector(const Vec:TVector3D):TVector3D;
var
  Mag : TFloat;
begin
  Mag := Magnitude(Vec);
  if Mag > 0.0 then
  begin
    Result.x := Vec.x / Mag;
    Result.y := Vec.y / Mag;
    Result.z := Vec.z / Mag;
  end
  else
  begin
    Result.x := 0.0;
    Result.y := 0.0;
    Result.z := 0.0;
  end;
end;
(* End of UnitVector *)


function Magnitude(const Vec:TVector2D):TFloat;
begin
  Result := Sqrt((Vec.x * Vec.x) + (Vec.y * Vec.y));
end;
(* End of Magnitude *)


function Magnitude(const Vec:TVector3D):TFloat;
begin
  Result := Sqrt((Vec.x * Vec.x) + (Vec.y * Vec.y) + (Vec.z * Vec.z));
end;
(* End of Magnitude *)


function DotProduct(const Vec1,Vec2:TVector2D):TFloat;
begin
  Result := Vec1.x * Vec2.x + Vec1.y * Vec2.y;
end;
(* End of dotProduct *)


function DotProduct(const Vec1,Vec2:TVector3D):TFloat;
begin
  Result := Vec1.x * Vec2.x + Vec1.y * Vec2.y + Vec1.z * Vec2.z;
end;
(* End of dotProduct *)


function Scale(const Vec:TVector2D; const Factor:TFloat):TVector2D;
begin
  Result.x := Vec.x * Factor;
  Result.y := Vec.y * Factor;
end;
(* End of Scale *)


function Scale(const Vec:TVector3D; const Factor:TFloat):TVector3D;
begin
  Result.x := Vec.x * Factor;
  Result.y := Vec.y * Factor;
  Result.z := Vec.z * Factor;
end;
(* End of Scale *)


function Scale(const Vec:TVector2DArray; const Factor:TFloat):TVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := Vec[i].x * Factor;
    Result[i].y := Vec[i].y * Factor;
  end;
end;
(* End of Scale *)


function Scale(const Vec:TVector3DArray; const Factor:TFloat):TVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := Vec[i].x * Factor;
    Result[i].y := Vec[i].y * Factor;
    Result[i].z := Vec[i].z * Factor;
  end;
end;
(* End of Scale *)


function Negate(const Vec:TVector2D):TVector2D;
begin
  Result.x := -Vec.x;
  Result.y := -Vec.y;
end;
(* End of Negate *)


function Negate(const Vec:TVector3D):TVector3D;
begin
  Result.x := -Vec.x;
  Result.y := -Vec.y;
  Result.z := -Vec.z;
end;
(* End of Negate *)


function Negate(Vec:TVector2DArray):TVector2DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := -Vec[i].x;
    Result[i].y := -Vec[i].y;
  end;
end;
(* End of Negate *)


function Negate(Vec:TVector3DArray):TVector3DArray;
var
  i : Integer;
begin
  SetLength(Result,Length(Vec));
  for i := 0 to Length(Vec) - 1 do
  begin
    Result[i].x := -Vec[i].x;
    Result[i].y := -Vec[i].y;
    Result[i].z := -Vec[i].z;
  end;
end;
(* End of Negate *)


function RandomValue(ResInt : Integer = RandomResolutionInt; ResFlt : TFloat = RandomResolutionFlt) : TFloat;
begin
  Result := (1.0 * Random(ResInt)) / ResFlt;
end;
(* End of Random Value *)


procedure InitialiseTrigonometryTables;
var
  i : Integer;
begin
  (*
    Note: Trigonometry look-up tables are used to speed-up
    sine, cosine and tangent calculations.
  *)
  SetLength(CosTable,360);
  SetLength(SinTable,360);
  SetLength(TanTable,360);
  for i := 0 to 359 do
  begin
    CosTable[i] := Cos((1.0 * i) * PIDiv180);
    SinTable[i] := Sin((1.0 * i) * PIDiv180);
    TanTable[i] := Tan((1.0 * i) * PIDiv180);
  end;
end;
(* End of Initialise Trigonometry Tables *)


function IsEqual(const Val1,Val2,Epsilon:TFloat):Boolean;
var
  Diff : TFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon <= Diff) and (Diff <= Epsilon)) = (Abs(Diff) <= Epsilon),'Error - Illogical error in equality check. (IsEqual)');
  Result := ((-Epsilon <= Diff) and (Diff <= Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TPoint2D; const Epsilon:TFloat):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TPoint3D; const Epsilon:TFloat):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon) and IsEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Val1,Val2:TFloat):Boolean;
begin
  Result := IsEqual(Val1,Val2,Epsilon);
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TPoint2D):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of Is Equal *)


function IsEqual(const Point1,Point2:TPoint3D):Boolean;
begin
  Result := (IsEqual(Point1.x,Point2.x,Epsilon) and IsEqual(Point1.y,Point2.y,Epsilon) and IsEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of Is Equal *)


function NotEqual(const Val1,Val2,Epsilon:TFloat):Boolean;
var
  Diff : TFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon > Diff) or (Diff > Epsilon)) = (Abs(Val1 - Val2) > Epsilon),'Error - Illogical error in equality check. (NotEqual)');
  Result := ((-Epsilon > Diff) or (Diff > Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TPoint2D; const Epsilon:TFloat):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TPoint3D; const Epsilon:TFloat):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon) or NotEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Val1,Val2:TFloat):Boolean;
begin
  Result := NotEqual(Val1,Val2,Epsilon);
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TPoint2D):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon));
end;
(* End of not Equal *)


function NotEqual(const Point1,Point2:TPoint3D):Boolean;
begin
  Result := (NotEqual(Point1.x,Point2.x,Epsilon) or NotEqual(Point1.y,Point2.y,Epsilon) or NotEqual(Point1.z,Point2.z,Epsilon));
end;
(* End of not Equal *)


function LessThanOrEqual(const Val1,Val2,Epsilon:TFloat):Boolean;
begin
  Result := (Val1 < Val2) or IsEqual(Val1,Val2,Epsilon);
end;
(* End of Less Than Or Equal *)


function LessThanOrEqual(const Val1,Val2:TFloat):Boolean;
begin
  Result := (Val1 < Val2) or IsEqual(Val1,Val2);
end;
(* End of Less Than Or Equal *)


function GreaterThanOrEqual(const Val1,Val2,Epsilon:TFloat):Boolean;
begin
  Result := (Val1 > Val2) or IsEqual(Val1,Val2,Epsilon);
end;
(* End of Less Than Or Equal *)


function GreaterThanOrEqual(const Val1,Val2:TFloat):Boolean;
begin
  Result := (Val1 > Val2) or IsEqual(Val1,Val2);
end;
(* End of Less Than Or Equal *)


function IsEqualZero(const Val,Epsilon:TFloat):Boolean;
begin
  Result := (Val <= Epsilon);
end;
(* End of IsEqualZero *)


function IsEqualZero(const Val:TFloat):Boolean;
begin
  Result := IsEqualZero(Val,Epsilon);
end;
(* End of IsEqualZero *)


function IsDegenerate(const x1,y1,x2,y2:TFloat):Boolean;
begin
  Result := IsEqual(x1,x2) and IsEqual(y1,y2);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Segment:TSegment2D):Boolean;
begin
  Result := IsDegenerate(Segment[1].x,Segment[1].y,Segment[2].x,Segment[2].y);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Line:TLine2D):Boolean;
begin
  Result := IsDegenerate(Line[1].x,Line[1].y,Line[2].x,Line[2].y);
end;
(* End of IsDegenerate *)


function IsDegenerate(const x1,y1,z1,x2,y2,z2:TFloat):Boolean;
begin
  Result := IsEqual(x1,x2) and IsEqual(y1,y2) and IsEqual(z1,z2);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Segment:TSegment3D):Boolean;
begin
  Result := IsDegenerate(Segment[1].x,Segment[1].y,Segment[1].z,Segment[2].x,Segment[2].y,Segment[2].z);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Line:TLine3D):Boolean;
begin
  Result := IsDegenerate(Line[1].x,Line[1].y,Line[1].z,Line[2].x,Line[2].y,Line[2].z);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Triangle:TTriangle2D):Boolean;
begin
  Result := Collinear(Triangle[1],Triangle[2],Triangle[3]) or
            (IsEqual(Triangle[1],Triangle[2]))             or
            (IsEqual(Triangle[1],Triangle[3]))             or
            (IsEqual(Triangle[2],Triangle[3]));
end;
(* End of IsDegenerate *)


function IsDegenerate(const Triangle:TTriangle3D):Boolean;
begin
  Result := Collinear(Triangle[1],Triangle[2],Triangle[3]) or
            (IsEqual(Triangle[1],Triangle[2]))             or
            (IsEqual(Triangle[1],Triangle[3]))             or
            (IsEqual(Triangle[2],Triangle[3]));
end;
(* End of IsDegenerate *)


function IsDegenerate(const Quadix:TQuadix2D):Boolean;
begin
  Result :=
           (* Stage 1 unique points check *)
            IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[2].x,Quadix[2].y) or
            IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[3].x,Quadix[3].y) or
            IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[4].x,Quadix[4].y) or
            IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[3].x,Quadix[3].y) or
            IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[4].x,Quadix[4].y) or
            IsDegenerate(Quadix[3].x,Quadix[3].y,Quadix[4].x,Quadix[4].y) or
           (* Stage 2 collinearity check  *)
            Collinear(Quadix[1],Quadix[2],Quadix[3]) or
            Collinear(Quadix[2],Quadix[3],Quadix[4]) or
            Collinear(Quadix[3],Quadix[4],Quadix[1]) or
            Collinear(Quadix[4],Quadix[1],Quadix[2]) or
            Intersect(Quadix[1],Quadix[2],Quadix[3],Quadix[4]) or
            Intersect(Quadix[1],Quadix[4],Quadix[2],Quadix[3]) or
            (not ConvexQuadix(Quadix));
end;
(* End of IsDegenerate *)


function IsDegenerate(const Quadix:TQuadix3D):Boolean;
begin
  Result :=
          (* Stage 1 unique points check *)
           IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[1].z,Quadix[2].x,Quadix[2].y,Quadix[2].z) or
           IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[1].z,Quadix[3].x,Quadix[3].y,Quadix[3].z) or
           IsDegenerate(Quadix[1].x,Quadix[1].y,Quadix[1].z,Quadix[4].x,Quadix[4].y,Quadix[4].z) or
           IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[2].z,Quadix[3].x,Quadix[3].y,Quadix[3].z) or
           IsDegenerate(Quadix[2].x,Quadix[2].y,Quadix[2].z,Quadix[4].x,Quadix[4].y,Quadix[4].z) or
           IsDegenerate(Quadix[3].x,Quadix[3].y,Quadix[3].z,Quadix[4].x,Quadix[4].y,Quadix[4].z) or
          (* Stage 2 collinearity check  *)
           Collinear(Quadix[1],Quadix[2],Quadix[3]) or
           Collinear(Quadix[2],Quadix[3],Quadix[4]) or
           Collinear(Quadix[3],Quadix[4],Quadix[1]) or
           Collinear(Quadix[4],Quadix[1],Quadix[2])
end;
(* End of IsDegenerate *)


function IsDegenerate(const Rect:TRectangle):Boolean;
begin
  Result := IsEqual(Rect[1],Rect[2]);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Circle:TCircle):Boolean;
begin
  Result := LessThanOrEqual(Circle.Radius,0.0);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Sphere:TSphere):Boolean;
begin
  Result := LessThanOrEqual(Sphere.Radius,0.0);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Arc:TCircularArc2D):Boolean;
begin
  with Arc do
    Result := IsDegenerate(x1,y1,x2,y2)                                    or
              IsDegenerate(x1,y1,Cx,Cy)                                    or
              IsDegenerate(x2,y2,Cx,Cy)                                    or
              (LayDistance(x1,y1,Cx,Cy) <> LayDistance(x2,y2,Cx,Cy))       or
              (LayDistance(x1,y1,Cx,Cy) <> LayDistance(Px,Py,Cx,Cy))       or
              (CartesianAngle(x1 - Cx, y1 - Cy ) <> angle1)                or
              (CartesianAngle(x2 - Cx, y2 - Cy ) <> angle2)                or
              (CartesianAngle(Px - Cx, Py - Cy ) <> abs(angle1 - angle2))  or
              (FastGEO.Orientation (x1,y1,x2,y2,Px,Py) <> Arc.Orientation);
end;
(* End of IsDegenerate *)


function IsDegenerate(const Obj:TGeometricObject):Boolean;
begin
  case Obj.ObjectType of
    geoSegment2D  : Result := IsDegenerate(Obj.Segment2D );
    geoSegment3D  : Result := IsDegenerate(Obj.Segment3D );
    geoLine2D     : Result := IsDegenerate(Obj.Line2D    );
    geoLine3D     : Result := IsDegenerate(Obj.Line3D    );
    geoTriangle2D : Result := IsDegenerate(Obj.Triangle2D);
    geoTriangle3D : Result := IsDegenerate(Obj.Triangle3D);
    geoQuadix2D   : Result := IsDegenerate(Obj.Quadix2D  );
    geoQuadix3D   : Result := IsDegenerate(Obj.Quadix3D  );
    geoRectangle  : Result := IsDegenerate(Obj.Rectangle );
    geoCircle     : Result := IsDegenerate(Obj.Circle    );
    geoSphere     : Result := IsDegenerate(Obj.Sphere    );
  else
    Result := False;
  end;
end;
(* End of IsDegenerate *)


procedure Swap(var val1,val2:TFloat);
var
  Temp : TFloat;
begin
  Temp := Val1;
  Val1 := Val2;
  Val2 := Temp;
end;
(* End of Swap *)


procedure Swap(var val1,val2:Integer);
var
  Temp : Integer;
begin
  Temp := Val1;
  Val1 := Val2;
  Val2 := Temp;
end;
(* End of Swap *)


procedure Swap(var Point1,Point2:TPoint2D);
begin
  Swap(Point1.x,Point2.x);
  Swap(Point1.y,Point2.y);
end;
(* End of Swap *)


procedure Swap(var Point1,Point2:TPoint3D);
begin
  Swap(Point1.x,Point2.x);
  Swap(Point1.y,Point2.y);
  Swap(Point1.z,Point2.z);
end;
(* End of Swap *)


procedure Swap(var Segment1,Segment2:TSegment2D);
begin
  Swap(Segment1[1],Segment2[1]);
  Swap(Segment1[2],Segment2[2]);
end;
(* End of Swap *)


procedure Swap(var Segment1,Segment2:TSegment3D);
begin
  Swap(Segment1[1],Segment2[1]);
  Swap(Segment1[2],Segment2[2]);
end;
(* End of Swap *)


procedure Swap(var Line1,Line2:TLine2D);
begin
  Swap(Line1[1],Line2[1]);
  Swap(Line1[2],Line2[2]);
end;
(* End of Swap *)


procedure Swap(var Triangle1,Triangle2:TTriangle2D);
begin
  Swap(Triangle1[1],Triangle2[1]);
  Swap(Triangle1[2],Triangle2[2]);
  Swap(Triangle1[3],Triangle2[3]);
end;
(* End of Swap *)


procedure Swap(var Triangle1,Triangle2:TTriangle3D);
begin
  Swap(Triangle1[1],Triangle2[1]);
  Swap(Triangle1[2],Triangle2[2]);
  Swap(Triangle1[3],Triangle2[3]);
end;
(* End of Swap *)


procedure Swap(var Quadix1,Quadix2:TQuadix2D);
begin
  Swap(Quadix1[1],Quadix2[1]);
  Swap(Quadix1[2],Quadix2[2]);
  Swap(Quadix1[3],Quadix2[3]);
  Swap(Quadix1[4],Quadix2[4]);
end;
(* End of Swap *)


procedure Swap(var Quadix1,Quadix2:TQuadix3D);
begin
  Swap(Quadix1[1],Quadix2[1]);
  Swap(Quadix1[2],Quadix2[2]);
  Swap(Quadix1[3],Quadix2[3]);
  Swap(Quadix1[4],Quadix2[4]);
end;
(* End of Swap *)


procedure Swap(var Circle1,Circle2:TCircle);
begin
  Swap(Circle1.x,Circle2.x);
  Swap(Circle1.y,Circle2.y);
  Swap(Circle1.Radius,Circle2.Radius);
end;
(* End of Swap *)


procedure Swap(var Sphere1,Sphere2:TSphere);
begin
  Swap(Sphere1.x,Sphere2.x);
  Swap(Sphere1.y,Sphere2.y);
  Swap(Sphere1.z,Sphere2.z);
  Swap(Sphere1.Radius,Sphere2.Radius);
end;
(* End of Swap *)


procedure Swap(var Arc1,Arc2:TCircularArc2D);
begin
  Swap(Arc1.x1,Arc2.x1);
  Swap(Arc1.x2,Arc2.x2);
  Swap(Arc1.cx,Arc2.cx);
  Swap(Arc1.px,Arc2.px);
  Swap(Arc1.y1,Arc2.y1);
  Swap(Arc1.y2,Arc2.y2);
  Swap(Arc1.cy,Arc2.cy);
  Swap(Arc1.py,Arc2.py);
  Swap(Arc1.Angle1,Arc2.Angle1);
  Swap(Arc1.Angle2,Arc2.Angle2);
  Swap(Arc1.Orientation,Arc2.Orientation)
end;
(* End of Swap *)


function CalculateSystemEpsilon:TFloat;
var
  Epsilon   : TFloat;
  Check     : TFloat;
  LastCheck : TFloat;
begin
  Epsilon := 1.0;
  Check   := 1.0;
  repeat
    LastCheck := Check;
    Epsilon   := Epsilon * 0.5;
    Check     := 1.0 + Epsilon;
  until (Check = 1.0) or (Check = LastCheck);
  Result := Epsilon;
end;
(* End of CalculateSystemEpsilon *)


function ZeroEquivalency:Boolean;
begin
  Result := IsEqual(CalculateSystemEpsilon,0.0);
end;
(* End of ZeroEquivalency *)


function ExtendedFloatingPointTest:Boolean;
  (*
    Yet to be completed! **************************
  *)
  function RotationTest(RadiusLength : TFloat) : Boolean;
  const delta_angle = 0.036;
  var
  InitialPoint : TPoint2D;
  Point        : TPoint2D;
  TempPoint    : TPoint2D;
  i            : Integer;
  begin
    InitialPoint := EquatePoint(RadiusLength,0);
    Point        := InitialPoint;
    for i := 1 to 10000 do
    begin
      TempPoint := Rotate(delta_angle,Point);
      Point := TempPoint;
    end;
    Result := IsEqual(InitialPoint,Point);
  end;
var
  Large_Radius : TFloat;
  Small_Radius : TFloat;
begin
  Large_Radius := 100000.0;
  Small_Radius := 100000.0;
  Result := RotationTest(Large_Radius) And RotationTest(Small_Radius);
end;
(* End of ExtendedFloatingPointTest *)


function ExecuteTests : TNumericPrecisionResult;
begin
  Result.EEResult      := LessThanOrEqual(SystemEpsilon,Epsilon);
  Result.ZEResult      := ZeroEquivalency;
  Result.EFPResult     := ExtendedFloatingPointTest;
  Result.SystemEpsilon := SystemEpsilon;
end;
(* End of ExecuteTests *)


initialization

  SystemEpsilon :=  CalculateSystemEpsilon;
  MaximumX      :=  Infinity;
  MinimumX      := -Infinity;
  MaximumY      :=  Infinity;
  MinimumY      := -Infinity;
  MaximumZ      :=  Infinity;
  MinimumZ      := -Infinity;

  InitialiseTrigonometryTables;


  (* System precision sanity checks *)
  (*
     Assert(LessThanOrEqual(SystemEpsilon,Epsilon),#13+'WARNING - System epsilon is greater than static epsilon'+ #13 +
                                                       'Accuracy and robustness of calculations that use epsilon for equivalency operations may vary - hence cannot be guaranteed.'+#13);

     Assert(ZeroEquivalency,#13+'WARNING - Pseudo zero equivalency test failed!');

     Assert(ExtendedFloatingPointTest,#13+'WARNING - Extended floating point test has failed!');
  *)

finalization

  finalize(CosTable);
  finalize(SinTable);
  finalize(TanTable);

end.