{ -----------------------------------------------------------------------------
    This program is free software: Under statement of join file README - LGPL.txt
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-----------------------------------------------------------------------------
 Unit Name : GS.Direction
 Author    : Vincent Gsell (vincent dot gsell at gmail dot com)
 Purpose   : basic "Directional Vector" routines - TDirectionalObject helper class.
 Date:     : 20070102
 History   :
 20070102 - Creating unit.
 20091120 - Clearing code v1.0
 20180426 - Put this unit in GS collection. Freeing from Types dependancy.
 20200201 - Renaming and re-integrating to GS.Core
-----------------------------------------------------------------------------}
{$I GSCore.Inc}

Unit GS.Geometry.Direction;

interface


Const GLB_Math_PrecisionTolerance = 1.0E-12;
      GLB_PrecisionTolerance: Double = 1.0E-6;
      GLB_Pi = 3.141592654;
      GLB_TwoPi: Double   = 6.283185307179586476925286766559;
      GLB_PiOn2: Double   = 1.5707963267948966192313216916398;
      GLB_GripTolerance = 5.0;
      GLB_DegreeCst : Double = 180 / Pi;
      GLB_RadianCst : Double = Pi / 180;

Type
TPt = Record
  X,Y : Single;
end;

TLn = Record
case integer of
0: (A, B : TPt);
1: (X1, Y1, X2, Y2 : Single)
end;

TRct = Record
 Left, Top, Right, Bottom : Single;
end;

TVectorArray = array [0..1] of Single;
TVector = Record
case Integer of
  0: (V :TVectorArray);
  1: (X,Y : Single);
end;

TVectorObject = class
Public
  Origin : TPt;
  Direction : TVector;
  Constructor Create(X,Y,Norm : Double);
end;


TDirectionalObjectCompass = (dcE,dcESE,dcSE,dcSSE,dcS,dcSSW,dcSW,dcWSW,dcW,dcWNW,dcNW,dcNNW,dcN,dcNNE,dcNE,dcENE);

//In simple case, can be used instead of matrix.
TDirectionalObject =Class(TVectorObject)
Private
  function GetAngle: Double;
  function GetNorm: Double;
  procedure SetAngle(const Value: Double);
  procedure SetNorm(const Value: Double);
  function GetPositionX: Double;
  procedure SetPositionX(const Value: Double);
  function GetPositionY: Double;
  procedure SetPositionY(const Value: Double);

  Procedure ResetDirection;
  function GetAngleInDegree: Double;
  procedure SetAngleInDegree(const Value: Double); //20090723 - VG - For certain operation, it seems to be a "must have" to set right Direction vector initialisation. (Origin<>Direction before vpointat)
                            //This is valid for Norm-independant operation.
Public
  Procedure TurnLeft; Virtual;
  Procedure TurnRight; Virtual;
  Procedure TurnHalf; Virtual;

  Procedure TurnBy(AmountInDegree : Double); Virtual;

  Procedure MoveAhead; Virtual;
  Procedure MoveAheadBy(Amount : Double); Virtual;

  Procedure LookAt(aPoint : TPt); Overload; Virtual;
  Procedure PointAt(aPoint : TPt); Virtual;

  Function GetPointedCoord : TPt; Virtual;
  Procedure SetPointedCoord(aPoint : TPt); Virtual;
  Procedure SetOrigin(x,y : Double); Virtual;

  function slope : single;

  Property Norm : Double read GetNorm Write SetNorm;
  Property Angle : Double read GetAngle Write SetAngle;
  Property AngleInDegree : Double read GetAngleInDegree Write SetAngleInDegree;
  Property X : Double read GetPositionX Write SetPositionX;
  Property Y : Double read GetPositionY Write SetPositionY;
end;

function Line(var P1, P2: TPt): TLn; Overload; inline;
function Line(X, Y, X1, Y1: Single): TLn; Overload; inline;
function Point(X, Y: Single): TPt; Overload;inline;
function Rect(Left,Top, Right, Bottom : Single) : TRct;inline;

Procedure vInit(var vector : TVector);inline;
Function vNorm(var Vector : TVector) : Double; OVerload;inline;
Function vAngle(var Vector : TVector) : Double; Overload; inline;
procedure vReset(var Point: TPt);Overload;inline;
procedure vReset(var Point: TVector);Overload;inline;


Procedure vNormalyze(var Vector : TVector);

Procedure vNorm(var Vector : TVector; NewNorm : Double); Overload;inline;
Procedure vNormOn2(var Vector : TVector); Overload;
Procedure vNormDec(var Vector : TVector; PercentAmount : Integer);
Procedure vAngle(var Vector : TVector; Const NewAngle : Extended); Overload; inline;

Procedure vStepPoint(Var aP : TPt; Vector : TVector); Overload;
Procedure vStepPoint(Var X,Y : Double; Vector : TVector); Overload;
Procedure vStepPointby(Var X,Y : Double; Vector : TVector; Amount : Double);
Procedure vRotate(Var Vector : TVector; Const ByAngle : Double);
Procedure vTurnLeft(Var Vector : TVector); OVerload;
Procedure vTurnRight(Var Vector : TVector); Overload;
Procedure vTurnLeft(Var Vector : TVector; Amount : Double); Overload;
Procedure vTurnRight(Var Vector : TVector; Amount : Double); Overload;
Procedure vHalfTurn(Var Vector : TVector);
Procedure vLookAt(var Vector : TVector; X,Y,Z : Double); Overload;
Procedure vLookAt(var Vector : TVector; P : TPt); Overload;
Procedure vLookAt(Origin : TPt; var Vector : TVector; P : TPt); Overload;
Procedure vPointTo(Origin : TPt; var Vector : TVector; P : TPt); Overload;
function vSimulStep(var Vector: TVector): TPt; Overload;
function vSimulStepBy(var Vector : TVector; Amount: Double): TPt; Overload;


  //Standart Operation
Procedure vMultiply(Var ResultVector, VectorA,VectorB : TVector);
Procedure vAdd(Var ResultVector, VectorA,VectorB : TVector); Overload;
Procedure vAdd(Var AddedVector, VectorA : TVector); Overload;
Procedure vSub(Var ResultVector, VectorA,VectorB : TVector); Overload;
Function vSub(VectorA,VectorB : TVector) : TVector; Overload;
Procedure vInvert(var Vector : TVector);
Function vEqualNorm(Tolerance : Double; v1,v2 : TVector) : Boolean;
function vlerp(va,vb : TVector; percent : single) : TVector;

// for TPointVector
//Procedure ovStep(var ov : TPointVector);
//Procedure ovTurnBy(var ov : TPointVector; amount : Double);

Procedure PolarToCartesian(const R, Phi: Double; var X, Y: Single); Inline;
Procedure CartesianToPolar(const X, Y: Double; var R, Phi: Single); {$IFDEF D2009UP} Inline {$ENDIF}
function Sgn(const X: Double): Integer; {$IFDEF D2009UP} Inline {$ENDIF}


// math
//line intersection : x1,y1,x2,y2 : First line.
function vIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:Double; out ix,iy:Double):Boolean; inline;
function vRectIntersect(const aRect : TRct; const x1,y1,x2,y2 : Double; out A, B : Boolean; out Aix,Aiy,Bix,Biy : Double) : boolean; //Inline;
procedure vMirror(const Px,Py,x1,y1,x2,y2:Double;out Nx,Ny:Double); inline;
function vNotEqual(const Val1,Val2:double):Boolean; inline;
function InternalNotEqual(const Val1,Val2,aPrecisionTolerance:double):Boolean; inline;
function InternalIsEqual(const Val1,Val2,aPrecisionTolerance:double):Boolean; inline;
function vIsEqual(const Val1,Val2:double):Boolean; inline;


function vPtInRect(aPoint : TPt; aRect : TRct; Var aLocalResult : TPt) : Boolean;inline;

implementation

uses Math;

{ TofEngineVector }

procedure vHalfTurn(var Vector: TVector);
begin
  vRotate(vector,Pi)
end;


procedure vLookAt(var Vector: TVector; X,
  Y, Z: Double);
var n : Double;
begin
  n:=vNorm(Vector);
  Vector.X:=X;
  Vector.Y:=Y;
  vNorm(Vector,n);
end;

procedure vLookAt(var Vector: TVector; P: TPt);
var n : Double;
begin
  n:=vNorm(Vector);
  Vector.X:=P.X;
  Vector.Y:=P.Y;
  vNorm(Vector,n);
end;

procedure vRotate(var Vector: TVector;
  Const ByAngle: Double);
var n : Double;
begin
  n:=vAngle(Vector);
  n:=n+ByAngle;
  vAngle(Vector,n);
end;

procedure vTurnLeft(var Vector: TVector);
begin
  vRotate(Vector,-GLB_PiOn2);
end;

procedure vTurnRight(var Vector: TVector);
begin
  vRotate(Vector,GLB_PiOn2);
end;



function vSimulStep(var Vector: TVector): TPt;
begin
  Result.X:=Vector.X+Vector.X;
  Result.Y:=Vector.Y+Vector.Y;
end;

function vSimulStepBy(var Vector : TVector; Amount: Double): TPt;
begin
  Result.X:=Vector.X+Amount;
  Result.Y:=Vector.Y+Amount;
end;


function Sgn(const X: Double): Integer;
begin
  if X > 0.0 then
    Result := 1
  else
  if X < 0.0 then
    Result := -1
  else
    Result := 0;
end;

Procedure CartesianToPolar(const X, Y: Double; var R, Phi: Single);
begin
  R := Sqrt(Sqr(X) + Sqr(Y));

  if Abs(X) > GLB_PrecisionTolerance then
  begin
    Phi := ArcTan(Abs(Y) / Abs(X));
    if (x>=0) then begin
      if (y>=0) then begin
        Phi:=GLB_TwoPi-Phi;
      end
      else begin
      end;
    end
    else
    begin
      if (y>=0) then begin
        Phi:=Pi+Phi;
      end
      else begin
        Phi:=Pi-Phi;
      end;
    end;
  end
  else
  if Abs(Y) > GLB_PrecisionTolerance then
  begin
    Phi := Sgn(Y) * GLB_PiOn2 * -1;
    Phi := GLB_TwoPi - Phi * -1;
  end
  else
  begin
    R := 0;
    Phi := 0;
  end;
end;

Procedure PolarToCartesian(const R, Phi: Double; var X, Y: Single);
var
  Sine, CoSine: Double;
begin
  SinCos(Phi, Sine, CoSine);
  X := R * CoSine;
  Y := R * Sine *-1;
end;


function Line(var P1, P2: TPt): TLn;
begin
  Result.A:=P1;
  Result.B:=P2;
end;

function Line(X, Y, X1, Y1: Single): TLn;
begin
  Result.A.X:=X;
  Result.A.Y:=Y;
  Result.B.X:=X1;
  Result.B.Y:=Y1;
end;

function Point(X, Y: Single): TPt;
begin
  Result.X:=X;
  Result.Y:=Y;
end;

function Rect(Left,Top, Right, Bottom : Single) : TRct;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;



procedure vAngle(var Vector: TVector;
  Const NewAngle: Extended);
var n : Double;
begin
  n:=Sqrt(Vector.X*Vector.X+Vector.Y*Vector.Y);
  PolarToCartesian(n,NewAngle,Vector.X,Vector.Y);
  //Get Vector coords with the new angle.
end;

Procedure vStepPoint(Var aP : TPt; Vector : TVector);
begin
  aP.X:=aP.X+Vector.X;
  aP.Y:=aP.Y+Vector.Y;
end;

Procedure vStepPoint(Var X,Y : Double; Vector : TVector);
begin
  X:=X+Vector.X;
  Y:=Y+Vector.Y;
end;

Procedure vStepPointby(Var X,Y : Double; Vector : TVector; Amount : Double);
var v : TVector;
begin
  v:=Vector;
  vNorm(v,amount);
  X:=X+V.X;
  Y:=Y+V.Y;
end;


function vAngle(var Vector: TVector): Double;
var r,n : Single;
begin
  CartesianToPolar(Vector.x,Vector.y,n,r);
  result:=r;
end;

procedure vReset(var Point: TPt);
begin
  Point.X:=0;
  Point.Y:=0;
end;

procedure vReset(var Point: TVector);
begin
  Point.X:=0;
  Point.Y:=0;
end;

procedure vNorm(var Vector: TVector;
  NewNorm: Double);
var a : Double;
begin
  //Set new norm.
  a:=vangle(Vector);
  vReset(vector);
  Vector.X:=Abs(NewNorm);
  vAngle(Vector,a);
  if NewNorm<0 then
    vHalfTurn(Vector);
end;

Procedure vInit(var vector : TVector);
begin
  With Vector do
  begin
    x:=0;
    y:=0;
  end;
end;

function vNorm(var Vector: TVector): Double;
begin
  With Vector do
    Result:=Sqrt(x*x+y*y);
end;

Procedure vNormalyze(var Vector : TVector);
var d : Double;
begin
  d := vNorm(Vector);
  if d<>0 then  begin
    Vector.X := Vector.X / d;
    Vector.Y := Vector.Y / d;
  end;

end;

procedure vTurnLeft(var Vector: TVector;
  Amount: Double);
begin
  vRotate(Vector,-Amount);
end;

procedure vTurnRight(var Vector: TVector;
  Amount: Double);
begin
  vRotate(Vector,Amount);
end;

procedure vAdd(var ResultVector, VectorA, VectorB : TVector);
begin
  ResultVector.X:=VectorA.X+Vectorb.X;
  ResultVector.Y:=VectorA.Y+Vectorb.Y;
end;

procedure vMultiply(var ResultVector, VectorA,  VectorB : TVector);
begin
  ResultVector.X:=VectorA.X*Vectorb.X;
  ResultVector.Y:=VectorA.Y*Vectorb.Y;
end;

procedure vSub(var ResultVector, VectorA, VectorB : TVector);
begin
  ResultVector.X:=VectorA.X-Vectorb.X;
  ResultVector.Y:=VectorA.Y-Vectorb.Y;
end;

Function vSub(VectorA,VectorB : TVector) : TVector;
begin
  Result.X:=VectorA.X-Vectorb.X;
  Result.Y:=VectorA.Y-Vectorb.Y;
end;

procedure vNormOn2(var Vector: TVector);
begin
  Vector.X:=Vector.X /2;
  Vector.Y:=Vector.Y /2;
end;

Procedure vNormDec(var Vector : TVector; PercentAmount : Integer);
begin
  Vector.X:= Vector.X-(PercentAmount * Vector.X /100);
  Vector.Y:= Vector.Y-(PercentAmount * Vector.Y /100);
end;


procedure vLookAt(Origin: TPt;
  var Vector: TVector; P: TPt);
var n : Double;
begin
  n:=vNorm(Vector);
  Vector.X:=P.X-Origin.x;
  Vector.Y:=P.Y-Origin.Y;
  vNorm(Vector,n);
end;

Procedure vPointTo(Origin : TPt; var Vector : TVector; P : TPt); Overload;
var a,b : double;
begin
  vLookAt(Origin,Vector,P);
  a:=P.x-Origin.X;
  b:=P.y-Origin.y;
  a:=a*a;
  b:=b*b;
  vNorm(Vector,Sqrt(a+b));
end;

procedure vInvert(var Vector: TVector);
begin
  Vector.X:=Vector.X*-1;
  Vector.Y:=Vector.Y*-1;
end;

procedure vAdd(var AddedVector, VectorA: TVector);
begin
  AddedVector.X:=AddedVector.X+VectorA.X;
  AddedVector.Y:=AddedVector.Y+VectorA.Y;
end;

function vEqualNorm(Tolerance: Double; v1,
  v2: TVector): Boolean;
var a,b : Double;
begin
  a:=Abs(vNorm(v1));
  b:=Abs(vNorm(v2));

  Result:=(a>(b-tolerance)) and (a<=(b+tolerance));
end;

function vlerp(va,vb : TVector; percent : single) : TVector;
begin
  result.X := va.x + (vb.X-va.X) * percent;
  result.Y := va.y + (vb.X-va.X) * percent;
end;


{ TDirectionalObject }

procedure TDirectionalObject.LookAt(aPoint: TPt);
begin
  //ResetDirection;
  vLookAt(Origin,Direction,aPoint);
end;

procedure TDirectionalObject.MoveAhead;
//var a : TPt;
begin
  //a:=vSimulStep(Direction);
  //origin.X:=Origin.X+a.X;
  //origin.Y:=Origin.Y+a.Y;
  //origin.Z:=Origin.Z+a.Z;
  origin.X:=Origin.X+Direction.X;
  origin.Y:=Origin.Y+Direction.Y;
end;

procedure TDirectionalObject.MoveAheadBy(Amount: Double);
begin
  //Origin:=vSimulStep(Direction);
  origin.X:=Origin.X+Direction.X*Amount;
  origin.Y:=Origin.Y+Direction.Y*Amount;
end;

procedure TDirectionalObject.TurnBy(AmountInDegree: Double);
begin
  vRotate(Direction,AmountInDegree*Pi/180);
end;

procedure TDirectionalObject.TurnHalf;
begin
  vHalfTurn(Direction);
end;

procedure TDirectionalObject.TurnLeft;
begin
  vTurnLeft(Direction);
end;

procedure TDirectionalObject.TurnRight;
begin
   vTurnRight(Direction);
end;

function TDirectionalObject.GetAngle: Double;
begin
  Result:=vAngle(Direction);
end;

function TDirectionalObject.GetAngleInDegree: Double;
begin
  result := GetAngle * GLB_DegreeCst;
end;

function TDirectionalObject.GetNorm: Double;
begin
  result:=vNorm(Direction);
end;

function TDirectionalObject.GetPointedCoord: TPt;
begin
  Result:=Point(Origin.X+Direction.X,Origin.Y+Direction.Y);
end;

procedure TDirectionalObject.SetAngle(const Value: Double);
begin
  vAngle(Direction,Value);
end;

procedure TDirectionalObject.SetAngleInDegree(const Value: Double);
begin
  Angle := Value / GLB_DegreeCst;
end;

procedure TDirectionalObject.SetNorm(const Value: Double);
begin
  vNorm(Direction,Value);
end;


procedure TDirectionalObject.SetPointedCoord(aPoint: TPt);
var a,b : double;
begin
  LookAt(aPoint);
  a:=aPoint.x-Origin.X;
  b:=aPoint.y-Origin.y;
  a:=a*a;
  b:=b*b;
  vNorm(direction,Sqrt(a+b));
end;

procedure TDirectionalObject.SetOrigin(x, y : Double);
begin
  Origin.X:=x;
  Origin.Y:=Y;
end;

procedure TDirectionalObject.PointAt(aPoint: TPt);
begin
  ResetDirection;
  vPointTo(Origin,Direction,aPoint);
end;

function TDirectionalObject.GetPositionX: Double;
begin
  Result := Origin.X;
end;

procedure TDirectionalObject.SetPositionX(const Value: Double);
begin
  Origin.X := Value;
end;

function TDirectionalObject.GetPositionY: Double;
begin
  Result := Origin.Y;
end;

procedure TDirectionalObject.SetPositionY(const Value: Double);
begin
  Origin.Y:=Value;
end;

function TDirectionalObject.slope: single;
var x1,x2,y1,y2 : single;
begin
  x1 := Origin.X;
  x2 := Origin.X+Direction.X;
  if InternalIsEqual(x1-x2,0,GLB_Math_PrecisionTolerance) then
    result := 0;
  y1 := Origin.Y;
  y2 := Origin.Y+Direction.Y;

  result := y2-y1/x2-x1;
end;

procedure TDirectionalObject.ResetDirection;
begin
  Direction.X :=Origin.X;
  Direction.y :=Origin.y;
end;

function InternalNotEqual(const Val1,Val2,aPrecisionTolerance:double):Boolean;
var
  lDiff : single;
begin
  lDiff := Val1 - Val2;
  Result := ((-aPrecisionTolerance > lDiff) or (lDiff > aPrecisionTolerance));
end;

function vNotEqual(const Val1,Val2:double):Boolean;
begin
  Result := InternalNotEqual(Val1,Val2,GLB_Math_PrecisionTolerance);
end;

function InternalIsEqual(const Val1,Val2,aPrecisionTolerance:double):Boolean;
var
  lDiff : Single;
begin
  lDiff := Val1 - Val2;
  Result := ((-aPrecisionTolerance <= lDiff) and (lDiff <= aPrecisionTolerance));
end;

function vIsEqual(const Val1,Val2:double):Boolean;
begin
  Result := InternalIsEqual(Val1,Val2,GLB_Math_PrecisionTolerance);
end;



function vIntersect(const x1,y1,x2,y2,x3,y3,x4,y4:double; out ix,iy:double):Boolean;
var
  UpperX    : Single;
  UpperY    : Single;
  LowerX    : Single;
  LowerY    : Single;
  Ax        : Single;
  Bx        : Single;
  Cx        : Single;
  Ay        : Single;
  By        : Single;
  Cy        : Single;
  D         : Single;
  F         : Single;
  E         : Single;
  Ratio     : Single;
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

  if vNotEqual(Ratio,0.0) then
  begin
    Ratio := ((Cy * -Bx) - (Cx * -By)) / Ratio;
    ix    := x1 + (Ratio * Ax);
    iy    := y1 + (Ratio * Ay);
  end
  else
  begin
    //if Collinear(x1,y1,x2,y2,x3,y3) then
    if vIsEqual((Ax * -Cy),(-Cx * Ay)) then
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
function vRectIntersect(const aRect : TRct; const x1,y1,x2,y2 : Double; out A, B : Boolean; out Aix,Aiy,Bix,Biy : Double) : boolean; Inline;
var ta,tb : Double;
begin
  Aix := -1;
  Aiy := -1;
  Bix := -1;
  Biy := -1;
  A := false;
  B := false;
  if vIntersect(aRect.Left,aRect.Top,aRect.Right,aRect.Top,x1,y1,x2,y2,ta,tb) then
  begin
    A := true;
    Aix := ta;
    Aiy := tb;
  end;

  if vIntersect(aRect.Left,aRect.Top,aRect.Left,aRect.Bottom,x1,y1,x2,y2,ta,tb) then
  begin
    if A then
    begin
      B := true;
      Bix := ta;
      Biy := tb;
    end
    else
    begin
      A := true;
      Aix := ta;
      Aiy := tb;
    end;
  end;

  if vIntersect(aRect.Left,aRect.Bottom,aRect.Right,aRect.Bottom,x1,y1,x2,y2,ta,tb) then
  begin
    if A then
    begin
      B := true;
      Bix := ta;
      Biy := tb;
    end
    else
    begin
      A := true;
      Aix := ta;
      Aiy := tb;
    end;
  end;

  if vIntersect(aRect.Right,aRect.Top,aRect.Right,aRect.Bottom,x1,y1,x2,y2,ta,tb) then
  begin
    if A then
    begin
      B := true;
      Bix := ta;
      Biy := tb;
    end
    else
    begin
      A := true;
      Aix := ta;
      Aiy := tb;
    end;
  end;
  result := A or B;
end;

procedure vMirror(const Px,Py,x1,y1,x2,y2:Double;out Nx,Ny:Double);
var
  Vx    : Double;
  Vy    : Double;
  Wx    : Double;
  Wy    : Double;
  c1    : Double;
  c2    : Double;
  Ratio : Double;
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

  Nx := Px + 2 * (Nx - Px);
  Ny := Py + 2 * (Ny - Py);
end;

function vPtInRect(aPoint : TPt; aRect : TRct; Var aLocalResult : TPt) : Boolean;
begin
  Result := (aPoint.X>=aRect.Left) and (aPoint.X<=aRect.Right) And (aPoint.Y<=aRect.Bottom) and (aPoint.Y>=aRect.top);
  if Result then
    aLocalResult := Point(aPoint.x-aRect.Left,aPoint.Y-aRect.Top);
end;

{ TVectorObject }

constructor TVectorObject.Create(X, Y, Norm: Double);
begin
  Origin.X:=X;
  Origin.Y:=Y;
  Direction.X:=Norm;
  Direction.Y:=0;
end;

end.

