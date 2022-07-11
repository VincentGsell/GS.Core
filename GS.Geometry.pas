//Pure extraction from the very nice work of Wouter van Nifterick (aka WouterVanNifterick)
//from his delphi-shader project ---> https://github.com/WouterVanNifterick/delphi-shader
//Aim here is to obtain fast and easy to maintain cross platform mathematics tools.
//So, all original texture and shader stuff has been removed, as well as dependancies.
unit GS.Geometry;

{$I GSCore.Inc}
interface

uses Classes,
     SysUtils,
     types,
     Math;


{$R-}

{$IFNDEF CPUx64}
  {$DEFINE DO_INLINE}
{$ENDIF}
{$IFDEF DEBUG}
  {.$DEFINE DO_INLINE}
{$ENDIF}

const
  M_LN2 = 0.693147180559945309417;
  _EPSILON_ = 4.9406564584124654418e-324;

type
{$IFDEF CPUx64}
  TVecType = type Double;
const
  M_MXVT =  MaxDouble;
{$ELSE}
  TVecType = type Single;
const
  M_MXVT =  MaxSingle;
{$ENDIF}

Type

  int      = type Integer;
  bool     = type Boolean;
  Float    = type Double;
  PVec2    = ^Vec2;
  PVec3    = ^Vec3;
  PVec4    = ^Vec4;

  Vec1     = TVecType;

  Vec2 = record
    x, y: TVecType;
    function Length: TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function Normalize: PVec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function Cross(const b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function Dot(const b: Vec2): TVecType; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
    constructor create(ax, ay: TVecType);overload;
    constructor create(ax: TVecType);overload;
    class operator explicit(const b:TVecType):Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(const a, b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(const a: Vec2; b: TVecType): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(a: TVecType; const b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Subtract(const a, b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Subtract(const a: Vec2; b: TVecType): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Subtract(a: TVecType; const b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a: Vec2; const b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a: Vec2; b: TVecType): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(a: TVecType; const b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Divide(const a: Vec2; const b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Divide(const a: Vec2; b: TVecType): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Divide(a: TVecType; const b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Negative(const a: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Explicit(const a: Vec2): TPoint;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Explicit(const a: Vec2): TPointF;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Explicit(const a: TPoint): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Explicit(const a: TPointF): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}

    property r:TVecType read x write x;
    property g:TVecType read y write y;
  end;

  Vec2s = array of Vec2;
  Vec2sp = ^Vec2s;
  Vec2sArray = Array of Vec2s;

  Vec3 = record
    function Length: TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function Normalize: PVec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    procedure NormalizeSelf;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function Abs: Vec3; {$IFDEF DO_INLINE} inline;{$ENDIF}
    function Cross(const b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function Dot(const b: Vec3): TVecType; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
    constructor create(ax, ay, az: TVecType);overload;
    constructor create(ax: TVecType);overload;
    constructor create(const xy:Vec2;az: TVecType);overload;
    constructor create(aX:TVecType;const yz:Vec2);overload;
    class operator Subtract(const a, b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Subtract(const a: Vec3; const b: Vec2): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Subtract(const a:Vec2; const b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}

    class operator Add(const a, b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(const a: Vec3; b: TVecType): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Negative(const a: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a: Vec3; const b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a: Vec3; b: TVecType): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(a: TVecType; const b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Divide(const a: Vec3; const b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Divide(const a: Vec3; b: TVecType): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Implicit(a: TVecType): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    //Color 32 (int32 range test.
    class operator Explicit(const a: Vec3): Int32;{$IFDEF DO_INLINE} inline;{$ENDIF}

    class operator Equal(const a,b:Vec3):Boolean;{$IFDEF DO_INLINE} inline;{$ENDIF}

    var
      x, y, z: TVecType;
    property r:TVecType read x write x;
    property g:TVecType read y write y;
    property b:TVecType read z write z;

  end;

  Vec4 = record
    function Dot(const b: Vec4): TVecType; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
    constructor create(x: TVecType); overload;
    constructor create(x, y, z, w: TVecType); overload;
    constructor create(const x: Vec3; w: TVecType); overload;
    constructor create(w: TVecType;const ax: Vec3 ); overload;
    class operator Implicit(const a: Vec3): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Explicit(const a: Vec4): Int32;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a: Vec4; b: TVecType): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a: TVecType; const b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a,b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a:vec3;const b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a:vec4;const b: Vec3): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Divide(const a:vec4;b: TVecType): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Divide(const a:vec4;b: int64): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(const a, b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(a:TVecType; const b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(const a:Vec4; b: TVecType): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Subtract(const a,b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Negative(const a: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function getHeight: TVecType;
    function getWidth: TVecType;

{
    case RecType: byte of
      0:(x, y, z, w: TVecType);
      1:(r, g, b, a: TVecType);
}

    var
      x, y, z, w: TVecType;
    property r:TVecType read x write x;
    property g:TVecType read y write y;
    property b:TVecType read z write z;
    property a:TVecType read w write w;

    property Left:TVecType read x write x;
    property top:TVecType read y write y;
    property right:TVecType read z write z;
    property bottom:TVecType read w write w;

    property Width:TVecType read getWidth;
    property height:TVecType read getHeight;
  end;

  Mat3 = record
  private
    function getm11: TVecType;
    procedure setmm11(const Value: TVecType);
  public
    r1,r2,r3:Vec3;
    constructor Create(a1,a2,a3,b1,b2,b3,c1,c2,c3:TVecType);overload;
    constructor Create(const a,b,c:Vec3);overload;
    class operator Multiply(const a:Vec2;const b:Mat3):Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} //Graphics one.
    class operator Multiply(const a:Mat3;const b:Vec3):Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const a:Vec3;const b:Mat3):Vec3;
    class operator Multiply(const a:Mat3;const b:Mat3):Mat3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Add(const a:Mat3;const b:Vec3):Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Negative(const a: Mat3): Mat3;{$IFDEF DO_INLINE} inline;{$ENDIF}

    function Determinant : TVecType;
    function Adjoint : Mat3;
    function Inverse : Mat3;
    function Scale(factor : TVecType) : Mat3;
  end;

  Mat4 = record
  public
    r1,r2,r3,r4:Vec4;
    constructor Create(a1,a2,a3,a4,b1,b2,b3,b4,c1,c2,c3,c4,d1,d2,d3,d4:TVecType);
    class operator Multiply(const a,b:Mat4):Mat4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const b: Vec4; const a: Mat4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    class operator Multiply(const b:Vec3;const a:Mat4):Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} //Graphics one. (vec3*Mat4)
//    class operator Multiply(const a:Mat4;const b:Vec4):Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF}
    function Determinant : TVecType;
    function Adjoint : Mat4;
    function Scale(factor : TVecType) : Mat4;
    function Inverse : Mat4;
  end;



///Mat3 related.
function Mat3Identity : Mat3;
function Mat3CreateRotation(angle : single) : Mat3;
function Mat3CreateTranslation(deltax, deltay : TvecType) : Mat3;
function Mat3CreateScaling(scalex, scaley : TVecType) : Mat3;

///Mat4 related.
function Mat4Identity : Mat4;
function Mat4CreateTranslation(translation : vec3) : Mat4;
function Mat4CreateRotationX(angle : single): Mat4;
function Mat4CreateRotationY(angle : single): Mat4;
function Mat4CreateRotationZ(angle : single): Mat4;

function Mat4CreateOrthoOffCenterRH(Left, Top, Right, Bottom, ZNear, ZFar: Single): Mat4;

function Mat4CreateLookAtDirLH(source,direction,ceiling : Vec3) : Mat4;
function Mat4CreatePerspectiveFovLH(const FOV, Aspect, ZNear, ZFar: Single;
  const HorizontalFOV: Boolean = False): Mat4;



function _abs(x: Single) : Single;inline;overload;
function _abs(x: Double) : Double;inline;overload;
// function abs(x: Extended) : Extended;inline;overload;
function _pow(x, y: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _radians(degrees: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return T(M_PI/180)*degrees; }
function _degrees(radians: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return T(180/M_PI)*radians; }
function _exp2(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;        { return T(cmath::exp(x * M_LN2)); }
function _log(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;        { return T(cmath::log(x) / M_LN2); }
function _log(x: vec2): vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;        { return T(cmath::log(x) / M_LN2); }
function _log(x: vec3): vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;        { return T(cmath::log(x) / M_LN2); }
function _log2(x: single): single;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;        { return T(cmath::log2(x) / M_LN2); }
function _log2(x: double): double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;        { return T(cmath::log2(x) / M_LN2); }

function _inversesqrt(x: Single): Single;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return 1/cmath::sqrt(x); }
function _inversesqrt(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return 1/cmath::sqrt(x); }

function _sign(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;        { return T((x>0) ? T(1) : ((x<0) ? T(-1):T(0))); }
function _fract(x: Double): Double;inline;overload;       { return x - cmath::floor(x); }
function _fract(const a: Vec2): Vec2;inline;overload;       { return x - cmath::floor(x); }
function _fract(const a: Vec3): Vec3;inline;overload;       { return x - cmath::floor(x); }
function _fract(const a: Vec4): Vec4;inline;overload;       { return x - cmath::floor(x); }

function _floor(const a: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;       { return x - cmath::floor(x); }
function _floor(const a: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;       { return x - cmath::floor(x); }
function _floor(const a: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;       { return x - cmath::floor(x); }


function _clamp(x:Double; minVal:Double; maxVal: Double): Double;inline; overload; { return glsl::min(glsl::max(x,minVal),maxVal); }
function _clamp(x:Double): Double;inline; overload;
function _distance(p0, p1: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return length(p0-p1); }
function _Dot(x, y: Single): Single;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return x*y; }
function _Dot(x, y: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return x*y; }
function _Dot(const x, y: Vec2): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}   { return x*y; }
function _Dot(const x, y: Vec3): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}   { return x*y; }
function _Dot(const x, y: Vec4): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}   { return x*y; }

// function ceil(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _ceil(const a: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _ceil(const a: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _ceil(const a: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}



function _Reflect(I, n: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return I - T(2)*N*I*N; }
function _Reflect(const I, n: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return I - T(2)*N*I*N; }

function _FaceForward(const N,I,NRef:TVecType): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _FaceForward(const N,I,NRef:Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _FaceForward(const N,I,NRef:Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _FaceForward(const N,I,NRef:Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;

function _Refract(const I, N:TVecType; eta:Double):TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _Refract(const I, N:vec2; eta:Double):vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _Refract(const I, N:vec3; eta:Double):vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _Refract(const I, N:vec4; eta:Double):vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;

//function Abs(const x: TVecType): TVecType; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _Abs(const x: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _Abs(const x: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _Abs(const x: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _asin(x:Single):Single;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _acos(x:Single):Single;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _atan(x:Single):Single;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _atan(x,y:Single):Single;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _tan(x:Single):Single;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _asin(x:Double):Double;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _acos(x:Double):Double;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _atan(x:Double):Double;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _atan(x,y:Double):Double;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _tan(x:Double):Double;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}


function _cross(const a,b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _smoothstep(edge0, edge1, x: Single): Single; {$IFDEF DO_INLINE} inline;{$ENDIF}  overload; { T t = clamp((x-edge0) / (edge1-edge0), T(0), T(1)); return t * t * (3 - 2*t); }
function _smoothstep(edge0, edge1, x: Double): Double; {$IFDEF DO_INLINE} inline;{$ENDIF}  overload; { T t = clamp((x-edge0) / (edge1-edge0), T(0), T(1)); return t * t * (3 - 2*t); }
function _smoothstep(const edge0, edge1, x: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF} { T t = clamp((x-edge0) / (edge1-edge0), T(0), T(1)); return t * t * (3 - 2*t); }
function _smoothstep(const edge0, edge1, x: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { T t = clamp((x-edge0) / (edge1-edge0), T(0), T(1)); return t * t * (3 - 2*t); }
function _smoothstep(const edge0, edge1, x: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF} { T t = clamp((x-edge0) / (edge1-edge0), T(0), T(1)); return t * t * (3 - 2*t); }


function _distance(const a,b: Vec2): double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return length(p0-p1); }
function _distance(const a,b: Vec3): double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return length(p0-p1); }
function _distance(const a,b: Vec4): double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return length(p0-p1); }

function _fmods(a, b: double): Double;overload;
function _mod(a, b: Double): Double;{$IFDEF CPUx64}inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }

function _mod(const a, b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
function _mod(const a, b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
function _mod(const a, b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }

function _mod(const a: Vec2;b:TVecType): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
function _mod(const a: Vec3;b:TVecType): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
function _mod(const a: Vec4;b:TVecType): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }


function _min(x, y: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return y < x ? y : x; }
function _min(const x, y: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}        { return y < x ? y : x; }
function _min(const x, y: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _min(const x, y: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _max(x, y: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return x < y ? y : x; }
function _max(const x, y: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}        { return x < y ? y : x; }
function _max(const x, y: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}       { return x < y ? y : x; }
function _max(const x, y: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}       { return x < y ? y : x; }
function _maxComp(const p: Vec3): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _pow(const x, y: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _pow(const a, b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _pow(const x, y: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;


//function sqrt(const a: Single): Single;{$IFNDEF DEBUG} inline; {$ENDIF} overload;
function _sqrt(const a: Double): Double;{$IFDEF DO_INLINE}inline{$ENDIF} overload;
function _sqrt(const a: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _sqrt(const a: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;

function _sqrt(const a: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _sqrts(const a: TVecType): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _sqrts(const a: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _sqrts(const a: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _sqrts(const a: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;

function _clamp(const x, minVal, maxVal: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return glsl::min(glsl::max(x,minVal),maxVal); }
function _clamp(const x, minVal, maxVal: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return glsl::min(glsl::max(x,minVal),maxVal); }
function _clamp(const x:Vec2; minVal, maxVal: Double): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return glsl::min(glsl::max(x,minVal),maxVal); }
function _clamp(const x:Vec3; minVal, maxVal: Double): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return glsl::min(glsl::max(x,minVal),maxVal); }
function _clamp(const x:Vec4; minVal, maxVal: Double): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return glsl::min(glsl::max(x,minVal),maxVal); }

function _mix(x, y, a: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return x*(1-a) + y*a; }
function _mix(const x, y, a: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return x*(1-a) + y*a; }
function _mix(const x, y, a: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return x*(1-a) + y*a; }
function _mix(const x, y, a: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return x*(1-a) + y*a; }

function _mix(const x, y:Vec2; a: TVecType): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return x*(1-a) + y*a; }
function _mix(const x, y:Vec3; a: TVecType): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return x*(1-a) + y*a; }
function _mix(const x, y:Vec4; a: TVecType): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return x*(1-a) + y*a; }

function _step(edge, x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return x<=edge ? T(0) : T(1); }
function _step(const edge, x: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF} { return x<=edge ? T(0) : T(1); }
function _step(const edge, x: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF} { return x<=edge ? T(0) : T(1); }
function _step(const edge, x: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF} { return x<=edge ? T(0) : T(1); }

function _Length(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return cmath::sqrt(x*x); }
function _Length(const x: Vec2): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}    { return cmath::sqrt(x*x); }
function _Length(const x: Vec3): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}    { return cmath::sqrt(x*x); }
function _Length(const x: Vec4): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}    { return cmath::sqrt(x*x); }
function _length_sq(const x: Vec2): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF} { return cmath::sqrt(x*x); }
function _length_sq(const x: Vec3): Double; overload;{$IFDEF DO_INLINE} inline;{$ENDIF} { return cmath::sqrt(x*x); }

function _Normalize(x:double): Double; { return T(1); } inline; overload; // this is not the most useful function in the world
function _Normalize(const v: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _Normalize(const v: Vec3): Vec3;{$IFDEF DO_INLINE} inline; {$ENDIF} overload;
function _normalizeS(const v:Vec3) : vec3;inline;{$IFDEF DO_INLINE} inline; {$ENDIF} overload;

function _Normalize(const v: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;

function _sinLarge(const x: TVecType): TVecType; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _sinLarge(const x: Vec2   ): Vec2   ; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _sinLarge(const x: Vec3   ): Vec3   ; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _sinLarge(const x: Vec4   ): Vec4   ; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _cosLarge(const x: TVecType): TVecType; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _cosLarge(const x: Vec2   ): Vec2   ; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _cosLarge(const x: Vec3   ): Vec3   ; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _cosLarge(const x: Vec4   ): Vec4   ; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _sin(const x: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _sin(const x: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _sin(const x: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

function _cos(const x: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _cos(const x: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _cos(const x: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

procedure _cos(const x: Vec2;out Result:vec2); overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
procedure _cos(const x: Vec3;out Result:vec3); overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
procedure _cos(const x: Vec4;out Result:vec4); overload;{$IFDEF DO_INLINE} inline;{$ENDIF}


procedure _Mult(const input: Vec3;out Result:vec3);inline;

function _Ifthen(c:Boolean;const a,b:Vec2):Vec2;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _Ifthen(c:Boolean;const a,b:Vec3):Vec3;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
function _Ifthen(c:Boolean;const a,b:Vec4):Vec4;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}

/// <summary>
///  Available only in the fragment shader, dFdx and dFdy return the partial derivative of expression p in x and y, respectively.
///  Deviatives are calculated using local differencing.
///  Expressions that imply higher order derivatives such as dFdx(dFdx(n)) have undefined results,
//   as do mixed-order derivatives such as dFdx(dFdy(n)).
//   It is assumed that the expression p is continuous and therefore,
//   expressions evaluated via non-uniform control flow may be undefined.
/// </summary>
//function dFdx(a:TVecType):TVecType;
//function dFdx(a:Vec2):Vec2;
//function dFdx(a:Vec3):Vec3;
//function dFdx(a:Vec4):Vec4;

function _fwidth(const a: Vec2): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _fwidth(const a: Vec3): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _fwidth(const a: Vec4): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;

// Created by inigo quilez - iq/2013
// License Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License.
function _hash(n:Double):Double;overload;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _hash(const n:vec2):vec2;overload;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _hash(const n:vec3):vec3;overload;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
function _hash(const n:vec4):vec4;overload;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;

function _lerp(a,b : TVecType; rel : TVecType) : TVecType;

procedure _vect2sArrayCopy(var source,target : Vec2sArray);



const
  vec2Black:Vec2=(x:0;y:0);
  vec2White:Vec2=(x:1;y:1);
  vec2Gray:Vec2=(x:0.5;y:0.5);
  vec2_3_3:Vec2=(x:3;y:3);

  vec3Black:Vec3=(x:0;y:0;z:0);
  vec3White:Vec3=(x:1;y:1;z:1);
  vec3Gray:Vec3=(x:0.5;y:0.5;z:0.5);
  vec3Green:Vec3=(x:0;y:1;z:0);
  vecBlack:Vec3=(x:0;y:0;z:0);
  vecWhite:Vec3=(x:1;y:1;z:1);
  vec3Red:Vec3=(x:1;y:0;z:0);

  vec4Black:Vec4=(x:0;y:0;z:0;w:0);
  vec4White:Vec4=(x:1;y:1;z:1;w:1);
  vec4Gray:Vec4=(x:0.5;y:0.5;z:0.5;w:0.5);




implementation



function Mat3Identity : Mat3;
begin
  result := Mat3.Create( 1,0,0,
                         0,1,0,
                         0,0,1
                         );
end;

function Mat4Identity : Mat4;
begin
  result := Mat4.Create( 1,0,0,0,
                         0,1,0,0,
                         0,0,1,0,
                         0,0,0,1
                         );
end;

function Mat4CreateLookAtDirLH(source,direction,ceiling : Vec3) : Mat4;
var
  ZAxis, XAxis, YAxis: vec3;
begin
  ZAxis := - direction.Normalize^;
  XAxis := Ceiling.Cross(ZAxis).Normalize^;
  YAxis := ZAxis.Cross(XAxis);

  Result := Mat4Identity;
  Result.r1.x := XAxis.X;
  Result.r1.y := YAxis.X;
  Result.r1.z := ZAxis.X;
  Result.r2.x := XAxis.Y;
  Result.r2.y := YAxis.Y;
  Result.r2.z := ZAxis.Y;
  Result.r3.x := XAxis.Z;
  Result.r3.y := YAxis.Z;
  Result.r3.z := ZAxis.Z;
  Result.r4.x := - XAxis.Dot(source);
  Result.r4.y := - YAxis.Dot(source);
  Result.r4.z := - ZAxis.Dot(source);
end;


function Mat4CreatePerspectiveFovLH(const FOV, Aspect, ZNear, ZFar: Single;
  const HorizontalFOV: Boolean = False): Mat4;
var
  XScale, YScale: Single;
begin
  if HorizontalFOV then
  begin
    XScale := 1 / Tan(FOV / 2);
    YScale := XScale / Aspect;
  end else
  begin
    YScale := 1 / Tan(FOV / 2);
    XScale := YScale / Aspect;
  end;

  Result := Mat4Identity;
  Result.r1.x := XScale;
  Result.r2.y := YScale;
  Result.r3.z := ZFar / (ZFar - ZNear);
  Result.r3.w := 1;
  Result.r4.z := -ZNear * ZFar / (ZFar - ZNear);
  Result.r4.w := 0;
end;

function Mat4CreateRotationX(angle : single): Mat4;
var
  Sine, Cosine: Single;
begin
  Sine := System.sin(angle);
  Cosine := System.cos(angle);

  Result := Mat4Identity;
  Result.r2.y := Cosine;
  Result.r2.z := Sine;
  Result.r3.y := - Result.r2.z;
  Result.r3.z := Result.r2.y;
end;

function Mat4CreateRotationY(angle : single): Mat4;
var
  Sine, Cosine: Single;
begin
  Sine := System.sin(angle);
  Cosine := System.cos(angle);

  Result := Mat4Identity;
  Result.r1.x := Cosine;
  Result.r1.z := - Sine;
  Result.r3.x := - Result.r1.z;
  Result.r3.z := Result.r1.x;
end;

function Mat4CreateTranslation(translation : vec3) : Mat4;
begin
  Result := Mat4Identity;
  Result.r4.x := translation.X;
  Result.r4.y := translation.Y;
  Result.r4.z := translation.Z;
end;

function Mat4CreateRotationZ(angle : single): Mat4;
var
  Sine, Cosine: Single;
begin
  Sine := System.sin(angle);
  Cosine := System.cos(angle);

  Result := Mat4Identity;
  Result.r1.x := Cosine;
  Result.r1.y := Sine;
  Result.r2.x := - Result.r1.y;
  Result.r2.y := Result.r1.x;
end;

function Mat4CreateOrthoOffCenterRH(Left, Top, Right, Bottom, ZNear, ZFar: Single): Mat4;
begin
  Result := Mat4Identity;
  Result.r1.x := 2 / (Right - Left);
  Result.r2.y := 2 / (Top - Bottom);
  Result.r3.z := 1 / (ZFar - ZNear);
  Result.r4.x := (Left + Right) / (Left - Right);
  Result.r4.y := (Top + Bottom) / (Bottom - Top);
  Result.r4.z := ZNear / (ZNear - ZFar);
end;


function Mat3CreateRotation(angle : single) : Mat3;
var
  Sine, Cosine: Single;
begin
  Sine := System.sin(angle);
  Cosine := System.cos(angle);

  Result := Mat3Identity;
  Result.r1.x := Cosine;
  Result.r1.y := Sine;
  Result.r2.x := -Sine;
  Result.r2.y := Cosine;
end;

function Mat3CreateTranslation(deltax, deltay : TVecType) : Mat3;
begin
  result := Mat3Identity;
  result.r3.x := deltax;
  result.r3.y := deltay;
end;

function Mat3CreateScaling(scalex, scaley : TVecType) : Mat3;
begin
  result := Mat3Identity;
  result.r1.x := scalex;
  result.r2.y := scaley;
end;



function _EnsureRange(const AValue, AMin, AMax: Single): Single;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function _EnsureRange(const AValue, AMin, AMax: Double): Double;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;



{
function _fmod(a, b: extended): extended;
begin
  if IsZero(b) then
    Exit(0);

	Result := a - b * floor(a / b);
end;

function _fmod(a, b: double): double;
begin
  if IsZero(b) then
    Exit(0);

	Result := a - b * floor(a / b);
end;
}

function _fmod(a, b: single): single;overload; {$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  if IsZero(b) then
    Exit(0);

	Result := a - b * {math.floor}trunc(a / b);
end;

function _fmods(a, b: double): double;overload;
begin
  if IsZero(b) then
    Exit(0);
  if a>1e10 then
    Exit(0);
  if a<-1e10 then
    Exit(0);

	Result := a - b * math.floor(a / b);
end;


function _fmod(a, b: double): double;overload;
{$IFDEF CPUx86}
asm
  fld qword ptr[b]
  fld qword ptr[a]
@r:
  fprem
  fstsw ax
  sahf
  jp @r
  fstp st(1)
end;
{$ELSE}
begin
  if IsZero(b) then
    Exit(0);
//  if a>1e10 then
//    Exit(0);
//  if a<-1e10 then
//    Exit(0);

	Result := a - b * {math.floor}trunc(a / b);
end;
{$ENDIF}

/// <summary>
/// pow returns the value of x raised to the y power. i.e., xy. Results are undefined if x0 or if x0 and y0.
/// </summary>
function _pow(x,y:double):double;
begin
  if IsNaN(x) then
    Exit(0.000001);

  if x<0 then
    x := -x;

  if (x=0) and (y<0) then
    Exit(0.000001);

  Result := Math.Power(x,y)
end;
function _radians(degrees: double): double;
begin
  Result := (pi / 180) * degrees;
end;

function _degrees(radians: double): double;
begin
  Result := (180 / pi) * radians;
end;

function _exp2(x: double): double;
begin
  Result := Power(2,x);
end;

function _log(x: double): double;overload;
begin
  if x<0 then
    x := -x
  else
    if x=0 then
      exit(0);

  Result := ln(x);
end;

function _log(x: vec2): vec2;overload;
begin
  Result.x := ln(x.x);
  Result.y := ln(x.y);
end;
function _log(x: vec3): vec3;overload;
begin
  Result.x := ln(x.x);
  Result.y := ln(x.y);
  Result.z := ln(x.z);
end;

function _log2(x: single): single;
begin
  if x<0 then
    x := -x
  else
    if x=0 then
      exit(0);

  Result := Math.log2(x);
end;

function _log2(x: double): double;
begin
  if x<0 then
    x := -x
  else
    if x=0 then
      exit(0);

  Result := Math.log2(x);
end;

function _inversesqrt(x: single): single;
begin
  if x=0 then
    Exit(0)
  else
    if x<0 then
      x:=-x;

  Result := 1 / System.Sqrt(x);
end;

function _inversesqrt(x: double): double;
begin
  if x=0 then
    Exit(0)
  else
    if x<0 then
      x:=-x;

  Result := 1 / System.Sqrt(x);
end;

(*
// taken from:
// http://delphigamedev.com/forums/viewtopic.php?f=11&t=130
// it actually contains a bug, because this only works for 32 bits floats
function _inversesqrt(x: single): single;
var
   XHalf: Single;
   I: Integer Absolute X;
   X2: Single Absolute I;
   XB: Single;
begin
  XB:= X;
  XHalf:= 0.5 * X;
  I:= $5f3759df - (I SHR 1);
  X:= X2 * (1.5 - XHalf * X2 * X2);
  Result:= XB * X;
end;

function _inversesqrt(x: double): double;
var
   XHalf: Double;
   I: Int64 Absolute X;
   X2: Double Absolute I;
   XB: Double;
begin
  XB:= X;
  XHalf:= 0.5 * X;
  I:= $5fe6eb50c7b537a9 - (I SHR 1);
  X:= X2 * (1.5 - XHalf * X2 * X2);
  Result:= XB * X;
end;
*)


function _sign(x: double): double;
begin
  if x > 0 then
    Result := 1
  else if x < 0 then
    Result := -1
  else
    Result := 0;
end;

function _fract(x: double): double;
begin
//  Result := x - Math.Floor(x);
//  if IsNan(x) then  Exit(0);
//  if x>1e30 then
//    exit(0);
//  if x<-1e30 then
//    exit(0);

  Result := Trunc(X);
  if (X < 0) and (X - Result <> 0) then
    Result := Result-1;
   Result := x - Result;
end;

function _fract(const a: vec2): vec2;
begin
//  Result.x := x.x - Math.Floor(x.x);
//  Result.y := x.y - Math.Floor(x.y);

  Result.x := Trunc(a.x); if (a.x < 0) and (a.x - Result.x<>0) then Result.x := Result.x -1; Result.x := a.x - Result.x;
  Result.y := Trunc(a.y); if (a.y < 0) and (a.y - Result.y<>0) then Result.y := Result.y -1; Result.y := a.y - Result.y;
end;

function _fract(const a: vec3): vec3;
begin
//  Result.x := x.x - Math.Floor(x.x);
//  Result.y := x.y - Math.Floor(x.y);
//  Result.z := x.z - Math.Floor(x.z);
  Result.x := Trunc(a.x); if (a.x < 0) and (a.x - Result.x<>0) then Result.x := Result.x -1; Result.x := a.x - Result.x;
  Result.y := Trunc(a.y); if (a.y < 0) and (a.y - Result.y<>0) then Result.y := Result.y -1; Result.y := a.y - Result.y;
  Result.z := Trunc(a.z); if (a.z < 0) and (a.z - Result.z<>0) then Result.z := Result.z -1; Result.z := a.z - Result.z;


end;


function _fract(const a: vec4): vec4;
begin
//  Result.x := x.x - Math.Floor(x.x);
//  Result.y := x.y - Math.Floor(x.y);
//  Result.z := x.z - Math.Floor(x.z);
//  Result.w := x.w - Math.Floor(x.w);

  Result.x := Trunc(a.x); if (a.x < 0) and (a.x - Result.x<>0) then Result.x := Result.x -1; Result.x := a.x - Result.x;
  Result.y := Trunc(a.y); if (a.y < 0) and (a.y - Result.y<>0) then Result.y := Result.y -1; Result.y := a.y - Result.y;
  Result.z := Trunc(a.z); if (a.z < 0) and (a.z - Result.z<>0) then Result.z := Result.z -1; Result.z := a.z - Result.z;
  Result.w := Trunc(a.w); if (a.w < 0) and (a.w - Result.w<>0) then Result.w := Result.w -1; Result.w := a.w - Result.w;

end;


//function _floor(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;       { return x - cmath::floor(x); }
//begin
//  Result := Math.Floor(x);
//end;

function _floor(const a: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;       { return x - cmath::floor(x); }
begin
//  Result.x :=Math.Floor(x.x);
//  Result.y :=Math.Floor(x.y);

  Result.x := Trunc(a.x); if (a.x < 0) and (a.x-Result.x<>0) then Result.x := Result.x-1;
  Result.y := Trunc(a.y); if (a.y < 0) and (a.y-Result.y<>0) then Result.y := Result.y-1;
end;

function _floor(const a: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;       { return x - cmath::floor(x); }
begin
{  Result.x := Math.Floor(x.x);
  Result.y := Math.Floor(x.y);
  Result.z := Math.Floor(x.z);
  }

//  Result.x := IntegerTrunc(X.x); if Frac(X.x) < 0 then Result.x := Result.x-1;
//  Result.y := IntegerTrunc(X.y); if Frac(X.y) < 0 then Result.y := Result.y-1;
//  Result.z := IntegerTrunc(X.z); if Frac(X.z) < 0 then Result.z := Result.z-1;

  Result.x := Trunc(a.x); if (a.x < 0) and (a.x-Result.x<>0) then Result.x := Result.x-1;
  Result.y := Trunc(a.y); if (a.y < 0) and (a.y-Result.y<>0) then Result.y := Result.y-1;
  Result.z := Trunc(a.z); if (a.z < 0) and (a.z-Result.z<>0) then Result.z := Result.z-1;
end;

function _floor(const a: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;       { return x - cmath::floor(x); }
begin
{
  Result.x := Math.Floor(x.x);
  Result.y := Math.Floor(x.y);
  Result.z := Math.Floor(x.z);
  Result.w := Math.Floor(x.w);
}

//  Result.x := IntegerTrunc(X.x); if Frac(X.x) < 0 then Result.x := Result.x-1;
//  Result.y := IntegerTrunc(X.y); if Frac(X.y) < 0 then Result.y := Result.y-1;
//  Result.z := IntegerTrunc(X.z); if Frac(X.z) < 0 then Result.z := Result.z-1;
//  Result.w := IntegerTrunc(X.w); if Frac(X.w) < 0 then Result.w := Result.w-1;

  Result.x := Trunc(a.x); if (a.x < 0) and (a.x-Result.x<>0) then Result.x := Result.x-1;
  Result.y := Trunc(a.y); if (a.y < 0) and (a.y-Result.y<>0) then Result.y := Result.y-1;
  Result.z := Trunc(a.z); if (a.z < 0) and (a.z-Result.z<>0) then Result.z := Result.z-1;
  Result.w := Trunc(a.w); if (a.w < 0) and (a.w-Result.w<>0) then Result.w := Result.w-1;

end;


function _mod(a, b: double): double;
{$IFDEF CPUx86}
asm
  fld qword ptr[b]
  fld qword ptr[a]
@r:
  fprem
  fstsw ax
  sahf
  jp @r
  fstp st(1)
end;
{$ELSE}
begin
  if IsZero(b) then
    Exit(0);

	Result := a - b * math.Floor(a / b);
end;
{$ENDIF}

function _mod(const a, b: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
begin
  Result.x := _fMod(a.x, b.x);
  Result.y := _fMod(a.y, b.y);
end;

function _mod(const a, b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
begin
  Result.x := _fmod(a.x, b.x);
  Result.y := _fmod(a.y, b.y);
  Result.z := _fmod(a.z, b.z);
end;

function _mod(const a, b: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
begin
  Result.x := _fmod(a.x, b.x);
  Result.y := _fmod(a.y, b.y);
  Result.z := _fmod(a.z, b.z);
  Result.w := _fmod(a.w, b.w);
end;

function _mod(const a: Vec2;b:TVecType): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
begin
//  Result.x := fmod(a.x, b);
//  Result.y := fmod(a.y, b);
  Result.x := a.x - b * floor(a.x / b);
  Result.y := a.y - b * floor(a.y / b);
end;


function _mod(const a: Vec3;b:TVecType): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
begin
  Result.x := _fmod(a.x, b);
  Result.y := _fmod(a.y, b);
  Result.z := _fmod(a.z, b);
end;

function _mod(const a: Vec4;b:TVecType): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;     { return T(cmath::fmod(x, y)); }
begin
  Result.x := _fmod(a.x, b);
  Result.y := _fmod(a.y, b);
  Result.z := _fmod(a.z, b);
  Result.w := _fmod(a.w, b);
end;


function _Cross(const a,b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  Result := Vec3.create(
    a.y * b.z - a.z * b.y,
    a.z * b.x - a.x * b.z,
    a.x * b.y - a.y * b.x
  );

end;


function _min(x, y: double): double;
begin
  if y < x then
    Result := y
  else
    Result := x;
end;

function _max(x, y: double): double;
begin
  if x < y then
    Result := y
  else
    Result := x;
end;

function _clamp(x:double): Double;
begin
  if x>1 then
    Result := 1
  else
    if x<0 then
      Result := 0
    else
      Result := x
end;

function _clamp(x, minVal, maxVal: Double): Double;
begin
  //  Result := Math.min(Math.max(x, minVal), maxVal);
  if x>maxVal then
    Result := maxVal
  else
    if x<minVal then
      Result := minVal
    else
      Result := x
end;


function _mix(x, y, a: Double): Double;
begin
  Result := x * (1 - a) + y * a;
end;

function _step(edge, x: Double): Double;
begin
  if x <= edge then
    Result := 0
  else
    Result := 1;
end;



function _distance(p0, p1: double): double;
begin
  Result := system.sqrt(system.Abs(p0) - System.abs(p1));
end;

function _distance(const a,b: Vec2): double;
var dx,dy:double;
begin
  dx := a.x - b.x;
  dy := a.y - b.y;
  Result := system.sqrt(dx*dx + dy*dy);
end;

function _distance(const a,b: Vec3): double;
var dx,dy,dz:double;
begin
  dx := a.x - b.x;
  dy := a.y - b.y;
  dz := a.z - b.z;
  Result := system.sqrt(dx*dx + dy*dy + dz*dz);
end;

function _distance(const a,b: Vec4): double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload; { return length(p0-p1); }
var dx,dy,dz,dw:double;
begin
  dx := a.x - b.x;
  dy := a.y - b.y;
  dz := a.z - b.z;
  dw := a.w - b.w;
  Result := system.sqrt(dx*dx + dy*dy + dz*dz + dw*dw);
end;

function _Dot(x, y: Single): Single;
begin
  Result := x * y;
end;


function _dot(x, y: double): double;
begin
  Result := x * y;
end;

function _normalize(x:double): double;
begin
  if x<0 then
    Result := -1
  else
    Result := 1;
end;


function _reflect(I, n: double): double;
begin
  Result := I - 2 * n * I * n;
end;

function _reflect(const I, n: Vec3): Vec3;
begin
{  Result.x := I.x - 2 * n.x * I.x * n.x;
  Result.y := I.y - 2 * n.y * I.y * n.y;
  Result.z := I.z - 2 * n.z * I.z * n.z;
}
  Result.x := I.x - n.x * i.x * n.x * n.x;
  Result.y := I.y - n.y * i.y * n.y * n.y;
  Result.z := I.z - n.z * i.z * n.z * n.z;
end;



function _FaceForward(const N,I,NRef:TVecType): TVecType;overload;
begin
  if _dot(Nref, I) < 0 then
    Result := N
  else
    Result := -N
end;

function _FaceForward(const N,I,NRef:Vec2): Vec2; overload;
begin
  if _dot(Nref, I) < 0 then
    Result := N
  else
    Result := -N
end;

function _FaceForward(const N,I,NRef:Vec3): Vec3; overload;
begin
  if _dot(Nref, I) < 0 then
    Result := N
  else
    Result := -N
end;

function _FaceForward(const N,I,NRef:Vec4): Vec4; overload;
begin
  if _dot(Nref, I) < 0 then
    Result := N
  else
    Result := -N
end;


function _Refract(const I, N:TVecType; eta:Double):TVecType;overload;
var k:double; d:TVecType;
begin
  d := N * I;
  k := 1.0 - eta * eta * (1.0 - d * d);
  if k < 0.0 then
    Result := 0.0
  else
    Result := eta * I - (eta * d + system.sqrt(k)) * N
end;

function _Refract(const I, N:vec2; eta:Double):vec2;overload;
var k:double; d:float;
begin
  d := _dot(N , I);
  k := 1.0 - eta * eta * (1.0 - d * d);
  if k < 0.0 then
    Result := vec2Black
  else
    Result := eta * I - (eta * d + system.sqrt(k)) * N
end;

function _Refract(const I, N:vec3; eta:Double):vec3;overload;
var k:double; d:double;
begin
  d := _dot(N , I);
  k := 1 - eta * eta * (1 - d * d);
  if k < 0 then
    Result := vec3Black
  else
    Result := eta * I - (eta * d + system.sqrt(k)) * N
end;

function _Refract(const I, N:vec4; eta:Double):vec4;overload;
var k:double; d:double;
begin
  d := _dot(N , I);
  k := 1.0 - eta * eta * (1.0 - d * d);
  if k < 0.0 then
    Result := vec4Black
  else
    Result := eta * I - (eta * d + system.sqrt(k)) * N
end;



{------------------------------------------------------------------------------}

function _min(const x, y: vec2): vec2;
begin
  Result.x := Math.min(x.x,y.x);
  Result.y := Math.min(x.y,y.y);
end;

function _min(const x, y: vec3) : vec3;
begin
  Result.x := Math.min(x.x,y.x);
  Result.y := Math.min(x.y,y.y);
  Result.z := Math.min(x.z,y.z);
end;

function _min(const x, y: vec4) : vec4;
begin
  Result.x := Math.min(x.x,y.x);
  Result.y := Math.min(x.y,y.y);
  Result.z := Math.min(x.z,y.z);
  Result.w := Math.min(x.w,y.w);
end;

function _max(const x, y: vec2): vec2;
begin
  Result.x := Math.max(x.x,y.x);
  Result.y := Math.max(x.y,y.y);
end;

function _max(const x, y: vec3): vec3;
begin
  Result.x := Math.max(x.x,y.x);
  Result.y := Math.max(x.y,y.y);
  Result.z := Math.max(x.z,y.z);
end;

function _max(const x, y: vec4): vec4;
begin
  Result.x := Math.max(x.x,y.x);
  Result.y := Math.max(x.y,y.y);
  Result.z := Math.max(x.z,y.z);
  Result.w := Math.max(x.w,y.w);
end;

function _maxComp(const p: vec3) : double;
begin
  Result := Math.max(p.x,Math.max(p.y,p.z));
end;

function _pow(const x, y: Vec2): Vec2;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  Result.x := Power(x.x,y.x);
  Result.y := Power(x.y,y.y);
end;
function _pow(const a, b: Vec3): Vec3;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  Result.x := Power(a.x,b.x);
  Result.y := Power(a.y,b.y);
  Result.z := Power(a.z,b.z);
end;
function _pow(const x, y: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  Result.x := Power(x.x,y.x);
  Result.y := Power(x.y,y.y);
  Result.z := Power(x.z,y.z);
  Result.w := Power(x.w,y.w);
end;

{
function _f_root(i:single;n:integer):single;
var l:longint;
begin
 l:=longint((@i)^);
 l:=l-$3F800000;l:=l shr (n-1);
 l:=l+$3F800000;
 result:=single((@l)^);
end;
}


//function _sqrt(const a: Single): Single;
//begin
//  if a<=0 then
//    Exit(0)
//  else
//    Result := sqrtFast(a);
//end;

function _sqrt(const a: Double): Double;
begin
//  if a<=0 then
//    Exit(0)
//  else

    Result := system.Sqrt(a);
end;

function _sqrt(const a: Vec2): Vec2;
begin
  result.x := System.sqrt(a.x);
  result.y := System.sqrt(a.y);
end;

function _sqrt(const a: Vec3): Vec3;
begin
  result.x := System.sqrt(a.x);
  result.y := System.sqrt(a.y);
  result.z := System.sqrt(a.z);
end;

function _sqrt(const a: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  result.x := System.sqrt(a.x);
  result.y := System.sqrt(a.y);
  result.z := System.sqrt(a.z);
  result.w := System.sqrt(a.w);
end;

function _sqrts(const a: TVecType): TVecType;
begin
  if a<0 then result := -1 else result := System.sqrt(a);
//  if a<0 then result := 0 else result := System.sqrt(a);
end;


function _sqrts(const a: Vec2): Vec2;
begin
  if a.x<0 then result.x := 0 else result.x := System.sqrt(a.x);
  if a.y<0 then result.y := 0 else result.y := System.sqrt(a.y);
end;


function _sqrts(const a: Vec3): Vec3;
begin
  if a.x<0 then result.x := 0 else result.x := System.sqrt(a.x);
  if a.y<0 then result.y := 0 else result.y := System.sqrt(a.y);
  if a.z<0 then result.z := 0 else result.z := System.sqrt(a.z);
end;

function _sqrts(const a: Vec4): Vec4;
begin
  if a.x<0 then result.x := 0 else result.x := System.sqrt(a.x);
  if a.y<0 then result.y := 0 else result.y := System.sqrt(a.y);
  if a.z<0 then result.z := 0 else result.z := System.sqrt(a.z);
  if a.w<0 then result.w := 0 else result.w := System.sqrt(a.w);
end;





function _clamp(const x, minVal, maxVal: vec2): vec2;
begin
  Result.x := Math.min(Math.max(x.x, minVal.x), maxVal.x);
  Result.y := Math.min(Math.max(x.y, minVal.y), maxVal.y);
end;

function _clamp(const x, minVal, maxVal: vec3): vec3;
begin
  Result.x := Math.min(Math.max(x.x, minVal.x), maxVal.x);
  Result.y := Math.min(Math.max(x.y, minVal.y), maxVal.y);
  Result.z := Math.min(Math.max(x.z, minVal.z), maxVal.z);
end;

function _clamp(const x:Vec2; minVal, maxVal: Double): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return glsl::min(glsl::max(x,minVal),maxVal); }
begin
  Result.x := Math.min(Math.max(x.x, minVal), maxVal);
  Result.y := Math.min(Math.max(x.y, minVal), maxVal);
end;


function _clamp(const x:Vec3; minVal, maxVal: Double): Vec3; overload; inline; { return glsl::min(glsl::max(x,minVal),maxVal); }
begin

  Result.x := Math.min(Math.max(x.x, minVal), maxVal);
  Result.y := Math.min(Math.max(x.y, minVal), maxVal);
  Result.z := Math.min(Math.max(x.z, minVal), maxVal);

//  Result := x;
//  if x.x<minVal then Result.x := minVal else if x.x >maxVal then Result.x := maxVal;
//  if x.y<minVal then Result.y := minVal else if x.y >maxVal then Result.y := maxVal;
//  if x.z<minVal then Result.z := minVal else if x.z >maxVal then Result.z := maxVal;

//  Result.x := Math.EnsureRange(x.x,minVal,maxVal);
//  Result.y := Math.EnsureRange(x.y,minVal,maxVal);
//  Result.z := Math.EnsureRange(x.z,minVal,maxVal);
end;

function _clamp(const x:Vec4; minVal, maxVal: Double): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}  { return glsl::min(glsl::max(x,minVal),maxVal); }
begin
  Result.x := Math.min(Math.max(x.x, minVal), maxVal);
  Result.y := Math.min(Math.max(x.y, minVal), maxVal);
  Result.z := Math.min(Math.max(x.z, minVal), maxVal);
  Result.w := Math.min(Math.max(x.w, minVal), maxVal);
end;


function _mix(const x, y, a: vec2): vec2;
begin
  Result.x := x.x * (1 - a.x) + y.x * a.x;
  Result.y := x.y * (1 - a.y) + y.y * a.y;
end;

function _mix(const x, y, a: vec3): vec3;
begin
  Result.x := x.x * (1 - a.x) + y.x * a.x;
  Result.y := x.y * (1 - a.y) + y.y * a.y;
  Result.z := x.z * (1 - a.z) + y.z * a.z;
end;

function _mix(const x, y, a: vec4): vec4;
begin
  Result.x := x.x * (1 - a.x) + y.x * a.x;
  Result.y := x.y * (1 - a.y) + y.y * a.y;
  Result.z := x.z * (1 - a.z) + y.z * a.z;
  Result.w := x.w * (1 - a.w) + y.w * a.w;
end;

function _mix(const x, y:Vec2; a: TVecType): Vec2;
begin
  Result.x := x.x * (1 - a) + y.x * a;
  Result.y := x.y * (1 - a) + y.y * a;
end;

function _mix(const x, y:Vec3; a: TVecType): Vec3;
begin
  Result.x := x.x * (1 - a) + y.x * a;
  Result.y := x.y * (1 - a) + y.y * a;
  Result.z := x.z * (1 - a) + y.z * a;
end;


function _mix(const x, y:Vec4; a: TVecType): Vec4;
begin
  Result.x := x.x * (1 - a) + y.x * a;
  Result.y := x.y * (1 - a) + y.y * a;
  Result.z := x.z * (1 - a) + y.z * a;
  Result.w := x.w * (1 - a) + y.w * a;
end;

function _step(const edge, x: vec2): vec2;
begin
  if x.x<=edge.x then result.x := 0 else result.x := 1;
  if x.y<=edge.y then result.y := 0 else result.y := 1;
end;

function _step(const edge, x: vec3): vec3;
begin
  if x.x<=edge.x then result.x := 0 else result.x := 1;
  if x.y<=edge.y then result.y := 0 else result.y := 1;
  if x.z<=edge.z then result.z := 0 else result.z := 1;
end;

function _step(const edge, x: vec4): vec4;
begin
  if x.x<=edge.x then result.x := 0 else result.x := 1;
  if x.y<=edge.y then result.y := 0 else result.y := 1;
  if x.z<=edge.z then result.z := 0 else result.z := 1;
  if x.w<=edge.w then result.w := 0 else result.w := 1;
end;

function _length(x: double): double;
begin
  Result := system.Abs(x)
end;

function _length(const x: vec2): double;
begin
  Result := System.Sqrt( x.x * x.x
                       + x.y * x.y);
end;

function _Length(const x: Vec3): Double;
begin
  Result := System.Sqrt(
                  x.x * x.x
                + x.y * x.y
                + x.z * x.z
                );
end;

function _Length(const x: Vec4): Double;
begin
  Result := System.sqrt( x.x * x.x
                + x.y * x.y
                + x.z * x.z
                + x.w * x.w);
end;

function _length_sq(const x: Vec2): Double;
begin
  Result := x.x * x.x
          + x.y * x.y;
end;


function _length_sq(const x: Vec3): Double;
begin
  Result := x.x * x.x
          + x.y * x.y
          + x.z * x.z;
end;


{ Vec3 }

class operator Vec3.Add(const a, b: Vec3): Vec3;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
end;

function Vec3.Abs: Vec3;
begin
  Result.x := system.Abs(x);
  Result.y := system.Abs(y);
  Result.z := system.Abs(z);
end;

class operator Vec3.Add(const a: Vec3; b: TVecType): Vec3;
begin
  Result.x := a.x + b;
  Result.y := a.y + b;
  Result.z := a.z + b;
end;

constructor Vec3.create(aX: TVecType; const yz: Vec2);
begin
  x := x;
  y := yz.x;
  z := yz.y;
end;

constructor Vec3.create(const xy: Vec2; az: TVecType);
begin
  x := xy.x;
  y := xy.y;
  z := az;
end;

constructor Vec3.create(ax: TVecType);
begin
  x := ax;
  y := ax;
  z := ax;
end;

constructor Vec3.create(ax, ay, az: TVecType);
begin
  x := ax;
  y := ay;
  z := az;
end;


function Vec3.Cross(const b: Vec3): Vec3;
begin
  Result := Vec3.create(y * b.z - z * b.y, z * b.x - x * b.z, x * b.y - y * b.x);
end;

class operator Vec3.Divide(const a: Vec3; b: TVecType): Vec3;
begin
  Result.x := a.x / b;
  Result.y := a.y / b;
  Result.z := a.z / b;
end;

function Vec3.Dot(const b: Vec3): TVecType;
begin
  Result := x * b.x + y * b.y + z * b.z;
end;

class operator Vec3.Equal(const a, b: Vec3): Boolean;
begin
  Result := (a.x = b.x) and (a.y = b.y) and (a.z = b.z);
end;

class operator Vec3.Explicit(const a: Vec3): Int32;
var R,G,B:Byte;
begin
//  Result := Color32(Min(255, round(a.x)),Min(255,round(a.y)),Min(255,round(a.z)))
{  Result := Color32(
              EnsureRange(round(a.x),0,255),
              EnsureRange(round(a.y),0,255),
              EnsureRange(round(a.z),0,255)
              )}

  if a.r > 1 then r := 255 else if a.r < 0 then R := 0 else R := trunc(a.r*255);
  if a.g > 1 then g := 255 else if a.g < 0 then G := 0 else G := trunc(a.g*255);
  if a.b > 1 then b := 255 else if a.b < 0 then B := 0 else B := trunc(a.b*255);

  Result := $ff000000 or (R shl 16) or (G shl 8) or B;
end;


//class operator Vec3.Explicit(a: Vec3): TColor32;
//var r,g,b:byte;
//begin
//  {$R-}
//  r := Trunc(a.x);
//  g := Trunc(a.y);
//  b := Trunc(a.z);
//  {$R+}
//  Result := Color32(r,g,b)
//end;
//
class operator Vec3.Implicit(a: TVecType): Vec3;
begin
  Result.x := a;
  REsult.y := a;
  Result.z := a;
end;

class operator Vec3.Divide(const a, b: Vec3): Vec3;
begin
  Result.x := a.x / b.x;
  Result.y := a.y / b.y;
  Result.z := a.z / b.z;
end;

function Vec3.Length: TVecType;
begin
  Result := System.sqrt(x * x + y * y + z * z)
end;

function _abs(x: Single) : Single;
begin
  Result := System.Abs(x);
end;

function _abs(x: Double) : Double;
begin
  Result := System.Abs(x);
end;
{
function _abs(x: Extended) : Extended;
begin
  Result := System.Abs(x);
end;
}

function _abs(const x: vec2) : vec2;
begin
  Result.x := System.abs(x.x);
  Result.y := System.abs(x.y);
end;

function _abs(const x: vec3) : vec3;
begin
  Result.x := System.abs(x.x);
  Result.y := System.abs(x.y);
  Result.z := System.abs(x.z);
end;

function _abs(const x: vec4) : vec4;
begin
  Result.x := System.abs(x.x);
  Result.y := System.abs(x.y);
  Result.z := System.abs(x.z);
  Result.w := System.abs(x.w);
end;

function _asin(x:Single):Single;  begin Result := ArcSin(x) end;
function _acos(x:Single):Single;  begin Result := ArcCos(x) end;
//function _atan(x:Single):Single;  begin Result := x - (x * x * x * 0.333333333333) + (x * x * x * x * x * 0.2) - (x * x * x * x * x * x * x * 0.1428571429) + (x * x * x * x * x * x * x * x * x * 0.111111111111) - (x * x * x * x * x * x * x * x * x * x * x * 0.0909090909); end;
function _atan(x:Single):Single;  begin Result := arctan(x) end;
function _atan(x,y:Single):Single;begin Result := ArcTan2(x,y) end;
function _tan(x:Single):Single;   begin if x=pi/2 then Exit(0); Result := Tan(x); end;

function _asin(x: Double): Double;
var
  t: Double;
begin
  t := (1 + x) * (1 - x);
  if t < 0 then
    Exit(0);
  Result := Math.ArcTan2(x, System.Sqrt(t));
end;

function _acos(x: Double): Double;
var
  t: Double;
begin
  t := (1 + x) * (1 - x);
  if t < 0 then
    Exit(0);

  Result := Math.ArcTan2(System.sqrt(t), x);
end;

//function _atan(x:Double):Double;  begin Result := x - (x * x * x * 0.333333333333) + (x * x * x * x * x * 0.2) - (x * x * x * x * x * x * x * 0.1428571429) + (x * x * x * x * x * x * x * x * x * 0.111111111111) - (x * x * x * x * x * x * x * x * x * x * x * 0.0909090909); end;
function _atan(x:Double):Double;  begin Result := ArcTan(x) end;
function _atan(x,y:Double):Double;begin Result := ArcTan2(x,y) end;
function _tan(x:Double):Double;   begin if x=pi/2 then Exit(0); Result := Tan(x); end;

//function _acos(x:Extended):Extended;  begin Result := ArcCos(x) end;
//function _atan(x:Extended):Extended;  begin Result := x - (x * x * x * 0.333333333333) + (x * x * x * x * x * 0.2) - (x * x * x * x * x * x * x * 0.1428571429) + (x * x * x * x * x * x * x * x * x * 0.111111111111) - (x * x * x * x * x * x * x * x * x * x * x * 0.0909090909); end;
//function _atan(x,y:Extended):Extended;begin Result := ArcTan2(x,y) end;
//function _tan(x:Extended):Extended;   begin if x=pi/2 then Exit(0); Result := System.Tangent(x); end;


class operator Vec3.Multiply(const a: Vec3; b: TVecType): Vec3;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
  Result.z := a.z * b;
end;

class operator Vec3.Multiply(const a, b: Vec3): Vec3;
begin
  Result.x := a.x * b.x;
  Result.y := a.y * b.y;
  Result.z := a.z * b.z;
end;

class operator Vec3.Negative(const a: Vec3): Vec3;
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
  Result.Z := -a.Z;
end;

function Vec3.Normalize: PVec3;
var
  s, l: TVecType;
begin
  s := System.sqrt(x * x + y * y + z * z);
  if IsZero(s) then
    Exit(@self);

  l := 1 / s;
  x := x * l;
  y := y * l;
  z := z * l;
  Result := @Self;
end;



procedure Vec3.NormalizeSelf;
var
  s, l: TVecType;
begin
  s := x * x + y * y + z * z;

  if IsZero(s) then
    Exit;
  s := System.Sqrt(s);

  l := 1 / s;
  x := x * l;
  y := y * l;
  z := z * l;
end;


class operator Vec3.Subtract(const a: Vec2; const b: Vec3): Vec3;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
  result.z := 0   - b.z;
end;


class operator Vec3.Subtract(const a: Vec3; const b: Vec2): Vec3;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z;
end;


class operator Vec3.Subtract(const a, b: Vec3): Vec3;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z - b.z;
end;


class operator Vec3.Multiply(a: TVecType; const b: Vec3): Vec3;
begin
  Result.x := a * b.x;
  Result.y := a * b.y;
  Result.z := a * b.z;
end;

{ Vec2 }

class operator Vec2.Add(const a, b: Vec2): Vec2;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
end;

class operator Vec2.Add(const a: Vec2; b: TVecType): Vec2;
begin
  Result.x := a.x + b;
  Result.y := a.y + b;
end;

class operator Vec2.Add(a: TVecType; const b: Vec2): Vec2;
begin
  Result.x := a + b.x;
  Result.y := a + b.y;
end;

constructor Vec2.create(ax: TVecType);
begin
  x := ax;
  y := ax;
end;



constructor Vec2.create(ax, ay: TVecType);
begin
  x := ax;
  y := ay;
end;

function Vec2.Cross(const b: Vec2): Vec2;
begin
  Result := Vec2.create(y * b.x - x * b.y, x * b.y - y * b.x);
end;

class operator Vec2.Divide(const a, b: Vec2): Vec2;
begin
  Result.x := a.x / b.x;
  Result.y := a.y / b.y;
end;

class operator Vec2.Divide(const a: Vec2; b: TVecType): Vec2;
begin
  Result.x := a.x / b;
  Result.y := a.y / b;
end;

class operator Vec2.Divide(a: TVecType; const b: Vec2): Vec2;
begin
  Result.x := a / b.x;
  Result.y := a / b.y;
end;

function Vec2.Dot(const b: Vec2): TVecType;
begin
  Result := x * b.x + y * b.y;
end;

class operator Vec2.explicit(const a: Vec2): TPointF;
begin
  Result.X := a.x;
  Result.Y := a.y;
end;

class operator Vec2.explicit(const a: Vec2): TPoint;
begin
  result.X := round(a.x);
  result.Y := round(a.y);
end;

class operator Vec2.explicit(const b: TVecType): Vec2;
begin
  Result.x := b;
  Result.y := b;
end;

function Vec2.Length: TVecType;
begin
  Result := System.sqrt(x * x + y * y)
end;

class operator Vec2.Multiply(const a, b: Vec2): Vec2;
begin
  Result.x := a.x * b.x;
  Result.y := a.y * b.y;
end;

class operator Vec2.Multiply(const a: Vec2; b: TVecType): Vec2;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
end;

class operator Vec2.Multiply(a: TVecType; const b: Vec2): Vec2;
begin
  Result.x := a * b.x;
  Result.y := a * b.y;
end;

class operator Vec2.Negative(const a: Vec2): Vec2;
begin
  Result.x := -a.x;
  Result.y := -a.y;
end;

function Vec2.Normalize: PVec2;
var
  s, l: TVecType;
begin

    s := System.sqrt(x * x + y * y);
    if s = 0 then
      Exit(@self);

    l := 1.0 / s;
    x := x * l;
    y := y * l;

  Result := @Self;
end;

class operator Vec2.Subtract(const a: Vec2; b: TVecType): Vec2;
begin
  Result.x := a.x - b;
  Result.y := a.y - b;
end;

class operator Vec2.Subtract(a: TVecType; const b: Vec2): Vec2;
begin
  Result.x := a - b.x;
  Result.y := a - b.y;
end;

class operator Vec2.Subtract(const a, b: Vec2): Vec2;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
end;



function _dot(const x, y: vec2) : double;     overload;
begin
  Result := x.x * y.x +
            x.y * y.y;
end;

function _dot(const x, y: vec3) : double;     overload;
begin
  Result := x.x * y.x +
            x.y * y.y +
            x.z * y.z;
end;

function _dot(const x, y: vec4) : double;     overload;
begin
  Result := x.x * y.x +
            x.y * y.y +
            x.z * y.z +
            x.w * y.w;
end;

      {
// @@@ just copied from texture2d.. need to figure out how to implement this one
function _textureCube(tex:TBitmap32;Coords:Vec3):Vec4;
var
  x,y:Integer;
  c:TColor32;
begin
  x := round(abs(Coords.x) * tex.Width ) mod tex.Width;
  y := round(abs(Coords.y) * tex.Height) mod tex.Height;
  Assert(x>=0);
  Assert(y>=0);
  Assert(X<tex.Width);
  Assert(Y<tex.Height);
  c := tex.Pixel[x,y];
  Result.x := RedComponent(c);
  Result.y := GreenComponent(c);
  Result.z := BlueComponent(c);
  Result.m := AlphaComponent(c);
end;
}
(*
function _ceil(x: Double): Double;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  Result := math.Ceil(x)
end;
*)

function _ceil(const a: Vec2): Vec2; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  Result.x := Math.Ceil(a.x);
  Result.y := Math.Ceil(a.y);
end;

function _ceil(const a: Vec3): Vec3; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  Result.x := Math.Ceil(a.x);
  Result.y := Math.Ceil(a.y);
  Result.z := Math.Ceil(a.z);
end;

function _ceil(const a: Vec4): Vec4; overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  Result.x := Math.Ceil(a.x);
  Result.y := Math.Ceil(a.y);
  Result.z := Math.Ceil(a.z);
  Result.w := Math.Ceil(a.w);
end;



function _smoothstep(edge0, edge1, x: Single): Single;
var
  T: Double;
begin
  T := (x - edge0) / (edge1 - edge0);
  if T>1 then T := 1 else if T<0 then T := 0;
  Result := T * T * (3 - 2 * T);
end;

function _smoothstep(edge0, edge1, x: Double): Double;
var
  d, T: Double;
begin

  d := edge1-edge0;
  if Iszero(d) then
    Exit(0);

  T := (x - edge0) / d;
  if T>1 then T := 1 else if T<0 then T := 0;
  Result := T * T * (3 - 2 * T);
end;

{
function _smoothstep(edge0, edge1, x: Extended): Extended;
var
  T: Double;
begin
  T := (x - edge0) / (edge1 - edge0);
  if T>1 then T := 1 else if T<0 then T := 0;
  Result := T * T * (3 - 2 * T);
end;
}

function _smoothstep(const edge0, edge1, x: vec2): vec2;
begin
  {method 1: most readable}
//  Result.x := smoothstep(edge0.x,edge1.x,x.x);
//  Result.y := smoothstep(edge0.y,edge1.y,x.y);


  {method 2}
//  T.x := clamp((x.x - edge0.x) / (edge1.x - edge0.x), 0, 1);
//  Result.x := T.x * T.x * (3 - 2 * T.x);
//  T.y := clamp((x.y - edge0.y) / (edge1.y - edge0.y), 0, 1);
//  Result.y := T.y * T.y * (3 - 2 * T.y);

  {method 3: least readable, but almost twice as fast as method 1}
  result.x := (x.x - edge0.x) / (edge1.x - edge0.x);
  if result.x>1 then result.x := 1 else if result.x<0 then result.x := 0;
  Result.x := result.x * result.x * (3 - 2 * result.x);

  result.y := (x.y - edge0.y) / (edge1.y - edge0.y);
  if result.y>1 then result.y := 1 else if result.y<0 then result.y := 0;
  Result.y := result.y * result.y * (3 - 2 * result.y);
end;


function _smoothstep(const edge0, edge1, x: vec3) : vec3;overload;{ T t = clamp((x-edge0) / (edge1-edge0), T(0), T(1)); return t * t * (3 - 2*t); }
var T:vec3;
begin
{
  Result.x := smoothstep(edge0.x,edge1.x,x.x);
  Result.y := smoothstep(edge0.y,edge1.y,x.y);
  Result.z := smoothstep(edge0.z,edge1.z,x.z);
}
  T.x := (x.x - edge0.x) / (edge1.x - edge0.x);
  if T.x>1 then T.x := 1 else if T.x<0 then T.x := 0;
  Result.x := T.x * T.x * (3 - 2 * T.x);

  T.y := (x.y - edge0.y) / (edge1.y - edge0.y);
  if T.y>1 then T.y := 1 else if T.y<0 then T.y := 0;
  Result.y := T.y * T.y * (3 - 2 * T.y);

  T.z := (x.z - edge0.z) / (edge1.z - edge0.z);
  if T.z>1 then T.z := 1 else if T.z<0 then T.z := 0;
  Result.z := T.z * T.z * (3 - 2 * T.z);

end;


function _smoothstep(const edge0, edge1, x: vec4) : vec4;overload;{ T t = clamp((x-edge0) / (edge1-edge0), T(0), T(1)); return t * t * (3 - 2*t); }
var T:vec4;
begin
{
  Result.x := smoothstep(edge0.x,edge1.x,x.x);
  Result.y := smoothstep(edge0.y,edge1.y,x.y);
  Result.z := smoothstep(edge0.z,edge1.z,x.z);
  Result.w := smoothstep(edge0.w,edge1.w,x.w);
  }
  T.x := (x.x - edge0.x) / (edge1.x - edge0.x);
  if T.x>1 then T.x := 1 else if T.x<0 then T.x := 0;
  Result.x := T.x * T.x * (3 - 2 * T.x);

  T.y := (x.y - edge0.y) / (edge1.y - edge0.y);
  if T.y>1 then T.y := 1 else if T.y<0 then T.y := 0;
  Result.y := T.y * T.y * (3 - 2 * T.y);

  T.z := (x.z - edge0.z) / (edge1.z - edge0.z);
  if T.z>1 then T.z := 1 else if T.z<0 then T.z := 0;
  Result.z := T.z * T.z * (3 - 2 * T.z);

  T.w := (x.w - edge0.w) / (edge1.w - edge0.w);
  if T.w>1 then T.w := 1 else if T.w<0 then T.w := 0;
  Result.w := T.w * T.w * (3 - 2 * T.w);
end;


function _sinLarge(const x: TVecType): TVecType;
begin
  {$IFDEF CPUX64}
  Result := System.sin(_fMod(x,2*pi));
  {$ELSE}
  Result := System.sin(x);
  {$ENDIF}
end;

function _sinLarge(const x: Vec2): Vec2;
begin
  {$IFDEF CPUX64}
  Result.x := System.sin(_fMod(x.x,2*pi));
  Result.y := System.sin(_fMod(x.y,2*pi));
  {$ELSE}
  Result.x := System.sin(x.x);
  Result.y := System.sin(x.y);
  {$ENDIF}
end;

function _sinLarge(const x: Vec3): Vec3;
begin
  {$IFDEF CPUX64}
  Result.x := System.sin(_FMod(x.x,2*pi));
  Result.y := System.sin(_FMod(x.y,2*pi));
  Result.z := System.sin(_FMod(x.z,2*pi));
  {$ELSE}
  Result.x := System.sin(x.x);
  Result.y := System.sin(x.y);
  Result.z := System.sin(x.z);
  {$ENDIF}
end;

function _sinLarge(const x: Vec4): Vec4;
begin
  {$IFDEF CPUX64}
  Result.x := System.sin(_FMod(x.x,2*pi));
  Result.y := System.sin(_FMod(x.y,2*pi));
  Result.z := System.sin(_FMod(x.z,2*pi));
  Result.w := System.sin(_FMod(x.w,2*pi));
  {$ELSE}
  Result.x := System.sin(x.x);
  Result.y := System.sin(x.y);
  Result.z := System.sin(x.z);
  Result.w := System.sin(x.w);
  {$ENDIF}
end;

function _cosLarge(const x: TVecType): TVecType;
begin
  {$IFDEF CPUX64}
  Result := System.cos(_FMod(x,2*pi));
  {$ELSE}
  Result := System.cos(x);
  {$ENDIF}
end;

function _cosLarge(const x: Vec2): Vec2;
begin
  {$IFDEF CPUX64}
  Result.x := System.cos(_FMod(x.x,2*pi));
  Result.y := System.cos(_FMod(x.y,2*pi));
  {$ELSE}
  Result.x := System.cos(x.x);
  Result.y := System.cos(x.y);
  {$ENDIF}
end;

function _cosLarge(const x: Vec3): Vec3;
begin
  {$IFDEF CPUX64}
  Result.x := System.cos(_FMod(x.x,2*pi));
  Result.y := System.cos(_FMod(x.y,2*pi));
  Result.z := System.cos(_FMod(x.z,2*pi));
  {$ELSE}
  Result.x := System.cos(x.x);
  Result.y := System.cos(x.y);
  Result.z := System.cos(x.z);
  {$ENDIF}
end;

function _cosLarge(const x: Vec4): Vec4;
begin
  {$IFDEF CPUX64}
  Result.x := System.cos(_FMod(x.x,2*pi));
  Result.y := System.cos(_FMod(x.y,2*pi));
  Result.z := System.cos(_FMod(x.z,2*pi));
  Result.w := System.cos(_FMod(x.w,2*pi));
  {$ELSE}
  Result.x := System.cos(x.x);
  Result.y := System.cos(x.y);
  Result.z := System.cos(x.z);
  Result.w := System.cos(x.w);
  {$ENDIF}
end;



function _sin(const x: Vec2): Vec2;
begin
  result.x := System.sin(x.x);
  result.y := System.sin(x.y);
end;

function _sin(const x: Vec3): Vec3;
begin
  result.x := System.sin(x.x);
  result.y := System.sin(x.y);
  result.z := System.sin(x.z);
end;

function _sin(const x: Vec4): Vec4;
begin
  result.x := System.sin(x.x);
  result.y := System.sin(x.y);
  result.z := System.sin(x.z);
  result.w := System.sin(x.w);
end;

function _cos(const x: Vec2): Vec2;
begin
  result.x := System.cos(x.x);
  result.y := System.cos(x.y);
end;

function _cos(const x: Vec3): Vec3;
begin
  result.x := System.cos(x.x);
  result.y := System.cos(x.y);
  result.z := System.cos(x.z);
end;

function _cos(const x: Vec4): Vec4;
begin
  result.x := System.cos(x.x);
  result.y := System.cos(x.y);
  result.z := System.cos(x.z);
  result.w := System.cos(x.w);
end;


procedure _cos(const x: Vec2;out Result:vec2);
begin
  result.x := System.cos(x.x);
  result.y := System.cos(x.y);
end;

procedure _cos(const x: Vec3;out Result:vec3);
begin
  result.x := System.cos(x.x);
  result.y := System.cos(x.y);
  result.z := System.cos(x.z);
end;

procedure _cos(const x: Vec4;out Result:vec4);
begin
  result.x := System.cos(x.x);
  result.y := System.cos(x.y);
  result.z := System.cos(x.z);
  result.w := System.cos(x.w);
end;

procedure _Mult(const input: Vec3;out Result:vec3);inline;
begin
  Result.x := input.x * input.x;
  REsult.y := input.y * input.y;
  Result.z := input.z * input.z;
end;


function _normalize(const v:Vec2) : vec2;{$IFDEF DO_INLINE} inline;{$ENDIF}overload;
var
  m:TVecType;
begin
	m := System.sqrt(v.x * v.x + v.y * v.y);

	if(m >  0.000000001)then
		m := 1.0 / m
	else
		m := 0.0;

	Result := vec2.create(v.x * m, v.y * m);
end;

function _normalizeS(const v:Vec3) : vec3;inline;
var
  m:TVecType;
begin
  if IsNan(v.x) then exit(vec3Black);
  if IsNan(v.y) then exit(vec3Black);
  if IsNan(v.z) then exit(vec3Black);

	m := System.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);

	if(m >  0.000000001)then
		m := 1.0 / m
	else
		m := 0.0;

	Result.x := v.x * m;
  Result.y := v.y * m;
  Result.z := v.z * m;
end;


function _normalize(const v:Vec3) : vec3;inline;
var
  m:TVecType;
begin
	m := System.sqrt(v.x * v.x + v.y * v.y + v.z * v.z);
  if IsZero(m) then
    Exit(Vec3Black);

  m := 1.0 / m;
  Result.x := v.x * m;
  Result.y := v.y * m;
  Result.z := v.z * m;
end;

function _Normalize(const v: Vec4): Vec4;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
var
  a:TVecType;
begin
	a := System.sqrt(v.x * v.x + v.y * v.y + v.z * v.z + v.w * v.w);

	if(a >  0.000000001)then
		a := 1.0 / a
	else
		a := 0.0;

	Result.x := v.x * a;
  Result.y := v.y * a;
  Result.z := v.z * a;
  Result.w := v.w * a;
end;


constructor Vec4.Create(x, y, z, w: TVecType);
begin
  self.x := x;
  self.y := y;
  self.z := z;
  self.w := w;
end;

class operator Vec4.Add(const a, b: Vec4): Vec4;
begin
  Result.x := a.x + b.x;
  Result.y := a.y + b.y;
  Result.z := a.z + b.z;
  Result.w := a.w + b.w;
end;

class operator Vec4.Add(a: TVecType; const b: Vec4): Vec4;
begin
  Result.x := a + b.x;
  Result.y := a + b.y;
  Result.z := a + b.z;
  Result.w := a + b.w;
end;

class operator Vec4.Subtract(const a,b: Vec4): Vec4;
begin
  Result.x := a.x - b.x;
  Result.y := a.y - b.y;
  Result.z := a.z - b.z;
  Result.w := a.w - b.w;
end;


constructor Vec4.Create(const x: Vec3; w: TVecType);
begin
  Self.x := x.x;
  Self.y := x.y;
  Self.z := x.z;
  Self.w := w;
end;

constructor Vec4.create(x: TVecType);
begin
  self.x := x;
  self.y := x;
  self.z := x;
  self.w := x;
end;

class operator Vec4.Explicit(const a: Vec4): Int32;
var r_,g_,b_{,w_}:byte;
begin
{
  Result := Color32(
              Math.Min(255,round(a.x)),
              Math.Min(255,round(a.y)),
              Math.Min(255,round(a.z)),
              round(a.m)
  )           }
  {
  Result := Color32(
              EnsureRange(round(a.r*256),0,255),
              EnsureRange(round(a.g*256),0,255),
              EnsureRange(round(a.b*256),0,255),
              EnsureRange(round(a.w*256),0,255)
              );
}
{
  Result := ((EnsureRange(round(a.w*256),0,255)) shl 24)
          or (EnsureRange(round(a.r*256),0,255) shl 16)
          or (EnsureRange(round(a.g*256),0,255) shl  8)
          or EnsureRange(round(a.w*256),0,255);
}

{
  including alpha channel
  if a.r < 0 then R_ := 0 else if a.r > 1 then r_ := 255 else R_ := trunc(a.r*255);
  if a.g < 0 then G_ := 0 else if a.g > 1 then g_ := 255 else G_ := trunc(a.g*255);
  if a.b < 0 then B_ := 0 else if a.b > 1 then b_ := 255 else B_ := trunc(a.b*255);
  if a.w < 0 then W_ := 0 else if a.w > 1 then w_ := 255 else W_ := trunc(a.w*255);
  Result := (W_ shl 24) or (R_ shl 16) or (G_ shl 8) or B_;
}
  if a.r < 0 then R_ := 0 else if a.r > 1 then r_ := 255 else R_ := trunc(a.r*255);
  if a.g < 0 then G_ := 0 else if a.g > 1 then g_ := 255 else G_ := trunc(a.g*255);
  if a.b < 0 then B_ := 0 else if a.b > 1 then b_ := 255 else B_ := trunc(a.b*255);
  Result := (R_ shl 16) or (G_ shl 8) or B_;
end;


function Vec4.getHeight: TVecType;
begin
  result := abs(bottom-top);
end;

function Vec4.getWidth: TVecType;
begin
  result := abs(Right-left);
end;

class operator Vec4.Implicit(const a: Vec3): Vec4;
begin
  Result.x := a.x;
  Result.y := a.y;
  Result.z := a.z;
  Result.w := 0;
end;


class operator Vec4.Multiply(const a: vec3; const b: Vec4): Vec4;
begin
  Result.x := a.x*b.x;
  Result.y := a.y*b.y;
  Result.z := a.z*b.z;
  Result.w := 0;
end;

class operator Vec4.Multiply(const a, b: Vec4): Vec4;
begin
  Result.x := a.x * b.x;
  Result.y := a.y * b.y;
  Result.z := a.z * b.z;
  Result.w := a.w * b.w;
end;

class operator Vec4.Multiply(const a: Vec4; b: TVecType): Vec4;
begin
  Result.x := a.x * b;
  Result.y := a.y * b;
  Result.z := a.z * b;
  Result.w := a.w * b;
end;

class operator Vec4.Multiply(const a: vec4; const b: Vec3): Vec4;
begin
  Result.x := a.x*b.x;
  Result.y := a.y*b.y;
  Result.z := a.z*b.z;
  Result.w := 0;
end;

class operator Vec4.Negative(const a: Vec4): Vec4;
begin
  Result.x := -a.x;
  Result.y := -a.y;
  Result.z := -a.z;
  Result.w := -a.w;
end;


class operator Vec4.Multiply(const a: TVecType; const b: Vec4): Vec4;
begin
  Result.x := a*b.x;
  Result.y := a*b.y;
  Result.z := a*b.z;
  Result.w := a*b.w;
end;




function _Ifthen(c:Boolean;const a,b:Vec2):Vec2;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  if c then
    Result := a
  else
    Result := b
end;

function _Ifthen(c:Boolean;const a,b:Vec3):Vec3;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  if c then
    Result := a
  else
    Result := b
end;

function _Ifthen(c:Boolean;const a,b:Vec4):Vec4;overload;{$IFDEF DO_INLINE} inline;{$ENDIF}
begin
  if c then
    Result := a
  else
    Result := b
end;


class operator Vec4.Add(const a: Vec4; b: TVecType): Vec4;
begin
  Result.x := a.x + b;
  Result.y := a.y + b;
  Result.z := a.z + b;
  Result.w := a.w + b;
end;

constructor Vec4.create(w: TVecType; const ax: Vec3);
begin
  x := w;
  y := ax.x;
  z := ax.y;
  self.w := ax.z;
end;

class operator Vec4.Divide(const a: vec4; b: TVecType): Vec4;
var n:TVecType;
begin
  if b=0 then
    exit(default(vec4));
  n := 1/b;
  Result.x := a.x*n;
  Result.y := a.y*n;
  Result.z := a.z*n;
  Result.w := a.w*n;
end;

class operator Vec4.Divide(const a: vec4; b: int64): Vec4;
var n:TVecType;
begin
  if b=0 then
    exit(default(vec4));
  n := 1/b;
  Result.x := a.x*n;
  Result.y := a.y*n;
  Result.z := a.z*n;
  Result.w := a.w*n;
end;

function Vec4.Dot(const b: Vec4): TVecType;
begin
  Result := x * b.x + y * b.y + z * b.z + w * b.w;
end;

class operator Vec2.explicit(const a: TPoint): Vec2;
begin
  result.x := a.X;
  result.y := a.Y;
end;

class operator Vec2.explicit(const a: TPointF): Vec2;
begin
  result.x := a.X;
  result.y := a.Y;
end;

{ Mat3 }

class operator Mat3.Add(const a: Mat3; const b: Vec3): Vec3;
begin
  Result.x := a.r1.x + b.x;
  Result.y := a.r2.y + b.y;
  Result.z := a.r3.z + b.z;
end;

constructor Mat3.Create(a1, a2, a3, b1, b2, b3, c1, c2, c3: TVecType);
begin
  r1.x := a1;  r1.y := a2;  r1.z := a3;
  r2.x := b1;  r2.y := b2;  r2.z := b3;
  r3.x := c1;  r3.y := c2;  r3.z := c3;
end;

class operator Mat3.Multiply(const a: Vec3; const b: Mat3): Vec3;
begin
//  works, but needs extra function calls
//  Result.x := b.r1.Dot(a);
//  Result.y := b.r2.Dot(a);
//  Result.z := b.r3.Dot(a);


  Result.x := b.r1.x * a.x + b.r1.y * a.y + b.r1.z * a.z;
  Result.y := b.r2.x * a.x + b.r2.y * a.y + b.r2.z * a.z;
  Result.z := b.r3.x * a.x + b.r3.y * a.y + b.r3.z * a.z;

end;

class operator Mat3.Multiply(const a: Mat3; const b: Vec3): Vec3;
begin
//  Result.x := a.r1.Dot(b);
//  Result.y := a.r2.Dot(b);
//  Result.z := a.r3.Dot(b);

  Result.x := a.r1.x * b.x + a.r1.y * b.y + a.r1.z * b.z;
  Result.y := a.r2.x * b.x + a.r2.y * b.y + a.r2.z * b.z;
  Result.z := a.r3.x * b.x + a.r3.y * b.y + a.r3.z * b.z;
end;


constructor Mat3.Create(const a, b, c: Vec3);
begin
  r1 := a;
  r2 := b;
  r3 := c;
end;


function Mat3.getm11: TVecType;
begin
  result := r1.x
end;

function DetInternal(const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
begin
  Result := a1 * (b2 * c3 - b3 * c2) - b1 * (a2 * c3 - a3 * c2) + c1 * (a2 * b3 - a3 * b2);
end;


function Mat3.Determinant: TVecType;
begin
  Result := r1.x * DetInternal(r2.y, r2.y, r3.y, r2.z,  r2.z, r3.z, r2.z, r2.z, r3.z)
            - r1.y * DetInternal(r2.x, r2.x, r3.x, r2.z, r2.z, r3.z, r2.z, r2.z, r3.z)
            + r1.z * DetInternal(r2.x, r2.x, r3.x, r2.y, r2.y, r3.y, r2.z, r2.z, r3.z);
end;

function Mat3.Adjoint: Mat3;
var
  a1, a2, a3,
  b1, b2, b3,
  c1, c2, c3 : TVecType;
begin
  a1 := Self.r1.x;
  a2 := Self.r1.y;
  a3 := Self.r1.z;
  b1 := Self.r2.x;
  b2 := Self.r2.y;
  b3 := Self.r2.z;
  c1 := Self.r3.x;
  c2 := Self.r3.y;
  c3 := Self.r3.z;


  self.r1.x := (b2 * c3 - c2 * b3);
  self.r2.x := -(b1 * c3 - c1 * b3);
  self.r3.x := (b1 * c2 - c1 * b2);

  self.r1.y := -(a2 * c3 - c2 * a3);
  self.r2.y := (a1 * c3 - c1 * a3);
  self.r3.y := -(a1 * c2 - c1 * a2);

  self.r1.z := (a2 * b3 - b2 * a3);
  self.r2.z := -(a1 * b3 - b1 * a3);
  self.r3.z := (a1 * b2 - b1 * a2);
end;

function Mat3.Inverse: Mat3;
var
  Det: TVecType;
begin
  Det := Self.Determinant;
  if Abs(Det) < _EPSILON_ then
    Result := Mat3Identity
  else
    Result := self.Adjoint.Scale(1 / Det);
end;

class operator Mat3.Multiply(const a: Vec2; const b: Mat3): Vec2;
begin
  //gfx one : translation is in 3rd row of the matrix.
  Result.X := a.X * b.r1.x + a.Y * b.r2.x + b.r3.x;
  Result.Y := a.X * b.r1.y + a.Y * b.r2.y + b.r3.y;
end;


class operator Mat3.Multiply(const a, b: Mat3): Mat3;
begin
  Result.r1.x := a.r1.x * b.r1.x + a.r1.y * b.r2.x + a.r1.z * b.r3.x;
  Result.r1.y := a.r1.x * b.r1.y + a.r1.y * b.r2.y + a.r1.z * b.r3.y;
  Result.r1.z := a.r1.x * b.r1.z + a.r1.y * b.r2.z + a.r1.z * b.r3.z;
  Result.r2.x := a.r2.x * b.r1.x + a.r2.y * b.r2.x + a.r2.z * b.r3.x;
  Result.r2.y := a.r2.x * b.r1.y + a.r2.y * b.r2.y + a.r2.z * b.r3.y;
  Result.r2.z := a.r2.x * b.r1.z + a.r2.y * b.r2.z + a.r2.z * b.r3.z;
  Result.r3.x := a.r3.x * b.r1.x + a.r3.y * b.r2.x + a.r3.z * b.r3.x;
  Result.r3.y := a.r3.x * b.r1.y + a.r3.y * b.r2.y + a.r3.z * b.r3.y;
  Result.r3.z := a.r3.x * b.r1.z + a.r3.y * b.r2.z + a.r3.z * b.r3.z;
end;

class operator Mat3.Negative(const a: Mat3): Mat3;
begin
  Result.r1 := -a.r1;
  Result.r2 := -a.r2;
  Result.r3 := -a.r3;
end;

function Mat3.Scale(factor: TVecType): Mat3;
begin
  Result.r1.x := r1.x * factor;
  Result.r1.y := r1.y * factor;
  Result.r1.z := r1.z * factor;
  Result.r2.x := r2.x * factor;
  Result.r2.y := r2.y * factor;
  Result.r2.z := r2.z * factor;
  Result.r3.x := r3.x * factor;
  Result.r3.y := r3.y * factor;
  Result.r3.z := r3.z * factor;
end;

procedure Mat3.setmm11(const Value: TVecType);
begin
  r1.x := value;
end;

{ Mat4 }

constructor Mat4.Create(a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2,
  d3, d4: TVecType);
begin
  r1.x := a1;  r1.y := a2;  r1.z := a3; r1.w := a4;
  r2.x := b1;  r2.y := b2;  r2.z := b3; r2.w := b4;
  r3.x := c1;  r3.y := c2;  r3.z := c3; r3.w := c4;
  r4.x := d1;  r4.y := d2;  r4.z := d3; r4.w := d4;
end;

class operator Mat4.Multiply(const b: Vec4; const a: Mat4): Vec4;
begin
  Result.x := (b.X * a.r1.x) + (b.Y * a.r2.x) + (b.Z * a.r3.x) + (b.W * a.r4.x);
  Result.y := (b.X * a.r1.y) + (b.Y * a.r2.y) + (b.Z * a.r3.y) + (b.W * a.r4.y);
  Result.z := (b.X * a.r1.z) + (b.Y * a.r2.z) + (b.Z * a.r3.z) + (b.W * a.r4.z);
  Result.w := (b.X * a.r1.w) + (b.Y * a.r2.w) + (b.Z * a.r3.w) + (b.W * a.r4.w);
end;


function Mat4.Determinant: TVecType;
begin
  Result := r1.x * DetInternal(r2.y, r2.y, r3.y, r2.z,  r2.z, r3.z, r2.w, r2.w, r3.w)
            - r1.y * DetInternal(r2.x, r2.x, r3.x, r2.z, r2.z, r3.z, r2.w, r2.w, r3.w)
            + r1.z * DetInternal(r2.x, r2.x, r3.x, r2.y, r2.y, r3.y, r2.w, r2.w, r3.w)
            - r1.w * DetInternal(r2.x, r2.x, r3.x, r2.y, r2.y, r3.y, r2.z, r2.z, r3.z);
end;

function Mat4.Adjoint: Mat4;
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1, d2, d3, d4: Single;
begin
  a1 := Self.r1.x;
  b1 := Self.r1.y;
  c1 := Self.r1.z;
  d1 := Self.r1.w;
  a2 := Self.r2.x;
  b2 := Self.r2.y;
  c2 := Self.r2.z;
  d2 := Self.r2.w;
  a3 := Self.r3.x;
  b3 := Self.r3.y;
  c3 := Self.r3.z;
  d3 := Self.r3.w;
  a4 := Self.r4.x;
  b4 := Self.r4.y;
  c4 := Self.r4.z;
  d4 := Self.r4.w;

  Result.r1.x := DetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
  Result.r2.x := -DetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
  Result.r3.x := DetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
  Result.r4.x := -DetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

  Result.r1.y := -DetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
  Result.r2.y := DetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
  Result.r3.y := -DetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
  Result.r4.y := DetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

  Result.r1.z := DetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
  Result.r2.z := -DetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
  Result.r3.z := DetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
  Result.r4.z := -DetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

  Result.r1.w := -DetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
  Result.r2.w := DetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
  Result.r3.w := -DetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
  Result.r4.w := DetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

function Mat4.Inverse: Mat4;
var
  Det: TVecType;
begin
  Det := Self.Determinant;
  if Abs(Det) < _EPSILON_ then
    Result := Mat4Identity
  else
    Result := self.Adjoint.Scale(1 / Det);
end;

class operator Mat4.Multiply(const b: Vec3; const a: Mat4): Vec3;
begin
  Result.x := (b.x * a.r1.x) + (b.Y * a.r2.x) + (b.Z * a.r3.x) + a.r4.x;
  Result.y := (b.x * a.r1.y) + (b.Y * a.r2.y) + (b.Z * a.r3.y) + a.r4.y;
  Result.z := (b.x * a.r1.z) + (b.Y * a.r2.z) + (b.Z * a.r3.z) + a.r4.z;
end;


function Mat4.Scale(factor: TVecType): Mat4;
begin
  Result.r1.x := r1.x * factor;
  Result.r1.y := r1.y * factor;
  Result.r1.z := r1.z * factor;
  Result.r1.w := r1.w * factor;
  Result.r2.x := r2.x * factor;
  Result.r2.y := r2.y * factor;
  Result.r2.z := r2.z * factor;
  Result.r2.w := r2.w * factor;
  Result.r3.x := r3.x * factor;
  Result.r3.y := r3.y * factor;
  Result.r3.z := r3.z * factor;
  Result.r3.w := r3.w * factor;
  Result.r4.x := r4.x * factor;
  Result.r4.y := r4.y * factor;
  Result.r4.z := r4.z * factor;
  Result.r4.w := r4.w * factor;
end;

function _fwidth(const a: Vec2): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  Result := System.abs(a.x)+
            System.abs(a.y);
end;

function _fwidth(const a: Vec3): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  Result := System.abs(a.x)+
            System.abs(a.y)+
            System.abs(a.z);
end;

function _fwidth(const a: Vec4): TVecType;{$IFDEF DO_INLINE} inline;{$ENDIF} overload;
begin
  Result := System.abs(a.x)+
            System.abs(a.y)+
            System.abs(a.z)+
            System.abs(a.w);
end;




class operator Mat4.Multiply(const a, b: Mat4): Mat4;
begin
  Result.r1.x := a.r1.x * b.r1.x + a.r1.y * b.r2.x + a.r1.z * b.r3.x + a.r1.w * b.r4.x;
  Result.r1.y := a.r1.x * b.r1.y + a.r1.y * b.r2.y + a.r1.z * b.r3.y + a.r1.w * b.r4.y;
  Result.r1.z := a.r1.x * b.r1.z + a.r1.y * b.r2.z + a.r1.z * b.r3.z + a.r1.w * b.r4.z;
  Result.r1.w := a.r1.x * b.r1.w + a.r1.y * b.r2.w + a.r1.z * b.r3.w + a.r1.w * b.r4.w;
  Result.r2.x := a.r2.x * b.r1.x + a.r2.y * b.r2.x + a.r2.z * b.r3.x + a.r2.w * b.r4.x;
  Result.r2.y := a.r2.x * b.r1.y + a.r2.y * b.r2.y + a.r2.z * b.r3.y + a.r2.w * b.r4.y;
  Result.r2.z := a.r2.x * b.r1.z + a.r2.y * b.r2.z + a.r2.z * b.r3.z + a.r2.w * b.r4.z;
  Result.r2.w := a.r2.x * b.r1.w + a.r2.y * b.r2.w + a.r2.z * b.r3.w + a.r2.w * b.r4.w;
  Result.r3.x := a.r3.x * b.r1.x + a.r3.y * b.r2.x + a.r3.z * b.r3.x + a.r3.w * b.r4.x;
  Result.r3.y := a.r3.x * b.r1.y + a.r3.y * b.r2.y + a.r3.z * b.r3.y + a.r3.w * b.r4.y;
  Result.r3.z := a.r3.x * b.r1.z + a.r3.y * b.r2.z + a.r3.z * b.r3.z + a.r3.w * b.r4.z;
  Result.r3.w := a.r3.x * b.r1.w + a.r3.y * b.r2.w + a.r3.z * b.r3.w + a.r3.w * b.r4.w;
  Result.r4.x := a.r4.x * b.r1.x + a.r4.y * b.r2.x + a.r4.z * b.r3.x + a.r4.w * b.r4.x;
  Result.r4.y := a.r4.x * b.r1.y + a.r4.y * b.r2.y + a.r4.z * b.r3.y + a.r4.w * b.r4.y;
  Result.r4.z := a.r4.x * b.r1.z + a.r4.y * b.r2.z + a.r4.z * b.r3.z + a.r4.w * b.r4.z;
  Result.r4.w := a.r4.x * b.r1.w + a.r4.y * b.r2.w + a.r4.z * b.r3.w + a.r4.w * b.r4.w;
end;



function _hash(n:Double):Double;overload;
begin
  Result := _fract(_sinLarge(n) * 43758.5453123);
end;

function _hash(const n:vec2):vec2;overload;
begin
  Result := _fract(_sinLarge(n) * 43758.5453123);
end;

function _hash(const n:vec3):vec3;overload;
begin
  Result := _fract(_sinLarge(n) * 43758.5453123);
end;

function _hash(const n:vec4):vec4;overload;
begin
  Result := _fract(_sinLarge(n) * 43758.5453123);
end;

function _lerp(a,b : TVecType; rel : TVecType) : TVecType;
begin
  assert((rel>=0) and (rel<=1));
  result := _min(a,b) + (abs(a-b) * rel);
end;

procedure _vect2sArrayCopy(var source,target : Vec2sArray);
var i,j : integer;
begin
  if length(source)=0 then
    exit;
  setlength(target,length(source));
  for i := 0 to length(source)-1 do
    setlength(target[i],length(source[i]));
  for i := 0 to length(source)-1 do
    for j := 0 to length(source[i])-1 do
      target[i][j] := source[i][j];
end;

end.
