unit GS.Drawing.Shape;
interface
uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Objects,
  FMX.Layers3D,
  FMX.Controls.Presentation, FMX.StdCtrls,
  GS.Geometry.Direction,
  GS.Geometry,
  GS.Geometry.Mesh2D,
  GS.Geometry.Mesh2D.Tools;


Type

  TGS2dShape = Class(TShape)
  public
    vertices : Array of vec2;
    procedure AddVertices(x,y : single);
    procedure BuildVertices; Virtual; Abstract;
  End;

  TQuadEditor = Class(TControl)
  protected
    FLastSize  : TPointf;
    FVec : TDirectionalObject;
    FPA : TSelectionPoint;
    FPB : TSelectionPoint;
    FPC : TSelectionPoint;
    FPD : TSelectionPoint;

    FOwner : TGS2dShape;

    procedure OnInternalTrack(Sender: TObject; var X, Y: Single);
    procedure InternalOnResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure paint; override;

    property SelectionPointA : TSelectionPoint read FPA;
    property SelectionPointB : TSelectionPoint read FPB;
    property SelectionPointC : TSelectionPoint read FPC;
    property SelectionPointD : TSelectionPoint read FPD;
  End;

  TCircleEditor = Class(TControl)
  protected
    FLastSize  : TPointf;
    FPCenter : TSelectionPoint;
    FPRadius : TSelectionPoint;

    FOwner : TGS2dShape;
    procedure OnInternalTrack(Sender: TObject; var X, Y: Single);
    procedure InternalOnResize(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure paint; override;

    property SelectionPointCenter : TSelectionPoint read FPCenter;
    property SelectionPointRadius : TSelectionPoint read FPRadius;
  End;


  TQuadPoly = class(TGS2dShape)
  private
    FEd : TQuadEditor;
    FLastSize  : TPointf;
    pA, pB, pC, pD : TPointf;

    function GetEdEnabled: Boolean;
    procedure SetEdEnabled(const Value: Boolean);

    procedure InternalOnResize(Sender: TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
    procedure Paint; override;
    procedure BuildVertices; Override;

    property EditorEnabled : Boolean read GetEdEnabled write SetEdEnabled;
  end;

  TRegularPoly = class(TGS2dShape)
  Private
    FEd : TCircleEditor;
    FPolType : TGSShape2dType;
    FLastSize  : TPointf;
    pRadius, pCenter : Tpointf;

    function GetEdEnabled: Boolean;
    procedure SetEdEnabled(const Value: Boolean);

    procedure InternalOnResize(Sender: TObject);
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
    procedure Paint; override;
    procedure BuildVertices; Override;

    property EditorEnabled : Boolean read GetEdEnabled write SetEdEnabled;
  end;






implementation

{ TQuadPoly }

procedure TQuadPoly.BuildVertices;
var l : TGSRawMesh2d;
    i : integer;
begin
  vertices := nil;

  AddVertices(pA.X,pA.Y);
  AddVertices(pB.X,pB.Y);
  AddVertices(pC.X,pC.Y);
  AddVertices(pD.X,pD.Y);
end;

constructor TQuadPoly.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren := True;
  FLastSize := PointF(Width,Height);
  OnResize := InternalOnResize;
  pA := PointF(Width*0.3,Height*0.3);
  pB := PointF(Width*0.7,Height*0.3);
  pC := PointF(Width*0.7,Height*0.7);
  pD := PointF(Width*0.3,Height*0.7);
  BuildVertices;
end;

destructor TQuadPoly.Destroy;
begin
  inherited;
end;

function TQuadPoly.GetEdEnabled: Boolean;
begin
  result := Assigned(FEd);
end;

procedure TQuadPoly.InternalOnResize(Sender: TObject);
var fDelta : Tpointf;
    i : integer;
begin
//  if Not Assigned(FEd) then
  begin
    fDelta.X := Width/FLastSize.X;
    fDelta.Y := Height/FLastSize.Y;

    pA.X := pA.X * fDelta.X;
    pA.Y := pA.Y * fDelta.Y;
    pB.X := pB.X * fDelta.X;
    pB.Y := pB.Y * fDelta.Y;
    pC.X := pC.X * fDelta.X;
    pC.Y := pC.Y * fDelta.Y;
    pD.X := pD.X * fDelta.X;
    pD.Y := pD.Y * fDelta.Y;

    for i := 0 to length(vertices)-1 do
    begin
      vertices[i].x := vertices[i].x * fDelta.X;
      vertices[i].y := vertices[i].y * fDelta.Y;
    end;

    Repaint;
  end;

  FLastSize := PointF(Width,Height);
end;


procedure TQuadPoly.Paint;
var i : integer;
    p1, p2 : TPointf;
    p : TPolygon;
begin
  inherited;
  Canvas.Stroke.Thickness := 1;
  Canvas.Stroke.Dash := TStrokeDash.Solid;
  if Assigned(Fed) then
  begin
    pA := Fed.SelectionPointA.Position.Point;
    pB := Fed.SelectionPointB.Position.Point;
    pC := Fed.SelectionPointC.Position.Point;
    pD := Fed.SelectionPointD.Position.Point;
    Canvas.Stroke.Thickness := 2;
    Canvas.Stroke.Dash := TStrokeDash.DashDot;
    BuildVertices
  end;

  if Length(Vertices)>0 then
  begin
    SetLength(p,Length(vertices));
    for i := 0 to length(vertices)-1 do
      p[i] := pointf(vertices[i].x,vertices[i].y);
    Canvas.Stroke.Thickness := 2;
    Canvas.Stroke.Dash := TStrokeDash.Solid;
    Canvas.Stroke.Color := TAlphaColorRec.Black;
    Canvas.Fill.Color := TAlphaColorRec.Antiquewhite;
    Canvas.FillPolygon(p,AbsoluteOpacity);
    Canvas.DrawPolygon(p,AbsoluteOpacity);
  end;
end;

procedure TQuadPoly.SetEdEnabled(const Value: Boolean);
begin
  if Value then
  begin
    FEd := TQuadEditor.Create(Self);
    AddObject(Fed);
    FEd.Align := TAlignLayout.Client;
    FEd.SelectionPointA.Position.Point := pA;
    FEd.SelectionPointB.Position.Point := pB;
    FEd.SelectionPointC.Position.Point := pC;
    FEd.SelectionPointD.Position.Point := pD;
  end
  else
  begin
    BuildVertices;

    RemoveObject(FEd);
    FReeAndNil(Fed);
  end;
end;

{ TQuadEditor }

constructor TQuadEditor.Create(AOwner: TComponent);
var l : TLabel;
begin
  inherited;
  Assert(AOwner Is TGS2dShape);
  FOwner := TGS2dShape(AOwner);
  FVec := TDirectionalObject.Create(0,0,1);
  FPA := TSelectionPoint.Create(Self);
  FPB := TSelectionPoint.Create(Self);
  FPC := TSelectionPoint.Create(Self);
  FPD := TSelectionPoint.Create(Self);

  l := TLabel.Create(Self);
  l.Text := ' A';
  FPA.AddObject(l);
  l := TLabel.Create(Self);
  l.Text := ' B';
  FPB.AddObject(l);
  l := TLabel.Create(Self);
  l.Text := ' C';
  FPC.AddObject(l);
  l := TLabel.Create(Self);
  l.Text := ' D';
  FPD.AddObject(l);


  AddObject(FPA);
  AddObject(FPB);
  AddObject(FPC);
  AddObject(FPD);
  FPA.GripSize := 5;
  FPB.GripSize := 5;
  FPC.GripSize := 5;
  FPD.GripSize := 5;
  FPA.OnTrack := OnInternalTrack;
  FPB.OnTrack := OnInternalTrack;
  FPC.OnTrack := OnInternalTrack;
  FPD.OnTrack := OnInternalTrack;
  HitTest := False;
  ClipChildren := true;
  FPA.ClipChildren := False;
  FPB.ClipChildren := False;
  FPC.ClipChildren := False;
  FPD.ClipChildren := False;

  FPA.Position.Point := PointF(Width * 0.1,Height * 0.1);
  FPB.Position.Point := PointF(Width * 0.9,Height * 0.1);
  FPC.Position.Point := PointF(Width * 0.9,Height * 0.9);
  FPD.Position.Point := PointF(Width * 0.1,Height * 0.9);
  FLastSize := PointF(Width,Height);

  OnResize := InternalOnResize;
end;

destructor TQuadEditor.Destroy;
begin
  FreeAndNil(FPA);
  FreeAndNil(FPB);
  FreeAndNil(FPC);
  FreeAndNil(FVec);
  inherited;
end;

procedure TQuadEditor.InternalOnResize(Sender: TObject);
var fDelta : Tpointf;
    i : integer;
begin
  fDelta.X := Width/FLastSize.X;
  fDelta.Y := Height/FLastSize.Y;

  FPA.Position.Point := Pointf(FPA.Position.Point.X * fDelta.X,FPA.Position.Point.Y * fDelta.Y);
  FPB.Position.Point := Pointf(FPB.Position.Point.X * fDelta.X,FPB.Position.Point.Y * fDelta.Y);
  FPC.Position.Point := Pointf(FPC.Position.Point.X * fDelta.X,FPC.Position.Point.Y * fDelta.Y);
  FPD.Position.Point := Pointf(FPD.Position.Point.X * fDelta.X,FPD.Position.Point.Y * fDelta.Y);

  FLastSize := PointF(Width,Height);
end;

procedure TQuadEditor.OnInternalTrack(Sender: TObject; var X, Y: Single);
begin
  if Assigned(FOwner) then
    FOwner.Repaint;
end;


procedure TQuadEditor.paint;
begin
  Canvas.Stroke.Color := TAlphaColorRec.Blue;
  Canvas.Stroke.Dash := TStrokeDash.Dash;
  Canvas.DrawLine(FPA.Position.Point,FPB.Position.Point,AbsoluteOpacity);
  Canvas.DrawLine(FPB.Position.Point,FPC.Position.Point,AbsoluteOpacity);
  Canvas.DrawLine(FPC.Position.Point,FPD.Position.Point,AbsoluteOpacity);
  Canvas.DrawLine(FPD.Position.Point,FPA.Position.Point,AbsoluteOpacity);
end;

{ TGS2dShape }

procedure TGS2dShape.AddVertices(x, y: single);
var l : integer;
begin
  l := Length(vertices);
  SetLength(vertices,l+1);
  vertices[l].x := x;
  vertices[l].y := y;
end;

{ TCircleEditor }

constructor TCircleEditor.Create(AOwner: TComponent);
var l : TLabel;
begin
  inherited;
  Assert(AOwner Is TGS2dShape);
  FOwner := TGS2dShape(AOwner);
  FPCenter := TSelectionPoint.Create(Self);
  FPRadius := TSelectionPoint.Create(Self);

  l := TLabel.Create(Self);
  l.Text := ' C';
  FPCenter.AddObject(l);
  l := TLabel.Create(Self);
  l.Text := ' R';
  FPRadius.AddObject(l);


  AddObject(FPCenter);
  AddObject(FPRadius);
  FPCenter.GripSize := 5;
  FPRadius.GripSize := 5;
  FPCenter.OnTrack := OnInternalTrack;
  FPRadius.OnTrack := OnInternalTrack;
  HitTest := False;
  ClipChildren := true;
  FPCenter.ClipChildren := False;
  FPRadius.ClipChildren := False;

  FPCenter.Position.Point := PointF(Width * 0.1,Height * 0.1);
  FPRadius.Position.Point := PointF(Width * 0.9,Height * 0.1);
  FLastSize := PointF(Width,Height);

  OnResize := InternalOnResize;
end;

destructor TCircleEditor.Destroy;
begin
  FreeAndNil(FPCenter);
  FreeAndNil(FPRadius);
  inherited;
end;

procedure TCircleEditor.InternalOnResize(Sender: TObject);
var fDelta : Tpointf;
    i : integer;
begin
  fDelta.X := Width/FLastSize.X;
  fDelta.Y := Height/FLastSize.Y;

  FPCenter.Position.Point := Pointf(FPCenter.Position.Point.X * fDelta.X,FPCenter.Position.Point.Y * fDelta.Y);
  FPRadius.Position.Point := Pointf(FPRadius.Position.Point.X * fDelta.X,FPRadius.Position.Point.Y * fDelta.Y);
  FLastSize := PointF(Width,Height);
end;

procedure TCircleEditor.OnInternalTrack(Sender: TObject; var X, Y: Single);
begin
  if Assigned(FOwner) then
    FOwner.Repaint;
end;

procedure TCircleEditor.paint;
var r : TRectf;
    delta : TPointf;
begin
  inherited;
  Canvas.Stroke.Color := TAlphaColorRec.Blue;
  Canvas.Stroke.Dash := TStrokeDash.Dash;

  delta.x := FPCenter.Position.Point.X - FPRadius.Position.Point.X;
  delta.y := FPCenter.Position.Point.Y - FPRadius.Position.Point.Y;

  r.Create(
    FPCenter.Position.Point.x - delta.X,
    FPCenter.Position.Point.Y - delta.X,
    FPCenter.Position.Point.x + delta.X,
    FPCenter.Position.Point.Y + delta.X);

  Canvas.DrawEllipse(r,AbsoluteOpacity);
end;

{ TRegularPoly }

procedure TRegularPoly.BuildVertices;
var l : TGSRawMesh2d;
    i : integer;
begin
  l := TGSRawMesh2d.Create;
  l.SetShapeType(FPolType);
  l.Scale(pRadius.X - pCenter.X,pRadius.X - pCenter.X);
  l.Pan(pCenter.X,pCenter.Y);

  SetLength(vertices,length(l.vertices));
  for i := 0 to length(l.vertices)-1 do
  begin
    vertices[i].x := l.vertices[i].x;
    vertices[i].y := l.vertices[i].y;
  end;
end;

constructor TRegularPoly.Create(AOwner: TComponent);
begin
  inherited;
  ClipChildren := True;
  FLastSize := PointF(Width,Height);
  pRadius.X := Width * 0.8;
  pRadius.Y := Height * 0.8;
  pCenter := PointF(Width/2,Height/2);
  OnResize := InternalOnResize;
  FPolType := TGSShape2dType.hepta;
  BuildVertices;
end;

destructor TRegularPoly.Destroy;
begin

  inherited;
end;

function TRegularPoly.GetEdEnabled: Boolean;
begin
  result := Assigned(FEd);
end;

procedure TRegularPoly.InternalOnResize(Sender: TObject);
var fDelta : Tpointf;
    i : integer;
begin
  fDelta.X := Width/FLastSize.X;
  fDelta.Y := Height/FLastSize.Y;
  for i := 0 to length(vertices)-1 do
  begin
    vertices[i].x := vertices[i].x * fDelta.X;
    vertices[i].y := vertices[i].y * fDelta.Y;
  end;

  Repaint;
  FLastSize := PointF(Width,Height);
end;

procedure TRegularPoly.Paint;
var i : integer;
    p : TPolygon;
begin
  if Assigned(Fed) then
  begin
    pCenter := Fed.SelectionPointCenter.Position.Point;
    pRadius := Fed.SelectionPointRadius.Position.Point;
    BuildVertices;
  end;


  if Length(Vertices)>0 then
  begin
    SetLength(p,Length(vertices));
    for i := 0 to length(vertices)-1 do
      p[i] := pointf(vertices[i].x,vertices[i].y);
    Canvas.Stroke.Thickness := 2;
    Canvas.Stroke.Dash := TStrokeDash.Solid;
    Canvas.Stroke.Color := TAlphaColorRec.Black;
    Canvas.Fill.Color := TAlphaColorRec.Antiquewhite;
    Canvas.FillPolygon(p,AbsoluteOpacity);
    Canvas.DrawPolygon(p,AbsoluteOpacity);
  end;
end;

procedure TRegularPoly.SetEdEnabled(const Value: Boolean);
begin
  if Value then
  begin
    FEd := TCircleEditor.Create(Self);
    AddObject(Fed);
    FEd.Align := TAlignLayout.Client;
    FEd.SelectionPointCenter.Position.Point := pRadius;
    FEd.SelectionPointRadius.Position.Point := pCenter;
  end
  else
  begin
    RemoveObject(FEd);
    FReeAndNil(Fed);
  end;
  BuildVertices;
  Repaint;
end;

end.
