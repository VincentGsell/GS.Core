unit uTransparentPanel;
 
{$mode delphi}{$H+}
 
interface
 
uses
 
  Classes, SysUtils, LMessages, Forms, Controls, Graphics, LCLType, types, Windows;
 
type
 
  { TTransparentPanel }
 
  TTransparentPanel = class(TCustomControl)
 
  private
 
    fBuffer: Graphics.TBitmap;
 
    fBufferChanged : boolean;
 
    procedure SetColor(Value: TColor); override;
 
  protected
 
    function getBuffer : Graphics.TBitmap; virtual;
 
    procedure WMWindowPosChanged(var Message: TLMWindowPosChanged); message LM_WINDOWPOSCHANGED;
 
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
 
    procedure CreateParams(var Params: TCreateParams);
 
    procedure Paint; override;
 
    procedure Resize; override;
 
    procedure redrawBackgroundBuffer(var buffer : Graphics.TBitmap); virtual;
 
    function getBufferChanged : boolean; virtual;
 
    procedure setBufferChanged(val : boolean); virtual;
 
    procedure Invalidate; override;
 
  public
 
    constructor Create(AOwner : TComponent); override;
 
    destructor Destroy; override;
 
  published
 
    property OnPaint;
    property OnMouseMove;
 
    property Color;
 
    property Align;
 
    property Height;
 
    property Cursor;
 
    property HelpContext;
 
    property HelpType;
 
    property Hint;
 
    property Left;
 
    property Name;
 
    property Tag;
 
    property Top;
 
    property Width;
 
    property Anchors;
 
    property Constraints;
 
  end;
 
procedure Register;
 
implementation
 
procedure Register;
 
begin
 
  RegisterComponents('aess',[TTransparentPanel]);
 
end;
 
{ TTransparentPanel }
 
procedure TTransparentPanel.SetColor(Value: TColor);
 
begin
 
  inherited SetColor(Value);
 
  RecreateWnd(Self);
 
end;
 
function TTransparentPanel.getBuffer: Graphics.TBitmap;
 
begin
 
  Result := fBuffer;
 
end;
 
procedure TTransparentPanel.WMWindowPosChanged(var Message: TLMWindowPosChanged);
 
begin
 
  setBufferChanged(true);
 
  Invalidate;
 
  inherited;
 
end;
 
procedure TTransparentPanel.WMEraseBkgnd(var Message: TLMEraseBkgnd);
 
begin
 
  Message.Result := 1;
 
end;
 
procedure TTransparentPanel.CreateParams(var Params: TCreateParams);
 
begin
 
  inherited CreateParams(Params);
 
  params.exstyle := params.exstyle or WS_EX_TRANSPARENT;
 
end;
 
procedure TTransparentPanel.Paint;
 
begin
 
  if getBufferChanged then
 
  begin
 
    redrawBackgroundBuffer(fBuffer);
 
    setBufferChanged(false);
 
  end;
 
  Canvas.Draw(0, 0, fBuffer);
 
  if assigned(OnPaint) then
 
    OnPaint(Self);
 
end;
 
procedure TTransparentPanel.Resize;
 
begin
 
  setBufferChanged(true);
 
  Invalidate;
 
  inherited Resize;
 
end;
 
procedure TTransparentPanel.redrawBackgroundBuffer(var buffer : Graphics.TBitmap);
 
var
 
  rDest : TRect;
 
  bmp : Graphics.TBitmap;
 
begin
 
  bmp := Graphics.TBitmap.Create;
 
  try
 
    bmp.PixelFormat := pf24bit;
 
    bmp.Width := Parent.Width;
 
    bmp.Height := Parent.Height;
 
    bmp.TransparentColor:= Self.Color;
 
    bmp.Canvas.brush.Color:=TCustomForm(parent).Color;
 
    bmp.Canvas.FillRect(types.rect(0,0,bmp.width,bmp.height));
 
    SendMessage(parent.Handle, WM_PAINT, bmp.Canvas.handle, 0);
 
    Application.ProcessMessages;
 
    buffer.Width:= Self.Width;
 
    buffer.Height := Self.Height;
 
    rDest := types.Rect(0,0,Width, Height);
 
    buffer.Canvas.CopyRect(rDest, bmp.Canvas, BoundsRect);
 
  finally
 
    freeandnil(bmp);
 
  end;//fianlly
 
end;
 
function TTransparentPanel.getBufferChanged: boolean;
 
begin
 
  Result := fBufferChanged;
 
end;
 
procedure TTransparentPanel.setBufferChanged(val: boolean);
 
begin
 
  fBufferChanged := val;
 
end;
 
procedure TTransparentPanel.Invalidate;
 
begin
 
  if assigned(parent) and parent.HandleAllocated then
 
  begin
 
    InvalidateRect(parent.Handle, BoundsRect, true);
 
    inherited Invalidate;
 
  end
 
  else
 
    inherited Invalidate;
 
end;
 
constructor TTransparentPanel.Create(AOwner: TComponent);
 
begin
 
  inherited Create(AOwner);
 
  fBuffer := Graphics.TBitmap.Create;
 
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
 
  csDoubleClicks, csReplicatable];
 
  Width := 200;
 
  Height := 150;
 
//  ParentCtl3d := False;
 
//  Ctl3D := False;
 
  ParentColor := False;
 
  fBufferChanged:= false;
 
  inherited Color := clWindow;
 
end;
 
destructor TTransparentPanel.Destroy;
 
begin
 
  fBuffer.Free;
 
  inherited Destroy;
 
end;
 
end.
 
