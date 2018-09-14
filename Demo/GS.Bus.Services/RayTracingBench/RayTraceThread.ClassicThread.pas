unit RayTraceThread.ClassicThread;

interface

uses
  raytrace,
  Classes;

Type

  //Classic TThread implementation.
  TRaytracerThread = class(TThread)
  private
    FID: Integer;
    FX2: Integer;
    FX1: Integer;
    FModel: Scene;
    FBmp: TRawBitmap;
    FRt : RayTracerEngine;
    FBMPSideSize: Integer;

  Public
    Procedure Execute; Override;

    Property Id : Integer read FID Write FID;
    Property X1 : Integer read FX1 Write FX1;
    Property X2 : Integer read FX2 Write FX2;
    Property Model : Scene read FModel Write FModel;
    Property Engine : RayTracerEngine read frt write frt;
    Property Bitmap : TRawBitmap read FBmp Write FBmp;
    Property BmpSideSize : Integer read FBMPSideSize Write FBMPSideSize;
  end;


implementation

{ TRaytracerThread }

procedure TRaytracerThread.Execute;
begin
  FRt.render(FModel,FBMPSideSize,FBMPSideSize,X1,X2,FBmp);
end;

end.
