unit RayTraceThread.GSTask;

interface

Uses
  raytrace,
  SysUtils,
  System.Threading,
  GS.Task;

Type

  //GS.Task TThread format (TopTask) (a TopTask is a TThread).
  //TopTask is designed to have a pool of resident thread. It as facility to
  //give global order from manager to thread or to thread each other.
  //It give you precise data of usage and process time.
  TRaytracerTask = class(topTask)
  private
    FID: Integer;
    FX2: Integer;
    FX1: Integer;
    FModel: Scene;
    FBmp: TRawBitmap;
    FRt : RayTracerEngine;
    FBMPSideSize: Integer;

  Public
    Procedure RunLoop; Override;
    Destructor Destroy; Override;

    Property Id : Integer read FID Write FID;
    Property X1 : Integer read FX1 Write FX1;
    Property X2 : Integer read FX2 Write FX2;
    Property Model : Scene read FModel Write FModel;
    Property Engine : RayTracerEngine read frt write frt;
    Property Bitmap : TRawBitmap read FBmp Write FBmp;
    Property BmpSideSize : Integer read FBMPSideSize Write FBMPSideSize;
  end;




implementation

{ TRaytracerTask }


destructor TRaytracerTask.Destroy;
begin
  FreeAndNil(FModel);
  FreeAndNil(frt);
  inherited;
end;

procedure TRaytracerTask.RunLoop;
begin
  FRt.render(FModel,BmpSideSize,BmpSideSize,X1,X2,FBmp);
  DoTaskProgress('Render finish'); //Will draw stuff in main thread.
  //  Synchronize(InternalSynchro); No need to synchronize, after run, we will call "Log" to flush result.
  Terminate;
end;

end.
