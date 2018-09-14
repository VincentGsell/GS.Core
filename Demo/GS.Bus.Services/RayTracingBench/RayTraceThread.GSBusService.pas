unit RayTraceThread.GSBusService;

interface

Uses
  SysUtils,
  raytrace,
  GS.Bus.Services;

Type
  //GS.Bus.Services TThread format (TThreadService)
  //GS.Bus.Services is designed to have a pool of thread, as TopTask of GS.Task.
  //As GS.Task, there are a lot of facility to give global order from manager to thread or to thread each other.
  //It's differ from GS.Task in several point :
  // - the manager is a thread itself : It offer *MONITORING* of your thread.
  // - This manager is built upon GS.Bus.TBus : Which is a highly communication skills thread.
  // - It let you launch a service (which is a "customisable" nest for your thread) with :
  //     1 - a simple TThread descendant (xxx = Class(TThread))
  //     2 - a TThreadService descandant (TThreadService remain simple, it is a TThread desandant with iThreadService interface impl.)
  //     3 - It let you send a TThread Descendant with your own iThreadService interface implementation.
  //     Note : In case 1 and 3, stats reporting may be incomplet.
  //            Use case 2, for complete support of stats. (but you can with case 3 reproduce it easely. Watch the source)
  TRaytraceTaskService = Class(TServiceTask)
  private
    FID: Integer;
    FX2: Integer;
    FX1: Integer;
    FModel: Scene;
    FBmp: TRawBitmap;
    FRt : RayTracerEngine;
    FBMPSideSize: Integer;

  Public
    Constructor Create(aVarValue : Double); Reintroduce;
    Destructor Destroy; Override;
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

{ TRaytraceTaskService }

constructor TRaytraceTaskService.Create(aVarValue: Double);
begin
  inherited Create;
  Engine := RayTracerEngine.Create;
  Model := Scene.Create(aVarValue);
  Bitmap := TRawBitmap.Create;
end;

destructor TRaytraceTaskService.Destroy;
begin
  FreeAndNil(FBmp);
  FreeAndNil(Frt);
  FreeAndNil(FModel);
  inherited;
end;

Procedure TRaytraceTaskService.Execute;
begin
  FRt.Render(FModel,FBMPSideSize,FBMPSideSize,X1,X2,FBmp);
end;

end.
