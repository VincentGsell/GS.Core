unit RayTraceThread.GSThreadsPool;
interface

Uses
  sysutils, classes,
  raytrace,
  System.Threading,
  GS.Threads.pool;

Type

  //GS.Thread.Pool TThread format (TStaskTask) .
  //this designed aim to submit a task (TObject descendant), wich will be submit to a thread.
  //this thread come form a limited pool of resident thread. The thread itself get the task form a
  //stack : you can submit thousands of task, wish will be stacked, and will be processed
  //by the fixed pool of thread.
  //this design is aimed for Server (Connect, and gice the transfert to a stacktask) or
  //huge processing task, such as graphics or calculation tasks.
  TRaytracerStackTask = class(TStackTask)
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
    Constructor Create(aVarValue : Double); Reintroduce;
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



constructor TRaytracerStackTask.Create(aVarValue : Double);
begin
  inherited Create;
  Engine := RayTracerEngine.Create;
  Model := Scene.Create(aVarValue);
  Bitmap := TRawBitmap.Create;
end;

destructor TRaytracerStackTask.Destroy;
begin
  engine.Free;
  model.Free;
  Bitmap.Free;
  inherited;
end;

procedure TRaytracerStackTask.Execute;
begin
  FRt.render(FModel,BmpSideSize,BmpSideSize,X1,X2,FBmp);
end;

end.
