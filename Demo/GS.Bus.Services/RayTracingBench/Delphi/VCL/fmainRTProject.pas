unit fmainRTProject;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  System.Diagnostics,
  raytrace,
  System.Threading,
  GS.Task,
  GS.Bus.Services,
  GS.Threads.Pool;



Const
  BMPSIZE = 600; //1200; //400; //2400;
  GTHREADCOUNT = 8;  //Thread count and BMPSIZE should be multiple of 2 :  For rebuliding final image without mantissa issue.

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    Button2: TButton;
    Memo1: TMemo;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    gsw  : TStopwatch;
    { Public declarations }
    Procedure ConvertRawToBitmapAndDrawIt(Index : Integer; Raw : TRawBitmap);

    //GS.Task progress event.
    Procedure OnProgress(Sender : TObject; Task : TopTask; aProgressString : String);

    //GS.Threads.Pool event.
    Procedure OnTaskFinished(Const aThreadIndex : UInt32; aStackTask : TStackTask; TaskProcessTimeValue : UInt64);
  end;

var
  Form1: TForm1;
  aThreadPool : TStackThreadPool;


implementation

Uses
  RayTraceThread.ClassicThread,
  RayTraceThread.GSTask,
  RayTraceThread.GSBusService,
  RayTraceThread.GSThreadsPool;

{$R *.dfm}


procedure TForm1.ConvertRawToBitmapAndDrawIt(Index: Integer; Raw: TRawBitmap);
var  lbmp : TBitmap;
     p     :  ^Integer;
     coffset :   Integer;
     c : Integer;
begin
  c := Round(BMPSIZE / GTHREADCOUNT);
  coffset := ((Index-1) * (c));

  lbmp := TBitmap.Create;
  lbmp.SetSize(Raw.Width,Raw.Height);
  lbmp.PixelFormat := pf32bit;

  p := lbmp.Scanline[lbmp.Height-1];  // starting address of "last" row
  Move(Raw.Buffer[0], p^, lbmp.Width*lbmp.Height*SizeOf(Integer));

  if (Form1.Image1.Picture.Bitmap.Width<>BMPSIZE) Or
     (Form1.Image1.Picture.Bitmap.Height<>BMPSIZE) then
  begin
    Form1.Image1.Picture.Bitmap.SetSize(BMPSIZE,BMPSIZE);
  end;
  Form1.Image1.Picture.Bitmap.Canvas.CopyRect(Rect(coffset,0,coffset+c,BMPSIZE),lbmp.Canvas,Rect(0,0,lbmp.Width,lbmp.Height));
  lbmp.Free;
end;



procedure TForm1.Button1Click(Sender: TObject);
var
  sc  : Scene;
  rt  : RayTracerEngine;
  s   : String;

  bmp : TBitmap;
  RawBmp  : TRawBitmap;
  p     :  ^Integer;
begin
  Button1.Enabled := false;
  memo1.Clear;

  sc := Scene.Create(Sin(Random(360)));
  rt := RayTracerEngine.Create();

  RawBmp := TRawBitmap.Create;
  RawBmp.SetSize(BMPSIZE,BMPSIZE);

  gsw.Reset;
  gsw.Start;
  rt.render(sc,BMPSIZE,BMPSIZE,0,BMPSIZE,RawBmp);
  gsw.Stop;

  bmp := TBitmap.Create;
  bmp.SetSize(BMPSIZE,BMPSIZE);
  bmp.PixelFormat := pf32bit;

  p := bmp.Scanline[bmp.Height-1];  // starting address of "last" row
  Move(RawBmp.Buffer[0], p^, bmp.Width*bmp.Height*SizeOf(Integer));
  Image1.Picture.Graphic := bmp;

  sc.Free;
  rt.Free;
  bmp.Free;
  RawBmp.Free;

  Memo1.Lines.Add('[NO THREAD] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  Button1.Enabled := true;
end;

procedure TForm1.Button2Click(Sender: TObject);


var aT : TTaskManager;
    m : TRaytracerTask;
    i : integer;

    report : TStringList;
    l : TopTaskLog;
    lv : Double;
begin
  Button2.Enabled := false;
  memo1.Clear;

  aT := TTaskManager.Create;
  aT.OnTaskProgress := OnProgress;
  at.ClearTasks;

  lv := Sin(Random(360));
  for i := 0 to GTHREADCOUNT-1 do
  begin
    m := TRaytracerTask(at.CreateTask(TRaytracerTask,False,False));
    m.Id := i+1;
    m.X1 := (i) * (BMPSIZE div GTHREADCOUNT);
    m.X2 := (i+1) * (BMPSIZE div GTHREADCOUNT);
    m.Engine := RayTracerEngine.Create;
    m.Model := Scene.Create(lv);
    m.Bitmap := TRawBitmap.Create;
    m.BmpSideSize := BMPSIZE;
    m.Bitmap.SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
  end;

  gsw.Reset;
  gsw.Start;
  at.StartAll;
  at.WaitForAll;
  gsw.Stop;
  at.Log().Free;

  //Just display stat in memo. Not to include in time's measuring.
  Report := TStringList(Memo1.Lines);
  at.TaskReport(Report);
  Memo1.Lines.Add('[GS.TASK] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  freeAndNil(at);
  Button2.Enabled := true;
end;

procedure TForm1.Button3Click(Sender: TObject);
var i : Integer;
    b : array of TRawBitmap;
    lv : Double;
begin
  Button3.Enabled := false;
  memo1.Clear;

  SetLength(b,GTHREADCOUNT);
  for I := 0 to GTHREADCOUNT-1 do
  begin
    b[i] := TRawBitmap.Create;
    b[i].SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
  end;

  lv := Sin(Random(360));
  gsw.reset;
  gsw.Start;
  TParallel.&For(0, GTHREADCOUNT-1,

    Procedure(AIndex : Integer)
    var a : Double;
        r : RayTracerEngine;
        m : Scene;
    begin
      m := Scene.Create(lv);
      r := RayTracerEngine.Create;
      try
        r.render( m,
                BMPSIZE,
                BMPSIZE,
                (AIndex) * BMPSIZE div GTHREADCOUNT,
                (AIndex+1) * BMPSIZE div GTHREADCOUNT,
                b[AIndex]);
      finally
        FreeAnDNil(r);
        FreeAndNil(m);
      end;
    end

    );
  gsw.Stop;

  for I := 0 to GTHREADCOUNT-1 do
  begin
    ConvertRawToBitmapAndDrawIt(i+1,b[i]);
    b[i].Free;
  end;


  Memo1.Lines.Add('[Delphi ITASK] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  Button3.Enabled := true;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
  gsw := TStopWatch.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(aThreadPool) then
    FreeAndNil(aThreadPool);
end;

procedure TForm1.OnProgress(Sender : TObject; Task : TopTask; aProgressString : String);
begin
//  Memo1.Lines.Add( IntTostr(TRaytracerTask(Task).Id)+ ' '+ aProgressString);
  ConvertRawToBitmapAndDrawIt(TRaytracerTask(Task).Id,TRaytracerTask(Task).Bitmap);
  TRaytracerTask(Task).Bitmap.Free;
  Application.ProcessMessages;
end;


procedure TForm1.OnTaskFinished(const aThreadIndex: UInt32;
  aStackTask: TStackTask; TaskProcessTimeValue: UInt64);
var l : TRaytracerStackTask;
begin
  l := TRaytracerStackTask(aStackTask);
  Memo1.lines.add(IntTosTr(aThreadIndex)+' '+IntToStr(l.ID)+' '+l.ClassName+' '+IntToStr(TaskProcessTimeValue div 1000000)+'ms');

  ConvertRawToBitmapAndDrawIt( l.ID,
                               l.Bitmap);
end;

{ TRaytracerThread }



procedure TForm1.Button4Click(Sender: TObject);
var i : integer;

    lt : Array of TraytracerThread;
    lv : Double;
begin
  Button4.Enabled := false;
  memo1.Clear;

  lv := (Sin(Random(360)));
  SetLength(lt,GTHREADCOUNT-1);
  for i := 0 to GTHREADCOUNT-1 do
  begin
    lt[i] := TRaytracerthread.Create(true);
    lt[i].Id := i+1;
    lt[i].X1 := (i) * (BMPSIZE div GTHREADCOUNT);
    lt[i].X2 := (i+1) * (BMPSIZE div GTHREADCOUNT);
    lt[i].Engine := RayTracerEngine.Create;
    lt[i].Model := Scene.Create(lv);
    lt[i].Bitmap := TRawBitmap.Create;
    lt[i].Bitmap.SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
    lt[i].BmpSideSize := BMPSIZE;
  end;

  gsw.Reset;
  gsw.Start;
  for i := 0 to GTHREADCOUNT-1 do
  begin
    lt[i].Start;
  end;
  for i := 0 to GTHREADCOUNT-1 do
  begin
    lt[i].WaitFor;
  end;
  gsw.Stop;


  for i := 0  to GTHREADCOUNT-1 do
  begin
    ConvertRawToBitmapAndDrawIt(lt[i].id,lt[i].Bitmap);
    lt[i].Bitmap.Free;
    lt[i].Model.Free;
    lt[i].Engine.Free;
    lt[i].Free;
  end;

  Memo1.Lines.Add('[Classic TThread] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');

  Button4.Enabled := true;
end;


procedure TForm1.Button5Click(Sender: TObject);
var lb : TCustomServiceManager;
    ls :  TCustomService;
    i : Integer;
    lt : TRaytraceTaskService;
    lta : TRaytracerThread;

    lStats : TThreadServiceStat;
    lv : Double;
begin
  Button5.Enabled := false;
  memo1.Clear;

  lb := TCustomServiceManager.Create;
  lb.Start;

  lv := (Sin(Random(360)));
  for I := 0 to GTHREADCOUNT-1 do
  begin
    ls := TcustomService.Create;
    lb.RegisterService(ls);

    lt := TRaytraceTaskService.Create(lv);
    lt.Id := i+1;
    lt.X1 := (i) * (BMPSIZE div GTHREADCOUNT);
    lt.X2 := (i+1) * (BMPSIZE div GTHREADCOUNT);
    lt.Bitmap.SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
    lt.BmpSideSize := BMPSIZE;
    ls.Task := lt;
  end;

  gsw.Reset;
  gsw.Start;
  for i := 0  to GTHREADCOUNT-1 do
  begin
    lb.Services[i].StartService;
  end;
  for i := 0  to GTHREADCOUNT-1 do
  begin
    lb.Services[i].WaitFor;
  end;
  gsw.Stop;

  for i := 0  to GTHREADCOUNT-1 do
  begin
    Memo1.Lines.add(lb.Services[i].ServiceStats.AsString);
    ConvertRawToBitmapAndDrawIt( TRaytraceTaskService(lb.Services[i].Task).id,
                                 TRaytraceTaskService(lb.Services[i].Task).Bitmap);
    lb.Services[i].Task.Free;
  end;

  FreeAndNil(lb);

  Memo1.Lines.Add('[GS.Bus.Services] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');

  Button5.Enabled := true;
end;


procedure TForm1.Button6Click(Sender: TObject);
var ltak : Array of TRaytracerStackTask;
    i : integer;
    lv : Double;
begin
  if not Assigned(AthreadPool) then
  begin
    AThreadPool := TStackThreadPool.Create(GTHREADCOUNT);
    AthreadPool.OnTaskFinished := OnTaskFinished;
  end;

  Button6.Enabled := false;
  memo1.Clear;

  lv := (Sin(Random(360)));
  SetLength(ltak,GTHREADCOUNT);
  for I := 0 to GTHREADCOUNT-1 do
  begin
    ltak[i] := TRaytracerStackTask.Create(lv);
    ltak[i].Id := i+1;
    ltak[i].X1 := (i) * (BMPSIZE div GTHREADCOUNT);
    ltak[i].X2 := (i+1) * (BMPSIZE div GTHREADCOUNT);
    ltak[i].Bitmap.SetSize(BMPSIZE div GTHREADCOUNT,BMPSIZE);
    ltak[i].BmpSideSize := BMPSIZE;
  end;


  gsw.Reset;
  gsw.Start;
  for i := 0  to GTHREADCOUNT-1 do
  begin
    aThreadPool.Submit(ltak[i]); //Start immediatly, the stack is consumed by a pool of GTRHREADCOUNT threads.
  end;

  while not(aThreadPool.Idle) do
  begin
    application.ProcessMessages; //By default, Threapool is configured as "synchrone". Wait sequence need the call of app.processmessages.
  end;
  gsw.Stop;

  Memo1.Lines.Add('[GS.Threads.Pool] Completed in: ' + IntToStr(gsw.ElapsedMilliseconds) + ' ms');
  Button6.Enabled := true;
end;

end.
