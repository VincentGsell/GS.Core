unit TaskMessage.fmain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  GS.Task, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ListView1: TListView;
    Timer1: TTimer;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
  public
    aTaskManager :  TTaskManager;

    Procedure MyTaskProgressing(Sender : TObject; Task : TopTask; aProgressString : String);
    Procedure MyTaskFinished(Sender : TObject; Task : TopTask);

  end;


  //----------------------------------------------
  TTaskMessageDemo = Class(TopTask)
  private
    i : Int64;

  Public
    LastMessage : TopTaskMessage;

    Procedure BeforeLoop;
    Procedure RunLoop; Override;

    Procedure OnMessageDelivered(var aNewMessaqe: TopTaskMessage); Override;
  End;

  TTaskMessageDemoWithMessageWait = Class(TopTask)
  Private
  Public
    Procedure RunLoop; Override;

    Procedure OnMessageDelivered(var aNewMessaqe: TopTaskMessage); Override;

  End;


var
  Form3: TForm3;

implementation

{$R *.dfm}

procedure TForm3.FormCreate(Sender: TObject);
var t : TopTask;
    b : TListItem;
begin
  aTaskManager :=  TTaskManager.Create;
  aTaskManager.OnTaskProgress := MyTaskProgressing;
  aTaskManager.OnTaskFinish := MyTaskFinished;

  //Start the task manager.
  t := aTaskManager.CreateTask(TTaskMessageDemoWithMessageWait);
  b := ListView1.Items.Add;
  b.Data := t;
  t.Start;
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  FreeAndNil(aTaskManager);
end;

procedure TForm3.ListView1Click(Sender: TObject);
begin
  if Assigned(ListView1.ItemFocused) then
  begin
    Button3.Enabled := TObject(ListView1.ItemFocused.Data) is TTaskMessageDemoWithMessageWait;
  end;
end;

procedure TForm3.MyTaskFinished(Sender: TObject; Task: TopTask);
begin
  MyTaskProgressing(Sender,Task,EmptyStr);
end;

procedure TForm3.MyTaskProgressing(Sender : TObject; Task : TopTask; aProgressString : String);
var i,j : Integer;
begin
  for i := 0 to ListView1.Items.Count-1 do
  begin
    if ListView1.Items[i].Data = Task then
    begin
      if ListView1.Items[i].SubItems.Count=0 then
      begin
        for j := 0 to 4 do ListView1.Items[i].SubItems.Add(EmptyStr);
      end;
      ListView1.Items[i].Caption := '['+Task.TaskStatusAsString+'] ';
      ListView1.Items[i].SubItems[0] := Task.ClassName;
      ListView1.Items[i].SubItems[1] := Task.LastProgressString;
      ListView1.Items[i].SubItems[2] := IntToStr(Task.TotalMessageInQueue);
      ListView1.Items[i].SubItems[3] := IntToStr(Task.TotalMessageDelivered);
      break;
    end;
  end;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  aTaskManager.Log(True).Free;
end;

procedure TForm3.Button1Click(Sender: TObject);
var aM : TopTaskMessage;
    i : integer;
begin
  for i := 0 to 999 do
  begin
    aTaskManager.SendMessage(aM);
  end;
end;

procedure TForm3.Button2Click(Sender: TObject);
var t : TopTask;
    b : TListItem;
begin
  t := aTaskManager.CreateTask(TTaskMessageDemoWithMessageWait);
  b := ListView1.Items.Add;
  b.Data := t;
  t.Start;
end;



procedure TForm3.Button3Click(Sender: TObject);
var task : TTaskMessageDemoWithMessageWait;
    a : TopTaskMessage;
    i : integer;
begin
  if Assigned(ListView1.ItemFocused) then
  begin
    if TObject(ListView1.ItemFocused.Data) is TTaskMessageDemoWithMessageWait then
    begin
      task := TTaskMessageDemoWithMessageWait(ListView1.ItemFocused.Data);
      for I := 0 to 499 do
        Task.DeliverMessage(a);
    end;
  end;
end;

procedure TForm3.Button4Click(Sender: TObject);
var t : TopTask;
    b : TListItem;
begin
  t := aTaskManager.CreateTask(TTaskMessageDemo);
  b := ListView1.Items.Add;
  b.Data := t;
  t.Start;
end;

procedure TForm3.Button5Click(Sender: TObject);
begin
  if Assigned(ListView1.ItemFocused) then
  begin
    if TObject(ListView1.ItemFocused.Data) is TopTask then
    begin
      aTaskManager.KillTask(ListView1.ItemIndex);
    end;
  end;
end;

procedure TForm3.Button6Click(Sender: TObject);
begin
  if Assigned(ListView1.ItemFocused) then
  begin
    if TObject(ListView1.ItemFocused.Data) is TopTask then
    begin
      if aTaskManager.KillAndRemoveTask(ListView1.ItemIndex) then
      begin
        ListView1.DeleteSelected;
      end;
    end;
  end;
end;

{ TTaskMessageDemo }

procedure TTaskMessageDemo.BeforeLoop;
begin
  i := 0;
end;

procedure TTaskMessageDemo.OnMessageDelivered(var aNewMessaqe: TopTaskMessage);
begin
  LastMessage := aNewMessaqe; //Deep copy message (aNewMessage will be disposed after this call.
end;

procedure TTaskMessageDemo.RunLoop;
var MyProgressString : String;
begin
  MyProgressString := 'Hello '+IntToStr(LoopCount);
  //Make a Message check every 1000 loops.
  if LoopCount Mod 1000 = 0 then
    DoProcessMessage; //This enable automaticaly the thread to receive message.
                      //And will process one (1) message per call.
                      //If you want process more message per loop, make a loop of DoProcessMessage.

  //Make a report every 10000 loops.
  if LoopCount Mod 10000 = 0 then
  begin
    DoTaskProgress(MyProgressString); //trig OnProgress at TaskManager side.
  end;

  //After 4000000 of loops, we check if there are no message in queue, and stop work.
  if LoopCount>4000000 then
  begin
    if TotalMessageInQueue=0 then
    begin
//      DoTaskProgress(MyProgressString); //Make sure to report a correct display
      Terminate;
    end;
  end
  else
  begin
    inc(i);
  end;

end;

{ TTaskMessageDemoWithMessageWait }

procedure TTaskMessageDemoWithMessageWait.OnMessageDelivered(
  var aNewMessaqe: TopTaskMessage);
begin
  //inherited;
  //Heritate this method is mandatory : Nothing to do here, it's a demo.
end;

procedure TTaskMessageDemoWithMessageWait.RunLoop;
var MyProgressString : String;
begin
  MyProgressString := 'Hello '+IntToStr(LoopCount);
  DoTaskProgress(MyProgressString);
  DoProcessMessage(true); //This will wait until next message. (Thread CPU during wait = 0.00(x)1 !
end;

end.
