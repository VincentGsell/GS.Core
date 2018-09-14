unit fmain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, GS.Bus;

type

  { TForm1 }

  TForm1 = class(TForm)
    btn1: TButton;
    btn2: TButton;
    btn3: TButton;
    btn4: TButton;
    edt1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbl2: TLabel;
    lblChannels: TLabel;
    ListView1: TListView;
    ListView2: TListView;
    lst2: TListBox;
    Panel1: TPanel;
    pnl2: TPanel;
    TimerBusQuery: TTimer;
    TimerGui: TTimer;
    procedure btn1Click(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btn3Click(Sender: TObject);
    procedure btn4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerBusQueryTimer(Sender: TObject);
    procedure TimerGuiTimer(Sender: TObject);
  private
  public
    procedure OnReceived(Sender : TObject;Var Packet : TBusEnvelop);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.OnReceived(Sender: TObject; Var Packet: TBusEnvelop);
begin
  label1.Caption := IntToStr(StrToIntDef(label1.Caption,0)+1);
end;

procedure TForm1.btn2Click(Sender: TObject);
var la : TBusClientReader;
begin
  la := Bus.Subscribe(edt1.Text,OnReceived);
  lst2.AddItem('Reader on '+la.ChannelListening,la);
  //"la" is only given by bus. App owned this instance : manage it.
end;

procedure TForm1.btn3Click(Sender: TObject);
var lb : TBusMessage;
    i : integer;
begin
  for I := 0 to TButton(Sender).Tag-1 do
  begin
    lb.FromString('['+IntToStr(i)+'] This a test at '+DateToStr(Now));
    Bus.Send(lb, edt1.Text);
  end;
end;

procedure TForm1.btn1Click(Sender: TObject);
var lb : TBusMessage;
begin
  lb.FromString('This a test at '+DateToStr(Now));
  Bus.Send(lb, edt1.Text);
end;

procedure TForm1.btn4Click(Sender: TObject);
var la : TBusClientReader;
begin
  ListView1.Clear; //Will be rebuild by timer.
  if lst2.ItemIndex>-1 then
  begin
     la := TBusClientReader(lst2.Items.Objects[lst2.ItemIndex]);
     if Bus.UnSubscribe(la) then
     begin
       la.Free;
       lst2.DeleteSelected;
     end;
  end
  else
  begin
    ShowMessage('Select a subscribter first.');
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  StartStandartBus;
  ListView1.DoubleBuffered:= true;
  ListView2.DoubleBuffered:= true;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var i : integer;
    lb : TBusClientReader;
begin
  for i := 0 to lst2.Count-1 do
  begin
    lb := TBusClientReader(lst2.Items.Objects[i]);
    try
      Bus.UnSubscribe(lb);
    finally
      freeAndNil(lb);
    end;
  end;
  ReleaseStandartBus;
end;


procedure TForm1.TimerBusQueryTimer(Sender: TObject);
var lb : array of TBusClientReader;
    i : integer;
begin
  //Build array for submiting to Bus Query...
  SetLength(lb,lst2.Count);
  for i := 0 to lst2.Count-1 do
  begin
    lb[i] := TBusClientReader(lst2.Items.Objects[i]);
  end;
  //ask bus if we get message.
  bus.ProcessMessages(lb);
end;

procedure TForm1.TimerGuiTimer(Sender: TObject);
var astrSub, asub, astrchan, achan : TStringList;
    lv : TListItem;

    Procedure GUIUpdateSubList;
    var i : Integer;
    begin
      ListView1.Items.BeginUpdate;
      try
        astrSub.Delete(0); //header.
        if astrSub.Count=0 then
          Exit;

        for i := 0 to astrSub.Count-1 do
        begin
          asub.DelimitedText := astrSub[i];
          if ListView1.Items.Count<=i then //Create new line if needed.
          begin
            lv := ListView1.Items.Add;
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
          end;

          ListView1.Items[i].caption := asub[0];
          ListView1.Items[i].SubItems[0] := asub[1];
          ListView1.Items[i].SubItems[1] := asub[2];
          ListView1.Items[i].SubItems[2] := asub[3];
        end;
      finally
        ListView1.Items.EndUpdate;
      end;
    end;

    Procedure GUIUpdateChanList;
    var i : Integer;
    begin
      ListView2.Items.BeginUpdate;
      try
       astrchan.Delete(0); //Header.
        if astrchan.Count=0 then
          Exit;

        for i := 0 to astrchan.Count-1 do
        begin
          achan.DelimitedText := astrchan[i];
          if ListView2.Items.Count<=i then //Create new line if needed.
          begin
            lv := ListView2.Items.Add;
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
            lv.SubItems.Add(EmptyStr);
          end;

          ListView2.Items[i].caption := achan[0];
          ListView2.Items[i].SubItems[0] := achan[1];
          ListView2.Items[i].SubItems[1] := achan[2];
          ListView2.Items[i].SubItems[2] := achan[3];
          ListView2.Items[i].SubItems[3] := achan[4];
          ListView2.Items[i].SubItems[4] := achan[5];
          ListView2.Items[i].SubItems[5] := achan[6];
        end;
      finally
        ListView2.Items.EndUpdate;
      end;
    end;

begin
  astrSub := TStringList.Create;
  astrChan := TStringList.Create;
  aSub := TStringList.Create;
  aChan := TStringList.Create;
  try
    Bus.GetSubscribtersConfigurationAsCSV(astrSub);
    Bus.GetChannelsConfigurationAsCSV(astrChan);

    //Update list view's GUI.
    GUIUpdateSubList;
    GUIUpdateChanList;

    Label4.Caption := Bus.Stats;

  finally
    FreeAndNil(astrsub);
    FreeAndNil(astrchan);
    FreeAndNil(asub);
    FreeAndNil(achan);
  end;
end;

end.

