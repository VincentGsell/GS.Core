unit dmData;

interface

uses
  System.SysUtils, System.Classes,
  GS.MVVM, Vcl.ExtCtrls;
{$M+}
type
  TDataModule1 = class(TDataModule)
    Timer1: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FInputString: String;
    FModelTime: String;
    procedure SetInputString(const Value: String);
    procedure SetModelTime(const Value: String);
    { Private declarations }
  public
    MyLocalMVVMComponent : TGSMVVM;
    { Public declarations }
  Published
    Property InputString : String read FInputString Write SetInputString;
    Property ModelTime : String read FModelTime Write SetModelTime;
  end;

var
  DataModule1: TDataModule1;

implementation


{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
  MyLocalMVVMComponent := TGSMVVM.Create;
  //Here, We declare simply the DM property as a published ressource of the MVVM.
  //Note that the property behind the ressource have a setter, where precessing is done.
  MyLocalMVVMComponent.Declare('MyMainDataModule',Self,'InputString','InputEngineString');
  MyLocalMVVMComponent.Declare('MyMainDataModule',Self,'ModelTime','MyModelTime');
end;

procedure TDataModule1.DataModuleDestroy(Sender: TObject);
begin
  MyLocalMVVMComponent.Free;
end;

procedure TDataModule1.SetInputString(const Value: String);
begin
  FInputString := 'Hello World '+UpperCase(Value)+' !!!';
  MyLocalMVVMComponent.Activity(Self);
end;

procedure TDataModule1.SetModelTime(const Value: String);
begin
  FModelTime := Value;
  MyLocalMVVMComponent.Activity(Self);
end;

procedure TDataModule1.Timer1Timer(Sender: TObject);
begin
  ModelTime := DateTimeToStr(Now);
end;

end.
