//Warning, this requiert GS.Core, and GS.Bus
//(threading, communication, and optionnaly, network backend processing.)
unit GS.Pixel32.Service.Image32.Backend;


interface

uses
  sysUtils,
  Classes,
  SyncObjs,
  GS.Bus,
  GS.Pixel32.Service.Image32.Types,
  Image32;

Type
  TImage32Service = class(TThread)
  public
    ServiceClient : TBusClientReader;
    Constructor Create; reintroduce;
    Destructor Destroy; override;

    Procedure Execute; override;

    Procedure ClientAskIncoming(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);
  end;

implementation

var service : TImage32Service;

procedure RegisterService;
begin
  service :=TImage32Service.Create;
end;


procedure UnregisterService;
begin
  Service.Terminate;
  service.WaitFor;
  FreeandNil(service);
end;

{ TImage32Service }

procedure TImage32Service.ClientAskIncoming(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
begin
  raise Exception.Create('ohoh');
  //ohoh
end;

constructor TImage32Service.Create;
begin
  inherited create;
  assert(assigned(Bus));
  assert(Bus.Started);
  ServiceClient := Nil;
  ServiceClient := Bus.Subscribe(CST_IMAGE32_URI,ClientAskIncoming);
  ServiceClient.Event := Bus.GetNewEvent;
end;

destructor TImage32Service.Destroy;
begin
  Bus.UnSubscribe(ServiceClient);
  FreeAndNil(ServiceClient.Event);
  FreeAndNil(ServiceClient);
  inherited;
end;

procedure TImage32Service.Execute;
begin
  while not(Terminated) do
  begin
    case ServiceClient.Event.WaitFor(CST_BUSTIMER) of
      TWaitResult.wrSignaled :
      begin
        BusProcessMessages([ServiceClient]);
      end;
    end;
  end;
end;

initialization

RegisterService;

finalization

UnregisterService;

end.
