//Bus based service access.
// Allow to have full multithreading capabilities (throught bus)
// Allow easy move to a full C/S (GRID or other) system.
unit GS.Pixel.Service;

interface

uses classes, sysutils, GS.Pixel, GS.Bus, GS.Stream;

Type

TPixelCustomService = class(TPixel32InterfacedObject,iPixService)
protected
  furi : string;
  fcli : TBusClientReader;
  fcontent, freport : TMemoryStream;

  //Bus related.
  Procedure IncomingMessage(Sender : TBusSystem; aReader : TBusClientReader; Var Packet : TBusEnvelop);

public
  Constructor Create(_uri : string); reintroduce;
  Destructor Destroy; override;

  //iPixSurface
  function uri : string; virtual;
  function id : string; virtual; abstract;
  procedure Ask(param : TStream); virtual;
  procedure Answer(content : TStream; success : boolean; report : TStream); virtual; abstract;
end;


implementation

{ TPixel32CustomPXLService }


procedure TPixelCustomService.Ask(param: TStream);
var mes : TBusMessage;
begin
  assert(assigned(param));
  param.Position := 0;
  mes.FromStream(param);
  Bus.Send(mes,furi,'','',False,id,'');
end;

constructor TPixelCustomService.Create(_uri: string);
begin
  assert(length(trim(_uri))>0);
  inherited create;
  furi := trim(lowercase(_uri));
  fcli := Bus.Subscribe(_Uri,IncomingMessage);
  fcontent := TMemoryStream.Create;
  freport := TMemoryStream.Create;
end;

destructor TPixelCustomService.Destroy;
begin
  Bus.UnSubscribe(fCli);
  FreeAndNil(fcli);
  FreeAndNil(freport);
  FreeAndNil(fcontent);
  inherited;
end;

procedure TPixelCustomService.IncomingMessage(Sender: TBusSystem;
  aReader: TBusClientReader; var Packet: TBusEnvelop);
var l : TMemoryStream;
    r : Boolean;
begin
  if Packet.ClientSourceId = id then
  begin
    l := Packet.ContentMessage.AsStream;
    try
      fcontent.Clear;
      freport.Clear;
      ReadStream(l,TStream(fcontent));
      r := ReadBoolean(l);
      ReadStream(l,TStream(freport));
      Answer(fcontent,r,freport);
    finally
      FreeAndNil(l);
    end;
  end;
end;

function TPixelCustomService.uri: string;
begin
  result := furi;
end;

initialization
  StartStandartBus;

finalization
  ReleaseStandartBus;

end.
