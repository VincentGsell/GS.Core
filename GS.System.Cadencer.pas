unit GS.System.Cadencer;

interface

Uses Classes,
     Sysutils,
     GS.System.CPU;


Type
  TGSCadencer = class(TObject)
  private
    FLastCall : Cardinal;
    FTimeScaler: Double;
    FDeltaTime : Cardinal;
    FTic : Cardinal;
    FCallBySec : Cardinal;
    FInternalCallBySec : Cardinal;
    FMCSec : Cardinal;
    FSecFromStart : Cardinal;
    FStartTime : Cardinal;
    FTotalCallBySecond : Cardinal;
    function GetSystemTick: Cardinal;
    function GetStartTime: Cardinal;
  Public
    constructor Create;

    Procedure Update;
    Procedure Reset;

    Function TimeSliceValue(aValueToSlice : Double) : Double;

    property TimeScaler : Double read FTimeScaler Write FTimeScaler;
    Property Tic : Cardinal read FTic;
    Property DeltaTime : Cardinal read FDeltaTime;

    Property CallBySecond : Cardinal read FCallBySec;
    property TimeElapsedSinceStart : Cardinal read GetStartTime;
    property TotalCallSinceStart : Cardinal read FTotalCallBySecond;

  end;

var
  GSGlobalCadencer : TGSCadencer;

Implementation


 { TGSCadencer }

  constructor TGSCadencer.Create;
  begin
    Inherited;
    Reset;
    FSecFromStart := 0;
    FTotalCallBySecond := 0;
    FStartTime := gsGetTickCount;
  end;

  function TGSCadencer.TimeSliceValue(aValueToSlice: Double): Double;
  begin
    Result := aValueToSlice * FDeltaTime / 1000;  //div by 1000, cause we assume that value is by seconds.
  end;


  procedure TGSCadencer.Update;
  var h : cardinal;
  begin
    FTic := gsGetTickCount;
    FDeltaTime := Trunc((FTic - FLastCall) * FTimeScaler);
    FLastCall := FTic;

    h := Round(Ftic/1000);
    if h<>FMCSec then
    begin
      FMCSec := h;
      FCallBySec := FInternalCallBySec;
      FTotalCallBySecond := FTotalCallBySecond + FInternalCallBySec;
      FInternalCallBySec:=0;
      Inc(FSecFromStart);
    end
    else
    begin
      Inc(FInternalCallBySec);
    end;
  end;

 procedure TGSCadencer.Reset;
begin
  TimeScaler := 1; //Realtime by Default. 1 sec real world = 1 sec in game.
  FLastCall := gsGetTickCount;
  FInternalCallBySec := 0;
  FMCSec := 0;
end;

function TGSCadencer.GetStartTime: Cardinal;
begin
  result := gsGetTickCount - FStartTime;
end;

function TGSCadencer.GetSystemTick: Cardinal;
begin
  result := Ftic;
end;


Initialization;

  GSGlobalCadencer := TGSCadencer.Create;
  GSGlobalCadencer.Reset;

finalization

  GSGlobalCadencer.Free;

end.
