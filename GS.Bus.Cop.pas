///-------------------------------------------------------------------------------
/// Title      : GS.Bus.cop
/// Short Desc : Easy Communication layer fo GS.Bus.
/// Source     : https://github.com/VincentGsell
/// Aim        : - Bus COP : COmmunication Point.
///              - Object to easy communicate between each other.
///
/// History
/// 20180720 - VGS - Created.
///-------------------------------------------------------------------------------
unit GS.Bus.COP;

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

Uses
{$IFDEF FPC}
  Classes,
  SysUtils,
  Generics.Collections,
{$ELSE}
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
{$ENDIF}
  GS.Bus;

Const
  CST_CHECKMESSAGETIMEOUT = 500; //ms

Type

//FullDuplex communication object. For User.
TCustomBusCommunicationWire = class
Protected
Public
  Procedure CheckMessages; virtual; abstract;
  Procedure SendMessage(Const aMessage : TBusMessage); virtual; abstract;

  Constructor Create(Const aComId : String; Const aComName : String = ''); virtual; Abstract;
end;

//Object witch manage communication
TCustomBusCommunicationPoint = class
  Function GetCommunicationWire(adresse : String; Out aCommDevice :TCustomBusCommunicationWire) : Boolean; virtual; Abstract;

end;





implementation

end.
