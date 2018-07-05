unit unElementobject;

interface

uses
{$IFDEF DELPHINATIVEJSON}
  Rest.Json;
{$ELSE}
  GS.JSon;
{$ENDIF}

type

{
            "name": "Vanadium",
            "appearance": "blue-silver-grey metal",
            "atomic_mass": 50.94151,
            "boil": 3680,
            "category": "transition metal",
            "color": null,
            "density": 6.0,
            "discovered_by": "Andr\u00e9s Manuel del R\u00edo",
            "melt": 2183,
            "molar_heat": 24.89,
            "named_by": "Isotopes of vanadium",
            "number": 23,
            "period": 4,
            "phase": "Solid",
            "source": "https://en.wikipedia.org/wiki/Vanadium",
            "spectral_img": null,
            "summary": "Vanadium is a chemical element with symbol V and atomic number 23. It is a hard, silvery grey, ductile and malleable transition metal. The element is found only in chemically combined form in nature, but once isolated artificially, the formation of an oxide layer stabilizes the free metal somewhat against further oxidation.",
            "symbol": "V",
            "xpos": 5,
            "ypos": 4,
            "shells": [
                8,
                11,
                2
            ]
}
  TElement = class;

  //Json list marshalling build on typed Array of -> Define the needed typed array
  TArrayOfElement = array of TElement;
  TArrayOfString = array of String;
  TArrayOfInteger = array of Integer;

  TElement = class
  private
  protected
    FSymbol: string;
    FName: string;
    FXPos: Integer;
    FMolarHeat: Double;
    FYPos: Integer;
    FSource: string;
    fDiscovered_by: string;
    FDensity: Double;
    FPhase: string;
    FColor: string;
    FAppearance: string;
    FSpectral_Img: string;
    fBoil: Double;
    FNumber: Integer;
    FPeriod: Integer;
    FSummary: string;
    FMelt: Double;
    FCategory: string;
    FNamed_By: string;
    FAtomic_Mass: Double;
    FShells : TArrayofInteger;
  Public
  Published
    property Name: string read FName Write FName;
    property Appearance: string read FAppearance Write FAppearance;
    property Atomic_Mass: Double read FAtomic_Mass Write FAtomic_Mass;
    property Boil: Double read fBoil write fBoil;
    property Category: string read FCategory write FCategory;
    property Color: string read FColor write fColor;
    property Density: Double read FDensity Write FDensity;
    property DiscoveredBy: string read fDiscovered_by write fDiscovered_by;
    property Melt: Double read FMelt write FMelt;
    property MolarHeat: Double read FMolarHeat Write FMolarHeat;
    property Named_by: string read FNamed_By write FNamed_By;
    property Number: Integer read FNumber write FNumber;
    property Periond: Integer read FPeriod Write FPeriod;
    property Phase: string read FPhase Write FPhase;
    property Source: string read FSource Write FSource;
    property Spectral_Img: string read FSpectral_Img Write FSpectral_Img;
    property Summary: string read FSummary Write FSummary;
    property Symbol: string read FSymbol Write FSymbol;
    property XPos: Integer read FXPos Write FXPos;
    property YPos: Integer read FYPos Write FYPos;
    property Shells : TArrayOfInteger read FShells Write FShells; //TODO : does not work yet.
  end;

  TPeriodicTable = Class
  private
    FElements : TArrayOfElement;
    FFirstElem: TElement;
    FDateTest: TDateTime;
  public
    Function AddElement : TElement;

    Constructor Create; Virtual;
    Destructor Destroy; Override;
  published
    property DateTest : TDateTime read FDateTest Write FDateTEst;
    property FirstElement : TElement read FFirstElem Write FFirstElem;
    Property Elements : TArrayOfElement read FElements Write FElements;
  End;

implementation

{ TPeriodicTable }

function TPeriodicTable.addElement: TElement;
begin
  SetLength(FElements,Length(Elements)+1);
  result := TElement.Create;
  FElements[Length(Elements)-1] := Result;
end;

constructor TPeriodicTable.Create;
begin
  FFirstElem := Nil;
  FElements:= nil;
end;

destructor TPeriodicTable.Destroy;
var l : TElement;
begin
  for l in FElements do
    l.Free;
  if Assigned(FFirstElem) then
    FFirstElem.Free;
  FElements:= nil;
  inherited;
end;



end.

