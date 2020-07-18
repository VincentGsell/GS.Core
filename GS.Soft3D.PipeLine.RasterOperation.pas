unit GS.Soft3D.PipeLine.RasterOperation;

interface


uses Classes,
     SysUtils,
     GS.Common.Monitoring,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.FragmentShader;
Type

  TS3DRasterOperation = class(TS3DPipeLineStep)
  private
  protected
    FClearRaster: boolean;
    FFullWireFrame: boolean;
    FEnableRasterEngine: boolean;
  public
    FragControl : TS3DFragmentShaderControl;

    //Must be implemented into surface enabled techno descendant (such as Pixel32)
    function Run : boolean; virtual; abstract;
    //Resisze pixel buffer.
    procedure Resize(_width, _height : Uint32); Virtual; abstract;

    Constructor Create(Owner : TObject; _in : TS3DInputData3D; _wo : TS3DPipeLineData); Override;
    Destructor Destroy; Override;

    property EnableClearRaster : boolean read FClearRaster write FClearRaster;
    property EnableFullWireFrame : boolean read FFullWireFrame write FFullWireFrame;
    property EnableRasterEngine : boolean read FEnableRasterEngine write FEnableRasterEngine;
  end;


implementation

{ TS3DRasterOperation }

constructor TS3DRasterOperation.Create(Owner: TObject; _in: TS3DInputData3D;
  _wo: TS3DPipeLineData);
begin
  inherited;
  FragControl :=  TS3DFragmentShaderControl.Create(Owner,_in,_wo);
  FClearRaster:=true;
  FFullWireFrame:=false;
  FEnableRasterEngine:=true;
end;

destructor TS3DRasterOperation.Destroy;
begin
  FreeAndNil(FragControl);
  inherited;
end;

end.
