unit GS.Soft3D.PipeLine.FragmentShader;

interface

uses Classes,
     SysUtils,
     GS.Geometry,
     GS.Soft3d.Types,
     GS.Soft3d.MeshTools,
     GS.Soft3d.PipeLine.Types,
     GS.Soft3d.PipeLine.Raster;

Type
  TS3DFragmentShader = class;

  TS3DFragmentShaderControl = class(TS3DPipeLineStep)
    defaultShader : TS3DFragmentShader;
    x,y : Uint32;
    Z : Single;
    ObjIndex,
    FaceIndex : Uint32;

    result_rgba : Vec4;
  public
    function Run : Boolean; Override;

    Constructor Create(Owner : TObject; _in : TS3DInputData3D; _wo : TS3DPipeLineData); Override;
    Destructor Destroy; override;
  end;

    TS3DFragmentShader = class
    public
      control : TS3DFragmentShaderControl;

      procedure Process; virtual; abstract;
    end;

    TS3DFragmentShader_DepthColor = class(TS3DFragmentShader)
    public
      procedure Process; override;
    end;

    TS3DFragmentShader_Color = class(TS3DFragmentShader)
    public
      procedure Process; override;
    end;

implementation

uses GS.Soft3d.PipeLine;

{ TS3DFragmentShaderControl }


constructor TS3DFragmentShaderControl.Create(Owner: TObject;
  _in: TS3DInputData3D; _wo: TS3DPipeLineData);
begin
  inherited;
  defaultShader := TS3DFragmentShader_Color.Create;
end;

destructor TS3DFragmentShaderControl.Destroy;
begin
  FreeAndNil(defaultShader);
  inherited;
end;

function TS3DFragmentShaderControl.Run : boolean;
begin
  result := true;
  defaultShader.control := self;
  defaultShader.Process;
end;

{ TS3DFragmentShader_DepthColor }

procedure TS3DFragmentShader_DepthColor.Process;
var v : double;
begin
  v := _Clamp(control.Z*10,0,1);
  control.result_rgba.create(v,v,v,v);
end;

{ TS3DFragmentShader_Color }

procedure TS3DFragmentShader_Color.Process;
begin
  control.result_rgba := control.InputData.Objects[control.ObjIndex].DefaultColor;
end;

end.
