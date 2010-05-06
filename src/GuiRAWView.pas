unit GuiRAWView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GLScene, GLGraph, GLCoordinates, GLCrossPlatform, BaseClasses,
  GLWin32Viewer, GLObjects, GLColor, VectorGeometry, GLSimpleNavigation,
  JvExControls, JvInspector, SpTBXItem, TB2Item, GLVectorFileObjects, GLMesh;

type
  TRAWViewForm = class(TForm)
    Viewer: TGLSceneViewer;
    Scene: TGLScene;
    Camera: TGLCamera;
    DummyCube: TGLDummyCube;
    Navigation: TGLSimpleNavigation;
    CamLight: TGLLightSource;
    HeightField: TGLHeightField;
    WaterPlane: TGLPlane;
    CameraTarget: TGLDummyCube;
    Inspector: TJvInspector;
    StatusBar: TSpTBXStatusBar;
    ZLabel: TSpTBXLabelItem;
    YLabel: TSpTBXLabelItem;
    XLabel: TSpTBXLabelItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBSeparatorItem2: TTBSeparatorItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ViewerPostRender(Sender: TObject);
  private
    FApplicationTitle : string;
    FInitLoad : boolean;
    FMouseMoveMutex : boolean;
    FBuffer : TMemoryStream;
    FMapSize: integer;
    FWorldSize: integer;
    FMapHeightScale: Single;

    FMinZ : single;
    FMaxZ : single;
    { Déclarations privées }
    procedure BattlefieldFormula(const x, y: Single; var z: Single; var color: TColorVector; var texPoint: TTexPoint);
    procedure SetMapSize(const Value: integer);
    procedure SetMapHeightScale(const Value: Single);
    procedure SetWorldSize(const Value: integer);
    function GetTitle: string;
    procedure SetTitle(const Value: string);
  public
    { Déclarations publiques }
    procedure LoadTerrain(Filename: string); overload;
    procedure LoadHeightmap(Filename: string); overload;
    procedure LoadTerrain(Data: TStream); overload;
    procedure LoadHeightmap(Data : TStream); overload;
    property MapSize : integer read FMapSize write SetMapSize;
    property MapHeightScale : Single read FMapHeightScale write SetMapHeightScale;
    property WorldSize : integer read FWorldSize write SetWorldSize;


    property Title : string read GetTitle write SetTitle;
  end;

var
  RAWViewForm: TRAWViewForm;

const
  MOUSEWIDTH = 2;

implementation

{$R *.dfm}

uses
  Math, VectorTypes, DbugIntf, StringFunction;

{ TRAWViewForm }


procedure TRAWViewForm.FormCreate(Sender: TObject);
begin
  FApplicationTitle := Caption + ' - ' + Application.Title;;

  FMapSize := -1;
  FBuffer := TMemoryStream.Create;

  Inspector.InspectObject := WaterPlane;
  Inspector.Free;
end;

procedure TRAWViewForm.FormDestroy(Sender: TObject);
begin
  FBuffer.Free;
end;




function GetStringFromProperty(Data:TStringList; Elt : string) : string;
var
  Line, Pos : Integer;
  Wtd : string;
begin
  //Pos := Data.IndexOf(Elt);
  for Line := 0 to Data.Count - 1 do
  begin
    Pos := AnsiPos(Elt, Data[Line]);
    if Pos > 0 then
      break;
  end;

  if (Line > 0) and (Pos > 0) then
  begin
    Wtd := StringReplace(Data[Line], Elt, EmptyStr, [rfReplaceAll, rfIgnoreCase]);
    Wtd := Trim(Wtd);
    Result := Wtd;
  end
    else
      Result := EmptyStr;
end;

function GetFloatFromProperty(Data:TStringList; Elt : string) : Extended;
var
  SystemDS : Char;
begin
  SystemDS := DecimalSeparator;
  DecimalSeparator := '.';
  Result := StrToFloatDef(GetStringFromProperty(Data, Elt), 0);
  DecimalSeparator := SystemDS;
end;

function GetIntFromProperty(Data:TStringList; Elt : string) : integer;
begin
  Result := StrToIntDef(GetStringFromProperty(Data, Elt), 0);
end;

procedure TRAWViewForm.LoadTerrain(Filename: string);
var
  Stream : TFileStream;
begin
  if FileExists(Filename) then
  begin
    Title := Filename;
    Stream := TFileStream.Create(Filename, fmOpenRead);
    LoadTerrain(Stream);
    Stream.Free;
  end;
end;


procedure TRAWViewForm.LoadTerrain(Data: TStream);
var
  TxtData : TStringList;
  flworldSize, flmaterialSize, flwaterLevel, flseaFloorLevel : extended;
  flyScale : extended;
begin
  Data.Position := 0;

  TxtData := TStringList.Create;
  TxtData.LoadFromStream(Data);

  //Showmessage(Data.Text);

  flmaterialSize := GetFloatFromProperty(TxtData, 'GeometryTemplate.materialSize');
  flworldSize := GetFloatFromProperty(TxtData, 'GeometryTemplate.worldSize');
  flyScale := GetFloatFromProperty(TxtData, 'GeometryTemplate.yScale');
  flwaterLevel := GetFloatFromProperty(TxtData, 'GeometryTemplate.waterLevel');
  flseaFloorLevel := GetFloatFromProperty(TxtData, 'GeometryTemplate.seaFloorLevel');

  MapSize := Round(flmaterialSize);
  MapHeightScale := flyScale;
  WorldSize := Round(flworldSize);
  WaterPlane.Position.Z := flwaterLevel;

  TxtData.Free;
end;


procedure TRAWViewForm.LoadHeightmap(Filename: string);
var
  Stream : TFileStream;
begin
  if FileExists(Filename) then
  begin
    Stream := TFileStream.Create(Filename, fmOpenRead);
    LoadHeightmap(Stream);
    Stream.Free;
  end;
end;

procedure TRAWViewForm.LoadHeightmap(Data: TStream);
begin
  FMinZ := MAXWORD;
  FMaxZ := 0;

  Data.Position := 0;
  FBuffer.LoadFromStream(Data);

  with HeightField do
  begin
    with XSamplingScale do
    begin
      Min := 0;
      Max := FWorldSize-1;
      Step := 4;
    end;
    with YSamplingScale do
    begin
      Min := 0;
      Max := FWorldSize-1;
      Step := 4;
    end;
    OnGetHeight:=BattlefieldFormula;
  end;
  HeightField.StructureChanged;
  HeightField.Options:=HeightField.Options+[hfoTwoSided];
  FInitLoad := true;
end;

procedure TRAWViewForm.BattlefieldFormula(const x, y: Single; var z: Single;
  var color: TColorVector; var texPoint: TTexPoint);
var
  XPos, YPos : LongWord;
  ZValue : Word;
begin
  if FMapSize < 0 then
  begin
    z := 0;
    Exit;
  end;

  // We need a step of 2 due to WORD length
  XPos := Round(x/HeightField.XSamplingScale.Step) * 2;
  YPos := Round(y/HeightField.YSamplingScale.Step) * 2 * (FWorldSize div 4);

  FBuffer.Seek(XPos+YPos, soFromBeginning);
  FBuffer.Read(ZValue, 2);

  z := ZValue/(256/FMapHeightScale);

  FMinZ := Min(FMinZ,z);
  FMaxZ := Max(FMaxZ,z);

  VectorLerp(clrGreen, clrWhite, (z+1)/256, color);
end;

procedure TRAWViewForm.SetMapHeightScale(const Value: Single);
begin
  FMapHeightScale := Value;
end;


procedure TRAWViewForm.SetWorldSize(const Value: integer);
begin
  FWorldSize := Value;

  CameraTarget.Position.X := FWorldSize div 2;
  CameraTarget.Position.Y := FWorldSize div 2;

  WaterPlane.Width := FWorldSize;
  WaterPlane.Height := FWorldSize;

  WaterPlane.Position.X := FWorldSize div 2;
  WaterPlane.Position.Y := FWorldSize div 2;
end;

procedure TRAWViewForm.SetMapSize(const Value: integer);
begin
  FMapSize := Value;
end;


procedure TRAWViewForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Coo3D: TVector3f;
  v : TAffineVector;
begin

  if FMouseMoveMutex or not Assigned(Scene.CurrentBuffer) then
    Exit
  else
    FMouseMoveMutex := true;

  if not (ssLeft in Shift) and not (ssRight in Shift) then
  begin
    Coo3D := Scene.CurrentBuffer.OrthoScreenToWorld(X,Y);

    // In Paint mode
    // get absolute 3D coordinates of the point below the mouse
    v:=Scene.CurrentBuffer.PixelRayToWorld(x, y);
    // convert to heightfield local coordinates
    v:=HeightField.AbsoluteToLocal(v);

    XLabel.Caption := Format('X=%.2f',[v[0]]);
    YLabel.Caption := Format('Y=%.2f',[v[1]]);
    ZLabel.Caption := Format('Z=%.2f',[v[2]]);
  end;

   FMouseMoveMutex := false;
end;

procedure TRAWViewForm.ViewerPostRender(Sender: TObject);
begin
  if FInitLoad then
  begin
    Camera.Position.Y := FMaxZ;

    FInitLoad := false;
  end;
end;


function TRAWViewForm.GetTitle: string;
begin
 Result := Caption
end;

procedure TRAWViewForm.SetTitle(const Value: string);
begin
  if Value <> EmptyStr then
    Caption := Format('%s - %s',[ExtractFilename(Value), FApplicationTitle])
  else
    Caption := FApplicationTitle;
end;

end.
