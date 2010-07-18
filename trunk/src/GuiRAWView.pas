(* ***** BEGIN LICENSE BLOCK *****
  * Version: GNU GPL 2.0
  *
  * The contents of this file are subject to the
  * GNU General Public License Version 2.0; you may not use this file except
  * in compliance with the License. You may obtain a copy of the License at
  * http://www.gnu.org/licenses/gpl.html
  *
  * Software distributed under the License is distributed on an "AS IS" basis,
  * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  * for the specific language governing rights and limitations under the
  * License.
  *
  * The Original Code is GuiRAWView (http://code.google.com/p/bga)
  *
  * The Initial Developer of the Original Code is
  * Yann Papouin <yann.papouin at @ gmail.com>
  *
  * ***** END LICENSE BLOCK ***** *)

unit GuiRAWView;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  GuiFormCommon,
  Dialogs,
  GLScene,
  GLGraph,
  GLCoordinates,
  GLCrossPlatform,
  BaseClasses,
  GLWin32Viewer,
  GLObjects,
  GLColor,
  DDS,
  VectorGeometry,
  VectorTypes,
  GLSimpleNavigation,
  JvExControls,
  JvInspector,
  SpTBXItem,
  TB2Item,
  GLVectorFileObjects,
  GLMesh,
  SpTBXControls,
  BGALib,
  Generics.Collections,
  GLMaterial;

type

  THFList = class(TObjectList<TGLHeightField>)
  private
  public
    constructor Create;
  end;

  TRAWViewForm = class(TFormCommon)
    Viewer: TGLSceneViewer;
    Scene: TGLScene;
    Camera: TGLCamera;
    Navigation: TGLSimpleNavigation;
    CamLight: TGLLightSource;
    WaterPlane: TGLPlane;
    CameraTarget: TGLDummyCube;
    Inspector: TJvInspector;
    StatusBar: TSpTBXStatusBar;
    ZLabel: TSpTBXLabelItem;
    YLabel: TSpTBXLabelItem;
    XLabel: TSpTBXLabelItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBSeparatorItem2: TTBSeparatorItem;
    GLMaterialLibrary: TGLMaterialLibrary;
    Root: TGLDummyCube;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerPostRender(Sender: TObject);
  private
    FInitLoad: boolean;
    FMouseMoveMutex: boolean;
    FBuffer: TMemoryStream;
    FMapSize: Integer;
    FWorldSize: Integer;
    FRawStep: Integer;
    FMapHeightScale: Single;

    FMinZ: Single;
    FMaxZ: Single;

    FHeightfields : THFList;
    { Déclarations privées }
    procedure BattlefieldFormula(Sender: TObject; const x, y: Single; var z: Single; var color: TVector4f; var texPoint: TTexPoint);
    procedure SetMapSize(const Value: Integer);
    procedure SetMapHeightScale(const Value: Single);
    procedure SetWorldSize(const Value: Integer);

    procedure LoadTerrain(Data: TStream); overload;
    procedure LoadHeightmap(Data: TStream); overload;
  public
    { Déclarations publiques }
    GetFileByPath: TBgaGetFileByPath;

    TexturePart: Integer;
    TextureSize: Integer;
    TextureBaseName: string;
    HeightMap: string;

    procedure LoadTerrain(Filename: string); overload;
    property MapSize: Integer read FMapSize write SetMapSize;
    property MapHeightScale: Single read FMapHeightScale write SetMapHeightScale;
    property WorldSize: Integer read FWorldSize write SetWorldSize;
  end;


var
  RAWViewForm: TRAWViewForm;

const
  MOUSEWIDTH = 2;

implementation

{$R *.dfm}

uses
  Math,
  DbugIntf,
  StringFunction,
  AppLib,
  CONLib;

{ TRAWViewForm }

procedure TRAWViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FMapSize := -1;
  FBuffer := TMemoryStream.Create;
  FHeightfields := THFList.Create;

  Inspector.InspectObject := WaterPlane;
  Inspector.Free;
end;

procedure TRAWViewForm.FormDestroy(Sender: TObject);
begin
  FHeightfields.Free;
  FBuffer.Free;
end;

procedure TRAWViewForm.LoadTerrain(Filename: string);
var
  Stream: TFileStream;
  HeightMapFile, TextureName, TextureFile: string;
  Row, Col: Integer;
  LibMaterial: TGLLibMaterial;
  HeightField : TGLHeightField;
begin
  GLMaterialLibrary.Materials.Clear;

  if FileExists(Filename) then
  begin
    Title := Filename;
    Stream := TFileStream.Create(Filename, fmOpenRead);
    LoadTerrain(Stream);
    Stream.Free;

    for Col := 0 to TexturePart - 1 do
      for Row := 0 to TexturePart - 1 do
      begin
        HeightField := TGLHeightField.Create(Self);
        HeightField.Parent := Root;
        HeightField.Visible := true;
        FHeightfields.Add(HeightField);

        TextureName := Format('%s%.2dx%.2d.dds', [TextureBaseName, Col, Row]); // SendDebug(TextureName);
        TextureFile := GetFileByPath(Self, TextureName);

        if FileExists(TextureFile) then
        begin
          LibMaterial := GLMaterialLibrary.AddTextureMaterial(TextureName, TextureFile);
          HeightField.Material.MaterialLibrary := GLMaterialLibrary;
          HeightField.Material.LibMaterialName := TextureName;
        end;
      end;

    HeightMapFile := GetFileByPath(Self, HeightMap);
    Stream := TFileStream.Create(HeightMapFile, fmOpenRead);
    LoadHeightmap(Stream);
    Stream.Free;

  end;
end;

procedure TRAWViewForm.LoadTerrain(Data: TStream);
var
  TxtData: TStringList;
  flworldSize, flmaterialSize, flwaterLevel, flseaFloorLevel: extended;
  flyScale: extended;
  strFile, strtexBaseName: string;
begin
  Data.Position := 0;

  TxtData := TStringList.Create;
  TxtData.LoadFromStream(Data);

  strFile := GetStringFromProperty(TxtData, 'GeometryTemplate.file');
  strtexBaseName := GetStringFromProperty(TxtData, 'GeometryTemplate.texBaseName');

  flmaterialSize := GetFloatFromProperty(TxtData, 'GeometryTemplate.materialSize');
  flworldSize := GetFloatFromProperty(TxtData, 'GeometryTemplate.worldSize');
  flyScale := GetFloatFromProperty(TxtData, 'GeometryTemplate.yScale');
  flwaterLevel := GetFloatFromProperty(TxtData, 'GeometryTemplate.waterLevel');
  flseaFloorLevel := GetFloatFromProperty(TxtData, 'GeometryTemplate.seaFloorLevel');

  HeightMap := strFile + '.raw';
  TextureBaseName := strtexBaseName;

  MapSize := Round(flmaterialSize);
  MapHeightScale := flyScale;
  WorldSize := Round(flworldSize);
  WaterPlane.Position.z := flwaterLevel;
  FRawStep := FWorldSize div MapSize;
  TextureSize := WorldSize * 4;
  TexturePart := TextureSize div 1024;

  TxtData.Free;
end;

procedure TRAWViewForm.LoadHeightmap(Data: TStream);
var
  i :integer;
  XState, YState, Part : integer;
  Row, Col: Integer;
begin
  FMinZ := MAXWORD;
  FMaxZ := 0;

  Data.Position := 0;
  FBuffer.LoadFromStream(Data);

  XState := 0;
  YState := 0;
  Part := (FWorldSize-1) div Round(Sqrt(FHeightfields.Count)) ;

  i := 0;
  for Col := 0 to TexturePart - 1 do
    for Row := 0 to TexturePart - 1 do
    begin
      XState := Row * Part;
      YState := Col * Part;

      FHeightfields[i].XSamplingScale.Min := XState;
      FHeightfields[i].YSamplingScale.Min := YState;

      XState := (Row+1) * Part;
      YState := (Col+1) * Part;

      if Row < TexturePart - 1 then
      begin
        XState := XState + FRawStep;
      end;

      if Col < TexturePart - 1 then
      begin
        YState := YState + FRawStep;
      end;

      FHeightfields[i].XSamplingScale.Max := XState;
      FHeightfields[i].YSamplingScale.Max := YState;

      SendDebugFmt('HF %.2d  XMin = %.4d   XMax = %.4d',[i, Round(FHeightfields[i].XSamplingScale.Min), Round(FHeightfields[i].XSamplingScale.Max)]);
      SendDebugFmt('         YMin = %.4d   YMax = %.4d',[   Round(FHeightfields[i].YSamplingScale.Min), Round(FHeightfields[i].YSamplingScale.Max)]);

      FHeightfields[i].XSamplingScale.Step := FRawStep;
      FHeightfields[i].YSamplingScale.Step := FRawStep;
      FHeightfields[i].OnGetHeight2 := BattlefieldFormula;
      FHeightfields[i].StructureChanged;
      FHeightfields[i].Options := FHeightfields[i].Options + [hfoTwoSided];

      Inc(i);
  end;

  FInitLoad := true;
end;

procedure TRAWViewForm.BattlefieldFormula(Sender: TObject; const X, Y: Single; var z: Single; var color: TColorVector; var texPoint: TTexPoint);
var
  XPos, YPos: LongWord;
  ZValue: Word;
begin
  if FMapSize < 0 then
  begin
    z := 0;
    Exit;
  end;

  // We need a step of 2 due to WORD length
  XPos := Round(X / (Sender as TGLHeightField).XSamplingScale.Step) * 2;
  YPos := Round(Y / (Sender as TGLHeightField).YSamplingScale.Step) * 2 * (FWorldSize div FRawStep);

  FBuffer.Seek(XPos + YPos, soFromBeginning);
  FBuffer.Read(ZValue, 2);

  z := ZValue / (256 / FMapHeightScale);

  FMinZ := Min(FMinZ, z);
  FMaxZ := Max(FMaxZ, z);

  VectorLerp(clrGreen, clrWhite, (z + 1) / 256, color);
end;

procedure TRAWViewForm.SetMapHeightScale(const Value: Single);
begin
  FMapHeightScale := Value;
end;

procedure TRAWViewForm.SetWorldSize(const Value: Integer);
begin
  FWorldSize := Value;

  CameraTarget.Position.X := FWorldSize div 2;
  CameraTarget.Position.Y := FWorldSize div 2;

  WaterPlane.Width := FWorldSize;
  WaterPlane.Height := FWorldSize;

  WaterPlane.Position.X := FWorldSize div 2;
  WaterPlane.Position.Y := FWorldSize div 2;
end;

procedure TRAWViewForm.SetMapSize(const Value: Integer);
begin
  FMapSize := Value;
end;

procedure TRAWViewForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Coo3D: TVector3f;
  v: TAffineVector;
begin

  if FMouseMoveMutex or not Assigned(Scene.CurrentBuffer) then
    Exit
  else
    FMouseMoveMutex := true;

  if not(ssLeft in Shift) and not(ssRight in Shift) then
  begin
    Coo3D := Scene.CurrentBuffer.OrthoScreenToWorld(X, Y);

    // In Paint mode
    // get absolute 3D coordinates of the point below the mouse
    v := Scene.CurrentBuffer.PixelRayToWorld(X, Y);
    // convert to heightfield local coordinates
    //v := HeightFieldBase.AbsoluteToLocal(v);

    XLabel.Caption := Format('X=%.2f', [v[0]]);
    YLabel.Caption := Format('Y=%.2f', [-v[2]]);
    ZLabel.Caption := Format('Z=%.2f', [v[1]]);
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

{ THFList }

constructor THFList.Create;
begin
  OwnsObjects := true;
end;

end.
