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
  SpTBXItem,
  SpTBXControls,
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
  GLTexture,
  DDS,
  VectorGeometry,
  VectorTypes,
  GLSimpleNavigation,
  JvExControls,
  JvInspector,
  TB2Item,
  GLVectorFileObjects,
  GLMesh,
  BGALib,
  generics.defaults,
  generics.Collections,
  GLMaterial,
  SpTBXDkPanels, GLHeightData, GLTerrainRenderer;

type

  THFList = class(TObjectList<TGLHeightField>)
  private
  public
    constructor Create;
    procedure SortByTag;
  end;

  THFComparer = class(TComparer<TGLHeightField>)
    function Compare(const Left, Right: TGLHeightField): Integer; override;
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
    SpTBXSplitter1: TSpTBXSplitter;
    BattlefieldHDS: TGLCustomHDS;
    TerrainRenderer: TGLTerrainRenderer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerPostRender(Sender: TObject);
    procedure ViewerDblClick(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BattlefieldHDSStartPreparingData(heightData: THeightData);
  private
    v: TAffineVector;
    M: TPoint;

    FInitLoad: boolean;
    FMouseMoveMutex: boolean;
    FBuffer: TMemoryStream;
    FMapSize: Integer;
    FWorldSize: Integer;
    FRawStep: Integer;
    FMapHeightScale: Single;

    FMinZ: Single;
    FMaxZ: Single;

    HeightfieldList: THFList;
    { Déclarations privées }
    procedure BattlefieldFormula(Sender: TObject; const X, Y: Single; var z: Single; var color: TVector4f; var texPoint: TTexPoint);
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
    DetailBaseName: string;
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
  HeightfieldList := THFList.Create;

  // Inspector.InspectObject := WaterPlane;
  Inspector.Free;
end;

procedure TRAWViewForm.FormDestroy(Sender: TObject);
begin
  HeightfieldList.Free;
  FBuffer.Free;
end;

procedure TRAWViewForm.LoadTerrain(Filename: string);
var
  Stream: TFileStream;
  TextureName, TextureFile, DetailFile, HeightMapFile: string;
  i, Row, Col: Integer;
  LibMaterial: TGLLibMaterial;
  HeightField: TGLHeightField;
{$IFDEF RAWVIEW_DRAW_NAME}
  Bmp: TBitmap;
{$ENDIF}
begin
  Assert(Assigned(GetFileByPath));
  HeightfieldList.Clear;
  GLMaterialLibrary.Materials.Clear;

  if FileExists(Filename) then
  begin
    Title := Filename;
    Stream := TFileStream.Create(Filename, fmOpenRead);
    LoadTerrain(Stream);
    Stream.Free;

    DetailFile := GetFileByPath(Self, DetailBaseName);

    for Row := 0 to TexturePart - 1 do
      for Col := 0 to TexturePart - 1 do
      begin
        HeightField := TGLHeightField.Create(Self);
        HeightField.Tag := HeightfieldList.Count;
        HeightField.Parent := Root;
        HeightField.Visible := true;
        HeightfieldList.Add(HeightField);

        TextureName := Format('%s%.2dx%.2d.dds', [TextureBaseName, Col, Row]); // SendDebug(TextureName);
        TextureFile := GetFileByPath(Self, TextureName);

        if FileExists(TextureFile) then
        begin
          LibMaterial := GLMaterialLibrary.AddTextureMaterial(TextureName, TextureFile);
          HeightField.Material.MaterialLibrary := GLMaterialLibrary;
          HeightField.Material.LibMaterialName := TextureName;
          LibMaterial.Texture2Name := DetailFile;

{$IFDEF RAWVIEW_DRAW_NAME}
          Bmp := TBitmap.Create;
          Bmp.PixelFormat := pf24bit;
          LibMaterial.Material.Texture.Image.AssignToBitmap(Bmp);
          with Bmp.Canvas do
          begin
            Font.Size := 86;
            Font.color := clWhite;
            Brush.color := clBlack;
            TextOut(50, 50, SFRightRight('\', TextureName));
          end;
          LibMaterial.Material.Texture.Image.Assign(Bmp);
          Bmp.Free;
{$ENDIF}
        end;
      end;

    HeightMapFile := GetFileByPath(Self, HeightMap);
    if FileExists(HeightMapFile) then
    begin
      Stream := TFileStream.Create(HeightMapFile, fmOpenRead);
      LoadHeightmap(Stream);
      Stream.Free;
    end;

  end;
end;

procedure TRAWViewForm.LoadTerrain(Data: TStream);
var
  TxtData: TStringList;
  flworldSize, flmaterialSize, flwaterLevel, flseaFloorLevel: extended;
  flyScale: extended;
  strFile, strtexBaseName, strdetailTexName: string;
  HeightMapFile: string;
  Stream: TFileStream;
begin
  Data.Position := 0;

  TxtData := TStringList.Create;
  TxtData.LoadFromStream(Data);

  strFile := GetStringFromProperty(TxtData, 'GeometryTemplate.file');
  strtexBaseName := GetStringFromProperty(TxtData, 'GeometryTemplate.texBaseName');
  strdetailTexName := GetStringFromProperty(TxtData, 'GeometryTemplate.detailTexName');

  flmaterialSize := GetFloatFromProperty(TxtData, 'GeometryTemplate.materialSize');
  flworldSize := GetFloatFromProperty(TxtData, 'GeometryTemplate.worldSize');
  flyScale := GetFloatFromProperty(TxtData, 'GeometryTemplate.yScale');
  flwaterLevel := GetFloatFromProperty(TxtData, 'GeometryTemplate.waterLevel');
  flseaFloorLevel := GetFloatFromProperty(TxtData, 'GeometryTemplate.seaFloorLevel');

  HeightMap := strFile + '.raw';
  TextureBaseName := strtexBaseName;
  DetailBaseName := strdetailTexName + '.dds';

  MapSize := Round(flmaterialSize);
  MapHeightScale := flyScale;
  WorldSize := Round(flworldSize);
  WaterPlane.Position.z := flwaterLevel;
  FRawStep := FWorldSize div MapSize;
  TextureSize := WorldSize * 4;
  TexturePart := TextureSize div 1024;

  TxtData.Free;

end;
   {
procedure TRAWViewForm.LoadHeightmap(Data: TStream);
begin
  FBuffer.LoadFromStream(Data);
  FInitLoad := true;
end;
  }
procedure TRAWViewForm.LoadHeightmap(Data: TStream);
var
  i: Integer;
  XState, YState, Part: Integer;
  Row, Col: Integer;
  DivFact: Integer;
begin
  FMinZ := MAXWORD;
  FMaxZ := 0;

  Data.Position := 0;
  FBuffer.LoadFromStream(Data);

  XState := 0;
  YState := 0;

  if HeightfieldList.Count > 0 then
    Part := (FWorldSize - 1) div Round(Sqrt(HeightfieldList.Count));

  for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
  begin

    with GLMaterialLibrary.Materials[i] do
    begin
      Material.MaterialOptions := [moNoLighting];
      Material.Texture.MappingMode := tmmObjectLinear;
      Material.Texture.BorderColor.RandomColor;
      (*
        Texture.MappingSCoordinates.W := -4 + (0.5 / FRawStep); //3.98824691772461    0.012
        Texture.MappingSCoordinates.X := 1 / (Part+FRawStep/2);
        Texture.MappingSCoordinates.Y := 0;

        Texture.MappingTCoordinates.W := 4 - (0.5 / FRawStep);
        Texture.MappingTCoordinates.X := 0;
        Texture.MappingTCoordinates.Y := -1 / (Part+FRawStep/2);

        Texture.TextureWrap := twSeparate;
        Texture.TextureWrapR := twClampToBorder;
        Texture.TextureWrapS := twClampToBorder;
        Texture.TextureWrapT := twClampToBorder;
      *)
      TextureOffset.X := 0;
      TextureOffset.Y := 0;

      TextureScale.X := (1 / (Part));
      TextureScale.Y := -(1 / (Part));
      TextureScale.z := (1 / (Part));
      // GLMaterialLibrary.Materials.GetLibMaterialByName(HeightField.Material.LibMaterialName)
    end;
  end;

  DivFact := 2;
  i := 0;
  for Col := 0 to TexturePart - 1 do
    for Row := 0 to TexturePart - 1 do
    begin
      XState := Row * Part;
      YState := Col * Part;

      if Row > 0 then
      begin
        XState := XState - FRawStep div DivFact;
      end;

      if Col > 0 then
      begin
        YState := YState - FRawStep div DivFact;
      end;

      HeightfieldList[i].XSamplingScale.Min := XState;
      HeightfieldList[i].YSamplingScale.Min := YState;

      XState := (Row + 1) * Part;
      YState := (Col + 1) * Part;

      if Row < TexturePart - 1 then
      begin
        XState := XState + FRawStep div DivFact;
      end;

      if Col < TexturePart - 1 then
      begin
        YState := YState + FRawStep div DivFact;
      end;

      HeightfieldList[i].XSamplingScale.Max := XState;
      HeightfieldList[i].YSamplingScale.Max := YState;

      // SendDebugFmt('HF %.2d  XMin = %.4d   XMax = %.4d', [i, Round(FHeightfields[i].XSamplingScale.Min), Round(FHeightfields[i].XSamplingScale.Max)]);
      // SendDebugFmt('         YMin = %.4d   YMax = %.4d', [Round(FHeightfields[i].YSamplingScale.Min), Round(FHeightfields[i].YSamplingScale.Max)]);

      HeightfieldList[i].XSamplingScale.Step := FRawStep;
      HeightfieldList[i].YSamplingScale.Step := FRawStep;
      HeightfieldList[i].OnGetHeight2 := BattlefieldFormula;
      HeightfieldList[i].Options := HeightfieldList[i].Options + [hfoTwoSided];
      HeightfieldList[i].StructureChanged;

      // FHeightfields[i].Position.X := -FRawStep * Row;
      // FHeightfields[i].Position.Y := -FRawStep * Col;

      Inc(i);
    end;

  FInitLoad := true;
end;











procedure TRAWViewForm.BattlefieldHDSStartPreparingData(heightData: THeightData);
var
  Y: integer;
  X: integer;
  ZValue: Word;
  z : single;
  RasterLine: GLHeightData.PSingleArray;
  OldType    : THeightDataType;
  Notify : boolean;
begin
  if not InRange(heightData.YTop,0 , FWorldSize) or not InRange(heightData.XLeft,0 , FWorldSize) then
  begin

    heightData.DataState := hdsNone;
    Exit;
  end;

  heightData.DataState := hdsPreparing;
  OldType := heightData.DataType;
  heightData.Allocate(hdtSingle);
  Notify := false;
  for Y := heightData.YTop to heightData.YTop + heightData.Size - 1 do
  begin
    RasterLine := heightData.SingleRaster[Y - heightData.YTop];
    for X := heightData.XLeft to heightData.XLeft + heightData.Size - 1 do
    begin
      if (Y mod FRawStep = 0) and (X mod FRawStep = 0) then
      begin
        FBuffer.Seek(X + Y, soFromBeginning);
        FBuffer.Read(ZValue, 2);
        z := Round(ZValue / (256 / FMapHeightScale));
      end;

      //SendDebugFmt('z=%f from X+Y=%d',[z,FBuffer.Position - X + Y]);
      RasterLine[X - heightData.XLeft] := 76  ;//Random(200);
    end;

  end;
  if OldType <> hdtSingle then
    heightData.DataType := OldType;

  heightData.DataState := hdsReady;
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

  //SendDebugFmt('z=%f from X+Y=%d',[z,FBuffer.Position - XPos + YPos]);
  // z := 100;

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

procedure TRAWViewForm.ViewerDblClick(Sender: TObject);
var
  i: Integer;
  PickList: TGLPicklist;
  MaterialName: string;
  LibMaterial: TGLLibMaterial;
  p0, p1, raystart, rayvector: TVector;
begin
  inherited;
  CameraTarget.AbsoluteAffinePosition := v;
  (*
    //Inspector.Clear;
    if Inspector.Root.Count = 0 then

    begin
    //for i := 0 to FHeightfields.Count - 1 do
    i := 28;
    begin
    MaterialName := FHeightfields[i].Material.LibMaterialName;
    if MaterialName <> EmptyStr then
    begin
    LibMaterial := GLMaterialLibrary.Materials.GetLibMaterialByName(MaterialName);
    Inspector.AddComponent(LibMaterial, ExtractFileName(MaterialName));
    end;
    end;
    end;
  *)
  (*
    p0:=viewer.Buffer.ScreenToWorld(vectormake(m.x,viewer.height-m.y,0));
    p1:=viewer.Buffer.ScreenToWorld(vectormake(m.x,viewer.height-m.y,1));
    raystart:=p0;
    rayvector:=vectornormalize(vectorsubtract(p1,p0));

    for i := 0 to FHeightfields.Count - 1 do
    begin
    if FHeightfields[i].RayCastIntersect(raystart, rayvector) then
    begin
    Inspector.Clear;
    Inspector.AddComponent(FHeightfields[i], 'aa');
    end;
    end;
  *)
  (*
    PickList := TGLPicklist.Create;
    Scene.CurrentBuffer.PickObjects(Rect(M.x, M.y, M.x, M.y), PickList, 1);
    for i := 0 to PickList.Count - 1 do
    begin
    if PickList[i] is TGLHeightField then
    begin
    MaterialName := (PickList[i] as TGLHeightField).Material.LibMaterialName;
    if MaterialName <> EmptyStr then
    begin
    LibMaterial := GLMaterialLibrary.Materials.GetLibMaterialByName(MaterialName);
    Inspector.AddComponent(PickList[i], ExtractFileName(MaterialName));
    end;
    end;
    end;
    PickList.Free;
  *)
end;

procedure TRAWViewForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  M.X := X;
  M.Y := Y;
end;

procedure TRAWViewForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Coo3D: TVector3f;
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
    // v := HeightFieldBase.AbsoluteToLocal(v);

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

procedure THFList.SortByTag;
begin
  Sort(THFComparer.Default);
end;

{ THFComparer }

function THFComparer.Compare(const Left, Right: TGLHeightField): Integer;
begin
  if (Left <> nil) and (Right <> nil) then
    result := CompareValue(Left.Tag, Right.Tag)
  else
    result := -1;
end;

end.
