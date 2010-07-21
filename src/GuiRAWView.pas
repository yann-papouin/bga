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
  StdCtrls,
  Forms,
  SpTBXEditors,
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
  TB2Item,
  DDS,
  VectorGeometry,
  VectorTypes,
  GLSimpleNavigation,
  JvExControls,
  JvInspector,
  GLVectorFileObjects,
  GLMesh,
  BGALib,
  generics.defaults,
  generics.Collections,
  GLMaterial,
  GLState,
  GLKeyboard,
  GLHeightData,
  GLTerrainRenderer,
  GLROAMPatch,
  GLBitmapFont,
  GLWindowsFont,
  GLHUDObjects,
  GLCadencer;

type


  TRAWViewForm = class(TFormCommon)
    Viewer: TGLSceneViewer;
    Scene: TGLScene;
    Camera: TGLCamera;
    Navigation: TGLSimpleNavigation;
    CamLight: TGLLightSource;
    WaterPlane: TGLPlane;
    CameraTarget: TGLDummyCube;
    StatusBar: TSpTBXStatusBar;
    ZLabel: TSpTBXLabelItem;
    YLabel: TSpTBXLabelItem;
    XLabel: TSpTBXLabelItem;
    TBSeparatorItem1: TTBSeparatorItem;
    TBSeparatorItem2: TTBSeparatorItem;
    GLMaterialLibrary: TGLMaterialLibrary;
    Root: TGLDummyCube;
    BattlefieldHDS: TGLCustomHDS;
    TerrainRenderer: TGLTerrainRenderer;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    GLInfos: TGLHUDText;
    Cadencer: TGLCadencer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerPostRender(Sender: TObject);
    procedure ViewerDblClick(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BattlefieldHDSStartPreparingData(heightData: THeightData);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
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

    { Déclarations privées }

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

procedure TRAWViewForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  Cadencer.Enabled := False;
end;

procedure TRAWViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FMapSize := -1;
  FBuffer := TMemoryStream.Create;

end;

procedure TRAWViewForm.FormDestroy(Sender: TObject);
begin
  FBuffer.Free;
end;

procedure TRAWViewForm.FormKeyPress(Sender: TObject; var Key: Char);
var
  List : TStringList;
  i :integer;
  PolygonMode : byte;
  MaterialOptions : TMaterialOptions;
begin
  inherited;
   case Key of
      'O','o' : with TerrainRenderer do
         if CLODPrecision>0 then CLODPrecision:=CLODPrecision-1;
      'P','p' : with TerrainRenderer do
         if CLODPrecision<1000 then CLODPrecision:=CLODPrecision+1;
      'L','l' : with TerrainRenderer do
         if QualityDistance>40 then QualityDistance:=Round(QualityDistance*0.8);
      'M','m' : with TerrainRenderer do
         if QualityDistance<1000 then QualityDistance:=Round(QualityDistance*1.2);

      'W','w' :
      for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
      begin
        PolygonMode := Byte(GLMaterialLibrary.Materials[i].Material.PolygonMode);
        PolygonMode := (PolygonMode+1) mod 3;
        GLMaterialLibrary.Materials[i].Material.PolygonMode := TPolygonMode(PolygonMode);
      end;

      'X','x' :
      for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
      begin
        MaterialOptions := GLMaterialLibrary.Materials[i].Material.MaterialOptions;
        if moNoLighting in MaterialOptions then
          Exclude(MaterialOptions, moNoLighting)
        else
          Include(MaterialOptions, moNoLighting);

        GLMaterialLibrary.Materials[i].Material.MaterialOptions := MaterialOptions;
      end;
   end;

   List := TStringList.Create;
   List.Add(Format('CLODPrecision = %d',[TerrainRenderer.CLODPrecision]));
   List.Add(Format('QualityDistance = %f',[TerrainRenderer.QualityDistance]));
   GLInfos.Text := List.Text;
   List.free;
end;

procedure TRAWViewForm.LoadTerrain(Filename: string);
var
  Stream: TFileStream;
  TextureName, TextureFile, DetailFile, HeightMapFile: string;
  Row, Col: Integer;
  LibMaterial: TGLLibMaterial;
{$IFDEF RAWVIEW_DRAW_NAME}
  Bmp: TBitmap;
{$ENDIF}
begin
  Cadencer.Enabled := false;
  Assert(Assigned(GetFileByPath));
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
        TextureName := Format('%s%.2dx%.2d.dds', [TextureBaseName, Col, Row]);
        // SendDebug(TextureName);
        TextureFile := GetFileByPath(Self, TextureName);

        if FileExists(TextureFile) then
        begin
          LibMaterial := GLMaterialLibrary.AddTextureMaterial(TextureName, TextureFile);
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

    Cadencer.Enabled := true;
  end;
end;

procedure TRAWViewForm.LoadTerrain(Data: TStream);
var
  TxtData: TStringList;
  flworldSize, flmaterialSize, flwaterLevel, flseaFloorLevel: extended;
  flyScale: extended;
  strFile, strtexBaseName, strdetailTexName: string;
begin

  TxtData := TStringList.Create;
  try
    Data.Position := 0;
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
  finally
    TxtData.Free;
  end;

end;

procedure TRAWViewForm.LoadHeightmap(Data: TStream);
begin
  FMinZ := MAXWORD;
  FMaxZ := 0;
  Data.Position := 0;
  FBuffer.LoadFromStream(Data);
  BattlefieldHDS.MaxPoolSize := Sqr(FWorldSize);
  FInitLoad := true;
end;

procedure TRAWViewForm.BattlefieldHDSStartPreparingData(heightData: THeightData);
type
  PRasterArray = GLHeightData.PSingleArray;
const
  HdsType = hdtSingle;
var
  Y: Integer;
  X: Integer;
  Z: Single;
  RawValue: Word;
  RasterLine: PRasterArray;
  OldType: THeightDataType;
  Notify: boolean;
  Position: Integer;
  i, j, n: Integer;
  offset: TTexPoint;

  MaxTileSize, OffsetModulo: Integer;
  TextureScale: Extended;
begin
  if not InRange(heightData.YTop, 0, FWorldSize - 1) or not InRange(heightData.XLeft, 0, FWorldSize - 1) then
  begin
    heightData.DataState := hdsNone;
    Exit;
  end;

  heightData.DataState := hdsPreparing;
  OldType := heightData.DataType;
  heightData.Allocate(HdsType);
  Notify := false;

  MaxTileSize := FWorldSize div TexturePart;
  OffsetModulo := Sqr(TexturePart);
  TextureScale := TerrainRenderer.TileSize / MaxTileSize;

  i := (heightData.XLeft div MaxTileSize);
  j := (heightData.YTop div MaxTileSize);
  if (i < OffsetModulo) and (j < OffsetModulo) then
  begin
    heightData.MaterialName := Format('%s%.2dx%.2d.dds', [TextureBaseName, i, j]);
    heightData.TextureCoordinatesMode := tcmLocal;
    n := (heightData.XLeft div TerrainRenderer.TileSize) mod OffsetModulo;
    offset.S := n * TextureScale;
    n := (heightData.YTop div TerrainRenderer.TileSize) mod OffsetModulo;
    offset.T := -n * TextureScale;
    heightData.TextureCoordinatesOffset := offset;
    TextureScale := TextureScale;// - 0.0025;
    heightData.TextureCoordinatesScale := TexPointMake(TextureScale, TextureScale);
    // heightData.HeightMin:=htfHD.HeightMin;
    // heightData.HeightMax:=htfHD.HeightMax;
  end;

  for Y := heightData.YTop to heightData.YTop + heightData.Size - 1 do
  begin
    RasterLine := heightData.SingleRaster[Y - heightData.YTop];
    for X := heightData.XLeft to heightData.XLeft + heightData.Size - 1 do
    begin
      if (Y < FWorldSize) and (X < FWorldSize) then
      begin
        Position := Round(X / FRawStep) * 2 + (Round(Y / FRawStep) * 2 * (FWorldSize div FRawStep));
        FBuffer.Seek(Position, soFromBeginning);
        FBuffer.Read(RawValue, 2);

        Z := RawValue * FMapHeightScale / 2;
      end
      else
        Z := 0;

      // SendDebugFmt('z=%f (%d) from Position=%d',[z, ZValue, Position]);
      RasterLine[X - heightData.XLeft] := Z;
    end;

  end;
  (*
    if OldType <> HdsType then
    heightData.DataType := OldType;
    *)
  heightData.DataState := hdsReady;
end;

procedure TRAWViewForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
var
   speed : Single;
begin
   // handle keypresses
   if IsKeyDown(VK_SHIFT) then
      speed:=5*deltaTime
   else speed:=deltaTime;
   with Camera.Position do
   begin
      if IsKeyDown(VK_RIGHT) then
         CameraTarget.Translate(Z*speed, 0, -X*speed);
      if IsKeyDown(VK_LEFT) then
         CameraTarget.Translate(-Z*speed, 0, X*speed);
      if IsKeyDown(VK_UP) then
         CameraTarget.Translate(-X*speed, 0, -Z*speed);
      if IsKeyDown(VK_DOWN) then
         CameraTarget.Translate(X*speed, 0, Z*speed);
   end;
end;


procedure TRAWViewForm.SetMapHeightScale(const Value: Single);
begin
  FMapHeightScale := Value;
end;

procedure TRAWViewForm.SetWorldSize(const Value: Integer);
begin
  FWorldSize := Value;

  CameraTarget.Position.X := FWorldSize div 2;
  CameraTarget.Position.Z := -FWorldSize div 2;
  CameraTarget.Position.Y := 100;

  Camera.Position.X := FWorldSize div 4;
  Camera.Position.z := -FWorldSize div 4;
  Camera.Position.Y := 120;


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
begin
  inherited;
  CameraTarget.AbsoluteAffinePosition := v;
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
    v := Scene.CurrentBuffer.PixelRayToWorld(X, Y);

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
    FInitLoad := false;
  end;
end;



end.
