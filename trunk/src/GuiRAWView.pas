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
  GLCadencer,
  GLRenderContextInfo;

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
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private
    v: TAffineVector;
    M: TPoint;

    FFirstRow : Integer;
    FFirstCol : Integer;
    FLastRow : Integer;
    FLastCol : Integer;

    // Terrain ranges
    FRangeMin : TPoint;
    FRangeMax : TPoint;

    FInitLoad: boolean;
    FMouseMoveMutex: boolean;
    FBuffer: TMemoryStream;
    FMapSize: Integer;
    FWorldSize: Integer;
    FRawStep: Integer;
    FMapHeightScale: Single;

    FMinZ: Single;
    FMaxZ: Single;
    FWireFrame: boolean;
    FRestrict: boolean;
    { Déclarations privées }

    procedure SetMapSize(const Value: Integer);
    procedure SetMapHeightScale(const Value: Single);
    procedure SetWorldSize(const Value: Integer);

    procedure LoadTerrain(Data: TStream); overload;
    procedure LoadHeightmap(Data: TStream); overload;
    procedure CalcTerrainRange;
    procedure CalcCameraPosition;

    procedure InvalidateTerrain;
    procedure SetWireFrame(const Value: boolean);
    procedure SetRestrict(const Value: boolean);
  public
    { Déclarations publiques }
    GetFileByPath: TBgaGetFileByPath;

    TexturePart: Integer;
    TextureSize: Integer;
    TextureOffset : TPoint;
    TextureBaseName: string;
    DetailBaseName: string;
    HeightMap: string;
    WaterLevel : single;
    UseTexture : boolean;

    procedure LoadTerrain(Filename: string); overload;
    property MapSize: Integer read FMapSize write SetMapSize;
    property MapHeightScale: Single read FMapHeightScale write SetMapHeightScale;
    property WorldSize: Integer read FWorldSize write SetWorldSize;

    property WireFrame : boolean read FWireFrame write SetWireFrame;
    property Restrict : boolean read FRestrict write SetRestrict;
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
  CommonLib,
  CONLib;

{ TRAWViewForm }



procedure TRAWViewForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  Cadencer.Enabled := False;
end;

procedure TRAWViewForm.FormCreate(Sender: TObject);
var
  Key : Char;
  Color : TColorVector;
begin
  inherited;
  FMapSize := -1;
  FBuffer := TMemoryStream.Create;

  TerrainRenderer.TileManagement := [tmClearUsedFlags, tmMarkUsedTiles, tmReleaseUnusedTiles, tmAllocateNewTiles, tmWaitForPreparing];
  BattlefieldHDS.MaxPoolSize:= 256*1024*1024;
  SendDebugFmt('HDS size is fixed to %s',[SizeToStr(BattlefieldHDS.MaxPoolSize)]);
(*
  Color[0] := 0.832;
  Color[1] := 0.699;
  Color[2] := 0.52;
  Color[3] := 1;
  Viewer.Buffer.BackgroundColor :=  ConvertColorVector(Color); //0.832/0.699/0.52
*)
  Key := #0;
  FormKeyPress(Self, Key);
end;

procedure TRAWViewForm.FormActivate(Sender: TObject);
begin
  inherited;
  if Cadencer.Tag = 1 then
  begin
    Cadencer.Tag := 0;
    Cadencer.Enabled := true;
  end;
end;

procedure TRAWViewForm.FormDeactivate(Sender: TObject);
begin
  inherited;
  if Cadencer.Enabled then
  begin
    Cadencer.Tag := 1;
    Cadencer.Enabled := False;
  end;
end;

procedure TRAWViewForm.FormDestroy(Sender: TObject);
begin
  FBuffer.Free;
end;

procedure TRAWViewForm.FormKeyPress(Sender: TObject; var Key: Char);
var
  List : TStringList;
  i :integer;
  PolygonMode, QualityStyle : byte;
  MaterialOptions : TMaterialOptions;
begin
  inherited;
   case Key of
      'I','i' : with TerrainRenderer do
         if TileSize > 8 then TileSize := TileSize div 2;

      'U','u' : with TerrainRenderer do
         if TileSize < 256 then TileSize := TileSize*2;

      'O','o' : with TerrainRenderer do
         if CLODPrecision = 0 then CLODPrecision := 1
         else
         if CLODPrecision>0 then CLODPrecision:=Round(CLODPrecision*1.2);

      'P','p' : with TerrainRenderer do
         if CLODPrecision<1000 then CLODPrecision:=Round(CLODPrecision*0.8);

      'L','l' : with TerrainRenderer do
         if QualityDistance>3 then QualityDistance:=Round(QualityDistance*0.8)
         else
          QualityDistance := 3;

      'M','m' : with TerrainRenderer do
         if QualityDistance<1000 then QualityDistance:=Round(QualityDistance*1.2);

      'K','k' : with Camera do
         if DepthOfView < 10000 then DepthOfView:=Round(DepthOfView*1.2);

      'J','j' : with Camera do
         if DepthOfView > 10 then DepthOfView:=Round(DepthOfView/1.2);

      'W','w' : Wireframe := not WireFrame;

      'X','x' :
      for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
      begin
        MaterialOptions := GLMaterialLibrary.Materials[i].Material.MaterialOptions;
        if moNoLighting in MaterialOptions then
          Exclude(MaterialOptions, moNoLighting)
        else
          Include(MaterialOptions, moNoLighting);

        GLMaterialLibrary.Materials[i].Material.MaterialOptions := MaterialOptions;
        TerrainRenderer.Material.MaterialOptions := MaterialOptions;
      end;

      'C','c' :
      begin
        QualityStyle := Byte(TerrainRenderer.QualityStyle);
        QualityStyle := (QualityStyle+1) mod 2;
        TerrainRenderer.QualityStyle := TTerrainHighResStyle(QualityStyle);
        BattlefieldHDS.MarkDirty;
      end;

      'V','v' :
      begin
        UseTexture := not UseTexture;
        BattlefieldHDS.MarkDirty;
      end;

      'B','b' :
      begin
        Restrict := not Restrict;
      end;
   end;

   Viewer.Buffer.FogEnable := True;
   Viewer.Buffer.FogEnvironment.FogColor.AsWinColor :=  Viewer.Buffer.BackgroundColor;
   Viewer.Buffer.FogEnvironment.FogStart := TerrainRenderer.QualityDistance;
   Viewer.Buffer.FogEnvironment.FogEnd := Camera.DepthOfView;


   List := TStringList.Create;
   List.Add(Format('[U-][I+] TileSize = %d',[TerrainRenderer.TileSize]));
   List.Add(Format('[J-][K+] DepthOfView = %f',[Camera.DepthOfView]));
   List.Add(Format('[L-][M+] QualityDistance = %f',[TerrainRenderer.QualityDistance]));

   if TerrainRenderer.QualityStyle = hrsTesselated then
    List.Add(Format('[O-][P+] Tesselation = %d',[TerrainRenderer.CLODPrecision]));

   List.Add('');
   List.Add('[W] Wireframe/Polygon');
   List.Add('[X] Lighting ON/OFF');
   List.Add('[C] Tesselation Enabled/Disabled');
   List.Add('[V] Textures ON/OFF');
   List.Add('[B] Terrain restriction ON/OFF');

   GLInfos.Text := List.Text;
   List.free;
end;



procedure TRAWViewForm.LoadTerrain(Data: TStream);
var
  TxtData: TStringList;
  flworldSize, flmaterialSize, flwaterLevel, flseaFloorLevel: extended;
  flyScale: extended;
  intTexOffsetX, intTexOffsetY : integer;
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

    intTexOffsetX := GetIntFromProperty(TxtData, 'GeometryTemplate.texOffsetX');
    intTexOffsetY := GetIntFromProperty(TxtData, 'GeometryTemplate.texOffsetY');

    HeightMap := strFile + '.raw';
    TextureBaseName := strtexBaseName;
    DetailBaseName := strdetailTexName + '.dds';

    MapSize := Round(flmaterialSize);
    MapHeightScale := flyScale;
    WorldSize := Round(flworldSize);
    WaterLevel := flwaterLevel;
    WaterPlane.Position.z := WaterLevel;
    FRawStep := FWorldSize div MapSize;

    TextureSize := WorldSize * 4;
    TexturePart := TextureSize div 256;

    TextureOffset.X := intTexOffsetX;
    TextureOffset.Y := intTexOffsetY;
  finally
    TxtData.Free;
  end;


end;

{.$DEFINE RAWVIEW_DRAW_NAME}

procedure TRAWViewForm.LoadTerrain(Filename: string);
var
  Stream: TFileStream;
  TextureName, TextureFile, DetailFile, HeightMapFile: string;
  Row, Col: Integer;
  LibMaterial, DetailMaterial: TGLLibMaterial;
{$IFDEF RAWVIEW_DRAW_NAME}
  Bmp: TBitmap;
{$ENDIF}
begin
  Assert(Assigned(GetFileByPath));
  Cadencer.Enabled := false;
  GLMaterialLibrary.Materials.Clear;
  LibMaterial := nil;
  DetailMaterial := nil;

  FWireFrame := false;
  FRestrict := True;
  UseTexture := True;

  FFirstRow := -1;
  FFirstCol := -1;
  FLastRow := -1;
  FLastCol := -1;

  if FileExists(Filename) then
  begin
    Title := Filename;
    Stream := TFileStream.Create(Filename, fmOpenRead);
    LoadTerrain(Stream);
    Stream.Free;

    DetailFile := GetFileByPath(Self, DetailBaseName);
    if FileExists(DetailFile) then
    begin
      DetailMaterial := GLMaterialLibrary.AddTextureMaterial('Details', DetailFile);
      DetailMaterial.Material.MaterialOptions := [moNoLighting];
      DetailMaterial.Material.Texture.ImageBrightness := 2;
      DetailMaterial.Material.Texture.TextureMode := tmModulate;
      DetailMaterial.TextureScale.X := Sqrt(MapSize);
      DetailMaterial.TextureScale.Y := Sqrt(MapSize);
    end;

    for Row := 0 to TexturePart - 1 do
      for Col := 0 to TexturePart - 1 do
      begin
        TextureName := Format('%s%.2dx%.2d.dds', [TextureBaseName, Col, Row]);
        // SendDebug(TextureName);
        TextureFile := GetFileByPath(Self, TextureName);

        if FileExists(TextureFile) then
        begin
          if FFirstRow = -1 then
            FFirstRow := Row + TextureOffset.X
          else
            FLastRow := Row + TextureOffset.X;

          if FFirstCol = -1 then
            FFirstCol := Col + TextureOffset.Y
          else
            FLastCol := Col + TextureOffset.Y;

          UseTexture := true;
          LibMaterial := GLMaterialLibrary.AddTextureMaterial(TextureName, TextureFile);
          LibMaterial.Material.MaterialOptions := [moNoLighting];

          if Assigned(DetailMaterial) then
            LibMaterial.Texture2Name := DetailMaterial.Name;

{$IFDEF RAWVIEW_DRAW_NAME}
          Bmp := TBitmap.Create;
          Bmp.PixelFormat := pf24bit;
          LibMaterial.Material.Texture.Image.AssignToBitmap(Bmp);
          with Bmp.Canvas do
          begin
            Font.Size := 24;
            Font.color := clWhite;
            Brush.color := clBlack;
            TextOut(8, 8, SFRightRight('\', TextureName));
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
      TerrainRenderer.Scale.X := FRawStep;
      TerrainRenderer.Scale.Y := FRawStep;
      TerrainRenderer.Scale.Z := FRawStep;
      Stream.Free;
    end;

    InvalidateTerrain;
    CalcCameraPosition;
    Cadencer.Enabled := true;
  end;
end;



procedure TRAWViewForm.InvalidateTerrain;
begin
  CalcTerrainRange;
  BattlefieldHDS.MarkDirty;
end;

procedure TRAWViewForm.LoadHeightmap(Data: TStream);
begin
  FMinZ := MAXWORD;
  FMaxZ := 0;
  Data.Position := 0;
  FBuffer.LoadFromStream(Data);
  FInitLoad := true;
end;

procedure TRAWViewForm.CalcCameraPosition;
begin
  CameraTarget.Position.X := WaterPlane.Position.X;
  CameraTarget.Position.Z := WaterPlane.Position.Y;
  CameraTarget.Position.Y := WaterLevel;

  Camera.Position.X := 0;
  Camera.Position.Z := -100;
  Camera.Position.Y := WaterLevel*2;
end;

procedure TRAWViewForm.CalcTerrainRange;
var
  TileSize : Integer;
  Rate : Integer;
begin

  // Init with maximum values
  FRangeMin.X := 0;
  FRangeMin.Y := 0;
  FRangeMax.X := (FWorldSize div FRawStep);
  FRangeMax.Y := (FWorldSize div FRawStep);

  if FRestrict then
  begin
    TileSize := FWorldSize div TexturePart;
    Rate := Round(Sqrt(TileSize));

    FRangeMin.X := (FWorldSize div FRawStep) * (FFirstRow) div Rate;
    FRangeMin.Y := (FWorldSize div FRawStep) * (FFirstCol) div Rate;

    if FRangeMin.X > 0 then
      FRangeMin.X := FRangeMin.X -1;
    if FRangeMin.Y > 0 then
      FRangeMin.Y := FRangeMin.Y -1;

    FRangeMax.X := (FWorldSize div FRawStep) * (FLastRow)  div Rate + TileSize;
    FRangeMax.Y := (FWorldSize div FRawStep) * (FLastCol)  div Rate + TileSize;

    if FRangeMax.X > 0 then
      FRangeMax.X := FRangeMax.X -1;
    if FRangeMax.Y > 0 then
      FRangeMax.Y := FRangeMax.Y -1;
  end;

  // Set water plane position and size

  WaterPlane.Width := (FRangeMax.X - FRangeMin.X)*FRawStep;
  WaterPlane.Height := (FRangeMax.Y - FRangeMin.Y)*FRawStep;

  WaterPlane.Position.X := FRangeMin.X*FRawStep + WaterPlane.Width/2;
  WaterPlane.Position.Y := FRangeMin.Y*FRawStep + WaterPlane.Width/2;
end;



procedure TRAWViewForm.BattlefieldHDSStartPreparingData(heightData: THeightData);
type
  PRasterArray = PSingleArray;
const
  HdsType = hdtSingle;
var
  Y: Integer;
  X: Integer;
  Z: Single;
  RawValue: Word;
  RasterLine: PRasterArray;
  Position: Integer;
  i, j, n: Integer;
  offset: TTexPoint;

  MaxTileSize, OffsetModulo, TileSize: Integer;
  TextureScale: Extended;
begin

  if not InRange(heightData.YTop, FRangeMin.Y, FRangeMax.Y)
  or not InRange(heightData.XLeft, FRangeMin.X, FRangeMax.X) then
  begin
    heightData.DataState := hdsNone;
    Exit;
  end;

  heightData.DataState := hdsPreparing;
  heightData.Allocate(HdsType);

  MaxTileSize := FWorldSize div TexturePart;
  OffsetModulo := Sqr(TexturePart);
  TileSize := TerrainRenderer.TileSize;
  TextureScale := TerrainRenderer.TileSize / MaxTileSize;

  i := (heightData.XLeft div MaxTileSize);
  j := (heightData.YTop div MaxTileSize);
  if (i < OffsetModulo) and (j < OffsetModulo) then
  begin
    if UseTexture then
    begin
      heightData.MaterialName := Format('%s%.2dx%.2d.dds', [TextureBaseName, i-TextureOffset.X, j-TextureOffset.Y]);

      // Auto remove material if it doesn't exists
      if GLMaterialLibrary.Materials.GetLibMaterialByName(heightData.MaterialName) = nil then
        heightData.MaterialName := '';
    end;

    heightData.TextureCoordinatesMode := tcmLocal;
    n := (heightData.XLeft div TileSize) mod OffsetModulo;
    offset.S := n * TextureScale;
    n := (heightData.YTop div TileSize) mod OffsetModulo;
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
        Position := X*2 + Y*2 * (FWorldSize div FRawStep);
        FBuffer.Seek(Position, soFromBeginning);
        FBuffer.Read(RawValue, 2);

        Z := RawValue * (FMapHeightScale/2) * (1/FRawStep);
      end
      else
        Z := 0;

      RasterLine[X - heightData.XLeft] := Z;
    end;
  end;
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

procedure TRAWViewForm.SetWireFrame(const Value: boolean);
var
  i :integer;
  PolygonMode : TPolygonMode;
begin
  FWireFrame := Value;

  if FWireFrame then
    PolygonMode := pmLines
  else
    PolygonMode := pmFill;

  for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
  begin
    GLMaterialLibrary.Materials[i].Material.PolygonMode := PolygonMode;
    TerrainRenderer.Material.PolygonMode := TPolygonMode(PolygonMode);
  end;
end;

procedure TRAWViewForm.SetWorldSize(const Value: Integer);
begin
  FWorldSize := Value;
end;

procedure TRAWViewForm.SetMapSize(const Value: Integer);
begin
  FMapSize := Value;
end;

procedure TRAWViewForm.SetRestrict(const Value: boolean);
begin
  FRestrict := Value;
  InvalidateTerrain;
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
    YLabel.Caption := Format('Y=%.2f', [v[1]]);
    ZLabel.Caption := Format('Z=%.2f', [v[2]]);
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
