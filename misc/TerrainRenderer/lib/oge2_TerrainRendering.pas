unit oge2_TerrainRendering;

interface

uses
  Classes,
  GLScene,
  GLUtils,
  GLTexture,
  GLMaterial,
  GLRenderContextInfo,
  VectorLists,
  VectorGeometry,
  oge2_HashList,
  oge2_HeightMap,
  oge2_TerrainTileRender;

type
  TTerrainLODType = (tlodNone, tlodIllyrium, tlodIllyriumVBO);
  TTerrainOcclusionTesselate = (totTesselateAlways, totTesselateIfVisible);
  TOGEDataSourceStartPreparingData = procedure(HeightData: TOGEHeightMap) of Object;

  TOGETerrainRendering = class;

  TOGEHeightDataSource = class(TObject)
  private
    FOGETerrainRendering: TOGETerrainRendering;
    FTilesList: THashTable;
    FTilesCache: TList;
    FOnStartPreparingData: TOGEDataSourceStartPreparingData;
    Procedure BuildTilesCache;
    procedure ClearTilesCache;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MarkDirty(x1, y1, x2, y2: Integer);
    function GetTile(X, Y: Integer): TOGEHeightMap;
    procedure RemoveTile(X, Y: Integer);
    function InterpolatedHeight(X, Y: Single; tileSize: Integer): Single;
    property TilesCache: TList read FTilesCache;
    property OnStartPreparingData: TOGEDataSourceStartPreparingData read FOnStartPreparingData write FOnStartPreparingData;
  end;

  TOGETerrainRendering = class(TGLSceneObject)
  private
    FHeightDataSource: TOGEHeightDataSource;
    FDrawTextured: Boolean;
    FDrawWireFrame: Boolean;
    FinvTileSize: Extended;
    FTileSize: Integer;

    FMaterialLibrary: TGLMaterialLibrary;
    procedure SetHeightDataSource(const Value: TOGEHeightDataSource);
    procedure SetDrawTextured(const Value: Boolean);
    procedure SetDrawWireframe(const Value: Boolean);
    function GetPreparedPatch(const tilePos, eyePos: TAffineVector; texFactor: Single): TOGEBaseHeightMapRender;
    procedure SetTileSize(const Value: Integer);
  public
    lodType: TTerrainLODType;
    QualityDistance: Integer;
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    function RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean; override;
    function InterpolatedHeight(const p: TVector): Single; overload;
    function InterpolatedHeight(const p: TAffineVector): Single; overload;
  published
    property tileSize: Integer read FTileSize write SetTileSize;
    property HeightDataSource: TOGEHeightDataSource read FHeightDataSource write SetHeightDataSource;
    property DrawTextured: Boolean read FDrawTextured write SetDrawTextured;
    property DrawWireframe: Boolean read FDrawWireFrame write SetDrawWireframe;
    property MaterialLibrary: TGLMaterialLibrary read FMaterialLibrary write FMaterialLibrary;
  end;

implementation

uses
  GLContext,
  OpenGLTokens,
  VectorTypes,
  XOpengl,
  oge2_TerrainTileLodRenderer,
  oge2_TerrainTileLodVBORenderer,
  forms,
  SysUtils,
  Math;
{ TOGETerrainRendering }

constructor TOGETerrainRendering.Create(AOwner: TComponent);
begin
  inherited;
  self.ShowAxes := true;
  FDrawTextured := true;
  FDrawWireFrame := false;
  tileSize := 64;
  ObjectStyle := ObjectStyle + [osDirectDraw];
  lodType := tlodIllyriumVBO;
end;

destructor TOGETerrainRendering.Destroy;
begin
  inherited;
end;

function TOGETerrainRendering.GetPreparedPatch(const tilePos, eyePos: TAffineVector; texFactor: Single): TOGEBaseHeightMapRender;
var
  tile: TOGEHeightMap;
  patch: TOGEBaseHeightMapRender;
  xLeft, yTop: Integer;
begin
  xLeft := Round(tilePos.V[0] * FinvTileSize - 0.5) * tileSize;
  yTop := Round(tilePos.V[1] * FinvTileSize - 0.5) * tileSize;
  tile := FHeightDataSource.GetTile(xLeft, yTop);

  Result := nil;
  if not Assigned(tile) then
    Exit;

  // application.MessageBox(pchar(inttostr(XLeft)+' '+inttostr(YTop)),'');
  // if tile.DataState=hdsNone then begin
  if tile.DataState <> dsReady then
  begin
    Result := nil; // if the tile is still not hdsReady, then skip it
  end
  else
  begin
    patch := TOGEBaseHeightMapRender(tile.Renderer);
    if not Assigned(patch) then
    begin
      case lodType of
        tlodNone:
          begin
            tile.Renderer := TOGEHeightMapRender.Create(tile);
            TOGEHeightMapRender(tile.Renderer).BuildQuadTree;
          end;
        tlodIllyrium:
          begin
            tile.Renderer := TOGEHeightMapLODRender.Create(tile);
            TOGEHeightMapLODRender(tile.Renderer).BuildQuadTree;
          end;
        tlodIllyriumVBO:
          begin
            tile.Renderer := TOGEHeightMapLODVBORender.Create(tile);
            TOGEHeightMapLODVBORender(tile.Renderer).BuildQuadTree;
          end;
      end;
    end;
    Result := patch;
  end;
end;

procedure TOGETerrainRendering.BuildList(var rci: TRenderContextInfo);
var
  v: TVector3f;

  procedure ApplyMaterial(fname: String);
  begin
  end;

  procedure UpdateFrustum(var frustum: TFrustum);
  var
    View, Proj, Clip: TMatrix;
  begin
    GL.GetFloatv(GL_PROJECTION_MATRIX, @Proj);
    GL.GetFloatv(GL_MODELVIEW_MATRIX, @View);
    Clip := MatrixMultiply(View, Proj);
    // RIGHT plane
    frustum.pRight.V[0] := Clip.V[0].V[3] - Clip.V[0].V[0];
    frustum.pRight.V[1] := Clip.V[1].V[3] - Clip.V[1].V[0];
    frustum.pRight.V[2] := Clip.V[2].V[3] - Clip.V[2].V[0];
    frustum.pRight.V[3] := Clip.V[3].V[3] - Clip.V[3].V[0];
    NormalizePlane(frustum.pRight);
    // LEFT plane
    frustum.pLeft.V[0] := Clip.V[0].V[3] + Clip.V[0].V[0];
    frustum.pLeft.V[1] := Clip.V[1].V[3] + Clip.V[1].V[0];
    frustum.pLeft.V[2] := Clip.V[2].V[3] + Clip.V[2].V[0];
    frustum.pLeft.V[3] := Clip.V[3].V[3] + Clip.V[3].V[0];
    NormalizePlane(frustum.pLeft);
    // BOTTOM plane
    frustum.pBottom.V[0] := Clip.V[0].V[3] + Clip.V[0].V[1];
    frustum.pBottom.V[1] := Clip.V[1].V[3] + Clip.V[1].V[1];
    frustum.pBottom.V[2] := Clip.V[2].V[3] + Clip.V[2].V[1];
    frustum.pBottom.V[3] := Clip.V[3].V[3] + Clip.V[3].V[1];
    NormalizePlane(frustum.pBottom);
    // TOP plane
    frustum.pTop.V[0] := Clip.V[0].V[3] - Clip.V[0].V[1];
    frustum.pTop.V[1] := Clip.V[1].V[3] - Clip.V[1].V[1];
    frustum.pTop.V[2] := Clip.V[2].V[3] - Clip.V[2].V[1];
    frustum.pTop.V[3] := Clip.V[3].V[3] - Clip.V[3].V[1];
    NormalizePlane(frustum.pTop);
    // FAR plane
    frustum.pFar.V[0] := Clip.V[0].V[3] - Clip.V[0].V[2];
    frustum.pFar.V[1] := Clip.V[1].V[3] - Clip.V[1].V[2];
    frustum.pFar.V[2] := Clip.V[2].V[3] - Clip.V[2].V[2];
    frustum.pFar.V[3] := Clip.V[3].V[3] - Clip.V[3].V[2];
    NormalizePlane(frustum.pFar);
    // NEAR plane
    frustum.pNear.V[0] := Clip.V[0].V[3] + Clip.V[0].V[2];
    frustum.pNear.V[1] := Clip.V[1].V[3] + Clip.V[1].V[2];
    frustum.pNear.V[2] := Clip.V[2].V[3] + Clip.V[2].V[2];
    frustum.pNear.V[3] := Clip.V[3].V[3] + Clip.V[3].V[2];
    NormalizePlane(frustum.pNear);
  end;

var
  vEye, vEyeDirection: TVector;
  tilePos, absTilePos, observer: TAffineVector;
  deltaX, nbX, iX: Integer;
  deltaY, nbY, iY: Integer;
  f, tileRadius, tileGroundRadius, texFactor: Single;
  rcci: TRenderContextClippingInfo;
  maxTilePosX, maxTilePosY, minTilePosX, minTilePosY: Single;
  patch: TOGEBaseHeightMapRender;
  glmat: TGLLibMaterial;
begin
  if csDesigning in ComponentState then
    Exit;
  if FHeightDataSource = nil then
    Exit;

  v.V[0] := rci.cameraPosition.V[0];
  v.V[1] := rci.cameraPosition.V[1];
  v.V[2] := rci.cameraPosition.V[2];

  (* glMatrixMode(GL_PROJECTION);
    glPushMatrix;
    glLoadIdentity;
    gluPerspective(45{CullFovX},TempW / Temph, 0.1, 131072);
    UpdateFrustum(Frustum);
    Frustum2.plane[0]:=Frustum.pRight;
    Frustum2.plane[1]:=Frustum.pLeft;
    Frustum2.plane[2]:=Frustum.pBottom;
    Frustum2.plane[3]:=Frustum.pTop;
    Frustum2.plane[4]:=Frustum.pFar;
    Frustum2.plane[5]:=Frustum.pNear;
    glPopMatrix; *)

  GL.MatrixMode(GL_MODELVIEW);

  vEye := VectorTransform(rci.cameraPosition, InvAbsoluteMatrix);
  vEyeDirection := VectorTransform(rci.cameraDirection, InvAbsoluteMatrix);
  SetVector(observer, vEye);
  vEye.V[0] := Round(vEye.V[0] * FinvTileSize - 0.5) * tileSize + tileSize * 0.5;
  vEye.V[1] := Round(vEye.V[1] * FinvTileSize - 0.5) * tileSize + tileSize * 0.5;
  tileGroundRadius := Sqr(tileSize * 0.5 * Scale.X) + Sqr(tileSize * 0.5 * Scale.Y);
  tileRadius := Sqrt(tileGroundRadius + Sqr(256 * Scale.Z));
  tileGroundRadius := Sqrt(tileGroundRadius);
  // now, we render a quad grid centered on eye position
  SetVector(tilePos, vEye);
  tilePos.V[2] := 0;
  f := (rci.rcci.farClippingDistance + tileGroundRadius) / Scale.X;
  f := Round(f * FinvTileSize + 1.0) * tileSize;
  maxTilePosX := vEye.X + f;
  maxTilePosY := vEye.Y + f;
  minTilePosX := vEye.X - f;
  minTilePosY := vEye.Y - f;

  if (maxTilePosX < minTilePosX) or (maxTilePosY < minTilePosY) then
    Exit;

  nbX := Round((maxTilePosX - minTilePosX) / tileSize);
  nbY := Round((maxTilePosY - minTilePosY) / tileSize);
  texFactor := 1 / ( { TilesPerTexture } 1 * tileSize);
  rcci := rci.rcci;

  AbsoluteMatrix; // makes sure it is available

  if vEyeDirection.V[0] >= 0 then
    deltaX := tileSize
  else
  begin
    deltaX := -tileSize;
    minTilePosX := maxTilePosX;
  end;
  if vEyeDirection.V[1] >= 0 then
    deltaY := tileSize
  else
  begin
    deltaY := -tileSize;
    minTilePosY := maxTilePosY;
  end;

  GL.MatrixMode(GL_MODELVIEW);
  GL.PushMatrix;
  GL.Enable(GL_COLOR_MATERIAL);
  tileRadius := tileRadius;
  tilePos.V[1] := minTilePosY;
  for iY := 0 to nbY - 1 do
  begin
    tilePos.V[0] := minTilePosX;
    for iX := 0 to nbX do
    begin
      absTilePos := VectorTransform(tilePos, DirectAbsoluteMatrix^);
      if not IsVolumeClipped(absTilePos, tileRadius, rcci.frustum) then
      begin
        patch := GetPreparedPatch(tilePos, observer, texFactor);
        if Assigned(patch) then
        begin
          GL.color3f(0, 0, 0);
          case lodType of
            tlodNone:
              begin
                if FDrawTextured then
                begin
                  GL.PolygonMode(GL_FRONT, GL_Fill);
                  GL.Begin_(GL_QUADS);
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(true, true);
                  GL.End_;
                end;
                if FDrawWireFrame then
                begin
                  GL.PolygonMode(GL_FRONT_AND_BACK, GL_LINE);
                  GL.Enable(GL_LINE_SMOOTH);
                  GL.color4f(1, 1, 1, 1);
                  GL.Begin_(GL_QUADS);
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(false, false);
                  GL.End_;
                end;
              end;
            tlodIllyrium:
              begin
                GL.FrontFace(GL_CW);

                glmat := MaterialLibrary.LibMaterialByName(patch.HeightData.MaterialName);
                if Assigned(glmat) then
                begin
                  glmat.Apply(rci);
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(FDrawTextured, FDrawWireFrame);
                  glmat.UnApply(rci);
                end
                else
                begin

                  TOGEBaseHeightMapRender(patch).RenderQuadTree(false, FDrawWireFrame);
                end;

                GL.FrontFace(GL_CCW);
              end;
            tlodIllyriumVBO:
              begin
                GL.FrontFace(GL_CW);
                glmat := MaterialLibrary.LibMaterialByName(patch.HeightData.MaterialName);
                if Assigned(glmat) then
                begin
                  glmat.Apply(rci);
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(FDrawTextured, FDrawWireFrame);
                  glmat.UnApply(rci);
                end
                else
                begin
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(false, FDrawWireFrame);
                end;

                GL.FrontFace(GL_CCW);
              end;
          end;
        end;
      end;
      tilePos.V[0] := tilePos.V[0] + deltaX;
    end;
    tilePos.V[1] := tilePos.V[1] + deltaY;
  end;

  GL.Disable(GL_COLOR_MATERIAL);
  GL.PopMatrix;

end;

procedure TOGETerrainRendering.SetDrawTextured(const Value: Boolean);
begin
  if FDrawTextured <> Value then
  begin
    FDrawTextured := Value;
    StructureChanged;
  end;
end;

procedure TOGETerrainRendering.SetDrawWireframe(const Value: Boolean);
begin
  if FDrawWireFrame <> Value then
  begin
    FDrawWireFrame := Value;
    StructureChanged;
  end;
end;

procedure TOGETerrainRendering.SetHeightDataSource(const Value: TOGEHeightDataSource);
begin
  FHeightDataSource := Value;
  FHeightDataSource.FOGETerrainRendering := self;
end;

procedure TOGETerrainRendering.SetTileSize(const Value: Integer);
begin
  if Value <> FTileSize then
  begin
    if Value < 8 then
      FTileSize := 8
    else
      FTileSize := RoundUpToPowerOf2(Value);
    FinvTileSize := 1 / FTileSize;
    // ReleaseAllTiles;
    StructureChanged;
  end;
end;

function TOGETerrainRendering.RayCastIntersect(const rayStart, rayVector: TVector; intersectPoint: PVector = nil; intersectNormal: PVector = nil): Boolean;
var
  p1, d, p2, p3: TVector;
  step, i, h, minH, maxH, p1height: Single;
  startedAbove: Boolean;
  failSafe: Integer;
  AbsX, AbsY, AbsZ: TVector;
begin
  Result := false;
  if Assigned(HeightDataSource) then
  begin
    step := (Scale.X + Scale.Y); // Initial step size guess
    i := step;
    d := VectorNormalize(rayVector);
    AbsZ := VectorNormalize(LocalToAbsolute(ZHMGVector));
    startedAbove := ((InterpolatedHeight(rayStart) - VectorDotProduct(rayStart, AbsZ)) < 0);
    maxH := Scale.Z * 256;
    minH := -Scale.Z * 256;
    failSafe := 0;
    while true do
    begin
      p1 := VectorCombine(rayStart, d, 1, i);
      h := InterpolatedHeight(p1);
      p1height := VectorDotProduct(AbsZ, p1);
      if Abs(h - p1height) < 0.1 then
      begin // Need a tolerance variable here (how close is good enough?)
        Result := true;
        Break;
      end
      else
      begin
        if startedAbove then
        begin
          if h < p1height then
            i := i + step;
          if (h - p1height) > 0 then
          begin
            step := step * 0.5;
            i := i - step;
          end;
        end
        else
        begin
          if h > p1height then
            i := i + step;
        end;
      end;
      Inc(failSafe);
      if failSafe > 1024 then
        Break;
      if VectorDotProduct(AbsZ, d) < 0 then
      begin
        if h < minH then
          Exit
      end
      else if h > maxH then
        Exit;
    end;

    if Result then
    begin
      p1 := VectorAdd(p1, VectorScale(AbsZ, InterpolatedHeight(p1) - VectorDotProduct(p1, AbsZ)));
      if Assigned(intersectPoint) then
        intersectPoint^ := p1;

      // Calc Normal
      if Assigned(intersectNormal) then
      begin
        // Get 2 nearby points for cross-product
        AbsX := VectorNormalize(LocalToAbsolute(XHMGVector));
        AbsY := VectorNormalize(LocalToAbsolute(YHMGVector));
        p2 := VectorAdd(p1, VectorScale(AbsX, 0.1));
        p2 := VectorAdd(p2, VectorScale(AbsZ, InterpolatedHeight(p2) - VectorDotProduct(p2, AbsZ)));
        p3 := VectorAdd(p1, VectorScale(AbsY, 0.1));
        p3 := VectorAdd(p3, VectorScale(AbsZ, InterpolatedHeight(p3) - VectorDotProduct(p3, AbsZ)));

        intersectNormal^ := VectorNormalize(VectorCrossProduct(VectorSubtract(p1, p2), VectorSubtract(p3, p1)));
      end;
    end;
  end;
end;

function TOGETerrainRendering.InterpolatedHeight(const p: TVector): Single;
var
  pLocal: TVector;
begin

  if Assigned(FHeightDataSource) then
  begin
    pLocal := AbsoluteToLocal(p);
    Result := FHeightDataSource.InterpolatedHeight(pLocal.V[0], pLocal.V[1], FTileSize + 1) * Scale.Z * (1 / 128);
  end
  else
    Result := 0;
end;

// InterpolatedHeight (affine)
//
function TOGETerrainRendering.InterpolatedHeight(const p: TAffineVector): Single;
begin
  Result := InterpolatedHeight(PointMake(p));
end;

{ TOGEHeightDataSource }

function GetTileIndex(X, Y: Integer): String;
begin
  Result := 'T_' + inttostr(X) + '_' + inttostr(Y);
end;

constructor TOGEHeightDataSource.Create;
begin
  FTilesList := THashTable.Create;
  FTilesCache := TList.Create;

end;

destructor TOGEHeightDataSource.Destroy;
begin
  FTilesList.Free;
  FTilesCache.Free;
  inherited;
end;

procedure TOGEHeightDataSource.ClearTilesCache;
begin
  FTilesCache.Clear
end;

procedure TOGEHeightDataSource.BuildTilesCache;
var
  ii: Integer;
begin
  ClearTilesCache;
  for ii := 0 to FTilesList.Count - 1 do
  begin
    FTilesCache.Add(FTilesList.Items[ii]);
  end;
end;

procedure TOGEHeightDataSource.MarkDirty(x1, y1, x2, y2: Integer);
var
  i, j: Integer;
  TempTile: TOGEHeightMap;
  tileSize: Integer;
begin
  tileSize := FOGETerrainRendering.tileSize;
  for i := floor(x1 / tileSize) to Ceil(x2 / tileSize) - 1 do
  begin
    for j := floor(y1 / tileSize) to Ceil(y2 / tileSize) - 1 do
    begin
      TempTile := GetTile(i * tileSize, j * tileSize);
      if Assigned(TempTile.Renderer) then
      begin
        TempTile.Renderer.Free;
        TempTile.Renderer := nil;
      end;
      if Assigned(FOnStartPreparingData) then
        FOnStartPreparingData(TempTile);
      if TempTile.DataState <> dsReady then
        RemoveTile(i * tileSize, j * tileSize);
    end;
  end;
  BuildTilesCache;
end;

function TOGEHeightDataSource.InterpolatedHeight(X, Y: Single; tileSize: Integer): Single;
var
  i: Integer;
  hd, foundHd: TOGEHeightMap;
begin
  try
    // first, lookup data list to find if aHeightData contains our point
    foundHd := nil;
    for i := 0 to FTilesList.Count - 1 do
    begin
      hd := TOGEHeightMap(FTilesList.Items[i]);
      if (hd.xLeft <= X) and (hd.yTop <= Y) and (hd.xLeft + hd.Size - 1 > X) and (hd.yTop + hd.Size - 1 > Y) then
      begin
        foundHd := hd;
        Break;
      end;
    end;
  finally
  end;
  { if (foundHd=nil) then begin
    // not found, request one... slowest mode (should be avoided)
    if tileSize>1 then
    foundHd:=GetData(Round(x/(tileSize-1)-0.5)*(tileSize-1),
    Round(y/(tileSize-1)-0.5)*(tileSize-1), tileSize, hdtDefault)
    else begin
    Result:=DefaultHeight;
    Exit;
    end;
    end else begin
    // request it using "standard" way (takes care of threads)
    foundHd:=GetData(foundHd.XLeft, foundHd.YTop, foundHd.Size, foundHd.DataType);
    end; }
  Result := 0;
  if Assigned(foundHd) then
    if foundHd.DataState = dsNone then
      Result := { DefaultHeight } 0
    else
      Result := foundHd.GetHeight(Round(X - foundHd.xLeft), Round(Y - foundHd.yTop));
end;

function TOGEHeightDataSource.GetTile(X, Y: Integer): TOGEHeightMap;
var
  TempTile: TOGEHeightMap;
begin
  if Assigned(FTilesList.Get(GetTileIndex(X, Y))) = false then
  begin
    TempTile := TOGEHeightMap.Create(X, Y, FOGETerrainRendering.tileSize + 1);
    FTilesList.Add(GetTileIndex(X, Y), TempTile);
  end
  else
  begin
    TempTile := TOGEHeightMap(FTilesList.Get(GetTileIndex(X, Y)));
  end;
  Result := TempTile;
end;

procedure TOGEHeightDataSource.RemoveTile(X, Y: Integer);
begin
  if Assigned(FTilesList.Get(GetTileIndex(X, Y))) then
  begin
    FTilesList.Delete(GetTileIndex(X, Y));
  end;
end;

end.
