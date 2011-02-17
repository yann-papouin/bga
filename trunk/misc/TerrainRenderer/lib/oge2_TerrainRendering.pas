unit oge2_TerrainRendering;

interface

uses
  Classes,
  GLScene,
  GLTexture,
  GLMaterial,
  GLRenderContextInfo,
  VectorLists,
  VectorGeometry,
  oge2_TerrainDataSource,
  oge2_HeightMap,
  //oge2_TerrainEngine,
  oge2_TerrainTileRender;

type
  TTerrainLODType = (tlodNone, tlodIllyrium, tlodIllyriumVBO);
  TTerrainOcclusionTesselate = (totTesselateAlways, totTesselateIfVisible);

type
  TOGETerrainRendering = class(TGLSceneObject)
  private
    FHeightDataSource: TOGEHeightDataSource;
    FDrawTextured: Boolean;
    FDrawWireFrame: Boolean;
    FinvTileSize: Single;
    FTileSize: Integer;
    FMaxCLODTriangles, FCLODPrecision: Integer;
    //FTerrainEngine: TOGETerrainEngine;

    FQualityDistance: Integer;
    FLastTriangleCount: Integer;
    FMaterialLibrary: TGLMaterialLibrary;
    procedure SetHeightDataSource(const Value: TOGEHeightDataSource);
    procedure SetDrawTextured(const Value: Boolean);
    procedure SetDrawWireframe(const Value: Boolean);
    procedure SetCLODPrecision(const val: Integer);
    procedure RebuildCLOD;
    function GetPreparedPatch(const tilePos, eyePos: TAffineVector;
      texFactor: Single): TOGEBaseHeightMapRender;
  public
    lodType: TTerrainLODType;
    QualityDistance: Integer;
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure BuildList(var rci: TRenderContextInfo); override;
    function RayCastIntersect(const rayStart, rayVector: TVector;
      intersectPoint: PVector = nil;
      intersectNormal: PVector = nil): Boolean; override;
    function InterpolatedHeight(const p: TVector): Single; overload;
    function InterpolatedHeight(const p: TAffineVector): Single; overload;
  published
    property TileSize: Integer read FTileSize;
    property HeightDataSource
      : TOGEHeightDataSource read FHeightDataSource
      write SetHeightDataSource;
    property DrawTextured: Boolean read FDrawTextured write SetDrawTextured;
    property DrawWireframe: Boolean read FDrawWireFrame write SetDrawWireframe;
    property MaxCLODTriangles
      : Integer read FMaxCLODTriangles write FMaxCLODTriangles default
      65536;
    property MaterialLibrary
      : TGLMaterialLibrary read FMaterialLibrary write FMaterialLibrary;
    // property OcclusionTesselate:TTerrainOcclusionTesselate read FOcclusionTesselate write FOcclusionTesselate default totTesselateIfVisible;
    { : Precision of CLOD tiles.<p>
      The lower the value, the higher the precision and triangle count.
      Large values will result in coarse terrain.<br>
      high-resolution tiles (closer than QualityDistance) ignore this setting. }
    property CLODPrecision
      : Integer read FCLODPrecision write SetCLODPrecision
      default 100;

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
  FTileSize := 64;
  FinvTileSize := 1 / FTileSize;
  FMaxCLODTriangles := 65536;
  FCLODPrecision := 100;
  // FTerrainEngine:=TOGETerrainROAM1Engine.Create;
  FQualityDistance := 64;
  { FBufferVertices:=TAffineVectorList.Create;
    FBufferTexPoints:=TTexPointList.Create;
    FBufferVertexIndices:=TIntegerList.Create;
    FOcclusionTesselate:=totTesselateIfVisible; }
  ObjectStyle := ObjectStyle + [osDirectDraw];
  lodType := tlodIllyriumVBO;
end;

destructor TOGETerrainRendering.Destroy;
begin
  { FBufferVertices.Free;
    FBufferTexPoints.Free;
    FBufferVertexIndices.Free;
    FTerrainEngine.Free; }
  inherited;
end;

function TOGETerrainRendering.GetPreparedPatch(const tilePos,
  eyePos: TAffineVector; texFactor: Single): TOGEBaseHeightMapRender;
var
  tile: TOGEHeightMap;
  patch: TOGEBaseHeightMapRender;
  xLeft, yTop: Integer;
begin
  xLeft := Round(tilePos[0] * FinvTileSize - 0.5) * TileSize;
  yTop := Round(tilePos[1] * FinvTileSize - 0.5) * TileSize;
  tile := FHeightDataSource.GetTile(xLeft, yTop);
  result := nil;
  if not assigned(tile) then
    exit;

  // application.MessageBox(pchar(inttostr(XLeft)+' '+inttostr(YTop)),'');
  // if tile.DataState=hdsNone then begin
  if tile.DataState <> dsReady then
  begin
    result := nil; // if the tile is still not hdsReady, then skip it
  end
  else
  begin
    patch := TOGEBaseHeightMapRender(tile.Renderer);
    if not assigned(patch) then
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
    result := patch;
  end;
end;

procedure TOGETerrainRendering.BuildList(var rci: TRenderContextInfo);
var
  Cache: TList;
  ii: Integer;
  hd: TOGEHeightMap;
  v: TVector3f;
  frustum: TFrustum;

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
    frustum.pRight[0] := Clip[0][3] - Clip[0][0];
    frustum.pRight[1] := Clip[1][3] - Clip[1][0];
    frustum.pRight[2] := Clip[2][3] - Clip[2][0];
    frustum.pRight[3] := Clip[3][3] - Clip[3][0];
    NormalizePlane(frustum.pRight);
    // LEFT plane
    frustum.pLeft[0] := Clip[0][3] + Clip[0][0];
    frustum.pLeft[1] := Clip[1][3] + Clip[1][0];
    frustum.pLeft[2] := Clip[2][3] + Clip[2][0];
    frustum.pLeft[3] := Clip[3][3] + Clip[3][0];
    NormalizePlane(frustum.pLeft);
    // BOTTOM plane
    frustum.pBottom[0] := Clip[0][3] + Clip[0][1];
    frustum.pBottom[1] := Clip[1][3] + Clip[1][1];
    frustum.pBottom[2] := Clip[2][3] + Clip[2][1];
    frustum.pBottom[3] := Clip[3][3] + Clip[3][1];
    NormalizePlane(frustum.pBottom);
    // TOP plane
    frustum.pTop[0] := Clip[0][3] - Clip[0][1];
    frustum.pTop[1] := Clip[1][3] - Clip[1][1];
    frustum.pTop[2] := Clip[2][3] - Clip[2][1];
    frustum.pTop[3] := Clip[3][3] - Clip[3][1];
    NormalizePlane(frustum.pTop);
    // FAR plane
    frustum.pFar[0] := Clip[0][3] - Clip[0][2];
    frustum.pFar[1] := Clip[1][3] - Clip[1][2];
    frustum.pFar[2] := Clip[2][3] - Clip[2][2];
    frustum.pFar[3] := Clip[3][3] - Clip[3][2];
    NormalizePlane(frustum.pFar);
    // NEAR plane
    frustum.pNear[0] := Clip[0][3] + Clip[0][2];
    frustum.pNear[1] := Clip[1][3] + Clip[1][2];
    frustum.pNear[2] := Clip[2][3] + Clip[2][2];
    frustum.pNear[3] := Clip[3][3] + Clip[3][2];
    NormalizePlane(frustum.pNear);
  end;

var
  Temph, TempW: Single;
  //frustum2: TFrustumSOAR;
var
  vEye, vEyeDirection: TVector;
  tilePos, absTilePos, observer: TAffineVector;
  deltaX, nbX, iX: Integer;
  deltaY, nbY, iY: Integer;
  rpIdxDelta, accumCount: Integer;
  f, tileRadius, tileGroundRadius, texFactor, tileDist, qDist: Single;
  rcci: TRenderContextClippingInfo;
  currentMaterialName: String;
  maxTilePosX, maxTilePosY, minTilePosX, minTilePosY: Single;
  t_l, t_t, t_r, t_b: Single;
  patch: TOGEBaseHeightMapRender;
  glmat: TGLLibMaterial;
  nrtiles: Integer;
begin
  nrtiles := 0;
  if csDesigning in ComponentState then
    exit;
  if FHeightDataSource = nil then
    exit;

  TempW := TGLSceneBuffer(rci.buffer).Width;
  Temph := TGLSceneBuffer(rci.buffer).Height;
  v[0] := rci.cameraPosition[0];
  v[1] := rci.cameraPosition[1];
  v[2] := rci.cameraPosition[2];

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
  vEye[0] := Round(vEye[0] * FinvTileSize - 0.5) * TileSize + TileSize * 0.5;
  vEye[1] := Round(vEye[1] * FinvTileSize - 0.5) * TileSize + TileSize * 0.5;
  tileGroundRadius := Sqr(TileSize * 0.5 * Scale.X) + Sqr
    (TileSize * 0.5 * Scale.Y);
  tileRadius := Sqrt(tileGroundRadius + Sqr(256 * Scale.Z));
  tileGroundRadius := Sqrt(tileGroundRadius);
  // now, we render a quad grid centered on eye position
  SetVector(tilePos, vEye);
  tilePos[2] := 0;
  f := (rci.rcci.farClippingDistance + tileGroundRadius) / Scale.X;
  f := Round(f * FinvTileSize + 1.0) * TileSize;
  maxTilePosX := vEye[0] + f;
  maxTilePosY := vEye[1] + f;
  minTilePosX := vEye[0] - f;
  minTilePosY := vEye[1] - f;

  if (maxTilePosX < minTilePosX) or (maxTilePosY < minTilePosY) then
    exit;

  nbX := Round((maxTilePosX - minTilePosX) / TileSize);
  nbY := Round((maxTilePosY - minTilePosY) / TileSize);
  texFactor := 1 / ( { TilesPerTexture } 1 * TileSize);
  rcci := rci.rcci;
  if QualityDistance > 0 then
    qDist := QualityDistance + tileRadius * 0.5
  else
    qDist := -1;

  AbsoluteMatrix; // makes sure it is available

  if vEyeDirection[0] >= 0 then
    deltaX := TileSize
  else
  begin
    deltaX := -TileSize;
    minTilePosX := maxTilePosX;
  end;
  if vEyeDirection[1] >= 0 then
    deltaY := TileSize
  else
  begin
    deltaY := -TileSize;
    minTilePosY := maxTilePosY;
  end;

  GL.MatrixMode(GL_MODELVIEW);
  GL.PushMatrix;
  GL.Enable(GL_COLOR_MATERIAL);
  tileRadius := tileRadius;
  tilePos[1] := minTilePosY;
  for iY := 0 to nbY - 1 do
  begin
    tilePos[0] := minTilePosX;
    for iX := 0 to nbX do
    begin
      absTilePos := VectorTransform(tilePos, DirectAbsoluteMatrix^);
      if not IsVolumeClipped(absTilePos, tileRadius, rcci.frustum) then
      begin
        patch := GetPreparedPatch(tilePos, observer, texFactor);
        tileDist := VectorDistance(PAffineVector(@rcci.origin)^, absTilePos)
          / 64;
        if assigned(patch) then
        begin
          nrtiles := nrtiles + 1;
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
                  GL.enable(GL_LINE_SMOOTH);
                  GL.color4f(1, 1, 1, 1);
                  GL.Begin_(GL_QUADS);
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(false, false);
                  GL.End_;
                end;
              end;
            tlodIllyrium:
              begin
                GL.FrontFace(GL_CW);

                glmat := MaterialLibrary.LibMaterialByName
                  (patch.HeightData.MaterialName);
                if assigned(glmat) then
                begin
                  glmat.Apply(rci);
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(FDrawTextured,
                    FDrawWireFrame);
                  glmat.UnApply(rci);
                end
                else
                begin
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(false,
                    FDrawWireFrame);
                end;

                GL.FrontFace(GL_CCW);
              end;
            tlodIllyriumVBO:
              begin
                GL.FrontFace(GL_CW);
                // glmat:=MaterialLibrary.Materials[0];
                glmat := MaterialLibrary.LibMaterialByName
                  (patch.HeightData.MaterialName);
                if assigned(glmat) then
                begin
                  glmat.Apply(rci);
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(FDrawTextured,
                    FDrawWireFrame);
                  glmat.UnApply(rci);
                end
                else
                begin
                  TOGEBaseHeightMapRender(patch).RenderQuadTree(false,
                    FDrawWireFrame);
                end;

                GL.FrontFace(GL_CCW);
              end;
          end;
        end;
      end;
      tilePos[0] := tilePos[0] + deltaX;
    end;
    tilePos[1] := tilePos[1] + deltaY;
  end;

  GL.Disable(GL_COLOR_MATERIAL);
  GL.PopMatrix;
  // writeln(nrtiles);
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

procedure TOGETerrainRendering.SetHeightDataSource
  (const Value: TOGEHeightDataSource);
begin
  FHeightDataSource := Value;
end;

procedure TOGETerrainRendering.SetCLODPrecision(const val: Integer);
begin
  if val <> FCLODPrecision then
  begin
    FCLODPrecision := val;
    if FCLODPrecision < 1 then
      FCLODPrecision := 1;
    RebuildCLOD;
  end;
end;

procedure TOGETerrainRendering.RebuildCLOD;
var
  i, k: Integer;
  hd: TOGEHeightMap;
begin
  // drop all ROAM data (CLOD has changed, rebuild required)
  { for i:=0 to cTilesHashSize do with FTilesHash[i] do begin
    for k:=Count-1 downto 0 do begin
    hd:=THeightData(List^[k]);
    if Assigned(hd.ObjectTag) then begin
    (hd.ObjectTag as TGLROAMPatch).Free;
    hd.ObjectTag:=nil;
    end;
    end;
    Clear;
    end; }
end;

function TOGETerrainRendering.RayCastIntersect(const rayStart,
  rayVector: TVector; intersectPoint: PVector = nil;
  intersectNormal: PVector = nil): Boolean;
var
  p1, d, p2, p3: TVector;
  step, i, h, minH, maxH, p1height: Single;
  startedAbove: Boolean;
  failSafe: Integer;
  AbsX, AbsY, AbsZ: TVector;
begin
  result := false;
  if assigned(HeightDataSource) then
  begin
    step := (Scale.X + Scale.Y); // Initial step size guess
    i := step;
    d := VectorNormalize(rayVector);
    AbsZ := VectorNormalize(LocalToAbsolute(ZHMGVector));
    startedAbove := ((InterpolatedHeight(rayStart) - VectorDotProduct(rayStart,
          AbsZ)) < 0);
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
        result := true;
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
          exit
      end
      else if h > maxH then
        exit;
    end;

    if result then
    begin
      p1 := VectorAdd(p1, VectorScale(AbsZ,
          InterpolatedHeight(p1) - VectorDotProduct(p1, AbsZ)));
      if assigned(intersectPoint) then
        intersectPoint^ := p1;

      // Calc Normal
      if assigned(intersectNormal) then
      begin
        // Get 2 nearby points for cross-product
        AbsX := VectorNormalize(LocalToAbsolute(XHMGVector));
        AbsY := VectorNormalize(LocalToAbsolute(YHMGVector));
        p2 := VectorAdd(p1, VectorScale(AbsX, 0.1));
        p2 := VectorAdd(p2, VectorScale(AbsZ,
            InterpolatedHeight(p2) - VectorDotProduct(p2, AbsZ)));
        p3 := VectorAdd(p1, VectorScale(AbsY, 0.1));
        p3 := VectorAdd(p3, VectorScale(AbsZ,
            InterpolatedHeight(p3) - VectorDotProduct(p3, AbsZ)));

        intersectNormal^ := VectorNormalize
          (VectorCrossProduct(VectorSubtract(p1, p2),
            VectorSubtract(p3, p1)));
      end;
    end;
  end;
end;

function TOGETerrainRendering.InterpolatedHeight(const p: TVector): Single;
var
  pLocal: TVector;
begin
  if assigned(FHeightDataSource) then
  begin
    pLocal := AbsoluteToLocal(p);
    result := FHeightDataSource.InterpolatedHeight(pLocal[0], pLocal[1],
      FTileSize + 1) * Scale.Z * (1 / 128);
  end
  else
    result := 0;
end;

// InterpolatedHeight (affine)
//
function TOGETerrainRendering.InterpolatedHeight(const p: TAffineVector)
  : Single;
begin
  result := InterpolatedHeight(PointMake(p));
end;

end.
