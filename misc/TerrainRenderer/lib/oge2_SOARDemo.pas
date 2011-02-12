{
  glSOAR

  ObjectPascal implementation of SOAR terrain rendering algorithm using OpenGL
  by Marek Mauder (pentar@seznam.cz)

  The contents of this file are used with permission, subject to the Mozilla
  Public License Version 1.1 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  Alternatively, the contents of this file may be used under the terms of the
  GNU Lesser General Public License (the  "LGPL License"), in which case the
  provisions of the LGPL License are applicable instead of those above.
  If you wish to allow use of your version of this file only under the terms
  of the LGPL License and not to allow others to use your version of this file
  under the MPL, indicate your decision by deleting  the provisions above and
  replace  them with the notice and other provisions required by the LGPL
  License.  If you do not delete the provisions above, a recipient may use
  your version of this file under either the MPL or the LGPL License.

  For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html
}

unit oge2_SoarDemo;

interface

uses
  SysUtils,
  GLScene,
  VectorTypes,
  VectorGeometry,
  oge2_HeightMap,
  oge2_TerrainTileRender;

type
  { Terrain vertex type. }
  TVertex = record
    Pos: TVector3f;
    Error: Single;
    Radius: Single;
  end;

  PVertex = ^TVertex;

  { Triangle vertex index type. }
  TIndex = LongWord;
  PIndex = ^TIndex;

  TIndexArray = array [0 .. MaxInt div SizeOf(TIndex) - 1] of TIndex;
  PIndexArray = ^TIndexArray;

  { Terrain triangle type. }
  TTriangle = record
    I, J, K: TIndex;
  end;

  { View culling frustum type. }
  TFrustumSoar = record
    plane: Array [0 .. 5] of THmgPlane;
  end;

  { Available error metric type. }
  TErrorMetric = (emIsotropic, emAnisotropic);

  TEdgeFlag = (efPosX, efPosY, efNegX, efNegY);
  TEdgeFlags = set of TEdgeFlag;

const
  SphereUndecided = 10;
  SphereVisible = 32;
  { SphereUndecided = 64;
    SphereVisible = 127; }
  IndexBufferCapacity = 64 * 64;

Type
  TSoarTerrain = class(TOGEBaseHeightMapRender)
  private

    CurrentTerrainSize: LongInt;
    RefineViewPoint: TVector3f;
    RefineFrustum: TFrustumSoar;
    RefineEdges: TEdgeFlags;
    Kappa: Single;
    InverseKappa: Single;
    PosXEdgeMinZ, PosYEdgeMinZ, NegXEdgeMinZ, NegYEdgeMinZ: Single;
    FTerrainSize, FTerrainBlocks: Integer;
    FRenderTexture { ,FRenderWireframe } : Boolean;
    procedure AppendEdgeTriangles(Index1, Index2, VertexOffset, Index1Bottom,
      Index2Bottom: TIndex; var NumIndices: Integer;
      var Indices: array of TIndex);
    procedure CheckTriangleOnEdge(ToAppend, Head, Tail: TIndex);
  public
    { Number of LODs uf current terrain. Most detailed level is 0. }
    RefinementLevels: LongInt;
    { Current screen space error treshhold. }
    TolerancePixels: LongInt;
    { Activates/Deactivates view culling (do not refine triangle outside frustum). }
    ActiveCulling: Boolean;
    { Forces maximal possible refinement (as if tolerance is 1 pixel). }
    ForceFullRefinement: Boolean;
    { Refinement will no continue bellow this level. }
    MinLOD: LongInt;
    { Error metric used in refinemnt. }
    ErrorMetric: TErrorMetric;

    { Vertex buffer for whole terrain. }
    VertexBuffer: array of TVertex;
    NumVertices: LongInt;
    { Index buffer for current terrain triangulation. }
    IndexBuffer: array of TIndex;
    { Index and triangle count for currently refined mesh. }
    NumIndices: LongInt;
    IndexBufferSize: LongInt;
    NumTriangles: LongInt;
    { Index buffers and counts for skirts that fill cracks between blocks. }
    PosXEdgeIndices, PosYEdgeIndices, NegXEdgeIndices,
      NegYEdgeIndices: array of TIndex;
    NumPosXEdgeIndices, NumPosYEdgeIndices, NumNegXEdgeIndices,
      NumNegYEdgeIndices: LongInt;
    Constructor Create(aOwner: TOGEHeightMap); override;
    Destructor Destroy; override;

    Procedure RenderTerrain(FRenderWireframe: Boolean;
      const ViewportWidth, FovX: Single; const ViewPoint: TVector3f;
      const Frustum: TFrustumSoar);

    { Loads heightmap from image and builds vertices. }
    procedure LoadTerrain(HasWrappedEdges: Boolean);
    { Frees all terrain resources. }
    procedure FreeTerrain;
    { Precomputes error and radius for vertex. }
    procedure VertexLODCompute(I, J, DI, DJ, N, TerrainSize: LongInt);
    { Returns height of the terrain at given [X, Y] coordinates. }
    function GetTerrainZ(X, Y, OldZ: Single): Single;

    { Starts mesh refinement. }
    procedure MeshRefine(const ViewportWidth, FovX: Single;
      const Edges: TEdgeFlags; const ViewPoint: TVector3f;
      const Frustum: TFrustumSoar);
    { Starts submesh refinement. }
    function SubMeshRefine(LastIndex: PIndex; Level: LongInt;
      Tri: TTriangle): PIndex;
    { Starts submesh refinement. }
    function SubMeshRefineVisible(LastIndex: PIndex; Level: LongInt;
      Tri: TTriangle; Mask: LongWord): PIndex;

    { Calculates vertex bounding sphere visibility. }
    function IsSphereVisible(const V: TVertex; Mask: LongWord): LongWord;
    { Returns tru if vertex is active in current mesh. }
    function IsVertexActive(const V: TVertex): Boolean;

    { Begins new triangle strip. }
    function TriStripBegin(FirstIndex: TIndex; Parity: Boolean): PIndex;
    { Appends triangles to existing strip. }
    function TriStripAppend(LastIndex: PIndex; ToAppend: TIndex;
      Parity: Boolean): PIndex;
    { Finishes existing strip. }
    procedure TriStripEnd(LastIndex: PIndex; ToAppend: TIndex);

    { Functions for vertex index computations. }
    function LinearIndex(I, J, M: TIndex): TIndex;
    function IndexSW(M: TIndex): TIndex;
    function IndexSE(M: TIndex): TIndex;
    function IndexNW(M: TIndex): TIndex;
    function IndexNE(M: TIndex): TIndex;
    function IndexC(M: TIndex): TIndex;
    function RootS(M: TIndex): TTriangle;
    function RootN(M: TIndex): TTriangle;
    function RootW(M: TIndex): TTriangle;
    function RootE(M: TIndex): TTriangle;
    function Split(I, J, K: TIndex): TIndex;
    function ChildR(I, J, K: TIndex): TTriangle;
    function ChildL(I, J, K: TIndex): TTriangle;

    // property TerrainSize:Integer read FTerrainSize;
  end;

implementation

uses
  OpenGLTokens, GLContext;

function Vec3DistSqr(const V1, V2: TVector3f): Single;
{$IFDEF USE_ASM}
asm
  FLD    V1[0]
  FSUB   V2[0]
  FMUL   ST, ST
  FLD    V1[1]
  FSUB   V2[1]
  FMUL   ST, ST
  FADD
  FLD    V1[2]
  FSUB   V2[2]
  FMUL   ST, ST
  FADD
end;
{$ELSE}
begin
  Result := (V1[0] - V2[0]) * (V1[0] - V2[0]) + (V1[1] - V2[1]) *
    (V1[1] - V2[1]) + (V1[2] - V2[2]) * (V1[2] - V2[2]);
end;
{$ENDIF}

function Iff(Condition: Boolean; TruePart, FalsePart: LongInt): LongInt;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function IffFloat(Condition: Boolean; TruePart, FalsePart: Single): Single;
begin
  if Condition then
    Result := TruePart
  else
    Result := FalsePart;
end;

function MaxFloat(A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

procedure TSoarTerrain.LoadTerrain(HasWrappedEdges: Boolean);
var
  X, Y, OldBitCount, Count, MaxNumIndices, A, B, C, M, N, I, J, S: LongInt;
  Time: Int64;

begin

  RefinementLevels := Trunc(Log2(FTerrainSize - 1) * 2);
  CurrentTerrainSize := HeightData.Size;

  NumVertices := FTerrainSize * FTerrainSize;
  SetLength(VertexBuffer, NumVertices + FTerrainSize * 4);
  MaxNumIndices := FTerrainSize * FTerrainSize * 4;
  IndexBufferSize := IndexBufferCapacity;
  SetLength(IndexBuffer, IndexBufferSize);

  PosXEdgeMinZ := 1E20;
  PosYEdgeMinZ := 1E20;
  NegXEdgeMinZ := 1E20;
  NegYEdgeMinZ := 1E20;

  SetLength(PosXEdgeIndices, FTerrainSize * 6);
  SetLength(PosYEdgeIndices, FTerrainSize * 6);
  SetLength(NegXEdgeIndices, FTerrainSize * 6);
  SetLength(NegYEdgeIndices, FTerrainSize * 6);

  // Set vertex positions
  for Y := 0 to FTerrainSize - 1 do
    for X := 0 to FTerrainSize - 1 do
    begin
      I := Y * FTerrainSize + X;
      VertexBuffer[I].Pos := Vector3fMake(HeightData.XLeft + X,
        HeightData.YTop + Y, HeightData.GetHeight(X, Y) / 128);
      // Store minimal heights for aech block edge
      if (X = FTerrainSize - 1) and (PosXEdgeMinZ > VertexBuffer[I].Pos[2]) then
        PosXEdgeMinZ := VertexBuffer[I].Pos[2];
      if (Y = FTerrainSize - 1) and (PosYEdgeMinZ > VertexBuffer[I].Pos[2]) then
        PosYEdgeMinZ := VertexBuffer[I].Pos[2];
      if (X = 0) and (NegXEdgeMinZ > VertexBuffer[I].Pos[2]) then
        NegXEdgeMinZ := VertexBuffer[I].Pos[2];
      if (Y = 0) and (NegYEdgeMinZ > VertexBuffer[I].Pos[2]) then
        NegYEdgeMinZ := VertexBuffer[I].Pos[2];
    end;

  // Set vertices used for bottom of edge triangles
  X := FTerrainSize - 1;
  for Y := 0 to FTerrainSize - 1 do
  begin
    I := Y * FTerrainSize + X;
    VertexBuffer[NumVertices + Y + FTerrainSize * 0] := VertexBuffer[I];
    VertexBuffer[NumVertices + Y + FTerrainSize * 0].Pos[2] := PosXEdgeMinZ;
  end;
  Y := FTerrainSize - 1;
  for X := 0 to FTerrainSize - 1 do
  begin
    I := Y * FTerrainSize + X;
    VertexBuffer[NumVertices + X + FTerrainSize * 1] := VertexBuffer[I];
    VertexBuffer[NumVertices + X + FTerrainSize * 1].Pos[2] := PosYEdgeMinZ;
  end;
  X := 0;
  for Y := 0 to FTerrainSize - 1 do
  begin
    I := Y * FTerrainSize + X;
    VertexBuffer[NumVertices + Y + FTerrainSize * 2] := VertexBuffer[I];
    VertexBuffer[NumVertices + Y + FTerrainSize * 2].Pos[2] := NegXEdgeMinZ;
  end;
  Y := 0;
  for X := 0 to FTerrainSize - 1 do
  begin
    I := Y * FTerrainSize + X;
    VertexBuffer[NumVertices + X + FTerrainSize * 3] := VertexBuffer[I];
    VertexBuffer[NumVertices + X + FTerrainSize * 3].Pos[2] := NegYEdgeMinZ;
  end;

  // If edges should be wrapped (more than one SOAR blocks are used) copy
  // edge vertices' Z heights
  if HasWrappedEdges then
  begin
    for Y := 0 to FTerrainSize - 2 do
      VertexBuffer[Y * FTerrainSize + FTerrainSize - 1].Pos[2] := VertexBuffer
        [Y * FTerrainSize].Pos[2];
    for X := 0 to FTerrainSize - 2 do
      VertexBuffer[(FTerrainSize - 1) * FTerrainSize + X].Pos[2] := VertexBuffer
        [X].Pos[2];
    VertexBuffer[(FTerrainSize - 1) * FTerrainSize + FTerrainSize - 1].Pos[2]
      := VertexBuffer[0].Pos[2];
  end;
  // MARCAT
  // MsgOut(Format('Terrain geometry build in   %5.0n ms', [(GetTimeMicroseconds - Time) / 1000]));

  // Compute error metrics and vertex radii

  // MARCAT  Time := GetTimeMicroseconds;

  N := 1 shl (RefinementLevels div 2);
  M := N div 2;

  A := 1;
  B := 2;
  C := 1;
  S := 0;

  while A <> N do
  begin
    J := A;
    while J < N do
    begin
      I := 0;
      while I <= N do
      begin
        VertexLODCompute(I, J, 0, A, S, FTerrainSize);
        VertexLODCompute(J, I, A, 0, S, FTerrainSize);
        Inc(I, B);
      end;
      Inc(J, B);
    end;

    J := A;
    while J < N do
    begin
      I := A;
      while I < N do
      begin
        VertexLODCompute(I, J, A, C, N, FTerrainSize);
        Inc(I, B);
      end;
      Inc(J, B);
    end;
    A := B;
    C := B;
    B := B * 2;
    S := N;
  end;
  // MARCAT
  // MsgOut(Format('Error metrics calculated in %5.0n ms', [(GetTimeMicroseconds - Time) / 1000]));

  VertexBuffer[0].Error := 1E06;
  VertexBuffer[N].Error := 1E06;
  VertexBuffer[N * FTerrainSize].Error := 1E06;
  VertexBuffer[N * FTerrainSize + N].Error := 1E06;
  VertexBuffer[M * FTerrainSize + M].Error := 1E06;

  VertexBuffer[0].Radius := 1E06;
  VertexBuffer[N].Radius := 1E06;
  VertexBuffer[N * FTerrainSize].Radius := 1E06;
  VertexBuffer[N * FTerrainSize + N].Radius := 1E06;
  VertexBuffer[M * FTerrainSize + M].Radius := 1E06;
end;

procedure TSoarTerrain.FreeTerrain;
begin
  SetLength(VertexBuffer, 0);
  SetLength(IndexBuffer, 0);
  SetLength(PosXEdgeIndices, 0);
  SetLength(PosYEdgeIndices, 0);
  SetLength(NegXEdgeIndices, 0);
  SetLength(NegYEdgeIndices, 0);
end;

procedure TSoarTerrain.VertexLODCompute(I, J, DI, DJ, N, TerrainSize: LongInt);
var
  VP, CP: PVertex;
  R: Single;
  K: LongInt;
begin
  VP := @VertexBuffer[J * TerrainSize + I];

  VP.Error := Abs(VP.Pos[2] - 0.5 *
      (VertexBuffer[(J - DJ) * TerrainSize + I - DI].Pos[2] + VertexBuffer[
        (J + DJ) * TerrainSize + I + DI].Pos[2]));
  VP.Radius := 0.0;

  if N <> 0 then
  begin
    DJ := (DI + DJ) div 2;
    DI := DI - DJ;
    K := 4;
    repeat
      if ((I <> 0) or (DI >= 0)) and ((I <> N) or (DI <= 0)) and
        ((J <> 0) or (DJ >= 0)) and ((J <> N) or (DJ <= 0)) then
      begin
        CP := @VertexBuffer[(J + DJ) * TerrainSize + I + DI];
        VP.Error := MaxFloat(VP.Error, CP.Error);
        R := VectorDistance(VP.Pos, CP.Pos) + CP.Radius;
        VP.Radius := MaxFloat(VP.Radius, R);
      end;
      DJ := DJ + DI;
      DI := DI - DJ;
      DJ := DJ + DI;
      Dec(K);
    until K = 0;
  end;
end;

function TSoarTerrain.GetTerrainZ(X, Y, OldZ: Single): Single;
var
  IX, IY: LongInt;
  H1, H2, H3: Single;
begin
  IX := Trunc(X);
  X := Frac(X);
  IY := Trunc(Y);
  Y := Frac(Y);

  if (IX >= CurrentTerrainSize) or (IY >= CurrentTerrainSize) or (IX < 0) or
    (IY < 0) then
  begin
    Result := OldZ;
    Exit;
  end;

  if X + Y <= 1 then
  begin
    // top-left triangle
    H1 := VertexBuffer[IY * CurrentTerrainSize + IX].Pos[2];
    H2 := VertexBuffer[IY * CurrentTerrainSize + IX + 1].Pos[2];
    H3 := VertexBuffer[(IY + 1) * CurrentTerrainSize + IX].Pos[2];
    Result := (H1 + (H2 - H1) * X + (H3 - H1) * Y);
  end
  else
  begin
    // bottom-right triangle
    H1 := VertexBuffer[(IY + 1) * CurrentTerrainSize + IX + 1].Pos[2];
    H2 := VertexBuffer[(IY + 1) * CurrentTerrainSize + IX].Pos[2];
    H3 := VertexBuffer[IY * CurrentTerrainSize + IX + 1].Pos[2];
    Result := (H1 + (H2 - H1) * (1 - X) + (H3 - H1) * (1 - Y));
  end;
end;

procedure TSoarTerrain.MeshRefine(const ViewportWidth, FovX: Single;
  const Edges: TEdgeFlags; const ViewPoint: TVector3f;
  const Frustum: TFrustumSoar);
var
  CurrIndex: PIndex;
begin
  RefineViewPoint := ViewPoint;
  RefineFrustum := Frustum;
  RefineEdges := Edges;

  NumPosXEdgeIndices := 0;
  NumPosYEdgeIndices := 0;
  NumNegXEdgeIndices := 0;
  NumNegYEdgeIndices := 0;

  Kappa := (TolerancePixels / ViewportWidth) * DegToRad(FovX);
  InverseKappa := IffFloat(Kappa > 0.0, 1.0 / Kappa, 1E06);

  CurrIndex := TriStripBegin(IndexSW(RefinementLevels div 2), True);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1,
    RootS(RefinementLevels div 2), Iff(ActiveCulling, SphereUndecided,
      SphereVisible));
  CurrIndex := TriStripAppend(CurrIndex, IndexSE(RefinementLevels div 2),
    False);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1,
    RootE(RefinementLevels div 2), Iff(ActiveCulling, SphereUndecided,
      SphereVisible));
  CurrIndex := TriStripAppend(CurrIndex, IndexNE(RefinementLevels div 2),
    False);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1,
    RootN(RefinementLevels div 2), Iff(ActiveCulling, SphereUndecided,
      SphereVisible));
  CurrIndex := TriStripAppend(CurrIndex, IndexNW(RefinementLevels div 2),
    False);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1,
    RootW(RefinementLevels div 2), Iff(ActiveCulling, SphereUndecided,
      SphereVisible));

  TriStripEnd(CurrIndex, IndexSW(RefinementLevels div 2));
end;

function TSoarTerrain.SubMeshRefine(LastIndex: PIndex; Level: LongInt;
  Tri: TTriangle): PIndex;
var
  Refine: Boolean;
begin
  Refine := (Level > MinLOD) and (ForceFullRefinement or IsVertexActive
      (VertexBuffer[Split(Tri.I, Tri.J, Tri.K)]));

  if Refine then
    LastIndex := SubMeshRefine(LastIndex, Level - 1,
      ChildL(Tri.I, Tri.J, Tri.K));
  LastIndex := TriStripAppend(LastIndex, Tri.I, Boolean(Level and 1));
  if Refine then
    LastIndex := SubMeshRefine(LastIndex, Level - 1,
      ChildR(Tri.I, Tri.J, Tri.K));

  Result := LastIndex;
end;

function TSoarTerrain.SubMeshRefineVisible(LastIndex: PIndex; Level: LongInt;
  Tri: TTriangle; Mask: LongWord): PIndex;
var
  Refine: Boolean;
begin
  if Mask = SphereVisible then
  begin
    Result := SubMeshRefine(LastIndex, Level, Tri);
    Exit;
  end
  else
  begin
    Mask := IsSphereVisible(VertexBuffer[Split(Tri.I, Tri.J, Tri.K)], Mask);
    Refine := (Level > MinLOD) and (Mask > 0) and IsVertexActive
      (VertexBuffer[Split(Tri.I, Tri.J, Tri.K)]);
  end;

  if Refine then
    LastIndex := SubMeshRefineVisible(LastIndex, Level - 1,
      ChildL(Tri.I, Tri.J, Tri.K), Mask);
  LastIndex := TriStripAppend(LastIndex, Tri.I, Boolean(Level and 1));
  if Refine then
    LastIndex := SubMeshRefineVisible(LastIndex, Level - 1,
      ChildR(Tri.I, Tri.J, Tri.K), Mask);

  Result := LastIndex;
end;

function TSoarTerrain.IsSphereVisible(const V: TVertex;
  Mask: LongWord): LongWord;
var
  I: LongInt;
  D, Rad: Single;
  tempvec: Tvector4f;
  tempvec2: TVector3f;
begin
  Result := Mask;
  Rad := V.Radius + 1.0;
  for I := 0 to 5 do
    if (Result and (1 shl I)) = 0 then
    begin
      tempvec := RefineFrustum.plane[I];
      tempvec2[0] := tempvec[0];
      tempvec2[1] := tempvec[1];
      tempvec2[2] := tempvec[2];
      D := VectorDotProduct(V.Pos, tempvec2) + tempvec[3];
      if D < -Rad then
      begin
        Result := 0;
        Exit;
      end;
      if D > Rad then
        Result := Result or (1 shl I);
    end;
end;

function TSoarTerrain.IsVertexActive(const V: TVertex): Boolean;
var
  ASqr: Single;
  Pos: TVector3f;
begin
  if not ForceFullRefinement then
  begin
    Pos := V.Pos;
    if ErrorMetric = emAnisotropic then
    begin
      ASqr := Sqr(RefineViewPoint[0] - Pos[0]) + Sqr
        (RefineViewPoint[1] - Pos[1]);
      Result := V.Error * V.Error * ASqr > Sqr(MaxFloat(0.0,
          Kappa * (Vec3DistSqr(Pos, RefineViewPoint) - V.Radius * V.Radius)
            - V.Error * V.Radius));
    end
    else
      Result := Sqr(InverseKappa * V.Error + V.Radius) > Vec3DistSqr(Pos,
        RefineViewPoint);
  end
  else
    Result := True;
end;

procedure TSoarTerrain.AppendEdgeTriangles(Index1, Index2, VertexOffset,
  Index1Bottom, Index2Bottom: TIndex; var NumIndices: LongInt;
  var Indices: array of TIndex);
begin
  Indices[NumIndices + 0] := Index1;
  Indices[NumIndices + 1] := TIndex(NumVertices) + VertexOffset + Index1Bottom;
  Indices[NumIndices + 2] := Index2;

  Indices[NumIndices + 3] := Index2;
  Indices[NumIndices + 4] := TIndex(NumVertices) + VertexOffset + Index1Bottom;
  Indices[NumIndices + 5] := TIndex(NumVertices) + VertexOffset + Index2Bottom;

  NumIndices := NumIndices + 6;
end;

procedure TSoarTerrain.CheckTriangleOnEdge(ToAppend, Head, Tail: TIndex);
var
  EdgeIdx: TIndex;
begin
  if efPosX in RefineEdges then
  begin
    if (VertexBuffer[ToAppend].Pos[0] = CurrentTerrainSize - 1) and
      ((VertexBuffer[Head].Pos[0] = CurrentTerrainSize - 1) or
        (VertexBuffer[Tail].Pos[0] = CurrentTerrainSize - 1)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos[0] = CurrentTerrainSize - 1, Head,
        Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, 0,
        Round(VertexBuffer[ToAppend].Pos[1]),
        Round(VertexBuffer[EdgeIdx].Pos[1]), NumPosXEdgeIndices,
        PosXEdgeIndices);
    end;
  end;
  if efPosY in RefineEdges then
  begin
    if (VertexBuffer[ToAppend].Pos[1] = CurrentTerrainSize - 1) and
      ((VertexBuffer[Head].Pos[1] = CurrentTerrainSize - 1) or
        (VertexBuffer[Tail].Pos[1] = CurrentTerrainSize - 1)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos[1] = CurrentTerrainSize - 1, Head,
        Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, CurrentTerrainSize,
        Round(VertexBuffer[ToAppend].Pos[0]),
        Round(VertexBuffer[EdgeIdx].Pos[0]), NumPosYEdgeIndices,
        PosYEdgeIndices);
    end;
  end;
  if efNegX in RefineEdges then
  begin
    if (VertexBuffer[ToAppend].Pos[0] = 0) and
      ((VertexBuffer[Head].Pos[0] = 0) or (VertexBuffer[Tail].Pos[0] = 0)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos[0] = 0, Head, Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, CurrentTerrainSize * 2,
        Round(VertexBuffer[ToAppend].Pos[1]),
        Round(VertexBuffer[EdgeIdx].Pos[1]), NumNegXEdgeIndices,
        NegXEdgeIndices);
    end;
  end;
  if efNegY in RefineEdges then
  begin
    if (VertexBuffer[ToAppend].Pos[1] = 0) and
      ((VertexBuffer[Head].Pos[1] = 0) or (VertexBuffer[Tail].Pos[1] = 0)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos[1] = 0, Head, Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, CurrentTerrainSize * 3,
        Round(VertexBuffer[ToAppend].Pos[0]),
        Round(VertexBuffer[EdgeIdx].Pos[0]), NumNegYEdgeIndices,
        NegYEdgeIndices);
    end;
  end;
end;

function TSoarTerrain.TriStripBegin(FirstIndex: TIndex;
  Parity: Boolean): PIndex;
begin
  Result := @IndexBuffer[0];
  Result^ := FirstIndex;
  Inc(Result);
  Result^ := FirstIndex;
  Inc(Result);
  Result^ := TIndex(Parity);
  NumTriangles := 0;
end;

function TSoarTerrain.TriStripAppend(LastIndex: PIndex; ToAppend: TIndex;
  Parity: Boolean): PIndex;
var
  Tail, Head, StripPar: TIndex;
  Indices: LongInt;
begin
  Result := LastIndex;

  Dec(Result);
  Dec(Result);
  Tail := Result^;
  Inc(Result);
  Head := Result^;
  Inc(Result);
  StripPar := Result^;

  if (ToAppend <> Tail) and (ToAppend <> Head) then
  begin
    // Check if there is some free space left in index buffer and increase it
    // if necessary
    Indices := (LongWord(Result) - LongWord(IndexBuffer)) div SizeOf(TIndex);
    if Length(IndexBuffer) <= Indices + 16 then
    begin
      while IndexBufferSize <= Indices + 16 do
        Inc(IndexBufferSize, IndexBufferCapacity);

      SetLength(IndexBuffer, IndexBufferSize);
      // Pointer to array must be reinitialized, because IndexBuffer's starting
      // address may have changed
      Result := @IndexBuffer[Indices];
    end;

    if TIndex(Parity) = StripPar then
    begin
      Result^ := Tail;
      Inc(Result);
    end;
    Result^ := ToAppend;
    Inc(Result);
    Result^ := TIndex(Parity);
    Inc(NumTriangles);

    CheckTriangleOnEdge(ToAppend, Head, Tail);
  end;
end;

procedure TSoarTerrain.TriStripEnd(LastIndex: PIndex; ToAppend: TIndex);
var
  Head, Tail: TIndex;
begin
  LastIndex^ := ToAppend;
  Inc(LastIndex);
  Inc(NumTriangles);
  NumIndices := (LongWord(LastIndex) - LongWord(IndexBuffer)) div SizeOf
    (TIndex);

  Dec(LastIndex);
  Dec(LastIndex);
  Dec(LastIndex);
  Tail := LastIndex^;
  Inc(LastIndex);
  Head := LastIndex^;
  CheckTriangleOnEdge(ToAppend, Head, Tail);
end;

function TSoarTerrain.LinearIndex(I, J, M: TIndex): TIndex;
begin
  Result := I + J + (J shl M);
end;

function TSoarTerrain.IndexSW(M: TIndex): TIndex;
begin
  Result := LinearIndex(0 shl M, 0 shl M, M);
end;

function TSoarTerrain.IndexSE(M: TIndex): TIndex;
begin
  Result := LinearIndex(1 shl M, 0 shl M, M);
end;

function TSoarTerrain.IndexNW(M: TIndex): TIndex;
begin
  Result := LinearIndex(0 shl M, 1 shl M, M);
end;

function TSoarTerrain.IndexNE(M: TIndex): TIndex;
begin
  Result := LinearIndex(1 shl M, 1 shl M, M);
end;

function TSoarTerrain.IndexC(M: TIndex): TIndex;
begin
  Result := LinearIndex(1 shl (M - 1), 1 shl (M - 1), M);
end;

function TSoarTerrain.RootS(M: TIndex): TTriangle;
begin
  Result.I := IndexC(M);
  Result.J := IndexSW(M);
  Result.K := IndexSE(M);
end;

function TSoarTerrain.RootN(M: TIndex): TTriangle;
begin
  Result.I := IndexC(M);
  Result.J := IndexNE(M);
  Result.K := IndexNW(M);
end;

function TSoarTerrain.RootW(M: TIndex): TTriangle;
begin
  Result.I := IndexC(M);
  Result.J := IndexNW(M);
  Result.K := IndexSW(M);
end;

function TSoarTerrain.RootE(M: TIndex): TTriangle;
begin
  Result.I := IndexC(M);
  Result.J := IndexSE(M);
  Result.K := IndexNE(M);
end;

function TSoarTerrain.Split(I, J, K: TIndex): TIndex;
begin
  Result := (J + K) div 2;
end;

function TSoarTerrain.ChildR(I, J, K: TIndex): TTriangle;
begin
  Result.I := Split(I, J, K);
  Result.J := I;
  Result.K := K;
end;

function TSoarTerrain.ChildL(I, J, K: TIndex): TTriangle;
begin
  Result.I := Split(I, J, K);
  Result.J := J;
  Result.K := I;
end;

constructor TSoarTerrain.Create;
begin
  inherited;
  FTerrainSize := HeightData.Size;
  FRenderTexture := True;
  FTerrainBlocks := 1;
  RefinementLevels := 1;
  TolerancePixels := 4;
  ActiveCulling := True;
  ForceFullRefinement := False;
  MinLOD := 0;
  ErrorMetric := emAnisotropic;

end;

destructor TSoarTerrain.Destroy;
begin
  FreeTerrain;
  inherited;
end;

procedure TSoarTerrain.RenderTerrain(FRenderWireframe: Boolean;
  const ViewportWidth, FovX: Single; const ViewPoint: TVector3f;
  const Frustum: TFrustumSoar);
var
  Time: Int64;
  EdgeFlags: TEdgeFlags;
begin
  EdgeFlags := [efPosX, efPosY, efNegX, efNegY];

  { if (X >= 0) and (X < FTerrainBlocks - 1) then Include(EdgeFlags, efPosX);
    if (X > 0) and (X <= FTerrainBlocks - 1) then Include(EdgeFlags, efNegX);
    if (Y >= 0) and (Y < FTerrainBlocks - 1) then Include(EdgeFlags, efPosY);
    if (Y > 0) and (Y <= FTerrainBlocks - 1) then Include(EdgeFlags, efNegY); }

  // Time := GetTimeMicroseconds; MARCAT
  MeshRefine(ViewportWidth, FovX, EdgeFlags, ViewPoint, Frustum);
  // Inc(RefineTime, GetTimeMicroseconds - Time); MARCAT

  // Enable/disable textures
  { if FRenderTexture then
    begin
    glEnable(GL_TEXTURE_2D);
    {    if MultiTexturing then
    begin
    glActiveTextureARB(GL_TEXTURE1_ARB);
    if ActiveDetailTexture then //MARCAT
    glEnable(GL_TEXTURE_2D)
    else
    glDisable(GL_TEXTURE_2D);
    glActiveTextureARB(GL_TEXTURE0_ARB);
    end; }
  { end
    else
    begin
    //    DisableTextures;   MARCAT
    //    glColor4fv(@GroundColor); MARCAT
    end; }

  if FRenderWireframe then
  begin

    GL.Enable(GL_POLYGON_OFFSET_FILL);
    GL.PolygonOffset(1.0, 4.0);
  end;

  GL.MatrixMode(GL_MODELVIEW);
  GL.PushMatrix;

  GL.VertexPointer(3, GL_FLOAT, SizeOf(TVertex), @VertexBuffer[0].Pos);

  // Time := GetTimeMicroseconds; MARCAT
  // Draw terrain block
  GL.DrawElements(GL_TRIANGLE_STRIP, NumIndices, GL_UNSIGNED_INT, IndexBuffer);
  // Draw block edges if necessary
  { if efPosX in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumPosXEdgeIndices, GL_UNSIGNED_INT, PosXEdgeIndices);
    if efPosY in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumPosYEdgeIndices, GL_UNSIGNED_INT, PosYEdgeIndices);
    if efNegX in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumNegXEdgeIndices, GL_UNSIGNED_INT, NegXEdgeIndices);
    if efNegY in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumNegYEdgeIndices, GL_UNSIGNED_INT, NegYEdgeIndices); }
  // Inc(RenderTime, GetTimeMicroseconds - Time); MARCAT

  if FRenderWireframe then
  begin
    GL.PushAttrib(GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT or GL_POLYGON_BIT);
    GL.PolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    GL.Color3f(1, 1, 1);
    GL.BlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    GL.Disable(GL_LIGHTING);
    GL.Enable(GL_BLEND);
    GL.Enable(GL_LINE_SMOOTH);
    // DisableTextures; MARCAT
    GL.DrawElements(GL_TRIANGLE_STRIP, NumIndices, GL_UNSIGNED_INT, IndexBuffer);
    GL.PopAttrib;
    GL.Disable(GL_POLYGON_OFFSET_FILL);
  end;

  GL.PopMatrix;

  // Inc(TrianglesDrawn, NumTriangles);
end;

end.
