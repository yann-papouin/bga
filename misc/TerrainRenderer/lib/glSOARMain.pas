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

unit glSOARMain;

interface

uses
  SysUtils,
  VUMaths,
  ImagingTypes,
  ImagingClasses{,
  ImagingUtility};

type
  { Terrain vertex type.}
  TVertex = record
    Pos: TVector3;
    Error: Single;
    Radius: Single;
  end;
  PVertex = ^TVertex;

  { Triangle vertex index type.}
  TIndex = LongWord;
  PIndex = ^TIndex;

  TIndexArray = array[0..MaxInt div SizeOf(TIndex) - 1] of TIndex;
  PIndexArray = ^TIndexArray;

  { Terrain triangle type.}
  TTriangle = record
    I, J, K: TIndex;
  end;

  { View culling frustum type.}
  TFrustum = array[0..5] of TPlane;

  { Available error metric type.}
  TErrorMetric = (emIsotropic, emAnisotropic);

  TEdgeFlag = (efPosX, efPosY, efNegX, efNegY);
  TEdgeFlags = set of TEdgeFlag;

const
  SphereUndecided = 64;
  SphereVisible = 127;
  IndexBufferCapacity = 1024 * 1024;
Type TSoarTerrain=class(TObject)
     private
            FTerrainSize,FTerrainBlocks:Integer;
            FRenderTexture,FRenderWireframe:Boolean;
            procedure AppendEdgeTriangles(Index1, Index2, VertexOffset,
               Index1Bottom, Index2Bottom: TIndex; var NumIndices: Integer;
               var Indices: array of TIndex);
            procedure CheckTriangleOnEdge(ToAppend, Head, Tail: TIndex);
     public
            { Number of LODs uf current terrain. Most detailed level is 0.}
            RefinementLevels: LongInt;
            { Current screen space error treshhold.}
            TolerancePixels: LongInt;
            { Activates/Deactivates view culling (do not refine triangle outside frustum).}
            ActiveCulling: Boolean;
            { Forces maximal possible refinement (as if tolerance is 1 pixel).}
            ForceFullRefinement: Boolean;
            { Refinement will no continue bellow this level.}
            MinLOD: LongInt;
            { Error metric used in refinemnt.}
            ErrorMetric: TErrorMetric;

            { Source heightmap image in 16bit grayscale format.}
            HeightMap: TSingleImage;
            { Vertex buffer for whole terrain.}
            VertexBuffer: array of TVertex;
            NumVertices: LongInt;
            { Index buffer for current terrain triangulation.}
            IndexBuffer: array of TIndex;
            { Index and triangle count for currently refined mesh.}
            NumIndices: LongInt;
            IndexBufferSize: LongInt;
            NumTriangles: LongInt;
            { Index buffers and counts for skirts that fill cracks between blocks.}
            PosXEdgeIndices, PosYEdgeIndices, NegXEdgeIndices, NegYEdgeIndices: array of TIndex;
            NumPosXEdgeIndices, NumPosYEdgeIndices, NumNegXEdgeIndices, NumNegYEdgeIndices: LongInt;
            Constructor Create(TerrainSize:Integer);
            Destructor Destroy;override;

            Procedure RenderTerrain(X, Y: LongInt;const ViewportWidth, FovX: Single;
  const ViewPoint: TVector3; const Frustum: TFrustum; var RefineTime, RenderTime: Int64);

            { Loads heightmap from image and builds vertices.}
            procedure LoadTerrain(const HeightMapFile: string; TerrainSize: LongInt;
              const TerrainZScale: Single; HasWrappedEdges, Filter8BitHeightmap: Boolean);
            { Frees all terrain resources.}
            procedure FreeTerrain;
            { Precomputes error and radius for vertex.}
            procedure VertexLODCompute(I, J, DI, DJ, N, TerrainSize: LongInt);
            { Returns height of the terrain at given [X, Y] coordinates.}
            function GetTerrainZ(X, Y, OldZ: Single): Single;

            { Starts mesh refinement.}
            procedure MeshRefine(const ViewportWidth, FovX: Single; const Edges: TEdgeFlags;
              const ViewPoint, ShiftVector: TVector3; const Frustum: TFrustum);
            { Starts submesh refinement.}
            function SubMeshRefine(LastIndex: PIndex; Level: LongInt;  Tri: TTriangle): PIndex;
            { Starts submesh refinement.}
            function SubMeshRefineVisible(LastIndex: PIndex; Level: LongInt;  Tri: TTriangle; Mask: LongWord): PIndex;

            { Calculates vertex bounding sphere visibility.}
            function IsSphereVisible(const V: TVertex; Mask: LongWord): LongWord;
            { Returns tru if vertex is active in current mesh.}
            function IsVertexActive(const V: TVertex): Boolean;

            { Begins new triangle strip.}
            function TriStripBegin(FirstIndex: TIndex; Parity: Boolean): PIndex;
            { Appends triangles to existing strip.}
            function TriStripAppend(LastIndex: PIndex; ToAppend: TIndex; Parity: Boolean): PIndex;
            { Finishes existing strip.}
            procedure TriStripEnd(LastIndex: PIndex; ToAppend: TIndex);

            { Functions for vertex index computations.}
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
     end;
implementation

uses
  gl;

var
  CurrentTerrainSize: LongInt;
  RefineViewPoint: TVector3;
  RefineShift: TVector3;
  RefineFrustum: TFrustum;
  RefineEdges: TEdgeFlags;
  Kappa: Single;
  InverseKappa: Single;
  PosXEdgeMinZ, PosYEdgeMinZ, NegXEdgeMinZ, NegYEdgeMinZ: Single;












(*Type
  TImageFormat = (
    ifUnknown        = 0,
    ifDefault        = 1,
    { Indexed formats using palette.}
    ifIndex8         = 10,
    { Grayscale/Luminance formats.}
    ifGray8          = 40,
    ifA8Gray8        = 41,
    ifGray16         = 42,
    ifGray32         = 43,
    ifGray64         = 44,
    ifA16Gray16      = 45,
    { ARGB formats.}
    ifX5R1G1B1       = 80,
    ifR3G3B2         = 81,
    ifR5G6B5         = 82,
    ifA1R5G5B5       = 83,
    ifA4R4G4B4       = 84,
    ifX1R5G5B5       = 85,
    ifX4R4G4B4       = 86,
    ifR8G8B8         = 87,
    ifA8R8G8B8       = 88,
    ifX8R8G8B8       = 89,
    ifR16G16B16      = 90,
    ifA16R16G16B16   = 91,
    ifB16G16R16      = 92,
    ifA16B16G16R16   = 93,
    { Floating point formats.}
    ifR32F           = 170,
    ifA32R32G32B32F  = 171,
    ifA32B32G32R32F  = 172,
    { Special formats.}
    ifDXT1           = 220,
    ifDXT3           = 221,
    ifDXT5           = 222);
  TResizeFilter = (
    rfNearest  = 0,
    rfBilinear = 1,
    rfBicubic  = 2);*)

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

















procedure TSoarTerrain.LoadTerrain(const HeightMapFile: string; TerrainSize: LongInt;
  const TerrainZScale: Single; HasWrappedEdges, Filter8BitHeightmap: Boolean);
var
  X, Y, OldBitCount, Count, MaxNumIndices,
    A, B, C, M, N,
    I, J, S: LongInt;
  Time: Int64;

  function GetHexelAtPoint(Y, X: LongInt): Word;
  begin
    if HasWrappedEdges then
    begin
      if X = -2 then X := TerrainSize - 2;
      if X = -1 then X := TerrainSize - 1;
      if Y = -2 then Y := TerrainSize - 2;
      if Y = -1 then Y := TerrainSize - 1;
      if X = TerrainSize then X := 0;
      if X = TerrainSize + 1 then X := 1;
      if Y = TerrainSize then Y := 0;
      if Y = TerrainSize + 1 then Y := 1;
    end
    else
    begin
      if X < 0 then X := 0;
      if Y < 0 then Y := 0;
      if X > TerrainSize - 3 then X := TerrainSize - 3;
      if Y > TerrainSize - 3 then Y := TerrainSize - 3;
    end;

    Result := PWordArray(HeightMap.Bits)[Y * TerrainSize + X]
  end;

begin
//  Time := GetTimeMicroseconds;  //MARCAT
  HeightMap := TSingleImage.CreateFromFile(HeightMapFile);
  OldBitCount := HeightMap.FormatInfo.BytesPerPixel div HeightMap.FormatInfo.ChannelCount * 8;
  // First convert heightmap to 16bit grayscale
  HeightMap.Format := ifGray16;
  HeightMap.Resize(TerrainSize, TerrainSize, rfBicubic);

  if Filter8BitHeightmap and (OldBitCount < 16) and (TerrainSize < 4097) then
  begin
    // Aplly 5x5 linear gauss filter to 8 bit heightmaps
    for Y := 0 to TerrainSize - 1 do
      for X := 0 to TerrainSize - 1 do
      begin
        Count :=
          GetHexelAtPoint(Y - 2, X - 2) * 1  +
          GetHexelAtPoint(Y - 2, X - 1) * 4  +
          GetHexelAtPoint(Y - 2, X + 0) * 6  +
          GetHexelAtPoint(Y - 2, X + 1) * 4  +
          GetHexelAtPoint(Y - 2, X + 2) * 1  +
          GetHexelAtPoint(Y - 1, X - 2) * 4  +
          GetHexelAtPoint(Y - 1, X - 1) * 16 +
          GetHexelAtPoint(Y - 1, X + 0) * 24 +
          GetHexelAtPoint(Y - 1, X + 1) * 16 +
          GetHexelAtPoint(Y - 1, X + 2) * 4  +
          GetHexelAtPoint(Y + 0, X - 2) * 6  +
          GetHexelAtPoint(Y + 0, X - 1) * 24 +
          GetHexelAtPoint(Y + 0, X + 0) * 36 +
          GetHexelAtPoint(Y + 0, X + 1) * 24 +
          GetHexelAtPoint(Y + 0, X + 2) * 6  +
          GetHexelAtPoint(Y + 1, X - 2) * 4  +
          GetHexelAtPoint(Y + 1, X - 1) * 16 +
          GetHexelAtPoint(Y + 1, X + 0) * 24 +
          GetHexelAtPoint(Y + 1, X + 1) * 16 +
          GetHexelAtPoint(Y + 1, X + 2) * 4  +
          GetHexelAtPoint(Y + 2, X - 2) * 1  +
          GetHexelAtPoint(Y + 2, X - 1) * 4  +
          GetHexelAtPoint(Y + 2, X + 0) * 6  +
          GetHexelAtPoint(Y + 2, X + 1) * 4  +
          GetHexelAtPoint(Y + 2, X + 2) * 1;
        Count := Count div 256;
        PWordArray(HeightMap.Bits)[Y * TerrainSize + X] := Count;
      end;
  end;
//marcat
//  MsgOut(Format('Heightmap prepared in       %5.0n ms', [(GetTimeMicroseconds - Time) / 1000]));

  RefinementLevels := Trunc(Log2(TerrainSize - 1) * 2);
  CurrentTerrainSize := HeightMap.Width;

//  Time := GetTimeMicroseconds; Marcat

  NumVertices := TerrainSize * TerrainSize;
  SetLength(VertexBuffer, NumVertices + TerrainSize * 4);
  MaxNumIndices := TerrainSize * TerrainSize * 4;
  IndexBufferSize := IndexBufferCapacity;
  SetLength(IndexBuffer, IndexBufferSize);

  PosXEdgeMinZ := 1e20;
  PosYEdgeMinZ := 1e20;
  NegXEdgeMinZ := 1e20;
  NegYEdgeMinZ := 1e20;

  SetLength(PosXEdgeIndices, TerrainSize * 6);
  SetLength(PosYEdgeIndices, TerrainSize * 6);
  SetLength(NegXEdgeIndices, TerrainSize * 6);
  SetLength(NegYEdgeIndices, TerrainSize * 6);

  // Set vertex positions
  for Y := 0 to TerrainSize - 1 do
    for X := 0 to TerrainSize - 1 do
    begin
      I := Y * TerrainSize + X;
      VertexBuffer[I].Pos := Vector3(X, Y,
        PWordArray(HeightMap.Bits)[I] / 65536 * TerrainSize * TerrainZScale);
      // Store minimal heights for aech block edge
      if (X = TerrainSize - 1) and (PosXEdgeMinZ > VertexBuffer[I].Pos.Z) then
        PosXEdgeMinZ := VertexBuffer[I].Pos.Z;
      if (Y = TerrainSize - 1) and (PosYEdgeMinZ > VertexBuffer[I].Pos.Z) then
        PosYEdgeMinZ := VertexBuffer[I].Pos.Z;
      if (X = 0) and (NegXEdgeMinZ > VertexBuffer[I].Pos.Z) then
        NegXEdgeMinZ := VertexBuffer[I].Pos.Z;
      if (Y = 0) and (NegYEdgeMinZ > VertexBuffer[I].Pos.Z) then
        NegYEdgeMinZ := VertexBuffer[I].Pos.Z;
    end;

  // Set vertices used for bottom of edge triangles
  X := TerrainSize - 1;
  for Y := 0 to TerrainSize - 1 do
  begin
    I := Y * TerrainSize + X;
    VertexBuffer[NumVertices + Y + TerrainSize * 0] := VertexBuffer[I];
    VertexBuffer[NumVertices + Y + TerrainSize * 0].Pos.Z := PosXEdgeMinZ;
  end;
  Y := TerrainSize - 1;
  for X := 0 to TerrainSize - 1 do
  begin
    I := Y * TerrainSize + X;
    VertexBuffer[NumVertices + X + TerrainSize * 1] := VertexBuffer[I];
    VertexBuffer[NumVertices + X + TerrainSize * 1].Pos.Z := PosYEdgeMinZ;
  end;
  X := 0;
  for Y := 0 to TerrainSize - 1 do
  begin
    I := Y * TerrainSize + X;
    VertexBuffer[NumVertices + Y + TerrainSize * 2] := VertexBuffer[I];
    VertexBuffer[NumVertices + Y + TerrainSize * 2].Pos.Z := NegXEdgeMinZ;
  end;
  Y := 0;
  for X := 0 to TerrainSize - 1 do
  begin
    I := Y * TerrainSize + X;
    VertexBuffer[NumVertices + X + TerrainSize * 3] := VertexBuffer[I];
    VertexBuffer[NumVertices + X + TerrainSize * 3].Pos.Z := NegYEdgeMinZ;
  end;

  // If edges should be wrapped (more than one SOAR blocks are used) copy
  // edge vertices' Z heights
  if HasWrappedEdges then
  begin
    for Y := 0 to TerrainSize - 2 do
      VertexBuffer[Y * TerrainSize + TerrainSize - 1].Pos.Z :=
        VertexBuffer[Y * TerrainSize].Pos.Z;
    for X := 0 to TerrainSize - 2 do
      VertexBuffer[(TerrainSize - 1) * TerrainSize + X].Pos.Z :=
        VertexBuffer[X].Pos.Z;
    VertexBuffer[(TerrainSize - 1) * TerrainSize + TerrainSize - 1].Pos.Z :=
      VertexBuffer[0].Pos.Z;
  end;
//MARCAT
//  MsgOut(Format('Terrain geometry build in   %5.0n ms', [(GetTimeMicroseconds - Time) / 1000]));

  // Compute error metrics and vertex radii

//MARCAT  Time := GetTimeMicroseconds;

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
        VertexLODCompute(I, J, 0, A, S, TerrainSize);
        VertexLODCompute(J, I, A, 0, S, TerrainSize);
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
        VertexLODCompute(I, J, A, C, N, TerrainSize);
        Inc(I, B);
      end;
      Inc(J, B);
    end;
    A := B;
    C := B;
    B := B * 2;
    S := N;
  end;
//MARCAT
//  MsgOut(Format('Error metrics calculated in %5.0n ms', [(GetTimeMicroseconds - Time) / 1000]));

  VertexBuffer[0].Error := 1e06;
  VertexBuffer[N].Error := 1e06;
  VertexBuffer[N * TerrainSize].Error := 1e06;
  VertexBuffer[N * TerrainSize + N].Error := 1e06;
  VertexBuffer[M * TerrainSize + M].Error := 1e06;

  VertexBuffer[0].Radius := 1e06;
  VertexBuffer[N].Radius := 1e06;
  VertexBuffer[N * TerrainSize].Radius := 1e06;
  VertexBuffer[N * TerrainSize + N].Radius := 1e06;
  VertexBuffer[M * TerrainSize + M].Radius := 1e06;
end;

procedure TSoarTerrain.FreeTerrain;
begin
  SetLength(VertexBuffer, 0);
  SetLength(IndexBuffer, 0);
  SetLength(PosXEdgeIndices, 0);
  SetLength(PosYEdgeIndices, 0);
  SetLength(NegXEdgeIndices, 0);
  SetLength(NegYEdgeIndices, 0);
  HeightMap.Free;
end;

procedure TSoarTerrain.VertexLODCompute(I, J, DI, DJ, N, TerrainSize: LongInt);
var
  VP, CP: PVertex;
  R: Single;
  K: LongInt;
begin
  VP := @VertexBuffer[J * TerrainSize + I];

  VP.Error := Abs(VP.Pos.Z - 0.5 *
    (VertexBuffer[(J - DJ) * TerrainSize + I - DI].Pos.Z +
    VertexBuffer[(J + DJ) * TerrainSize + I + DI].Pos.Z));
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
        R := Vec3Dist(VP.Pos, CP.Pos) + CP.Radius;
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

  if (IX >= CurrentTerrainSize) or (IY >= CurrentTerrainSize) or (IX < 0) or (IY < 0) then
  begin
    Result := OldZ;
    Exit;
  end;

  if X + Y <= 1 then
  begin
    // top-left triangle
    H1 := VertexBuffer[IY * CurrentTerrainSize + IX].Pos.Z;
    H2 := VertexBuffer[IY * CurrentTerrainSize + IX + 1].Pos.Z;
    H3 := VertexBuffer[(IY + 1)* CurrentTerrainSize + IX].Pos.Z;
    Result := (H1 + (H2 - H1) * X + (H3 - H1) * Y);
  end
  else
  begin
    // bottom-right triangle
    H1 := VertexBuffer[(IY + 1)* CurrentTerrainSize + IX + 1].Pos.Z;
    H2 := VertexBuffer[(IY + 1)* CurrentTerrainSize + IX].Pos.Z;
    H3 := VertexBuffer[IY * CurrentTerrainSize + IX + 1].Pos.Z;
    Result := (H1 + (H2 - H1) * (1 - X) + (H3 - H1) * (1 - Y));
  end;
end;

procedure TSoarTerrain.MeshRefine(const ViewportWidth, FovX: Single; const Edges: TEdgeFlags;
  const ViewPoint, ShiftVector: TVector3; const Frustum: TFrustum);
var
  CurrIndex: PIndex;
begin
  RefineViewPoint := ViewPoint;
  RefineShift := ShiftVector;
  RefineFrustum := Frustum;
  RefineEdges := Edges;

  NumPosXEdgeIndices := 0;
  NumPosYEdgeIndices := 0;
  NumNegXEdgeIndices := 0;
  NumNegYEdgeIndices := 0;

  Kappa := (TolerancePixels / ViewportWidth) * DegToRad(FovX);
  InverseKappa := IffFloat(Kappa > 0.0, 1.0 / Kappa, 1e06);

  CurrIndex := TriStripBegin(IndexSW(RefinementLevels div 2), True);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1, RootS(RefinementLevels div 2),
    Iff(ActiveCulling, SphereUndecided, SphereVisible));
  CurrIndex := TriStripAppend(CurrIndex, IndexSE(RefinementLevels div 2), False);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1, RootE(RefinementLevels div 2),
    Iff(ActiveCulling, SphereUndecided, SphereVisible));
  CurrIndex := TriStripAppend(CurrIndex, IndexNE(RefinementLevels div 2), False);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1, RootN(RefinementLevels div 2),
    Iff(ActiveCulling, SphereUndecided, SphereVisible));
  CurrIndex := TriStripAppend(CurrIndex, IndexNW(RefinementLevels div 2), False);
  CurrIndex := SubMeshRefineVisible(CurrIndex, RefinementLevels - 1, RootW(RefinementLevels div 2),
    Iff(ActiveCulling, SphereUndecided, SphereVisible));

  TriStripEnd(CurrIndex, IndexSW(RefinementLevels div 2));
end;

function TSoarTerrain.SubMeshRefine(LastIndex: PIndex; Level: LongInt;  Tri: TTriangle): PIndex;
var
  Refine: Boolean;
begin
  Refine := (Level > MinLOD) and (ForceFullRefinement or
    IsVertexActive(VertexBuffer[Split(Tri.I, Tri.J, Tri.K)]));

  if Refine then
    LastIndex := SubMeshRefine(LastIndex, Level - 1, ChildL(Tri.I, Tri.J, Tri.K));
  LastIndex := TriStripAppend(LastIndex, Tri.I, Boolean(Level and 1));
  if Refine then
    LastIndex := SubMeshRefine(LastIndex, Level - 1, ChildR(Tri.I, Tri.J, Tri.K));

  Result := LastIndex;
end;

function TSoarTerrain.SubMeshRefineVisible(LastIndex: PIndex; Level: LongInt;  Tri: TTriangle; Mask: LongWord): PIndex;
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
    Refine := (Level > MinLOD) and (Mask > 0) and
      IsVertexActive(VertexBuffer[Split(Tri.I, Tri.J, Tri.K)]);
  end;

  if Refine then
    LastIndex := SubMeshRefineVisible(LastIndex, Level - 1, ChildL(Tri.I, Tri.J, Tri.K), Mask);
  LastIndex := TriStripAppend(LastIndex, Tri.I, Boolean(Level and 1));
  if Refine then
    LastIndex := SubMeshRefineVisible(LastIndex, Level - 1, ChildR(Tri.I, Tri.J, Tri.K), Mask);

  Result := LastIndex;
end;

function TSoarTerrain.IsSphereVisible(const V: TVertex; Mask: LongWord): LongWord;
var
  I: LongInt;
  D, Rad: Single;
begin
  Result := Mask;
  Rad := V.Radius + 1.0;
  for I := 0 to 5 do
    if (Result and (1 shl I)) = 0 then
    begin
      D := Vec3Dot(Vec3Add(V.Pos, RefineShift), RefineFrustum[I].Normal) + RefineFrustum[I].D;
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
  Pos: TVector3;
begin
  if not ForceFullRefinement then
  begin
    Pos := Vec3Add(V.Pos, RefineShift);
    if ErrorMetric = emAnisotropic then
    begin
      ASqr := Sqr(RefineViewPoint.X - Pos.X) + Sqr(RefineViewPoint.Y - Pos.Y);
      Result := V.Error * V.Error * ASqr > Sqr(
        MaxFloat(0.0,  Kappa * (Vec3DistSqr(Pos, RefineViewPoint) - V.Radius * V.Radius) -
        V.Error * V.Radius));
    end
    else
      Result := Sqr(InverseKappa * V.Error + V.Radius) > Vec3DistSqr(Pos, RefineViewPoint);
  end
  else
    Result := True;
end;

procedure TSoarTerrain.AppendEdgeTriangles(Index1, Index2, VertexOffset, Index1Bottom, Index2Bottom: TIndex;
  var NumIndices: LongInt; var Indices: array of TIndex);
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
    if (VertexBuffer[ToAppend].Pos.X = CurrentTerrainSize - 1) and
      ((VertexBuffer[Head].Pos.X = CurrentTerrainSize - 1) or
       (VertexBuffer[Tail].Pos.X = CurrentTerrainSize - 1)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos.X = CurrentTerrainSize - 1, Head, Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, 0, Round(VertexBuffer[ToAppend].Pos.Y),
        Round(VertexBuffer[EdgeIdx].Pos.Y), NumPosXEdgeIndices, PosXEdgeIndices);
    end;
  end;
  if efPosY in RefineEdges then
  begin
    if (VertexBuffer[ToAppend].Pos.Y = CurrentTerrainSize - 1) and
      ((VertexBuffer[Head].Pos.Y = CurrentTerrainSize - 1) or
       (VertexBuffer[Tail].Pos.Y = CurrentTerrainSize - 1)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos.Y = CurrentTerrainSize - 1, Head, Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, CurrentTerrainSize, Round(VertexBuffer[ToAppend].Pos.X),
        Round(VertexBuffer[EdgeIdx].Pos.X), NumPosYEdgeIndices, PosYEdgeIndices);
    end;
  end;
  if efNegX in RefineEdges then
  begin
    if (VertexBuffer[ToAppend].Pos.X = 0) and
      ((VertexBuffer[Head].Pos.X = 0) or
       (VertexBuffer[Tail].Pos.X = 0)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos.X = 0, Head, Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, CurrentTerrainSize * 2,
        Round(VertexBuffer[ToAppend].Pos.Y), Round(VertexBuffer[EdgeIdx].Pos.Y),
        NumNegXEdgeIndices, NegXEdgeIndices);
    end;
  end;
  if efNegY in RefineEdges then
  begin
    if (VertexBuffer[ToAppend].Pos.Y = 0) and
      ((VertexBuffer[Head].Pos.Y = 0) or
       (VertexBuffer[Tail].Pos.Y = 0)) then
    begin
      EdgeIdx := Iff(VertexBuffer[Head].Pos.Y = 0, Head, Tail);
      AppendEdgeTriangles(ToAppend, EdgeIdx, CurrentTerrainSize * 3,
        Round(VertexBuffer[ToAppend].Pos.X), Round(VertexBuffer[EdgeIdx].Pos.X),
        NumNegYEdgeIndices, NegYEdgeIndices);
    end;
  end;
end;

function TSoarTerrain.TriStripBegin(FirstIndex: TIndex; Parity: Boolean): PIndex;
begin
  Result := @IndexBuffer[0];
  Result^ := FirstIndex;
  Inc(Result);
  Result^ := FirstIndex;
  Inc(Result);
  Result^ := TIndex(Parity);
  NumTriangles := 0;
end;

function TSoarTerrain.TriStripAppend(LastIndex: PIndex; ToAppend: TIndex; Parity: Boolean): PIndex;
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
  NumIndices := (LongWord(LastIndex) - LongWord(IndexBuffer)) div SizeOf(TIndex);

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
  Result := LinearIndex(1 shl (M - 1) , 1 shl (M - 1), M);
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
     inherited Create;
     FTerrainSize:=TerrainSize;
     FRenderTexture:=true;
     FRenderWireframe:=false;
     FterrainBlocks:=1;
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

procedure TSoarTerrain.RenderTerrain(X, Y: Integer;const ViewportWidth, FovX: Single;
  const ViewPoint: TVector3; const Frustum: TFrustum; var RefineTime,
  RenderTime: Int64);
var
  Time: Int64;
  ShiftVector: TVector3;
  EdgeFlags: TEdgeFlags;
begin
  ShiftVector := Vector3(X * (FTerrainSize - 1), Y * (FTerrainSize - 1), 0.0);
  EdgeFlags := [];

  if (X >= 0) and (X < FTerrainBlocks - 1) then Include(EdgeFlags, efPosX);
  if (X > 0) and (X <= FTerrainBlocks - 1) then Include(EdgeFlags, efNegX);
  if (Y >= 0) and (Y < FTerrainBlocks - 1) then Include(EdgeFlags, efPosY);
  if (Y > 0) and (Y <= FTerrainBlocks - 1) then Include(EdgeFlags, efNegY);

//  Time := GetTimeMicroseconds; MARCAT
  MeshRefine(ViewPortWidth, FovX, EdgeFlags, ViewPoint, ShiftVector, Frustum);
//  Inc(RefineTime, GetTimeMicroseconds - Time); MARCAT

  // Enable/disable textures
  if FRenderTexture then
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
    end;}
  end
  else
  begin
//    DisableTextures;   MARCAT
//    glColor4fv(@GroundColor); MARCAT
  end;

  if FRenderWireframe then
  begin
    glEnable(GL_POLYGON_OFFSET_FILL);
    glPolygonOffset(1.0, 4.0);
  end;

  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glTranslatef(ShiftVector.X, ShiftVector.Y, ShiftVector.Z);

  glVertexPointer(3, GL_FLOAT, SizeOf(TVertex), @VertexBuffer[0].Pos);

//  Time := GetTimeMicroseconds; MARCAT
  // Draw terrain block
  glDrawElements(GL_TRIANGLE_STRIP, NumIndices, GL_UNSIGNED_INT, IndexBuffer);
  // Draw block edges if necessary
  if efPosX in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumPosXEdgeIndices, GL_UNSIGNED_INT, PosXEdgeIndices);
  if efPosY in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumPosYEdgeIndices, GL_UNSIGNED_INT, PosYEdgeIndices);
  if efNegX in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumNegXEdgeIndices, GL_UNSIGNED_INT, NegXEdgeIndices);
  if efNegY in EdgeFlags then
    glDrawElements(GL_TRIANGLES, NumNegYEdgeIndices, GL_UNSIGNED_INT, NegYEdgeIndices);
//  Inc(RenderTime, GetTimeMicroseconds - Time); MARCAT

  if FRenderWireframe then
  begin
    glPushAttrib(GL_ENABLE_BIT or GL_COLOR_BUFFER_BIT or GL_POLYGON_BIT);
    glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_BLEND);
    glEnable(GL_LINE_SMOOTH);
//    DisableTextures; MARCAT
    glDrawElements(GL_TRIANGLE_STRIP, NumIndices, GL_UNSIGNED_INT, IndexBuffer);
    glPopAttrib;
    glDisable(GL_POLYGON_OFFSET_FILL);
  end;

  glPopMatrix;

//  Inc(TrianglesDrawn, NumTriangles);
end;

end.
