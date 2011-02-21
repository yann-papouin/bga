unit oge2_TerrainTileLodVBORenderer;

interface

uses
  Classes,
  OpenGLTokens,
  OpenGLAdapter,
  GLContext,
  GLState,
  VectorTypes,
  VectorGeometry,
  VectorLists,
  oge2_HeightMap,
  oge2_TerrainTileRender;

type
  TOGEHeightMapLODVBORender = class(TOGEBaseHeightMapRender)
  private
    FVertexHandle: GLUint;
    FTexCoordsHandle: GLUint;
    VerticlesCount: Longint;
    VertexCoords: Array of TVertex;
    TexCoords: Array of TVector2f;
  protected
    procedure ClearQuadTree;
  public
    Constructor Create(aOwner: TOGEHeightMap); override;
    Destructor Destroy; override;
    Procedure BuildQuadTree; override;
    procedure RenderQuadTree(RenderTextures, RenderWireframe: Boolean); override;
  end;

implementation

{ TOGEHeightMapLODVBORender }

procedure TOGEHeightMapLODVBORender.BuildQuadTree;
var
  i, j, k: Integer;
  Vertex: TVertex;

  X, Y, Z :Single;
  U, V : Single;

  UVDiv : Single;

  procedure AddVertex(ind: Integer; vX, vY, vZ, tU, tV: Single);
  begin
    Vertex[0] := vX;
    Vertex[1] := vY;
    Vertex[2] := vZ;

    VertexCoords[ind] := Vertex;
    TexCoords[ind][0] := tU;
    TexCoords[ind][1] := 1 - tV;
  end;

const
  RATIO_HEIGHT = 128;
var
  ind: Integer;
begin
  VerticlesCount := (HeightData.Size * 2) * (HeightData.Size - 1);
  SetLength(VertexCoords, VerticlesCount);
  SetLength(TexCoords, VerticlesCount);

  ind := -1;
  (*
  with HeightData do
  begin
    for j := 0 to HeightData.Size - 2 do
    begin
      if j mod 2 = 0 then
      begin
        for i := 0 to HeightData.Size - 1 do
        begin
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j, HeightData.GetHeight(i, j) / RATIO_HEIGHT, i / (HeightData.Size), j / (HeightData.Size));
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j + 1, HeightData.GetHeight(i, j + 1) / RATIO_HEIGHT, i / (HeightData.Size), (j + 1) / (HeightData.Size));
        end;
      end
        else
      begin
        for i := HeightData.Size - 1 downto 0 do
        begin
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j + 1, HeightData.GetHeight(i, j + 1) / RATIO_HEIGHT, i / (HeightData.Size), (j + 1) / (HeightData.Size));
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j, HeightData.GetHeight(i, j) / RATIO_HEIGHT, i / (HeightData.Size), j / (HeightData.Size));
        end;
      end;
    end;
  end;
  *)

  UVDiv := HeightData.Size; // HeightData.Size-1;
{
  with HeightData do
  begin
    for j := 0 to HeightData.Size - 2 do
    begin
      for i := 0 to HeightData.Size - 1 do
      begin

        if (j mod 2 = 0) and (i mod 2 = 0) then
        begin
          X := XLeft +i;
          Y := YTop  +j;
          Z := HeightData.GetHeight(i, j) / RATIO_HEIGHT;

          U := i / UVDiv;
          V := j / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);


          X := XLeft +i;
          Y := YTop  +j+1;
          Z := HeightData.GetHeight(i, j+1) / RATIO_HEIGHT;

          U := i     / UVDiv;
          V := (j+1) / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);

          X := XLeft +i+1;
          Y := YTop  +j+1;
          Z := HeightData.GetHeight(i+1, j+1) / RATIO_HEIGHT;

          U := (i+1) / UVDiv;
          V := (j+1) / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);

          X := XLeft +i+1;
          Y := YTop  +j;
          Z := HeightData.GetHeight(i+1, j) / RATIO_HEIGHT;

          U := (i+1) / UVDiv;
          V := j / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);

        end;

      end;
    end;
  end;
}

  with HeightData do
  begin
    for j := 0 to HeightData.Size - 2 do
    begin
      if j mod 2 = 0 then
      begin
        for i := 0 to HeightData.Size - 1 do
        begin

          X := XLeft +i;
          Y := YTop  +j;
          Z := HeightData.GetHeight(i, j) / RATIO_HEIGHT;

          U := i / UVDiv;
          V := j / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);


          X := XLeft +i;
          Y := YTop  +j+1;
          Z := HeightData.GetHeight(i, j+1) / RATIO_HEIGHT;

          U := i     / UVDiv;
          V := (j+1) / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);

          (*
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j, HeightData.GetHeight(i, j) / RATIO_HEIGHT, i / (HeightData.Size), j / (HeightData.Size));
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j + 1, HeightData.GetHeight(i, j + 1) / RATIO_HEIGHT, i / (HeightData.Size), (j + 1) / (HeightData.Size));
          *)
        end;
      end
        else
      begin
        for i := HeightData.Size - 1 downto 0 do
        begin

          X := XLeft +i;
          Y := YTop  +j+1;
          Z := HeightData.GetHeight(i, j+1) / RATIO_HEIGHT;

          U := i     / UVDiv;
          V := (j+1) / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);

          X := XLeft +i;
          Y := YTop  +j;
          Z := HeightData.GetHeight(i, j) / RATIO_HEIGHT;

          U := i / UVDiv;
          V := j / UVDiv;

          Inc(ind);
          AddVertex(ind, X, Y, Z, U, V);


          (*
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j + 1, HeightData.GetHeight(i, j + 1) / RATIO_HEIGHT, i / (HeightData.Size), (j + 1) / (HeightData.Size));
          Inc(ind);
          AddVertex(ind, XLeft + i, YTop + j, HeightData.GetHeight(i, j) / RATIO_HEIGHT, i / (HeightData.Size), j / (HeightData.Size));
          *)
        end;
      end;
    end;
  end;


  GL.BindBuffer(GL_ARRAY_BUFFER, FVertexHandle);
  GL.BufferData(GL_ARRAY_BUFFER, VerticlesCount * 3 * 4, VertexCoords, GL_STATIC_DRAW);
  GL.BindBuffer(GL_ARRAY_BUFFER, FTexCoordsHandle);
  GL.BufferData(GL_ARRAY_BUFFER, VerticlesCount * 2 * 4, TexCoords, GL_STATIC_DRAW);
  GL.BindBuffer(GL_ARRAY_BUFFER, 0);
  ClearQuadTree;
end;

procedure TOGEHeightMapLODVBORender.ClearQuadTree;
var
  ii: Integer;
  Vertex: PVertex;
begin
  SetLength(VertexCoords, 0);
  SetLength(TexCoords, 0);
end;

constructor TOGEHeightMapLODVBORender.Create(aOwner: TOGEHeightMap);
begin
  inherited;
  GL.GenBuffers(1, @FVertexHandle);
  GL.GenBuffers(1, @FTexCoordsHandle);
end;

destructor TOGEHeightMapLODVBORender.Destroy;
begin
  ClearQuadTree;
  GL.DeleteBuffers(1, @FVertexHandle);
  GL.DeleteBuffers(1, @FTexCoordsHandle);
  inherited;
end;

procedure TOGEHeightMapLODVBORender.RenderQuadTree(RenderTextures, RenderWireframe: Boolean);
begin


  if RenderTextures then
  begin
    GL.Enable(GL_TEXTURE_2D);
    GL.PolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    GL.EnableClientState(GL_TEXTURE_COORD_ARRAY);
    GL.BindBuffer(GL_ARRAY_BUFFER, FTexCoordsHandle);
    GL.TexCoordPointer(2, GL_FLOAT, 0, 0);
  end;

  GL.BindBuffer(GL_ARRAY_BUFFER, FVertexHandle);
  GL.EnableClientState(GL_VERTEX_ARRAY);
  GL.VertexPointer(3, GL_FLOAT, 0, 0);
  GL.DrawArrays(GL_TRIANGLE_STRIP, 0, VerticlesCount);
  GL.DisableClientState(GL_VERTEX_ARRAY);

  if RenderTextures then
  begin
    GL.DisableClientState(GL_TEXTURE_COORD_ARRAY);
  end;
(*
  if RenderWireframe then
  begin

    GL.Disable(GL_TEXTURE_2D);
    GL.PolygonMode(GL_FRONT_AND_BACK, GL_FILL);
    GL.Enable(GL_LINE_SMOOTH);
    GL.Color4f(1, 0, 0, 0);
    GL.EnableClientState(GL_VERTEX_ARRAY);
    GL.VertexPointer(3, GL_FLOAT, 0, 0);
    GL.DrawArrays(GL_TRIANGLE_STRIP, 0, VerticlesCount);
    GL.DisableClientState(GL_VERTEX_ARRAY);
  end;
*)
  GL.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

end.
