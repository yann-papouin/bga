unit oge2_TerrainTileLodRenderer;

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
  TOGEHeightMapLODRender = class(TOGEBaseHeightMapRender)
  private
    VerticlesCount: Longint;
    VertexCoords: Array of TVertex;
    TexCoords: Array of TVector2f;
    procedure ClearQuadTree;
  public
    Constructor Create(aOwner: TOGEHeightMap); override;
    Destructor Destroy; override;
    Procedure BuildQuadTree; override;
    procedure RenderQuadTree(RenderTextures, RenderWireframe: Boolean);
      override;
  end;

implementation

{ TOGEHeightMapLODRender }

procedure TOGEHeightMapLODRender.BuildQuadTree;
var
  i, j, k: Integer;
  Vertex: TVertex;

  procedure AddVertex(ind: Integer; X, Y, Z, TX, TY: Single);
  begin
    Vertex[0] := X;
    Vertex[1] := Y;
    Vertex[2] := Z;
    VertexCoords[ind] := Vertex;
    TexCoords[ind][0] := TX;
    TexCoords[ind][1] := 1 - TY;
  end;

var
  ind: Integer;
begin
  ClearQuadTree;
  VerticlesCount := (HeightData.Size * 2) * (HeightData.Size - 1);
  SetLength(VertexCoords, VerticlesCount);
  SetLength(TexCoords, VerticlesCount);

  ind := -1;
  with HeightData do
  begin
    for j := 0 to HeightData.Size - 2 do
    begin
      if j mod 2 = 0 then
      begin
        for i := 0 to HeightData.Size - 1 do
        begin
          ind := ind + 1;
          AddVertex(ind, XLeft + i, YTop + j,
            HeightData.GetHeight(i, j) / 128, i / (HeightData.Size),
            j / (HeightData.Size));
          ind := ind + 1;
          AddVertex(ind, XLeft + i, YTop + j + 1,
            HeightData.GetHeight(i, j + 1) / 128, i / (HeightData.Size),
            (j + 1) / (HeightData.Size));
        end;
      end
      else
      begin
        for i := HeightData.Size - 1 downto 0 do
        begin
          ind := ind + 1;
          AddVertex(ind, XLeft + i, YTop + j + 1,
            HeightData.GetHeight(i, j + 1) / 128, i / (HeightData.Size),
            (j + 1) / (HeightData.Size));
          ind := ind + 1;
          AddVertex(ind, XLeft + i, YTop + j,
            HeightData.GetHeight(i, j) / 128, i / (HeightData.Size),
            j / (HeightData.Size));
        end;
      end;
    end;
  end;
end;

procedure TOGEHeightMapLODRender.ClearQuadTree;
var
  ii: Integer;
  Vertex: PVertex;
begin
  SetLength(VertexCoords, 0);
  SetLength(TexCoords, 0);
end;

constructor TOGEHeightMapLODRender.Create(aOwner: TOGEHeightMap);
begin
  inherited;
end;

destructor TOGEHeightMapLODRender.Destroy;
begin
  inherited;
end;

procedure TOGEHeightMapLODRender.RenderQuadTree(RenderTextures,
  RenderWireframe: Boolean);
begin
  GL.EnableClientState(GL_VERTEX_ARRAY);
  GL.VertexPointer(3, GL_FLOAT, 0, VertexCoords);
  if RenderTextures then
  begin
    GL.EnableClientState(GL_TEXTURE_COORD_ARRAY);
    GL.TexCoordPointer(2, GL_FLOAT, 0, TexCoords);
  end;
  GL.DrawArrays(GL_TRIANGLE_STRIP, 0, VerticlesCount);
  if RenderTextures then
  begin
    GL.DisableClientState(GL_TEXTURE_COORD_ARRAY);
  end;

  if RenderWireframe then
  begin
    GL.Disable(GL_TEXTURE_2D);
    GL.PolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    GL.Enable(GL_LINE_SMOOTH);
    GL.color4f(1, 1, 1, 1);
    GL.DrawArrays(GL_TRIANGLE_STRIP, 0, VerticlesCount);
  end;

  GL.DisableClientState(GL_VERTEX_ARRAY);

end;

end.
