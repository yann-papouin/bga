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
    procedure RenderQuadTree(RenderTextures, RenderWireframe: Boolean);
      override;
  end;

implementation

{ TOGEHeightMapLODVBORender }

procedure TOGEHeightMapLODVBORender.BuildQuadTree;
var
  ii, jj, kk: Integer;
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
  VerticlesCount := (HeightData.Size * 2) * (HeightData.Size - 1);
  SetLength(VertexCoords, VerticlesCount);
  SetLength(TexCoords, VerticlesCount);

  ind := -1;
  with HeightData do
  begin
    for jj := 0 to HeightData.Size - 2 do
    begin
      if jj mod 2 = 0 then
      begin
        for ii := 0 to HeightData.Size - 1 do
        begin
          ind := ind + 1;
          AddVertex(ind, XLeft + ii, YTop + jj,
            HeightData.GetHeight(ii, jj) / 128, ii / (HeightData.Size),
            jj / (HeightData.Size));
          ind := ind + 1;
          AddVertex(ind, XLeft + ii, YTop + jj + 1,
            HeightData.GetHeight(ii, jj + 1) / 128, ii / (HeightData.Size),
            (jj + 1) / (HeightData.Size));
        end;
      end
      else
      begin
        for ii := HeightData.Size - 1 downto 0 do
        begin
          ind := ind + 1;
          AddVertex(ind, XLeft + ii, YTop + jj + 1,
            HeightData.GetHeight(ii, jj + 1) / 128, ii / (HeightData.Size),
            (jj + 1) / (HeightData.Size));
          ind := ind + 1;
          AddVertex(ind, XLeft + ii, YTop + jj,
            HeightData.GetHeight(ii, jj) / 128, ii / (HeightData.Size),
            jj / (HeightData.Size));
        end;
      end;
    end;
  end;

  GL.BindBuffer(GL_ARRAY_BUFFER, FVertexHandle);
  GL.BufferData(GL_ARRAY_BUFFER, VerticlesCount * 3 * 4, VertexCoords,
    GL_STATIC_DRAW);
  GL.BindBuffer(GL_ARRAY_BUFFER, FTexCoordsHandle);
  GL.BufferData(GL_ARRAY_BUFFER, VerticlesCount * 2 * 4, TexCoords,
    GL_STATIC_DRAW);
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

  if RenderWireframe then
  begin

    GL.Disable(GL_TEXTURE_2D);
    GL.PolygonMode(GL_FRONT_AND_BACK, GL_LINE);
    GL.Enable(GL_LINE_SMOOTH);
    GL.Color4f(1, 1, 1, 1);
    GL.DrawArrays(GL_TRIANGLE_STRIP, 0, VerticlesCount);
  end;

  GL.BindBuffer(GL_ARRAY_BUFFER, 0);
end;

end.
