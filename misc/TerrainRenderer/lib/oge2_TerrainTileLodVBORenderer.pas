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

uses
  Sysutils, DbugIntf;

{$Define TRIANGLE_RENDERING}
{.$Define TRIANGLE_STRIP_RENDERING}

procedure TOGEHeightMapLODVBORender.BuildQuadTree;
const
  RATIO_HEIGHT = 128;

var
  ind: Integer;
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

  procedure ExAddVertex( Xi, Yj : Integer);
  begin
    X := HeightData.XLeft + Xi;
    Y := HeightData.YTop  + Yj;
    Z := HeightData.GetHeight(Xi, Yj) / RATIO_HEIGHT;

    U := Xi / UVDiv;
    V := Yj / UVDiv;

    Inc(ind);
    Assert(ind < VerticlesCount, Format('ExAddVertex error with %d >= %d',[ind, VerticlesCount]) );
    AddVertex(ind, X, Y, Z, U, V);
  end;

var

  i, j, k: Integer;

begin
 //
  {$IfDef TRIANGLE_RENDERING}
  VerticlesCount := ((HeightData.Size-1) * 2) * ((HeightData.Size-1) * 2) + ((HeightData.Size-1)*(HeightData.Size-1) * 2);
  {$EndIf}

  {$IfDef TRIANGLE_STRIP_RENDERING}
  VerticlesCount := (HeightData.Size * 2) * (HeightData.Size - 1)*2;
  {$EndIf}

  SetLength(VertexCoords, VerticlesCount);
  SetLength(TexCoords, VerticlesCount);

  ind := -1;

  UVDiv := HeightData.Size-1;

  {$IfDef TRIANGLE_RENDERING}

  for j := 0 to HeightData.Size - 1 - 1 do
  begin
    for i := 0 to HeightData.Size - 1 - 1 do
    begin
      ExAddVertex(i,j);
      ExAddVertex(i,j+1);
      ExAddVertex(i+1,j+1);

      ExAddVertex(i,j);
      ExAddVertex(i+1,j+1);
      ExAddVertex(i+1,j);
    end;
  end;

  {$EndIf}

  {$IfDef TRIANGLE_STRIP_RENDERING}

  for j := 0 to HeightData.Size - 2 do
  begin
    for i := 0 to HeightData.Size - 2 do
    begin
      ExAddVertex(i,j);
      ExAddVertex(i,j+1);
      ExAddVertex(i+1,j+1);
      ExAddVertex(i+1,j);
    end;
  end;
(*
  for j := 0 to HeightData.Size - 2 do
  begin
    if j mod 2 = 0 then
    begin
      for i := 0 to HeightData.Size - 1 do
      begin
        ExAddVertex(i,j);
        ExAddVertex(i,j+1);
      end;
    end
      else
    begin
      for i := HeightData.Size - 1 downto 0 do
      begin
        ExAddVertex(i,j+1);
        ExAddVertex(i,j);
      end;
    end;
  end;
*)
  {$EndIf}
 (*
  if ind > 0 then
    Assert(ind = VerticlesCount-1, Format('ExAddVertex error with %d > %d',[VerticlesCount, ind+1]) );
 *)
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

    GL.ClientActiveTexture(GL_TEXTURE1);
    GL.EnableClientState(GL_TEXTURE_COORD_ARRAY);
    GL.BindBuffer(GL_ARRAY_BUFFER, FTexCoordsHandle);
    GL.TexCoordPointer(2, GL_FLOAT, 0, 0);

   // GL.ActiveTexture(GL_TEXTURE1);
  //  GL.Enable(GL_TEXTURE_2D);
  end;

  GL.BindBuffer(GL_ARRAY_BUFFER, FVertexHandle);
  GL.EnableClientState(GL_VERTEX_ARRAY);
  GL.VertexPointer(3, GL_FLOAT, 0, 0);


  {$IfDef TRIANGLE_RENDERING}
  GL.DrawArrays(GL_TRIANGLES, 0, VerticlesCount);
  {$EndIf}

  {$IfDef TRIANGLE_STRIP_RENDERING}
  GL.DrawArrays(GL_TRIANGLE_STRIP, 0, VerticlesCount);
  {$EndIf}

  GL.DisableClientState(GL_VERTEX_ARRAY);

//  GL.Disable(GL_TEXTURE_2D);
 // GL.ActiveTexture(GL_TEXTURE0);

 // GL.Disable(GL_TEXTURE_2D);


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
