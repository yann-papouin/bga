unit oge2_TerrainTileRender;

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
  oge2_HeightMap;

type
  TQuad = record
    V: Array [0 .. 3] of TVector3f;
  end;

  TQuad2 = Array [0 .. 3] of TVector3f;
  PQuad = ^TQuad;
  TVertex = TVector3f;
  PVertex = ^TVertex;

type
  TOGEBaseHeightMapRender = class(TObject)
  private
    FHighRes: Boolean;
    FHeightData: TOGEHeightMap; // Referred, not owned
    // procedure SetHeightMap(val : TOGEHeightMap);
  public
    constructor Create(aOwner: TOGEHeightMap); virtual;
    procedure RenderQuadTree(RenderTextures, RenderWireframe: Boolean); virtual;
    Procedure BuildQuadTree; virtual;
    property HeightData: TOGEHeightMap read FHeightData { write SetHeightMap } ;
  end;

  TTRenderer = class of TOGEBaseHeightMapRender;

type
  TOGEHeightMapRender = class(TOGEBaseHeightMapRender)
  private
    FQuadTree: TList;
    VerticlesCount: Longint;
    lst: Array of TQuad2;
    procedure ClearQuadTree;
  public
    Constructor Create(aOwner: TOGEHeightMap); override;
    Destructor Destroy; override;
    Procedure BuildQuadTree; override;
    procedure RenderQuadTree(RenderTextures, RenderWireframe: Boolean);
      override;
  end;

implementation

{ TOGEHeightMapRender }

procedure TOGEHeightMapRender.BuildQuadTree;
var
  ii, jj, kk: Integer;
  Quad: PQuad;
begin
  ClearQuadTree;
  VerticlesCount := HeightData.Size * HeightData.Size;
  SetLength(lst, VerticlesCount);
  kk := 0;
  with HeightData do
  begin
    for ii := 0 to HeightData.Size - 2 do
    begin
      for jj := 0 to HeightData.Size - 2 do
      begin
        lst[kk][0, 0] := HeightData.XLeft + ii;
        lst[kk][0, 1] := HeightData.YTop + jj;
        lst[kk][0, 2] := HeightData.GetHeight(ii, jj) / 128;
        lst[kk][1, 0] := HeightData.XLeft + ii + 1;
        lst[kk][1, 1] := HeightData.YTop + jj;
        lst[kk][1, 2] := HeightData.GetHeight(ii + 1, jj) / 128;
        lst[kk][2, 0] := HeightData.XLeft + ii + 1;
        lst[kk][2, 1] := HeightData.YTop + jj + 1;
        lst[kk][2, 2] := HeightData.GetHeight(ii + 1, jj + 1) / 128;
        lst[kk][3, 0] := HeightData.XLeft + ii;
        lst[kk][3, 1] := HeightData.YTop + jj + 1;
        lst[kk][3, 2] := HeightData.GetHeight(ii, jj + 1) / 128;
        kk := kk + 1;
        new(Quad);
        Quad^.V[0, 0] := HeightData.XLeft + ii;
        Quad^.V[0, 1] := HeightData.YTop + jj;
        Quad^.V[0, 2] := HeightData.GetHeight(ii, jj) / 128;
        Quad^.V[1, 0] := HeightData.XLeft + ii + 1;
        Quad^.V[1, 1] := HeightData.YTop + jj;
        Quad^.V[1, 2] := HeightData.GetHeight(ii + 1, jj) / 128;
        Quad^.V[2, 0] := HeightData.XLeft + ii + 1;
        Quad^.V[2, 1] := HeightData.YTop + jj + 1;
        Quad^.V[2, 2] := HeightData.GetHeight(ii + 1, jj + 1) / 128;
        Quad^.V[3, 0] := HeightData.XLeft + ii;
        Quad^.V[3, 1] := HeightData.YTop + jj + 1;
        Quad^.V[3, 2] := HeightData.GetHeight(ii, jj + 1) / 128;
        FQuadTree.Add(Quad);
      end;
    end;
  end;
end;

procedure TOGEHeightMapRender.ClearQuadTree;
var
  ii: Integer;
  Quad: PQuad;
begin
  for ii := 0 to FQuadTree.Count - 1 do
  begin
    Quad := FQuadTree.Items[ii];
    dispose(Quad);
    Quad := nil;
  end;
  FQuadTree.Clear;
end;

constructor TOGEHeightMapRender.Create(aOwner: TOGEHeightMap);
begin
  inherited;
  FQuadTree := TList.Create;
end;

destructor TOGEHeightMapRender.Destroy;
begin
  FQuadTree.Free;
  inherited;
end;

procedure TOGEHeightMapRender.RenderQuadTree(RenderTextures, RenderWireframe: Boolean);
var
  ii, jj: Integer;
  Quad: PQuad;
begin
 (*
  GL.VertexPointer( 3, GL_FLOAT, length(lst), @lst[0] );
  GL.DrawElements(GL_Triangle_Strip,VerticlesCount,GL_FLOAT,@lst[0]);
  *)
  for ii := 0 to FQuadTree.Count - 1 do
  begin
    Quad := FQuadTree.Items[ii];
    for jj := 0 to 3 do
    begin
      GL.Vertex3fv(@Quad^.V[jj]);
    end;
  end;

end;

{ TOGEBaseHeightMapRender }

procedure TOGEBaseHeightMapRender.BuildQuadTree;
begin
  // nothing now;
end;

constructor TOGEBaseHeightMapRender.Create;
begin
  inherited Create;
  FHeightData := aOwner;
  // nothing now;
end;

procedure TOGEBaseHeightMapRender.RenderQuadTree;
begin
  // nothing now;
end;

{ procedure TOGEBaseHeightMapRender.SetHeightMap(val : TOGEHeightMap);
  begin
  FHeightData:=val;

  end; }

end.
