unit oge2_TerrainDataSource;

interface

uses
  oge2_HashList,
  oge2_HeightMap,
  Classes;

Type
  TOGEDataSourceStartPreparingData = procedure(HeightData: TOGEHeightMap)
    of Object;

type
  TOGEHeightDataSource = class(TObject)
  private
    FTilesList: THashTable;
    FTilesCache: TList;
    FOnStartPreparingData: TOGEDataSourceStartPreparingData;
    FVisibleDistance: Integer;
    FTileSize: Integer;
    Procedure BuildTilesCache;
    procedure ClearTilesCache;
  public
    Constructor Create;
    Destructor Destroy; override;
    procedure MarkDirty(x1, y1, x2, y2: Integer);
    Function GetTile(X, Y: Integer): TOGEHeightMap;
    Procedure RemoveTile(X, Y: Integer);
    function InterpolatedHeight(X, Y: Single; tileSize: Integer): Single;
  published
    property TilesCache: TList read FTilesCache;
    property OnStartPreparingData
      : TOGEDataSourceStartPreparingData read FOnStartPreparingData write
      FOnStartPreparingData;
  end;

implementation

uses
  SysUtils,
  Math,
  oge2_TerrainTileRender;

Function GetTileIndex(X, Y: Integer): String;
begin
  result := 'T_' + inttostr(X) + '_' + inttostr(Y);
end;
{ TOGEHeightDataSource }

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

constructor TOGEHeightDataSource.Create;
begin
  FTilesList := THashTable.Create;
  FTilesCache := TList.Create;
  FTileSize := 64;
end;

destructor TOGEHeightDataSource.Destroy;
begin
  FTilesList.Free;
  FTilesCache.Free;
  inherited;
end;

procedure TOGEHeightDataSource.MarkDirty(x1, y1, x2, y2: Integer);
var
  ii, jj: Integer;
  TempTile: TOGEHeightMap;
begin
  for ii := floor(x1 / FTileSize) to Ceil(x2 / FTileSize) - 1 do
  begin
    for jj := floor(y1 / FTileSize) to Ceil(y2 / FTileSize) - 1 do
    begin
      TempTile := GetTile(ii * FTileSize, jj * FTileSize);
      if Assigned(TempTile.Renderer) then
      begin
        TempTile.Renderer.Free;
        TempTile.Renderer := nil;
      end;
      if Assigned(FOnStartPreparingData) then
        FOnStartPreparingData(TempTile);
      if TempTile.DataState <> dsReady then
        RemoveTile(ii * FTileSize, jj * FTileSize);
    end;
  end;
  BuildTilesCache;
end;

function TOGEHeightDataSource.InterpolatedHeight(X, Y: Single;
  tileSize: Integer): Single;
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
      if (hd.XLeft <= X) and (hd.YTop <= Y) and (hd.XLeft + hd.Size - 1 > X)
        and (hd.YTop + hd.Size - 1 > Y) then
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
  result := 0;
  if Assigned(foundHd) then
    if foundHd.DataState = dsNone then
      result := { DefaultHeight } 0
    else
      result := foundHd.GetHeight(round(X - foundHd.XLeft),
        round(Y - foundHd.YTop));
end;

function TOGEHeightDataSource.GetTile(X, Y: Integer): TOGEHeightMap;
var
  TempTile: TOGEHeightMap;
begin
  TempTile := nil;
  if Assigned(FTilesList.Get(GetTileIndex(X, Y))) = false then
  begin
    TempTile := TOGEHeightMap.Create(X, Y, FTileSize + 1);
    FTilesList.Add(GetTileIndex(X, Y), TempTile);
  end
  else
  begin
    TempTile := TOGEHeightMap(FTilesList.Get(GetTileIndex(X, Y)));
  end;
  result := TempTile;
end;

procedure TOGEHeightDataSource.RemoveTile(X, Y: Integer);
begin
  if Assigned(FTilesList.Get(GetTileIndex(X, Y))) then
  begin
    FTilesList.Delete(GetTileIndex(X, Y));
  end;
end;

end.
