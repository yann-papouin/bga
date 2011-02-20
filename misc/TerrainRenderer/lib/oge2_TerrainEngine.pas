unit oge2_TerrainEngine;

interface

uses
  oge2_TerrainTileRender;

Type
  TOGETerrainEngine = class(TObject)
  private
    FTileRenderer: TTRenderer;
  public
    procedure SetTrianglesCapacity(nb: Integer); virtual;
    function GetTrianglesCapacity: Integer; virtual;
    property TileRenderer: TTRenderer read FTileRenderer write FTileRenderer;
  end;

implementation

{ TOGETerrainEngine }

function TOGETerrainEngine.GetTrianglesCapacity: Integer;
begin

end;

procedure TOGETerrainEngine.SetTrianglesCapacity(nb: Integer);
begin

end;

end.
