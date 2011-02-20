unit oge2_HeightMap;

interface

Type
  THeight = SmallInt;
  PHeight = ^THeight;
  TSmallintArray = array [0 .. MaxInt shr 2] of SmallInt;
  PSmallIntArray = ^TSmallintArray;
  TSmallIntRaster = array [0 .. MaxInt shr 3] of PSmallIntArray;
  PSmallIntRaster = ^TSmallIntRaster;
  TOGEHeightMapDataState = (dsPreparing, dsNone, dsReady);

Type
  TOGEHeightMap = class(TObject)
  private
    FAllocated: Boolean;
    FSize: Integer;
    FDataSize: Integer;
    FXLeft, FYTop: Integer;
    FRenderer: TObject;
    FSmallIntData: PSmallIntArray;
    FSmallIntRaster: PSmallIntRaster;
    FMaterialName: String;
    FDataState: TOGEHeightMapDataState;
    procedure ClearTile;
    procedure BuildSmallIntRaster;
  public
    Constructor Create(X, Y, TileSize: Integer);
    Function GetHeight(X, Y: Integer): THeight;
    procedure SetHeight(X, Y: Integer; Value: THeight);
    Destructor Destroy; override;

    procedure Allocate; dynamic;
    procedure DeAllocate; dynamic;
    property DataState: TOGEHeightMapDataState read FDataState write FDataState;
    property XLeft: Integer read FXLeft;
    property YTop: Integer read FYTop;
    property Size: Integer read FSize;
    property Renderer: TObject read FRenderer write FRenderer;
    property SmallIntData: PSmallIntArray read FSmallIntData;
    property SmallIntRaster: PSmallIntRaster read FSmallIntRaster;
    property MaterialName: String read FMaterialName write FMaterialName;
  end;

implementation

{ TOGEHeightMap }

procedure TOGEHeightMap.BuildSmallIntRaster;
var
  i: Integer;
begin
  GetMem(FSmallIntRaster, Size * SizeOf(PSmallIntArray));
  for i := 0 to Size - 1 do
    FSmallIntRaster^[i] := @FSmallIntData[i * Size]
end;

procedure TOGEHeightMap.Allocate;
begin
  if FDataSize = 0 then
  begin
    FDataSize := (Size) * (Size) * SizeOf(SmallInt);
    GetMem(FSmallIntData, FDataSize);
    BuildSmallIntRaster;
  end;
end;

procedure TOGEHeightMap.ClearTile;
begin
  if FAllocated then
  begin
    FAllocated := false;
    FSize := 0;
  end;
end;

constructor TOGEHeightMap.Create(X, Y, TileSize: Integer);
begin
  FXLeft := X;
  FYTop := Y;
  FSize := TileSize;
  DataState := dsNone;
end;

destructor TOGEHeightMap.Destroy;
begin
  DeAllocate;
  ClearTile;
  inherited;
end;

procedure TOGEHeightMap.SetHeight(X, Y: Integer; Value: THeight);
begin
  SmallIntRaster^[Y]^[X] := Value;
end;

function TOGEHeightMap.GetHeight(X, Y: Integer): THeight;
begin
  Result := 0;
  if FDataSize > 0 then
    if (X >= 0) and (Y >= 0) and (X < Size) and (Y < Size) then
      result := SmallIntRaster^[Y]^[X];
end;

procedure TOGEHeightMap.DeAllocate;
begin
  FreeMem(FSmallIntData);
  FreeMem(FSmallIntRaster);
  FDataSize := 0;
end;

end.
