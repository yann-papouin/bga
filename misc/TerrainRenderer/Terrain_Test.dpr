program Terrain_Test;
//{$APPTYPE CONSOLE}
uses
  ExceptionLog,
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  oge2_HashList in 'lib\oge2_HashList.pas',
  oge2_HeightMap in 'lib\oge2_HeightMap.pas',
  oge2_SOARDemo in 'lib\oge2_SOARDemo.pas',
  oge2_TerrainDataSource in 'lib\oge2_TerrainDataSource.pas',
  oge2_TerrainEngine in 'lib\oge2_TerrainEngine.pas',
  oge2_TerrainRendering in 'lib\oge2_TerrainRendering.pas',
  oge2_TerrainTileLodRenderer in 'lib\oge2_TerrainTileLodRenderer.pas',
  oge2_TerrainTileLodVBORenderer in 'lib\oge2_TerrainTileLodVBORenderer.pas',
  oge2_TerrainTileRender in 'lib\oge2_TerrainTileRender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
