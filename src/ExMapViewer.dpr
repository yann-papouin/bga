program ExMapViewer;

uses
  Forms,
  ExMapViewerMain in 'ExMapViewerMain.pas' {ExMapViewerMainForm},
  GuiMapView in 'GuiMapView.pas' {MapViewForm},
  StringFunction in 'StringFunction.pas',
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  SvnInfo in 'Lib\SvnInfo.pas',
  BGALib in 'BGALib.pas',
  GuiFormCommon in 'GuiFormCommon.pas' {FormCommon},
  oge2_HashList in '..\misc\TerrainRenderer\lib\oge2_HashList.pas',
  oge2_HeightMap in '..\misc\TerrainRenderer\lib\oge2_HeightMap.pas',
  oge2_SOARDemo in '..\misc\TerrainRenderer\lib\oge2_SOARDemo.pas',
  oge2_TerrainDataSource in '..\misc\TerrainRenderer\lib\oge2_TerrainDataSource.pas',
  oge2_TerrainEngine in '..\misc\TerrainRenderer\lib\oge2_TerrainEngine.pas',
  oge2_TerrainRendering in '..\misc\TerrainRenderer\lib\oge2_TerrainRendering.pas',
  oge2_TerrainTileLodRenderer in '..\misc\TerrainRenderer\lib\oge2_TerrainTileLodRenderer.pas',
  oge2_TerrainTileLodVBORenderer in '..\misc\TerrainRenderer\lib\oge2_TerrainTileLodVBORenderer.pas',
  oge2_TerrainTileRender in '..\misc\TerrainRenderer\lib\oge2_TerrainTileRender.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExMapViewerMainForm, ExMapViewerMainForm);
  Application.CreateForm(TMapViewForm, MapViewForm);
  Application.Run;
end.
