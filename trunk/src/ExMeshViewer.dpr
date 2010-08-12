program ExMeshViewer;

uses
  ExceptionLog,
  Forms,
  ExMeshViewerMain in 'ExMeshViewerMain.pas' {ExMeshViewerMainForm},
  GuiSMView in 'GuiSMView.pas' {SMViewForm},
  BGALib in 'BGALib.pas',
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  SvnInfo in 'Lib\SvnInfo.pas',
  VirtualTreeviewTheme in 'Lib\VirtualTreeviewTheme.pas',
  FileSM in 'GLSceneExt\FileSM.pas',
  GLFileSM in 'GLSceneExt\GLFileSM.pas',
  TypesSM in 'GLSceneExt\TypesSM.pas',
  RSLib in 'RSLib.pas',
  GuiFormCommon in 'GuiFormCommon.pas' {FormCommon},
  Resources in 'Resources.pas' {ResourcesForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExMeshViewerMainForm, ExMeshViewerMainForm);
  Application.CreateForm(TSMViewForm, SMViewForm);
  Application.CreateForm(TResourcesForm, ResourcesForm);
  Application.Run;
end.
