program ExFilesystemViewer;

uses
  Forms,
  GuiFSSettings in 'GuiFSSettings.pas' {FSSettingsForm},
  GuiFSView in 'GuiFSView.pas' {FSViewForm},
  GuiFormCommon in 'GuiFormCommon.pas' {FormCommon},
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  SvnInfo in 'Lib\SvnInfo.pas',
  Resources in 'Resources.pas' {ResourcesForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFSViewForm, FSViewForm);
  Application.CreateForm(TFSSettingsForm, FSSettingsForm);
  Application.CreateForm(TFormCommon, FormCommon);
  Application.CreateForm(TResourcesForm, ResourcesForm);
  Application.Run;
end.
