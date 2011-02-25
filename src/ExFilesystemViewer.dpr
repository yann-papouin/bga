program ExFilesystemViewer;

uses
  ExceptionLog,
  Forms,
  GuiFSSettings in 'GuiFSSettings.pas' {FSSettingsForm},
  GuiFSView in 'GuiFSView.pas' {FSViewForm},
  GuiFormCommon in 'GuiFormCommon.pas' {FormCommon},
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  SvnInfo in 'Lib\SvnInfo.pas',
  Resources in 'Resources.pas' {ResourcesForm},
  FSLib in 'FSLib.pas',
  MD5Api in 'Lib\MD5Api.pas',
  MD5Core in 'Lib\MD5Core.pas',
  MiniLZO in 'Lib\MiniLZO.pas',
  GuiWait in 'GuiWait.pas' {WaitForm},
  GuiSkinDialog in 'GuiSkinDialog.pas' {SkinDialogForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFSViewForm, FSViewForm);
  Application.CreateForm(TFSSettingsForm, FSSettingsForm);
  Application.CreateForm(TFormCommon, FormCommon);
  Application.CreateForm(TResourcesForm, ResourcesForm);
  Application.CreateForm(TWaitForm, WaitForm);
  Application.CreateForm(TSkinDialogForm, SkinDialogForm);
  Application.Run;
end.
