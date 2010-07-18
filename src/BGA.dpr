program BGA;

uses
  ExceptionLog,
  Forms,
  GuiRFASettings in 'GuiRFASettings.pas' {RFASettingsForm},
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  MD5Api in 'Lib\MD5Api.pas',
  MD5Core in 'Lib\MD5Core.pas',
  MiniLZO in 'Lib\MiniLZO.pas',
  RFALib in 'RFALib.pas',
  StringFunction in 'StringFunction.pas',
  GuiRAWView in 'GuiRAWView.pas' {RAWViewForm},
  CONLib in 'CONLib.pas',
  GuiAbout in 'GuiAbout.pas' {AboutForm},
  GuiSMView in 'GuiSMView.pas' {SMViewForm},
  FileSM in 'GLSceneExt\FileSM.pas',
  GLFileSM in 'GLSceneExt\GLFileSM.pas',
  TypesSM in 'GLSceneExt\TypesSM.pas',
  SvnInfo in 'Lib\SvnInfo.pas',
  GuiUpdateManager in 'GuiUpdateManager.pas' {UpdateManagerForm},
  GuiBrowse in 'GuiBrowse.pas' {BrowseForm},
  GuiSkinDialog in 'GuiSkinDialog.pas' {SkinDialogForm},
  GuiBrowsePack in 'GuiBrowsePack.pas' {BrowsePackForm},
  GuiBrowseExtract in 'GuiBrowseExtract.pas' {BrowseExtractForm},
  GuiRFACommon in 'GuiRFACommon.pas' {RFACommonForm},
  Resources in 'Resources.pas' {ResourcesForm},
  GuiRFAView in 'GuiRFAView.pas' {RFAViewForm},
  GuiFSView in 'GuiFSView.pas' {FSViewForm},
  GuiFSSettings in 'GuiFSSettings.pas' {FSSettingsForm},
  FSLib in 'FSLib.pas',
  VirtualTreeviewTheme in 'Lib\VirtualTreeviewTheme.pas',
  GuiFormCommon in 'GuiFormCommon.pas' {FormCommon},
  GuiPicView in 'GuiPicView.pas' {PICViewForm},
  UAC in 'UAC.pas',
  BGALib in 'BGALib.pas',
  RSLib in 'RSLib.pas';

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  if UAC.Perform then
  begin

  end
    else
  begin
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'BGA : Battlefield 1942 Game Archive Tool';
    Application.CreateForm(TRFAViewForm, RFAViewForm);
  Application.CreateForm(TRFASettingsForm, RFASettingsForm);
  Application.CreateForm(TRAWViewForm, RAWViewForm);
  Application.CreateForm(TAboutForm, AboutForm);
  Application.CreateForm(TSMViewForm, SMViewForm);
  Application.CreateForm(TUpdateManagerForm, UpdateManagerForm);
  Application.CreateForm(TBrowseForm, BrowseForm);
  Application.CreateForm(TSkinDialogForm, SkinDialogForm);
  Application.CreateForm(TBrowsePackForm, BrowsePackForm);
  Application.CreateForm(TBrowseExtractForm, BrowseExtractForm);
  Application.CreateForm(TRFACommonForm, RFACommonForm);
  Application.CreateForm(TResourcesForm, ResourcesForm);
  Application.CreateForm(TFSViewForm, FSViewForm);
  Application.CreateForm(TFSSettingsForm, FSSettingsForm);
  Application.CreateForm(TFormCommon, FormCommon);
  Application.CreateForm(TPICViewForm, PICViewForm);
  Application.Run;
  end;
end.
