program BGA;

uses
  ExceptionLog,
  Forms,
  GuiRFASettings in 'GuiRFASettings.pas' {RFASettingsForm},
  GuiRFAView in 'GuiRFAView.pas' {RFAViewForm},
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
  GuiSkinDialog in 'GuiSkinDialog.pas' {SkinDialogForm};

{$R *.res}

begin
  //ReportMemoryLeaksOnShutdown := DebugHook <> 0;

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
  Application.Run;
end.
