program ExPictureViewer;

uses
  Forms,
  ExPictureViewerMain in 'ExPictureViewerMain.pas' {ExPictureViewerMainForm},
  GuiPicView in 'GuiPicView.pas' {PICViewForm},
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  SvnInfo in 'Lib\SvnInfo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExPictureViewerMainForm, ExPictureViewerMainForm);
  Application.CreateForm(TPICViewForm, PICViewForm);
  Application.Run;
end.
