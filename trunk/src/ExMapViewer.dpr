program ExMapViewer;

uses
  ExceptionLog,
  Forms,
  ExMapViewerMain in 'ExMapViewerMain.pas' {ExMapViewerMainForm},
  GuiRAWView in 'GuiRAWView.pas' {RAWViewForm},
  StringFunction in 'StringFunction.pas',
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  SvnInfo in 'Lib\SvnInfo.pas',
  BGALib in 'BGALib.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TExMapViewerMainForm, ExMapViewerMainForm);
  Application.CreateForm(TRAWViewForm, RAWViewForm);
  Application.Run;
end.
