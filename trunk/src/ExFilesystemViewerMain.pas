unit ExFilesystemViewerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TExFilesystemViewerMainForm = class(TForm)
    procedure FormActivate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  ExFilesystemViewerMainForm: TExFilesystemViewerMainForm;

implementation

{$R *.dfm}

uses
  GuiFsView;

procedure TExFilesystemViewerMainForm.FormActivate(Sender: TObject);
begin
  FSViewForm.ShowModal;
  Close;
end;

end.
