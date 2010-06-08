unit ExPictureViewerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TExPictureViewerMainForm = class(TForm)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  ExPictureViewerMainForm: TExPictureViewerMainForm;

implementation

{$R *.dfm}

uses
  GuiPicView;

procedure TExPictureViewerMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  PICViewForm.LoadTexture('C:\Windows\winsxs\x86_microsoft-windows-photosamples_31bf3856ad364e35_6.1.7600.16385_none_974f72e1e322d188\Penguins.jpg');
  PICViewForm.Preview;
end;

end.
