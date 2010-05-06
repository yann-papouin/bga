unit GuiAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpTBXItem, SpTBXControls, ExtCtrls, pngimage, ActnList, StdCtrls, ComCtrls;

type
  TAboutForm = class(TForm)
    Image1: TImage;
    Image2: TImage;
    SpTBXButton1: TSpTBXButton;
    Actions: TActionList;
    CloseForm: TAction;
    RichEdit1: TRichEdit;
    Label1: TLabel;
    procedure CloseFormExecute(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

procedure TAboutForm.CloseFormExecute(Sender: TObject);
begin
  Close;
end;

end.
