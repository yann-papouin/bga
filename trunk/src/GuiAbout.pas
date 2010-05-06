(* ***** BEGIN LICENSE BLOCK *****
 * Version: GNU GPL 2.0
 *
 * The contents of this file are subject to the
 * GNU General Public License Version 2.0; you may not use this file except
 * in compliance with the License. You may obtain a copy of the License at
 * http://www.gnu.org/licenses/gpl.html
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is GuiAbout (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

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
    InfoVersion: TLabel;
    procedure CloseFormExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.dfm}

uses
  SvnInfo;

procedure TAboutForm.CloseFormExecute(Sender: TObject);
begin
  Close;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  InfoVersion.Caption := Format('Revision #%d, %s', [SVN_REVISION, SVN_NOW]);
end;

end.
