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
 * The Original Code is GuiBrowsePack (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiBrowsePack;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GuiBrowse, ActnList, JvComponentBase, JvFormPlacement, JvBaseDlg,
  JvBrowseFolder, SpTBXControls, SpTBXItem, ExtCtrls, StdCtrls, SpTBXEditors;

type
  TBrowsePackForm = class(TBrowseForm)
    Base: TSpTBXEdit;
    UseBasePath: TSpTBXCheckBox;
    procedure UseBasePathClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  BrowsePackForm: TBrowsePackForm;

implementation

{$R *.dfm}

procedure TBrowsePackForm.UseBasePathClick(Sender: TObject);
begin
  inherited;
  Base.Enabled := UseBasePath.Checked;
end;

end.
