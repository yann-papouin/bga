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
 * The Original Code is GuiFSView (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiFSView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GuiFormCommon, ActnList, SpTBXControls, StdCtrls, SpTBXEditors, SpTBXItem, VirtualTrees,
  ExtCtrls, TB2Item, TB2Dock, TB2Toolbar, FSLib;

type


  TFSViewForm = class(TFormCommon)
    Actions: TActionList;
    Panel2: TPanel;
    SpTBXButton2: TSpTBXButton;
    Settings: TAction;
    FilesystemChoice: TSpTBXComboBox;
    SpTBXLabel2: TSpTBXLabel;
    TopDock: TSpTBXDock;
    tbMenuBar: TSpTBXToolbar;
    mFile: TSpTBXSubmenuItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    mEdit: TSpTBXSubmenuItem;
    SpTBXItem23: TSpTBXItem;
    Add: TAction;
    Import: TAction;
    Update: TAction;
    SpTBXItem1: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    procedure SettingsExecute(Sender: TObject);
    procedure AddExecute(Sender: TObject);
    procedure ImportExecute(Sender: TObject);
    procedure UpdateExecute(Sender: TObject);
    procedure FilesystemChoiceChange(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FSViewForm: TFSViewForm;

implementation

{$R *.dfm}

uses
  GuiFSSettings, Resources,  IOUtils, Types;


procedure TFSViewForm.ImportExecute(Sender: TObject);
begin
//
end;

procedure TFSViewForm.UpdateExecute(Sender: TObject);
begin
//
end;


procedure TFSViewForm.FilesystemChoiceChange(Sender: TObject);
begin
  inherited;
  Settings.Enabled := FileExists(FilesystemChoice.Text);
end;


procedure TFSViewForm.AddExecute(Sender: TObject);
begin
  FSSettingsForm.OpenMode := omAdd;
  if FSSettingsForm.ShowModal = mrOk then
  begin

  end;
end;

procedure TFSViewForm.SettingsExecute(Sender: TObject);
begin
  FSSettingsForm.OpenMode := omEdit;
  if FSSettingsForm.ShowModal = mrOk then
  begin

  end;
end;



end.
