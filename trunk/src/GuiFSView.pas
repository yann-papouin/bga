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
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GuiRFACommon,
  ActnList,
  SpTBXControls,
  StdCtrls,
  SpTBXEditors,
  SpTBXItem,
  VirtualTrees,
  ExtCtrls,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  FSLib;

type

  TFSViewForm = class(TRFACommonForm)
    Panel2: TPanel;
    SpTBXButton2: TSpTBXButton;
    Load: TAction;
    ModList: TSpTBXComboBox;
    SpTBXLabel2: TSpTBXLabel;
    TopDock: TSpTBXDock;
    Footer: TSpTBXPanel;
    ButtonOk: TSpTBXButton;
    ButtonCancel: TSpTBXButton;
    Ok: TAction;
    Cancel: TAction;
    SpTBXButton3: TSpTBXButton;
    Settings: TAction;
    procedure OkExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SettingsExecute(Sender: TObject);
  private
    { Déclarations privées }
    procedure FileSystemChange(Sender: TObject);
    procedure ListMods(Sender: TObject; Name, Path: string; ID: integer);
    procedure ListArchives(Sender: TObject; Name, Path: string; ID: integer);
    procedure ListFiles(Sender: TObject; Name, Path: string; ID: integer);
  public
    { Déclarations publiques }
  end;

var
  FSViewForm: TFSViewForm;

implementation

{$R *.dfm}

uses
  GuiFSSettings,
  Resources,
  IOUtils,
  Types;



procedure TFSViewForm.OkExecute(Sender: TObject);
begin
  // FormStorage.SaveFormPlacement;
  ModalResult := mrOk;
  Close;
end;


procedure TFSViewForm.CancelExecute(Sender: TObject);
begin
  // FormStorage.RestoreFormPlacement;
  ModalResult := mrCancel;
  Close;
end;

procedure TFSViewForm.SettingsExecute(Sender: TObject);
begin
  FSSettingsForm.ShowModal;
end;

procedure TFSViewForm.FileSystemChange(Sender: TObject);
begin
  ModList.Clear;
  FSSettingsForm.ListMods;
end;

procedure TFSViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FSSettingsForm.OnChange := FileSystemChange;
  FSSettingsForm.OnListMods := ListMods;
  FSSettingsForm.OnListArchives := ListArchives;
  FSSettingsForm.OnListFiles := ListFiles;
end;

procedure TFSViewForm.ListMods(Sender: TObject; Name, Path: string; ID: integer);
begin
  ModList.Items.Add(Name);
end;


(*
procedure TFSViewForm.ModsClick(Sender: TObject);
begin
  Files.Clear;
  FSSettingsForm.ListFiles(1);
  Files.Lines.Add('Ok');
end;
*)

procedure TFSViewForm.ListArchives(Sender: TObject; Name, Path: string; ID: integer);
begin

end;

procedure TFSViewForm.ListFiles(Sender: TObject; Name, Path: string; ID: integer);
begin
  //Files.Lines.Add(Name);
end;

end.
