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
  SyncObjs,
  Dialogs,
  GuiFormCommon,
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
  FSLib,
  JvComponentBase,
  JvFormPlacement,
  RFALib,
  JvAppStorage,
  JvAppRegistryStorage,
  Grids,
  DBGrids,
  JvExDBGrids,
  JvDBGrid,
  JvExControls,
  JvAnimatedImage,
  JvGIFCtrl;

type

  TFSViewForm = class(TFormCommon)
    Actions: TActionList;
    Cancel: TAction;
    Ok: TAction;
    Background: TSpTBXPanel;
    Footer: TSpTBXPanel;
    ButtonOk: TSpTBXButton;
    ButtonCancel: TSpTBXButton;
    procedure CancelExecute(Sender: TObject);
    procedure OkExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
  DbugIntf,
  GuiWait,
  AppLib,
  CommonLib,
  GuiSkinDialog,
  Resources,
  IOUtils,
  Types,
  StringFunction,
  TypInfo,
  MD5Api;

procedure TFSViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  //
end;

procedure TFSViewForm.FormDestroy(Sender: TObject);
begin
  //
  inherited;
end;

procedure TFSViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  //
end;

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

end.
