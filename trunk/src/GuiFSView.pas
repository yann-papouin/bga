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
    Panel2: TSpTBXPanel;
    SpTBXButton2: TSpTBXButton;
    Load: TAction;
    ModList: TSpTBXComboBox;
    SpTBXLabel2: TSpTBXLabel;
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
    procedure LoadExecute(Sender: TObject);
    procedure ModListChange(Sender: TObject);
    procedure RFAListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: string);
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
  DbugIntf,
  GuiFSSettings,
  Resources,
  IOUtils,
  JclFileUtils,
  JclStrings,
  StringFunction,
  Types;


procedure TFSViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FSSettingsForm.OnChange := FileSystemChange;
  FSSettingsForm.OnListMods := ListMods;
  FSSettingsForm.OnListArchives := ListArchives;
  FSSettingsForm.OnListFiles := ListFiles;

  FSSettingsForm.ApplicationRun.Execute;
end;


procedure TFSViewForm.OkExecute(Sender: TObject);
begin
  // FormStorage.SaveFormPlacement;
  ModalResult := mrOk;
  Close;
end;

procedure TFSViewForm.RFAListGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  Data : pFse;
  FloatValue : extended;
begin
  Data := Sender.GetNodeData(Node);
  HintText := Data.W32Path;
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
  ModList.Text := EmptyStr;
  FSSettingsForm.ListMods;
end;

procedure TFSViewForm.LoadExecute(Sender: TObject);
begin
  inherited;
  RFAList.Clear;
  RFAList.BeginUpdate;
  FSSettingsForm.ListFiles(ModList.Tag);
  Sort;
  RFAList.EndUpdate;
end;

procedure TFSViewForm.ModListChange(Sender: TObject);
begin
  inherited;
  ModList.Tag := ModList.Items.IndexOf(ModList.Text);
  Load.Enabled := ModList.Tag > 0;
end;


procedure TFSViewForm.ListMods(Sender: TObject; Name, Path: string; ID: integer);
begin
  ModList.Items.Add(Name);
end;

procedure TFSViewForm.ListArchives(Sender: TObject; Name, Path: string; ID: integer);
begin

end;


procedure TFSViewForm.ListFiles(Sender: TObject; Name, Path: string; ID: integer);
var
  Node: PVirtualNode;
  Data : pFse;
  W32Path : AnsiString;
begin
  Path := StringReplace(Path, ARCHIVE_PATH, '/', [rfReplaceAll]);
  W32Path := StringReplace(Path,'/','\',[rfReplaceAll]) + Name;

  Node := GetBuildPath(W32Path);

  Node := RFAList.AddChild(Node);
  Data := RFAList.GetNodeData(Node);
  Data.RFAFileHandle := nil;
  Data.RFAFileName := '';

  Data.EntryName := Name;
  Data.Offset := 0;
  Data.Size := 0;
  Data.Compressed := false;
  Data.CompSize := 0;

  Data.W32Path := W32Path;
  Data.W32Name := ExtractFileName(W32Path);
  Data.W32Ext := ExtractFileExt(LowerCase(W32Path));
  Data.FileType := ExtensionToType(Data.W32Ext);
  Data.ExternalFilePath := EmptyStr;
end;

end.
