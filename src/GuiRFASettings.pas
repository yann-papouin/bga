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
  * The Original Code is GuiRFASettings (http://code.google.com/p/bga)
  *
  * The Initial Developer of the Original Code is
  * Yann Papouin <yann.papouin at @ gmail.com>
  *
  * ***** END LICENSE BLOCK ***** *)

unit GuiRFASettings;

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
  Grids,
  ValEdit,
  SpTBXItem,
  SpTBXControls,
  JvComponentBase,
  JvFormPlacement,
  VirtualTrees,
  ActnList,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  ExtCtrls;

type
  TRFASettingsForm = class(TForm)
    SpTBXGroupBox1: TSpTBXGroupBox;
    FormStorage: TJvFormStorage;
    ExtList: TVirtualStringTree;
    Actions: TActionList;
    Add: TAction;
    Remove: TAction;
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    ChoosePath: TAction;
    OpenDialog: TOpenDialog;
    Background: TSpTBXPanel;
    DoubleClickOption: TSpTBXRadioGroup;
    Ok: TAction;
    Cancel: TAction;
    Browse: TAction;
    Edit: TAction;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    GeneralOptions: TSpTBXGroupBox;
    OpenLast: TSpTBXCheckBox;
    Footer: TSpTBXPanel;
    ButtonOk: TSpTBXButton;
    ButtonCancel: TSpTBXButton;
    procedure ExtListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure ExtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure ExtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure AddExecute(Sender: TObject);
    procedure RemoveExecute(Sender: TObject);
    procedure ExtListEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
    procedure ExtListEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure ExtListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure OkExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure FormStorageBeforeSavePlacement(Sender: TObject);
    procedure FormStorageAfterRestorePlacement(Sender: TObject);
    procedure ExtListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure ExtListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EditExecute(Sender: TObject);
    procedure BrowseExecute(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function GetProgramByExt(Ext: string): string;
  end;

  pExtData = ^rExtData;

  rExtData = record
    Extension: string;
    Path: string;
  end;

var
  RFASettingsForm: TRFASettingsForm;

const
  EXT_SEPARATOR = '|';

implementation

{$R *.dfm}

uses
  GuiRFAView,
  StringFunction,
  CommonLib,
  Resources;

{ TRFASettingsForm }

const
  ExtensionCharSet = ['a' .. 'z', 'A' .. 'Z', '0' .. '9'];


procedure TRFASettingsForm.FormCreate(Sender: TObject);
begin
  FormStorage.RestoreFormPlacement;
end;

procedure TRFASettingsForm.OkExecute(Sender: TObject);
begin
  FormStorage.SaveFormPlacement;
  ModalResult := mrOk;
end;

procedure TRFASettingsForm.BrowseExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pExtData;
begin
  Node := ExtList.GetFirstSelected;
  if Node <> nil then
  begin
    Data := ExtList.GetNodeData(Node);
    OpenDialog.InitialDir := ExtractFilePath(Data.Path);
    OpenDialog.FileName := ExtractFileName(Data.Path);

    if OpenDialog.Execute then
    begin
      Data.Path := OpenDialog.FileName;
    end;
  end;
end;

procedure TRFASettingsForm.EditExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := ExtList.GetFirstSelected;
  if Node <> nil then
  begin
    ExtList.EditNode(Node,0);
  end;
end;

procedure TRFASettingsForm.CancelExecute(Sender: TObject);
begin
  FormStorage.RestoreFormPlacement;
  ModalResult := mrCancel;
end;


procedure TRFASettingsForm.AddExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pExtData;
begin
  Node := ExtList.AddChild(nil);
  Data := ExtList.GetNodeData(Node);
  Data.Extension := '';
  Data.Path := GetProgramFiles;
  ExtList.EditNode(Node, 0);
end;

procedure TRFASettingsForm.RemoveExecute(Sender: TObject);
var
  Node, NextNode: PVirtualNode;
begin
  Node := ExtList.GetFirstSelected;
  while Node <> nil do
  begin
    NextNode := ExtList.GetNextSelected(Node);
    ExtList.DeleteNode(Node);
    Node := NextNode;
  end;
end;

procedure TRFASettingsForm.FormStorageAfterRestorePlacement(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pExtData;
  Count, i: Integer;
  Txt: string;
begin
  ExtList.Clear;
  Count := FormStorage.ReadInteger('ExtensionCount', 0);
  for i := 0 to Count - 1 do
  begin
    Node := ExtList.AddChild(nil);
    Data := ExtList.GetNodeData(Node);
    Txt := FormStorage.ReadString(IntToStr(i), EXT_SEPARATOR);
    Data.Extension := SFLeft(EXT_SEPARATOR, Txt);
    Data.Path := SFRight(EXT_SEPARATOR, Txt);
  end;
end;

procedure TRFASettingsForm.FormStorageBeforeSavePlacement(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pExtData;
begin
  Node := ExtList.GetFirst;
  FormStorage.WriteInteger('ExtensionCount', ExtList.RootNodeCount);
  while Node <> nil do
  begin
    Data := ExtList.GetNodeData(Node);
    FormStorage.WriteString(IntToStr(Node.Index), Data.Extension + EXT_SEPARATOR + Data.Path);
    Node := ExtList.GetNext(Node);
  end;
end;

function TRFASettingsForm.GetProgramByExt(Ext: string): string;
var
  Node: PVirtualNode;
  Data: pExtData;
begin
  Ext := StripNonConforming(Ext, ExtensionCharSet);
  Node := ExtList.GetFirst;
  while Node <> nil do
  begin
    Data := ExtList.GetNodeData(Node);
    if AnsiSameText(Data.Extension, Ext) then
    begin
      Result := Data.Path;
      Exit;
    end;
    Node := ExtList.GetNext(Node);
  end;
  Result := EmptyStr;
end;

procedure TRFASettingsForm.ExtListEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var Allowed: Boolean);
begin
  //
end;

procedure TRFASettingsForm.ExtListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Data: pExtData;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0:
      begin
        NewText := StripNonConforming(NewText, ExtensionCharSet);
        Data.Extension := NewText;
      end;
    1:
      begin
        Data.Path := NewText;
      end;
  end;
end;



procedure TRFASettingsForm.ExtListDblClick(Sender: TObject);
begin
  Browse.Execute;
end;

procedure TRFASettingsForm.ExtListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: pExtData;
begin
  Data := Sender.GetNodeData(Node);
  if Column = 1 then
  begin
    if DirectoryExists(Data.Path) then
    begin
      TargetCanvas.Font.Color := clWebOrange;
    end
    else
    if not FileExists(Data.Path) then
    begin
      TargetCanvas.Font.Color := clRed;
    end;
  end;
end;

procedure TRFASettingsForm.ExtListEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  //
end;

procedure TRFASettingsForm.ExtListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: pExtData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TRFASettingsForm.ExtListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rExtData);
end;

procedure TRFASettingsForm.ExtListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: pExtData;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0:
      begin
        CellText := Data.Extension;
      end;
    1:
      begin
        CellText := Data.Path;
      end;
  end;
end;

end.
