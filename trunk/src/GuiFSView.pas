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
  Generics.Collections,
  GuiRFACommon,

  GuiFSSettings,
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

  TModItem = class
  private
    FName: string;
    FPath: string;
    FID: integer;
    procedure SetID(const Value: integer);
    procedure SetName(const Value: string);
    procedure SetPath(const Value: string);
  protected
  public
    property ID: integer read FID write SetID;
    property Path: string read FPath write SetPath;
    property Name: string read FName write SetName;
  end;

  TModList = TObjectList<TModItem>;

  TFSViewForm = class(TRFACommonForm)
    Panel2: TSpTBXPanel;
    SpTBXButton2: TSpTBXButton;
    Load: TAction;
    Mods: TSpTBXComboBox;
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
    procedure ModsChange(Sender: TObject);
    procedure RFAListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
    procedure FormDestroy(Sender: TObject);
    procedure RFAListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  private
    { Déclarations privées }
    FModList : TModList;
    FCurrent : TModItem;

    procedure FileSystemChange(Sender: TObject);
    procedure ListMods(Sender: TObject; FSData: TFSModData);
    procedure ListArchives(Sender: TObject; FSData: TFSArchiveData);
    procedure ListFiles(Sender: TObject; FSData: TFSFileData);

  public
    { Déclarations publiques }
  end;

var
  FSViewForm: TFSViewForm;

implementation

{$R *.dfm}

uses
  DbugIntf,
  Resources,
  IOUtils,
  JclFileUtils,
  JclStrings,
  StringFunction,
  Types;

procedure TFSViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FModList := TModList.Create;

  FSSettingsForm.OnChange := FileSystemChange;
  FSSettingsForm.OnListMods := ListMods;
  FSSettingsForm.OnListArchives := ListArchives;
  FSSettingsForm.OnListFiles := ListFiles;

  FSSettingsForm.ApplicationRun.Execute;
end;

procedure TFSViewForm.FormDestroy(Sender: TObject);
begin
  FModList.Free;
  inherited;
end;

procedure TFSViewForm.OkExecute(Sender: TObject);
begin
  // FormStorage.SaveFormPlacement;
  ModalResult := mrOk;
  Close;
end;

procedure TFSViewForm.RFAListGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: string);
var
  Data: pFse;
  FloatValue: extended;
begin
  Data := Sender.GetNodeData(Node);
  HintText := Data.W32Path;
end;

procedure TFSViewForm.RFAListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data: pFse;
begin
  inherited;
  Data := Sender.GetNodeData(Node);

  if Column = 5 then
  begin
    if IsFile(Data.FileType) then
    begin
      CellText := Data.RFAFileName;
    end
      else
    begin
      CellText := EmptyStr;
    end;
  end;

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
  Mods.Clear;
  FModList.Clear;

  Mods.Text := EmptyStr;
  FSSettingsForm.ListMods;
end;

procedure TFSViewForm.LoadExecute(Sender: TObject);
begin
  inherited;
  RFAList.Clear;
  RFAList.BeginUpdate;
  FSSettingsForm.ListFiles(FCurrent.ID);
  Sort;
  RFAList.EndUpdate;
end;

procedure TFSViewForm.ModsChange(Sender: TObject);
var
  Index : integer;
begin
  inherited;
  Index := Mods.Items.IndexOf(Mods.Text);

  if Index >= 0 then
    FCurrent := Mods.Items.Objects[Index] as TModItem
  else
    FCurrent := nil;

  Load.Enabled := Assigned(FCurrent);
end;

procedure TFSViewForm.ListMods(Sender: TObject; FSData: TFSModData);
var
  Item : TModItem;
begin
  Item := TModItem.Create;
  Item.Name := FSData.Name;
  Item.Path := FSData.Path;
  Item.ID := FSData.ID;

  FModList.Add(Item);
  Mods.Items.AddObject(FSData.Name, Item);
end;

procedure TFSViewForm.ListArchives(Sender: TObject; FSData: TFSArchiveData);
begin

end;

procedure TFSViewForm.ListFiles(Sender: TObject; FSData: TFSFileData);
var
  Node: PVirtualNode;
  Data: pFse;
  W32Path: AnsiString;
begin
  FSData.Path := StringReplace(FSData.Path, ARCHIVE_PATH, '/', [rfReplaceAll]);
  W32Path := StringReplace(FSData.Path, '/', '\', [rfReplaceAll]) + FSData.Filename;

  Node := GetBuildPath(W32Path);

  Node := RFAList.AddChild(Node);
  Data := RFAList.GetNodeData(Node);
  Data.RFAFileHandle := nil;
  Data.RFAFileName := FSData.Archive.Path + FSData.Archive.Name;

  Data.EntryName := FSData.Filename;
  Data.Offset := FSData.Offset;
  Data.Size := FSData.Size;
  Data.Compressed := FSData.Compressed;
  Data.CompSize := FSData.CompSize;

  Data.W32Path := W32Path;
  Data.W32Name := ExtractFileName(W32Path);
  Data.W32Ext := ExtractFileExt(LowerCase(W32Path));
  Data.FileType := ExtensionToType(Data.W32Ext);
  Data.ExternalFilePath := EmptyStr;
end;

{ TModItem }

procedure TModItem.SetID(const Value: integer);
begin
  FID := Value;
end;

procedure TModItem.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TModItem.SetPath(const Value: string);
begin
  FPath := Value;
end;

end.
