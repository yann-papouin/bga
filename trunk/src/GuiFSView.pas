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
  ExtCtrls, TB2Item, TB2Dock, TB2Toolbar, FSLib, JvComponentBase, JvFormPlacement, DB, SqlitePassDbo, JvAppStorage, JvAppRegistryStorage, Grids, DBGrids, JvExDBGrids, JvDBGrid;

type

  pFilesystemData = ^rFilesystemData;

  rFilesystemData = record
    Name: string;
    Path: string;
  end;


  TFSViewForm = class(TFormCommon)
    Actions: TActionList;
    Add: TAction;
    Import: TAction;
    Update: TAction;
    Remove: TAction;
    Edit: TAction;
    SpTBXGroupBox1: TSpTBXGroupBox;
    FilesystemList: TVirtualStringTree;
    SpTBXDock1: TSpTBXDock;
    SpTBXToolbar1: TSpTBXToolbar;
    SpTBXRightAlignSpacerItem1: TSpTBXRightAlignSpacerItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem5: TSpTBXItem;
    Background: TSpTBXPanel;
    Cancel: TAction;
    FormStorage: TJvFormStorage;
    Footer: TSpTBXPanel;
    ButtonOk: TSpTBXButton;
    ButtonCancel: TSpTBXButton;
    Ok: TAction;
    SpTBXItem1: TSpTBXItem;
    TemporaryAppStorage: TJvAppRegistryStorage;
    DataSource: TDataSource;
    DBGrid1: TDBGrid;
    Database: TSqlitePassDatabase;
    Dataset: TSqlitePassDataset;
    procedure AddExecute(Sender: TObject);
    procedure ImportExecute(Sender: TObject);
    procedure UpdateExecute(Sender: TObject);
    procedure FilesystemChoiceChange(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure FormStorageBeforeSavePlacement(Sender: TObject);
    procedure FormStorageAfterRestorePlacement(Sender: TObject);
    procedure FilesystemListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure FilesystemListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FilesystemListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure FilesystemListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure OkExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RemoveExecute(Sender: TObject);
    procedure EditExecute(Sender: TObject);
    procedure FilesystemListDblClick(Sender: TObject);
  private
    procedure ApplySettingsData(Data: pFilesystemData);
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;


var
  FSViewForm: TFSViewForm;

const
  FS_SEPARATOR = '|';

implementation

{$R *.dfm}

uses
  GuiFSSettings, Resources,  IOUtils, Types, StringFunction, TypInfo ;


procedure TFSViewForm.FormCreate(Sender: TObject);
begin
  FormStorage.RestoreFormPlacement;
end;


procedure TFSViewForm.OkExecute(Sender: TObject);
begin
  FormStorage.SaveFormPlacement;
  ModalResult := mrOk;
  Close;
end;

procedure TFSViewForm.CancelExecute(Sender: TObject);
begin
  FormStorage.RestoreFormPlacement;
  ModalResult := mrCancel;
  Close;
end;


procedure TFSViewForm.RemoveExecute(Sender: TObject);
var
  Node, NextNode: PVirtualNode;
begin
  Node := FilesystemList.GetFirstSelected;
  while Node <> nil do
  begin
    NextNode := FilesystemList.GetNextSelected(Node);
    FilesystemList.DeleteNode(Node);
    Node := NextNode;
  end;
end;

(*
  procedure TForm1.BtCreateDatabaseClick(Sender: TObject);
  begin
  if Not FileExists('NewDb.db3')
     then SqlitePassDatabase1.CreateDatabase('NewDb.db3');

  SqlitePassDatabase1.Close;
  SqlitePassDatabase1.Database := 'NewDb.db3';
  SqlitePassDatabase1.Open;
  SqlitePassDatabase1.TableDefs.CreateTable(
  'CREATE TABLE [SampleTable] ( [AutoIncField] AUTOINC, [BinIntField] BIGINT,' +
  '[BinaryField] BINARY, [BlobField] BLOB, [BooleanField] BOOLEAN, [CharField] CHAR,' +
  '[ClobField] CLOB, [CurrencyField] CURRENCY, [DateField] DATE, [DateTextField] DATE,' +
  '[DateTimeField] DATETIME, [DecField] DEC, [DecimalField] DECIMAL, [DoubleField] DOUBLE,' +
  '[DoublePrecisionField] DOUBLE PRECISION, [FloatField] FLOAT, [GaphicField] GRAPHIC,'+
  '[GuidField] GUID, [IntField] INT, [Int64Field] INT64);');
  SqlitePassDataset1.DatasetName := 'SampleTable';
  SqlitePassDataset1.Open;
  end;
*)

procedure TFSViewForm.UpdateExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
  i :Integer;
begin
  // Reset components to empty vars
  //Dataset.Active := false;
  Database.Connected := false;
  Database.Database := EmptyStr;

  // Find selected file system
  Node := FilesystemList.GetFirstSelected;
  if Node <> nil then
  begin
    Data := FilesystemList.GetNodeData(Node);
    Database.Database := IncludeTrailingBackslash(Data.Path) + Data.Name;
    FSSettingsForm.ReadModsInfos(IncludeTrailingBackslash(Data.Path) +'Mods');
  end;

  // Open database if settings are correct
  if FileExists(Database.Database) then
  begin
    if Database.SQLiteLibrary = EmptyStr then
      Database.SQLiteLibrary := 'C:\Users\Yann\Documents\RAD Studio\Libraries\SQLitePass_0.55\sqlite3.dll';

    Database.Connected := true;
    if Database.Connected then
    begin
      //Database.Engine.ExecSQL('INSERT INTO "ARCHIVE" VALUES(NULL, "Name", "Path", "md5", NULL)');

      Dataset.Close;
      Dataset.Params.Clear;
      Dataset.ParamCheck := True;

      //Dataset.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE path=:PathAsWidestring;';
      //Dataset.Params.ParamByName('PathAsWidestring').Value := '1';

      Dataset.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE path=:PathAsString;';
      Dataset.Params.ParamByName('PathAsString').Value := ('path');

      //Dataset.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE id=:IdAsInteger;';
      //Dataset.Params.ParamByName('IdAsInteger').Value := ('1');

     (*
      for i := 0 to Dataset.Params.Count - 1 do
      begin
        ShowMessageFmt('Type returned : %s',[GetEnumName(TypeInfo(TFieldType), Ord(Dataset.Params.Items[i].DataType))]);
      end;
      *)


      Dataset.Open;


    end;
  end;
end;

procedure TFSViewForm.ImportExecute(Sender: TObject);
begin
//
end;

procedure TFSViewForm.AddExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  FSSettingsForm.OpenMode := omAdd;
  if FSSettingsForm.ShowModal = mrOk then
  begin
    Node := FilesystemList.AddChild(nil);
    Data := FilesystemList.GetNodeData(Node);
    ApplySettingsData(Data);
  end;
end;


procedure TFSViewForm.ApplySettingsData(Data: pFilesystemData);
begin
  Data.Name := FSSettingsForm.FilesystemName.Text;
  Data.Path := FSSettingsForm.BattlefieldDir.Text;
end;

procedure TFSViewForm.EditExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  Node := FilesystemList.GetFirstSelected;
  if Node <> nil then
  begin
    Data := FilesystemList.GetNodeData(Node);
    FSSettingsForm.FilesystemName.Text := Data.Name;
    FSSettingsForm.BattlefieldDir.Text := Data.Path;

    FSSettingsForm.OpenMode := omEdit;
    if FSSettingsForm.ShowModal = mrOk then
    begin
      ApplySettingsData(Data);
    end;
  end;
end;

procedure TFSViewForm.FilesystemChoiceChange(Sender: TObject);
begin
  inherited;
  //Settings.Enabled := FileExists(FilesystemChoice.Text);
end;

procedure TFSViewForm.FilesystemListDblClick(Sender: TObject);
begin
  Edit.Execute;
end;

procedure TFSViewForm.FilesystemListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data: pFilesystemData;
begin
  Data := Sender.GetNodeData(Node);
  if Column = 1 then
  begin
    if not DirectoryExists(Data.Path) then
    begin
      TargetCanvas.Font.Color := clRed;
    end;
  end;
end;

procedure TFSViewForm.FilesystemListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: pFilesystemData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TFSViewForm.FilesystemListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rFilesystemData);
end;

procedure TFSViewForm.FilesystemListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: pFilesystemData;
begin
  Data := Sender.GetNodeData(Node);
  case Column of
    0:
      begin
        CellText := Data.Name;
      end;
    1:
      begin
        CellText := Data.Path;
      end;
  end;
end;

procedure TFSViewForm.FormStorageBeforeSavePlacement(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  Node := FilesystemList.GetFirst;
  FormStorage.WriteInteger('FilesystemCount', FilesystemList.RootNodeCount);
  while Node <> nil do
  begin
    Data := FilesystemList.GetNodeData(Node);
    FormStorage.WriteString(IntToStr(Node.Index), Data.Name + FS_SEPARATOR + Data.Path);
    Node := FilesystemList.GetNext(Node);
  end;
end;


procedure TFSViewForm.FormStorageAfterRestorePlacement(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
  Count, i: Integer;
  Txt: string;
begin
  FilesystemList.Clear;
  Count := FormStorage.ReadInteger('FilesystemCount', 0);
  for i := 0 to Count - 1 do
  begin
    Node := FilesystemList.AddChild(nil);
    Data := FilesystemList.GetNodeData(Node);
    Txt := FormStorage.ReadString(IntToStr(i), FS_SEPARATOR);
    Data.Name := SFLeft(FS_SEPARATOR, Txt);
    Data.Path := SFRight(FS_SEPARATOR, Txt);
  end;
end;




end.
