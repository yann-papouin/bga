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
  ExtCtrls, TB2Item, TB2Dock, TB2Toolbar, FSLib, JvComponentBase, JvFormPlacement, DB, RFALib,
  SqlitePassDbo, JvAppStorage, JvAppRegistryStorage, Grids, DBGrids, JvExDBGrids, JvDBGrid;

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
    SubDataset: TSqlitePassDataset;
    Sync: TTimer;
    Init: TAction;
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
    procedure SyncTimer(Sender: TObject);
    procedure InitExecute(Sender: TObject);
  private
    FArchiveID : integer;
    procedure ApplySettingsData(Data: pFilesystemData);
    procedure SyncReadEntry(Sender: TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed: boolean; cSize: integer);

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
  DbugIntf, AppLib, GuiFSSettings, Resources, IOUtils, Types, StringFunction, TypInfo, MD5Api;


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
  'CREATE TABLE [SampleTable]
  ( [AutoIncField] AUTOINC, [BinIntField] BIGINT,' +
  '[BinaryField] BINARY, [BlobField] BLOB, [BooleanField] BOOLEAN, [CharField] CHAR,' +
  '[ClobField] CLOB, [CurrencyField] CURRENCY, [DateField] DATE, [DateTextField] DATE,' +
  '[DateTimeField] DATETIME, [DecField] DEC, [DecimalField] DECIMAL, [DoubleField] DOUBLE,' +
  '[DoublePrecisionField] DOUBLE PRECISION, [FloatField] FLOAT, [GaphicField] GRAPHIC,'+
  '[GuidField] GUID, [IntField] INT, [Int64Field] INT64);');
  *)

procedure TFSViewForm.InitExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  // Reset components to empty vars
  Database.Connected := false;
  Database.Database := EmptyStr;
  Current_battlefield_path := EmptyStr;

  if Database.SQLiteLibrary = EmptyStr then
    Database.SQLiteLibrary := GetAppDirectory + 'sqlite3.dll';

  // Find selected file system
  Node := FilesystemList.GetFirstSelected;
  if Node <> nil then
  begin
    Data := FilesystemList.GetNodeData(Node);
    Current_battlefield_path := IncludeTrailingBackslash(Data.Path);
    Database.Database := Current_battlefield_path + Data.Name;
  end;
end;


procedure TFSViewForm.UpdateExecute(Sender: TObject);
var
  i :Integer;
  SQL : string;
  ModEntry : TBattlefieldModEntry;
  ModPath : string;
  ModID : integer;
  Options : TLocateOptions;


  procedure InsertAchivesFromPath(Path : string);
  var
    k : integer;
    ArchiveName : string;
    ArchivePath : string;

    Content : TStringDynArray;
  begin
    Content := TDirectory.GetFileSystemEntries(IncludeTrailingBackslash(Path));
    for k := 0 to Length(Content) - 1 do
    begin
      SendDebugFmt('%d:%s',[k, Content[k]]);

      if TDirectory.Exists(Content[k]) then
      begin
        InsertAchivesFromPath(Content[k]);
      end
        else
      if TFile.Exists(Content[k]) and (UpperCase(ExtractFileExt(Content[k])) = RFA_EXTENSION)  then
      begin
        // Check if is a valid RFA
        ArchiveName := ExtractFileName(Content[k]);
        ArchivePath := FsAbsToRel(IncludeTrailingBackslash(ExtractFilePath(Content[k])));

        if not SubDataset.Locate('path; name', VarArrayOf([ArchivePath, ArchiveName]), Options) then
        begin
          SQL := Format('INSERT INTO "ARCHIVE" VALUES(NULL, "%s", "%s", NULL, NULL, "%d");', [ArchiveName, ArchivePath, ModID]);
          SendDebug(SQL);
          Database.Engine.ExecSQL(SQL);
        end;
      end

    end;
  end;

begin
  Init.Execute;
  Options := [loCaseInsensitive, loPartialKey];

  if FileExists(Database.Database) then
    DeleteFile(Database.Database);

  if not FileExists(Database.Database) then
  begin
    Database.CreateDatabase(Database.Database, dbtSqlitePass);
    Database.Open;

    Database.TableDefs.CreateTable
    (
      'CREATE TABLE "MOD"' +
      '(' +
      '"id" INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '"name" STRING,' +
      '"path" STRING' +
      ');'
    );

    Database.TableDefs.CreateTable
    (
      'CREATE TABLE "DEPENDENCY"' +
      '(' +
      '"mod" INTEGER,' +
      '"depends" INTEGER,' +
      '"order" INTEGER' +
      ');'
    );

    Database.TableDefs.CreateTable
    (
      'CREATE TABLE "ARCHIVE"' +
      '(' +
      '"id" INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '"name" STRING,' +
      '"path" STRING,' +
      '"age" DATETIME,' +
      '"md5" STRING,' +
      '"mod" INTEGER' +
      ');'
    );

    Database.TableDefs.CreateTable
    (
      'CREATE TABLE "FILE"' +
      '(' +
      '"id" INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '"filename" STRING,' +
      '"path" STRING' +
      '"offset" INTEGER' +
      '"size" INTEGER' +
      '"compressed" BOOLEAN' +
      '"csize" INTEGER' +
      '"archive" INTEGER' +
      ');'
    );

    Database.Close;
  end;

  // Open database if settings are correct
  if FileExists(Database.Database) then
  begin

    Database.Connected := true;
    if Database.Connected then
    begin
      Dataset.Close;
      Dataset.SQL.Text := 'SELECT * FROM "MOD";';
      Dataset.Open;

      // Find selected file system
      if Database.Database <> EmptyStr then
      begin
        FSSettingsForm.ReadModsInfos(Current_battlefield_path + MOD_DIRECTORY_NAME);
      end;

      for i:= 0 to FSSettingsForm.Modentries.Count - 1 do
      begin
        ModEntry := FSSettingsForm.Modentries[i];

        if not Dataset.Locate('name', ModEntry.GameName, Options) then
        begin
          SQL := Format('INSERT INTO "MOD" VALUES(NULL, "%s", "%s");', [ModEntry.GameName, FsAbsToRel(ModEntry.AbsolutePath)]);
          Database.Engine.ExecSQL(SQL);
        end;
      end;

      Dataset.Refresh;
      while Dataset.RecNo < Dataset.RecordCount - 1 do
      begin
        SendDebugFmt('%d:%s',[Dataset.RecNo, Dataset.FieldByName('Name').AsString]);

        ModID := Dataset.FieldByName('id').AsInteger;
        ModPath := FsRelToAbs(Dataset.FieldByName('Path').AsString) + IncludeTrailingBackslash(ARCHIVE_DIRECTORY_NAME);

        if DirectoryExists(ModPath) then
        begin
          SubDataset.Close;
          SubDataset.ParamCheck := true;
          SubDataset.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE mod=:IdAsInteger;';
          SubDataset.Params.ParamByName('IdAsInteger').Value := IntToStr(ModID);
          SubDataset.Open;

          InsertAchivesFromPath(ModPath);
        end;

        Dataset.Next;
      end;

      SyncTimer(self);

      /// RebuildModDependency
      /// RebuildArchiveList with md5 hash
      ///


    end;
  end;
end;



procedure TFSViewForm.SyncReadEntry(Sender: TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed: boolean; cSize: integer);
var
  Filename : string;
  Path : string;
  SQL : string;
begin
  Filename := ExtractUnixFileName(Name);
  Path := ARCHIVE_PATH + ExtractUnixFilePath(Name);

  SQL := Format('INSERT INTO "FILE" VALUES(NULL, "%s", "%s", "%d", "%d", "%d", "%d", "%d");', [Filename, Path, Offset, ucSize, Integer(Compressed), cSize, FArchiveID]);
  Database.Engine.ExecSQL(SQL);
end;

procedure TFSViewForm.SyncTimer(Sender: TObject);
var
  Archive : TRFAFile;
  ArchiveID : integer;
  ArchiveFilename : string;
  ArchiveDateTime: TDateTime;
  ArchiveMD5 : string;
  FileDateAndTime : TDateTime;
  SQL : string;


begin
  //Init.Execute;

  if Database.Connected then
  begin
    Dataset.Close;
    Dataset.SQL.Text := 'SELECT * FROM "ARCHIVE";';
    Dataset.Open;

    while Dataset.RecNo < Dataset.RecordCount - 1 do
    begin
      ArchiveID := Dataset.FieldByName('id').AsInteger;
      ArchiveFilename := FsRelToAbs(Dataset.FieldByName('path').AsString + Dataset.FieldByName('name').AsString);
      ArchiveDateTime := Dataset.FieldByName('age').AsDateTime;
      ArchiveMD5 :=  Dataset.FieldByName('md5').AsString;

      //SendDebug();

      if FileAge(ArchiveFilename, FileDateAndTime) then
      begin
        if (FileDateAndTime <> ArchiveDateTime) then
        begin
          if MD5FromFile(ArchiveFilename) <> ArchiveMD5 then
          begin
            // Updates archives
            SQL := Format('DELETE * FROM "FILES" WHERE archive=%d',[ArchiveID]);
            Database.Engine.ExecSQL(SQL);


            FArchiveID := ArchiveID;
            Archive := TRFAFile.Create;
            Archive.OnReadEntry := SyncReadEntry;
            if Archive.Open(ArchiveFilename) >=0 then
            begin
            // Update DB data like MD5 & filedate

            /// FileAge(Content[k], DateAndTime);
            /// ArchiveAge := FormatDateTime('YYYY-MM-DD hh:mm:ss.zzz', DateAndTime);
            /// FileMD5 := MD5FromFile(Content[k]);
            end;
            Archive.Free;


          end
          else

        end;
      end
        else
      begin
        // File doesn't exists anymore, remove it
        //Database.Engine.ExecSQL('DELETE')
      end;


      Dataset.Next;
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
