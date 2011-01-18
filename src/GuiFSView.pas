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
    Database: TSqlitePassDatabase;
    Dataset: TSqlitePassDataset;
    SubDataset: TSqlitePassDataset;
    Sync: TTimer;
    Init: TAction;
    SyncDataset: TSqlitePassDataset;
    SpTBXItem2: TSpTBXItem;
    Active: TAction;
    SyncStatusGroup: TSpTBXGroupBox;
    SyncStatusAction: TSpTBXLabel;
    SyncProgressBar: TSpTBXProgressBar;
    SyncStatusArchiveName: TSpTBXLabel;
    Delete: TAction;
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
    procedure FormDestroy(Sender: TObject);
    procedure ActiveExecute(Sender: TObject);
    procedure FilesystemListStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure FilesystemListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
  private
    FActiveIndex : Integer;
    FArchiveID : integer;
    FSQL : TStringList;
    FSyncRunning : Boolean;
    FSyncMutex : boolean;
    FPassCount : integer;
    procedure ApplySettingsData(Data: pFilesystemData);
    procedure SyncReadEntry(Sender: TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed: boolean; cSize: integer);

    { Déclarations privées }
    procedure SyncStart;
    procedure SyncStop;
    procedure SetActiveIndex(const Value: Integer);
  public
    { Déclarations publiques }
    property ActiveIndex : Integer read FActiveIndex write SetActiveIndex;
  end;


var
  FSViewForm: TFSViewForm;

const
  FS_SEPARATOR = '|';
  SYNC_HARDTIME = 5;
  SYNC_EASYTIME = 400;

implementation

{$R *.dfm}

uses
  DbugIntf, GuiWait, AppLib, GuiFSSettings, Resources, IOUtils, Types, StringFunction, TypInfo, MD5Api;


procedure TFSViewForm.FormCreate(Sender: TObject);
begin
  FSQL := TStringList.Create;
  FormStorage.RestoreFormPlacement;
end;


procedure TFSViewForm.FormDestroy(Sender: TObject);
begin
  Ok.Enabled := false;
  Cancel.Enabled := false;
  SyncStop;

  Database.Close;
  FSQL.Free;
  inherited;
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
begin
  // Reset components to empty vars
  Database.Connected := false;
  Database.Database := EmptyStr;
  Current_battlefield_path := EmptyStr;

  if Database.SQLiteLibrary = EmptyStr then
    Database.SQLiteLibrary := GetAppDirectory + 'sqlite3.dll';
end;


procedure TFSViewForm.UpdateExecute(Sender: TObject);
var
  i,j :Integer;
  Order : integer;
  Query : string;
  ModEntry : TBattlefieldModEntry;
  ModPath : string;
  ModID : integer;
  DepID : Integer;
  Options : TLocateOptions;
  Node: PVirtualNode;
  Data: pFilesystemData;

  procedure PrepareInsertAchivesFromPath(Path : string);
  var
    k : integer;
    ArchiveName : string;
    ArchivePath : string;

    Content : TStringDynArray;
  begin
    Content := TDirectory.GetFileSystemEntries(IncludeTrailingBackslash(Path));
    for k := 0 to Length(Content) - 1 do
    begin
      //SendDebugFmt('%d:%s',[k, Content[k]]);

      if TDirectory.Exists(Content[k]) then
      begin
        PrepareInsertAchivesFromPath(Content[k]);
      end
        else
      if TFile.Exists(Content[k]) and (UpperCase(ExtractFileExt(Content[k])) = RFA_EXTENSION)  then
      begin
        // Check if is a valid RFA
        ArchiveName := ExtractFileName(Content[k]);
        ArchivePath := FsAbsToRel(IncludeTrailingBackslash(ExtractFilePath(Content[k])));

        if not SubDataset.Locate('path; name', VarArrayOf([ArchivePath, ArchiveName]), Options) then
        begin
          FSQL.Add(Format('INSERT INTO "ARCHIVE" VALUES(NULL, "%s", "%s", NULL, NULL, "%d");', [ArchiveName, ArchivePath, ModID]));
          //SendDebug(FSQL.Strings[FSQL.Count-1]);
        end;
      end

    end;
  end;

begin

  SyncStop;
  WaitForm.BeginWait;
  WaitForm.IncProgress('Init', 5);
  Init.Execute;

  // Find selected file system
  Node := FilesystemList.GetFirstSelected;
  if Node <> nil then
  begin
    Data := FilesystemList.GetNodeData(Node);
    Current_battlefield_path := IncludeTrailingBackslash(Data.Path);
    Database.Database := Current_battlefield_path + Data.Name;
  end;

  Options := [loCaseInsensitive, loPartialKey];
(*
  if FileExists(Database.Database) then
    DeleteFile(Database.Database);
*)
  if not FileExists(Database.Database) then
  begin
    Database.CreateDatabase(Database.Database, dbtSqlitePass);
    Database.Open;
    WaitForm.IncProgress('Creating tables');

    Database.TableDefs.CreateTable
    (
      'CREATE TABLE "MOD"' +
      '(' +
      '"id" INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '"name" BLOB,' +
      '"path" BLOB' +
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
      '"name" BLOB,' +
      '"path" BLOB,' +
      '"age" DATETIME,' +
      '"hash" BLOB,' +
      '"mod" INTEGER' +
      ');'
    );

    Database.TableDefs.CreateTable
    (
      'CREATE TABLE "FILE"' +
      '(' +
      '"id" INTEGER PRIMARY KEY AUTOINCREMENT,' +
      '"filename" BLOB,' +
      '"path" BLOB,' +
      '"offset" INTEGER,' +
      '"size" INTEGER,' +
      '"compressed" BOOLEAN,' +
      '"csize" INTEGER,' +
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
      WaitForm.IncProgress('Updating MODs');

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
          Query := Format('INSERT INTO "MOD" VALUES(NULL, "%s", "%s");', [ModEntry.GameName, FsAbsToRel(ModEntry.AbsolutePath)]);
          Database.Engine.ExecSQL(Query);
        end;
      end;

      for i:= 0 to FSSettingsForm.Modentries.Count - 1 do
      begin
        ModEntry := FSSettingsForm.Modentries[i];
        Dataset.Close;
        Dataset.SQL.Text := Format('SELECT * FROM "MOD" WHERE path ="%s";',[FsAbsToRel(ModEntry.AbsolutePath)]);
        Dataset.Open;

        if Dataset.Locate('name', ModEntry.GameName, Options) then
        begin
          Order := 0;
          ModID := Dataset.FieldByName('id').AsInteger;

          // Remove existing dependencies
          Query := Format('DELETE FROM "DEPENDENCY" WHERE mod=%d;',[ModID]);
          Database.Engine.ExecSQL(Query);

          for j := 0 to ModEntry.PathList.Count - 1 do
          begin
            SubDataset.Close;
            SubDataset.ParamCheck := true;
            SubDataset.SQL.Text := Format('SELECT * FROM "MOD" WHERE path LIKE "%%%s%%"',[ModEntry.PathList[j]]);
            SubDataset.Open;

            while not SubDataset.Eof do
            begin
             Inc(Order);
             DepID := SubDataset.FieldByName('id').AsInteger;
             Query := Format('INSERT INTO "DEPENDENCY" VALUES(%d, %d, %d);', [ModID, DepID, Order]);
             Database.Engine.ExecSQL(Query);

             SubDataset.Next;
            end;
          end;
        end
          else
        begin
          SendDebugError('MOD Not found');
        end;

      end;

      Dataset.Close;
      Dataset.SQL.Text := 'SELECT * FROM "MOD";';
      Dataset.Open;
      while not Dataset.Eof do
      begin
        //SendDebugFmt('%d:%s',[Dataset.RecNo, Dataset.FieldByName('Name').AsString]);

        ModID := Dataset.FieldByName('id').AsInteger;
        ModPath := FsRelToAbs(Dataset.FieldByName('Path').AsString) + IncludeTrailingBackslash(ARCHIVE_DIRECTORY_NAME);

        if DirectoryExists(ModPath) then
        begin
          WaitForm.IncProgress('Updating ARCHIVES');
          SubDataset.Close;
          SubDataset.ParamCheck := true;
          SubDataset.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE mod=:IdAsInteger;';
          SubDataset.Params.ParamByName('IdAsInteger').Value := IntToStr(ModID);
          SubDataset.Open;

          FSQL.Clear;
          FSQL.Add('BEGIN;');
          PrepareInsertAchivesFromPath(ModPath);
          FSQL.Add('COMMIT;');
          Database.Engine.ExecSQL(FSQL.Text);
        end;

        Dataset.Next;
      end;

      /// RebuildModDependency
    end;
  end;

  Active.Execute;
  SyncStart;
  WaitForm.IncProgress('Syncing started');
  Sleep(500);
  WaitForm.EndWait;
end;


procedure TFSViewForm.SetActiveIndex(const Value: Integer);
var
  Status : Boolean;
begin
  Status := FSyncRunning;
  if Status then SyncStop;
  FActiveIndex := Value;
  if Status then SyncStart;
  FPassCount := 0;
end;

procedure TFSViewForm.SyncReadEntry(Sender: TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed: boolean; cSize: integer);
var
  Filename : string;
  Path : string;
  Query : string;
begin
  SyncProgressBar.Max := Sender.Count;
  SyncProgressBar.Position := SyncProgressBar.Position+1;
  Filename := ExtractUnixFileName(Name);
  Path := ARCHIVE_PATH + ExtractUnixFilePath(Name);

  Query := Format('INSERT INTO "FILE" VALUES(NULL, "%s", "%s", "%d", "%d", "%d", "%d", "%d");', [Filename, Path, Offset, ucSize, Integer(Compressed), cSize, FArchiveID]);
  FSQL.Add(Query);

  if Sync.Enabled then
    Application.ProcessMessages;
end;

procedure TFSViewForm.SyncStart;
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  Assert(not Sync.Enabled, 'Syncing must be started/stopped with Start and Stop');
  FSyncRunning := true;
  Init.Execute;

  // Find active file system
  Node := FilesystemList.GetFirst;
  while (Node <> nil) do
  begin
    if (Node.Index = ActiveIndex) then
    begin
      Data := FilesystemList.GetNodeData(Node);
      Current_battlefield_path := IncludeTrailingBackslash(Data.Path);
      Database.Database := Current_battlefield_path + Data.Name;
    end;
    Node := FilesystemList.GetNext(Node);
  end;

  if FileExists(Database.Database) then
    Database.Open;

  Sync.Enabled := FSyncRunning;
  SyncProgressBar.Visible := False;
  SyncStatusGroup.Visible := FSyncRunning;
end;

procedure TFSViewForm.SyncStop;
begin
  FSyncRunning := false;
  SyncStatusGroup.Visible := FSyncRunning;
  Sync.Enabled := FSyncRunning;

  while FSyncMutex do
  begin
    Application.ProcessMessages;
    SendDebugWarning('Waiting for MUTEX');
  end;


  SyncDataset.Close;
end;


procedure TFSViewForm.SyncTimer(Sender: TObject);
var
  Archive : TRFAFile;
  ArchiveID : integer;
  ArchiveFilename : string;
  ArchiveDateTime: TDateTime;
  ArchiveMD5 : string;
  FileDateAndTime : TDateTime;
  FileMD5 : string;
  FileAgeString : string;
  Query : string;
  CommonActionText : string;

begin
  if FSyncMutex then
    Exit
  else
    FSyncMutex := true;

  try
    if Database.Connected then
    begin
      if not SyncDataset.Active or (SyncDataset.Active and SyncDataset.Eof) then
      begin
        Inc(FPassCount);
        SyncDataset.Close;
        SyncDataset.SQL.Text := 'SELECT * FROM "ARCHIVE";';
        SyncDataset.Open;
      end;

      ArchiveID := SyncDataset.FieldByName('id').AsInteger;
      ArchiveFilename := FsRelToAbs(SyncDataset.FieldByName('path').AsString + SyncDataset.FieldByName('name').AsString);
      ArchiveDateTime := SyncDataset.FieldByName('age').AsDateTime;
      ArchiveMD5 :=  SyncDataset.FieldByName('hash').AsString;
      CommonActionText := Format(' archive (%d/%d)',[SyncDataset.RecNo+1, SyncDataset.RecordCount]);

      SyncStatusAction.Caption := 'Scanning'+CommonActionText;
      SyncStatusArchiveName.Caption := ArchiveFilename;

      if FileAge(ArchiveFilename, FileDateAndTime) = true then
      begin
        // We need to compare time with an Epsilon due to rounding errors
        if Abs(FileDateAndTime - ArchiveDateTime) > 0.00001 then
        begin
          //SendDebugWarning('Date & Time compare failed');
          SyncStatusAction.Caption := 'Storing'+CommonActionText;
          FileMD5 :=  MD5FromFile(ArchiveFilename);
          if FileMD5 <> ArchiveMD5 then
          begin
            // Updates archives
            Query := Format('DELETE FROM "FILE" WHERE archive=%d;',[ArchiveID]);
            Database.Engine.ExecSQL(Query);

            FSQL.Clear;
            FSQL.Add('BEGIN;');
            FArchiveID := ArchiveID;
            Archive := TRFAFile.Create;
            SendDebugFmt('Reading entries from %d:%s',[SyncDataset.RecNo, SyncDataset.FieldByName('Name').AsString]);
            Archive.OnReadEntry := SyncReadEntry;
            SyncProgressBar.Visible := true;
            SyncProgressBar.Position := 0;
            if Archive.Open(ArchiveFilename) >=0 then
            begin
              FSQL.Add('COMMIT;');

              try
                if Sync.Enabled then
                  Database.Engine.ExecSQL(FSQL.Text)
                else
                begin
                  FSyncMutex := false;
                  Exit;
                end;
              except
                on e:Exception do
                begin
                  SyncStop;
                  SendDebugError(e.Message);
                end;
              end;

            end;
            Archive.Free;
            SyncProgressBar.Visible := false;
          end;

          FileAgeString := FormatDateTime('YYYY-MM-DD hh:mm:ss', FileDateAndTime);
          Query := Format('UPDATE "ARCHIVE" SET age="%s", hash="%s" WHERE id=%d;',[FileAgeString, FileMD5, ArchiveID]);
          Sync.Interval := SYNC_HARDTIME;

          try
            SendDebug(Query);
            Database.Engine.ExecSQL(Query);
          except
            on e:Exception do
            begin
              SyncStop;
              SendDebugError(e.Message);
            end;
          end;

        end
          else
        begin
          if FPassCount <= 1 then
            Sync.Interval := SYNC_HARDTIME
          else
            Sync.Interval := SYNC_EASYTIME;
        end;
      end
        else
      begin
        // File doesn't exists anymore, remove it
        Query := Format('DELETE FROM "FILE" WHERE archive=%d;',[ArchiveID]);
        Database.Engine.ExecSQL(Query);
        Query := Format('DELETE FROM "ARCHIVE" WHERE id=%d;',[ArchiveID]);
        Database.Engine.ExecSQL(Query);
      end;

      SyncDataset.Next;

    end
      else
    begin
      SyncStop;
      SendDebugWarning('Auto stop synctimer');
    end;

  except
    on e:Exception do
    begin
      SyncStop;
      SendDebugError(e.Message);
    end;

  end;
  FSyncMutex := false;
end;

procedure TFSViewForm.ImportExecute(Sender: TObject);
begin
//
end;

procedure TFSViewForm.ActiveExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := FilesystemList.GetFirstSelected;
  if Node <> nil then
  begin
    ActiveIndex := Node.Index;
    Active.Enabled := False;
    FilesystemList.Repaint;
  end;
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

procedure TFSViewForm.FilesystemListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  inherited;

  if Node.Index = ActiveIndex then
  with TargetCanvas do
  begin
    Pen.Style := psClear;
    Brush.Color := $00B0FFB0;
    InflateRect(CellRect,1,1);
    Rectangle(CellRect);
  end;
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

procedure TFSViewForm.FilesystemListStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
var
  Node: PVirtualNode;
begin
  if tsChangePending in Leave then
  begin
    Node := Sender.GetFirstSelected;
    if Node <> nil then
    begin
      Active.Enabled := ActiveIndex <> Node.Index;
    end
      else
    Active.Enabled := False;
  end;
end;

procedure TFSViewForm.FormStorageBeforeSavePlacement(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  Node := FilesystemList.GetFirst;
  FormStorage.WriteInteger('FilesystemCount', FilesystemList.RootNodeCount);
  FormStorage.WriteInteger('FilesystemSelected', ActiveIndex);
  while Node <> nil do
  begin
    Data := FilesystemList.GetNodeData(Node);
    FormStorage.WriteString(IntToStr(Node.Index)+':NAME', Data.Name);
    FormStorage.WriteString(IntToStr(Node.Index)+':PATH', Data.Path);
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
  SyncStop;
  FilesystemList.Clear;
  Count := FormStorage.ReadInteger('FilesystemCount', 0);
  for i := 0 to Count - 1 do
  begin
    Node := FilesystemList.AddChild(nil);
    Data := FilesystemList.GetNodeData(Node);
    Data.Name := FormStorage.ReadString(IntToStr(i)+':NAME');
    Data.Path := FormStorage.ReadString(IntToStr(i)+':PATH');
  end;
  ActiveIndex := FormStorage.ReadInteger('FilesystemSelected', -1);
  SyncStart;
end;




end.
