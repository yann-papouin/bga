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
 * The Original Code is GuiFSSettings (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiFSSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SyncObjs,
  Dialogs, GuiFormCommon, ActnList, SpTBXControls, StdCtrls, SpTBXEditors, SpTBXItem, VirtualTrees,
  ExtCtrls, TB2Item, TB2Dock, TB2Toolbar, FSLib, JvComponentBase, JvFormPlacement, DB, RFALib,
  SqlitePassDbo, JvAppStorage, JvAppRegistryStorage, Grids, DBGrids, JvExDBGrids, JvDBGrid, JvExControls, JvAnimatedImage, JvGIFCtrl;

type

  pFilesystemData = ^rFilesystemData;

  rFilesystemData = record
    Name: string;
    Path: string;
  end;

  TFSSettingsForm = class;

  TSyncState =
  (
    ssWaiting,
    ssStopping,
    ssWorking
  );

  TSyncThread = class(TThread)
  private
    FMutex : boolean;
    FWaitTime : integer;
    FPassCount : integer;
    FUpdatePass : integer;
    FArchiveID : integer;
    FSQL : TStringList;
    FOwner : TFSSettingsForm;
    FDataset: TSqlitePassDataset;

    FTextAction : string;
    FTextFilename : string;
    FTextStatus : string;
    FImageStatus : integer;
    FAnimScan : boolean;
    FAnimStore : boolean;
  protected
    procedure Execute; override;
    procedure SyncReadEntry(Sender: TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed: boolean; cSize: integer);

  public
    State : TSyncState;
    constructor Create(CreateSuspended:boolean);
    destructor Destroy; override;
  end;

  TFSModData = record
    ID : integer;
    Path : string[255];
    Name : string[255];
  end;

  TFSArchiveData = record
    ID : integer;
    Path : string[255];
    Name : string[255];
  end;

  TFSFileData = record
    ID : integer;
    Path : string[255];
    Filename : string[255];
    Offset : Int64;
    Size : Int64;
    Compressed : boolean;
    CompSize : Int64;
    Archive : TFSArchiveData;
  end;


  TFSListMods     = procedure(Sender: TObject; FSData: TFSModData) of object;
  TFSListArchives = procedure(Sender: TObject; FSData: TFSArchiveData) of object;
  TFSListFiles    = procedure(Sender: TObject; FSData: TFSFileData) of object;

  //TFSList = procedure(Sender: TObject; Name: string; Path: string; ID: integer) of object;

  TFSSettingsForm = class(TFormCommon)
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
    SyncDataset: TSqlitePassDataset;
    SpTBXItem2: TSpTBXItem;
    Active: TAction;
    SyncStatusGroup: TSpTBXGroupBox;
    SyncStatusAction: TSpTBXLabel;
    SyncStatusArchiveName: TSpTBXLabel;
    Delete: TAction;
    SyncAnimScan: TJvGIFAnimator;
    SyncAnimStore: TJvGIFAnimator;
    SyncStatus: TSpTBXLabel;
    UpdateVCL: TTimer;
    ApplicationRun: TAction;
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
    procedure FormDestroy(Sender: TObject);
    procedure ActiveExecute(Sender: TObject);
    procedure FilesystemListStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure FilesystemListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure UpdateVCLTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplicationRunExecute(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FActiveIndex : Integer;
    FSQL : TStringList;
    FSyncThread : TSyncThread;
    FOnChange: TNotifyEvent;
    FOnListMods: TFSListMods;
    FOnListArchives: TFSListArchives;
    FOnListFiles: TFSListFiles;
    procedure ApplySettingsData(Data: pFilesystemData);
    { Déclarations privées }
    function SyncInit : boolean;
    procedure SyncStart;
    procedure SyncStop;
    procedure PrepareUpdateDatabase;
    procedure SetActiveIndex(const Value: Integer);
  public
    { Déclarations publiques }
    procedure ListMods;
    procedure ListArchives(ModID : integer);
    procedure ListFiles(ModID : integer);

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnListMods: TFSListMods read FOnListMods write FOnListMods;
    property OnListArchives: TFSListArchives read FOnListArchives write FOnListArchives;
    property OnListFiles: TFSListFiles read FOnListFiles write FOnListFiles;

    property ActiveIndex: Integer read FActiveIndex write SetActiveIndex;
  end;


var
  FSSettingsForm: TFSSettingsForm;

const
  FS_SEPARATOR = '|';
  SYNC_HARDTIME = 20;
  SYNC_EASYTIME = 200;

  IMG_SYNC_STOP = 312;
  IMG_SYNC_INIT = 325;
  IMG_SYNC_RUNNING = 321;
  IMG_SYNC_READY = 319;

implementation

{$R *.dfm}

uses
  DbugIntf,
  Math,
  GuiWait,
  AppLib,
  CommonLib,
  GuiFSEdit,
  GuiSkinDialog,
  Resources,
  IOUtils,
  Types,
  StringFunction,
  TypInfo,
  MD5Api;


procedure TFSSettingsForm.FormCreate(Sender: TObject);
begin
  FSQL := TStringList.Create;
  FSyncThread := TSyncThread.Create(false);
  FSyncThread.FOwner := Self;
  FSyncThread.FDataset := SyncDataset;
end;

procedure TFSSettingsForm.FormDestroy(Sender: TObject);
begin
  Ok.Enabled := false;
  Cancel.Enabled := false;
  SyncStop;

  Database.Close;
  FSyncThread.Free;
  FSQL.Free;
  inherited;
end;

procedure TFSSettingsForm.FormShow(Sender: TObject);
begin
  inherited;
  if not Assigned(FSEditForm) then
    Application.CreateForm(TFSEditForm, FSEditForm);
end;


procedure TFSSettingsForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //SyncStop;
end;

procedure TFSSettingsForm.FormActivate(Sender: TObject);
begin
  inherited;
  ApplicationRun.Execute;
end;

procedure TFSSettingsForm.ApplicationRunExecute(Sender: TObject);
begin
  ApplicationRun.Enabled := false;
  FormStorage.RestoreFormPlacement;
end;


procedure TFSSettingsForm.OkExecute(Sender: TObject);
begin
  FormStorage.SaveFormPlacement;
  ModalResult := mrOk;
  Close;
end;



procedure TFSSettingsForm.CancelExecute(Sender: TObject);
begin
  FormStorage.RestoreFormPlacement;
  ModalResult := mrCancel;
  Close;
end;



procedure TFSSettingsForm.RemoveExecute(Sender: TObject);
var
  Node, NextNode: PVirtualNode;
begin
  Node := FilesystemList.GetFirstSelected;

  if Node <> nil then
    SyncStop;

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

procedure TFSSettingsForm.PrepareUpdateDatabase;
var
  i,j :Integer;
  Order : integer;
  Query : string;
  ModEntry : TBattlefieldModEntry;
  ModPath : string;
  ModID : integer;
  DepID : Integer;
  Options : TLocateOptions; 
  Node : PVirtualNode;
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
  // Find selected file system
  Node := FilesystemList.GetFirstSelected;

  if Node = nil then
    Exit;

  SyncStop;

  if SyncInit then
  begin
    WaitForm.BeginWait;
    WaitForm.IncProgress('Init', 5);


    if Node <> nil then
    begin
      Data := FilesystemList.GetNodeData(Node);
      Current_battlefield_path := IncludeTrailingBackslash(Data.Path);
      Database.Database := Current_battlefield_path + Data.Name;
    end;

    Options := [loCaseInsensitive, loPartialKey];

    if FileExists(Database.Database) and (DebugHook <> 0) then
      if ShowDialog('Database exists', 'Delete older data and create a new database?', mtInformation, mbYesNo, mbNo, 0)= mrOk then
        DeleteFile(Database.Database);

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
          FSEditForm.ReadModsInfos(Current_battlefield_path + MOD_DIRECTORY_NAME);
        end;

        for i:= 0 to FSEditForm.Modentries.Count - 1 do
        begin
          ModEntry := FSEditForm.Modentries[i];

          if not Dataset.Locate('name', ModEntry.GameName, Options) then
          begin
            Query := Format('INSERT INTO "MOD" VALUES(NULL, "%s", "%s");', [ModEntry.GameName, FsAbsToRel(ModEntry.AbsolutePath)]);
            Database.Engine.ExecSQL(Query);
          end;
        end;

        for i:= 0 to FSEditForm.Modentries.Count - 1 do
        begin
          ModEntry := FSEditForm.Modentries[i];
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

        SubDataset.Close;
        Dataset.Close;
      end;
    end;

    Active.Enabled := true;
    Active.Execute;
    WaitForm.IncProgress('Syncing started');
    Sleep(500);
    WaitForm.EndWait;
  end
    else
  begin
    ShowError('Database engine not found', 'You need the sqlite3.dll library in the same folder than BGA.exe in order to use this feature.');
  end;

end;


procedure TFSSettingsForm.UpdateExecute(Sender: TObject);
begin
  PrepareUpdateDatabase;
end;


procedure TFSSettingsForm.SetActiveIndex(const Value: Integer);
var
  State : TSyncState;
begin
  State := FSyncThread.State;
  if State <> ssWaiting then SyncStop;
  FActiveIndex := Value;
  SyncStart;
  FSyncThread.FPassCount := 0;

  if Assigned(OnChange) then
    OnChange(Self);
end;


function TFSSettingsForm.SyncInit : boolean;
var
  SQLiteDll :string;
begin
  Result := false;

  // Reset components to empty vars
  Database.Connected := false;
  Database.Database := EmptyStr;
  Current_battlefield_path := EmptyStr;
  SQLiteDll := GetAppDirectory + 'sqlite3.dll';

  if FileExists(SQLiteDll) then
  begin
    Result := true;
    if Database.SQLiteLibrary = EmptyStr then
      Database.SQLiteLibrary := SQLiteDll;
  end;
end;

procedure TFSSettingsForm.SyncStart;
var
  Node: PVirtualNode;
  Data: pFilesystemData;
  WarnMsg : string;
begin
  Assert(FSyncThread.State = ssWaiting, 'Syncing must be started when not already running');

  if SyncInit then
  begin
    SyncStatus.Caption := 'Init';
    SyncStatus.ImageIndex := IMG_SYNC_INIT;

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

    if Database.Database <> EmptyStr then
    begin
      if FileExists(Database.Database) then
        Database.Open
      else
      begin
        WarnMsg := 'Please run "Update" first to init the filesystem database named ';
        WarnMsg := WarnMsg + ExtractFileName(Database.Database);
        ShowMessage('Syncing disabled', WarnMsg);
      end;
    end;

    SyncStatusGroup.Visible := True;
    SyncAnimScan.Animate := True;
    SyncAnimStore.Animate := True;
    SyncAnimScan.Visible := True;
    SyncAnimStore.Visible := false;

    /// Initialize transition values with current values
    with FSyncThread do
    begin
      FTextAction := SyncStatusAction.Caption;
      FTextFilename := SyncStatusArchiveName.Caption;

      FAnimScan := SyncAnimScan.Visible;
      FAnimStore := SyncAnimStore.Visible;

      FImageStatus := SyncStatus.ImageIndex;
      FTextStatus := SyncStatus.Caption;
    end;

    FSyncThread.State := ssWorking;
  end
    else
  begin
    SyncStatus.Caption := 'Engine not found';
    SyncStatus.ImageIndex := IMG_SYNC_STOP;
  end;

end;

procedure TFSSettingsForm.SyncStop;
begin
  if FSyncThread.State <> ssWaiting then
  begin
    FSyncThread.State := ssStopping;

    SyncStatus.Caption := 'Stopping';
    while FSyncThread.State <> ssWaiting do
    begin
      // Wait here for the thread to finish //Sleep(100);
    end;
  end;

  SyncAnimScan.Animate := False;
  SyncAnimStore.Animate := False;
  SyncStatusGroup.Visible := False;
  SyncDataset.Close;

  SyncStatus.Caption := 'Disabled';
  SyncStatus.ImageIndex := IMG_SYNC_STOP;
end;



procedure TFSSettingsForm.ImportExecute(Sender: TObject);
begin
//
end;

procedure TFSSettingsForm.ActiveExecute(Sender: TObject);
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

procedure TFSSettingsForm.AddExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  FSEditForm.OpenMode := omAdd;
  if FSEditForm.ShowModal = mrOk then
  begin
    Node := FilesystemList.AddChild(nil);
    Data := FilesystemList.GetNodeData(Node);
    ApplySettingsData(Data);
  end;
end;


procedure TFSSettingsForm.ApplySettingsData(Data: pFilesystemData);
begin
  Data.Name := FSEditForm.FilesystemName.Text;
  Data.Path := FSEditForm.BattlefieldDir.Text;
end;

procedure TFSSettingsForm.EditExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  Node := FilesystemList.GetFirstSelected;
  if Node <> nil then
  begin
    Data := FilesystemList.GetNodeData(Node);
    FSEditForm.FilesystemName.Text := Data.Name;
    FSEditForm.BattlefieldDir.Text := Data.Path;

    FSEditForm.OpenMode := omEdit;
    if FSEditForm.ShowModal = mrOk then
    begin
      ApplySettingsData(Data);
    end;
  end;
end;

procedure TFSSettingsForm.FilesystemChoiceChange(Sender: TObject);
begin
  inherited;
  //Settings.Enabled := FileExists(FilesystemChoice.Text);
end;

procedure TFSSettingsForm.FilesystemListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
  inherited;

  if Node.Index = ActiveIndex then
  with TargetCanvas do
  begin
    Pen.Style := psClear;
    Brush.Color := $00DDDDDD;
    InflateRect(CellRect,1,1);
    Rectangle(CellRect);
  end;
end;

procedure TFSSettingsForm.FilesystemListDblClick(Sender: TObject);
begin
  Edit.Execute;
end;

procedure TFSSettingsForm.FilesystemListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
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

procedure TFSSettingsForm.FilesystemListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: pFilesystemData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TFSSettingsForm.FilesystemListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rFilesystemData);
end;

procedure TFSSettingsForm.FilesystemListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
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

procedure TFSSettingsForm.FilesystemListStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
var
  Node: PVirtualNode;
begin
  if tsChangePending in Leave then
  begin
    Node := Sender.GetFirstSelected;
    Active.Enabled := (Node <> nil) and (ActiveIndex <> Node.Index);
    Remove.Enabled := (Node <> nil);
    Edit.Enabled   := (Node <> nil);
    Update.Enabled := (Node <> nil);
  end;
end;

procedure TFSSettingsForm.FormStorageBeforeSavePlacement(Sender: TObject);
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


procedure TFSSettingsForm.FormStorageAfterRestorePlacement(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFilesystemData;
  Count, i: Integer;
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
end;


procedure TFSSettingsForm.UpdateVCLTimer(Sender: TObject);
begin
  inherited;

  if FSyncThread.State = ssWorking then
  begin
    with FSyncThread do
    begin
      SyncStatusAction.Caption := FTextAction;
      SyncStatusArchiveName.Caption := FTextFilename;

      SyncAnimScan.Visible := FAnimScan;
      SyncAnimStore.Visible := FAnimStore;

      SyncStatus.ImageIndex := FImageStatus;
      SyncStatus.Caption := FTextStatus;
    end;
  end;

end;


procedure TFSSettingsForm.ListMods;
var
  Data : TFSModData;
begin
  // Open database if settings are correct
  if FileExists(Database.Database) then
  begin
    Database.Connected := true;
    if Database.Connected then
    begin
      Dataset.Close;
      Dataset.SQL.Text := 'SELECT * FROM "MOD";';
      Dataset.Open;

      while not Dataset.Eof do
      begin
        Data.ID := Dataset.FieldByName('id').AsInteger;
        Data.Name := Dataset.FieldByName('name').AsString;
        Data.Path := Dataset.FieldByName('path').AsString;

        if Assigned(OnListMods) then
          OnListMods(Self, Data);

        Dataset.Next;
      end;

      Dataset.Close;
    end;
  end;

end;

procedure TFSSettingsForm.ListArchives(ModID : integer);
var
  Data : TFSArchiveData;
begin
  if Assigned(OnListArchives) then
    OnListArchives(Self, Data);
end;

procedure DebugFields(List : TFieldList);
var
  i: integer;
begin
  for i := 0 to List.Count - 1 do
  begin
    SendDebug(List.Fields[i].FullName);
  end;
end;

{.$DEFINE DEBUG_FS}

procedure TFSSettingsForm.ListFiles(ModID : integer);
var
  Data : TFSFileData;
  {$IfDef DEBUG_FS}
  DbgLow, DbgHigh : integer;
  {$EndIf}
begin

  {$IfDef DEBUG_FS}
  DbgLow := 19489;
  DbgHigh := 19520;
  {$EndIf}

  // Open database if settings are correct
  if FileExists(Database.Database) then
  begin
    Database.Connected := true;
    if Database.Connected then
    begin
      Dataset.Close;

      Dataset.SQL.Clear;
      Dataset.SQL.Add('SELECT file.id, file.filename, file.path, file.offset,');
      Dataset.SQL.Add('  file.size, file.compressed, file.csize, archive.id,');
      Dataset.SQL.Add('  archive.name, archive.path');
      Dataset.SQL.Add(' FROM mod, archive, file');
      Dataset.SQL.Add(' WHERE mod.id = ' + IntToStr(ModID));
      Dataset.SQL.Add(' AND file.archive = archive.id');
      Dataset.SQL.Add(' AND archive.mod = mod.id;');

      Dataset.Open;

      while not Dataset.Eof do
      begin
        //DebugFields(Dataset.FieldList);

        Data.ID := Dataset.FieldByName('id').AsInteger;
        Data.Path := Dataset.FieldByName('path').AsString;
        Data.Filename := Dataset.FieldByName('filename').AsString;
        Data.Offset := Dataset.FieldByName('offset').AsInteger;
        Data.Size := Dataset.FieldByName('size').AsInteger;
        Data.Compressed := Dataset.FieldByName('compressed').AsBoolean;
        Data.CompSize := Dataset.FieldByName('csize').AsInteger;

        Data.Archive.ID := Dataset.FieldByName('archive.id').AsInteger;
        Data.Archive.Name := Dataset.FieldByName('name').AsString;
        Data.Archive.Path := Dataset.FieldByName('archive.path').AsString;

        {$IfDef DEBUG_FS}
        if InRange(ID, DbgLow, DbgHigh) then
        {$EndIf}
          if Assigned(OnListFiles) then
            OnListFiles(Self, Data);

        Dataset.Next;
      end;

      Dataset.Close;
    end;
  end;

end;


{ TSyncThread }

constructor TSyncThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate:=false;
  Priority:=tpNormal;

  FSQL := TStringList.Create;
  FWaitTime := SYNC_EASYTIME;
end;

destructor TSyncThread.Destroy;
begin
  FSQL.Free;
  inherited;
end;

procedure TSyncThread.Execute;
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
  inherited;
  repeat
  Sleep(FWaitTime); //en millisecondes

  if (State = ssStopping) then
    State := ssWaiting;

  if (State = ssWorking) and not FMutex then
  begin
    FMutex := true;


    try
      if FDataset.Database.Connected then
      begin
        if not FDataset.Active or (FDataset.Active and FDataset.Eof) then
        begin
          Inc(FPassCount);
          FDataset.Close;
          FDataset.SQL.Text := 'SELECT * FROM "ARCHIVE";';
          FDataset.Open;
        end;

        ArchiveID := FDataset.FieldByName('id').AsInteger;
        ArchiveFilename := FsRelToAbs(FDataset.FieldByName('path').AsString + FDataset.FieldByName('name').AsString);
        ArchiveDateTime := FDataset.FieldByName('age').AsDateTime;
        ArchiveMD5 :=  FDataset.FieldByName('hash').AsString;
        CommonActionText := Format(' archive (%d/%d)',[FDataset.RecNo+1, FDataset.RecordCount]);

        FTextAction := 'Scanning' + CommonActionText;
        FTextFilename := ArchiveFilename;

        if FileAge(ArchiveFilename, FileDateAndTime) = true then
        begin
          // We need to compare time with an Epsilon due to rounding errors
          if Abs(FileDateAndTime - ArchiveDateTime) > 0.00001 then
          begin
            //SendDebugWarning('Date & Time compare failed');

            FAnimScan := false;
            FAnimStore := true;
            FTextAction := 'Storing'+ CommonActionText;
            FUpdatePass := FPassCount;

            FTextStatus := 'Running';
            FImageStatus := IMG_SYNC_RUNNING;

            FileMD5 :=  MD5FromFile(ArchiveFilename);
            if FileMD5 <> ArchiveMD5 then
            begin
              // Updates archives
              Query := Format('DELETE FROM "FILE" WHERE archive=%d;',[ArchiveID]);
              FDataset.Database.Engine.ExecSQL(Query);

              FSQL.Clear;
              FSQL.Add('BEGIN;');
              FArchiveID := ArchiveID;
              Archive := TRFAFile.Create;
              //SendDebugFmt('Reading entries from %d:%s',[FDataset.RecNo, FDataset.FieldByName('Name').AsString]);
              Archive.OnReadEntry := SyncReadEntry;

              if Archive.Open(ArchiveFilename, True) = orReadOnly then
              begin
                FSQL.Add('COMMIT;');
                FDataset.Database.Engine.ExecSQL(FSQL.Text);
              end;
              Archive.Free;
            end;

            FileAgeString := FormatDateTime('YYYY-MM-DD hh:mm:ss', FileDateAndTime);
            Query := Format('UPDATE "ARCHIVE" SET age="%s", hash="%s" WHERE id=%d;',[FileAgeString, FileMD5, ArchiveID]);
            FWaitTime := SYNC_HARDTIME;

            FDataset.Database.Engine.ExecSQL(Query);
          end
            else
          begin

            FAnimStore := false;
            FAnimScan := true;


            if FPassCount <= 1 then
              FWaitTime := SYNC_HARDTIME
            else
              FWaitTime := SYNC_EASYTIME;
          end;
        end
          else
        begin
          // File doesn't exists anymore, remove it
          Query := Format('DELETE FROM "FILE" WHERE archive=%d;',[ArchiveID]);
          FDataset.Database.Engine.ExecSQL(Query);
          Query := Format('DELETE FROM "ARCHIVE" WHERE id=%d;',[ArchiveID]);
          FDataset.Database.Engine.ExecSQL(Query);
        end;

        if (FPassCount > FUpdatePass) and (FPassCount > 1) then
        begin
          FTextStatus := 'Ready';
          FImageStatus := IMG_SYNC_READY;
        end;

        if FDataset.Active then
          FDataset.Next;

      end
        else
      begin
        State := ssWaiting;
        FOwner.SyncStop;
      end;

    except
      on e:Exception do
      begin   
        SendDebugError(e.Message);
        State := ssWaiting;
        FOwner.SyncStop;
      end;

    end;

    FMutex := false;
  end;

  until Terminated;
end;


procedure TSyncThread.SyncReadEntry(Sender: TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed: boolean; cSize: integer);
var
  Filename : string;
  Path : string;
  Query : string;
begin
  //Sender.Count;
  Filename := ExtractUnixFileName(Name);
  Path := ARCHIVE_PATH + ExtractUnixFilePath(Name);

  Query := Format('INSERT INTO "FILE" VALUES(NULL, "%s", "%s", "%d", "%d", "%d", "%d", "%d");', [Filename, Path, Offset, ucSize, Integer(Compressed), cSize, FArchiveID]);
  FSQL.Add(Query);
end;

end.
