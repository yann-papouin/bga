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

  TFSViewForm = class;

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
    FOwner : TFSViewForm;
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
    Init: TAction;
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
    procedure InitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActiveExecute(Sender: TObject);
    procedure FilesystemListStateChange(Sender: TBaseVirtualTree; Enter, Leave: TVirtualTreeStates);
    procedure FilesystemListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure UpdateVCLTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FActiveIndex : Integer;
    FSQL : TStringList;
    FSyncThread : TSyncThread;
    procedure ApplySettingsData(Data: pFilesystemData);
    procedure SyncThreadTerminated(Sender : TObject);
    { Déclarations privées }
    procedure SyncStart;
    procedure SyncStop;
    procedure PrepareUpdateDatabase;
    procedure SetActiveIndex(const Value: Integer);
  public
    { Déclarations publiques }
    property ActiveIndex : Integer read FActiveIndex write SetActiveIndex;
  end;


var
  FSViewForm: TFSViewForm;

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
  GuiWait,
  AppLib,
  CommonLib,
  GuiFSSettings,
  GuiSkinDialog,
  Resources,
  IOUtils,
  Types,
  StringFunction,
  TypInfo,
  MD5Api;


procedure TFSViewForm.FormCreate(Sender: TObject);
begin
  FSQL := TStringList.Create;
  FSyncThread := TSyncThread.Create(false);
  FSyncThread.FOwner := Self;
  FSyncThread.FDataset := SyncDataset;

  FormStorage.RestoreFormPlacement;
end;


procedure TFSViewForm.FormDestroy(Sender: TObject);
begin
  Ok.Enabled := false;
  Cancel.Enabled := false;
  SyncStop;

  Database.Close;
  FSyncThread.Free;
  FSQL.Free;
  inherited;
end;


procedure TFSViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SyncStop;
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

procedure TFSViewForm.InitExecute(Sender: TObject);
begin
  // Reset components to empty vars
  Database.Connected := false;
  Database.Database := EmptyStr;
  Current_battlefield_path := EmptyStr;

  if Database.SQLiteLibrary = EmptyStr then
    Database.SQLiteLibrary := GetAppDirectory + 'sqlite3.dll';
end;

procedure TFSViewForm.PrepareUpdateDatabase;
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
  WaitForm.BeginWait;
  WaitForm.IncProgress('Init', 5);
  Init.Execute;

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
  WaitForm.IncProgress('Syncing started');
  Sleep(500);
  WaitForm.EndWait;
end;


procedure TFSViewForm.UpdateExecute(Sender: TObject);
begin
  PrepareUpdateDatabase;
end;


procedure TFSViewForm.SetActiveIndex(const Value: Integer);
var
  State : TSyncState;
begin
  State := FSyncThread.State;
  if State <> ssWaiting then SyncStop;
  FActiveIndex := Value;
  SyncStart;
  FSyncThread.FPassCount := 0;
end;


procedure TFSViewForm.SyncStart;
var
  Node: PVirtualNode;
  Data: pFilesystemData;
begin
  Assert(FSyncThread.State = ssWaiting, 'Syncing must be started when not already running');

  SyncStatus.Caption := 'Init';
  SyncStatus.ImageIndex := IMG_SYNC_INIT;

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
    Database.Open
  else
    ShowMessage('Syncing disabled', 'Please run "Update" first to init filesystem database');

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
end;

procedure TFSViewForm.SyncStop;
begin
  if FSyncThread.State <> ssWaiting then
  begin
    FSyncThread.State := ssStopping;

    SyncStatus.Caption := 'Stopping';
    while FSyncThread.State <> ssWaiting do
    begin
      Sleep(100);
    end;
  end;

  SyncAnimScan.Animate := False;
  SyncAnimStore.Animate := False;
  SyncStatusGroup.Visible := False;
  SyncDataset.Close;

  SyncStatus.Caption := 'Disabled';
  SyncStatus.ImageIndex := IMG_SYNC_STOP;
end;


procedure TFSViewForm.SyncThreadTerminated(Sender: TObject);
begin
  ShowMessage('SyncThreadTerminated');
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
    Brush.Color := $00DDDDDD;
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
  //SyncStart;
end;


procedure TFSViewForm.UpdateVCLTimer(Sender: TObject);
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
