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
 * The Original Code is GuiRFAView (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiRFAView;

interface

uses
  JvGnuGetText, DbugIntf, ShellAPI,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, VirtualTrees, JvComponentBase, JvFormPlacement, ImgList,
  JclFileUtils, PngImageList, ExtCtrls, ActiveX, Types, SpTBXControls, SpTBXItem,
  TB2Item, TB2Dock, TB2Toolbar, ActnList, JvMRUList, JvAppInst,
  Menus, RFALib, SpTBXEditors, JvBaseDlg, JvAppStorage,
  JvAppRegistryStorage, GuiUpdateManager, DragDrop, DropSource, DragDropFile;

type

  TFileType =
  (
    ftFolder,
    ftFile,
    ftFileCON,
    ftFileINC,
    ftFileSSC,
    ftFileTXT,
    ftFileBAK,
    ftFileBIK,
    ftFileWAV,
    ftFileDDS,
    ftFileTGA,
    ftFileRAW,
    ftFileDAT,
    ftFileRS,
    ftFileRCM,
    ftFileLSB,
    ftFilePAL,
    ftFileZIP,
    ftFileRAR,
    ftFileGZ,
    ftFileBZ2,
    ftFile7Z,
    ftFileDIF,
    ftFileFONT,
    ftFileBAF,
    ftFileSKN,
    ftFileSKE,
    ftFileVSO,
    ftFileSM,
    ftFileTM
  );

  TEntryModification =
  (
    fsNew,
    fsDelete,
    fsEntry,
    fsExternal,
    fsConflict
  );

  TEntryStatus = set of TEntryModification;

  TShiftWay =
  (
    shLeft,
    shRight
  );

  TEditResult =
  (
    edCancel,
    edOk,
    edInvalid
  );

  TRFAViewForm = class(TForm)
    RFAList: TVirtualStringTree;
    FormStorage: TJvFormStorage;
    ExplorerImg: TPngImageList;
    TotalProgressBar: TSpTBXProgressBar;
    TotalProgressLabel: TSpTBXLabel;
    Sync: TTimer;
    StatusBar: TSpTBXStatusBar;
    tbMenuBar: TSpTBXToolbar;
    mFile: TSpTBXSubmenuItem;
    mEdit: TSpTBXSubmenuItem;
    mHelp: TSpTBXSubmenuItem;
    TopDock: TSpTBXDock;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    Actions: TActionList;
    RecentList: TJvMruList;
    SaveDialog: TSaveDialog;
    OpenDialog: TOpenDialog;
    Open: TAction;
    Save: TAction;
    SaveAs: TAction;
    RebuildRecentList: TAction;
    RecentMenu: TSpTBXSubmenuItem;
    SpTBXItem4: TSpTBXItem;
    Quit: TAction;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem7: TSpTBXItem;
    Recent: TAction;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXSubmenuItem3: TSpTBXSubmenuItem;
    PackDirectory: TAction;
    ExtractAll: TAction;
    ExtractSelected: TAction;
    ExtractModFolder: TAction;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXItem11: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    About: TAction;
    AppInstances: TJvAppInstances;
    ApplicationRun: TAction;
    Settings: TAction;
    ViewerPopup: TSpTBXPopupMenu;
    PreviewRAW: TAction;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXItem13: TSpTBXItem;
    SearchBar: TSpTBXPanel;
    Search: TSpTBXEdit;
    SpTBXLabel1: TSpTBXLabel;
    SpTBXButton1: TSpTBXButton;
    CloseSearchBar: TAction;
    ShowSearchBar: TAction;
    SpTBXItem12: TSpTBXItem;
    Preview: TAction;
    SpTBXItem14: TSpTBXItem;
    AppStorage: TJvAppRegistryStorage;
    Defrag: TAction;
    Action1: TAction;
    SpTBXItem15: TSpTBXItem;
    New: TAction;
    SpTBXItem16: TSpTBXItem;
    NewVersionAvailable: TAction;
    SpTBXItem17: TSpTBXItem;
    SubProgressBar: TSpTBXProgressBar;
    ProgressPanel: TSpTBXPanel;
    SpTBXButton2: TSpTBXButton;
    Cancel: TAction;
    Fragmentation: TSpTBXLabelItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    ArchiveSize: TSpTBXLabelItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    ArchiveFileCount: TSpTBXLabelItem;
    ExpandAll: TAction;
    CollapseAll: TAction;
    ExpandSelected: TAction;
    CollapseSelected: TAction;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXItem19: TSpTBXItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    DropFileSource: TDropFileSource;
    NewFolder: TAction;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    SpTBXItem22: TSpTBXItem;
    SpTBXSubmenuItem4: TSpTBXSubmenuItem;
    procedure FormCreate(Sender: TObject);
    procedure RFAListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure RFAListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure RFAListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure RFAListDblClick(Sender: TObject);
    procedure RFAListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean;
      var ImageIndex: Integer);
    procedure RFAListDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure RFAListCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure RFAListDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure RFAListKeyAction(Sender: TBaseVirtualTree; var CharCode: Word;
      var Shift: TShiftState; var DoDefault: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SyncTimer(Sender: TObject);
    procedure RFAListBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure OpenExecute(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
    procedure RecentListEnumText(Sender: TObject; Value: string; Index: Integer);
    procedure RebuildRecentListExecute(Sender: TObject);
    procedure OpenRecentClick(Sender: TObject);
    procedure RecentExecute(Sender: TObject);
    procedure ApplicationRunExecute(Sender: TObject);
    procedure AppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
    procedure QuitExecute(Sender: TObject);
    procedure RFAListStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure PreviewRAWExecute(Sender: TObject);
    procedure ShowSearchBarExecute(Sender: TObject);
    procedure CloseSearchBarExecute(Sender: TObject);
    procedure SearchChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AboutExecute(Sender: TObject);
    procedure PreviewExecute(Sender: TObject);
    procedure ExtractSelectedExecute(Sender: TObject);
    procedure ExtractAllExecute(Sender: TObject);
    procedure NewExecute(Sender: TObject);
    procedure PackDirectoryExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DefragExecute(Sender: TObject);
    procedure NewVersionAvailableExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure ExpandSelectedExecute(Sender: TObject);
    procedure CollapseSelectedExecute(Sender: TObject);
    procedure ExpandAllExecute(Sender: TObject);
    procedure CollapseAllExecute(Sender: TObject);
    procedure NewFolderExecute(Sender: TObject);
    procedure RFAListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure RFAListEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure RFAListNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SaveExecute(Sender: TObject);
    procedure DropFileSourceDrop(Sender: TObject; DragType: TDragType;
      var ContinueDrop: Boolean);
    procedure DropFileSourceGetData(Sender: TObject;
      const FormatEtc: tagFORMATETC; out Medium: tagSTGMEDIUM;
      var Handled: Boolean);
    procedure DropFileSourceAfterDrop(Sender: TObject; DragResult: TDragResult;
      Optimized: Boolean);
    procedure RFAListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    FEditResult : TEditResult;
    FApplicationTitle : string;
    FSearchText: string;
    FSyncNode : PVirtualNode;
    FArchive : TRFAFile;
    procedure RemoveEmptyFolders;
    function CountFilesByStatus(Node: PVirtualNode; Status: TEntryStatus; IncludeWithoutStatus : boolean = false) : cardinal;
    //procedure DeleteSelectionByData;
    procedure DeleteSelection;
    procedure ExtendSelection(Node : PVirtualNode);
    procedure Sort;
    procedure SyncStart;
    procedure SyncStop;
    procedure SyncAll;
    procedure CheckStatus(Node : PVirtualNode);
    procedure PropagateStatus(Node : PVirtualNode; Status : TEntryModification);

    function FindFileByName(Filename :string) : PVirtualNode;

    procedure ExtractTo(Directory : string; List : TStringList = nil);
    procedure ExpandSelection(Value : boolean);

    function IsFile(FileType : TFileType) : boolean;
    function GetTitle: string;
    procedure SetTitle(const Value: string);

    procedure Add(MainNode : PVirtualNode; List: TStringList; Path : string = '');

    procedure ShiftData(ShiftData : TRFAResult; ShiftWay : TShiftWay; IgnoreNode : PVirtualNode = nil);
    procedure SetSearchText(const Value: string);

    function LastOne(Offset : Int64) : boolean;

    function ExtractTemporary(Node : PVirtualNode) : string;
    procedure EditSelection;
    procedure Reset;
    function QuickSave(Defrag : boolean): boolean;
    function QuickOpen: boolean;

    procedure UpdateInfobar;

    procedure UpdateReply(Sender: TObject; Result : TUpdateResult);
    procedure ReadEntry(Sender : TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed : boolean; cSize : integer);
    procedure SubProgress(Sender : TRFAFile; Operation : TRFAOperation; Value : Integer = 0);
    procedure TotalProgress(Operation : TRFAOperation; Value : Integer; Max:integer);
  public
    { Déclarations publiques }
    function LoadMap(Path : string) :boolean;
    function SaveMap(Path : string; Defrag : boolean = false) :boolean;
    procedure ExportFile(Node: PVirtualNode; OutputStream : TStream);
    property Title : string read GetTitle write SetTitle;
    property SearchText : string read FSearchText write SetSearchText;

  end;

  pFse = ^rFse;
  rFse = record
    RFAFile : TRFAFile;
    // Data fields
    //BF42FullName : AnsiString;
    EntryName : AnsiString; (* == BF42FullName*)
    Offset : Int64;
    Size : Int64;
    Compressed : boolean;
    CompSize : integer;
    // Extended fields
    W32Path : string[255];
    W32Name : string[255];
    W32Ext : string[255];
    FileType : TFileType;
    Status : TEntryStatus;
    // External fields
    ExternalFilePath : string[255];
    ExternalMD5 : Ansistring;
    ExternalAge : TDateTime;
  end;

var
  RFAViewForm: TRFAViewForm;

implementation

{$R *.dfm}

uses
  GuiAbout, GuiMain, GuiRAWView, GuiSMView, GuiBrowse, GuiBrowsePack, GuiSkinDialog, Masks,
  Math, StringFunction, GuiBrowseExtract,
  CommonLib, AppLib, MD5Api;


var
  FLastNode : PVirtualNode;

function IsFolder(Name: String) : boolean;
begin
  result := SFRight(DirDelimiter,Name) <> EmptyStr;
end;


function FindPath(Path: String) : PVirtualNode;
var
  Node: PVirtualNode;
  Data : pFse;
begin
  Result := nil;
  Node := RFAViewForm.RFAList.GetFirst;
  Path := ExcludeTrailingPathDelimiter(ExtractFilePath(Path));

  while Node <> nil do
  begin
    Data := RFAViewForm.RFAList.GetNodeData(Node);

    if (Data.FileType = ftFolder) and (Data.W32Path = Path) then
    begin
      Result := Node;
      Break;
    end;

    Node := RFAViewForm.RFAList.GetNext(Node);
  end;
end;

(*
function CheckValidity(Node : PVirtualNode) : boolean;
var
  ParentData : pFse;
begin
  result := false;
  if (Node.Parent <> nil) and (Node.Parent <> ViewForm.RFAList.RootNode) then
  begin
    ParentData := ViewForm.RFAList.GetNodeData(Node.Parent);
    if ParentData.FileType = ftFile then
      raise Exception.Create('A file/folder parent must be a folder not a file');
  end;
  result := true;
end;
*)

function BuildTreeFromFullPath(Path: AnsiString) : PVirtualNode;
var
  PvNode, Node: PVirtualNode;
  Data : pFse;
  Middle, Left : AnsiString;
  i : integer;
begin
  Node := nil;
  Path := ExtractFilePath(Path);

  for i := 0 to SFCountSubstr(DirDelimiter, Path)-1 do
  begin
    Middle := SFLeftNRight(DirDelimiter, Path, i);
    Middle := trim(Middle);

    if Length(Middle) > 0 then
    begin
      Left := IncludeTrailingPathDelimiter(SFNLeft(DirDelimiter, Path, i+1));

      PvNode := Node;
      Node := FindPath(Left);

      if Node = nil then
      begin
        if PvNode <> nil then
          Node := PvNode
        else
          if i>0 then
            Node := RFAViewForm.RFAList.GetFirstLevel(i-1)
          else
            Node := RFAViewForm.RFAList.RootNode;

        Node := RFAViewForm.RFAList.AddChild(Node);
        Data := RFAViewForm.RFAList.GetNodeData(Node);

        Data.W32Path := SFNLeft(DirDelimiter, Path, i+1);
        Data.W32Name := Middle;
        Data.FileType := ftFolder;

        //CheckValidity(Node);
      end;
    end;
  end;
  Result := Node;
end;


function BuildEntryNameFromTree(Node : PVirtualNode; SelectionOnly : boolean = false) : string;
var
  Data : pFse;
begin
  Data := RFAViewForm.RFAList.GetNodeData(Node);
  Result := Data.W32Name;

  while True do
  begin
    Node := RFAViewForm.RFAList.NodeParent[Node];

    if (Node = nil) or (SelectionOnly and not RFAViewForm.RFAList.Selected[Node]) then
      Break;

    Result := '/' + Result;
    Data := RFAViewForm.RFAList.GetNodeData(Node);
    Result := Data.W32Name + Result;
  end;
end;


procedure TRFAViewForm.ReadEntry(Sender : TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed : boolean; cSize : integer);
var
  Node: PVirtualNode;
  Data : pFse;
  W32Path : AnsiString;
  //Updating : boolean;

  (*
  function FindFile(BF42FullName : AnsiString) : PVirtualNode;
  var
    Data : pFse;
  begin
    Result := RFAViewForm.RFAList.GetFirst;

    while Result <> nil do
    begin
      Data := RFAViewForm.RFAList.GetNodeData(Result);

      if AnsiCompareText(Data.BF42FullName, BF42FullName) = 0 then
        Exit
      else
        Result := RFAViewForm.RFAList.GetNext(Result, true);
    end;

  end;
  *)

begin
  TotalProgress(roLoad, PG_AUTO, Sender.Count);

  W32Path := StringReplace(Name,'/','\',[rfReplaceAll]);
  BuildTreeFromFullPath(W32Path);

  Node := FindPath(W32Path);
  if Node = nil then
    Node := RFAList.RootNode;

  Node := RFAList.AddChild(Node);

(*
  Node := FindFile(Name);

  if Node = nil then
  begin
    Updating := false;
    Node := FindPath(W32Path);

    if Node = nil then
      Node := RFAList.RootNode;

    Node := RFAList.AddChild(Node);
  end
    else
      Updating := true;
*)
  Data := RFAList.GetNodeData(Node);
  Data.RFAFile := Sender;
(*
  if Updating then
  begin
    if  Data.Size <> ucSize then
      ShowMessageFmt('???ucSize (%d, %d)',[Data.Size, ucSize]);
    if  Data.Compressed <> Compressed then
      Showmessage('???Compressed');
    if  Data.CompSize <> cSize then
      Showmessage('???cSize');
    if  Data.W32Path <> W32Path then
      Showmessage(Data.W32Path + #10+#13 + W32Path);
  end;
*)

  Data.EntryName := Name;
  //Data.EntryName := EmptyStr;
  Data.Offset := Offset;
  Data.Size := ucSize;
  Data.Compressed := Compressed;
  Data.CompSize := cSize;

  Data.W32Path := W32Path;
  Data.W32Name := ExtractFileName(W32Path);
  Data.W32Ext := ExtractFileExt(LowerCase(W32Path));

  if Data.W32Ext = '.con' then
    Data.FileType := ftFileCON
  else
  if Data.W32Ext = '.inc' then
    Data.FileType := ftFileINC
  else
  if Data.W32Ext = '.ssc' then
    Data.FileType := ftFileSSC
  else
  if Data.W32Ext = '.txt' then
    Data.FileType := ftFileTXT
  else
  if Data.W32Ext = '.bak' then
    Data.FileType := ftFileBAK
  else
  if Data.W32Ext = '.wav' then
    Data.FileType := ftFileWAV
  else
  if Data.W32Ext = '.bik' then
    Data.FileType := ftFileBIK
  else
  if Data.W32Ext = '.dds' then
    Data.FileType := ftFileDDS
  else
  if Data.W32Ext = '.tga' then
    Data.FileType := ftFileTGA
  else
  if Data.W32Ext = '.raw' then
    Data.FileType := ftFileRAW
  else
  if Data.W32Ext = '.dat' then
    Data.FileType := ftFileDAT
  else
  if Data.W32Ext = '.rs' then
    Data.FileType := ftFileRS
  else
  if Data.W32Ext = '.rcm' then
    Data.FileType := ftFileRCM
  else
  if Data.W32Ext = '.lsb' then
    Data.FileType := ftFileLSB
  else
  if Data.W32Ext = '.pal' then
    Data.FileType := ftFilePAL
  else
  if Data.W32Ext = '.zip' then
    Data.FileType := ftFileZIP
  else
  if Data.W32Ext = '.rar' then
    Data.FileType := ftFileRAR
  else
  if Data.W32Ext = '.gz' then
    Data.FileType := ftFileGZ
  else
  if Data.W32Ext = '.bz2' then
    Data.FileType := ftFileBZ2
  else
  if Data.W32Ext = '.7z' then
    Data.FileType := ftFile7Z
  else
  if Data.W32Ext = '.dif' then
    Data.FileType := ftFileDIF
  else
  if Data.W32Ext = '.font' then
    Data.FileType := ftFileFONT
  else
  if Data.W32Ext = '.baf' then
    Data.FileType := ftFileBAF
  else
  if Data.W32Ext = '.skn' then
    Data.FileType := ftFileSKN
  else
  if Data.W32Ext = '.ske' then
    Data.FileType := ftFileSKE
  else
  if Data.W32Ext = '.vso' then
    Data.FileType := ftFileVSO
  else
  if Data.W32Ext = '.sm' then
    Data.FileType := ftFileSM
  else
  if Data.W32Ext = '.tm' then
    Data.FileType := ftFileTM
  else
    Data.FileType := ftFile;

  Data.ExternalFilePath := EmptyStr;

 // if CheckValidity(Node) then
  FLastNode := Node;
end;


procedure TRFAViewForm.ExpandSelection(Value: boolean);
var
  Node : PVirtualNode;
begin
  RFAList.BeginUpdate;
  Node := RFAList.GetFirstSelected(true);
  while Node <> nil do
  begin
    ExtendSelection(Node);
    RFAList.Expanded[Node] := Value;
    Node := RFAList.GetNextSelected(Node, true);
  end;
  RFAList.EndUpdate;
end;


procedure TRFAViewForm.CollapseAllExecute(Sender: TObject);
begin
  RFAList.SelectAll(false);
  CollapseSelected.Execute;
end;

procedure TRFAViewForm.CollapseSelectedExecute(Sender: TObject);
begin
  ExpandSelection(false);
end;

procedure TRFAViewForm.ExpandAllExecute(Sender: TObject);
begin
  RFAList.SelectAll(false);
  ExpandSelected.Execute;
end;

procedure TRFAViewForm.ExpandSelectedExecute(Sender: TObject);
begin
  ExpandSelection(true);
end;

procedure TRFAViewForm.ExportFile(Node: PVirtualNode; OutputStream : TStream);
var
  Data : pFse;
begin
  Data := RFAList.GetNodeData(Node);

  if Data.Compressed then
  begin
    Data.RFAFile.DecompressToStream(OutputStream, Data.Offset, Data.CompSize);
  end
    else
  begin
    Data.RFAFile.ExtractToStream(OutputStream, Data.Offset, Data.CompSize, Data.Size);
  end
end;


procedure TRFAViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormStorage.SaveFormPlacement;

  if DirectoryExists(GetAppTempDirectory) then
    DeleteDirectory(GetAppTempDirectory, false);
end;

procedure TRFAViewForm.FormCreate(Sender: TObject);
begin
  FApplicationTitle := Caption + ' - ' + ApplicationSvnTitle;
  Reset;

end;

procedure TRFAViewForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FArchive) then
    FArchive.Free;
end;

procedure TRFAViewForm.FormActivate(Sender: TObject);
begin
  ActiveControl := RFAList;
  ApplicationRun.Execute;
end;

function TRFAViewForm.GetTitle: string;
begin
  Result := Caption
end;

procedure TRFAViewForm.SetTitle(const Value: string);
begin
  if Value <> EmptyStr then
    Caption := Format('%s - %s',[ExtractFilename(Value), FApplicationTitle])
  else
    Caption := FApplicationTitle;
end;


procedure TRFAViewForm.AppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
begin
  if CmdLine.Count > 0 then
    if FileExists(CmdLine[0]) then
    begin
      OpenDialog.FileName := CmdLine[0];
      QuickOpen;
    end;
end;

procedure TRFAViewForm.ApplicationRunExecute(Sender: TObject);
begin
  ApplicationRun.Enabled := false;
  FormStorage.RestoreFormPlacement;

  UpdateManagerForm.OnUpdateReply := UpdateReply;
  UpdateManagerForm.Check.Execute;

  RecentList.Open;
  RebuildRecentList.Execute;
  Application.ProcessMessages;

  if ParamCount > 0 then
  begin
    OpenDialog.FileName := ParamStr(1);
    QuickOpen;
  end
    else
  if RecentMenu.Enabled then
    RecentMenu.Items[0].Click
  else
    OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);

end;


procedure TRFAViewForm.Reset;
begin
  SyncStop;
  RFAList.Clear;
  Title := EmptyStr;

  if Assigned(FArchive) then
    FArchive.Free;

  FArchive := nil;
  Save.Enabled := false;
  Defrag.Enabled := false;
  SaveDialog.FileName := EmptyStr;
  UpdateInfobar;
end;

function TRFAViewForm.LoadMap(Path : string) :boolean;
var
  Node: PVirtualNode;
begin
  Result := false;
  SyncStop;
  FSyncNode := nil;

  RFAList.BeginUpdate;
  Reset;

  if Assigned(FArchive) then
    FArchive.Free;

  FArchive := TRFAFile.Create;
  FArchive.OnReadEntry := ReadEntry;
  FArchive.OnProgress := SubProgress;

  TotalProgress(roBegin, 0, 0);

  try
    if FArchive.Open(Path) < 0 then
    begin
      ShowError('Archive opening error', 'This file is already used by another application');
      FreeAndNil(FArchive);
    end
      else
    begin
      Result := true;
    end;

    Node := RFAList.GetFirst;
    while Node <> nil do
    begin
      if RFAList.GetNodeLevel(Node) <= 2 then
        RFAList.Expanded[Node] := true;

      Node := RFAList.GetNext(Node);
    end;
    Sort;
  finally
    TotalProgress(roEnd, 0, 0);
    RFAList.EndUpdate;
  end;

  if Result then
  begin
    UpdateInfobar;
    SyncStart;
  end;
end;

procedure TRFAViewForm.UpdateInfobar;
begin
  if Assigned(FArchive) then
  begin
    ArchiveSize.Visible := true;
    ArchiveSize.Caption := Format('Archive = %s',[SizeToStr(FArchive.DataSize)]);

    Fragmentation.Visible := true;
    Fragmentation.Caption := Format('Fragmentation = %s',[SizeToStr(FArchive.Fragmentation)]);

    ArchiveFileCount.Visible := true;
    ArchiveFileCount.Caption := Format('%d file(s)',[FArchive.Count]);
  end
    else
  begin
    ArchiveSize.Visible := false;
    Fragmentation.Visible := false;
    ArchiveFileCount.Visible := false;
  end;
end;

procedure TRFAViewForm.CancelExecute(Sender: TObject);
begin
  Cancel.Enabled := false;
end;

procedure TRFAViewForm.CheckStatus(Node: PVirtualNode);
var
  Data : pFse;
  PreviousStatus : TEntryStatus;
  FileDateTime: TDateTime;
  ConflictFound : boolean;
  Child : PVirtualNode;
  ChildData : pFse;
begin
  if Node <> nil then
  begin
    Data := RFAList.GetNodeData(Node);
    PreviousStatus := Data.Status;
    if IsFile(Data.FileType) then
    begin
      if (Data.ExternalFilePath <> EmptyStr) and not (fsNew in Data.Status) then
      begin
        // Note : file age can change without data change
        if FileAge(Data.ExternalFilePath, FileDateTime) then
        begin
          if (FileDateTime <> Data.ExternalAge) then
          begin
            if MD5FromFile(Data.ExternalFilePath) <> Data.ExternalMD5 then
              Include(Data.Status, fsExternal)
            else
              Exclude(Data.Status, fsExternal)
          end;
        end
          else
            Exclude(Data.Status, fsExternal)
      end;

      if (fsEntry in Data.Status) and (Node.Parent <> nil) then
      begin
        ConflictFound := false;
        Child := Node.Parent.FirstChild;
        while Child <> nil do
        begin
          if Child <> Node then
          begin
            ChildData := RFAList.GetNodeData(Child);
            if ChildData.W32Name = Data.W32Name then
            begin
              Include(Data.Status, fsConflict);
              ConflictFound := true;
              Break;
            end;
          end;
          Child := RFAList.GetNextSibling(Child);
        end;
        if not ConflictFound then
          Exclude(Data.Status, fsConflict);
      end;
    end;

    if PreviousStatus <> Data.Status then
      RFAList.InvalidateNode(Node);
  end;
end;


procedure TRFAViewForm.PropagateStatus(Node: PVirtualNode; Status: TEntryModification);
var
  Data : pFse;
begin
  Node := RFAList.GetFirstChild(Node);
  while Node <> nil do
  begin
    PropagateStatus(Node, Status);
    Data := RFAList.GetNodeData(Node);
    Include(Data.Status, fsEntry);
    Node := RFAList.GetNextSibling(Node);
  end;
end;

procedure TRFAViewForm.NewExecute(Sender: TObject);
begin
  Reset;
  //Save.Enabled := false;
end;



procedure TRFAViewForm.NewFolderExecute(Sender: TObject);
var
  Node : pVirtualNode;
  Data : pFse;
begin
  Node := RFAList.GetFirstSelected;
  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);
    if not IsFile(Data.FileType) then
      Break;
    Node := RFAList.NodeParent[Node];
  end;

  if (Node = nil) or not IsFile(Data.FileType) then
  begin
    Node := RFAList.AddChild(Node);
    Data := RFAList.GetNodeData(Node);
    //Data.W32Path := Filename;
    //Data.W32Name := ExtractFileName(Filename);
    Data.W32Name := 'New folder';
    Data.FileType := ftFolder;
    Sort;
    RFAList.EditNode(Node,0);
  end;
end;

procedure MakePathVisible(Table : TBaseVirtualTree; Node: PVirtualNode);
begin
  repeat
    if Node = Table.RootNode then
      Break;

    Table.IsVisible[Node] := true;
    Table.Expanded[Node] := true;
    Node := Node.Parent;
  until False;
end;


procedure TRFAViewForm.SetSearchText(const Value: string);
var
  Node : pVirtualNode;
  Data : pFse;
  ShowAll : boolean;
  SearchList : TStringList;

  function LocalMatchesMask(AString : string) : boolean;
  var
    i :integer;
    Mask : string;
  begin
    for i := 0 to SearchList.Count - 1 do
    begin
      Mask := '*'+trim(SearchList[i])+'*';
      if MatchesMask(AString, Mask) then
      begin
        Result := true;
        Break;
      end;
      Result := false;
    end;
  end;

begin
  FSearchText := Value;
  SearchList := TStringList.Create;
  SearchList.Delimiter := ';';
  SearchList.DelimitedText := FSearchText;

  RFAList.BeginUpdate;

  Node := RFAList.GetFirst(true);
  while Node <> nil do
  begin
    RFAList.IsVisible[Node] := (FSearchText = EmptyStr);

    if RFAList.GetNodeLevel(Node) <= 2 then
      RFAList.Expanded[Node] := true
    else
      RFAList.Expanded[Node] := not (FSearchText = EmptyStr);

    Node := RFAList.GetNext(Node, true);
  end;

  if (FSearchText <> EmptyStr) then
  begin
    Node := RFAList.GetFirst(true);
    while Node <> nil do
    begin
      Data := RFAList.GetNodeData(Node);

      if LocalMatchesMask(Data.W32Name) then
      begin
        MakePathVisible(RFAList, Node);
        RFAList.ScrollIntoView(Node,true);
      end;

      Node := RFAList.GetNext(Node, true);
    end;
  end;

  RFAList.EndUpdate;
  SearchList.Free;
end;



procedure TRFAViewForm.OpenRecentClick(Sender: TObject);
begin
  if (Sender is TSpTBXItem) then
  begin
    OpenDialog.FileName := (Sender as TSpTBXItem).Caption;
    QuickOpen;
  end;
end;



procedure TRFAViewForm.OpenExecute(Sender: TObject);
begin
  if OpenDialog.FileName = EmptyStr then
    OpenDialog.FileName := ExtractFilePath(Application.ExeName);

  OpenDialog.InitialDir := ExtractFilePath(OpenDialog.FileName);
  OpenDialog.FileName := ExtractFileName(OpenDialog.FileName);
  If OpenDialog.Execute then
  Begin
    QuickOpen;
  End;
end;

function TRFAViewForm.QuickOpen: boolean;
begin
  QuickOpen := false;
  if FileExists(OpenDialog.FileName) then
  begin
    // Loading file
    if LoadMap(OpenDialog.FileName) then
    begin
      RecentList.AddString(OpenDialog.FileName);
      RebuildRecentList.Execute;
      SaveDialog.FileName := OpenDialog.FileName;
      Save.Enabled := true;
      Defrag.Enabled := true;
      Title := OpenDialog.FileName;
      QuickOpen := true;
    end;
  end
    else
  begin
    ShowError('Open error', Format(_('File (%s) not found'), [OpenDialog.FileName]));
  end;
end;



procedure TRFAViewForm.QuitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TRFAViewForm.SaveAsExecute(Sender: TObject);
var
  UseDefrag : boolean;
begin
  UseDefrag := false;
  SaveDialog.InitialDir := ExtractFilePath(SaveDialog.FileName);
  SaveDialog.FileName := ExtractFileName(SaveDialog.FileName);

  if SaveDialog.Execute then
  Begin
    if OpenDialog.FileName = SaveDialog.FileName then
      UseDefrag := true;

    if QuickSave(UseDefrag) then
    begin
      Save.Enabled := true;
      Defrag.Enabled := true;
    end;
  End;
end;



function TRFAViewForm.QuickSave(Defrag: boolean): boolean;
begin
  Result := false;
  if SaveMap(SaveDialog.FileName) then
  begin
    OpenDialog.FileName := SaveDialog.FileName;
    RecentList.AddString(SaveDialog.FileName);
    RebuildRecentList.Execute;
    Result := true;
  end
    else
  begin
    SaveDialog.FileName := OpenDialog.FileName;
  end;
end;

procedure TRFAViewForm.SaveExecute(Sender: TObject);
begin
  QuickSave(false);
end;

procedure TRFAViewForm.DefragExecute(Sender: TObject);
begin
  QuickSave(true);
end;

procedure TRFAViewForm.ShiftData(ShiftData: TRFAResult; ShiftWay : TShiftWay; IgnoreNode : PVirtualNode = nil);
var
  Node : PVirtualNode;
  Data : pFse;
begin
  Node := RFAList.GetFirst;

  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);
    if (Node <> IgnoreNode) and IsFile(Data.FileType) and (Data.Offset > ShiftData.offset) then
    begin
      if (ShiftWay = shLeft) then
      begin
        SendDebugFmt('Shift %s from 0x%.8x to 0x%.8x',[Data.W32Name, Data.Offset, Data.Offset - ShiftData.size]);
        Data.Offset := Data.Offset - ShiftData.size;
      end;

      if (ShiftWay = shRight) then
      begin
        SendDebugFmt('Shift %s from 0x%.8x to 0x%.8x',[Data.W32Name, Data.Offset, Data.Offset + ShiftData.size]);
        Data.Offset := Data.Offset + ShiftData.size;
      end;

      FArchive.UpdateEntry(Data.EntryName, Data.Offset, Data.Size, Data.CompSize);
      RFAList.InvalidateNode(Node);
    end;
    Node := RFAList.GetNext(Node);
  end;
end;

function TRFAViewForm.LastOne(Offset: Int64): boolean;
var
  Node : PVirtualNode;
  Data : pFse;
  MaxOffset : int64;
begin
  MaxOffset := 0;
  Node := RFAList.GetFirst;

  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);
    if IsFile(Data.FileType) then
    begin
      MaxOffset := Max(MaxOffset, Data.Offset);
      if MaxOffset > Offset then
        break;
    end;
    Node := RFAList.GetNext(Node);
  end;

  Result := (MaxOffset = Offset);
end;

function TRFAViewForm.SaveMap(Path: string; Defrag : boolean = false): boolean;
var
  NextNode, Node : PVirtualNode;
  Data : pFse;
  DeleteResult, InsertResult : TRFAResult;
  ExternalFile : TFileStream;
  InternalFile : TMemoryStream;
  Size : int64;
  NewResult : integer;
  TmpArchive : TRFAFile;
  TmpFilename : string;

begin
  Result := false;
  SyncStop;
  RemoveEmptyFolders;
  SyncAll;


  if CountFilesByStatus(RFAList.RootNode, [fsConflict], false) > 0 then
  begin
    ShowError('Name conflict', 'Some files are in conflict, please solve this before');
  end
    else
  begin
    Cancel.Enabled := true;

    if Assigned(FArchive) and (FArchive.Filepath = Path) and not Defrag then
    begin
      TotalProgress(roBegin, 0, RFAList.TotalCount*3);

      /// Step-1 : First delete entries
      Node := RFAList.GetFirst;
      while Node <> nil do
      begin
        if not Cancel.Enabled then
          Break;

        NextNode := RFAList.GetNext(Node);
        Data := RFAList.GetNodeData(Node);

        if (fsDelete in Data.Status) and IsFile(Data.FileType) then
        begin
          DeleteResult := FArchive.DeleteEntry(Data.EntryName);
          //ShiftData(DeleteResult, shLeft);
          RFAList.DeleteNode(Node);
        end;

        TotalProgress(roSave, PG_AUTO, RFAList.TotalCount*3);
        Node := NextNode;
      end;


      /// Step-2 : Update edited files
      Node := RFAList.GetFirst;
      while Node <> nil do
      begin
        if not Cancel.Enabled then
          Break;

        NextNode := RFAList.GetNext(Node);
        Data := RFAList.GetNodeData(Node);

        if (fsExternal in Data.Status) and IsFile(Data.FileType) then
        begin

          // if the file is the last one then remove it first
          if LastOne(Data.Offset) then
          begin
            DeleteResult := FArchive.DeleteFile(Data.Offset, Data.CompSize);
            ShiftData(DeleteResult, shLeft, Node);
          end;

          ExternalFile := TFileStream.Create(Data.ExternalFilePath, fmOpenRead);
          Size := ExternalFile.Size;
          InsertResult := FArchive.InsertFile(ExternalFile, COMPRESSED_DATA);
          ShiftData(InsertResult, shRight, Node);
          ExternalFile.Free;

          FArchive.UpdateEntry(Data.EntryName, InsertResult.offset, Size, InsertResult.size);

          Exclude(Data.Status, fsExternal);
          Data.Size := Size;
          Data.Offset := InsertResult.offset;
          Data.Compressed := COMPRESSED_DATA;
          Data.CompSize := InsertResult.size;
          Data.ExternalMD5 := MD5FromFile(Data.ExternalFilePath);
          RFAList.InvalidateNode(Node);
        end;

        TotalProgress(roSave, PG_AUTO, RFAList.TotalCount*3);
        Node := NextNode;
      end;

      /// Step-3 : Add new files
      Node := RFAList.GetFirst;
      while Node <> nil do
      begin
        if not Cancel.Enabled then
          Break;

        NextNode := RFAList.GetNext(Node);
        Data := RFAList.GetNodeData(Node);

        if (fsNew in Data.Status) and IsFile(Data.FileType) then
        begin
          ExternalFile := TFileStream.Create(Data.ExternalFilePath, fmOpenRead);
          Size := ExternalFile.Size;
          InsertResult := FArchive.InsertFile(ExternalFile, COMPRESSED_DATA);
          ShiftData(InsertResult, shRight, Node);
          ExternalFile.Free;

          Data.EntryName := BuildEntryNameFromTree(Node);
          FArchive.InsertEntry(Data.EntryName, InsertResult.offset, Size, InsertResult.size, 0);

          Exclude(Data.Status, fsNew);
          Exclude(Data.Status, fsEntry);
          Data.Size := Size;
          Data.Offset := InsertResult.offset;
          Data.Compressed := COMPRESSED_DATA;
          Data.CompSize := InsertResult.size;
          Data.ExternalFilePath := EmptyStr;
          Data.ExternalMD5 := EmptyStr;
          RFAList.InvalidateNode(Node);
        end;

        TotalProgress(roSave, PG_AUTO, RFAList.TotalCount*3);
        Node := NextNode;
      end;

      /// Step-4 : Update modified entries
      Node := RFAList.GetFirst;
      while Node <> nil do
      begin
        if not Cancel.Enabled then
          Break;

        NextNode := RFAList.GetNext(Node);
        Data := RFAList.GetNodeData(Node);

        if (fsEntry in Data.Status) and IsFile(Data.FileType) then
        begin
          FArchive.DeleteEntry(Data.EntryName);
          Data.EntryName := BuildEntryNameFromTree(Node);
          FArchive.InsertEntry(Data.EntryName, Data.offset, Data.Size, Data.CompSize, 0);
        end;

        TotalProgress(roSave, PG_AUTO, RFAList.TotalCount*3);
        Node := NextNode;
      end;

      TotalProgress(roEnd, PG_NULL, PG_NULL);
      UpdateInfobar;
      Result := true;
    end
      else
    begin
      TmpArchive := TRFAFile.Create;
      TmpArchive.OnProgress := SubProgress;

      if Assigned(FArchive) and Defrag then
      begin
        repeat
          TmpFilename := ExtractFilePath(Path) + RandomString('333333') + '.tmp';
        until not FileExists(TmpFilename);
        NewResult := TmpArchive.New(TmpFilename);
      end
      else
        NewResult := TmpArchive.New(Path);

      if NewResult < 0 then
      begin
         ShowError('Archive saving error', 'This file is already used by another application');
         FreeAndNil(TmpArchive);
      end
        else
      begin
        TotalProgress(roBegin, 0, RFAList.TotalCount*4);

        /// Step-0 : Immediatly update all needed entries
        Node := RFAList.GetFirst;
        while Node <> nil do
        begin
          if not Cancel.Enabled then
            Break;

          NextNode := RFAList.GetNext(Node);
          Data := RFAList.GetNodeData(Node);

          if (fsEntry in Data.Status) and IsFile(Data.FileType) then
          begin
            Data.EntryName := BuildEntryNameFromTree(Node);
            Exclude(Data.Status, fsEntry);
          end;

          TotalProgress(roSave, PG_AUTO, PG_SAME);
          Node := NextNode;
        end;

        /// Step-1 : Add same files without lost data
        Node := RFAList.GetFirst;
        while Node <> nil do
        begin
          if not Cancel.Enabled then
            Break;

          NextNode := RFAList.GetNext(Node);
          Data := RFAList.GetNodeData(Node);

          if not (Data.Status = []) and IsFile(Data.FileType) then
          begin
            //SendDebugFmt('File %s exported',[Data.W32Name]);
            InternalFile := TMemoryStream.Create;
            ExportFile(Node, InternalFile);
            Size := InternalFile.Size;
            InsertResult := TmpArchive.InsertFile(InternalFile, Data.Compressed);
            InternalFile.Free;
            TmpArchive.InsertEntry(Data.EntryName, InsertResult.offset, Size, InsertResult.size, 0);
          end;

          TotalProgress(roSave, PG_AUTO, PG_SAME);
          Node := NextNode;
        end;

        /// Step-2 : Add edited files
        Node := RFAList.GetFirst;
        while Node <> nil do
        begin
          if not Cancel.Enabled then
            Break;

          NextNode := RFAList.GetNext(Node);
          Data := RFAList.GetNodeData(Node);

          if (fsExternal in Data.Status) and IsFile(Data.FileType) then
          begin
            ExternalFile := TFileStream.Create(Data.ExternalFilePath, fmOpenRead);
            Size := ExternalFile.Size;
            InsertResult := TmpArchive.InsertFile(ExternalFile, COMPRESSED_DATA);
            ExternalFile.Free;
            TmpArchive.InsertEntry(Data.EntryName, InsertResult.offset, Size, InsertResult.size, 0);
          end;

          TotalProgress(roSave, PG_AUTO, PG_SAME);
          Node := NextNode;
        end;

        /// Step-3 : Add new files
        Node := RFAList.GetFirst;
        while Node <> nil do
        begin
          if not Cancel.Enabled then
            Break;

          NextNode := RFAList.GetNext(Node);
          Data := RFAList.GetNodeData(Node);

          if (fsNew in Data.Status) and IsFile(Data.FileType) then
          begin
            ExternalFile := TFileStream.Create(Data.ExternalFilePath, fmOpenRead);
            Size := ExternalFile.Size;
            InsertResult := TmpArchive.InsertFile(ExternalFile, COMPRESSED_DATA);
            ExternalFile.Free;
            TmpArchive.InsertEntry(Data.EntryName, InsertResult.offset, Size, InsertResult.size, 0);
          end;

          TotalProgress(roSave, PG_AUTO, PG_SAME);
          Node := NextNode;
        end;

        TotalProgress(roEnd, PG_NULL, PG_NULL);
        Result := true;

        TmpArchive.Free;

        if Assigned(FArchive) and Defrag then
        begin
          Reset;
          DeleteFile(Path);
          RenameFile(TmpFilename, Path);
        end;

        OpenDialog.FileName := Path;
        QuickOpen;
      end;
    end;
  end;

  SyncStart;
end;

procedure TRFAViewForm.RebuildRecentListExecute(Sender: TObject);
begin
  RecentMenu.Clear;
  RecentList.EnumItems;
  RecentMenu.Enabled := (RecentMenu.Count > 0);
end;

procedure TRFAViewForm.RecentExecute(Sender: TObject);
begin
//
end;

procedure TRFAViewForm.RecentListEnumText(Sender: TObject; Value: string; Index: Integer);
var
  MenuItem : TSpTBXItem;
begin
  MenuItem := TSpTBXItem.Create(RecentMenu);
  MenuItem.OnClick := OpenRecentClick;
  MenuItem.Caption := Value;
  RecentMenu.Add(MenuItem);
end;


procedure TRFAViewForm.SyncAll;
var
  Node : PVirtualNode;
begin
  Node := RFAList.GetFirst;
  while Node <> nil do
  begin
    CheckStatus(Node);
    Node := RFAList.GetNext(Node, true);
  end;
end;

procedure TRFAViewForm.SyncStart;
begin
  FSyncNode := nil;
  Sync.Enabled := true;
end;

procedure TRFAViewForm.SyncStop;
begin
  Sync.Enabled := false;
end;

procedure TRFAViewForm.SyncTimer(Sender: TObject);
var
  i, Count : integer;
begin
  Sync.Enabled := false;

  if Assigned(FArchive) then
  begin
    Count := Max(1, FArchive.Count div 100);

    for i := 0 to Count - 1 do
    begin
      if (FSyncNode = nil) then
        FSyncNode := RFAList.GetFirst
      else
        FSyncNode := RFAList.GetNext(FSyncNode, true);

      CheckStatus(FSyncNode);
    end;

    Sync.Enabled := true;
  end;
end;

function TRFAViewForm.FindFileByName(Filename: string): PVirtualNode;
var
  Data : pFse;
begin
  Result := RFAList.GetFirst;

  while Result <> nil do
  begin
    Data := RFAList.GetNodeData(Result);

    if AnsiCompareText(Data.W32Name, Filename) = 0 then
      Exit
    else
      Result := RFAList.GetNext(Result, true);
  end;
end;

procedure TRFAViewForm.PreviewRAWExecute(Sender: TObject);
var
  TerrainNode, HeightmapNode : PVirtualNode;
  TerrainFile, HeightmapFile : string;
begin

  TerrainNode := FindFileByName('Terrain.con');
  HeightmapNode := FindFileByName('Heightmap.raw');

  if (TerrainNode <> nil) and (HeightmapNode <> nil) then
  begin
    TerrainFile := ExtractTemporary(TerrainNode);
    HeightmapFile := ExtractTemporary(HeightmapNode);

    RAWViewForm.LoadTerrain(TerrainFile);
    RAWViewForm.LoadHeightmap(HeightmapFile);
    RAWViewForm.Show;
  end
    else
      ShowWarning('RAW Preview', 'Terrain data not found in this archive');

end;



procedure TRFAViewForm.PackDirectoryExecute(Sender: TObject);
var
  BasePath : string;
  Node : PVirtualNode;
begin
  if (BrowsePackForm.ShowModal = mrOk) then
    if DirectoryExists(BrowsePackForm.Directory) then
    begin
      Node := RFAList.RootNode;
      Reset;

      if BrowsePackForm.UseBasePath.Checked then
      begin
        BasePath := StringReplace(BrowsePackForm.Base.Text,'/','\',[rfReplaceAll]);
        BasePath := IncludeTrailingBackslash(BasePath);

        if not ValidFilename(BasePath, true) then
        begin
          ShowMessage('Base path error', Format('Base file path (%s) is invalid',[BasePath]));
          Exit;
        end;

        Node := BuildTreeFromFullPath(BasePath);
      end;

      Add(Node, nil, BrowsePackForm.Directory);
    end;
end;

procedure TRFAViewForm.ExtendSelection(Node : PVirtualNode);
begin
  if RFAList.IsVisible[Node] then
    RFAList.Selected[Node] := true;

  Node := Node.FirstChild;
  while Node <> nil do
  begin
    ExtendSelection(Node);
    Node := RFAList.GetNextSibling(Node);
  end;
end;

procedure TRFAViewForm.ExtractAllExecute(Sender: TObject);
begin
  RFAList.SelectAll(false);
  ExtractSelected.Execute;
end;


procedure TRFAViewForm.ExtractTo(Directory: string; List : TStringList = nil);
var
  Data : pFse;
  Node: PVirtualNode;
  ExternalFilePath : string;
  ExternFile : TFileStream;
  W32Path : string;
begin
  Cancel.Enabled := true;

  if Assigned(List) then
    List.Clear;

  TotalProgress(roBegin, PG_NULL, RFAList.SelectedCount);
  Node := RFAList.GetFirstSelected;
  while Node <> nil do
  begin
    if not Cancel.Enabled then
      Break;

    ExtendSelection(Node);
    Data := RFAList.GetNodeData(Node);

    if IsFile(Data.FileType) then
    begin
      if BrowseExtractForm.RecreateFullPath.Checked then
        W32Path := Data.W32Path
      else
      begin
        W32Path := BuildEntryNameFromTree(Node, true);
        W32Path := StringReplace(W32Path,'/','\',[rfReplaceAll]);
      end;

      ExternalFilePath := IncludeTrailingBackslash(Directory) + W32Path;
      ForceDirectories(ExtractFilePath(ExternalFilePath));

      if Assigned(List) then
      begin
        //SendDebug(ExternalFilePath);
        List.Add(ExternalFilePath);
      end;

      ExternFile := TFileStream.Create(ExternalFilePath, fmOpenWrite or fmCreate);
      ExportFile(Node, ExternFile);
      ExternFile.Free;
    end;

    TotalProgress(roExport, PG_AUTO, RFAList.SelectedCount);
    Node := RFAList.GetNextSelected(Node);
  end;
  TotalProgress(roEnd, PG_NULL, PG_NULL);
end;


procedure TRFAViewForm.ExtractSelectedExecute(Sender: TObject);
begin
  if (BrowseExtractForm.ShowModal = mrOk) then
  begin
    ExtractTo(BrowseExtractForm.Directory);
  end;
end;

function TRFAViewForm.ExtractTemporary(Node: PVirtualNode): string;
var
  Data : pFse;
  ExternFile : TFileStream;
begin
  Result := EmptyStr;
  if (Node = nil) or (Node = RFAList.RootNode) then
    Exit;

  Data := RFAList.GetNodeData(Node);

  if IsFile(Data.FileType) then
  begin
    if Data.ExternalFilePath = EmptyStr then
    begin
      if Data.Size > 0 then
      begin
        repeat
          Data.ExternalFilePath := GetMapTempDirectory + RandomString('333333')
        until not DirectoryExists(Data.ExternalFilePath);

        Data.ExternalFilePath := IncludeTrailingBackslash(Data.ExternalFilePath) + ExtractFileName(Data.W32Path);
        ForceDirectories(ExtractFilePath(Data.ExternalFilePath));

        ExternFile := TFileStream.Create(Data.ExternalFilePath, fmOpenWrite or fmCreate);
        try
          ExportFile(Node, ExternFile);
        finally
          ExternFile.Free;
        end;

        FileAge(Data.ExternalFilePath, Data.ExternalAge);
        Data.ExternalMD5 := MD5FromFile(Data.ExternalFilePath);
      end;
    end;

    Result := Data.ExternalFilePath;
  end;
end;

procedure TRFAViewForm.EditSelection;
var
  Node: PVirtualNode;
  Filepath : string;
begin
  Node := RFAList.GetFirstSelected;

  while Node <> nil do
  begin
    Filepath := ExtractTemporary(Node);

    if FileExists(Filepath) then
      ShellExecute(Handle,'open',PChar(Filepath),nil,nil,SW_SHOW);

    Node := RFAList.GetNextSelected(Node, true);
  end;
end;



procedure TRFAViewForm.PreviewExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data : pFse;
begin
  Node := RFAList.GetFirstSelected;

  if (Node = nil) or (Node = RFAList.RootNode) then
    Exit;

  Data := RFAList.GetNodeData(Node);

  case Data.FileType of
    ftFileSM:
    begin
      SMViewForm.LoadStandardMesh(ExtractTemporary(Node));
      SMViewForm.Show;
    end;
    ftFileCON, ftFileINC:
    begin

    end

    else
      begin
        ShowMessage('No preview', 'There is not built-in preview for the selected file');
      end;
  end;

end;


procedure TRFAViewForm.AboutExecute(Sender: TObject);
begin
  AboutForm.Showmodal;
end;

procedure TRFAViewForm.Add(MainNode: PVirtualNode; List: TStringList; Path: string);
var
  i :integer;
  Node: PVirtualNode;
  Data : pFse;
  Sender: TBaseVirtualTree;

  procedure AddFolder(Filename : string);
  var
    SubList: TStringList;
  begin
    if IsDirectory(Filename) then
    begin
      Node := Sender.AddChild(MainNode);
      Data := Sender.GetNodeData(Node);
      Data.W32Path := Filename;
      Data.W32Name := ExtractFileName(Filename);
      Data.FileType := ftFolder;

      SubList := TStringList.Create;
      BuildFileList(IncludeTrailingPathDelimiter(Data.W32Path)+'*', faAnyFile - faHidden, SubList);
      Add(Node, SubList, Data.W32Path);
      SubList.Free;
    end;
  end;

  procedure AddFile(Filename : string);
  begin
    if not IsDirectory(Filename) then
    begin
      Node := Sender.AddChild(MainNode);
      Data := Sender.GetNodeData(Node);
      Data.W32Path := Filename;
      Data.W32Name := ExtractFileName(Filename);
      Data.ExternalFilePath := Data.W32Path;
      Data.Size := FileGetSize(Data.W32Path);
      Data.FileType := ftFile;
      Include(Data.Status, fsNew);
      Include(Data.Status, fsEntry);
    end;
  end;

begin
  Sender := RFAList;

  if Assigned(List) then
  begin
    if Path <> EmptyStr then
      Path := IncludeTrailingPathDelimiter(Path);

    List.Sort;

    for i := 0 to List.Count - 1 do
      AddFolder(Path + List[i]);

    for i := 0 to List.Count - 1 do
      AddFile(Path + List[i]);
  end
    else
  begin
    AddFolder(Path);
    AddFile(Path);
  end;
end;


function TRFAViewForm.CountFilesByStatus(Node: PVirtualNode; Status:TEntryStatus; IncludeWithoutStatus : boolean = false) : cardinal;
var
  Data : pFse;
  NextNode: PVirtualNode;
  ModStatus : TEntryModification;
begin
  Result := 0;
  Assert(Node <> nil);

  NextNode := Node.NextSibling;
  Node := Node.FirstChild;
  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);

    if IsFile(Data.FileType) then
    begin
      if IncludeWithoutStatus and (Data.Status = []) then
        Inc(Result)
      else
        for ModStatus := Low(TEntryModification) to High(TEntryModification) do
        begin
          if (ModStatus in Status) and (ModStatus in Data.Status) then
            Inc(Result);
        end;
    end;

    Node := RFAList.GetNext(Node);

    if Node = NextNode then
      Break;
  end;
end;


procedure TRFAViewForm.RemoveEmptyFolders;
var
  Node, NextNode: PVirtualNode;
  Data : pFse;
begin
  Node := RFAList.GetFirst;

  RFAList.BeginUpdate;
  while Node <> nil do
  begin
    NextNode := RFAList.GetNext(Node, true);
    Data := RFAList.GetNodeData(Node);

    if (Data.FileType = ftFolder) and (CountFilesByStatus(Node, [fsNew, fsEntry, fsExternal], true) = 0) then
    begin
      NextNode := RFAList.GetNextSibling(Node);
      RFAList.FullyVisible[Node] := false;
      Include(Data.Status, fsDelete);
    end;

    Node := NextNode;
  end;

  RFAList.EndUpdate;
end;



procedure TRFAViewForm.DeleteSelection;
var
  Node, ExtendNode, NextNode: PVirtualNode;
  Data : pFse;
begin
  Node := RFAList.GetFirstSelected;
  RFAList.BeginUpdate;
  while Node <> Nil do
  begin
    ExtendNode := Node;
    ExtendSelection(ExtendNode);

    NextNode := RFAList.GetNextSelected(Node, true);
    Data := RFAList.GetNodeData(Node);

    if IsFile(Data.FileType) and (Data.Size > 0) then
    begin
      if (Data.Status = []) then
      begin
        Include(Data.Status, fsDelete);
        RFAList.FullyVisible[Node] := false;
      end
      else if (fsNew in Data.Status) then
      begin
        RFAList.DeleteNode(Node);
      end
    end;

    Node := NextNode;
  end;

  RFAList.EndUpdate;

end;


procedure TRFAViewForm.DropFileSourceDrop(Sender: TObject; DragType: TDragType; var ContinueDrop: Boolean);
var
  List : TStringList;
  ExternalPath : string;
  i :integer;
begin
  DropFileSource.Files.Clear;
  //if DropFileSource.InShellDragLoop then
  if true then
  begin
    repeat
      ExternalPath := GetMapTempDirectory + RandomString('333333')
    until not DirectoryExists(ExternalPath);

    ExtractTo(ExternalPath);

    List := TStringList.Create;
    BuildFileList(IncludeTrailingPathDelimiter(ExternalPath)+'*', faAnyFile - faHidden, List);

    for i := 0 to List.Count - 1 do
      DropFileSource.Files.Add(IncludeTrailingPathDelimiter(ExternalPath) + List[i]);

    List.Free;
  end
    else
  begin
    ContinueDrop := false;
  end;
end;


procedure TRFAViewForm.DropFileSourceAfterDrop(Sender: TObject; DragResult: TDragResult; Optimized: Boolean);
begin
  DropFileSource.Files.Clear;
end;

procedure TRFAViewForm.DropFileSourceGetData(Sender: TObject; const FormatEtc: tagFORMATETC; out Medium: tagSTGMEDIUM; var Handled: Boolean);
begin
  if (FormatEtc.cfFormat = CF_HDROP) then
  begin

  end;
end;

function TRFAViewForm.IsFile(FileType: TFileType): boolean;
begin
  Result := not (FileType = ftFolder);
end;

procedure TRFAViewForm.CloseSearchBarExecute(Sender: TObject);
begin
  SearchBar.Hide;
  SearchText := EmptyStr;
end;

procedure TRFAViewForm.ShowSearchBarExecute(Sender: TObject);
begin
  SearchBar.Show;
  SearchText := Search.Text;
  ActiveControl := Search;
end;

procedure TRFAViewForm.SearchChange(Sender: TObject);
begin
  SearchText := Search.Text;
end;

procedure TRFAViewForm.SubProgress(Sender: TRFAFile; Operation: TRFAOperation; Value: Integer);
begin
  if Operation = roBegin then
  begin
    SubProgressBar.Position := 0;
    SubProgressBar.Max := Value;
  end
    else
  begin
    if Value = PG_AUTO then
      SubProgressBar.Position := SubProgressBar.Position+1
    else
      SubProgressBar.Position := Value;
  end;

  Application.ProcessMessages;
end;


procedure TRFAViewForm.TotalProgress(Operation: TRFAOperation; Value: Integer; Max:integer);
begin
  if TotalProgressLabel.Tag <> Ord(Operation) then
  begin
    case Operation of
      roLoad: TotalProgressLabel.Caption := 'Loading';
      roSave: TotalProgressLabel.Caption := 'Saving';
      roExport: TotalProgressLabel.Caption := 'Exporting';
        else
      TotalProgressLabel.Caption := EmptyStr;
    end;

    TotalProgressLabel.Tag := Ord(Operation);
  end;

  if Operation = roBegin then
  begin
    ProgressPanel.Show;
    TotalProgressBar.Position := 0;
    TotalProgressBar.Max := Max;
    //SubProgressBar.Position := 0;
  end
    else
  if Operation = roEnd then
  begin
    ProgressPanel.Hide;
  end
    else
  begin
    if Max <> PG_SAME then
      TotalProgressBar.Max := Max;

    if Value = PG_AUTO then
      TotalProgressBar.Position := TotalProgressBar.Position+1
    else
      TotalProgressBar.Position := Value;
  end;

  Application.ProcessMessages;
end;


procedure TRFAViewForm.RFAListDblClick(Sender: TObject);
begin
  EditSelection;
end;

function FilesFromIDataObject(ADataObject: IDataObject; AFiles: TStrings): Integer;
var
  I: Integer;
  Data: TStgMedium;
  SOF: TFormatEtc;
  Enum: IEnumFORMATETC;
begin
  Result:=0;
  if ADataObject = nil then
    exit;

  ZeroMemory(@Data, SizeOf(Data));
  ADataObject.EnumFormatEtc(DATADIR_GET, Enum);
  while (Enum.Next(1, SOF, @I) = S_OK) and (I > 0) do
    if SOF.cfFormat = CF_HDROP then
    begin
      if Succeeded(ADataObject.GetData(SOF, Data)) then
      try
        case SOF.tymed of
          TYMED_FILE: AFiles.Text:=Data.lpszFileName;
          TYMED_HGLOBAL: ReadFilesFromHGlobal(Data.hGlobal, AFiles);
          else exit;
        end;
        Result:=AFiles.Count;
        exit;
      finally
        ReleaseStgMedium(Data);
      end;
    end;
end;

procedure TRFAViewForm.RFAListDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  DropData: TStgMedium;
  SOF: TFormatEtc;
  FileList : TStringList;
  HitNode: PVirtualNode;
  AttachMode: TVTNodeAttachMode;

  SourceNode : PVirtualNode;
  TargetNode : PVirtualNode;
  Data : pFse;
begin
  Data := nil;

  if (Source = nil) and not (DropFileSource.DragInProgress) then
  begin
    FileList := TStringList.Create;
    DataObject.GetData(SOF, DropData);
    FilesFromIDataObject(DataObject, FileList);

    HitNode := Sender.GetNodeAt(Pt.X, Pt.Y);

    if (HitNode <> nil) and (HitNode <> Sender.RootNode) then
    begin
      Data := Sender.GetNodeData(HitNode);

      while IsFile(Data.FileType) do
      begin
        HitNode := HitNode.Parent;

        if (HitNode = nil) or (HitNode = Sender.RootNode) then
          Break
        else
          Data := Sender.GetNodeData(HitNode);
      end;
    end;

    Add(HitNode, FileList);
    FileList.Free;
  end
    else
  begin
    SourceNode := Sender.GetFirstSelected;
    TargetNode := Sender.DropTargetNode;

    case DragMode of
      dmManual: ;
      dmAutomatic: ;
    end;

    AttachMode := amNoWhere;
    case Mode of
      dmAbove:
        AttachMode := amInsertBefore;
      dmOnNode:
        AttachMode := amAddChildLast;
      dmBelow:
        AttachMode := amInsertAfter;
    end;

    if TargetNode = nil then
      TargetNode := Sender.RootNode
    else
      Data := Sender.GetNodeData(TargetNode);

    if (TargetNode = Sender.RootNode) or (Data.FileType = ftFolder) then
    begin
      while SourceNode <> nil do
      begin
        if not Sender.HasAsParent(TargetNode, SourceNode) then
          Sender.MoveTo(SourceNode,TargetNode, AttachMode,false);

        Sender.Selected[SourceNode] := false;
        SourceNode :=  Sender.GetFirstSelected;
      end;
    end

  end;

  Sort;
end;

procedure TRFAViewForm.RFAListDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
var
  TargetNode : PVirtualNode;
  Data : pFse;
begin
  Accept := false;
  TargetNode := Sender.DropTargetNode;

  if (TargetNode <> nil) and (TargetNode <> Sender.RootNode) then
  begin
  (*
    if Sender.HasAsParent(TargetNode, Sender.GetFirstSelected) then
      Exit;
   *)
    Data := Sender.GetNodeData(TargetNode);
    if IsFile(Data.FileType) then
      Exit;
  end;

  if Source = nil then
    Accept := true
  else
    if Source = Sender then
      Accept := true;

end;


procedure TRFAViewForm.RFAListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
var
  Data : pFse;
begin
  FEditResult := edCancel;
  Data := Sender.GetNodeData(Node);
  if (NewText <> Data.W32Name) then
  begin
    if ValidFilename(NewText, false) then
    begin
      Data.W32Name := NewText;
      FEditResult := edOk;

      if (Data.FileType = ftFolder) then
        PropagateStatus(Node, fsEntry)
      else
        Include(Data.Status, fsEntry);
    end
      else
    begin
      FEditResult := edInvalid;
      Beep;
    end;
  end;
end;

procedure TRFAViewForm.RFAListEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if FEditResult = edInvalid then

  else if FEditResult = edOk then
    Sort;
end;


procedure TRFAViewForm.RFAListNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data : pFse;
begin
  Data := RFAList.GetNodeData(Node);
  Include(Data.Status, fsEntry);
end;

procedure TRFAViewForm.RFAListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
var
  Data : pFse;
begin
  Data := RFAList.GetNodeData(Node);

  with TargetCanvas do
  begin
    Pen.Style := psClear;
    InflateRect(CellRect,1,1);

    Brush.Color := clNone;

    if fsExternal in Data.Status then
      Brush.Color := $0093DCFF;

    if fsNew in Data.Status then
      Brush.Color := $0080FF80;

    if fsConflict in Data.Status then
      Brush.Color := $008080FF;

    if Brush.Color <> clNone then
      Rectangle(CellRect);
  end;
end;

procedure TRFAViewForm.RFAListCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2 : pFse;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);

  if IsFile(Data1.FileType) = IsFile(Data2.FileType) then
    Result := CompareStr(UpperCase(Data1.W32Name), UpperCase(Data2.W32Name))
  else
  if IsFile(Data1.FileType) then
    Result := GreaterThanValue
  else
  if IsFile(Data2.FileType) then
    Result := LessThanValue

end;

procedure TRFAViewForm.RFAListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
var
  Data : pFse;
  FloatValue : extended;
begin
  Data := Sender.GetNodeData(Node);

  if IsFile(Data.FileType) then
  begin
    case Column of
      0: CellText := Data.W32Name;
      1: CellText := Format('%s',[SizeToStr(Data.Size)]);
      2: CellText := Format('%s',[SizeToStr(Data.CompSize)]);

      3:
      if Data.Compressed then
      begin
        FloatValue := Data.CompSize / Data.Size  * 100;
        CellText := Format('%.2f%%',[FloatValue])
      end
        else
          CellText := 'NONE';

      4: CellText := Format('0x%.8x',[Data.Offset]);
    end;
  end
    else
  begin
    if Column = 0 then
      CellText := Data.W32Name
    else
      CellText := EmptyStr;
  end;

end;



procedure TRFAViewForm.RFAListKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  if (ssCtrl in Shift) then
  begin
    if CharCode = VK_ADD then
    begin
      DoDefault := false;
      ExpandSelected.Execute;
    end;
    if CharCode = VK_SUBTRACT then
    begin
      DoDefault := false;
      CollapseSelected.Execute;
    end;
  end;

  if CharCode = VK_DELETE then
  begin
    DeleteSelection;
  end;
end;



procedure TRFAViewForm.RFAListMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
(*
  if not RFAList.Dragging and (DragDetectPlus(TWinControl(Sender))) then
  begin
    DropFileSource.Files.Clear;
    DropFileSource.Files.Add(FArchive.Filepath);
    DropFileSource.Execute;
  end;
*)
end;

procedure TRFAViewForm.RFAListStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  (Sender as TBaseVirtualTree).CancelEditNode;
end;

procedure TRFAViewForm.RFAListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data : pFse;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TRFAViewForm.RFAListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data : pFse;
begin
  Data := Sender.GetNodeData(Node);

  case Column of
    0: case data.FileType of
        ftFile, ftFileDAT, ftFilePAL, ftFileLSB : ImageIndex := 113;
        ftFileRAW : ImageIndex := 177;
        ftFileTXT  : ImageIndex := 111;
        ftFileBAK  : ImageIndex := 133;
        ftFileRCM  : ImageIndex := 110;
        ftFileWAV  : ImageIndex := 106;
        ftFileBIK  : ImageIndex := 583;
        ftFileRS   : ImageIndex := 119;
        ftFileSKE  : ImageIndex := 387;
        ftFileSKN  : ImageIndex := 1132;
        ftFileDIF  : ImageIndex := 107;
        ftFileFONT  : ImageIndex := 1098;
        ftFileBAF  : ImageIndex := 87;
        ftFileVSO  : ImageIndex := 118;
        ftFileCON, ftFileINC: ImageIndex := 114;
        ftFileSSC : ImageIndex := 136;
        ftFileDDS, ftFileTGA : ImageIndex := 108;
        ftFileZIP, ftFileRAR, ftFileGZ, ftFileBZ2, ftFile7Z : ImageIndex := 109;
        ftFileSM : ImageIndex := 174;
        ftFileTM : ImageIndex := 175;
        ftFolder : ImageIndex := 122;
       end;
    else
      ImageIndex := -1;
  end;
end;

procedure TRFAViewForm.RFAListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rFse);
end;

procedure TRFAViewForm.Sort;
begin
  RFAList.SortTree(0, sdAscending);
end;



procedure TRFAViewForm.UpdateReply(Sender: TObject; Result: TUpdateResult);
begin
  NewVersionAvailable.Enabled := true;
  if Result = rs_UpdateFound then
  begin
    if not NewVersionAvailable.Visible then
    begin
      NewVersionAvailable.Visible := true;
      NewVersionAvailable.Execute;
    end
    else
      NewVersionAvailable.Visible := true;
  end
    else
  begin
    NewVersionAvailable.Visible := false;
  end;
end;

procedure TRFAViewForm.NewVersionAvailableExecute(Sender: TObject);
begin
  UpdateManagerForm.ShowModal;
end;

end.
