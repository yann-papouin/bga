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
  Menus, RFALib, SpTBXEditors, JvBaseDlg, JvBrowseFolder, JvAppStorage,
  JvAppRegistryStorage;

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

  TFseStatus =
  (
    fs_No_Change,
    fs_New,
    fs_Modified,
    fs_Delete
  );

  TShiftWay =
  (
    shLeft,
    shRight
  );

  TRFAViewForm = class(TForm)
    RFAList: TVirtualStringTree;
    FormStorage: TJvFormStorage;
    ExplorerImg: TPngImageList;
    LoadBar: TSpTBXPanel;
    TotalProgress: TSpTBXProgressBar;
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
    QuickOpen: TAction;
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
    Browse: TJvBrowseForFolderDialog;
    AppStorage: TJvAppRegistryStorage;
    Defrag: TAction;
    Action1: TAction;
    SpTBXItem15: TSpTBXItem;
    New: TAction;
    SpTBXItem16: TSpTBXItem;
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
    procedure SaveExecute(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
    procedure RecentListEnumText(Sender: TObject; Value: string; Index: Integer);
    procedure RebuildRecentListExecute(Sender: TObject);
    procedure OpenRecentClick(Sender: TObject);
    procedure QuickOpenExecute(Sender: TObject);
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
  private
    { Déclarations privées }
    FApplicationTitle : string;
    FSearchText: string;
    FSyncNode : PVirtualNode;
    FArchive : TRFAFile;
    procedure RemoveEmptyFolders;
    function CountFilesByStatus(Node: PVirtualNode; Status:TFseStatus) : cardinal;
    //procedure DeleteSelectionByData;
    procedure DeleteSelection;
    procedure ExtendSelection(Node : PVirtualNode);
    procedure Sort;
    procedure ModifCheck;
    function FindFileByName(Filename :string) : PVirtualNode;

    function IsFile(FileType : TFileType) : boolean;
    function GetTitle: string;
    procedure SetTitle(const Value: string);

    procedure Add(MainNode : PVirtualNode; List: TStringList; Path : string = '');
    procedure RecursiveAdd;

    procedure ShiftData(ShiftData : TRFAResult; ShiftWay : TShiftWay; IgnoreNode : PVirtualNode = nil);
    procedure SetSearchText(const Value: string);

    function LastOne(Offset : Int64) : boolean;

    function ExtractTemporary(Node : PVirtualNode) : string;
    procedure EditSelection;
    function CreateNew(Filename : string = ''): boolean;
    procedure Reset;
  public
    { Déclarations publiques }
    function LoadMap(Path : string) :boolean;
    function SaveMap(Path : string) :boolean;
    procedure ExportFile(Node: PVirtualNode; OutputStream : TStream);
    property Title : string read GetTitle write SetTitle;
    property SearchText : string read FSearchText write SetSearchText;
  end;



  pFse = ^rFse;
  rFse = record
    RFAFile : TRFAFile;
    // Data fields
    BF42FullName : AnsiString;
    Offset : Int64;
    Size : Int64;
    Compressed : boolean;
    CompSize : integer;
    // Extended fields
    W32Path : String;
    W32Name : String;
    W32Ext : String;
    FileType : TFileType;
    Status : TFseStatus;
    Recursive : boolean;
    // External fields
    ExternalFilePath : string;
    ExternalMD5 : Ansistring;
    ExternalAge : TDateTime;
  end;


var
  RFAViewForm: TRFAViewForm;



implementation

uses

  GuiAbout, GuiMain, GuiRAWView, GuiSMView, Masks, Math, StringFunction, DragDropFile, CommonLib, AppLib, MD5Api;

{$R *.dfm}

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

procedure BuildTreeFromFullPath(Path: AnsiString);
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


function BuildFullPathFromTree(Node : PVirtualNode) : string;
var
  Data : pFse;
begin
  Data := RFAViewForm.RFAList.GetNodeData(Node);
  Result := Data.W32Name;

  while True do
  begin
    Node := Node.Parent;

    if (Node = nil) or (Node = RFAViewForm.RFAList.RootNode)  then
      Break;

    Result := '/' + Result;
    Data := RFAViewForm.RFAList.GetNodeData(Node);
    Result := Data.W32Name + Result;
  end;
end;

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


procedure ReadEntry(Sender : TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed : boolean; cSize : integer);
var
  Node: PVirtualNode;
  Data : pFse;
  W32Path : AnsiString;
  Updating : boolean;
begin
  W32Path := StringReplace(Name,'/','\',[rfReplaceAll]);
  BuildTreeFromFullPath(W32Path);

  Node := FindFile(Name);

  if Node = nil then
  begin
    Updating := false;
    Node := FindPath(W32Path);

    if Node = nil then
      Node := RFAViewForm.RFAList.RootNode;

    Node := RFAViewForm.RFAList.AddChild(Node);
  end
    else
      Updating := true;
      
  Data := RFAViewForm.RFAList.GetNodeData(Node);
  Data.RFAFile := Sender;

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

  Data.BF42FullName := Name;
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
  if DirectoryExists(GetAppTempDirectory) then
    DeleteDirectory(GetAppTempDirectory, false);
end;

procedure TRFAViewForm.FormCreate(Sender: TObject);
begin
  FApplicationTitle := Caption + ' - ' + ApplicationSvnTitle;

  (*
  RFALib.FseAddProc := RFAListDirAdd;
  RFALib.FseDetailsProc := RFADetails;
  *)
end;

procedure TRFAViewForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FArchive) then
    FArchive.Free;

end;

procedure TRFAViewForm.FormActivate(Sender: TObject);
begin
  ActiveControl := RFAList;
end;


procedure TRFAViewForm.AppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
begin
  if CmdLine.Count > 0 then
    if FileExists(CmdLine[0]) then
    begin
      OpenDialog.FileName := CmdLine[0];
      QuickOpen.Execute;
    end;
end;

procedure TRFAViewForm.ApplicationRunExecute(Sender: TObject);
begin
  RecentList.Open;
  RebuildRecentList.Execute;
  Application.ProcessMessages;

  if ParamCount > 0 then
  begin
    OpenDialog.FileName := ParamStr(1);
    QuickOpen.Execute;
  end
    else
  if RecentMenu.Enabled then
    RecentMenu.Items[0].Click
  else
    OpenDialog.InitialDir := ExtractFilePath(Application.ExeName);
end;


procedure TRFAViewForm.Reset;
begin
  Sync.Enabled := false;
  RFAList.Clear;
  Title := EmptyStr;

  if Assigned(FArchive) then
    FArchive.Free;

  FArchive := nil;
  Save.Enabled := false;
  SaveDialog.FileName := EmptyStr;
end;

function TRFAViewForm.LoadMap(Path : string) :boolean;
var
  Node: PVirtualNode;
begin
  Result := false;
  Sync.Enabled := false;
  FSyncNode := nil;

  RFAList.BeginUpdate;
  Reset;

  if Assigned(FArchive) then
    FArchive.Free;

  FArchive := TRFAFile.Create;
  FArchive.OnReadEntry := ReadEntry;

  if FArchive.Open(Path) < 0 then
  begin
    ShowMessage('This file is already used');
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

  RFAList.EndUpdate;
  Sync.Enabled := true;
end;

procedure TRFAViewForm.ModifCheck;
var
  Data : pFse;
  PreviousStatus : TFseStatus;
  FileDateTime: TDateTime;
begin
  if FSyncNode = nil then
    FSyncNode := RFAList.GetFirst
  else
    FSyncNode := RFAList.GetNext(FSyncNode, true);

  if FSyncNode <> nil then
  begin
    Data := RFAList.GetNodeData(FSyncNode);
    PreviousStatus := Data.Status;
    if IsFile(Data.FileType) and (Data.ExternalFilePath <> EmptyStr) and not (Data.Status = fs_New) then
    begin
      // Note : file age can change without data change
      if FileAge(Data.ExternalFilePath, FileDateTime) then
      begin
        if (FileDateTime <> Data.ExternalAge) then
        begin
          if MD5FromFile(Data.ExternalFilePath) <> Data.ExternalMD5 then
            Data.Status := fs_Modified
          else
            Data.Status := fs_No_Change;
        end;
      end
        else
          Data.Status := fs_No_Change;
    end;

    if PreviousStatus <> Data.Status then
      RFAList.InvalidateNode(FSyncNode);
  end;

end;


function TRFAViewForm.CreateNew(Filename : string = ''): boolean;
var
  Newfile : string;
  TmpRFA : TRFAFile;
begin
  Result := false;

  if Filename <> Emptystr then
    Newfile := Filename
  else
    Newfile := SaveDialog.FileName;

  SaveDialog.InitialDir := ExtractFilePath(Newfile);
  SaveDialog.FileName := ExtractFileName(Newfile);

  if (Filename <> Emptystr ) or SaveDialog.Execute then
  begin
    OpenDialog.FileName := SaveDialog.FileName;
    Save.Enabled := true;

    TmpRFA := TRFAFile.Create;
    TmpRFA.New(SaveDialog.FileName);
    TmpRFA.Free;

    QuickOpen.Execute;
    Result := true;
  end;
end;

procedure TRFAViewForm.NewExecute(Sender: TObject);
begin
  Reset;

  //CreateNew;
end;

function TRFAViewForm.GetTitle: string;
begin
  Result := Caption
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

procedure TRFAViewForm.SetTitle(const Value: string);
begin
  if Value <> EmptyStr then
    Caption := Format('%s - %s',[ExtractFilename(Value), FApplicationTitle])
  else
    Caption := FApplicationTitle;
end;


procedure TRFAViewForm.OpenRecentClick(Sender: TObject);
begin
  if (Sender is TSpTBXItem) then
  begin
    OpenDialog.FileName := (Sender as TSpTBXItem).Caption;
    QuickOpen.Execute;
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
    QuickOpen.Execute;
  End;
end;

procedure TRFAViewForm.QuickOpenExecute(Sender: TObject);
begin
  if FileExists(OpenDialog.FileName) then
  begin
    // Loading file

    if LoadMap(OpenDialog.FileName) then
    begin
      RecentList.AddString(OpenDialog.FileName);
      RebuildRecentList.Execute;
      SaveDialog.FileName := OpenDialog.FileName;
      Save.Enabled := true;
      Title := OpenDialog.FileName;
    end;
  end
    else
  begin
    ShowMessageFmt(_('File (%s) not found'), [OpenDialog.FileName]);
  end;
end;


procedure TRFAViewForm.QuitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TRFAViewForm.SaveAsExecute(Sender: TObject);
begin
  SaveDialog.InitialDir := ExtractFilePath(SaveDialog.FileName);
  SaveDialog.FileName := ExtractFileName(SaveDialog.FileName);

  if SaveDialog.Execute then
  Begin
    OpenDialog.FileName := SaveDialog.FileName;
    Save.Enabled := true;
    Save.Execute;
    //Title := OpenDialog.FileName;
  End;
end;


procedure TRFAViewForm.SaveExecute(Sender: TObject);
begin
  // Execute save operation here

  if SaveMap(SaveDialog.FileName) then
  begin

    //Save.Enabled := false;
    RecentList.AddString(SaveDialog.FileName);
    RebuildRecentList.Execute;
  end;

  // Reopen
  //QuickOpen.Execute;
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

      FArchive.UpdateEntry(Data.BF42FullName, Data.Offset, Data.Size, Data.CompSize);
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

function TRFAViewForm.SaveMap(Path: string): boolean;
var
  NextNode, Node : PVirtualNode;
  Data : pFse;
  DeleteResult, InsertResult : TRFAResult;
  ExternalFile : TFileStream;
  Size : int64;
begin
  Result := false;

  if Assigned(FArchive) and (FArchive.Filepath = Path) then
  begin
    /// Step-1 : First delete entries
    Node := RFAList.GetFirst;
    while Node <> nil do
    begin
      NextNode := RFAList.GetNext(Node);
      Data := RFAList.GetNodeData(Node);

      if (Data.Status = fs_Delete) and IsFile(Data.FileType) then
      begin
        DeleteResult := FArchive.DeleteEntry(Data.BF42FullName);
        //ShiftData(DeleteResult, shLeft);
        RFAList.DeleteNode(Node);
      end;

      Node := NextNode;
    end;

    /// Step-2 : Update edited files
    ModifCheck;
    Node := RFAList.GetFirst;
    while Node <> nil do
    begin
      NextNode := RFAList.GetNext(Node);
      Data := RFAList.GetNodeData(Node);

      if (Data.Status = fs_Modified) and IsFile(Data.FileType) then
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

        FArchive.UpdateEntry(Data.BF42FullName, InsertResult.offset, Size, InsertResult.size);

        Data.Size := Size;
        Data.Offset := InsertResult.offset;
        Data.Compressed := COMPRESSED_DATA;
        Data.CompSize := InsertResult.size;
        Data.ExternalMD5 := MD5FromFile(Data.ExternalFilePath);
        Data.Status := fs_No_Change;
        //Data.ExternalFilePath := EmptyStr;
        //Data.ExternalMD5 := EmptyStr;
        //DeleteFile(Data.ExternalFilePath);
        RFAList.InvalidateNode(Node);
      end;

      Node := NextNode;
    end;

    /// Step-3 : Add new files
    Node := RFAList.GetFirst;
    while Node <> nil do
    begin
      NextNode := RFAList.GetNext(Node);
      Data := RFAList.GetNodeData(Node);

      if (Data.Status = fs_New) and IsFile(Data.FileType) then
      begin
        ExternalFile := TFileStream.Create(Data.ExternalFilePath, fmOpenRead);
        Size := ExternalFile.Size;
        InsertResult := FArchive.InsertFile(ExternalFile, COMPRESSED_DATA);
        ShiftData(InsertResult, shRight, Node);
        ExternalFile.Free;

        Data.BF42FullName := BuildFullPathFromTree(Node); // a drag/drop can change this value
        FArchive.InsertEntry(Data.BF42FullName, InsertResult.offset, Size, InsertResult.size, 0);

        Data.Status := fs_No_Change;
        Data.Size := Size;
        Data.Offset := InsertResult.offset;
        Data.Compressed := COMPRESSED_DATA;
        Data.CompSize := InsertResult.size;
        Data.Status := fs_No_Change;
        Data.ExternalFilePath := EmptyStr;
        Data.ExternalMD5 := EmptyStr;
        RFAList.InvalidateNode(Node);
      end;

      Node := NextNode;
    end;

    Result := true;
  end
    else
  begin
    Showmessage('Not same file, not supported actually');

  end;

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


procedure TRFAViewForm.SyncTimer(Sender: TObject);
var
  i, Count : integer;
begin
  Sync.Enabled := false;

  if Assigned(FArchive) then
  begin
    Count := Max(1, FArchive.Count div 100);

    for i := 0 to Count - 1 do
      ModifCheck;

    Sync.Enabled := true;
  end;
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

    if Data.Status = fs_Modified then
      Brush.Color := $0093DCFF;

    if Data.Status = fs_New then
      Brush.Color := $0080FF80;

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
    Result := CompareStr(UpperCase(Data1.BF42FullName), UpperCase(Data2.BF42FullName))
  else
  if IsFile(Data1.FileType) then
    Result := GreaterThanValue
  else
  if IsFile(Data2.FileType) then
    Result := LessThanValue

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
      ShowMessage('Terrain data not found');

end;


procedure TRFAViewForm.PackDirectoryExecute(Sender: TObject);
var
  FileList : TStringList;
begin
  if Browse.Execute then
  begin
    Reset;
    Browse.Directory := IncludeTrailingPathDelimiter(Browse.Directory);
    FileList := TStringList.Create;
    BuildFileList(Browse.Directory+'*', faAnyFile - faHidden, FileList);
    Add(RFAList.RootNode, FileList, Browse.Directory);
    FileList.Free;
    RecursiveAdd;
  end;
end;

procedure TRFAViewForm.ExtendSelection(Node : PVirtualNode);
begin
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

procedure TRFAViewForm.ExtractSelectedExecute(Sender: TObject);
var
  Data : pFse;
  Node: PVirtualNode;
  ExternalFilePath : string;
  ExternFile : TFileStream;
begin
  if Browse.Execute then
  begin
    //Browse.Directory;

    Node := RFAList.GetFirstSelected;
    while Node <> nil do
    begin
      ExtendSelection(Node);
      Data := RFAList.GetNodeData(Node);

      if IsFile(Data.FileType) then
      begin
        ExternalFilePath := IncludeTrailingBackslash(Browse.Directory);
        ExternalFilePath := ExternalFilePath + Data.W32Path;
        ForceDirectories(ExtractFilePath(ExternalFilePath));

        ExternFile := TFileStream.Create(ExternalFilePath, fmOpenWrite or fmCreate);
        ExportFile(Node, ExternFile);
        ExternFile.Free;
      end;

      Node := RFAList.GetNextSelected(Node);
    end;
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
        ExportFile(Node, ExternFile);
        ExternFile.Free;

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
        ShowMessage('Cannot preview selected file');
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
begin
  Sender := RFAList;
  List.Sort;

  if Path <> EmptyStr then
    Path := IncludeTrailingPathDelimiter(Path);

  for i := 0 to List.Count - 1 do
    if IsDirectory(Path + List[i]) then
    begin
      Node := Sender.AddChild(MainNode);
      Data := Sender.GetNodeData(Node);
     // Data.Path := Path + List[i];
      Data.W32Path := Path + List[i];
      Data.W32Name := ExtractFileName(List[i]);
      Data.FileType := ftFolder;
      Data.Recursive := true;
      //Data.Status := fs_New;
    end;

  for i := 0 to List.Count - 1 do
    if not IsDirectory(Path + List[i]) then
    begin
      Node := Sender.AddChild(MainNode);
      Data := Sender.GetNodeData(Node);
      //Data.Path := Path + List[i];
      Data.ExternalFilePath := Path + List[i];
      Data.W32Path := Path + List[i];
      Data.W32Name := ExtractFileName(List[i]);
      Data.Size := FileGetSize(Data.W32Path);
      Data.FileType := ftFile;
      Data.Status := fs_New;
    end;

end;



procedure TRFAViewForm.RecursiveAdd;
var
  Sender: TBaseVirtualTree;
  Node: PVirtualNode;
  Data : pFse;
  FileList : TStringList;
begin
  Sender := RFAList;
  Node := Sender.GetFirst;

  while Node <> nil do
  begin
    Data := Sender.GetNodeData(Node);

    if Data.Recursive then
    begin
      FileList := TStringList.Create;
      BuildFileList(IncludeTrailingPathDelimiter(Data.W32Path)+'*', faAnyFile - faHidden, FileList);

      Add(Node, FileList, Data.W32Path);
      FileList.Free;

      Data.Recursive := false;
      Node := Sender.GetFirst;
    end
      else
    Node := Sender.GetNext(Node, true);
  end;

end;



function TRFAViewForm.CountFilesByStatus(Node: PVirtualNode; Status:TFseStatus) : cardinal;
var
  Data : pFse;
  NextNode: PVirtualNode;
begin
  Result := 0;

  NextNode := Node.NextSibling;
  Node := Node.FirstChild;
  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);

    if IsFile(Data.FileType) and (Data.Status = Status) then
      Inc(Result);

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

    if (Data.FileType = ftFolder) and (CountFilesByStatus(Node, fs_No_Change) = 0) and (CountFilesByStatus(Node, fs_New) = 0) then
    begin
      NextNode := RFAList.GetNextSibling(Node);
      RFAList.FullyVisible[Node] := false;
      Data.Status := fs_Delete;
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
      case Data.Status of
        fs_No_Change:
        begin
          Data.Status := fs_Delete;
          RFAList.FullyVisible[Node] := false;
        end;
        fs_New:
        begin
          RFAList.DeleteNode(Node);
        end;
      end;
    end;

    Node := NextNode;
  end;

  RFAList.EndUpdate;
  RemoveEmptyFolders;

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

procedure TRFAViewForm.RFAListDragDrop(Sender: TBaseVirtualTree; Source: TObject;
  DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
  Pt: TPoint; var Effect: Integer; Mode: TDropMode);
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
  if Source = nil then
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
    RecursiveAdd;
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

procedure TRFAViewForm.RFAListDragOver(Sender: TBaseVirtualTree; Source: TObject;
  Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
  var Effect: Integer; var Accept: Boolean);
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
      (*
      4: CellText := Data.W32Path;
      5: if Data.EntryIndex = 0 then
          CellText := EmptyStr
         else
          CellText := Format('%d',[Data.EntryIndex]);
      *)
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

  if CharCode = VK_DELETE then
  begin
    DeleteSelection;
  end;

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


end.
