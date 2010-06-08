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
  JvGnuGetText, ShellAPI,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GuiRFACommon, ActnList, VirtualTrees, TB2Item, SpTBXItem, TB2Dock,
  TB2Toolbar, ExtCtrls, JvFormPlacement, JvAppStorage, JvAppRegistryStorage, Menus,
  JvComponentBase, JvMRUList, JvAppInst, DragDrop, DropSource, DragDropFile, StdCtrls,
  SpTBXEditors, SpTBXControls, GuiUpdateManager, ActiveX, RFALib, JclFileUtils;

type

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

  TRFAViewForm = class(TRFACommonForm)
    Open: TAction;
    Save: TAction;
    SaveAs: TAction;
    Quit: TAction;
    Recent: TAction;
    PackDirectory: TAction;
    ExtractModFolder: TAction;
    About: TAction;
    ApplicationRun: TAction;
    Settings: TAction;
    PreviewRAW: TAction;
    Defrag: TAction;
    New: TAction;
    NewVersionAvailable: TAction;
    Cancel: TAction;
    NewFolder: TAction;
    TopDock: TSpTBXDock;
    tbMenuBar: TSpTBXToolbar;
    mFile: TSpTBXSubmenuItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    RecentMenu: TSpTBXSubmenuItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem15: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    SpTBXItem7: TSpTBXItem;
    mEdit: TSpTBXSubmenuItem;
    SpTBXItem1: TSpTBXItem;
    SpTBXItem12: TSpTBXItem;
    SpTBXSeparatorItem4: TSpTBXSeparatorItem;
    SpTBXItem16: TSpTBXItem;
    SpTBXItem18: TSpTBXItem;
    SpTBXSubmenuItem1: TSpTBXSubmenuItem;
    SpTBXItem13: TSpTBXItem;
    SpTBXSubmenuItem3: TSpTBXSubmenuItem;
    SpTBXItem11: TSpTBXItem;
    SpTBXItem10: TSpTBXItem;
    SpTBXSeparatorItem3: TSpTBXSeparatorItem;
    SpTBXItem9: TSpTBXItem;
    SpTBXSubmenuItem2: TSpTBXSubmenuItem;
    SpTBXItem8: TSpTBXItem;
    mHelp: TSpTBXSubmenuItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem17: TSpTBXItem;
    DropFileSource: TDropFileSource;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    AppInstances: TJvAppInstances;
    RecentList: TJvMruList;
    ViewerPopup: TSpTBXPopupMenu;
    SpTBXItem22: TSpTBXItem;
    SpTBXSubmenuItem4: TSpTBXSubmenuItem;
    SpTBXSeparatorItem5: TSpTBXSeparatorItem;
    SpTBXItem21: TSpTBXItem;
    SpTBXItem20: TSpTBXItem;
    SpTBXSeparatorItem6: TSpTBXSeparatorItem;
    SpTBXItem19: TSpTBXItem;
    AppStorage: TJvAppRegistryStorage;
    FormStorage: TJvFormStorage;
    Sync: TTimer;
    StatusBar: TSpTBXStatusBar;
    ArchiveSize: TSpTBXLabelItem;
    SpTBXSeparatorItem7: TSpTBXSeparatorItem;
    Fragmentation: TSpTBXLabelItem;
    SpTBXSeparatorItem8: TSpTBXSeparatorItem;
    ArchiveFileCount: TSpTBXLabelItem;
    ProgressPanel: TSpTBXPanel;
    SubProgressBar: TSpTBXProgressBar;
    TotalProgressBar: TSpTBXProgressBar;
    TotalProgressLabel: TSpTBXLabel;
    SpTBXButton2: TSpTBXButton;
    ExtractAll: TAction;
    ExtractSelected: TAction;
    SpTBXItem23: TSpTBXItem;
    Filesystem: TAction;
    SpTBXSubmenuItem5: TSpTBXSubmenuItem;
    SkinGroup: TSpTBXSkinGroupItem;
    Theme: TSpTBXEdit;
    SpTBXSeparatorItem9: TSpTBXSeparatorItem;
    SpTBXSeparatorItem10: TSpTBXSeparatorItem;
    SelectionText: TSpTBXLabelItem;
    UseCompression: TSpTBXItem;
    Revert: TAction;
    SpTBXItem24: TSpTBXItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure AppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
    procedure ApplicationRunExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure NewExecute(Sender: TObject);
    procedure NewFolderExecute(Sender: TObject);
    procedure OpenRecentClick(Sender: TObject);
    procedure OpenExecute(Sender: TObject);
    procedure QuitExecute(Sender: TObject);
    procedure SaveAsExecute(Sender: TObject);
    procedure SaveExecute(Sender: TObject);
    procedure DefragExecute(Sender: TObject);
    procedure AboutExecute(Sender: TObject);
    procedure RecentExecute(Sender: TObject);
    procedure RecentListEnumText(Sender: TObject; Value: string; Index: Integer);
    procedure SyncTimer(Sender: TObject);
    procedure RFAListDragDrop(Sender: TBaseVirtualTree; Source: TObject; DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure RFAListDblClick(Sender: TObject);
    procedure RFAListDragOver(Sender: TBaseVirtualTree; Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
    procedure DropFileSourceDrop(Sender: TObject; DragType: TDragType; var ContinueDrop: Boolean);
    procedure DropFileSourceAfterDrop(Sender: TObject; DragResult: TDragResult; Optimized: Boolean);
    procedure DropFileSourceGetData(Sender: TObject; const FormatEtc: tagFORMATETC; out Medium: tagSTGMEDIUM; var Handled: Boolean);
    procedure PreviewRAWExecute(Sender: TObject);
    procedure PackDirectoryExecute(Sender: TObject);
    procedure RFAListNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; NewText: string);
    procedure RFAListEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
    procedure RFAListNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure RFAListBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
    procedure RFAListStartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure NewVersionAvailableExecute(Sender: TObject);
    procedure ExtractAllExecute(Sender: TObject);
    procedure ExtractSelectedExecute(Sender: TObject);
    procedure FilesystemExecute(Sender: TObject);
    procedure SkinGroupSkinChange(Sender: TObject);
    procedure RFAListKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure RFAListStateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
    procedure RFAListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const Text: string;
      const CellRect: TRect; var DefaultDraw: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure RevertExecute(Sender: TObject);
  private
    FEditResult : TEditResult;
    FSyncNode : PVirtualNode;
    FArchive : TRFAFile;
    FResetMutex : boolean;
    procedure ReadEntry(Sender: TRFAFile; Name: AnsiString; Offset, ucSize: Int64; Compressed: boolean; cSize: integer);
    { Déclarations privées }
    procedure SubProgress(Sender : TRFAFile; Operation : TRFAOperation; Value : Integer = 0);
    procedure TotalProgress(Operation : TRFAOperation; Value : Integer; Max:integer);
    procedure Add(MainNode: PVirtualNode; List: TStringList; Path: string = '');
    procedure CheckStatus(Node: PVirtualNode);
    procedure DeleteSelection;
    procedure EditSelection;
    function LastOne(Offset: Int64): boolean;
    function LoadMap(Path: string): boolean;
    function QuickOpen: boolean;
    function QuickSave(Defragmentation: boolean): boolean;
    procedure RebuildRecentList;
    procedure RemoveEmptyFolders;
    function Reset(Ask : boolean = false) : TModalResult;
    function SaveMap(Path: string; Defrag: boolean = false): boolean;
    procedure ExtractTo(Directory: string; List: TStringList = nil);
    procedure ShiftData(ShiftData: TRFAResult; ShiftWay: TShiftWay; IgnoreNode: PVirtualNode = nil);
    procedure SyncAll;
    procedure SyncStart;
    procedure SyncStop;
    procedure UpdateInfobar;
    procedure UpdateReply(Sender: TObject; Result: TUpdateResult);
  protected
    procedure CancelChange;
    procedure NotifyChange;
  public
    { Déclarations publiques }
  end;

var
  RFAViewForm: TRFAViewForm;

implementation

{$R *.dfm}

uses
  DbugIntf, VirtualTreeviewTheme,
  GuiAbout, GuiRAWView, GuiSMView, GuiBrowsePack, GuiSkinDialog, GuiFSView, SpTBXSkins,
  Resources, Masks, Math, StringFunction, GuiBrowseExtract, CommonLib, AppLib, MD5Api;

var
  FLastNode : PVirtualNode;

const
  ASK_BEFORE_RESET = true;

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
  Data.RFAFileHandle := Sender;
  Data.RFAFileName := Data.RFAFileHandle.Filepath;
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
  Data.Offset := Offset;
  Data.Size := ucSize;
  Data.Compressed := Compressed;
  Data.CompSize := cSize;

  Data.W32Path := W32Path;
  Data.W32Name := ExtractFileName(W32Path);
  Data.W32Ext := ExtractFileExt(LowerCase(W32Path));
  Data.FileType := ExtensionToType(Data.W32Ext);
  Data.ExternalFilePath := EmptyStr;

  FLastNode := Node;
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
      if (Data.ExternalFilePath <> EmptyStr) and not (fsNew in Data.Status) or (fsExternal in Data.Status) then
      begin
        // Note : file age can change without data change
        if FileAge(Data.ExternalFilePath, FileDateTime) then
        begin
          if (FileDateTime <> Data.ExternalAge) then
          begin
            if MD5FromFile(Data.ExternalFilePath) <> Data.ExternalMD5 then
            begin
              Include(Data.Status, fsExternal);
              NotifyChange;
            end
            else
              Exclude(Data.Status, fsExternal)
          end;
        end
          else
        begin
          Exclude(Data.Status, fsExternal);
          // Maybe the file has been deleted manually
          if not FileExists(Data.ExternalFilePath) then
            Data.ExternalFilePath := EmptyStr;
        end;
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



procedure TRFAViewForm.Add(MainNode: PVirtualNode; List: TStringList; Path: string = '');
var
  i :integer;
  Node: PVirtualNode;
  Data : pFse;
  Sender: TBaseVirtualTree;

  function FindNode(Node: PVirtualNode; Filename : string) : PVirtualNode;
  var
    Data : pFse;
  begin
    Result := nil;
    Node := Node.FirstChild;
    while Node <> nil do
    begin
      Data := Sender.GetNodeData(Node);
      if UpperCase(Data.W32Name) = UpperCase(Filename) then
      begin
        Result := Node;
        Break;
      end;
      Node := Node.NextSibling;
    end;
  end;

  procedure AddFolder(Filename : string);
  var
    SubList: TStringList;
  begin
    if IsDirectory(Filename) then
    begin
      Node := FindNode(MainNode, ExtractFileName(Filename));
      if Node = nil then
      begin
        Node := Sender.AddChild(MainNode);
        Data := Sender.GetNodeData(Node);
        Data.W32Path := Filename;
        Data.W32Name := ExtractFileName(Filename);
        Data.FileType := ftFolder;
      end
        else
      begin
        Data := Sender.GetNodeData(Node);
      end;

      SubList := TStringList.Create;
      BuildFileList(IncludeTrailingPathDelimiter(Filename)+'*', faAnyFile - faHidden, SubList);
      Add(Node, SubList, Filename);
      SubList.Free;
    end;
  end;

  procedure AddFile(Filename : string);
  begin
    if not IsDirectory(Filename) then
    begin
      Node := FindNode(MainNode, ExtractFileName(Filename));
      if Node = nil then
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
      end
        else
      begin
        Data := Sender.GetNodeData(Node);
        Data.ExternalFilePath := Filename;
      end;
    end;
  end;

begin
  BeginOperation;
  Sender := RFAList;
  NotifyChange;

  if MainNode = nil then
    MainNode := Sender.RootNode;

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
  EndOperation;
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
  inherited;
  ApplicationRun.Enabled := false;
  FormStorage.RestoreFormPlacement;

  if Theme.Text <> EmptyStr then
    SkinManager.SetSkin(Theme.Text);

  UpdateManagerForm.OnUpdateReply := UpdateReply;
  UpdateManagerForm.Check.Execute;

  RecentList.Open;
  RebuildRecentList;
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


function TRFAViewForm.Reset(Ask : boolean = false) : TModalResult;
begin
  Result := mrNone;

  if not FResetMutex and Ask and (Save.Enabled or (CountFilesByStatus(RFAList.RootNode, [fsNew], false) > 0)) then
  begin
    FResetMutex := true;
    Result := ShowDialog('Confirmation', 'Save changes ?', mtInformation, mbYesNoCancel, mbCancel, 0);
    case Result of
      mrYes:
      begin
        SaveAs.Execute;
        FResetMutex := false;
      end;
      mrCancel:
      begin
        FResetMutex := false;
        Exit;
      end;
    end;
  end;

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

  Result := mrOk;
end;



function TRFAViewForm.LoadMap(Path : string) :boolean;
var
  Node: PVirtualNode;
begin
  Result := false;
  SyncStop;
  FSyncNode := nil;

  RFAList.BeginUpdate;

  if Reset(ASK_BEFORE_RESET) = mrCancel then
  begin
    RFAList.EndUpdate;
    SyncStart;
    Exit;
  end;

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

      Data.RFAFileHandle.UpdateEntry(Data.EntryName, Data.Offset, Data.Size, Data.CompSize);
      RFAList.InvalidateNode(Node);
    end;
    Node := RFAList.GetNext(Node);
  end;
end;


procedure TRFAViewForm.SkinGroupSkinChange(Sender: TObject);
var
  i :integer;
begin
  for i := 0 to RFAList.Header.Columns.Count - 1 do
    RFAList.Header.Invalidate(RFAList.Header.Columns[i]);
    //RFAList.Header.Columns[i].ParentColorChanged;

  RFAList.Invalidate;
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
          DeleteResult := Data.RFAFileHandle.DeleteEntry(Data.EntryName);
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
            DeleteResult := Data.RFAFileHandle.DeleteFile(Data.Offset, Data.CompSize);
            ShiftData(DeleteResult, shLeft, Node);
          end;

          ExternalFile := TFileStream.Create(Data.ExternalFilePath, fmOpenRead);
          Size := ExternalFile.Size;
          InsertResult := Data.RFAFileHandle.InsertFile(ExternalFile, UseCompression.Checked);
          ShiftData(InsertResult, shRight, Node);
          ExternalFile.Free;

          Data.RFAFileHandle.UpdateEntry(Data.EntryName, InsertResult.offset, Size, InsertResult.size);

          Exclude(Data.Status, fsExternal);
          Data.Size := Size;
          Data.Offset := InsertResult.offset;
          Data.Compressed := UseCompression.Checked;
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
          InsertResult := FArchive.InsertFile(ExternalFile, UseCompression.Checked);
          ShiftData(InsertResult, shRight, Node);
          ExternalFile.Free;

          Data.EntryName := BuildEntryNameFromTree(Node);
          Data.RFAFileHandle := FArchive;
          Data.RFAFileHandle.InsertEntry(Data.EntryName, InsertResult.offset, Size, InsertResult.size, 0);

          Exclude(Data.Status, fsNew);
          Exclude(Data.Status, fsEntry);
          Data.Size := Size;
          Data.Offset := InsertResult.offset;
          Data.Compressed := UseCompression.Checked;
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
          Data.RFAFileHandle.DeleteEntry(Data.EntryName);
          Data.EntryName := BuildEntryNameFromTree(Node);
          Data.RFAFileHandle.InsertEntry(Data.EntryName, Data.offset, Data.Size, Data.CompSize, 0);
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

          if (Data.Status = []) and IsFile(Data.FileType) then
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
            InsertResult := TmpArchive.InsertFile(ExternalFile, UseCompression.Checked);
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
            InsertResult := TmpArchive.InsertFile(ExternalFile, UseCompression.Checked);
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
        CancelChange;
        QuickOpen;
      end;
    end;
  end;

  SyncStart;
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
    SelectionText.Visible := false;
  end;
end;


function TRFAViewForm.QuickOpen: boolean;
begin
  Result := false;
  if FileExists(OpenDialog.FileName) then
  begin
    // Loading file
    if LoadMap(OpenDialog.FileName) then
    begin
      RecentList.AddString(OpenDialog.FileName);
      RebuildRecentList;
      SaveDialog.FileName := OpenDialog.FileName;
      CancelChange;
      Title := OpenDialog.FileName;
      Result := true;
    end;
  end
    else
  begin
    ShowError('Open error', Format('File (%s) not found', [OpenDialog.FileName]));
  end;
end;

function TRFAViewForm.QuickSave(Defragmentation: boolean): boolean;
begin
  Result := false;
  if SaveMap(SaveDialog.FileName, Defragmentation) then
  begin
    OpenDialog.FileName := SaveDialog.FileName;
    RecentList.AddString(SaveDialog.FileName);
    RebuildRecentList;
    CancelChange;
    Result := true;
  end
    else
  begin
    SaveDialog.FileName := OpenDialog.FileName;
  end;
end;


procedure TRFAViewForm.RevertExecute(Sender: TObject);
var
  Node: PVirtualNode;
  Data: pFse;
begin
  Node := RFAList.GetFirstSelected;

  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);
    Data.ExternalFilePath := EmptyStr;
    Node := RFAList.GetNextSelected(Node, true);
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

procedure TRFAViewForm.ExtractAllExecute(Sender: TObject);
begin
  RFAList.SelectAll(false);
  ExtractSelected.Execute;
end;

procedure TRFAViewForm.ExtractSelectedExecute(Sender: TObject);
begin
  if (BrowseExtractForm.ShowModal = mrOk) then
  begin
    ExtractTo(BrowseExtractForm.Directory);
  end;
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
  if OperationPending then
    Exit;

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

procedure TRFAViewForm.RebuildRecentList;
begin
  RecentMenu.Clear;
  RecentList.EnumItems;
  RecentMenu.Enabled := (RecentMenu.Count > 0);
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


procedure TRFAViewForm.RFAListEdited(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex);
begin
  if FEditResult = edInvalid then

  else if FEditResult = edOk then
  begin
    NotifyChange;
    Sort;
  end;
end;

procedure TRFAViewForm.RFAListKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
begin
  inherited;
  if CharCode = VK_DELETE then
  begin
    DeleteSelection;
  end;
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

procedure TRFAViewForm.RFAListNodeMoved(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data : pFse;
begin
  Data := RFAList.GetNodeData(Node);
  Include(Data.Status, fsEntry);
end;

procedure TRFAViewForm.RFAListStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  (Sender as TBaseVirtualTree).CancelEditNode;
end;

procedure TRFAViewForm.RFAListStateChange(Sender: TBaseVirtualTree; Enter,
  Leave: TVirtualTreeStates);
begin
  inherited;
  //SelectionText.Caption := FormatDateTime('hh:nn:zz',now);

  if tsChangePending in Leave then
  begin
    SelectionText.Visible := Sender.SelectedCount > 0;

    if SelectionText.Visible then
    begin
      if Sender.SelectedCount = 1 then
        SelectionText.Caption := BuildEntryNameFromTree(Sender.GetFirstSelected)
      else
        SelectionText.Caption := Format('%d items in selection',[Sender.SelectedCount]);
    end;
  end;
end;

procedure TRFAViewForm.RFAListDblClick(Sender: TObject);
begin
  EditSelection;
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

procedure TRFAViewForm.RFAListDrawText(Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; const Text: string; const CellRect: TRect; var DefaultDraw: Boolean);
var
  Data : pFse;
begin
  Data := RFAList.GetNodeData(Node);

  if (fsExternal in Data.Status)
  or (fsNew in Data.Status)
  or (fsConflict in Data.Status) then
    TargetCanvas.Font.Color := clBlack;

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


{ TRFAViewForm }


procedure TRFAViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  EnableSkinning(RFAList);
  Reset;
end;


procedure TRFAViewForm.FilesystemExecute(Sender: TObject);
begin
  FSViewForm.Showmodal;
end;

procedure TRFAViewForm.FormActivate(Sender: TObject);
begin
  inherited;
  ActiveControl := RFAList;
  ApplicationRun.Execute;
end;

procedure TRFAViewForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FArchive) then
    FArchive.Free;

  inherited;
end;


procedure TRFAViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Theme.Text := SkinManager.CurrentSkinName;
  FormStorage.SaveFormPlacement;

  if DirectoryExists(GetAppTempDirectory) then
    DeleteDirectory(GetAppTempDirectory, false);
end;


procedure TRFAViewForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  Result : TModalResult;
begin
  inherited;

  if Save.Enabled or (CountFilesByStatus(RFAList.RootNode, [fsNew], false) > 0) then
  begin
    Result := ShowDialog('Close', 'Save changes before exiting?', mtInformation, mbYesNoCancel, mbCancel, 0);
    case Result of
      mrYes : SaveAs.Execute;
      mrCancel: CanClose := false;
    end;
  end;
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
  if OpenDialog.Execute then
  begin
    QuickOpen;
  end;
end;

procedure TRFAViewForm.CancelChange;
begin
  Save.Enabled := false;
  if Assigned(FArchive) then
    Defrag.Enabled := FArchive.Fragmentation > 0
  else
    Defrag.Enabled := false;
end;

procedure TRFAViewForm.NotifyChange;
begin
  if Assigned(FArchive) then
  begin
    Save.Enabled := true;
    Defrag.Enabled := true;
  end;
end;

procedure TRFAViewForm.SaveAsExecute(Sender: TObject);
var
  UseDefrag : boolean;
begin
  UseDefrag := false;
  SaveDialog.InitialDir := ExtractFilePath(SaveDialog.FileName);
  SaveDialog.FileName := ExtractFileName(SaveDialog.FileName);

  if SaveDialog.Execute then
  begin
    if OpenDialog.FileName = SaveDialog.FileName then
      UseDefrag := true;

    if QuickSave(UseDefrag) then
    begin
      //Save.Enabled := false;
      //Defrag.Enabled := false;
    end;
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

procedure TRFAViewForm.QuitExecute(Sender: TObject);
begin
  Application.Terminate;
end;


procedure TRFAViewForm.CancelExecute(Sender: TObject);
begin
  Cancel.Enabled := false;
end;

procedure TRFAViewForm.NewExecute(Sender: TObject);
begin
  Reset;
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

procedure TRFAViewForm.AboutExecute(Sender: TObject);
begin
  AboutForm.Showmodal;
end;


procedure TRFAViewForm.RecentExecute(Sender: TObject);
begin
//
end;


procedure TRFAViewForm.PackDirectoryExecute(Sender: TObject);
var
  FileDrive : string;
  BasePath : string;
  Node : PVirtualNode;
begin
  if (BrowsePackForm.ShowModal = mrOk) then
    if DirectoryExists(BrowsePackForm.Directory) then
    begin

      if Reset(ASK_BEFORE_RESET) = mrCancel then
      begin
        Exit;
      end;

      Node := RFAList.RootNode;

      if BrowsePackForm.UseBasePath.Checked then
      begin
        BasePath := StringReplace(BrowsePackForm.Base.Text,'/','\',[rfReplaceAll]);
        BasePath := IncludeTrailingBackslash(BasePath);
        FileDrive := ExtractFileDrive(BasePath);

        if FileDrive <> EmptyStr then
          BasePath := SFRight(FileDrive, BasePath);

        if not ValidDirectoryname(BasePath) then
        begin
          ShowMessage('Base path error', Format('Base file path (%s) is invalid',[BasePath]));
          Exit;
        end;

        Node := BuildTreeFromFullPath(BasePath);
      end;

      Add(Node, nil, BrowsePackForm.Directory);
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


procedure TRFAViewForm.NewVersionAvailableExecute(Sender: TObject);
begin
  UpdateManagerForm.ShowModal;
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
    tbMenuBar.Enabled := false;
    TotalProgressBar.Position := 0;
    TotalProgressBar.Max := Max;
    //SubProgressBar.Position := 0;
  end
    else
  if Operation = roEnd then
  begin
    ProgressPanel.Hide;
    tbMenuBar.Enabled := true;
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

end.
