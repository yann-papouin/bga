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
 * The Original Code is GuiRFACommon (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiRFACommon;

interface

{$I BGA.inc}

{$IfDef REMOVE_OPENGL_SUPPORT}
  {$Undef OPENGL_SUPPORT}
{$EndIf}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, GuiFormCommon,
  Dialogs, VirtualTrees, ActnList, JCLFileUtils, RFALib, Types, SpTBXControls,
  StdCtrls, SpTBXEditors, SpTBXItem, ExtCtrls, SyncObjs;

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
    ftFileBMP,
    ftFileJPG,
    ftFilePNG,
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

  TSortBy =
  (
    sbFilename,
    sbSize,
    sbCompressed,
    sbRatio,
    sbOffset
  );

  TEntryStatus = set of TEntryModification;

  TSearchState =
  (
    srWaiting,
    srStopping,
    srUpdating,
    srWorking
  );

  TSearchResult =
  (
    srNone,
    srYes,
    srNo,
    srOk
  );

  TRFACommonForm = class;

  TSearchThread = class(TThread)
  private
    FSearchNode : PVirtualNode;
    FGui : TRFACommonForm;
    FName: string;
  protected
    procedure Execute; override;
    procedure MatchNode;
    procedure HideNode;
  public
    State : TSearchState;
    constructor Create(CreateSuspended:boolean; Gui: TRFACommonForm; Name: string);
    destructor Destroy; override;
  end;


  TRFACommonForm = class(TFormCommon)
    Actions: TActionList;
    Preview: TAction;
    ExpandAll: TAction;
    CollapseAll: TAction;
    ExpandSelected: TAction;
    CollapseSelected: TAction;
    SearchStart: TAction;
    SearchStop: TAction;
    Container: TSpTBXPanel;
    RFAList: TVirtualStringTree;
    SearchBar: TSpTBXPanel;
    SearchEdit: TSpTBXEdit;
    SpTBXLabel1: TSpTBXLabel;
    SpTBXButton1: TSpTBXButton;
    SearchProgressBar: TSpTBXProgressBar;
    procedure CollapseSelectedExecute(Sender: TObject);
    procedure ExpandSelectedExecute(Sender: TObject);
    procedure CollapseAllExecute(Sender: TObject);
    procedure ExpandAllExecute(Sender: TObject);

    procedure RFAListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure RFAListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure RFAListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
    procedure RFAListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure RFAListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure RFAListKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
    procedure PreviewExecute(Sender: TObject);
    procedure SearchStartExecute(Sender: TObject);
    procedure SearchStopExecute(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure RFAListHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FSearchThread : TSearchThread;
    FSortBy : TSortBy;
    FSortDirection : TSortDirection;
  protected
    function FindFileByName(Filename: string; ParentNode : PVirtualNode = nil): PVirtualNode;
    function FindFileByPath(Path: string): PVirtualNode;
    function FindFile(Path: String): PVirtualNode;
    function FindPath(Path: String) : PVirtualNode;
    procedure PropagateStatus(Node: PVirtualNode; Status: TEntryModification);
    procedure ExportFile(const Node: PVirtualNode; OutputStream: TStream);
    function BuildEntryNameFromTree(Node : PVirtualNode; SelectionOnly : boolean = false) : string;
    //function BuildTreeFromFullPath(Path: AnsiString) : PVirtualNode;
    function GetBuildPath(Path : string) : PVirtualNode;
    procedure Sort;
    function IsFile(FileType: TFileType): boolean;
    procedure ExpandSelection(Value: boolean);
    procedure ExtendSelection(Node: PVirtualNode);
    function ExtractTemporary(Node: PVirtualNode): string;
    function CountFilesByStatus(Node: PVirtualNode; Status: TEntryStatus; IncludeWithoutStatus: boolean = false): cardinal;
    function ExtensionToType(Ext : string): TFileType;
    function PreviewSelection : boolean;
    function GetFileByPath(Sender: TObject; const VirtualPath : string) : string;
    procedure WarnAboutOpenGL;
    procedure GoToSelection;
    procedure SearchThreadCreate;
    procedure SearchThreadDestroy;
  public
    SearchList: TStringList;
  end;



  pFse = ^rFse;
  rFse = record
    RFAFileHandle : TRFAFile;
    RFAFileName : string[255];
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
    //
    SearchResult : TSearchResult;
  end;

var
  RFACommonForm: TRFACommonForm;

implementation

{$R *.dfm}

uses
  DbugIntf,
  {$IfDef PREVIEW_SUPPORT}
  GuiSMView,
  GuiPicView,
  {$EndIf}
  GuiSkinDialog,
  Resources,
  Masks,
  Math,
  JclStrings,
  StringFunction,
  CommonLib,
  AppLib,
  MD5Api;


function TRFACommonForm.IsFile(FileType: TFileType): boolean;
begin
  Result := not (FileType = ftFolder);
end;

function TRFACommonForm.GetFileByPath(Sender: TObject; const VirtualPath: string): string;
var
  Node: PVirtualNode;
begin
  Node := FindFile(VirtualPath);
  if Node <> nil then
    Result := ExtractTemporary(Node)
  else
    Result := EmptyStr;
end;



function TRFACommonForm.FindPath(Path: String) : PVirtualNode;
var
  Node: PVirtualNode;
  Data : pFse;
begin
  Result := nil;
  Node := RFAList.GetFirst; // Maybe we could use GetFirstLevel to be faster
  Path := ExcludeTrailingPathDelimiter(ExtractFilePath(Path));

  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);
    if (Data.FileType = ftFolder) and (Data.W32Path = Path) then
    begin
      Result := Node;
      Break;
    end;
    Node := RFAList.GetNext(Node);
  end;
end;


procedure TRFACommonForm.FormCreate(Sender: TObject);
begin
  inherited;
  SearchList := TStringList.Create;
  SearchList.Delimiter := ';';
end;

procedure TRFACommonForm.FormDestroy(Sender: TObject);
begin
  inherited;
  SearchStop.Execute;
  SearchList.Free;
end;

function TRFACommonForm.FindFile(Path: String) : PVirtualNode;
var
  Node: PVirtualNode;
  Data : pFse;
  Pos : Integer;
  NewDiff, PreviousDiff : integer;
begin
  Result := nil;
  PreviousDiff := -1;
  Node := RFAList.GetFirst;
  Path := StringReplace(Path,'\','/',[rfReplaceAll]);

  while Node <> nil do
  begin
    Data := RFAList.GetNodeData(Node);
    Pos := SFUniPos(Path, Data.EntryName);
    if Pos > 0 then
    begin
      SendDebugFmt('%s = %s',[Data.EntryName, Path]);
      NewDiff := Length(Data.EntryName) - Length(Path);

      if (NewDiff < PreviousDiff) or (PreviousDiff < 0) then
      begin
        PreviousDiff := NewDiff;
        Result := Node;
      end;
    end;
    Node := RFAList.GetNext(Node);
  end;
end;

{.$DEFINE DEBUG_BP}
{.$DEFINE DEBUG_BP_DETAILS}

function TRFACommonForm.GetBuildPath(Path: string): PVirtualNode;
var
  Count: integer;
  i: integer;
  Node, Parent, Previous : PVirtualNode;
  Data : pFse;
  {$IfDef DEBUG_BP}
  DataDbg: pFse;
  {$EndIf}
  SegPath, Current : string;
  Create : boolean;
begin
  Path := StrEnsurePrefix(DirDelimiter, Path);
  Count := StrCharCount(Path, DirDelimiter);
  Result := RFAList.RootNode;
  Node := nil;
  Parent := nil;
  Previous := nil;
  Create := false;
  SegPath := Path;

  {$IfDef DEBUG_BP}
  SendSeparator;
  SendDebug(Path);
  {$EndIf}

  for i := 0 to Count - 2 do
  begin
    Current := StrBetween(SegPath, DirDelimiter, DirDelimiter);
    {$IfDef DEBUG_BP}
    SendDebug(Current);
    {$EndIf}

    // If we not tag current (and children) as not existing then search it
    if not Create then
    begin
      Parent := Node;

      if Node = nil then
        Node := RFAList.GetFirstLevel(0)
      else
        Node := RFAList.GetFirstChild(Previous);

      while Node <> nil do
      begin
        Data := RFAList.GetNodeData(Node);
        {$IfDef DEBUG_BP_DETAILS}
        SendDebugFmt('Acual node = %s',[Data.W32Name]);
        {$EndIf}

        // We found a match
        if AnsiCompareText(Current, Data.W32Name) = 0 then
        begin
          Previous := Node;
          Result := Previous;
          {$IfDef DEBUG_BP_DETAILS}
          SendDebugFmt('Match found for %s',[Current]);
          {$EndIf}
          Break;
        end;

        // Select the next node in the same level
        Node := RFAList.GetNextSibling(Node);

        {$IfDef DEBUG_BP_DETAILS}
        if (Node <> nil) and (Node.Parent <> nil) and (Node.Parent <> RFAList.RootNode) then
        begin
          Data :=  RFAList.GetNodeData(Node.Parent);
          SendDebugFmt('Parent of current is %s',[Data.W32Name]);
        end;
        {$EndIf}

        // if we jump a parent node then cancel
        if (Node <> nil) and (Node.Parent <> Parent) and (Node.Parent <> RFAList.RootNode) then
        begin
          Node := nil;
          SendDebugFmt('Cancel "%s" after a jump',[Current]);
        end;

      end;
    end;

    // We didn't find any folder with the same name
    if Node = nil then
    begin
      Create := true;

      {$IfDef DEBUG_BP}
      SendDebugFmt('No match, create "%s" now',[Current]);
      {$EndIf}

      Node := RFAList.AddChild(Parent);
      Data := RFAList.GetNodeData(Node);

      Data.W32Path := SFNLeft(DirDelimiter, Path, i+2);
      Data.W32Name := Current;
      Data.FileType := ftFolder;

      Parent := Node;
      Result := Parent;
      Node := nil;
    end;

    SegPath := StrAfter(Current, SegPath);
  end;

end;

(*
function TRFACommonForm.BuildTreeFromFullPath(Path: AnsiString) : PVirtualNode;
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
            Node := RFAList.GetFirstLevel(i-1)
          else
            Node := RFAList.RootNode;

        Node := RFAList.AddChild(Node);
        Data := RFAList.GetNodeData(Node);

        Data.W32Path := SFNLeft(DirDelimiter, Path, i+1);
        Data.W32Name := Middle;
        Data.FileType := ftFolder;

        //CheckValidity(Node);
      end;
    end;
  end;
  Result := Node;
end;
*)


function TRFACommonForm.BuildEntryNameFromTree(Node : PVirtualNode; SelectionOnly : boolean = false) : string;
var
  Data : pFse;
begin
  Data := RFAList.GetNodeData(Node);
  Result := Data.W32Name;

  while True do
  begin
    Node := RFAList.NodeParent[Node];

    if (Node = nil) or (SelectionOnly and not RFAList.Selected[Node]) then
      Break;

    Result := '/' + Result;
    Data := RFAList.GetNodeData(Node);
    Result := Data.W32Name + Result;
  end;
end;


function TRFACommonForm.FindFileByName(Filename: string; ParentNode : PVirtualNode): PVirtualNode;
var
  Data : pFse;
begin
  if ParentNode = nil then
    Result := RFAList.GetFirst
  else
    Result := RFAList.GetFirstChild(ParentNode);


  while Result <> nil do
  begin
    Data := RFAList.GetNodeData(Result);

    if AnsiCompareText(Data.W32Name, Filename) = 0 then
      Exit
    else
    begin
      if ParentNode = nil then
        Result := RFAList.GetNext(Result, true)
      else
        Result := RFAList.GetNextSibling(Result);
    end;
  end;
end;


function TRFACommonForm.FindFileByPath(Path: string): PVirtualNode;
var
  Data : pFse;
begin
  Result := RFAList.GetFirst;

  while Result <> nil do
  begin
    Data := RFAList.GetNodeData(Result);

    if AnsiCompareText(Data.W32Path, Path) = 0 then
      Exit
    else
      Result := RFAList.GetNext(Result, true);
  end;
end;



function TRFACommonForm.ExtensionToType(Ext: string): TFileType;
begin
  if Ext = '.con' then
    Result := ftFileCON
  else
  if Ext = '.inc' then
    Result := ftFileINC
  else
  if Ext = '.ssc' then
    Result := ftFileSSC
  else
  if Ext = '.txt' then
    Result := ftFileTXT
  else
  if Ext = '.bak' then
    Result := ftFileBAK
  else
  if Ext = '.wav' then
    Result := ftFileWAV
  else
  if Ext = '.bik' then
    Result := ftFileBIK
  else
  if Ext = '.dds' then
    Result := ftFileDDS
  else
  if Ext = '.tga' then
    Result := ftFileTGA
  else
  if Ext = '.bmp' then
    Result := ftFileBMP
  else
  if Ext = '.jpg' then
    Result := ftFileJPG
  else
  if Ext = '.png' then
    Result := ftFilePNG
  else
  if Ext = '.raw' then
    Result := ftFileRAW
  else
  if Ext = '.dat' then
    Result := ftFileDAT
  else
  if Ext = '.rs' then
    Result := ftFileRS
  else
  if Ext = '.rcm' then
    Result := ftFileRCM
  else
  if Ext = '.lsb' then
    Result := ftFileLSB
  else
  if Ext = '.pal' then
    Result := ftFilePAL
  else
  if Ext = '.zip' then
    Result := ftFileZIP
  else
  if Ext = '.rar' then
    Result := ftFileRAR
  else
  if Ext = '.gz' then
    Result := ftFileGZ
  else
  if Ext = '.bz2' then
    Result := ftFileBZ2
  else
  if Ext = '.7z' then
    Result := ftFile7Z
  else
  if Ext = '.dif' then
    Result := ftFileDIF
  else
  if Ext = '.font' then
    Result := ftFileFONT
  else
  if Ext = '.baf' then
    Result := ftFileBAF
  else
  if Ext = '.skn' then
    Result := ftFileSKN
  else
  if Ext = '.ske' then
    Result := ftFileSKE
  else
  if Ext = '.vso' then
    Result := ftFileVSO
  else
  if Ext = '.sm' then
    Result := ftFileSM
  else
  if Ext = '.tm' then
    Result := ftFileTM
  else
    Result := ftFile;
end;

procedure TRFACommonForm.PropagateStatus(Node: PVirtualNode; Status: TEntryModification);
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


function TRFACommonForm.ExtractTemporary(Node: PVirtualNode): string;
var
  Data : pFse;
  ExternFile : TFileStream;
begin
  Result := EmptyStr;
  if (Node = nil) or (Node = RFAList.RootNode) then
    Exit;

  BeginOperation;
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
  EndOperation;
end;

procedure TRFACommonForm.ExtendSelection(Node : PVirtualNode);
begin
  RFAList.BeginUpdate;

  if RFAList.IsVisible[Node] then
    RFAList.Selected[Node] := true;

  Node := Node.FirstChild;
  while Node <> nil do
  begin
    ExtendSelection(Node);
    Node := RFAList.GetNextSibling(Node);
  end;

  RFAList.EndUpdate;
end;


procedure TRFACommonForm.ExpandSelection(Value: boolean);
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


procedure TRFACommonForm.CollapseAllExecute(Sender: TObject);
begin
  RFAList.SelectAll(false);
  CollapseSelected.Execute;
end;

procedure TRFACommonForm.CollapseSelectedExecute(Sender: TObject);
begin
  ExpandSelection(false);
end;

procedure TRFACommonForm.ExpandAllExecute(Sender: TObject);
begin
  RFAList.SelectAll(false);
  ExpandSelected.Execute;
end;

procedure TRFACommonForm.ExpandSelectedExecute(Sender: TObject);
begin
  ExpandSelection(true);
end;

procedure TRFACommonForm.ExportFile(const Node: PVirtualNode; OutputStream : TStream);
var
  Data : pFse;
begin
  Data := RFAList.GetNodeData(Node);

  if Data.Compressed then
  begin
    Data.RFAFileHandle.DecompressToStream(OutputStream, Data.Offset, Data.CompSize);
  end
    else
  begin
    Data.RFAFileHandle.ExtractToStream(OutputStream, Data.Offset, Data.CompSize, Data.Size);
  end
end;


function TRFACommonForm.CountFilesByStatus(Node: PVirtualNode; Status:TEntryStatus; IncludeWithoutStatus : boolean = false) : cardinal;
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

function TRFACommonForm.PreviewSelection: boolean;
var
  Node: PVirtualNode;
  Data : pFse;

  function FindRs : PVirtualNode;
  var
    rsData : pFse;
  begin
    Result := Node.Parent.FirstChild; // Should be ok, works fine even if the parent is RootNode
    while Result <> nil do
    begin
      rsData := RFAList.GetNodeData(Result);

      // just compare files in the same folder without their extension
      if rsData.FileType = ftFileRS then
        if SFLeftFromLast('.', rsData.W32Name) = SFLeftFromLast('.', Data.W32Name) then
          Break;

      Result := Result.NextSibling;
    end;
  end;

begin
  Result := false;

  Node := RFAList.GetFirstSelected;

  if (Node = nil) or (Node = RFAList.RootNode) then
    Exit;

  Data := RFAList.GetNodeData(Node);

  case Data.FileType of
    ftFileSM:
    begin
      Result := true;

      {$IfDef OPENGL_SUPPORT}
        if not Assigned(SMViewForm) then
          Application.CreateForm(TSMViewForm, SMViewForm);

        SMViewForm.GetFileByPath := GetFileByPath;
        SMViewForm.FreeMesh.MeshObjects.Clear;
        SMViewForm.LoadMaterials(ExtractTemporary(FindRs));
        SMViewForm.LoadStandardMesh(ExtractTemporary(Node));
        SMViewForm.Preview;
      {$Else}
        WarnAboutOpenGL;
      {$EndIf}

    end;
    ftFileDDS, ftFileTGA, ftFileBMP, ftFileJPG, ftFilePNG:
    begin
      Result := true;
      {$IfDef OPENGL_SUPPORT}
        if not Assigned(PICViewForm) then
          Application.CreateForm(TPICViewForm, PICViewForm);

        PICViewForm.LoadTexture(ExtractTemporary(Node));
        PICViewForm.Preview;
      {$Else}
        WarnAboutOpenGL;
      {$EndIf}
    end;
    else
      begin
        Result := false;
      end;
  end;
end;

procedure TRFACommonForm.PreviewExecute(Sender: TObject);
begin
  if PreviewSelection then
  else
    begin
      ShowMessage('No preview', 'There is not built-in preview for the selected file');
    end;
end;


procedure TRFACommonForm.RFAListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2 : pFse;
  FloatValue1 : single;
  FloatValue2 : single;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);


  if FSortby = sbFilename then
  begin
    if IsFile(Data1.FileType) = IsFile(Data2.FileType) then
      Result := CompareStr(UpperCase(Data1.W32Name), UpperCase(Data2.W32Name))
    else
    if IsFile(Data1.FileType) then
      Result := GreaterThanValue
    else
    if IsFile(Data2.FileType) then
      Result := LessThanValue;
  end;

  if FSortby = sbSize then
  begin
    if IsFile(Data1.FileType) = IsFile(Data2.FileType) then
      Result := CompareValue(Data1.Size, Data2.Size)
    else
    if IsFile(Data1.FileType) then
      Result := GreaterThanValue
    else
    if IsFile(Data2.FileType) then
      Result := LessThanValue;
  end;

  if FSortby = sbCompressed then
  begin
    if IsFile(Data1.FileType) = IsFile(Data2.FileType) then
      Result := CompareValue(Data1.CompSize, Data2.CompSize)
    else
    if IsFile(Data1.FileType) then
      Result := GreaterThanValue
    else
    if IsFile(Data2.FileType) then
      Result := LessThanValue;
  end;

  if FSortby = sbRatio then
  begin
    if IsFile(Data1.FileType) = IsFile(Data2.FileType) then
    begin
      FloatValue1 := Data1.CompSize / Data1.Size  * 100;
      FloatValue2 := Data2.CompSize / Data2.Size  * 100;
      Result := CompareValue(FloatValue1, FloatValue2);
    end
    else
    if IsFile(Data1.FileType) then
      Result := GreaterThanValue
    else
    if IsFile(Data2.FileType) then
      Result := LessThanValue;
  end;

  if FSortby = sbOffset then
  begin
    if IsFile(Data1.FileType) = IsFile(Data2.FileType) then
      Result := CompareValue(Data1.Offset, Data2.Offset)
    else
    if IsFile(Data1.FileType) then
      Result := GreaterThanValue
    else
    if IsFile(Data2.FileType) then
      Result := LessThanValue;
  end;

end;



procedure TRFACommonForm.RFAListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
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
        if Data.Size > 0 then
        begin
          FloatValue := Data.CompSize / Data.Size  * 100;
          CellText := Format('%.2f%%',[FloatValue]);
        end
          else
        begin
          CellText := '---'
        end;
      end
        else
          CellText := '---';

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

procedure TRFACommonForm.RFAListHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
var
  NewSort : TSortBy;
begin

  case HitInfo.Column of
    0: NewSort := sbFilename;
    1: NewSort := sbSize;
    2: NewSort := sbCompressed;
    3: NewSort := sbRatio;
    4: NewSort := sbOffset;
    else
       NewSort := sbFilename;
  end;

  if NewSort = FSortBy then
  begin
    if FSortDirection = sdAscending then
      FSortDirection := sdDescending
    else
    if FSortDirection = sdDescending then
      FSortDirection := sdAscending
  end
    else
  begin
    FSortDirection := sdAscending;
  end;

  FSortBy := NewSort;
  Sort;
end;

procedure TRFACommonForm.RFAListKeyAction(Sender: TBaseVirtualTree; var CharCode: Word; var Shift: TShiftState; var DoDefault: Boolean);
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
end;


procedure TRFACommonForm.RFAListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data : pFse;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;



procedure TRFACommonForm.RFAListGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: Integer);
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
        ftFileDDS, ftFileTGA, ftFileBMP, ftFilePNG, ftFileJPG : ImageIndex := 108;
        ftFileZIP, ftFileRAR, ftFileGZ, ftFileBZ2, ftFile7Z : ImageIndex := 109;
        ftFileSM : ImageIndex := 174;
        ftFileTM : ImageIndex := 175;
        ftFolder : ImageIndex := 122;
       end;
    else
      ImageIndex := -1;
  end;
end;


procedure TRFACommonForm.RFAListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rFse);
end;

procedure TRFACommonForm.SearchEditChange(Sender: TObject);
var
  Node : pVirtualNode;
begin
  if Assigned(FSearchThread) then
    SearchThreadDestroy;

  SearchProgressBar.Position := 0;
  SearchProgressBar.Max := RFAList.TotalCount;
  SearchList.DelimitedText := SearchEdit.Text;

  if SearchEdit.Text <> EmptyStr then
  begin
    if SearchBar.Visible then
      SearchThreadCreate;
  end
    else
  begin
    Node := RFAList.GetFirst(true);
    RFAList.BeginUpdate;
    while Node <> nil do
    begin
      RFAList.IsVisible[Node] := True;

      if RFAList.GetNodeLevel(Node) <= 2 then
        RFAList.Expanded[Node] := True
      else
        RFAList.Expanded[Node] := not True;

      Node := RFAList.GetNext(Node, true);
    end;
    RFAList.EndUpdate;
    GoToSelection;
  end;
end;

procedure TRFACommonForm.SearchStartExecute(Sender: TObject);
begin
  SearchBar.Show;
  ActiveControl := SearchEdit;
end;

procedure TRFACommonForm.SearchStopExecute(Sender: TObject);
begin
  SearchBar.Hide;
  SearchEdit.Text := EmptyStr;
end;

procedure TRFACommonForm.SearchThreadCreate;
begin
  if DebugHook <> 0 then
    Assert(FSearchThread = nil);

  if not Assigned(FSearchThread) then
    FSearchThread := TSearchThread.Create(False, Self, 'SearchThread');
end;

procedure TRFACommonForm.SearchThreadDestroy;
var
  Diff: Cardinal;
begin
  if Assigned(FSearchThread) then
  begin
    Diff := GetTickCount;
    FSearchThread.Terminate;
    if not FSearchThread.Suspended then
    begin
      Assert(FSearchThread.Terminated);
      FSearchThread.WaitFor;
      FSearchThread.Free;
    end;
    FSearchThread := nil;
    SendDebugFmt('SearchThread destroyed in %dms',[GetTickCount-Diff]);
  end;
end;


procedure TRFACommonForm.GoToSelection;
var
  SelectedNode : PVirtualNode;
begin
  SelectedNode := RFAList.GetFirstSelected;
  if (SelectedNode <> nil) and RFAList.IsVisible[SelectedNode] then
    RFAList.ScrollIntoView(SelectedNode,true);
end;


procedure TRFACommonForm.Sort;
begin
  RFAList.SortTree(0, FSortDirection);
end;


procedure TRFACommonForm.WarnAboutOpenGL;
begin
  ShowMessage('OpenGL Disabled', '3D rendering disabled for compatibility reasons');
end;



{ TSearchThread }

constructor TSearchThread.Create(CreateSuspended: boolean; Gui: TRFACommonForm; Name: string);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate:=false;
  Priority:=tpNormal;
  FSearchNode := nil;
  FGui := Gui;
  FName := Name;
end;

destructor TSearchThread.Destroy;
begin
  inherited;
end;

procedure TSearchThread.Execute;
var
  Node : PVirtualNode;
  Data : pFse;

  function LocalMatchesMask(AString : string) : boolean;
  var
    i :integer;
    Mask : string;
  begin
    Result := false;
    for i := 0 to FGui.SearchList.Count - 1 do
    begin
      Mask := '*'+trim(FGui.SearchList[i])+'*';
      if MatchesMask(AString, Mask) then
      begin
        Result := true;
        Break;
      end;
    end;
  end;

begin
  inherited;
  NameThreadForDebugging(FName);
  FSearchNode := FGui.RFAList.GetFirst;
  repeat
    if FSearchNode <> nil then
    begin
      Node := FSearchNode;
      Data := FGui.RFAList.GetNodeData(Node);
      if LocalMatchesMask(Data.W32Path) then
      begin
        Synchronize(MatchNode);
      end
        else
      begin
        Synchronize(HideNode);
      end;
      FSearchNode := FGui.RFAList.GetNext(FSearchNode, true);
    end;
  until
    Terminated;
end;

procedure TSearchThread.HideNode;
begin
  with FGui do
  begin
    RFAList.IsVisible[FSearchNode] := False;
    SearchProgressBar.Position := SearchProgressBar.Position+1;
  end;
end;

procedure TSearchThread.MatchNode;
var
  Node: PVirtualNode;
begin
  with FGui do
  begin
    RFAList.IsVisible[FSearchNode] := True;
    RFAList.Expanded[FSearchNode] := True;
    SearchProgressBar.Position := SearchProgressBar.Position+1;

    Node := FSearchNode;
    while (Node <> nil) and (Node <> FGui.RFAList.RootNode)  do
    begin
      RFAList.IsVisible[Node] := True;
      RFAList.Expanded[Node] := True;
      SearchProgressBar.Position := SearchProgressBar.Position+1;
      Node := Node.Parent;
    end;
  end;

end;


end.
