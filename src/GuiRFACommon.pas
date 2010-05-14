unit GuiRFACommon;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, ActnList, JCLFileUtils, RFALib, Types, SpTBXControls, StdCtrls, SpTBXEditors, SpTBXItem;

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


  TRFACommonForm = class(TForm)
    RFAList: TVirtualStringTree;
    Actions: TActionList;
    ExtractSelected: TAction;
    Preview: TAction;
    ExpandAll: TAction;
    CollapseAll: TAction;
    ExpandSelected: TAction;
    CollapseSelected: TAction;
    ExtractAll: TAction;
    SearchBar: TSpTBXPanel;
    Search: TSpTBXEdit;
    SpTBXLabel1: TSpTBXLabel;
    SpTBXButton1: TSpTBXButton;
    SearchStart: TAction;
    SearchStop: TAction;
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
    procedure ExtractSelectedExecute(Sender: TObject);
    procedure ExtractAllExecute(Sender: TObject);
    procedure SearchStartExecute(Sender: TObject);
    procedure SearchStopExecute(Sender: TObject);
    procedure SearchChange(Sender: TObject);
  private
    FSearchText: string;
    { D�clarations priv�es }
    function IsFolder(Name: String) : boolean;
    procedure SetSearchText(const Value: string);
  protected
    procedure MakePathVisible(Table: TBaseVirtualTree; Node: PVirtualNode);
    function FindFileByName(Filename: string): PVirtualNode;
    procedure ExtractTo(Directory: string; List: TStringList = nil);
    procedure PropagateStatus(Node: PVirtualNode; Status: TEntryModification);
    procedure ExportFile(Node: PVirtualNode; OutputStream: TStream);
    function FindPath(Path: String) : PVirtualNode;
    function BuildEntryNameFromTree(Node : PVirtualNode; SelectionOnly : boolean = false) : string;
    function BuildTreeFromFullPath(Path: AnsiString) : PVirtualNode;
    procedure Sort;
    function IsFile(FileType: TFileType): boolean;
    procedure ExpandSelection(Value: boolean);
    procedure ExtendSelection(Node: PVirtualNode);
    function ExtractTemporary(Node: PVirtualNode): string;
    function CountFilesByStatus(Node: PVirtualNode; Status: TEntryStatus; IncludeWithoutStatus: boolean = false): cardinal;
  public
    { D�clarations publiques }

    property SearchText : string read FSearchText write SetSearchText;
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
  end;

var
  RFACommonForm: TRFACommonForm;

implementation

{$R *.dfm}

uses
  GuiAbout, GuiMain, GuiRAWView, GuiSMView, GuiBrowse, GuiBrowsePack, GuiSkinDialog, Resources, Masks,
  Math, StringFunction, GuiBrowseExtract,
  CommonLib, AppLib, MD5Api;


function TRFACommonForm.IsFolder(Name: String) : boolean;
begin
  result := SFRight(DirDelimiter,Name) <> EmptyStr;
end;

function TRFACommonForm.IsFile(FileType: TFileType): boolean;
begin
  Result := not (FileType = ftFolder);
end;

function TRFACommonForm.FindPath(Path: String) : PVirtualNode;
var
  Node: PVirtualNode;
  Data : pFse;
begin
  Result := nil;
  Node := RFAList.GetFirst;
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


procedure TRFACommonForm.MakePathVisible(Table : TBaseVirtualTree; Node: PVirtualNode);
begin
  repeat
    if Node = Table.RootNode then
      Break;

    Table.IsVisible[Node] := true;
    Table.Expanded[Node] := true;
    Node := Node.Parent;
  until False;
end;




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


function TRFACommonForm.FindFileByName(Filename: string): PVirtualNode;
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


procedure TRFACommonForm.ExtractAllExecute(Sender: TObject);
begin
  RFAList.SelectAll(false);
  ExtractSelected.Execute;
end;

procedure TRFACommonForm.ExtractSelectedExecute(Sender: TObject);
begin
  if (BrowseExtractForm.ShowModal = mrOk) then
  begin
    ExtractTo(BrowseExtractForm.Directory);
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

procedure TRFACommonForm.ExtendSelection(Node : PVirtualNode);
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

procedure TRFACommonForm.ExportFile(Node: PVirtualNode; OutputStream : TStream);
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


procedure TRFACommonForm.ExtractTo(Directory: string; List : TStringList = nil);
var
  Data : pFse;
  Node: PVirtualNode;
  ExternalFilePath : string;
  ExternFile : TFileStream;
  W32Path : string;
begin
  //Cancel.Enabled := true;

  if Assigned(List) then
    List.Clear;

  //TotalProgress(roBegin, PG_NULL, RFAList.SelectedCount);
  Node := RFAList.GetFirstSelected;
  while Node <> nil do
  begin
    (*
    if not Cancel.Enabled then
      Break;
    *)

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

    //TotalProgress(roExport, PG_AUTO, RFAList.SelectedCount);
    Node := RFAList.GetNextSelected(Node);
  end;
  //TotalProgress(roEnd, PG_NULL, PG_NULL);
end;


procedure TRFACommonForm.PreviewExecute(Sender: TObject);
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


procedure TRFACommonForm.RFAListCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
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
(*
  if CharCode = VK_DELETE then
  begin
    DeleteSelection;
  end;
*)
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


procedure TRFACommonForm.RFAListGetNodeDataSize(Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rFse);
end;

procedure TRFACommonForm.SearchChange(Sender: TObject);
begin
  SearchText := Search.Text;
end;

procedure TRFACommonForm.SearchStartExecute(Sender: TObject);
begin
  SearchBar.Show;
  SearchText := Search.Text;
  ActiveControl := Search;
end;

procedure TRFACommonForm.SearchStopExecute(Sender: TObject);
begin
  SearchBar.Hide;
  SearchText := EmptyStr;
end;

procedure TRFACommonForm.SetSearchText(const Value: string);
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

procedure TRFACommonForm.Sort;
begin
  RFAList.SortTree(0, sdAscending);
end;



end.