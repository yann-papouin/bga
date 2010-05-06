unit GuiMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Registry, ActnList, XPStyleActnCtrls, ActnMan, StdCtrls, JclFileUtils,
  ExtCtrls, JvExExtCtrls, JvRadioGroup, CheckLst, JvExCheckLst, JvCheckListBox, JvExStdCtrls, JvButton, JvCtrls, JvFooter, JvExtComponent, jpeg, ComCtrls, JvExComCtrls, JvProgressBar, JvExControls,
  JvLabel, ShellAPI, JvAppStorage, JvAppXMLStorage, JvComponentBase, JvFormPlacement, JvComCtrls, SpTBXTabs, TB2Item, SpTBXItem, SpTBXControls, TB2Dock, TB2Toolbar,
  JvAppIniStorage, SpTBXEditors, JvAppInst, JvBaseDlg, JvBrowseFolder;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    Console: TMemo;
    Image1: TImage;
    Label1: TLabel;
    FormStorage: TJvFormStorage;
    JvFooter2: TJvFooter;
    CloseBtn: TJvFooterBtn;
    Tab: TSpTBXTabControl;
    SpTBXTabItem1: TSpTBXTabItem;
    SpTBXTabSheet1: TSpTBXTabSheet;
    MapGroup: TSpTBXGroupBox;
    Panel2: TPanel;
    ModeList: TSpTBXRadioGroup;
    CheckAll: TSpTBXButton;
    UncheckAll: TSpTBXButton;
    SpTBXPanel1: TSpTBXPanel;
    SpTBXButton1: TSpTBXButton;
    Actions: TActionList;
    Install: TAction;
    Cancel: TAction;
    SpTBXButton2: TSpTBXButton;
    TotalProgress: TSpTBXProgressBar;
    TotalProgressLabel: TSpTBXLabel;
    INIFileStorage: TJvAppIniFileStorage;
    Settings: TSpTBXTabItem;
    SpTBXTabSheet3: TSpTBXTabSheet;
    SpTBXLabel1: TSpTBXLabel;
    BFCustomPath: TSpTBXEdit;
    MapList: TSpTBXCheckListBox;
    AppInstances: TJvAppInstances;
    SpTBXGroupBox1: TSpTBXGroupBox;
    Directory: TSpTBXButtonEdit;
    BrowseForFolder: TJvBrowseForFolderDialog;
    EditFolder: TAction;
    procedure FormCreate(Sender: TObject);
    procedure CloseBtnClick(Sender: TObject);
    procedure InstallClick(Sender: TObject);
    procedure UncheckAllClick(Sender: TObject);
    procedure CheckAllClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CancelClick(Sender: TObject);
    procedure AppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
    procedure MapListDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure EditFolderExecute(Sender: TObject);
    procedure DirectoryChange(Sender: TObject);
    procedure BFCustomPathChange(Sender: TObject);
  private
    { Déclarations privées }
    CopyCancel : boolean;
    AppPath :string;
    BFPath :string;

    procedure Init;
    procedure Progress(Visible : boolean);
    procedure DoCopyFile(const SrcFile, DstFile: string);
  public
    { Déclarations publiques }
    function SearchBattleField : boolean;
    function ListMapsInSameDir : boolean;
  end;

var
  MainForm: TMainForm;

implementation

uses
  DbugIntf;

{$R *.dfm}


procedure TMainForm.DoCopyFile(const SrcFile, DstFile: string);
const
  //bufSize = 16384; {Use a 16K buffer. You can use whatever size suits you, though.}
  bufSize = 65536;
var
  sStream,
  dStream : TFileStream;
  pBuf    : Pointer;
  cnt     : Integer;
  //prgFrm  : TFileProg;
  totCnt,
  X, 
  strmSize    : LongInt;
  ProgressOffset : integer;
begin
  totCnt := 0;
  {Open up the Source File to read it}
  sStream := TFileStream.Create(SrcFile, fmOpenRead or fmShareDenyWrite);

  {Create the copying progress form and set property values}
  ProgressOffset := TotalProgress.Position;


  {Get the size of the entire stream to use for the progress gauge. Note
   we have to call FileSeek first because it will place the pointer
   at the end of the file when we get the file first return value.}
  strmSize := sStream.size;

  try
    { Create the destination file. If it already exists,
      overwrite it. }
    dStream := TFileStream.Create(DstFile, fmCreate or fmShareExclusive);
    try
      GetMem(pBuf, bufSize);
      try
        {Read and write first bufSize bytes from source into the buffer
         If the file size is smaller than the default buffer size, then
         all the user will see is a quick flash of the progress form.}
        cnt := sStream.Read(pBuf^, bufSize);
        cnt := dStream.Write(pBuf^, cnt);

        totCnt := totCnt + cnt;
        {Loop the process of reading and writing}
        while (cnt > 0) do begin
          {Let things in the background proceed while loop is processing}
          Application.ProcessMessages;

          {Read bufSize bytes from source into the buffer}
          cnt := sStream.Read(pBuf^, bufSize);

          {Now write those bytes into destination}
          cnt := dStream.Write(pBuf^, cnt);

          {Increment totCnt for progress and do arithmetic to update the
           gauge}
          totcnt := totcnt + cnt;
          if not CopyCancel then
            begin
              TotalProgress.Position := ProgressOffset + Round((totCnt / strmSize) * 100);
              Update;
            end
          else
            Break;       {If user presses cancel button, then break out of loop}
                         {which will make program go to finally blocks}
        end;

      finally
        FreeMem(pBuf, bufSize);
      end;
    finally
      dStream.Free;
      if CopyCancel then {If copying was cancelled, delete the destination file}
        DeleteFile(DstFile);  {after stream has been freed, which will close the file.}
    end;
  finally
    sStream.Free;
  end;
end;

procedure TMainForm.EditFolderExecute(Sender: TObject);
begin
  //Showmessage(Sender.ClassName);
  BrowseForFolder.Directory := ((Sender as TControl).Parent as TSpTBXButtonEdit).Text;
  if BrowseForFolder.Execute then
  begin
    ((Sender as TControl).Parent as TSpTBXButtonEdit).Text := BrowseForFolder.Directory;
  end;
end;

function WinCopyFile(Source, Dest: string): Boolean;
var
   Struct : TSHFileOpStruct;
   Resultval: integer;
begin
   ResultVal := 1;
   try
     Source := Source + #0#0;
     Dest := Dest + #0#0;
     Struct.wnd := 0;
     Struct.wFunc := FO_COPY;
     Struct.pFrom := PWideChar(Source);
     Struct.pTo := PWideChar(Dest);
     Struct.fFlags:= FOF_SIMPLEPROGRESS or FOF_NOERRORUI or FOF_NOCONFIRMATION;
     Struct.fAnyOperationsAborted := False;
     Struct.hNameMappings := nil;
     Resultval := ShFileOperation(Struct);
   finally
     Result := (Resultval = 0);
  end;
end;



procedure TMainForm.CloseBtnClick(Sender: TObject);
begin
  Close;  
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CloseBtn.Enabled;
  FormStorage.SaveFormPlacement;
end;
(*
procedure blabla;
var

tt, ta : TFileStream;
begin

  ta := TFileStream.Create('C:\Users\Yann\Documents\RAD Studio\Projets\Battlefield\bin\StaticObjectsZLIB.con', fmCreate);
  tt := TFileStream.Create('C:\Users\Yann\Documents\RAD Studio\Projets\Battlefield\bin\StaticObjectsUNCOMPRESSED.con', fmOpenReadWrite);

  ta.Size := tt.Size;

  ZCompressStream(tt, ta, clDefault );

  ta.Free;
  tt.Free;
end;
*)

procedure TMainForm.FormCreate(Sender: TObject);
begin
//  DoubleBuffered := true;
  Tab.ActiveTabIndex := 0;
  Install.Enabled := false;

  AppPath := ExtractFilePath(ParamStr(0));

  Init;
  FormStorage.RestoreFormPlacement;

  if Directory.Text = EmptyStr then
    Directory.Text := AppPath;
end;

procedure TMainForm.DirectoryChange(Sender: TObject);
begin

  if ListMapsInSameDir then
    Install.Enabled := BFPath <> EmptyStr
  else
    Console.Lines.Add('No maps found');
end;



procedure TMainForm.Init;
begin
  if not SearchBattleField then
  begin
    Console.Lines.Add('Battlefield path not found in registry nor custom path');
  end;
end;


function TMainForm.ListMapsInSameDir: boolean;
var
  FileList : TStringList;
  i :integer;
begin
  result := false;

  FileList := TStringList.Create;

  Console.Lines.Add('Current path is ' + Directory.Text);

  if not BuildFileList(IncludeTrailingBackslash(Directory.Text)+'*.rfa', faAnyFile, FileList) then
    Console.Lines.Add('Unable to list files in current directory');

  Console.Lines.Add(EmptyStr);

  MapList.Clear;
  for i := 0 to FileList.Count - 1 do
  begin
    MapList.Items.Add(FileList[i]);
    MapList.Checked[i] := true;
    result := true;
  end;

  FileList.Free;

end;

procedure TMainForm.MapListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := true;
end;

function TMainForm.SearchBattleField : boolean;
var
  Reg:TRegistry;
  FileList : TStringList;
  i :integer;
begin
  Result := false;
  BFPath := EmptyStr;

  Reg:= TRegistry.Create(KEY_READ);
  
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.KeyExists('SOFTWARE\EA GAMES\Battlefield 1942\') then
  begin
    Reg.OpenKey('SOFTWARE\EA GAMES\Battlefield 1942\', false);
    BFPath := Reg.ReadString('GAMEDIR');
    Reg.CloseKey;
  end;
  Reg.Free;

  if not DirectoryExists(BFPath) then
    if DirectoryExists(BFCustomPath.Text) then
      BFPath := BFCustomPath.Text
    else
      Exit;

  BFPath := BFPath+'\Mods\';
  //Showmessage(BFPath);

  FileList := TStringList.Create;
  BuildFileList(BFPath+'*',-1,FileList);

  for i := 0 to FileList.Count - 1 do
  if IsDirectory(BFPath+FileList[i]) then
    if DirectoryExists(BFPath+FileList[i]+'\Archives') then
      Modelist.Items.Add(FileList[i]);

  // Small hack to select Desert Combat final by default
  for i := 0 to Modelist.Items.Count - 1 do
  if Modelist.Items[i] = 'DC_Final' then
    Modelist.ItemIndex := i;

  FileList.Free;

  Result := true;
end;


procedure TMainForm.UncheckAllClick(Sender: TObject);
var
  i :integer;
begin
  for i := 0 to MapList.Count - 1 do
  begin
    MapList.Checked[i] := false;
  end;
end;


procedure TMainForm.AppInstancesCmdLineReceived(Sender: TObject; CmdLine: TStrings);
begin
//
end;

procedure TMainForm.BFCustomPathChange(Sender: TObject);
begin
  //Init;
end;

procedure TMainForm.CancelClick(Sender: TObject);
begin
  CopyCancel := true;
end;

procedure TMainForm.CheckAllClick(Sender: TObject);
var
  i :integer;
begin
  for i := 0 to MapList.Count - 1 do
  begin
    MapList.Checked[i] := true;
  end;
end;


procedure TMainForm.InstallClick(Sender: TObject);
var
  i :integer;
  ModPath : string;
  FromFile, ToFile :string;
  RunOnce : boolean;

  function OverWrite2Txt : string;
  begin
    if FileExists(ToFile)
    then result := ' (overwritten)'
    else result := EmptyStr;
  end;
begin
  CopyCancel := false;

  Progress(false);
  RunOnce := false;
  if Modelist.ItemIndex = -1 then
  begin
    Showmessage('Select a mod first');
    Exit;
  end;

  ModPath := Modelist.Items[Modelist.ItemIndex] + '\Archives\BF1942\levels\';

  Console.Lines.Add('Installing files to :');
  Console.Lines.Add(BFPath+ModPath);
  Console.Lines.Add(EmptyStr);

  Progress(true);
  TotalProgress.Max := MapList.Count * 100;

  for i := 0 to MapList.Count - 1 do
  if MapList.Checked[i] then
  begin
    Application.ProcessMessages;
    FromFile := IncludeTrailingBackslash(Directory.Text) +MapList.Items[i];
    ToFile := BFPath+ModPath+MapList.Items[i];

    DoCopyFile(FromFile, ToFile);
    if true then
    begin
      Console.Lines.Add('Install OK : ' + MapList.Items[i] + OverWrite2Txt);
      RunOnce := true;
    end
    else
      Console.Lines.Add('Cannot install :' + MapList.Items[i]);

    //TotalProgress.Position := i+1;
    Sleep(300);
  end;

  Progress(false);

  if not RunOnce then
    Console.Lines.Add('No map installed');
end;


procedure TMainForm.Progress(Visible : boolean);
begin
  TotalProgress.Visible := Visible;
  TotalProgressLabel.Visible := Visible;
  TotalProgress.Position := 0;
  Install.Enabled := not visible;
  CloseBtn.Enabled := not visible;
  MapGroup.Enabled := not visible;
  ModeList.Enabled := not visible;
  Cancel.Enabled := visible;
end;


end.
