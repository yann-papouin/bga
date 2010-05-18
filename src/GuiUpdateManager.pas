unit GuiUpdateManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, xmldom, XMLIntf, msxmldom, XMLDoc, StdCtrls, Generics.Collections,
  SpTBXItem, SpTBXControls, ExtCtrls, pngimage, JvComponentBase, JvThread;

type

  TUpdateResult = (
    rs_None,
    rs_UpToDate,
    rs_UpdateFound,
    rs_NoUpdate,
    rs_NoInternet
  );

  TUpdateEntry = class
  private
  public
    Datetime : string;
    Id : string;
    Link : string;
    Title : string;
    Revision : integer;
  end;

  TUpdateReply = procedure(Sender: TObject; Result : TUpdateResult) of object;

  TUpdateEntryList = TObjectList<TUpdateEntry>;

  TUpdateManagerForm = class(TForm)
    Actions: TActionList;
    Check: TAction;
    Shape1: TShape;
    SpTBXLabel1: TLabel;
    Shape2: TShape;
    VersionLink: TSpTBXLabel;
    VersionDateTime: TSpTBXLabel;
    Shape3: TShape;
    Label1: TLabel;
    SpTBXButton1: TSpTBXButton;
    CloseForm: TAction;
    Image1: TImage;
    SpTBXLabel2: TSpTBXLabel;
    CheckThread: TJvThread;
    wget: TIdHTTP;
    procedure CheckExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CloseFormExecute(Sender: TObject);
    procedure CheckThreadExecute(Sender: TObject; Params: Pointer);
    procedure CheckThreadFinish(Sender: TObject);
  private
    FUpdateResult : TUpdateResult;
    FOnUpdateReply: TUpdateReply;
    { Déclarations privées }
    function UpdateProcess : TUpdateResult;
    procedure SetOnUpdateReply(const Value: TUpdateReply);
  public
    { Déclarations publiques }
    Entries : TUpdateEntryList;
    property OnUpdateReply : TUpdateReply read FOnUpdateReply write SetOnUpdateReply;
  end;

var
  UpdateManagerForm: TUpdateManagerForm;

implementation

{$R *.dfm}

uses
 SvnInfo, ActiveX, StringFunction, Resources;


procedure TUpdateManagerForm.CheckExecute(Sender: TObject);
begin
  CheckThread.Execute(nil);
end;


function TUpdateManagerForm.UpdateProcess : TUpdateResult;
var
  HtmlDoc : TXMLDocument;
  Feed, Entry : IXMLNode;
  UpdateEntry : TUpdateEntry;
  LatestEntry : TUpdateEntry;

  i: integer;
  VersionList : TStringList;
  DownloadLine : integer;
  URL, Revision : string;
begin
  HtmlDoc := TXMLDocument.Create(Self);

  Result := rs_NoUpdate;
  try
    HtmlDoc.XML.Text := FormatXMLData(wget.Get('http://code.google.com/feeds/p/bga/downloads/basic'));
    HtmlDoc.Active := true;
  except
    on e:exception do
      Result := rs_NoInternet;
  end;

  if Result <> rs_NoInternet then
  begin
    Feed := HtmlDoc.DocumentElement;
    Entry := Feed.ChildNodes.FindNode('entry');

    while Entry <> nil do
    begin
      try
        UpdateEntry := TUpdateEntry.Create;
        UpdateEntry.datetime := Entry.ChildNodes.FindNode('updated').Text;
        UpdateEntry.id := Entry.ChildNodes.FindNode('id').Text;
        UpdateEntry.link := Entry.ChildNodes.FindNode('link').Attributes['href'];
        UpdateEntry.title := Entry.ChildNodes.FindNode('title').Text;
        Entries.Add(UpdateEntry);
      finally
        Entry := Entry.NextSibling;
      end;
    end;
  end;

  HtmlDoc.Active := false;
  HtmlDoc.Free;

  LatestEntry := nil;
  for i:= 0 to Entries.Count-1 do
  begin
    UpdateEntry := Entries[i];
    Revision := SFRight('BGA_rev_', UpdateEntry.link);
    Revision := SFLeft('.zip', Revision);
    UpdateEntry.Revision := StrToIntDef(Revision,0);

    if UpdateEntry.Revision > SVN_REVISION then
    begin
      if LatestEntry = nil then
        LatestEntry := UpdateEntry
      else
      if UpdateEntry.Revision > LatestEntry.Revision then
        LatestEntry := UpdateEntry;
    end;
  end;

  if LatestEntry <> nil then
  begin
    VersionLink.Caption := trim(LatestEntry.title);
    VersionLink.LinkText := LatestEntry.link;
    VersionDateTime.Caption := LatestEntry.datetime;
    Result := rs_UpdateFound;
  end;
end;

procedure TUpdateManagerForm.CheckThreadExecute(Sender: TObject; Params: Pointer);
var
  TickCapture : cardinal;

  function ThreadCanceled :boolean;
  begin
    Result := CheckThread.Terminated or (csDestroying in ComponentState);
  end;

begin
  TickCapture := GetTickCount;
  repeat
    if GetTickCount-TickCapture>5000 then
      Break;
  until ThreadCanceled;

  if not ThreadCanceled then
  begin
    CoInitialize(nil);
    FUpdateResult := UpdateProcess;
    CoUninitialize;
  end;
end;

procedure TUpdateManagerForm.CheckThreadFinish(Sender: TObject);
begin
  if Assigned(FOnUpdateReply) then
    FOnUpdateReply(Self, FUpdateResult);
end;

procedure TUpdateManagerForm.CloseFormExecute(Sender: TObject);
begin
  Close;
end;

procedure TUpdateManagerForm.FormCreate(Sender: TObject);
begin
  Entries := TUpdateEntryList.Create;
end;

procedure TUpdateManagerForm.FormDestroy(Sender: TObject);
begin
  CheckThread.Terminate;
  CheckThread.WaitFor;
  Entries.Free;
end;

procedure TUpdateManagerForm.SetOnUpdateReply(const Value: TUpdateReply);
begin
  FOnUpdateReply := Value;
end;

end.
