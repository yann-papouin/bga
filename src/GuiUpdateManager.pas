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
  end;


  TUpdateReply = procedure(Sender: TObject; Result : TUpdateResult) of object;

  TUpdateManagerForm = class(TForm)
    Actions: TActionList;
    wget: TIdHTTP;
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
    Entries : TObjectList<TUpdateEntry>;
    property OnUpdateReply : TUpdateReply read FOnUpdateReply write SetOnUpdateReply;
  end;

var
  UpdateManagerForm: TUpdateManagerForm;

implementation

{$R *.dfm}

uses
 SvnInfo, ActiveX, StringFunction;


procedure TUpdateManagerForm.CheckExecute(Sender: TObject);
begin
  CheckThread.Execute(nil);
end;


function TUpdateManagerForm.UpdateProcess : TUpdateResult;
var
  HtmlDoc : TXMLDocument;
  Feed, Entry : IXMLNode;
  UpdateEntry : TUpdateEntry;

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

  for i:= Entries.Count-1 to 0 do
  begin
    UpdateEntry := Entries[i];
    Revision := SFRight('BGA_rev_', UpdateEntry.link);
    Revision := SFLeft('.zip', Revision);

    if StrToIntDef(Revision,0) > SVN_REVISION then
    begin
      VersionLink.Caption := trim(UpdateEntry.title);
      VersionLink.LinkText := UpdateEntry.link;
      VersionDateTime.Caption := UpdateEntry.datetime;
      Result := rs_UpdateFound;
      Break;
    end;
  end;

end;

procedure TUpdateManagerForm.CheckThreadExecute(Sender: TObject; Params: Pointer);
var
  TickCapture : cardinal;
begin
  TickCapture := GetTickCount;
  repeat
    if GetTickCount-TickCapture>5000 then
      Break;
  until (csDestroying in ComponentState);

  if not (csDestroying in ComponentState) then
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
  Entries := TObjectList<TUpdateEntry>.Create;
end;

procedure TUpdateManagerForm.FormDestroy(Sender: TObject);
begin
  Entries.Free;
end;

procedure TUpdateManagerForm.SetOnUpdateReply(const Value: TUpdateReply);
begin
  FOnUpdateReply := Value;
end;

end.
