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
 * The Original Code is GuiUpdateManager (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiUpdateManager;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ActnList, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdHTTP, xmldom, XMLIntf, msxmldom, XMLDoc, StdCtrls, Generics.Collections,
  SpTBXItem, SpTBXControls, ExtCtrls, pngimage, JvComponentBase, JvThread, JvExControls, JvGradient;

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
    SpTBXLabel1: TLabel;
    Shape2: TShape;
    VersionLink: TSpTBXLabel;
    VersionDateTime: TSpTBXLabel;
    Label1: TLabel;
    SpTBXButton1: TSpTBXButton;
    CloseForm: TAction;
    Image1: TImage;
    SpTBXLabel2: TSpTBXLabel;
    CheckThread: TJvThread;
    wget: TIdHTTP;
    Shape3: TShape;
    Shape4: TShape;
    JvGradient1: TJvGradient;
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
 SvnInfo, ActiveX, StringFunction, Resources, CommonLib;


procedure TUpdateManagerForm.CheckExecute(Sender: TObject);
begin
  CheckThread.Execute(nil)
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
  SourceCode : string;
begin
  HtmlDoc := TXMLDocument.Create(Self);

  Result := rs_NoUpdate;
  try
    SourceCode := wget.Get('http://code.google.com/feeds/p/bga/downloads/basic');
    HtmlDoc.XML.Text := FormatXMLData(SourceCode);
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
