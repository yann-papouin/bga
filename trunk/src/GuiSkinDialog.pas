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
 * The Original Code is GuiSkinDialog (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiSkinDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SpTBXItem, SpTBXControls, ExtCtrls, ImgList, PngImageList, ActnList, StdCtrls;

type
  TSkinDialogForm = class(TForm)
    Footer: TPanel;
    Header: TPanel;
    Images: TPngImageList;
    SpTBXButton1: TSpTBXButton;
    Title: TSpTBXLabel;
    Panel1: TPanel;
    Text: TLabel;
    SkinForm: TSpTBXTitleBar;
    Actions: TActionList;
    Yes: TAction;
    No: TAction;
    Ok: TAction;
    Cancel: TAction;
    Abort: TAction;
    Retry: TAction;
    Ignore: TAction;
    All: TAction;
    NoToAll: TAction;
    YesToAll: TAction;
    Help: TAction;
    SpTBXButton2: TSpTBXButton;
    SpTBXButton3: TSpTBXButton;
    SpTBXButton4: TSpTBXButton;
    SpTBXButton5: TSpTBXButton;
    SpTBXButton6: TSpTBXButton;
    SpTBXButton7: TSpTBXButton;
    SpTBXButton8: TSpTBXButton;
    SpTBXButton9: TSpTBXButton;
    SpTBXButton10: TSpTBXButton;
    SpTBXButton11: TSpTBXButton;
    procedure OkExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure YesExecute(Sender: TObject);
    procedure NoExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure AbortExecute(Sender: TObject);
    procedure RetryExecute(Sender: TObject);
    procedure IgnoreExecute(Sender: TObject);
    procedure AllExecute(Sender: TObject);
    procedure NoToAllExecute(Sender: TObject);
    procedure YesToAllExecute(Sender: TObject);
    procedure HelpExecute(Sender: TObject);
    procedure FooterClick(Sender: TObject);
  private
    { Déclarations privées }
    FButtons: TMsgDlgButtons;
    FDefaultButton: TMsgDlgBtn;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    procedure SetButtons(const Value: TMsgDlgButtons);
    procedure SetDefaultButton(const Value: TMsgDlgBtn);
  protected
    procedure CalcSize;
    procedure SelectDefault;
  public
    { Déclarations publiques }
    property Caption : string read GetCaption write SetCaption;
    property Buttons: TMsgDlgButtons read FButtons write SetButtons;
    property DefaultButton: TMsgDlgBtn read FDefaultButton write SetDefaultButton;
  end;

  procedure ShowCopiedToClipboard(Title : string);

  function ShowIdeWarning(Msg : string; AdvMsg : string = '') : TModalResult;
  function ShowIdeError(Msg : string; AdvMsg : string = '') : TModalResult;
  function ShowIdeMessage(Msg : string; AdvMsg : string = '') : TModalResult;

  function ShowWarning(Msg : string; AdvMsg : string = '') : TModalResult;
  function ShowError(Msg : string; AdvMsg : string = '') : TModalResult;
  function ShowMessage(Msg : string; AdvMsg : string = '') : TModalResult;

  function ShowDialog(Msg : string; AdvMsg : string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint) : TModalResult;

  procedure PlayBeep(ActionType: TMsgDlgType);
  
var
  SkinDialogForm: TSkinDialogForm;

implementation

uses
  DbugIntf, Math, Clipbrd;

const
  MIN_WIDTH = 350;

{$R *.dfm}

procedure PlayBeep(ActionType: TMsgDlgType);
var
  mb: DWord;
begin
  case ActionType of
    mtInformation: mb := MB_ICONASTERISK; //SystemAsterisk
    mtWarning: mb := MB_ICONEXCLAMATION; //SystemExclamation
    mtError: mb := MB_ICONHAND; //SystemHand
    mtConfirmation: mb := MB_ICONQUESTION; //SystemQuestion
    mtCustom: mb := MB_OK; //SystemDefault
  else
    mb:= $0FFFFFFFF; //Standard beep using the computer speaker
  end;
  MessageBeep(mb);
end;


function ShowDialog(Msg : string; AdvMsg : string; DlgType: TMsgDlgType;
  Buttons: TMsgDlgButtons; DefaultButton: TMsgDlgBtn; HelpCtx: Longint) : TModalResult;
var
  SkinDialog: TSkinDialogForm;
begin
  SkinDialog:= TSkinDialogForm.Create(Application);

  case DlgType of
    mtWarning:
    begin
      SkinDialog.Caption := 'Warning';
      SkinDialog.Title.ImageIndex := 2;
    end;
    mtError:
    begin
      SkinDialog.Caption := 'Error';
      SkinDialog.Title.ImageIndex := 1;
    end;
    mtInformation:
    begin
      SkinDialog.Caption := 'Information';
      SkinDialog.Title.ImageIndex := 5;
    end;
    mtConfirmation:
    begin
      SkinDialog.Caption := 'Confirmation';
      SkinDialog.Title.ImageIndex := 4;
    end;
  end;

  SkinDialog.Buttons := Buttons;
  SkinDialog.DefaultButton := DefaultButton;
  SkinDialog.HelpContext := HelpCtx;

  SkinDialog.Title.Caption := Msg;
  SkinDialog.Text.Caption := AdvMsg;

  PlayBeep(DlgType);
  Result := SkinDialog.ShowModal;

  SkinDialog.Free;
end;


function ShowEx(Msg : string; AdvMsg : string = ''; DlgType: TMsgDlgType = mtInformation) : TModalResult;
begin
  Result := ShowDialog(Msg, AdvMsg, DlgType, [mbOK], mbOk, 0);
end;


function ShowWarning(Msg : string; AdvMsg : string = '') : TModalResult;
begin
  Result := ShowEx(Msg, AdvMsg, mtWarning);
end;

function ShowError(Msg : string; AdvMsg : string = '') : TModalResult;
begin
  Result := ShowEx(Msg, AdvMsg, mtError);
end;

function ShowMessage(Msg : string; AdvMsg : string = '') : TModalResult;
begin
  Result := ShowEx(Msg, AdvMsg, mtInformation);
end;


function ShowIdeWarning(Msg : string; AdvMsg : string = '') : TModalResult;
begin
  if DebugHook <> 0 then
  begin
    ShowWarning(Msg, AdvMsg);
  end
    else
  SendDebugWarning(AdvMsg);
end;

function ShowIdeError(Msg : string; AdvMsg : string = '') : TModalResult;
begin
  if DebugHook <> 0 then
  begin
    ShowError(Msg, AdvMsg);
  end
    else
  SendDebugError(AdvMsg);
end;

function ShowIdeMessage(Msg : string; AdvMsg : string = '') : TModalResult;
begin
  if DebugHook <> 0 then
  begin
    ShowMessage(Msg, AdvMsg);
  end
    else
  SendDebug(AdvMsg);
end;

procedure ShowCopiedToClipboard(Title : string);
var
  Msg : string;
begin
  Msg := Clipboard.AsText + #13+#10+ #13+#10 + '(Copied to clipboard, use CTRL+V to paste)';
  ShowMessage(Title, Msg);
end;


procedure TSkinDialogForm.FooterClick(Sender: TObject);
begin
  CalcSize;
end;

procedure TSkinDialogForm.FormCreate(Sender: TObject);
begin
  Title.ImageIndex := -1;
  Title.Caption := EmptyStr;
  Text.Caption := EmptyStr;
  //MessageDlg('', mtInformation, [mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll], 0);
  //Buttons: TMsgDlgButtons

  if Assigned(Application.MainForm) then
    Font.Assign(Application.MainForm.Font);
end;

procedure TSkinDialogForm.FormShow(Sender: TObject);
begin
  CalcSize;
  SelectDefault;
end;

procedure TSkinDialogForm.CalcSize;
var
  i, Calcwidth : integer;
begin
  //Beep;
  Calcwidth := 0;

  for i := 0 to Footer.ControlCount - 1 do
    if Footer.Controls[i].Visible then
    Calcwidth := Calcwidth + Footer.Controls[i].Width + Footer.Controls[i].Margins.Left + Footer.Controls[i].Margins.Right;

  Calcwidth := Calcwidth + Footer.Padding.Left;
  Calcwidth := Calcwidth + Footer.Padding.Right;    
  //SendInteger('Calcwidth', Calcwidth);
  Calcwidth := Max(Calcwidth + 60, MIN_WIDTH);

  Width := Calcwidth;
end;


procedure TSkinDialogForm.SelectDefault;
var
  i : integer;
begin

  for i := 0 to Footer.ControlCount - 1 do
    if ((Footer.Controls[i].Action = Yes) and (DefaultButton = mbYes))
    or ((Footer.Controls[i].Action = No) and (DefaultButton = mbNo))
    or ((Footer.Controls[i].Action = Ok) and (DefaultButton = mbOK))
    or ((Footer.Controls[i].Action = Cancel) and (DefaultButton = mbCancel))
    or ((Footer.Controls[i].Action = Abort) and (DefaultButton = mbAbort))
    or ((Footer.Controls[i].Action = Retry) and (DefaultButton = mbRetry))
    or ((Footer.Controls[i].Action = Ignore) and (DefaultButton = mbIgnore))
    or ((Footer.Controls[i].Action = NoToAll) and (DefaultButton = mbNoToAll))
    or ((Footer.Controls[i].Action = YesToAll) and (DefaultButton = mbYesToAll))
    or ((Footer.Controls[i].Action = Help) and (DefaultButton = mbHelp))
     then
      ActiveControl := Footer.Controls[i] as TWinControl;

end;

{$REGION 'GET/SET'}

function TSkinDialogForm.GetCaption: string;
begin
  result := inherited Caption;
end;


procedure TSkinDialogForm.SetButtons(const Value: TMsgDlgButtons);
begin
  FButtons := Value;

  Yes.Visible     := mbYes in FButtons;
  No.Visible      := mbNo in FButtons;
  Ok.Visible      := mbOK in FButtons;
  Cancel.Visible  := mbCancel in FButtons;
  Abort.Visible   := mbAbort in FButtons;
  Retry.Visible   := mbRetry in FButtons;
  Ignore.Visible  := mbIgnore in FButtons;
  All.Visible     := mbAll in FButtons;
  NoToAll.Visible := mbNoToAll in FButtons;
  YesToAll.Visible:= mbYesToAll in FButtons;
  Help.Visible    := mbHelp in FButtons;
end;

procedure TSkinDialogForm.SetCaption(const Value: string);
begin
  inherited Caption := Value;
  SkinForm.Caption := Caption;
end;


procedure TSkinDialogForm.SetDefaultButton(const Value: TMsgDlgBtn);
begin
  FDefaultButton := Value;
end;

{$ENDREGION}

{$REGION 'MODAL_RESULTS'}

procedure TSkinDialogForm.YesExecute(Sender: TObject);
begin
  ModalResult := mrYes;
end;

procedure TSkinDialogForm.NoExecute(Sender: TObject);
begin
  ModalResult := mrNo;
end;


procedure TSkinDialogForm.OkExecute(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TSkinDialogForm.CancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TSkinDialogForm.AbortExecute(Sender: TObject);
begin
  ModalResult := mrAbort;
end;

procedure TSkinDialogForm.RetryExecute(Sender: TObject);
begin
  ModalResult := mrRetry;
end;

procedure TSkinDialogForm.IgnoreExecute(Sender: TObject);
begin
  ModalResult := mrIgnore;
end;

procedure TSkinDialogForm.AllExecute(Sender: TObject);
begin
  ModalResult := mrAll;
end;

procedure TSkinDialogForm.NoToAllExecute(Sender: TObject);
begin
  ModalResult := mrNoToAll;
end;

procedure TSkinDialogForm.YesToAllExecute(Sender: TObject);
begin
  ModalResult := mrYesToAll;
end;

procedure TSkinDialogForm.HelpExecute(Sender: TObject);
begin
  Application.HelpSystem.ShowContextHelp(HelpContext , Application.HelpFile);
end;

{$ENDREGION}

end.
