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
 * The Original Code is GuiWait (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiWait;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, GR32_Image, SpTBXItem, SpTBXControls, StdCtrls, ComCtrls, JvTimer, AsyncTimer;

type
  TWaitForm = class(TForm)
    LabelText: TLabel;
    Panel: TSpTBXPanel;
    AsyncTimer1: TAsyncTimer;
    WaitBar: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    { Déclarations privées }
    FWindowList : TTaskWindowList;
    FWaitCounter : integer;
    FApplicationModal: boolean;
    procedure SetApplicationModal(const Value: boolean);
  public
    { Déclarations publiques }
    procedure BeginWait;
    procedure EndWait;
    procedure IncProgress(Text : string = ''; MaxStep : Integer = 0);
    property ApplicationModal : boolean read FApplicationModal write SetApplicationModal;
  end;

var
  WaitForm: TWaitForm;

implementation

{$R *.dfm}

uses
  DbugIntf, Dwmapi;


procedure TWaitForm.FormCreate(Sender: TObject);
var
  DiffHeight : integer;
begin

  if DwmCompositionEnabled then
  begin
    GlassFrame.Enabled := true;
    LabelText.GlowSize := 8;
  end
    else
  begin
    //GlassFrame.Enabled := false;
    DiffHeight := abs(Height - ClientHeight);
    BorderStyle := bsNone;
    Height := Height - DiffHeight;
    Panel.Align := alClient;
    Panel.Visible := true;
    LabelText.Parent := Panel;
  end;

end;

procedure TWaitForm.FormHide(Sender: TObject);
begin
  EnableTaskWindows(FWindowList);
end;

procedure TWaitForm.FormShow(Sender: TObject);
begin
  LabelText.Caption := 'BGA : Loading ...';
  WaitBar.Position := 0;
  FWindowList := DisableTaskWindows(0);
end;

procedure TWaitForm.IncProgress(Text : string = ''; MaxStep : Integer = 0);
begin
  if MaxStep > 0 then
   WaitBar.Max := MaxStep;

  if Text <> EmptyStr then
    LabelText.Caption := Text;

  if WaitBar.Position+1 > WaitBar.Max then
    WaitBar.Max := WaitBar.Max +1;

  WaitBar.Position := WaitBar.Position + 1;
  Application.ProcessMessages;
end;

procedure TWaitForm.BeginWait;
begin
  Inc(FWaitCounter);
  if FWaitCounter = 1 then
  begin
    if Application.ModalLevel = 0 then
    begin
      Show;
      Application.ProcessMessages;
    end;
  end;
end;

procedure TWaitForm.EndWait;
begin
  Dec(FWaitCounter);
  if FWaitCounter <= 0 then
  begin
    if DebugHook <> 0 then
      SendDebugFmt('Max value should be %d',[WaitBar.Position]);

    WaitBar.Position := WaitBar.Max;
    FWaitCounter := 0;
    Hide;
  end;
end;



procedure TWaitForm.SetApplicationModal(const Value: boolean);
begin
  FApplicationModal := Value;
  if FWaitCounter > 0 then
  begin
    Visible := not FApplicationModal;
  end;
end;

end.
