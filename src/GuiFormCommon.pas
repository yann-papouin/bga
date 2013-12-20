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
 * The Original Code is GuiFormCommon (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiFormCommon;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormCommon = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseEnter(Sender: TObject);
    procedure FormMouseLeave(Sender: TObject);
  private
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    { Déclarations privées }
  protected
    FOperationCount : integer;
    FApplicationTitle : string;
  public
    { Déclarations publiques }
    property Title : string read GetTitle write SetTitle;
    function OperationPending : boolean;
    procedure BeginOperation;
    procedure EndOperation;
  end;

var
  FormCommon: TFormCommon;

implementation

{$R *.dfm}

uses
  AppLib;

{ TFormCommon }

procedure TFormCommon.FormCreate(Sender: TObject);
begin
  DoubleBuffered := True;
  if DebugHook <> 0 then
    FApplicationTitle := Caption
  else
    FApplicationTitle := Caption + ' - ' + ApplicationSvnTitle;
end;

procedure TFormCommon.FormMouseEnter(Sender: TObject);
begin
  //Showmessage('FormMouseEnter');
end;

procedure TFormCommon.FormMouseLeave(Sender: TObject);
begin
  //Showmessage('FormMouseLeave');
end;

function TFormCommon.GetTitle: string;
begin
  Result := Caption;
end;

procedure TFormCommon.SetTitle(const Value: string);
begin
  if Value <> EmptyStr then
    Caption := Format('%s - %s',[ExtractFilename(Value), FApplicationTitle])
  else
    Caption := FApplicationTitle;
end;

procedure TFormCommon.BeginOperation;
begin
  Inc(FOperationCount);
end;

procedure TFormCommon.EndOperation;
begin
  Dec(FOperationCount);
  if FOperationCount < 0 then
    FOperationCount := 0;
end;

function TFormCommon.OperationPending: boolean;
begin
  Result := FOperationCount > 0;
end;

end.
