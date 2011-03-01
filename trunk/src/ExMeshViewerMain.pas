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
  * The Original Code is ExMeshViewerMain (http://code.google.com/p/bga)
  *
  * The Initial Developer of the Original Code is
  * Yann Papouin <yann.papouin at @ gmail.com>
  *
  * ***** END LICENSE BLOCK ***** *)

unit ExMeshViewerMain;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs, JvAppInst;

type
  TExMeshViewerMainForm = class(TForm)
    AppInstances: TJvAppInstances;
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
    function GetFileByPath(Sender: TObject; const VirtualPath: string): string;
  public
    { Déclarations publiques }
    procedure Launch;
  end;

var
  ExMeshViewerMainForm: TExMeshViewerMainForm;

implementation

{$R *.dfm}

uses
  GuiSMView;

procedure TExMeshViewerMainForm.FormShow(Sender: TObject);
begin
  Launch;
end;

procedure TExMeshViewerMainForm.Launch;
begin
  SMViewForm.GetFileByPath := GetFileByPath;
  SMViewForm.LoadMaterials(ExtractFilePath(Application.ExeName) + 'Ranger\Ranger_Hull_M1.rs');
  SMViewForm.LoadStandardMesh(ExtractFilePath(Application.ExeName) + 'Ranger\Ranger_Hull_M1.sm');
  SMViewForm.Preview;
end;

function TExMeshViewerMainForm.GetFileByPath(Sender: TObject; const VirtualPath: string): string;
begin
  Result := ExtractFilePath(Application.ExeName) + 'Ranger\Texture\manhunt.dds';
end;

end.
