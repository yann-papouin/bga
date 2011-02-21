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
 * The Original Code is ExMapViewerMain (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit ExMapViewerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TExMapViewerMainForm = class(TForm)
    procedure FormShow(Sender: TObject);
  private
    { Déclarations privées }
    function GetFileByPath(Sender: TObject; const VirtualPath : string) : string;
  public
    { Déclarations publiques }
    procedure Launch;
  end;

var
  ExMapViewerMainForm: TExMapViewerMainForm;

implementation

{$R *.dfm}

uses
  StringFunction, GuiMapView;


procedure TExMapViewerMainForm.FormShow(Sender: TObject);
begin
  Launch;
end;

procedure TExMapViewerMainForm.Launch;
begin
  MapViewForm.GetFileByPath := GetFileByPath;
  MapViewForm.LoadTerrain(ExtractFilePath(Application.ExeName)+'Init\Terrain.con');
  MapViewForm.ShowModal;
  Close;
end;

function TExMapViewerMainForm.GetFileByPath(Sender: TObject; const VirtualPath: string): string;
begin
  if SFUniPos('heightmap.raw', VirtualPath) > 0 then
    Result := ExtractFilePath(Application.ExeName)+'heightmap.raw'
  else
  begin
    Result := ExtractFilePath(Application.ExeName)+ 'Textures\' + SFRightRight('\', VirtualPath);
  end;

end;



end.
