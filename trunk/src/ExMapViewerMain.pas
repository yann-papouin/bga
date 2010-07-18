unit ExMapViewerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TExMapViewerMainForm = class(TForm)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    function GetFileByPath(Sender: TObject; const VirtualPath : string) : string;
  public
    { Déclarations publiques }
  end;

var
  ExMapViewerMainForm: TExMapViewerMainForm;

implementation

{$R *.dfm}

uses
  StringFunction, GuiRawView;

procedure TExMapViewerMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RAWViewForm.GetFileByPath := GetFileByPath;
  RAWViewForm.LoadTerrain('C:\Users\Yann\Documents\RAD Studio\Projets\BGA\src\_bin_bga\Init\Terrain.con');
  RAWViewForm.Show;
end;

function TExMapViewerMainForm.GetFileByPath(Sender: TObject; const VirtualPath: string): string;
begin
  if SFUniPos('heightmap.raw', VirtualPath) > 0 then
    Result := 'C:\Users\Yann\Documents\RAD Studio\Projets\BGA\src\_bin_bga\heightmap.raw'
  else
  begin
    Result := 'C:\Users\Yann\Documents\RAD Studio\Projets\BGA\src\_bin_bga\Textures\' + SFRightRight('\', VirtualPath);
  end;

end;

end.
