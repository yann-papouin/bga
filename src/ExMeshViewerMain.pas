unit ExMeshViewerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TExMeshViewerMainForm = class(TForm)
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
    function GetFileByPath(Sender: TObject; const VirtualPath : string) : string;
  public
    { Déclarations publiques }
  end;

var
  ExMeshViewerMainForm: TExMeshViewerMainForm;

implementation

{$R *.dfm}

uses
  GuiSMView;

procedure TExMeshViewerMainForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SMViewForm.GetFileByPath := GetFileByPath;
  SMViewForm.LoadMaterials(ExtractFilePath(Application.ExeName)+'\Ranger\Ranger_Hull_M1.rs');
  SMViewForm.LoadStandardMesh(ExtractFilePath(Application.ExeName)+'\Ranger\Ranger_Hull_M1.sm');
  SMViewForm.Preview;

end;

function TExMeshViewerMainForm.GetFileByPath(Sender: TObject; const VirtualPath: string): string;
begin
  Result := ExtractFilePath(Application.ExeName)+ '\Ranger\Texture\manhunt.dds';
end;

end.
