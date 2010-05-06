unit GuiRFASettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ValEdit, SpTBXItem, SpTBXControls;

type
  TRFASettingsForm = class(TForm)
    SpTBXGroupBox1: TSpTBXGroupBox;
    ValueListEditor1: TValueListEditor;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
    function GetProgramByExt(Ext : string) : string;
  end;

var
  RFASettingsForm: TRFASettingsForm;

implementation

{$R *.dfm}

{ TRFASettingsForm }

function TRFASettingsForm.GetProgramByExt(Ext: string): string;
begin
  result := EmptyStr;
end;

end.
