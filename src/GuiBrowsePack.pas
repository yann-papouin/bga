unit GuiBrowsePack;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GuiBrowse, ActnList, JvComponentBase, JvFormPlacement, JvBaseDlg,
  JvBrowseFolder, SpTBXControls, SpTBXItem, ExtCtrls, StdCtrls, SpTBXEditors;

type
  TBrowsePackForm = class(TBrowseForm)
    Base: TSpTBXEdit;
    UseBasePath: TSpTBXCheckBox;
    procedure UseBasePathClick(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  BrowsePackForm: TBrowsePackForm;

implementation

{$R *.dfm}

procedure TBrowsePackForm.UseBasePathClick(Sender: TObject);
begin
  inherited;
  Base.Enabled := UseBasePath.Checked;
end;

end.
