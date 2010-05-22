unit GuiFSView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GuiRFACommon, ActnList, SpTBXControls, StdCtrls, SpTBXEditors, SpTBXItem, VirtualTrees,
  ExtCtrls, TB2Item, TB2Dock, TB2Toolbar, FSLib;

type


  TFSViewForm = class(TRFACommonForm)
    Panel2: TPanel;
    SpTBXButton2: TSpTBXButton;
    Settings: TAction;
    FilesystemChoice: TSpTBXComboBox;
    SpTBXLabel2: TSpTBXLabel;
    TopDock: TSpTBXDock;
    tbMenuBar: TSpTBXToolbar;
    mFile: TSpTBXSubmenuItem;
    SpTBXItem14: TSpTBXItem;
    SpTBXItem2: TSpTBXItem;
    mEdit: TSpTBXSubmenuItem;
    SpTBXItem23: TSpTBXItem;
    Add: TAction;
    Import: TAction;
    Update: TAction;
    SpTBXItem1: TSpTBXItem;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    procedure SettingsExecute(Sender: TObject);
    procedure AddExecute(Sender: TObject);
    procedure ImportExecute(Sender: TObject);
    procedure UpdateExecute(Sender: TObject);
    procedure FilesystemChoiceChange(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  FSViewForm: TFSViewForm;

implementation

{$R *.dfm}

uses
  GuiFSSettings, Resources,  IOUtils, Types;

procedure TFSViewForm.AddExecute(Sender: TObject);
begin
  FSSettingsForm.OpenMode := omAdd;
  if FSSettingsForm.ShowModal = mrOk then
  begin

  end;
end;

procedure TFSViewForm.ImportExecute(Sender: TObject);
begin
//
end;

procedure TFSViewForm.UpdateExecute(Sender: TObject);
begin
//
end;


procedure TFSViewForm.FilesystemChoiceChange(Sender: TObject);
begin
  inherited;
  Settings.Enabled := FileExists(FilesystemChoice.Text);
end;


procedure TFSViewForm.SettingsExecute(Sender: TObject);
begin
  FSSettingsForm.OpenMode := omEdit;
  if FSSettingsForm.ShowModal = mrOk then
  begin

  end;
end;



end.
