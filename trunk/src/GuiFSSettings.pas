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
 * The Original Code is GuiFSSettings (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiFSSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SpTBXEditors, SpTBXItem, SpTBXControls, ActnList, ExtCtrls,
  FSLib;

type
  TOpenMode = (
    omAdd,
    omEdit
  );

  TFSSettingsForm = class(TForm)
    SpTBXLabel1: TSpTBXLabel;
    BattlefieldDir: TSpTBXButtonEdit;
    FilesystemName: TSpTBXEdit;
    SpTBXLabel2: TSpTBXLabel;
    Mods: TSpTBXListBox;
    SpTBXLabel3: TSpTBXLabel;
    SpTBXLabel4: TSpTBXLabel;
    ModPath: TSpTBXListBox;
    Actions: TActionList;
    AutoFill: TAction;
    Footer: TPanel;
    ButtonOk: TSpTBXButton;
    ButtonCancel: TSpTBXButton;
    Ok: TAction;
    Cancel: TAction;
    procedure OkExecute(Sender: TObject);
    procedure CancelExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BattlefieldDirChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ModsClick(Sender: TObject);
  private
    ModEntries : TModEntryList;
    function SearchBattleField: boolean;
    { Déclarations privées }
  public
    { Déclarations publiques }
    OpenMode : TOpenMode;
  end;

var
  FSSettingsForm: TFSSettingsForm;

implementation

{$R *.dfm}

uses
  GuiSkinDialog, CommonLib, Resources, Registry, IOUtils, Types;


function TFSSettingsForm.SearchBattleField : boolean;
var
  Reg:TRegistry;
  FileList : TStringList;
  i :integer;
begin
  Result := false;

  Reg:= TRegistry.Create(KEY_READ);
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.KeyExists('SOFTWARE\EA GAMES\Battlefield 1942\') then
  begin
    Reg.OpenKey('SOFTWARE\EA GAMES\Battlefield 1942\', false);
    BattlefieldDir.Text := Reg.ReadString('GAMEDIR');
    Reg.CloseKey;
  end;
  Reg.Free;

  if DirectoryExists(BattlefieldDir.Text) then
    result := true;
end;

procedure TFSSettingsForm.FormCreate(Sender: TObject);
begin
  ModEntries := TModEntryList.Create;
end;

procedure TFSSettingsForm.FormDestroy(Sender: TObject);
begin
  ModEntries.Free;
end;

procedure TFSSettingsForm.FormShow(Sender: TObject);
begin
  SearchBattleField;
end;


procedure TFSSettingsForm.BattlefieldDirChange(Sender: TObject);
var
  ModDir, InitFile : string;
  Dirs : TStringDynArray;
  i : integer;
  ModEntry : TBattlefieldModEntry;
begin
  ModEntries.Clear;
  Mods.Clear;
  ModPath.Clear;
  ModDir := IncludeTrailingBackslash(BattlefieldDir.Text) +'Mods';
  if DirectoryExists(ModDir) then
  begin
    Dirs := TDirectory.GetDirectories(IncludeTrailingBackslash(ModDir));

    for i := 0 to Length(Dirs) - 1 do
    begin
      InitFile := IncludeTrailingBackslash(Dirs[i])+'init.con';
      if FileExists(InitFile) then
      begin
        ModEntry := TBattlefieldModEntry.Create(nil);
        ModEntry.LoadFromConFile(InitFile);
        Mods.AddItem(ModEntry.GameName, ModEntry);
        ModEntries.Add(ModEntry);
      end;
    end;
  end;
end;

procedure TFSSettingsForm.ModsClick(Sender: TObject);
var
  ModEntry : TBattlefieldModEntry;
begin
  if Mods.ItemIndex <> -1 then
  begin
    ModEntry := Mods.Items.Objects[Mods.ItemIndex] as TBattlefieldModEntry;
    ModPath.Items.Assign(ModEntry.PathList);
  end;
end;



procedure TFSSettingsForm.OkExecute(Sender: TObject);
begin
  if ValidFilename(FilesystemName.Text) then
  begin
    if DirectoryExists(BattlefieldDir.Text) then
    begin
      if (OpenMode = omAdd) and FileExists(IncludeTrailingBackslash(BattlefieldDir.Text)+FilesystemName.Text)  then
        ShowError('Already exists', 'This file system name already exists, please choose a unique name')
      else
      begin
        // File system data will be stored in name.fsdata
        // File system settings will be stored in name.fsheader
        ModalResult := mrOk;
      end
    end
      else
    ShowError('Battlefield directory invalid', 'The directory must exists');
  end
    else
  ShowError('Filesystem Name invalid', 'The name must be a valid file name');

end;


procedure TFSSettingsForm.CancelExecute(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
