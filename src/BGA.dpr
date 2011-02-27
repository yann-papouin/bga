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
 * The Original Code is BGA (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

program BGA;

uses
  ExceptionLog,
  Forms,
  Windows,
  DbugIntf,
  GuiRFASettings in 'GuiRFASettings.pas' { RFASettingsForm },
  AppLib in 'Lib\AppLib.pas',
  CommonLib in 'Lib\CommonLib.pas',
  MD5Api in 'Lib\MD5Api.pas',
  MD5Core in 'Lib\MD5Core.pas',
  MiniLZO in 'Lib\MiniLZO.pas',
  RFALib in 'RFALib.pas',
  StringFunction in 'StringFunction.pas',
  GuiMapView in 'GuiMapView.pas' {MapViewForm},
  CONLib in 'CONLib.pas',
  GuiAbout in 'GuiAbout.pas' { AboutForm },
  GuiSMView in 'GuiSMView.pas' { SMViewForm },
  FileSM in 'GLSceneExt\FileSM.pas',
  GLFileSM in 'GLSceneExt\GLFileSM.pas',
  TypesSM in 'GLSceneExt\TypesSM.pas',
  SvnInfo in 'Lib\SvnInfo.pas',
  GuiUpdateManager in 'GuiUpdateManager.pas' { UpdateManagerForm },
  GuiBrowse in 'GuiBrowse.pas' { BrowseForm },
  GuiSkinDialog in 'GuiSkinDialog.pas' { SkinDialogForm },
  GuiBrowsePack in 'GuiBrowsePack.pas' { BrowsePackForm },
  GuiBrowseExtract in 'GuiBrowseExtract.pas' { BrowseExtractForm },
  GuiRFACommon in 'GuiRFACommon.pas' { RFACommonForm },
  Resources in 'Resources.pas' { ResourcesForm },
  GuiRFAView in 'GuiRFAView.pas' { RFAViewForm },
  GuiFSSettings in 'GuiFSSettings.pas' {FSSettingsForm},
  GuiFSEdit in 'GuiFSEdit.pas' {FSEditForm},
  FSLib in 'FSLib.pas',
  VirtualTreeviewTheme in 'Lib\VirtualTreeviewTheme.pas',
  GuiFormCommon in 'GuiFormCommon.pas' { FormCommon },
  GuiPicView in 'GuiPicView.pas' { PICViewForm },
  UAC in 'UAC.pas',
  BGALib in 'BGALib.pas',
  RSLib in 'RSLib.pas',
  FileTM in 'GLSceneExt\FileTM.pas',
  TypesTM in 'GLSceneExt\TypesTM.pas',
  GuiWait in 'GuiWait.pas' {WaitForm},
  oge2_HashList in '..\misc\TerrainRenderer\lib\oge2_HashList.pas',
  oge2_HeightMap in '..\misc\TerrainRenderer\lib\oge2_HeightMap.pas',
  oge2_TerrainRendering in '..\misc\TerrainRenderer\lib\oge2_TerrainRendering.pas',
  oge2_TerrainTileLodRenderer in '..\misc\TerrainRenderer\lib\oge2_TerrainTileLodRenderer.pas',
  oge2_TerrainTileLodVBORenderer in '..\misc\TerrainRenderer\lib\oge2_TerrainTileLodVBORenderer.pas',
  oge2_TerrainTileRender in '..\misc\TerrainRenderer\lib\oge2_TerrainTileRender.pas',
  GuiFSView in 'GuiFSView.pas' {FSViewForm};

{$R *.res}

{$I BGA.inc}

{$IfDef GLS_LOGGING}

{$EndIf}
var
  Bench : integer;

begin
  // ReportMemoryLeaksOnShutdown := DebugHook <> 0;

  if UAC.Perform then
  begin

  end
  else
  begin
    // Save current time in ms for a future diff
    Bench := GetTickCount;

    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.Title := 'BGA : Battlefield 1942 Game Archive Tool';

    Application.CreateForm(TRFAViewForm, RFAViewForm);
    Application.CreateForm(TRFASettingsForm, RFASettingsForm);
    Application.CreateForm(TUpdateManagerForm, UpdateManagerForm);
    Application.CreateForm(TFSSettingsForm, FSSettingsForm);
    Application.CreateForm(TFSViewForm, FSViewForm);
    Application.CreateForm(TWaitForm, WaitForm);

    // Show bench results of entries reading
    Bench := GetTickCount - Bench;
    SendDebugFmt('Application loaded in %dms',[Bench]);

    Application.Run;
  end;

end.
