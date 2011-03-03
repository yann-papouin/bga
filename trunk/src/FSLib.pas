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
 * The Original Code is FSLib (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit FSLib;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Generics.Collections;

type

  TBattlefieldFileSystem = class;

  TArchiveEntry = class
  private
    FMD5: string;
    FAbsolutePath: string;
    FRelativePath: string;
  public
    property RelativePath : string read FRelativePath;
    property AbsolutePath : string read FAbsolutePath;
    property MD5 : string read FMD5;
  end;

  TBattlefieldModEntry = class
  private
    FFilesystem: TBattlefieldFileSystem;
  public
    GameURL: string;
    GameVersion: string;
    GameName: string;
    AbsolutePath: string;
    PathList : TStringList;
    Archives : TObjectList<TArchiveEntry>;

    constructor Create(AFilesystem: TBattlefieldFileSystem);
    destructor Destroy; override;
    procedure LoadFromConFile(Filename : string);

    property Filesystem : TBattlefieldFileSystem read FFilesystem;
  end;

  TModEntryList = TObjectList<TBattlefieldModEntry>;

  TBattlefieldFileSystem = class
  private
    FVersion: word;
    FName: string;
    FBattlefieldPath: string;
  public
    ActiveMod: TBattlefieldModEntry;
    constructor Create;
    destructor Destroy; override;
    property Version : word read FVersion;
    property Name : string read FName;
    property BattlefieldPath : string read FBattlefieldPath;
  end;

  function FsAbsToRel(Path: String):string;
  function FsRelToAbs(Path: String):string;

  function ExtractUnixFilePath(const FileName: string): string;
  function ExtractUnixFileName(const FileName: string): string;
  function ExtractUnixFileExt(const FileName: string): string;

var
  Current_battlefield_path : string;
  //Current_mod_path         : string;
  //Current_archive_path     : string;

const
  UNIX_PATH_DELIM = '/';

  BATTLEFIELD_PATH       = 'bf://';   ///  C:\Program files\EA Games\BF1942\ -> bf://
  //MOD_PATH               = 'mod://';
  ARCHIVE_PATH           = 'archive://';

  MOD_DIRECTORY_NAME     = 'Mods';
  ARCHIVE_DIRECTORY_NAME = 'Archives';


implementation

uses
  CONLib, StringFunction;



function FsAbsToRel(Path: String):string;
begin
  Assert(AnsiPos('/', Path)=0);
(*
  if AnsiPos(Current_archive_path, Path) > 0 then
  begin
    Assert(Current_archive_path <> EmptyStr, 'Current_archive_path not set');
    Result := StringReplace(Path, Current_archive_path, ARCHIVE_PATH, [rfReplaceAll]);
  end
  else
  if AnsiPos(Current_mod_path, Path) > 0 then
  begin
    Assert(Current_mod_path <> EmptyStr, 'Current_mod_path not set');
    Result := StringReplace(Path, Current_mod_path, MOD_PATH, [rfReplaceAll])
  end
  else
*)
  if AnsiPos(Current_battlefield_path, Path) > 0 then
  begin
    Assert(Current_battlefield_path <> EmptyStr, 'Current_battlefield_path not set');
    Result := StringReplace(Path, Current_battlefield_path, BATTLEFIELD_PATH, [rfReplaceAll]);
  end;

  Result := StringReplace(Result,'\','/',[rfReplaceAll]);
end;


function FsRelToAbs(Path: String):string;
begin
  Assert(AnsiPos('\', Path)=0);
(*
  if AnsiPos(ARCHIVE_PATH, Path) > 0 then
  begin
    Assert(Current_archive_path <> EmptyStr, 'Current_archive_path not set');
    Result := StringReplace(Path, ARCHIVE_PATH, Current_archive_path, [rfReplaceAll]);
  end
  else
  if AnsiPos(MOD_PATH, Path) > 0 then
  begin
    Assert(Current_mod_path <> EmptyStr, 'Current_mod_path not set');
    Result := StringReplace(Path, MOD_PATH, Current_mod_path, [rfReplaceAll])
  end
  else
*)
  if AnsiPos(BATTLEFIELD_PATH, Path) > 0 then
  begin
    Assert(Current_battlefield_path <> EmptyStr, 'Current_battlefield_path not set');
    Result := StringReplace(Path, BATTLEFIELD_PATH, Current_battlefield_path, [rfReplaceAll]);
  end;

  Result := StringReplace(Result,'/','\',[rfReplaceAll]);
end;


function ExtractUnixFilePath(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(UNIX_PATH_DELIM, FileName);
  Result := Copy(FileName, 1, I);
end;

function ExtractUnixFileName(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter(UNIX_PATH_DELIM, FileName);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractUnixFileExt(const FileName: string): string;
var
  I: Integer;
begin
  I := LastDelimiter('.' + UNIX_PATH_DELIM, FileName);
  if (I > 0) and (FileName[I] = '.') then
    Result := Copy(FileName, I, MaxInt) else
    Result := '';
end;


{ TBattlefieldModEntry }

constructor TBattlefieldModEntry.Create(AFilesystem: TBattlefieldFileSystem);
begin
  FFilesystem := AFilesystem;
  PathList := TStringList.Create;
  Archives := TObjectList<TArchiveEntry>.Create;
end;

destructor TBattlefieldModEntry.Destroy;
begin
  Archives.Free;
  PathList.Free;
  inherited;
end;

procedure TBattlefieldModEntry.LoadFromConFile(Filename: string);
const
  BC1942_AUTO_MODPATH = 'rem Battlecraft 1942 added this command in order to generate lightmaps for this MOD';
var
  Text, ModPath : TStringList;
  i, Pos : integer;
begin
  PathList.Clear;
  Pos := 0;

  GameURL := Filename;
  GameName := Filename;
  GameVersion := Filename;

  AbsolutePath := ExtractFilePath(Filename);

  Text := TStringList.Create;
  ModPath := TStringList.Create;
  try
    Text.LoadFromFile(Filename);

    GameName := GetStringFromProperty(Text, 'game.CustomGameName');
    if GameName = EmptyStr then
      GameName := GetStringFromProperty(Text, 'game.setCustomGameName');

    GameVersion := GetStringFromProperty(Text, 'game.setCustomGameVersion');
    GameURL := GetStringFromProperty(Text, 'game.setCustomGameUrl');


    //Specific here, we need to remove modpath added by Battlecraft
    for i := 0 to Text.Count - 1 do
    begin
      // Find line that contains Battlecraft comment
      if Pos <= 0 then
        Pos := SFUniPos(BC1942_AUTO_MODPATH, Text[i])
      else
        // When we found it, delete all lines after it that contains modpaht data
        if SFUniPos('game.addModPath', Text[i]) > 0 then
          Text[i] := EmptyStr;
    end;

    GetStringListFromProperty(Text, 'game.addModPath', ModPath);

    for i := 0 to ModPath.Count - 1 do
    begin
      PathList.Insert(0, ModPath[i]);
    end;

  finally
    ModPath.Free;
    Text.Free;
  end;
end;

{ TBattlefieldFileSystem }

constructor TBattlefieldFileSystem.Create;
begin
  ActiveMod := TBattlefieldModEntry.Create(Self);
end;

destructor TBattlefieldFileSystem.Destroy;
begin
  ActiveMod.Free;
  inherited;
end;

end.
