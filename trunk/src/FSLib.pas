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
    RelativePath : string;
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

implementation

uses
  CONLib;

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
var
  Text, ModPath : TStringList;
  i : integer;
begin
  PathList.Clear;

  GameURL := Filename;
  GameName := Filename;
  GameVersion := Filename;

  Text := TStringList.Create;
  ModPath := TStringList.Create;
  try
    Text.LoadFromFile(Filename);

    GameName := GetStringFromProperty(Text, 'game.CustomGameName');
    if GameName = EmptyStr then
      GameName := GetStringFromProperty(Text, 'game.setCustomGameName');

    GameVersion := GetStringFromProperty(Text, 'game.setCustomGameVersion');
    GameURL := GetStringFromProperty(Text, 'game.setCustomGameUrl');

    GetStringListFromProperty(Text, 'game.addModPath', ModPath);

    for i := 0 to ModPath.Count - 1 do
    begin
      PathList.Add(ModPath[i]);
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
