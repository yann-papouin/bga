unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DBGrids, JvExDBGrids, DB, SqlitePassDbo, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    SqlitePassDatabase1: TSqlitePassDatabase;
    SqlitePassDataset1: TSqlitePassDataset;
    DataSource1: TDataSource;
    SelectInteger: TButton;
    DBGrid1: TDBGrid;
    SelectString: TButton;
    SelectWidestring: TButton;
    procedure SelectIntegerClick(Sender: TObject);
    procedure SelectStringClick(Sender: TObject);
    procedure SelectWidestringClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Déclarations privées }
    function AppPath : string;
  public
    { Déclarations publiques }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.AppPath: string;
begin
  result := IncludeTrailingBackslash(ExtractFilePath(Application.ExeName));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SqlitePassDatabase1.Database := AppPath + 'BGA.db';
  SqlitePassDatabase1.SQLiteLibrary := AppPath + 'sqlite3.dll';

  SqlitePassDatabase1.Connected := true;
end;

procedure TForm1.SelectIntegerClick(Sender: TObject);
begin
  SqlitePassDataset1.Close;
  SqlitePassDataset1.Params.Clear;
  SqlitePassDataset1.ParamCheck := True;

  SqlitePassDataset1.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE id=:IdAsInteger;';
  SqlitePassDataset1.Params.ParamByName('IdAsInteger').Value := ('1');
  SqlitePassDataset1.Open;
end;

procedure TForm1.SelectStringClick(Sender: TObject);
begin
  SqlitePassDataset1.Close;
  SqlitePassDataset1.Params.Clear;
  SqlitePassDataset1.ParamCheck := True;

  SqlitePassDataset1.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE path=:PathAsString;';
  SqlitePassDataset1.Params.ParamByName('PathAsString').Value := ('path');
  SqlitePassDataset1.Open;
end;

procedure TForm1.SelectWidestringClick(Sender: TObject);
begin
  SqlitePassDataset1.Close;
  SqlitePassDataset1.Params.Clear;
  SqlitePassDataset1.ParamCheck := True;

  SqlitePassDataset1.SQL.Text := 'SELECT * FROM "ARCHIVE" WHERE path=:PathAsWidestring;';
  SqlitePassDataset1.Params.ParamByName('PathAsWidestring').Value := '1';
  SqlitePassDataset1.Open;
end;

end.
