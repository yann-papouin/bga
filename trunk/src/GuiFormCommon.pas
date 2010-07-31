unit GuiFormCommon;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormCommon = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    { Déclarations privées }
  protected
    FOperationCount : integer;
    FApplicationTitle : string;
  public
    { Déclarations publiques }
    property Title : string read GetTitle write SetTitle;
    function OperationPending : boolean;
    procedure BeginOperation;
    procedure EndOperation;
  end;

var
  FormCommon: TFormCommon;

implementation

{$R *.dfm}

uses
  AppLib;

{ TFormCommon }

procedure TFormCommon.FormCreate(Sender: TObject);
begin
  if DebugHook <> 0 then
    FApplicationTitle := Caption
  else
    FApplicationTitle := Caption + ' - ' + ApplicationSvnTitle;
end;

function TFormCommon.GetTitle: string;
begin
  Result := Caption;
end;

procedure TFormCommon.SetTitle(const Value: string);
begin
  if Value <> EmptyStr then
    Caption := Format('%s - %s',[ExtractFilename(Value), FApplicationTitle])
  else
    Caption := FApplicationTitle;
end;

procedure TFormCommon.BeginOperation;
begin
  Inc(FOperationCount);
end;

procedure TFormCommon.EndOperation;
begin
  Dec(FOperationCount);
  if FOperationCount < 0 then
    FOperationCount := 0;
end;

function TFormCommon.OperationPending: boolean;
begin
  Result := FOperationCount > 0;
end;

end.
