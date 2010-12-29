unit SqlitePassErrorLogDialog;
{$i SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows, Messages,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, SqlitePassDbo;

type
  TSqlitePassErrorLogDlg = class(TForm)
    PanelIndexesToolBar: TPanel;
    PanelButtons: TPanel;
    SbOk: TSpeedButton;
    SbClear: TSpeedButton;
    SbSaveToFile: TSpeedButton;
    MemoErrorMsg: TMemo;
    SaveDialog: TSaveDialog;
    Panel1: TPanel;
    LabelErrorLogTitle: TLabel;
    ImageError: TImage;
    PanelErrorLogTitle: TPanel;
    LabelErrorDateTime: TLabel;
    ImageDate: TImage;
    SbPriorError: TSpeedButton;
    SbNextError: TSpeedButton;
    Bevel1: TBevel;
    Image1: TImage;
    LabelErrorIndex: TLabel;
    ImageErrorCode: TImage;
    Bevel2: TBevel;
    LabelErrorCode: TLabel;
    Bevel3: TBevel;
    procedure SbClearClick(Sender: TObject);
    procedure SbOkClick(Sender: TObject);
    procedure SbSaveToFileClick(Sender: TObject);
    procedure SbPriorErrorClick(Sender: TObject);
    procedure SbNextErrorClick(Sender: TObject);
  private
    FErrorLog: TSqlitePassDatabaseError;
    procedure ShowError;
  public
   constructor Create(ErrorLog: TSqlitePassDatabaseError); reintroduce;
   procedure ShowLastError;
  end;

var
  SqlitePassErrorLogDlg: TSqlitePassErrorLogDlg;

implementation

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}

constructor TSqlitePassErrorLogDlg.Create(ErrorLog: TSqlitePassDatabaseError);
begin
{ Must be Create(Nil) in order to properly display the form at design time ! }
inherited Create(nil);
FErrorLog := ErrorLog;
MemoErrorMsg.Clear;
If Assigned(FErrorLog.Database)
   then begin
        If Assigned(FErrorLog.Database.Options) and Not FErrorLog.Database.Options.LogErrors
           then MemoErrorMsg.Text := 'No error log available. Database.Options.LogErrors is set to False.'
           else LabelErrorLogTitle.Caption := 'Database : ' + FErrorLog.Database.Name + ' (' + IntToStr(FErrorLog.ErrorList.Count) + ' Logged Error(s))';
        end
   else LabelErrorLogTitle.Caption := 'Dataset : ' + FErrorLog.Dataset.Name + ' (' + IntToStr(FErrorLog.ErrorList.Count) + ' Logged Error(s))';

end;

procedure TSqlitePassErrorLogDlg.SbClearClick(Sender: TObject);
begin
FErrorLog.Clear;
MemoErrorMsg.Clear;
LabelErrorLogTitle.Caption := IntToStr(FErrorLog.ErrorList.Count) + ' Logged Error(s)';
end;

procedure TSqlitePassErrorLogDlg.SbOkClick(Sender: TObject);
begin
ModalResult := mrOk;
end;

procedure TSqlitePassErrorLogDlg.SbSaveToFileClick(Sender: TObject);
begin
If SaveDialog.Execute
   then FErrorLog.SaveToFile(SaveDialog.FileName);
end;

procedure TSqlitePassErrorLogDlg.SbPriorErrorClick(Sender: TObject);
begin
if FErrorLog.GetPriorError
   then ShowError;
end;

procedure TSqlitePassErrorLogDlg.SbNextErrorClick(Sender: TObject);
begin
 if FErrorLog.GetNextError
    then ShowError;
end;

procedure TSqlitePassErrorLogDlg.ShowLastError;
begin
 FErrorLog.GetLastError;
 ShowError;
end;

procedure TSqlitePassErrorLogDlg.ShowError;
begin
 SbPriorError.Enabled := (FErrorLog.ErrorList.Count > 0) and (FErrorLog.CurrentErrorIndex > 0);
 SbNextError.Enabled  := (FErrorLog.ErrorList.Count > 0) and (FErrorLog.CurrentErrorIndex < Pred(FErrorLog.ErrorList.Count));
 LabelErrorIndex.Caption := 'Error #' + IntToStr(FErrorLog.CurrentErrorIndex+1);
 if Assigned(FErrorLog.CurrentError) then
    begin
    LabelErrorDateTime.Caption := DateTimeToStr(FErrorLog.CurrentError.DateTime);
    LabelErrorCode.Caption := 'SQLite Error : ' + IntToStr(FErrorLog.CurrentError.Code);
    LabelErrorCode.Visible := FErrorLog.CurrentErrorIndex > -1;
    ImageErrorCode.Visible := FErrorLog.CurrentErrorIndex > -1;
    MemoErrorMsg.Text := FErrorLog.CurrentError.Message;
    end;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassErrorLogDialog.lrs}
 {$ENDIF}
end.
 
