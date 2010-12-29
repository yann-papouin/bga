{ This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Author : Luc DAVID Email: luckylazarus@free.fr
    2010
    Last update : 16/05/2010

  --------------------------------------------------------------------------- }

unit SqlitePassParamsDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
  LCLType,
 {$ELSE}
  Windows,
  Messages,
  Math,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ExtCtrls, DB, ComCtrls, SqlitePassDbo, SqlitePassUtils,
  SqlitePassVisualTools;

type

  { TSqlitePassDatatypesDlg }

  TSqlitePassParamsDlg = class(TForm)
    PanelBottom: TPanel;
    PanelDatatypes: TPanel;
    BtOk: TButton;
    BtnResetAll: TButton;
    BtDefault: TButton;
    PanelCtrlGridHeader: TPanel;
    Bevel5: TBevel;
    Shape5: TShape;
    Label6: TLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText6: TStaticText;
    StaticText4: TStaticText;
    PanelCtrlGrid: TPanel;
    PanelParams: TPanel;
    CbParamDataType: TComboBox;
    PanelCtrlGridNav: TPanel;
    EditParamName: TEdit;
    StaticText3: TStaticText;
    CbParamBound: TCheckBox;
    CbParamValue: TComboBox;
    Panel1: TPanel;
    LabelDataFormat: TLabel;
    ImageTips: TImage;
    BtnCancel: TButton;
    procedure BtOkClick(Sender: TObject);
    procedure BtnResetAllClick(Sender: TObject);
    procedure BtDefaultClick(Sender: TObject);
    procedure EditParamNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EditParamNameEnter(Sender: TObject);
    procedure CbParamValueChange(Sender: TObject);
    procedure CbParamDataTypeChange(Sender: TObject);
  private
    OldParams: TSqlitePassParams;
    Dataset: TSqlitePassDataset;
    CtrlGrid: TSPVTCtrlGrid;
    Navigator: TSPVTCtrlGridNavigator;
    procedure DisplayParams;
    procedure OnCtrlGridNewRecord(CtrlGrid: TSPVTCtrlGrid);
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  end;

var
  SqlitePassParamsDlg: TSqlitePassParamsDlg;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

{ Constructor and Destructor }

constructor TSqlitePassParamsDlg.Create(AOwner: TComponent);
var
ft: TFieldType;
begin
{ Must be Create(Nil) in order to properly display the form at design time ! }
Inherited Create(Nil);
Dataset := TSqlitePassDataset(AOwner);

With CbParamDataType do
     begin
     Clear;
     Sorted := True;
     for ft := Low(TFieldType) to High(TFieldType)
         do Items.Add(FieldTypeToString(ft));
     end;

CbParamValue.Clear;
CbParamValue.Items.Add('Null');
if Assigned(Dataset.MasterSource)
   then Dataset.MasterSource.Dataset.GetFieldNames(CbParamValue.Items);

CtrlGrid := TSPVTCtrlGrid.Create(PanelCtrlGrid);

With CtrlGrid do
     begin
     Align := AlClient;
     OnNewRecord := OnCtrlGridNewRecord;
     Panel := PanelParams;
     AutoScroll := True;
     end;

Navigator := TSPVTCtrlGridNavigator.Create(PanelCtrlGridNav);
Navigator.SetBounds(1,1,Navigator.Width,Navigator.Height);
Navigator.CtrlGrid := CtrlGrid;
OldParams := TSqlitePassParams.Create(Dataset);
OldParams.Assign(Dataset.Params);
DisplayParams;
end;

procedure TSqlitePassParamsDlg.DisplayParams;
var
i: Integer;
begin
Try
  CtrlGrid.Visible := False;
  CtrlGrid.Clear;
  For i := 0 to Pred(Dataset.Params.Count) do
      begin
      CtrlGrid.Append;
      With Dataset.Params[i] do
           begin
           TEdit(CtrlGrid.ControlByName('EditParamName')).Text := Name;
           TComboBox(CtrlGrid.ControlByName('CbParamDataType')).ItemIndex := TComboBox(CtrlGrid.ControlByName('CbParamDataType')).Items.IndexOf(FieldTypeToString(DataType));
           TComboBox(CtrlGrid.ControlByName('CbParamValue')).Text := Value;
           TCheckBox(CtrlGrid.ControlByName('CbParamBound')).Checked := Bound;
          end;
      end;
  { To Set State to cgsBrowse }
  CtrlGrid.First;
  if CtrlGrid.RowCount = 0
     then CtrlGrid.Append;
Finally
  CtrlGrid.Visible := True;
end;
end;


Destructor TSqlitePassParamsDlg.Destroy;
begin
OldParams.Free;
inherited;
end;

{ Buttons Events }

procedure TSqlitePassParamsDlg.BtOkClick(Sender: TObject);
var
i, j: integer;
ParamName: String;
NewParam: TSqlitePassParam;
begin
Dataset.Params.ClearParams;
j := 0;
For i := 0 to Pred(CtrlGrid.RowCount) do
    begin
    CtrlGrid.ActiveRowIndex := i;
    ParamName := TEdit(CtrlGrid.ControlByName('EditParamName')).Text;
    if ParamName <> '' then
       begin
       Inc(j);
       NewParam := TSqlitePassParam.Create(Dataset.Params);
       With NewParam do
            begin
            Name := ParamName;
            DataType := StringToFieldType(TComboBox(CtrlGrid.ControlByName('CbParamDataType')).Text);
            Value := TComboBox(CtrlGrid.ControlByName('CbParamValue')).Text;
            ParamIndex := j;
            Bound := TCheckBox(CtrlGrid.ControlByName('CbParamBound')).Checked;
            end;
       Dataset.Params.Add(NewParam);
       end;
    end;
if Dataset.Active
   then Dataset.Refresh;
end;

procedure TSqlitePassParamsDlg.OnCtrlGridNewRecord(
  CtrlGrid: TSPVTCtrlGrid);
begin
TEdit(CtrlGrid.ControlByName('EditParamName')).Text := '';
TComboBox(CtrlGrid.ControlByName('CbParamDataType')).ItemIndex := TComboBox(CtrlGrid.ControlByName('CbParamDataType')).Items.IndexOf('Unknown');
TComboBox(CtrlGrid.ControlByName('CbParamValue')).Text := '';
TComboBox(CtrlGrid.ControlByName('CbParamValue')).ItemIndex := -1;
TCheckBox(CtrlGrid.ControlByName('CbParamBound')).Checked := False;
end;

{ Events }

procedure TSqlitePassParamsDlg.BtnResetAllClick(Sender: TObject);
begin
Dataset.Params.Assign(OldParams);
DisplayParams;
end;

procedure TSqlitePassParamsDlg.BtDefaultClick(Sender: TObject);
begin
Dataset.UpdateParamsList;
DisplayParams;
end;

procedure TSqlitePassParamsDlg.EditParamNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
{ Classic up/down keys with comboboxes }
 if (Sender is TComboBox) then
    if (Key = VK_DOWN) or (Key = VK_UP) then Exit;

 CtrlGrid.KeyDown(Key, Shift);
end;

procedure TSqlitePassParamsDlg.EditParamNameEnter(Sender: TObject);
begin
  CtrlGrid.FocusActiveRow(Sender);
  CbParamDataTypeChange(Sender);
end;

procedure TSqlitePassParamsDlg.CbParamValueChange(Sender: TObject);
begin
 TCheckBox(CtrlGrid.ControlByName('CbParamBound')).Checked :=(TComboBox(CtrlGrid.ControlByName('CbParamValue')).Text <> '')
                                                              or (TComboBox(CtrlGrid.ControlByName('CbParamMasterField')).Text <> '');
end;

procedure TSqlitePassParamsDlg.CbParamDataTypeChange(Sender: TObject);
var
Datatype: TFieldType;
begin
Datatype := StringToFieldType(TComboBox(CtrlGrid.ControlByName('CbParamDataType')).Text);
Case Datatype of
     ftString,
     ftWideString : LabelDataFormat.Caption := 'Data format : You can use _ or % as WildCard chars with LIKE operator';
     ftDateTime   : LabelDataFormat.Caption := 'Data format : ' + Dataset.Database.DatatypeOptions.DateTimeFormat;
     ftDate       : LabelDataFormat.Caption := 'Data format : ' + Dataset.Database.DatatypeOptions.DateFormat;
     ftTime       : LabelDataFormat.Caption := 'Data format : ' + Dataset.Database.DatatypeOptions.TimeFormat;
     else           LabelDataFormat.Caption := '';
     end;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassParamsDialog.lrs}
 {$ENDIF}
end.
    
  