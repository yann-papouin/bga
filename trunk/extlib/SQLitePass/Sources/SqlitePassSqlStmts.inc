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

    Basic SQL Parser and Statements Management
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006 - 2010

    Major changes are indicated in the \Documentation\Changes.pdf file
    Last update 28/02/2010

  --------------------------------------------------------------------------- }

  { TSqlitePassSQLSections }

procedure TSqlitePassSQLSections.ClearSectionText(Const SectionName,
  NewStmt: String);
begin
  Values[SectionName] := '';
end;

function TSqlitePassSQLSections.GetSectionText(Const SectionName: String): String;
begin
  Result := Values[SectionName];
end;

function TSqlitePassSQLSections.GetSQLStmtFromSections: String;
var
i: integer;
TempText: String;
begin
Result := '';
for i := 0 to Pred(Count) do
    begin
    TempText := GetSectionText(Names[i]);
    if TempText <> ''
       then Result := Result + TempText + kwSpace;
    end;
end;

procedure TSqlitePassSQLSections.ReplaceSectionText(Const SectionName, NewStmt: String);
var
i: integer;
begin
i := IndexOfName(SectionName);
if i > -1
   then Strings[i] := SectionName + '=' + Trim(NewStmt);
end;

procedure TSqlitePassSQLSections.ReplaceSectionTextFromSQLSections(
  SQLSections: TSqlitePassSQLSections; Const SectionName: String);
begin
  if SQLSections = nil then Exit;
  if SectionName = ''
     then Assign(SQLSections)
     else SetSectionText(SectionName, SQLSections.GetSectionText(SectionName));
end;

procedure TSqlitePassSQLSections.SetSectionText(Const SectionName, Text: String);
begin
  Values[SectionName] := Text;
end;

procedure TSqlitePassSQLSections.SetSectionTextFromTokenizer(
  Const SectionName: String; Tokenizer: TSqlitePassSQLTokenizer);
begin
 Values[SectionName] := Tokenizer.Text;
end;

procedure TSqlitePassSQLSections.SplitSQLStmtIntoSections(Const SQLStmt: String; Const Keywords: Array of String);


  procedure split;
  var
  i: Integer;
  SectionName, SectionText: String;
  Tokenizer: TSqlitePassSQLTokenizer;

  begin
  Tokenizer := TSqlitePassSQLTokenizer.Create;
  Try
  Tokenizer.Text := SQLStmt;

  { Fills the StringList with empty sections }
   Clear;
   For i := Low(Keywords) to High(Keywords) do Add(Keywords[i]+'=');

  { splits sections }
  Repeat
    SectionName := Tokenizer.Token.Text;
    SectionText := Tokenizer.Token.Text;
    SectionText := SectionText + kwSpace + Tokenizer.GetTextAfter(ttKeyWord) + kwSpace;
    ReplaceSectionText(SectionName, SectionText);
  Until not Tokenizer.Next(ttKeyword)
  finally
  Tokenizer.Free;
  end;
  end;

begin
If (SQLStmt <> '') and (SizeOf(Keywords) > 0)
   then Split
end;



{ TSqlitePassSqlStmt }

constructor TSqlitePassSqlStmt.Create(Database: TSqlitePassDatabase);
begin
inherited Create;
FAvailableOperations := [];
FDatabase := Database;
FTokenizer := TSqlitePassSQLTokenizer.Create;
FSQLSections := TSqlitePassSQLSections.Create;
FOriginalSQLSections := TSqlitePassSQLSections.Create;
FStmtHandle := nil;
SQL := '';
end;


destructor TSqlitePassSqlStmt.Destroy;
begin
Unprepare;
FOriginalSQLSections.Free;
FSQLSections.Free;
FTokenizer.Free;
FDatabase := nil;
Inherited Destroy;
end;

procedure TSqlitePassSqlStmt.SetFSQLText(Value: String);
var
TempSQLText: String;
begin
TempSQLText := NormalizeText(Value);

if AnsiCompareText(FOriginalSQLText, TempSQLText) <> 0 then
   begin
   UnPrepare;
   if TempSQLText <> '' then
      begin
      FOriginalSQLText := TempSQLText;
      GetStmtType; { Prepare the FStmtKeywords array }
      FOriginalSQLSections.SplitSQLStmtIntoSections(FOriginalSQLText, FStmtKeywords);
      FSQLSections.Assign(FOriginalSQLSections);
      TempSQLText := NormalizeText(FSQLSections.GetSQLStmtFromSections);
      end;
   { Can we safely split and rebuilt the statement ? }
   FCanSplit := (FOriginalSQLText = TempSQLText) and (SQLType = stSelect);
   end;
end;

function TSqlitePassSqlStmt.GetFSQLText: String;
begin
If FCanSplit and (FSQLSections.Count > 0)
   then FSQLText := NormalizeText(FSQLSections.GetSQLStmtFromSections)
   else FSQLText := FOriginalSQLText;
Result := FSQLText;
end;

function TSqlitePassSqlStmt.NormalizeText(Text: String): String;
var
TempTokenizer: TSqlitePassSQLTokenizer;
PreviousToken: TSqlitePassSQLToken;
Accept: Boolean;
begin
Result := '';
TempTokenizer := TSqlitePassSQLTokenizer.Create;
TempTokenizer.Text := Text;
PreviousToken := nil;
Try
While Not TempTokenizer.EOF do
 begin
 Case TempTokenizer.Token.TokenType of
      ttWhiteSpace : Accept := Assigned(PreviousToken) and (PreviousToken.TokenType <> ttWhiteSpace);
//      ttCR         : Accept := False;
//      ttLF         : Accept := False;
      ttSemiColon  : Accept := False;
      else Accept := True;
      end;
 if Accept
    then Result := Result + TempTokenizer.Token.Text;
 PreviousToken := TempTokenizer.Token;
 TempTokenizer.Next;
 end;
{ Add ';' at the end of the SQL statement }
Result := Trim(Result);
if Result <> '' then Result := Result + kwSqlEnd;
finally
TempTokenizer.Free;
end;
end;

function TSqlitePassSqlStmt.GetStmtType: StmtType;
var
i: Integer;
begin
SetLength(FStmtKeyWords, 0);
Result := SQLitePassDbo.GetStmtType(FOriginalSQLText);
if Result = stSelect then
   begin
   SetLength(FStmtKeyWords, High(SelectStmtKeywords)+1);
   for i := 0 to High(SelectStmtKeywords)
       do FStmtKeyWords[i] := SelectStmtKeywords[i];
   end;
end;

procedure TSqlitePassSqlStmt.Prepare;
var
SQLStmt: String;
begin
  UnPrepare;
  SQLStmt := SQL;
  FDatabase.FEngine.PrepareStmt(FStmtHandle, pChar(SQLStmt));
end;

procedure TSqlitePassSqlStmt.UnPrepare;
begin
FAvailableOperations := [];
if Assigned(FDatabase) and Assigned(FDatabase.FEngine) and Assigned(FStmtHandle) then
    begin
    FDatabase.Engine.UnprepareStmt(FStmtHandle);
    FStmtHandle := nil;
    end;
end;

procedure TSqlitePassSqlStmt.QuoteString(var S: String);
begin
If S <> '' then
   begin
   S := StringReplace(S, kwDot, FDatabase.Options.FQuoteOpen + kwDot + FDatabase.Options.FQuoteClose, [rfReplaceAll]);
   S := FDatabase.Options.FQuoteOpen + S + FDatabase.Options.FQuoteClose;
   end;
end;

Procedure TSqlitePassSqlStmt.UnquoteString(var S: String; All: Boolean = True);
var
i: integer;
begin
 if S = '' then Exit;

 if All
    then for i := 0 to High(QuoteCharsArray) do
         S := StringReplace(S, QuoteCharsArray[i], '', [rfReplaceAll])
    else begin
         S := StringReplace(S, FDatabase.Options.FQuoteOpen, '', [rfReplaceAll]);
         S := StringReplace(S, FDatabase.Options.FQuoteClose, '', [rfReplaceAll]);
         end;
end;

procedure TSqlitePassSqlStmt.RestoreOriginalStmt;
begin
 SQL := OriginalSQL;
end;


{ TSqlitePassSelectStmt }

constructor TSqlitePassSelectStmt.Create(Dataset: TSqlitePassDataset);
begin
if Assigned(Dataset.FDatabase)
   then Inherited Create(Dataset.FDatabase)
   else Inherited Create(TSqlitePassDatabase(nil));
FDataset := Dataset;
FTableDefs := TSqlitePassSelectStmtTableDefs.Create(Self);
FFieldDefs := TSqlitePassSelectStmtFieldDefs.Create(nil);
FPrimaryKeyStmt := '';
end;

destructor TSqlitePassSelectStmt.Destroy;
begin
Unprepare;
FFieldDefs.Free;
FTableDefs.Free;
FDataset := nil;
inherited Destroy;
end;

procedure TSqlitePassSelectStmt.Prepare;
begin
try
if (SQL <> '') and (SQLType = StSelect) then
   begin
   PrepareAsReadOnly;
   BuildSchema;
   if (FAvailableOperations <> []) and FCanSplit and not FDataset.ReadOnly
      then PrepareWritingSQLStmts;
   end;
except
   Raise;
end;
end;

procedure TSqlitePassSelectStmt.PrepareAsReadOnly;
begin
{ Reset the PrimaryKey Count }
FPrimaryKeyCount := 0;
{ Removes any previously added internal "_rowid_" primary keys from the select statement }
SQLSections.ReplaceSectionTextFromSQLSections(OriginalSQLSections, kwSelect);
Inherited Prepare;
Include(FAvailableOperations, aoRead);
if SqliteDbv3_bind_parameter_count(FStmtHandle) > 0
   then Include(FAvailableOperations, aoBind);
end;

procedure TSqlitePassSelectStmt.PrepareWritingSQLStmts;
var
SavedAvailableOperations: TSqlitePassAvailableOperations;
begin
AddInternalPrimaryKeys;
SavedAvailableOperations := FAvailableOperations;
Inherited Prepare;
FAvailableOperations := SavedAvailableOperations;
PrepareBindValueDispacher;
end;

Procedure TSqlitePassSelectStmt.UnPrepare;
begin
UnprepareWritingSQLStmts;
Inherited UnPrepare;
end;

Procedure TSqlitePassSelectStmt.UnprepareWritingSQLStmts;
var
i: Integer;
begin
FPrimaryKeyStmt := '';
For i := 0 to Pred(FTableDefs.Count)
 do FTableDefs[i].UnprepareWritingSQLStmts;

Exclude(FAvailableOperations, aoInsert);
Exclude(FAvailableOperations, aoUpdate);
Exclude(FAvailableOperations, aoDelete);
FTableDefs.ClearAndFreeItems;
FFieldDefs.Clear;
end;

procedure TSqlitePassSelectStmt.PrepareBindValueDispacher;
var
i: Integer;
begin
 { Dataset Fields Columns }
 for i := 0 to Pred(FFieldDefs.Count) do
   FFieldDefs[i].BindValue := FDataset.FDatabase.PrepareBindValueDispacher(FFieldDefs[i].DataType);
end;

(*procedure TSqlitePassSelectStmt.PrepareBindValueDispacher;
var
i: Integer;

begin
   { Dataset Fields Columns }
   for i := 0 to Pred(FFieldDefs.Count) do
   with FFieldDefs[i] do
        begin

        { Safe assignation to a dummy procedure }
        BindValue := @BindDummyToSqliteValue;

        Case DataType of

          ftBoolean:
          begin
          Case FDataset.FDatabase.DatatypeOptions.FBooleanStorage of
               asInteger : BindValue := @BindWordBoolToSqliteValue;
               asText    : BindValue := @BindWordBoolToSqliteValueAsText;
               end;
          end;

          ftSmallint, ftInteger, ftWord:
            BindValue := @BindIntegerToSqliteValue;

          ftAutoInc, ftLargeint:
            BindValue := @BindInt64ToSqliteValue;

          ftFloat, ftCurrency:
            BindValue := @BindDoubleToSqliteValue;

          ftBCD:
            BindValue := @BindCurrencyToSqliteValue;

          ftDate:
          begin
          Case FDataset.FDatabase.DatatypeOptions.DateStorage of
               asInteger : BindValue := @BindIntegerToSqliteValue;
               asText    : BindValue := @BindDateToSqliteValueAsText;
               end;
          end;

          ftTime:
          begin
          Case FDatabase.DatatypeOptions.TimeStorage of
               asInteger : BindValue := @BindIntegerToSqliteValue;
               asText    : BindValue := @BindTimeToSqliteValueAsText;
               end;
          end;

          ftDateTime:
          begin
          Case FDatabase.DatatypeOptions.DateTimeStorage of
               dtsDateTime : BindValue := @BindDoubleToSqliteValue;
               dtsText     : BindValue := @BindDateTimeToSqliteValueAsText;
               dtsJulian   : BindValue := @BindDateTimeToSqliteValueAsJulianDateTime;
               dtsUnix     : BindValue := @BindDateTimeToSqliteValueAsUnixDateTime;
               dtsMac      : BindValue := @BindDateTimeToSqliteValueAsMacDateTime;
               end;
          end;

          // TODO UnicodeEnoding = ueRawText
          ftString, ftFixedChar :
          Case FDatabase.FSystemEncoding of
               sysANSI     : BindValue := @BindAnsiStringToSqliteValueAsUTF8;
               sysUTF8     : BindValue := @BindUTF8StringToSqliteValueAsUTF8;
               // TODO - Check with D2009
               sysUTF16    : BindValue := @BindUTF8StringToSqliteValueAsUTF16;
               end;

          ftWideString     : BindValue := @BindWideStringToSqliteValueAsUTF16;

          ftBytes, ftVarBytes, ftBlob, ftGraphic, ftMemo, ftFmtMemo {$IFDEF HasftWideMemo}, ftWideMemo {$ENDIF}
                           : BindValue :=  @BindBlobToSqliteValue;
          end; { Case }
        end; { With }
end; *)

Procedure TSqlitePassSelectStmt.AddInternalPrimaryKeys;
var
i: Integer;
TableName: String;
begin
  { Build the Primary Keys Statement and Prepare }
  FPrimaryKeyStmt := '';
  FPrimaryKeyCount := 0;

  if FTableDefs.Count = 0 then Exit;

  Include(FAvailableOperations, aoInsert);
  Include(FAvailableOperations, aoUpdate);
  Include(FAvailableOperations, aoDelete);

  for i := 0 to Pred(FTableDefs.Count) do
      begin
      TableName := FTableDefs[i].FTableName;
      { We exclude system read only tables (__RecordsetSchema, sqlite_master, sqlite_temp_master tables) }
      if (TableName <> '__RecordsetSchema')
         and (AnsiCompareText(TableName, 'sqlite_master') <> 0)
         and (AnsiCompareText(TableName, 'sqlite_temp_master') <> 0) then
         begin
         QuoteString(TableName);
         FPrimaryKeyStmt := FPrimaryKeyStmt + kwSpace + TableName + '._Rowid_,';
         FTableDefs[i].PrepareWritingSQLStmts;
         Inc(FPrimaryKeyCount);
         end;

      If Not (aoInsert in FTableDefs[i].AvailableOperations)
         then Exclude(FAvailableOperations, aoInsert);
      If Not (aoUpdate in FTableDefs[i].AvailableOperations)
         then Exclude(FAvailableOperations, aoUpdate);
      If Not (aoDelete in FTableDefs[i].AvailableOperations)
         then Exclude(FAvailableOperations, aoDelete);
      end;

  if FPrimaryKeyCount > 0 then
     begin
     Tokenizer.Text := SQLSections.GetSectionText(kwSelect);
     if Tokenizer.Locate(kwDistinct)
        then Tokenizer.InsertAfter(kwDistinct, FPrimaryKeyStmt)
        else Tokenizer.InsertAfter(kwSelect, FPrimaryKeyStmt);
     SQLSections.SetSectionTextFromTokenizer(kwSelect, Tokenizer);
     end;
end;

procedure TSqlitePassSelectStmt.BuildSchema;
var
i, ColumnCount: Integer;
TableName, FieldName: String;
begin
Try
ColumnCount := SqliteDbv3_column_count(StmtHandle);

for i := 0 to Pred(ColumnCount) do
     begin
     Case FDataset.FDatabase.SystemEncoding of
           sysANSI  : begin
                      TableName := SqlitePassUtils.UTF8ToAnsi(SqliteDbv3_column_table_name(StmtHandle, i));
                      FieldName := SqlitePassUtils.UTF8ToAnsi(SqliteDbv3_column_origin_name(StmtHandle, i));
                      end;
           sysUTF8  : begin
                      TableName := SqliteDbv3_column_table_name(StmtHandle, i);
                      FieldName := SqliteDbv3_column_origin_name(StmtHandle, i);
                      end;
           sysUTF16 : begin
                      TableName := SqliteDbv3_column_table_name16(StmtHandle, i);
                      FieldName := SqliteDbv3_column_origin_name16(StmtHandle, i);
                      end;
           end;

     if (TableName = '') or (FieldName = '')
        then FillTableDefsFromRecordsetSchema(i)
        else FillTableDefsFromDatabaseSchema(TableName, FieldName, i);

     end; { For }
except
 Raise;
end;
end;

{ The default column-name which identifies rows in SQLite is 'rowid',
  but for tables which have a primary integer key, this doesn't work
  (even though the documentation says it ought to). Therefore it is necessary
  to identify, for the given database, which tables need to use a different
  column-name for this. This gives a hash of table->column names. }
procedure TSqlitePassSelectStmt.FillTableDefsFromDatabaseSchema(Const TableName, FieldName: String; RecFieldIndex: Integer);
var
DatabaseTableDef: TSqlitePassTableDef;
StmtTableDef: TSqlitePassSelectStmtTableDef;
DatabaseFieldDef: TSqlitePassTableFieldDef;
StmtFieldDef: TSqlitePassSelectStmtFieldDef;

begin
{ Create a TableDef for a given table name }
 DatabaseTableDef := FDataset.Database.TableDefs.FindTable(TableName);
 if Assigned(DatabaseTableDef) then
    begin
    { if the TableDef doesn't already exist in select schema, we create it }
    StmtTableDef := TableDefs.FindTable(TableName);
    if StmtTableDef = nil then
       begin
       StmtTableDef := TSqlitePassSelectStmtTableDef.Create(FTableDefs);
       { Assigns doesn't fill FieldDefs }
       StmtTableDef.Assign(DatabaseTableDef);
       StmtTableDef.FTableDefNo := TableDefs.Add(StmtTableDef);
       end;

    { FieldDefs }
    DatabaseFieldDef := DatabaseTableDef.FieldDefs.FindFieldDef(TableName +  '.' + FieldName);
    if Assigned(DatabaseFieldDef) then
       begin
      { a same FieldDef can appear several times in a schema, so we always create one }
       StmtFieldDef := TSqlitePassSelectStmtFieldDef.Create(StmtTableDef.FFieldDefs);
       StmtFieldDef.Assign(DatabaseFieldDef);
       StmtFieldDef.FRecordFieldIndex := RecFieldIndex;
       StmtTableDef.FieldDefs.Add(StmtFieldDef);
       { Add this FieldDef to the AutoIncfieldDefs if it is an AutoInc Field }
       if StmtFieldDef.FAutoInc
          then StmtTableDef.FAutoIncFieldDefs.Add(StmtFieldDef);
       { Add this FieldDef to the SelectStmt fieldDefs too.
         That way End-Users will have a more convenient way to loop into FieldDefs }
       FFieldDefs.Add(StmtFieldDef);
       end;
    end;
end;

{ When field origine cannot be found from schema, we try to figure out
  the fields info and set minimum default setting for the field }
procedure TSqlitePassSelectStmt.FillTableDefsFromRecordsetSchema(RecFieldIndex: Integer);
var
StmtTableDef: TSqlitePassSelectStmtTableDef;
StmtFieldDef: TSqlitePassSelectStmtFieldDef;
begin
{ if the 'RecordsetSchema' TableDef doesn't already exist in select schema, we create it }
StmtTableDef := TableDefs.FindTable('__RecordsetSchema');
if StmtTableDef = nil then
   begin
   StmtTableDef := TSqlitePassSelectStmtTableDef.Create(FTableDefs);
   StmtTableDef.FTableName := '__RecordsetSchema';
   StmtTableDef.FTableDefNo := TableDefs.Add(StmtTableDef);
   end;

{ a same FieldDef can appear several times in a schema, so we always create one }
StmtFieldDef := TSqlitePassSelectStmtFieldDef.Create(StmtTableDef.FFieldDefs);
With StmtFieldDef do
     begin
     Case FDataset.FDatabase.SystemEncoding of
           sysANSI  : FFieldName := SqlitePassUtils.UTF8ToAnsi(SqliteDbv3_column_name(StmtHandle, RecFieldIndex));
           sysUTF8  : FFieldName := SqliteDbv3_column_name(StmtHandle, RecFieldIndex);
           sysUTF16 : FFieldName := SqliteDbv3_column_name16(StmtHandle, RecFieldIndex);
           end;

     UnquoteString(FFieldName);      
     FNativeDataTypeCode := SqliteDbv3_column_type(StmtHandle, RecFieldIndex);
     Case FNativeDataTypeCode of
          SQLITE_INTEGER: FDataType := ftLargeInt;
          SQLITE_FLOAT  : FDataType := ftFloat;
          SQLITE_BLOB   : FDataType := ftBlob;
          else begin {SQLITE_TEXT or SQLITE_NULL }
               FDataType := ftString;
               FSize := DefaultStringFieldSize;
               end;
          end;     
     FPrimaryKey := False;
     end;

StmtTableDef.FieldDefs.Add(StmtFieldDef);
{ Add this FieldDef to the SelectStmt fieldDefs too.
 That way End-Users will have a more convenient way to loop into FieldDefs }
FFieldDefs.Add(StmtFieldDef);
end;


procedure TSqlitePassSelectStmt.Post(Const PostType: TSqlitePassPostType; Const RecordBuffer: PRecBuffer);
var
i: Integer;
Msg: String;
begin
try
  FDataset.FDatabase.FEngine.Transaction.StartInternalTransaction;

  For i := 0 to Pred(FTableDefs.Count) do
     begin
     Case PostType of
           ptInsert: FTableDefs[i].PostInsert(RecordBuffer);
           ptUpdate: FTableDefs[i].PostUpdate(FDataset.FRecordset.GetRowId(RecordBuffer, i), RecordBuffer);
           ptDelete: FTableDefs[i].PostDelete(FDataset.FRecordset.GetRowId(RecordBuffer, i), RecordBuffer);
           end;
     end;
  if FDataset.FDatabase.FEngine.GetChangesCount = 0
     then begin
          For i := 0 to Pred(FTableDefs.Count) do
             begin
             Msg := '';
             Case PostType of
                   ptInsert: Msg := Msg + '"' + FTableDefs[i].FInsertStmtText + '" ';
                   ptUpdate: Msg := Msg + '"' + FTableDefs[i].FUpdateStmtText + '" ';
                   ptDelete: Msg := Msg + '"' + FTableDefs[i].FDeleteStmtText + '" ';
                   end;
             end;
          FDatabase.FDatabaseError.RaiseExceptionFmt('%s',[Msg3016 + ' : ' + Msg],SQLITE_CONSTRAINT, FDatabase, vlLogAndShow)
          end
     else FDataset.FDatabase.FEngine.Transaction.CommitInternalTransaction;
except
  FDataset.FDatabase.FEngine.Transaction.RollbackInternalTransaction;
  Raise;
end;
end;


{ TSqlitePassSelectStmtTableDefs }

constructor TSqlitePassSelectStmtTableDefs.Create(Owner: TSqlitePassSelectStmt);
begin
inherited Create;
FSQLStmt := Owner;
end;

destructor TSqlitePassSelectStmtTableDefs.Destroy;
begin
ClearAndFreeItems;
FSQLStmt := nil;
inherited Destroy;
end;

procedure TSqlitePassSelectStmtTableDefs.ClearAndFreeItems;
var
i: Integer;
TableDef: TSqlitePassSelectStmtTableDef;
begin
{ Clear the internal items }
for i := 0 to Pred(Count) do
  begin
  TableDef := GetItem(i);
  TableDef.Free;
  end;
{ Clear the list pointers }
inherited Clear;
end;

function TSqlitePassSelectStmtTableDefs.FindTable(
  const Value: String): TSqlitePassSelectStmtTableDef;
var
  I: Integer;
begin
  for I := 0 to Pred(Count) do
  begin
    Result := TSqlitePassSelectStmtTableDef(inherited Items[I]);
    if AnsiCompareText(Result.TableFullName, Value) = 0 then Exit;
  end;
  Result := nil;
end;

function TSqlitePassSelectStmtTableDefs.GetItem(Index: Integer): TSqlitePassSelectStmtTableDef;
begin
Result:=TSqlitePassSelectStmtTableDef(Inherited Items[Index]);
end;

procedure TSqlitePassSelectStmtTableDefs.SetItem(Index: Integer;
  const Value: TSqlitePassSelectStmtTableDef);
begin
  Put(Index,Value);
end;

{ TSqlitePassSelectStmtTableDef }

constructor TSqlitePassSelectStmtTableDef.Create(Owner: TSqlitePassSelectStmtTableDefs);
begin
 inherited Create;
 FTableDefs := Owner;
 FFieldDefs := TSqlitePassSelectStmtFieldDefs.Create(Self);
 FAutoIncFieldDefs := TSqlitePassSelectStmtFieldDefs.Create(Self);
end;


destructor TSqlitePassSelectStmtTableDef.Destroy;
begin
 UnprepareWritingSQLStmts;
 { FAutoIncFieldDefs must be cleared before calling FFieldDefs.Free }
 FAutoIncFieldDefs.Clear;
 FAutoIncFieldDefs.Free;
 FFieldDefs.Free;
 FTableDefs := nil;
 inherited Destroy;
end;

procedure TSqlitePassSelectStmtTableDef.Assign(Const Source: TSqlitePassTableDef);
begin
 FAttachedDatabase := Source.FAttachedDatabase;
 FAliasName  := Source.AliasName;
 FOnConflict := Source.OnConlict;
 FSql        := Source.Sql;
 FTableName  := Source.TableName;
 FTemporary  := Source.Temporary;
end;


{ Prepare standard Insert, Update, Delete sql statements to speed up
  engine operations. We will then just need to fill in the values when posting the data}
procedure TSqlitePassSelectStmtTableDef.PrepareWritingSQLStmts;
var
i: Integer;
FieldName, InsertStmtFields, InsertStmtValues: String;

begin
UnprepareWritingSQLStmts;

{ Insert statement }
InsertStmtFields := '';
InsertStmtValues := '';
FieldName := '';

Try
   For i := 0 to Pred(FFieldDefs.Count) do
       begin
       FieldName := FieldDefs[i].FieldName;
       FTableDefs.FSQLStmt.QuoteString(FieldName);
       InsertStmtFields := InsertStmtFields + FieldName + ', ';
       InsertStmtValues := InsertStmtValues + ':val_' + IntToStr(i) + ', ';
       end;
   System.Delete(InsertStmtFields,Length(InsertStmtFields)-1,2);
   System.Delete(InsertStmtValues,Length(InsertStmtValues)-1,2);
   FInsertStmtText := 'INSERT INTO "' + TableFullName + '" (' + InsertStmtFields +') VALUES (' + InsertStmtValues + ');';
   FUpdateStmtText := 'UPDATE "' + TableFullName + '" SET ';
   FDeleteStmtText := 'DELETE FROM "' + TableFullName + '" WHERE RowId=?;';

   with FTableDefs.FSQLStmt.FDatabase.FEngine do
        begin
        if PrepareStmt(FInsertStmt, FInsertStmtText, vlLogAndShow)
           then Include(FAvailableOperations, aoInsert);
        if PrepareStmt(FDeleteStmt, FDeleteStmtText, vlLogAndShow)
           then Include(FAvailableOperations, aoDelete);
        { Update Stmt is not prepared yet, because we need to only include
          the modified fields in the stmt }
        Include(FAvailableOperations, aoUpdate);
        end;

except
{ Exception while building statements are trapped here
  so we can continu }
On E: Exception do
end;
end;

Procedure TSqlitePassSelectStmtTableDef.UnprepareWritingSQLStmts;
begin
{ Unprepare any previously prepared statement }
With FTableDefs.FSQLStmt.FDatabase.FEngine do
     begin
     UnprepareStmt(FInsertStmt);
     UnprepareStmt(FUpdateStmt);
     UnprepareStmt(FDeleteStmt);
     end;
FInsertStmtText := '';
FUpdateStmtText := '';
FDeleteStmtText := '';
Exclude(FAvailableOperations, aoInsert);
Exclude(FAvailableOperations, aoUpdate);
Exclude(FAvailableOperations, aoDelete);
end;


{'INSERT INTO "' + TableFullName + '" (' + InsertStmtFields +') VALUES (' + InsertStmtValues + ');'; }
procedure TSqlitePassSelectStmtTableDef.PostInsert(Const RecordBuffer: PRecBuffer);
var
i: Integer;
FieldBuffer: PRecBuffer;
begin
if not (aoInsert in FAvailableOperations) then Exit;
 Try
 FTableDefs.FSQLStmt.FDatabase.FEngine.CheckResult(SqliteDbv3_clear_binding(FInsertStmt));
 for i := 0 to Pred(FieldDefs.Count) do
   With FieldDefs[i] do
        begin
        if FTableDefs.FSQLStmt.FDataset.FRecordset.FieldIsNull(RecordBuffer, FRecordFieldIndex)
           then SqliteDbv3_bind_null(FInsertStmt, Succ(i))
           else begin
                FieldBuffer := FTableDefs.FSQLStmt.FDataset.FRecordset.GetFieldValueBuffer(RecordBuffer, FRecordFieldIndex);
                BindValue(FTableDefs.FSQLStmt.FDataset.FRecordset,
                          FieldBuffer,
                          FInsertStmt,
                          Succ(i));
                end;
        end;
 FTableDefs.FSQLStmt.FDataset.FDatabase.FEngine.CheckResult(SqliteDbv3_step(FInsertStmt), vlLogAndShow);
 UpdateAutoIncValues(RecordBuffer);
 finally
 SqliteDbv3_reset(FInsertStmt);
 end;
end;

{ 'UPDATE "' + TableFullName + '" SET ' + UpdateStmtFields + ' WHERE RowId=?;' }
procedure TSqlitePassSelectStmtTableDef.PostUpdate(Const RowId: Int64; Const RecordBuffer: PRecBuffer);
var
i, StmtFieldCount: Integer;
UpdateStmtText, UpdateStmtFields: String;
FieldBuffer: PRecBuffer;

  function PrepareUpdateStmt: Boolean;
  var
  i: integer;
  begin
  Result := False;
  UpdateStmtFields := '';
  for i := 0 to Pred(FFieldDefs.Count) do
    begin
    if FTableDefs.FSQLStmt.FDataset.FRecordset.FieldWasModified(RecordBuffer, FFieldDefs[i].FRecordFieldIndex) then
       begin
       UpdateStmtFields := UpdateStmtFields + FieldDefs[i].FFieldUpdateStmt;
       Result := True;
       end;
    end;

  if Result then
     begin
     System.Delete(UpdateStmtFields,Length(UpdateStmtFields)-1,2);
     UpdateStmtText := FUpdateStmtText + UpdateStmtFields + ' WHERE RowId=?;';
     end;
  end;

begin
 if not (aoUpdate in FAvailableOperations) then Exit;
 if not PrepareUpdateStmt then Exit;
 FUpdateStmt := nil;
 StmtFieldCount := 0;
 if FTableDefs.FSQLStmt.FDatabase.FEngine.PrepareStmt(FUpdateStmt, UpdateStmtText) then
    try
    for i := 0 to Pred(FFieldDefs.Count) do
        begin
        if FTableDefs.FSQLStmt.FDataset.FRecordset.FieldWasModified(RecordBuffer, FFieldDefs[i].FRecordFieldIndex) then
           begin
           Inc(StmtFieldCount);
           With FieldDefs[i] do
                if FTableDefs.FSQLStmt.FDataset.FRecordset.FieldIsNull(RecordBuffer, FRecordFieldIndex)
                   then SqliteDbv3_bind_null(FUpdateStmt, StmtFieldCount)
                   else begin
                        FieldBuffer := FTableDefs.FSQLStmt.FDataset.FRecordset.GetFieldValueBuffer(RecordBuffer, FRecordFieldIndex);
                        BindValue(FTableDefs.FSQLStmt.FDataset.FRecordset,
                                  FieldBuffer,
                                  FUpdateStmt,
                                  StmtFieldCount);
                        end;
           end;
        end;
    FTableDefs.FSQLStmt.FDatabase.FEngine.CheckResult(SqliteDbv3_bind_int64(FUpdateStmt, Succ(StmtFieldCount), RowId));
    FTableDefs.FSQLStmt.FDatabase.FEngine.CheckResult(SqliteDbv3_step(FUpdateStmt), vlShow);
    finally
    SqliteDbv3_finalize(FUpdateStmt);
    FUpdateStmt := nil;
    end;
end;

{ 'DELETE FROM "' + TableFullName + '" WHERE RowId=?;'; }
procedure TSqlitePassSelectStmtTableDef.PostDelete(Const RowId: Int64; Const RecordBuffer: PRecBuffer);
begin
 if not (aoDelete in FAvailableOperations) then Exit;
 try
 FTableDefs.FSQLStmt.FDatabase.FEngine.CheckResult(SqliteDbv3_bind_int64(FDeleteStmt, 1, RowId));
 FTableDefs.FSQLStmt.FDatabase.FEngine.CheckResult(SqliteDbv3_step(FDeleteStmt), vlLog);
 finally
 SqliteDbv3_reset(FDeleteStmt);
 end;
end;

function TSqlitePassSelectStmtTableDef.GetCanModify: Boolean;
begin
 Result := (FAvailableOperations >= [aoInsert, aoUpdate, aoDelete]);
end;

procedure TSqlitePassSelectStmtTableDef.UpdateAutoIncValues(Const RecordBuffer: PRecBuffer);
var
i: Integer;
FieldBuffer: PRecBuffer;
begin
 FLastInsertedRowId := FTableDefs.FSQLStmt.FDataset.FDatabase.FEngine.GetLastInsertRowId;
 FTableDefs.FSQLStmt.FDataset.FRecordset.SetRowId(RecordBuffer, FTableDefNo, FLastInsertedRowId);

 for i := 0 to Pred(FAutoIncFieldDefs.Count) do
    begin
    FieldBuffer :=  FTableDefs.FSQLStmt.FDataset.FRecordset.GetFieldValueBuffer(RecordBuffer, FAutoIncFieldDefs[i].FRecordFieldIndex);
    Move(FLastInsertedRowId, FieldBuffer^, SizeOf(Int64));
    FTableDefs.FSQLStmt.FDataset.FRecordset.SetFieldNullValue(FieldBuffer, False);
  end;
end;

{ *** Conversion functions from an internal MemRecord storage to a SQLite Db Value *** }
{ Translates a value from internal buffer to an sqlite value.
  Then binds this value to parameter in INSERT - UPDATE - DELETE SQL statement }
procedure BindDummyFieldValueBufferToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
{ Just a dummy procedure to make sure TSqlitePassSelectStmtFieldDef.BindValue
  is always assigned }
end;

procedure BindWordBoolFieldValueBufferToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
IntegerValue: Integer;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.WordBoolToInteger(PWordBool(FieldValueBuffer)^, IntegerValue);
      FEngine.CheckResult(SqliteDbv3_bind_int64(PreparedStmt, PreparedStmtFieldNo, IntegerValue));
      end;
end;

{$IFDEF Delphi2009}
{$WARNINGS OFF}
{$ENDIF}
procedure BindWordBoolFieldValueBufferToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
StrValue: String;
UTF8StrValue: UTF8AnsiString;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.WordBoolToBooleanText(PWordBool(FieldValueBuffer)^, StrValue);
      FTranslator.SystemEncodingToUTF8(StrValue, UTF8StrValue);
      FEngine.CheckResult(SqliteDbv3_bind_text(PreparedStmt, PreparedStmtFieldNo, pAnsiChar(UTF8StrValue), Length(StrValue), SQLITE_TRANSIENT));
      end;
end;
{$IFDEF Delphi2009}
{$WARNINGS ON}
{$ENDIF}

procedure BindIntegerFieldValueBufferToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
 Recordset.FDataset.FDatabase.FEngine.CheckResult(SqliteDbv3_bind_int(PreparedStmt, PreparedStmtFieldNo, PInteger(FieldValueBuffer)^));
end;

procedure BindInt64FieldValueBufferToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
 Recordset.FDataset.FDatabase.FEngine.CheckResult(SqliteDbv3_bind_int64(PreparedStmt, PreparedStmtFieldNo, PInt64(FieldValueBuffer)^));
end;

procedure BindDoubleFieldValueBufferToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
 Recordset.FDataset.FDatabase.FEngine.CheckResult(SqliteDbv3_bind_double(PreparedStmt, PreparedStmtFieldNo, PDouble(FieldValueBuffer)^));
end;

procedure BindCurrencyFieldValueBufferToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
begin
 Recordset.FDataset.FDatabase.FEngine.CheckResult(SqliteDbv3_bind_Int64(PreparedStmt, PreparedStmtFieldNo, PInt64(FieldValueBuffer)^));
end;


{$IFDEF Delphi2009}
{$WARNINGS OFF}
{$ENDIF}
procedure BindDateFieldValueBufferToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
StrValue: String;
UTF8StrValue: UTF8AnsiString;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.IntegerToDateText(PInteger(FieldValueBuffer)^, StrValue);
      FTranslator.SystemEncodingToUTF8(StrValue, UTF8StrValue);
      FEngine.CheckResult(SqliteDbv3_bind_text(PreparedStmt, PreparedStmtFieldNo, pAnsiChar(StrValue), Length(StrValue), SQLITE_TRANSIENT));
      end;
end;

procedure BindTimeFieldValueBufferToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
StrValue: String;
UTF8StrValue: UTF8AnsiString;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.IntegerToTimeText(PInteger(FieldValueBuffer)^, StrValue);
      FTranslator.SystemEncodingToUTF8(StrValue, UTF8StrValue);
      FEngine.CheckResult(SqliteDbv3_bind_text(PreparedStmt, PreparedStmtFieldNo, pAnsiChar(StrValue), Length(StrValue), SQLITE_TRANSIENT));
      end;
end;


procedure BindDateTimeFieldValueBufferToSqliteValueAsText
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
StrValue: String;
UTF8StrValue: UTF8AnsiString;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.DoubleToDateTimeText(PDouble(FieldValueBuffer)^, StrValue);
      FTranslator.SystemEncodingToUTF8(StrValue, UTF8StrValue);
      FEngine.CheckResult(SqliteDbv3_bind_text(PreparedStmt, PreparedStmtFieldNo, pAnsiChar(UTF8StrValue), Length(UTF8StrValue), SQLITE_TRANSIENT));
      end;
end;
{$IFDEF Delphi2009}
{$WARNINGS ON}
{$ENDIF}


procedure BindDateTimeFieldValueBufferToSqliteValueAsJulianDateTime
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
DoubleValue: Double;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.DoubleToJulianDateTime(PDouble(FieldValueBuffer)^, DoubleValue);
      FEngine.CheckResult(SqliteDbv3_bind_double(PreparedStmt, PreparedStmtFieldNo, DoubleValue))
      end;
end;

procedure BindDateTimeFieldValueBufferToSqliteValueAsUnixDateTime
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
Int64Value: Int64;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.DoubleToUnixDateTime(PDouble(FieldValueBuffer)^, Int64Value);
      FEngine.CheckResult(SqliteDbv3_bind_int64(PreparedStmt, PreparedStmtFieldNo, Int64Value));
      end;
end;

procedure BindDateTimeFieldValueBufferToSqliteValueAsMacDateTime
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
Int64Value: Int64;
begin
 With Recordset.FDataset.FDatabase do
      begin
      FTranslator.DoubleToMacDateTime(PDouble(FieldValueBuffer)^, Int64Value);
      FEngine.CheckResult(SqliteDbv3_bind_int64(PreparedStmt, PreparedStmtFieldNo, Int64Value));
      end;
end;


procedure BindAnsiStringFieldValueBufferToSqliteValueAsUTF8
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
UTF8StrValue: UTF8AnsiString;
begin
 UTF8StrValue := SqlitePassUtils.AnsiToUTF8(Recordset.GetFieldAsAnsiString(FieldValueBuffer));
 With Recordset.FDataset.FDatabase
      do FEngine.CheckResult(SqliteDbv3_bind_text(PreparedStmt, PreparedStmtFieldNo, pAnsiChar(UTF8StrValue), Length(UTF8StrValue), SQLITE_TRANSIENT));
end;

procedure BindUTF8AnsiStringFieldValueBufferToSqliteValueAsUTF8
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
UTF8StrValue: UTF8AnsiString;
test : AnsiString;
begin
 UTF8StrValue := Recordset.GetFieldAsAnsiString(FieldValueBuffer);
 With Recordset.FDataset.FDatabase
      do FEngine.CheckResult(SqliteDbv3_bind_text(PreparedStmt, PreparedStmtFieldNo, pAnsiChar(UTF8StrValue), Length(UTF8StrValue), SQLITE_TRANSIENT));
end;

procedure BindUTF8AnsiStringFieldValueBufferToSqliteValueAsUTF16
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
  UTF16StrValue: UTF16WideString;
  test : string;
begin
  if Assigned(Recordset) then
    UTF16StrValue := SqlitePassUtils.UTF8Encode(Recordset.GetFieldAsAnsiString(FieldValueBuffer))
  else
    UTF16StrValue := SqlitePassUtils.UTF8Encode(PAnsiChar(FieldValueBuffer));

  with Recordset.FDataset.FDatabase
      do FEngine.CheckResult(SqliteDbv3_bind_text16(PreparedStmt, PreparedStmtFieldNo, pWideChar(UTF16StrValue), Length(UTF16StrValue) * 2, SQLITE_TRANSIENT));
end;

procedure BindWideStringFieldValueBufferToSqliteValueAsUTF16
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
UTF16StrValue: UTF16WideString;
begin
 UTF16StrValue := Recordset.GetFieldAsWideString(FieldValueBuffer);
 With Recordset.FDataset.FDatabase
      do FEngine.CheckResult(SqliteDbv3_bind_text16(PreparedStmt, PreparedStmtFieldNo, pWideChar(UTF16StrValue), Length(UTF16StrValue)* 2, SQLITE_TRANSIENT));
end;

procedure BindBlobFieldValueBufferToSqliteValue
          (Const Recordset: TSqlitePassRecordset;
           Const FieldValueBuffer: PRecBuffer;
           Const PreparedStmt: Pointer;
           Const PreparedStmtFieldNo: Integer);{$IFDEF HasInline} inline; {$ENDIF}
var
MemoryStream: TMemoryStream;
begin
 MemoryStream := Recordset.GetFieldAsBlob(FieldValueBuffer);
 if Assigned(MemoryStream)
    then Recordset.FDataset.FDatabase.Engine.CheckResult(
         SqliteDbv3_bind_blob(PreparedStmt,
                              PreparedStmtFieldNo,
                              MemoryStream.Memory,
                              MemoryStream.Size,
                              SQLITE_TRANSIENT));
end;


{ TSqlitePassSelectStmtFieldDef }

constructor TSqlitePassSelectStmtFieldDef.Create(
  Owner: TSqlitePassSelectStmtFieldDefs);
begin
 Inherited Create;
 FFieldDefs := Owner;
end;

destructor TSqlitePassSelectStmtFieldDef.Destroy;
begin
 BindValue := nil;
 FFieldDefs := nil;
 Inherited Destroy;
end;

function TSqlitePassSelectStmtFieldDef.GetFFieldFullName: String;
begin
 Result := FFieldDefs.FTableDef.TableName + '.' + FFieldName;
end;

procedure TSqlitePassSelectStmtFieldDef.SetFFieldName(Value: String);
begin
  inherited SetFFieldName(Value);
  FFieldUpdateStmt := Value;
  FieldDefs.FTableDef.FTableDefs.FSQLStmt.QuoteString(FFieldUpdateStmt);
  FFieldUpdateStmt := FFieldUpdateStmt +  '= ?, ';
end;


procedure TSqlitePassSelectStmtFieldDef.SetFDataType(Value: TFieldType);
begin
  inherited SetFDataType(Value);
end;


procedure TSqlitePassSelectStmtFieldDef.Assign(Const Source: TSqlitePassTableFieldDef);
begin
 inherited Assign(TSqlitePassGenericFieldDef(Source));
end;


{ TSqlitePassSelectStmtFieldDefs }

constructor TSqlitePassSelectStmtFieldDefs.Create(
  Owner: TSqlitePassSelectStmtTableDef);
begin
 Inherited Create;
 FTableDef := Owner;
end;

destructor TSqlitePassSelectStmtFieldDefs.Destroy;
begin
 { We clear Items only if FTableDef is Assigned otherwise it will be freed twice:
   TSqlitePassSelectStmtFieldDefs is used
     by TSqlitePassSelectStmtTableDef that assigns FTableDefs
     by TSqlitePassSelectStmt that does not assign FTableDefs
     but maintains a list pointing on the same FieldDefs as TSqlitePassSelectStmtTableDef
   }
 if Assigned(FTableDef) then
    begin
    ClearAndFreeItems;
    FTableDef := nil;
    end;
 Inherited Destroy;
end;

procedure TSqlitePassSelectStmtFieldDefs.ClearAndFreeItems;
var
i: Integer;
begin
{ Clears the internal items }
for i := 0 to Pred(Count)
    do Items[i].Free;
{ Clears the list pointers }
inherited Clear;
end;

function TSqlitePassSelectStmtFieldDefs.GetItem(Index: Integer): TSqlitePassSelectStmtFieldDef;
begin
  Result:=TSqlitePassSelectStmtFieldDef(Inherited Items[Index]);
end;

procedure TSqlitePassSelectStmtFieldDefs.SetItem(Index: Integer;
  const Value: TSqlitePassSelectStmtFieldDef);
begin
  Put(Index,Value);
end;

procedure TSqlitePassSelectStmtFieldDefs.Assign(Const Source: TSqlitePassTableFieldDefs);
var
i: integer;
NewFieldDef: TSqlitePassSelectStmtFieldDef;
begin
for i := 0 to Pred(Source.Count) do
   begin
   NewFieldDef := TSqlitePassSelectStmtFieldDef.Create(Self);
   NewFieldDef.Assign(Source[i]);
   Self.Add(NewFieldDef);
   end;
end;

function TSqlitePassSelectStmtFieldDefs.FieldDefByName(
  const Value: String): TSqlitePassSelectStmtFieldDef;
begin
  Result := FindFieldDef(Value);
  if Result = nil then
    DatabaseErrorFmt(Msg3910, [Value], nil) // TODO FTableDef.FTableDefs.FSQLStmt.FDataset.FDatabase); = Nil ...
end;

function TSqlitePassSelectStmtFieldDefs.FindFieldDef(
  const Value: String): TSqlitePassSelectStmtFieldDef;
var
  i: Integer;
begin
  for i := 0 to Pred(Count) do
  begin
    Result := TSqlitePassSelectStmtFieldDef(inherited Items[i]);
    if (AnsiCompareText(Result.FieldName, Value) = 0)
    or (AnsiCompareText(Result.FieldFullName, Value)= 0)
       then Exit;
  end;
  Result := nil;
end;


{ TSqlitePassSQLToken }

constructor TSqlitePassSQLToken.Create;
begin
 TokenType := ttUnknown;
end;

function TSqlitePassSQLToken.TokenTypeAsText: String;
begin
 Result := SqlitePassSQLTokenTypeAsString[Ord(TokenType)];
end;


{ TSqlitePassSQLTokens }

procedure TSqlitePassSQLTokens.ClearAndFreeItems;
var
i: Integer;
begin
{ Clear the internal items }
for i := 0 to Pred(Count) do
    begin
    {$IFDEF DEBUG_SQLStmt}
     SqlitePassUtils.OutputDebugString('TSqlitePassSQLTokens.ClearAndFreeItems : [' + IntToStr(Items[i].Id) + '=' + Items[i].Text +']');
    {$ENDIF}
    Items[i].Free;
    end;
{ Clear the list pointers }
inherited Clear;
end;

destructor TSqlitePassSQLTokens.Destroy;
begin
ClearAndFreeItems;
Inherited;
end;

function TSqlitePassSQLTokens.GetItem(Index: Integer): TSqlitePassSQLToken;
begin
 Result := TSqlitePassSQLToken(Inherited Items[Index]);
end;

procedure TSqlitePassSQLTokens.SetItem(Index: Integer;
  const Value: TSqlitePassSQLToken);
begin
  Put(Index,Value);
end;



{ TSqlitePassTokenizer }

constructor TSqlitePassSQLTokenizer.Create;
begin
  Inherited;
  FCurrentTokenIndex := -1;
  FNestingLevel := 0;
  FTokens := TSqlitePassSQLTokens.Create;
end;

destructor TSqlitePassSQLTokenizer.Destroy;
begin
  FTokens.Free;
  Inherited;
end;

function TSqlitePassSQLTokenizer.First: Boolean;
begin
 if FTokens.Count > 0
    then FCurrentTokenIndex := 0
    else FCurrentTokenIndex := -1;
 Result := (FCurrentTokenIndex <> -1);
end;

function TSqlitePassSQLTokenizer.GetFCount: Integer;
begin
 Result := FTokens.Count;
end;

function TSqlitePassSQLTokenizer.GetFCurrentToken: TSqlitePassSQLToken;
begin
  if (FCurrentTokenIndex > -1) and( FCurrentTokenIndex < FTokens.Count)
     then Result := FTokens[FCurrentTokenIndex]
     else Result := nil;
end;

function TSqlitePassSQLTokenizer.GetFEOF: Boolean;
begin
  Result := (FTokens.Count = 0) or ((FTokens.Count > 0) and (FCurrentTokenIndex = FTokens.Count));
end;

function TSqlitePassSQLTokenizer.GetFText: String;
var
i: Integer;
begin
if FTokens.Count > 0 then
   begin
   Result := '';
   for i := 0 to Pred(FTokens.Count)
       do Result := Result + FTokens[i].Text;
   end;
end;

procedure TSqlitePassSQLTokenizer.InsertAfter(TokenText, InsertedStmt: String);
var
NewToken: TSqlitePassSQLToken;
begin
If Locate(TokenText) then
   begin
   NewToken := TSqlitePassSQLToken.Create;
   NewToken.Text := InsertedStmt;
   FTokens.Insert(Succ(FCurrentTokenIndex), NewToken);
  end;
end;

procedure TSqlitePassSQLTokenizer.InsertBefore(TokenText,InsertedStmt: String);
var
NewToken: TSqlitePassSQLToken;
begin
If Locate(TokenText) then
   begin
   NewToken := TSqlitePassSQLToken.Create;
   NewToken.Text := InsertedStmt;
   FTokens.Insert(FCurrentTokenIndex, NewToken);
  end;
end;

function TSqlitePassSQLTokenizer.Last: Boolean;
begin
 FCurrentTokenIndex := Pred(FTokens.Count);
 Result := (FCurrentTokenIndex <> -1);
end;

function TSqlitePassSQLTokenizer.Locate(TokenText: String): Boolean;
var
i: Integer;
begin
 Result := False;
 for i := 0 to Pred(FTokens.Count) do
     if AnsiCompareText(FTokens[i].Text, TokenText) = 0 then
        begin
        FCurrentTokenIndex := i;
        Result := True;
        Break;
        end;
end;

function TSqlitePassSQLTokenizer.Locate(TokenType: TSqlitePassSQLTokenType): Boolean;
var
i: Integer;
begin
 Result := False;
 for i := 0 to Pred(FTokens.Count) do
     if FTokens[i].TokenType = TokenType then
        begin
        FCurrentTokenIndex := i;
        Result := True;
        Break;
        end;
end;

function TSqlitePassSQLTokenizer.Next(TokenType: TSqlitePassSQLTokenType): Boolean;
begin
Result := False;
while FCurrentTokenIndex < FTokens.Count do // EOF
  begin
  Inc(FCurrentTokenIndex);
  if Assigned(Token) and (Token.TokenType = TokenType) then
     begin
     Result := True;
     Break;
     end;
  end;
end;

function TSqlitePassSQLTokenizer.Next: Boolean;
begin
Result := False;
//if FCurrentTokenIndex < Pred(FTokens.Count) then
if FCurrentTokenIndex < FTokens.Count then { In order to set EOF }
   begin
   Inc(FCurrentTokenIndex);
   Result := True;
   end;
end;

function TSqlitePassSQLTokenizer.Prior(
  TokenType: TSqlitePassSQLTokenType): Boolean;
begin
Result := False;
while FCurrentTokenIndex > 0 do
  begin
  Dec(FCurrentTokenIndex);
  if FTokens[FCurrentTokenIndex].TokenType = TokenType then
     begin
     Result := True;
     Break;
     end;
  end;
end;

function TSqlitePassSQLTokenizer.Prior: Boolean;
begin
Result := False;
if FCurrentTokenIndex > 0 then
   begin
   Dec(FCurrentTokenIndex);
   Result := True;
   end;
end;

procedure TSqlitePassSQLTokenizer.Remove(TokenText: String);
begin
 If Locate(TokenText) then
    begin
    FTokens.Delete(FCurrentTokenIndex);
    Dec(FCurrentTokenIndex);
    end;
end;

procedure TSqlitePassSQLTokenizer.RemoveAfter(TokenText: String);
begin
If Locate(TokenText) and (FCurrentTokenIndex < Pred(FTokens.Count))
   then FTokens.Delete(Succ(FCurrentTokenIndex));
end;

procedure TSqlitePassSQLTokenizer.RemoveBefore(TokenText: String);
begin
If Locate(TokenText) and (FCurrentTokenIndex > 0) then
   begin
   FTokens.Delete(Pred(FCurrentTokenIndex));
   Dec(FCurrentTokenIndex);
   end;
end;

procedure TSqlitePassSQLTokenizer.Replace(OldText, NewText: String);
begin
 If Locate(OldText) then FTokens[FCurrentTokenIndex].Text := NewText;
end;

procedure TSqlitePassSQLTokenizer.SetFText(const Value: String);
begin
FText := Value;
Tokenize;
First;
end;

{ Returns the substring from then current position to the searched token type }
function TSqlitePassSQLTokenizer.GetTextAfter(TokenType: TSqlitePassSQLTokenType): String;
var
i, Index: Integer;
begin
  Index := Succ(FCurrentTokenIndex);
  Result := '';
  for i := Index to Pred(FTokens.Count) do
      begin
      if FTokens[i].TokenType = TokenType
         then Break;
      Result := Result + FTokens[i].Text;
      end;
end;

{ Returns the substring from then current position downto the searched token type }
function TSqlitePassSQLTokenizer.GetTextBefore(TokenType: TSqlitePassSQLTokenType): String;
var
i, Index: Integer;
begin
  Index := Pred(FCurrentTokenIndex);
  Result := '';
  for i := Index Downto 0 do
      begin
      if FTokens[i].TokenType = TokenType
         then Break;
      Result := FTokens[i].Text + Result;
      end;
end;

Procedure TSqlitePassSQLTokenizer.Clean(Options: TSqlitePassSQLTokenizerCleanOptions);
var
i: Integer;
begin
For i := Pred(Count) Downto 0 do
    if FTokens[i].TokenType in Options
       then begin
            FTokens[i].Free;
            FTokens.Delete(i);
            end;
First;
end;

procedure TSqlitePassSQLTokenizer.Tokenize(Options: TSqlitePassSQLTokenizerOptions);
var
CurrentChar, TokenStart: PChar;
NewToken: TSqlitePassSQLToken;

  { Sub-procedure to check the nesting stack state }
  Procedure CheckNestingStack;
  var
  LevelCompleted: Boolean;
  begin
  if FNestingLevel < 0 then Exit;

  Case CurrentChar^ of
     '''' : LevelCompleted := FNestingCharStack[FNestingLevel] = '''';
     '`'  : LevelCompleted := FNestingCharStack[FNestingLevel] = '`';
     '"'  : LevelCompleted := FNestingCharStack[FNestingLevel] = '"';
     '('  : LevelCompleted := FNestingCharStack[FNestingLevel] = ')';
     '['  : LevelCompleted := FNestingCharStack[FNestingLevel] = ']';
     '{'  : LevelCompleted := FNestingCharStack[FNestingLevel] = '}';
     '#'  : LevelCompleted := FNestingCharStack[FNestingLevel] = '#';
     else LevelCompleted := False;
     end;

  if LevelCompleted
     then begin
          FNestingCharStack[FNestingLevel] := #0;
          Dec(FNestingLevel);
          end
     else begin
          Inc(FNestingLevel);
          FNestingCharStack[FNestingLevel] := CurrentChar^;
          end;
   end;

  { List of Sub-Procedures to handle specialized processing depending on the token kind}
  Procedure ProcessEOF;
  begin
    NewToken.TokenType := ttEOF;
  end;

  Procedure ProcessLF;
  begin
   NewToken.TokenType := ttLF;
   Inc(CurrentChar);
  end;

  Procedure ProcessCR;
  begin
   NewToken.TokenType := ttCR;
   Inc(CurrentChar);
  end;

  Procedure ProcessSpace;
  begin
    NewToken.TokenType := ttWhiteSpace;
    Inc(CurrentChar);
  end;

  Procedure ProcessNumber;
  var
  CharSet: Set of Char;
  begin
  if Char(FNestingCharStack[FNestingLevel]) in ['''', '"', '#']
     then begin
          { We have DateTime, Date or Time ? }
          CharSet := ['0'..'9', '.', '-', '/', ':', ' '];
          NewToken.TokenType := ttDateTime;
          end
     else begin
          { We have number ? }
          CharSet := ['0'..'9', '.'];
          NewToken.TokenType := ttNumber;
          end;
  While (Char(CurrentChar^) in CharSet) do Inc(CurrentChar);
  end;

  Procedure ProcessIdentifier;
  var
  TempStr: String;

    function IsKeyword: Boolean;
    var
    i, sLength: Integer;
    TempText: String;
    NextChar: Char;
    begin
    Result := False;
    { Checks if the token is a SQL Keyword }
    i := 0;
    Repeat
      sLength := Length(SelectStmtKeywords[i]);
      SetString(TempText, TokenStart, sLength);
      NextChar := PChar(CurrentChar + sLength)^;
      if (AnsiCompareText(TempText, SelectStmtKeywords[i]) = 0)
         and (NextChar in AfterIdentifierSeparator) then
              begin
              Result := True;
              Inc(CurrentChar, sLength);
              Break;
              end
         else Inc(i);
    Until i = High(SelectStmtKeywords);
    end;

  begin
    if IsKeyword
       then NewToken.TokenType := ttKeyword
       else begin
            Case (CurrentChar-1)^ of
               { Checks for quoted identifiers }
               '[' : While (CurrentChar^ <> ']')  and (CurrentChar^ <> #0) and ((CurrentChar+1)^ <> '.') do Inc(CurrentChar);
               '''': While (CurrentChar^ <> '''') and (CurrentChar^ <> #0) and ((CurrentChar+1)^ <> '.') do Inc(CurrentChar);
               '`':  While (CurrentChar^ <> '`')  and (CurrentChar^ <> #0) and ((CurrentChar+1)^ <> '.') do Inc(CurrentChar);
               '"' : While (CurrentChar^ <> '"')  and (CurrentChar^ <> #0) and ((CurrentChar+1)^ <> '.') do Inc(CurrentChar);
               else  While (CurrentChar^ in IdentifierChar) do Inc(CurrentChar);
            end; {Case}
            NewToken.TokenType := ttIdentifier;
            { We check for 'Or' or 'And' (needed by filter parser }
            SetString(TempStr, TokenStart, CurrentChar-TokenStart);
            TempStr := Uppercase(TempStr);
                 if TempStr = 'OR'   then NewToken.TokenType := ttOr
            else if TempStr = 'AND'  then NewToken.TokenType := ttAnd
            else if TempStr = 'NULL' then NewToken.TokenType := ttNull
            else if TempStr = 'NOTNULL' then NewToken.TokenType := ttNotNull;
            end;
  end;

  Procedure ProcessBraceOpen;
  begin
  NewToken.TokenType := ttBraceOpen;
  CheckNestingStack;
  Inc(CurrentChar);
  end;

  Procedure ProcessBraceClose;
  begin
  NewToken.TokenType := ttBraceClose;
  Inc(CurrentChar);
  end;

  Procedure ProcessRoundOpen;
  begin
  NewToken.TokenType := ttRoundOpen;
  CheckNestingStack;
  Inc(CurrentChar);
  end;

  Procedure ProcessRoundClose;
  begin
  NewToken.TokenType := ttRoundClose;
  Inc(CurrentChar);
  end;

  Procedure ProcessStar;
  begin
  NewToken.TokenType := ttStar;
  Inc(CurrentChar);
  end;

  Procedure ProcessPercent;
  begin
  NewToken.TokenType := ttPercent;
  Inc(CurrentChar);
  end;

  Procedure ProcessPlus;
  begin
  NewToken.TokenType := ttPlus;
  Inc(CurrentChar);
  end;

  Procedure ProcessComma;
  begin
  NewToken.TokenType := ttComma;
  Inc(CurrentChar);
  end;

  Procedure ProcessMinus;
  begin
  NewToken.TokenType := ttMinus;
  Inc(CurrentChar);
  end;

  Procedure ProcessDot;
  begin
  NewToken.TokenType := ttDot;
  Inc(CurrentChar);
  end;

  Procedure ProcessSlash;
  begin
  NewToken.TokenType := ttSlash;
  Inc(CurrentChar);
  end;

  Procedure ProcessColon;
  begin
  NewToken.TokenType := ttColon;
  Inc(CurrentChar);
  end;

  Procedure ProcessSemiColon;
  begin
  NewToken.TokenType := ttSemiColon;
  Inc(CurrentChar);
  end;

  Procedure ProcessLower;
  begin
  case (CurrentChar+1)^ of
    '=':
      begin
        Inc(CurrentChar, 2);
        NewToken.TokenType := ttLesserOrEqual;
      end;
    '>':
      begin
        Inc(CurrentChar, 2);
        NewToken.TokenType := ttNotEqual;
      end
  else
    begin
      Inc(CurrentChar);
      NewToken.TokenType := ttLesser;
    end;
  end;
  end;

  Procedure ProcessEqual;
  begin
  NewToken.TokenType := ttEqual;
  Inc(CurrentChar);
  end;

  Procedure ProcessGreater;
  begin
  If (CurrentChar+1)^ = '='
      then begin
      NewToken.TokenType := ttGreaterOrEqual;
      Inc(CurrentChar, 2);
      end
      else begin
      NewToken.TokenType := ttGreater;
      Inc(CurrentChar);
      end;
  end;

  Procedure ProcessSquareOpen;
  begin
  NewToken.TokenType := ttSquareOpen;
  CheckNestingStack;
  Inc(CurrentChar);
  end;

  Procedure ProcessSquareClose;
  begin
  NewToken.TokenType := ttSquareClose;
  Inc(CurrentChar);
  end;

  Procedure ProcessSingleQuote;
  begin
  NewToken.TokenType := ttSingleQuote;
  CheckNestingStack;
  Inc(CurrentChar);
  end;

  Procedure ProcessDoubleQuote;
  begin
  NewToken.TokenType := ttDoubleQuote;
  CheckNestingStack;
  Inc(CurrentChar);
  end;

  Procedure ProcessGraveQuote;
  begin
  NewToken.TokenType := ttGraveQuote;
  CheckNestingStack;
  Inc(CurrentChar);
  end;

  Procedure ProcessHash;
  begin
  NewToken.TokenType := ttHash;
  CheckNestingStack;
  Inc(CurrentChar);
  end;

  Procedure ProcessSeparator;
  begin
  NewToken.TokenType := ttSeparator;
  Inc(CurrentChar);
  end;

  Procedure ProcessUnknown;
  begin
  NewToken.TokenType := ttUnknown;
  Inc(CurrentChar);
  end;

{ Main procedure "Tokenize" }
begin
FTokens.ClearAndFreeItems;
if FText = '' then Exit;
CurrentChar := PChar(FText);

while Not (CurrentChar^ = #0) do
 begin
 { Add a new token to the token list }
 NewToken := TSqlitePassSQLToken.Create;
 FTokens.Add(NewToken);
 
 {$IFDEF DEBUG_SQLStmt}
 NewToken.Id := GetTickCount + FTokens.Count;
 {$ENDIF}

 TokenStart:= CurrentChar;

 case CurrentChar^ of
   #0:  ProcessEOF;
   #10: ProcessLF;
   #13: ProcessCR;
   #1..#9, #11, #12, #14..#32: ProcessSpace;
   '0'..'9': ProcessNumber;
   '{': ProcessBraceOpen;
   '}': ProcessBraceClose;
   '(': ProcessRoundOpen;
   ')': ProcessRoundClose;
   '*': ProcessStar;
   '%': ProcessPercent;
   '+': ProcessPlus;
   ',': ProcessComma;
   '-': ProcessMinus;
   '.': ProcessDot;
   '/': ProcessSlash;
   ':': ProcessColon;
   ';': ProcessSemiColon;
   '<': ProcessLower;
   '=': ProcessEqual;
   '>': ProcessGreater;
   '[': ProcessSquareOpen;
   ']': ProcessSquareClose;
   '''': ProcessSingleQuote;
   '"': ProcessDoubleQuote;
   '`': ProcessGraveQuote;
   '#': ProcessHash;
   '|': ProcessSeparator;
   else ProcessIdentifier;  //'A'..'Z', 'a'..'z', '_', 'é', 'è', 'ê' ... : ProcessIdentifier;
   end;
   SetString(NewToken.Text, TokenStart, CurrentChar-TokenStart);
   NewToken.NestingLevel := FNestingLevel;

   {$IFDEF DEBUG_SQLStmt}
   SqlitePassUtils.OutputDebugString('NewToken : ['+IntToStr(NewToken.Id) + '=' + NewToken.Text +']');
   {$ENDIF}

end;
First;
end;


