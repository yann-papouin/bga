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

    Basic sqlite library wrapper for use with fpc compiler
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006 - 2010

    portions of initial code coming from Mike Cariotoglou work.
    Email: mike@deltasingular.gr

    Major changes are indicated in the \Documentation\Changes.pdf file
    Last update 23.05.2010

  --------------------------------------------------------------------------- }

{.$DEFINE DEBUG_SQLITEPASS}


{ ----- Utilities ----- }
(*function _ExecCallback(cbParam: PSqlitePassExecCallBack; Columns: Integer;
                       ColumnValues, ColumnNames: Ppchar): integer; cdecl;
begin
try
// Result:=cbparam^(columns,columnValues,columnNames);
except
 result:=1;
end;
end; *)


{ TSqlitePassEngine }

constructor TSqlitePassEngine.Create(Database: TSqlitePassDatabase);
begin
FDatabase := Database;
FLibraryLoaded := False;
FLibraryHandle := 0;
FConnectionHandle:= nil;
FQueryTimeOut := 0;
FTransaction := TSqlitePassTransaction.Create(Self);
{$IFDEF DEBUG_SQLITEPASS}
FPreparedStmts := TStringList.Create;
{$ENDIF}
end;

destructor TSqlitePassEngine.Destroy;
begin
CloseDatabase;
{$IFDEF DEBUG_SQLITEPASS}
FPreparedStmts.Free;
{$ENDIF}
FTransaction.Free;
FDatabase := nil;
inherited Destroy;
end;

function TSqlitePassEngine.CheckResult(Const i: Integer; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Integer;
var
ErrorMsg: String;
begin
Result := i;
If not (i in [SQLITE_OK, SQLITE_DONE]) then
   begin
   Case FDatabase.SystemEncoding of
     sysANSI  : ErrorMsg := SqlitePassUtils.UTF8ToAnsi(SqliteDbv3_Errmsg(FConnectionHandle));
     sysUTF8  : ErrorMsg := SqliteDbv3_Errmsg(FConnectionHandle);
     sysUTF16 : ErrorMsg := SqliteDbv3_Errmsg16(FConnectionHandle);
     end;
   FDatabase.FDatabaseError.RaiseExceptionFmt('%d: %s',[i,ErrorMsg], i, FDatabase, VerboseLevel);
   end;
end;

{$IFDEF Delphi2009}
{$WARNINGS OFF}
{$ENDIF}
function TSqlitePassEngine.ExecSQL(Const Sql: String; CallBackFunc: TSqlitePassExecCallBack = nil): Integer;
var
SQLText: UTF8AnsiString;
begin
  {SqliteDbv3_Exec is Utf8 only, SQLText is encoded to Utf8 if needed}
  FDatabase.FTranslator.SystemEncodingToUTF8(sql, SQLText);
  repeat
  // TODO
  (*if Assigned(CallBackFunc)
      then result:= SqliteDbv3_Exec(FConnectionHandle,Pchar(Sql),@_ExecCallBack,@@CallBackFunc,nil)
      else*)
  Result:= CheckResult(SqliteDbv3_Exec(FConnectionHandle,pUTF8AnsiString(SQLText),nil,nil,nil));
  until (result=SQLITE_OK) or not (result in [SQLITE_BUSY,SQLITE_LOCKED])
        or ((FQueryTimeOut>=0) and (Integer(GetTickCount)>=FStartTime));
end;
{$IFDEF Delphi2009}
{$WARNINGS ON}
{$ENDIF}

procedure TSqlitePassEngine.ExecQueryUTF8(Const Sql: UTF8AnsiString; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
var
PSQLText, pRemainingSql: pAnsiChar;
StmtPtr: Pointer;
Status: Integer;
SQLText: AnsiString;
begin
SetString(SqlText, PAnsiChar(Sql), Length(Sql));
PSQLText := PAnsiChar(SqlText);  //DW
PRemainingSQL := PSQLText;
StmtPtr := nil;

While (PSQLText <> nil) do
  begin
  CheckResult(SqliteDbv3_prepare_v2(FConnectionHandle, PSQLText, -1, StmtPtr, PRemainingSQL), VerboseLevel);

    Repeat
    Status := CheckResult(SqliteDbv3_step(StmtPtr), VerboseLevel);
    Until (Status=SQLITE_OK) or not (Status in [SQLITE_BUSY,SQLITE_LOCKED])
           or ((FQueryTimeOut>=0) and (Integer(GetTickCount)>=FStartTime));

  CheckResult(SqliteDbv3_Finalize(StmtPtr), VerboseLevel);
  PSQLText := PRemainingSQL;
  end;
end;

procedure TSqlitePassEngine.ExecQueryUTF16(Const Sql: UTF16WideString; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
var
PSql, pRemainingSql: pWideChar;
StmtPtr: Pointer;
Status: Integer;
begin
PSql := PWideChar(Sql);
PRemainingSQL := PSql;
StmtPtr := nil;

While (PSql <> nil) do
  begin
  CheckResult(SqliteDbv3_prepare16_v2(FConnectionHandle, PSql, -1, StmtPtr, PRemainingSQL), VerboseLevel);

    Repeat
    Status := CheckResult(SqliteDbv3_step(StmtPtr), VerboseLevel);
    Until (Status = SQLITE_OK) or not (Status in [SQLITE_BUSY,SQLITE_LOCKED])
           or ((FQueryTimeOut>=0) and (Integer(GetTickCount)>=FStartTime));

  CheckResult(SqliteDbv3_Finalize(StmtPtr), VerboseLevel);
  PSql := PRemainingSQL;
  end;
end;

procedure TSqlitePassEngine.ExecQuery(Const Sql: String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
begin
 Case FDatabase.SystemEncoding of
       sysANSI  : ExecQueryUTF8 (SqlitePassUtils.AnsiToUTF8(AnsiString(Sql)), VerboseLevel);
       sysUTF8  : ExecQueryUTF8 (AnsiString(Sql), VerboseLevel);
       sysUTF16 : ExecQueryUTF16(Sql, VerboseLevel);
       end;
end;


function TSqlitePassEngine.OpenDatabase(FullName: String; Const LibraryFile: String): Boolean;
begin
FLibraryLoaded := LoadSqliteLibrary(LibraryFile);
{$IFDEF UNIX}
if Not FLibraryLoaded
   then FLibraryHandle := LoadLibrary(PChar(DefaultSQLitePath + DefaultSQLiteLibrary));
{$ENDIF}
UniqueString(FullName);
Case FDatabase.SystemEncoding of
     sysANSI  : CheckResult(SqliteDbv3_Open(pAnsiChar(SqlitePassUtils.AnsiToUTF8(FullName)), FConnectionHandle, SQLITE_OPEN_READWRITE, ''));
     sysUTF8  : CheckResult(SqliteDbv3_Open(pAnsiChar(AnsiString(FullName)), FConnectionHandle, SQLITE_OPEN_READWRITE, ''));
     sysUTF16 : CheckResult(SqliteDbv3_Open16(pWideChar(FullName), FConnectionHandle));
     end;
Result := FLibraryLoaded;
end;

procedure TSqlitePassEngine.CloseDatabase;
begin
If Assigned(FConnectionHandle) then
   begin
   Transaction.CommitAll;
   {$IFDEF DEBUG_SQLITEPASS}
   FinalizePendingStmts;
   {$ENDIF}
   if CheckResult(SqliteDbv3_Close(FConnectionHandle)) in [SQLITE_OK, SQLITE_DONE] then
      begin
      FConnectionHandle := nil;
      Dec(EnginesCount);
      end;
   end;
if FLibraryLoaded and (EnginesCount = 0) then
   begin
   FreeLibrary(FLibraryHandle);
   FLibraryLoaded := False;
   end;
end;

Function TSqlitePassEngine.LoadSqliteLibrary(LibraryFile: String): Boolean;
begin
if LibraryFile = ''
   then LibraryFile := DefaultSqliteLibrary;

{ The library is already loaded and we try to load the same file again}
Result := FLibraryLoaded and (LibraryFile = FLibraryFile);

if Not Result then
   begin
   if FLibraryLoaded
      then FreeLibrary(FLibraryHandle);

   FLibraryFile := LibraryFile;
   {$IFDEF MSWINDOWS}
     FLibraryHandle := LoadLibrary(PChar(FLibraryFile));
   {$ELSE}
     FLibraryHandle := LoadLibrary(FLibraryFile);
   {$ENDIF}
   Result := FLibraryHandle <> 0;
   if Result then Try
                  LoadFunctions;
                  Except
                  Result := False;
                  end
             else FDatabase.DatabaseError.RaiseExceptionFmt(Msg3001, [FLibraryFile], vlLog);
   end;          
end;


function TSqlitePassEngine.PrepareStmt(var Stmt: pointer; Sql: String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Boolean;
var
{$IFDEF DEBUG_SQLITEPASS}
i: Integer;
{$ENDIF}
WStr : WideString;
RemainingSql: Pointer;
begin
 RemainingSql := nil;
 UnprepareStmt(Stmt);
 { We 'have to' get a unique String before sending the PChar to the sqlite lib, otherwise
   we could get unpredictable results and memory corruption...}
 UniqueString(Sql);
 Case FDatabase.SystemEncoding of
      sysANSI  : Result := (CheckResult(SqliteDbv3_prepare_v2(FConnectionHandle,pAnsiChar(SqlitePassUtils.AnsiToUtf8(AnsiString(Sql))),-1,Stmt,PAnsiChar(RemainingSql)), VerboseLevel) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF8  : Result := (CheckResult(SqliteDbv3_prepare_v2(FConnectionHandle,pAnsiChar(AnsiString(Sql)),-1,Stmt,PAnsiChar(RemainingSql)), VerboseLevel) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF16 : Result := (CheckResult(SqliteDbv3_prepare16_v2(FConnectionHandle,pWideChar(Sql),-1,Stmt,PWideChar(RemainingSql)), VerboseLevel) in [SQLITE_OK, SQLITE_DONE]);
      else Result := False;
      end;

 {$IFDEF DEBUG_SQLITEPASS}
 If Result then
    begin
    i := FPreparedStmts.IndexOfName(IntToStr(Integer(Stmt)));
    if i > -1
       then FPreparedStmts.Strings[i] := IntToStr(Integer(Stmt)) + '=' + Sql
       else FPreparedStmts.Add(IntToStr(Integer(Stmt)) + '=' + Sql);
    end;
 {$ENDIF}
end;

function TSqlitePassEngine.UnprepareStmt(var Stmt: pointer): Boolean;
{$IFDEF DEBUG_SQLITEPASS}
var
i: Integer;
{$ENDIF}
begin
Result := True;
if FConnectionHandle = nil then exit;
if Assigned(Stmt)
   then begin
        try
        Result := (CheckResult(SqliteDbv3_Finalize(Stmt)) in [SQLITE_OK, SQLITE_DONE]);
        {$IFDEF DEBUG_SQLITEPASS}
        i := FPreparedStmts.IndexOfName(IntToStr(Integer(Stmt)));
        if i > -1
           then FPreparedStmts.Delete(i);
        {$ENDIF}
        finally
        Stmt := nil;
        end;
        end;
end;

{ TODO : Workaround to cleanup unused 'garbage' statements
  These statements should not exist, but...I have got to check my code... }
procedure TSqlitePassEngine.FinalizePendingStmts;
//var
//Stmt: Pointer;
begin
// TODO Check this
(*While FPreparedStmts.Count > 0 do
  begin
  Stmt := Pointer(StrToInt(FPreparedStmts.Names[0]));
  UnPrepareStmt(Stmt);
  end;*)
end;

function TSqlitePassEngine.GetChangesCount: Integer;
begin
Result := SqliteDbv3_changes(FConnectionHandle);
end;

function TSqlitePassEngine.GetTotalChangesCount: Integer;
begin
Result := SqliteDbv3_changes(FConnectionHandle);
end;

function TSqlitePassEngine.GetLastInsertRowId: Int64;
begin
Result := SqliteDbv3_last_insert_rowid(FConnectionHandle);
end;

{ User Defined Functions}
function TSqlitePassEngine.CreateFunction(FuncName: String; ArgCount: ShortInt;  DefaultEncoding: Byte;
                                          UserData: Pointer; xFunc, xStep: TFuncHandler; xFinal: TFuncFinalizer): Boolean;
begin
 { We 'have to' get a unique String before sending the PChar to the sqlite lib, otherwise
   we could get unpredictable results and memory corruption...}
 UniqueString(FuncName);

 Case FDatabase.SystemEncoding of
      sysANSI  : Result := (CheckResult(SqliteDbv3_create_function(FConnectionHandle,
                                                                   pAnsiChar(SqlitePassUtils.AnsiToUtf8(AnsiString(FuncName))),
                                                                   ArgCount, DefaultEncoding, UserData,
                                                                   xFunc, xStep, xFinal)) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF8  : Result := (CheckResult(SqliteDbv3_create_function(FConnectionHandle,
                                                                   pAnsiChar(AnsiString(FuncName)),
                                                                   ArgCount, DefaultEncoding, UserData,
                                                                   xFunc, xStep, xFinal)) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF16 : Result := (CheckResult(SqliteDbv3_create_function16(FConnectionHandle,
                                                                     pWideChar(FuncName),
                                                                     ArgCount, DefaultEncoding, UserData,
                                                                     xFunc, xStep, xFinal)) in [SQLITE_OK, SQLITE_DONE]);
      else Result := False;
      end;
end;

{ Library Function loading }

{ This function is called by TSqlitePassEngine to load library functions }
function TSqlitePassEngine.LoadFunctions: Boolean;
var
ErrorList: TStringList;

  function LoadFunction(FunctionName: String):Pointer;
  begin
  Result:=GetProcAddress(FLibraryHandle,pChar(FunctionName));
  if not Assigned(Result)
     then ErrorList.Add(Format(Msg3000,[FunctionName]));
  end;

begin
Inc(EnginesCount);
ErrorList := TStringList.Create;
Try
@SqliteDbv3_SqliteLibVersion     := LoadFunction('sqlite3_libversion');
@SqliteDbv3_SqliteLibVersionNumber := LoadFunction('sqlite3_libversion_number');
@SqliteDbv3_SqliteLibSourceId    := LoadFunction('sqlite3_sourceid');
@SqliteDbv3_compileoption_used   := LoadFunction('sqlite3_compileoption_used');
@SqliteDbv3_close                := LoadFunction('sqlite3_close');
@SqliteDbv3_exec                 := LoadFunction('sqlite3_exec');
@SqliteDbv3_sql                  := LoadFunction('sqlite3_sql');
@SqliteDbv3_last_insert_rowid    := LoadFunction('sqlite3_last_insert_rowid');
@SqliteDbv3_changes              := LoadFunction('sqlite3_changes');
@SqliteDbv3_total_changes        := LoadFunction('sqlite3_total_changes');
@SqliteDbv3_interrupt            := LoadFunction('sqlite3_interrupt');
@SqliteDbv3_complete             := LoadFunction('sqlite3_complete');
@SqliteDbv3_busy_handler         := LoadFunction('sqlite3_busy_handler');
@SqliteDbv3_busy_timeout         := LoadFunction('sqlite3_busy_timeout');
@SqliteDbv3_free                 := LoadFunction('sqlite3_free');
@SqliteDbv3_open                 := LoadFunction('sqlite3_open');
@SqliteDbv3_open16               := LoadFunction('sqlite3_open16');
@SqliteDbv3_errcode              := LoadFunction('sqlite3_errcode');
@SqliteDbv3_errmsg               := LoadFunction('sqlite3_errmsg');
@SqliteDbv3_errmsg16             := LoadFunction('sqlite3_errmsg16');

@SqliteDbv3_prepare_v2           := LoadFunction('sqlite3_prepare_v2');
@SqliteDbv3_prepare16_v2         := LoadFunction('sqlite3_prepare16_v2');

@SqliteDbv3_get_table            := LoadFunction('sqlite3_get_table');
@SqliteDbv3_free_table           := LoadFunction('sqlite3_free_table');
{ binding values to parameters }
@SqliteDbv3_clear_binding        := LoadFunction('sqlite3_clear_bindings');
@SqliteDbv3_bind_parameter_index := LoadFunction('sqlite3_bind_parameter_index');
@SqliteDbv3_bind_double          := LoadFunction('sqlite3_bind_double');
@SqliteDbv3_bind_int             := LoadFunction('sqlite3_bind_int');
@SqliteDbv3_bind_int64           := LoadFunction('sqlite3_bind_int64');
@SqliteDbv3_bind_null            := LoadFunction('sqlite3_bind_null');
//@SqliteDbv3_bind_value:= LoadFunction('sqlite3_bind_value');
@SqliteDbv3_bind_text            := LoadFunction('sqlite3_bind_text');
@SqliteDbv3_bind_text16          := LoadFunction('sqlite3_bind_text16');
@SqliteDbv3_bind_blob            := LoadFunction('sqlite3_bind_blob');

@SqliteDbv3_column_count         := LoadFunction('sqlite3_column_count');
@SqliteDbv3_column_name          := LoadFunction('sqlite3_column_name');
@SqliteDbv3_column_name16        := LoadFunction('sqlite3_column_name16');
@SqliteDbv3_column_decltype      := LoadFunction('sqlite3_column_decltype');
@SqliteDbv3_column_decltype16    := LoadFunction('sqlite3_column_decltype16');
@SqliteDbv3_step                 := LoadFunction('sqlite3_step');
@SqliteDbv3_data_count           := LoadFunction('sqlite3_data_count');
@SqliteDbv3_column_blob          := LoadFunction('sqlite3_column_blob');
@SqliteDbv3_column_bytes         := LoadFunction('sqlite3_column_bytes');
@SqliteDbv3_column_double        := LoadFunction('sqlite3_column_double');
@SqliteDbv3_column_int           := LoadFunction('sqlite3_column_int');
@SqliteDbv3_column_int64         := LoadFunction('sqlite3_column_int64');
@SqliteDbv3_column_text          := LoadFunction('sqlite3_column_text');
@SqliteDbv3_column_text16        := LoadFunction('sqlite3_column_text16');
@SqliteDbv3_column_type          := LoadFunction('sqlite3_column_type');

{ This API is only available if the library was compiled with
  the SQLITE_ENABLE_COLUMN_METADATA preprocessor symbol defined. }
@SqliteDbv3_column_origin_name   := LoadFunction('sqlite3_column_origin_name');
@SqliteDbv3_column_origin_name16 := LoadFunction('sqlite3_column_origin_name16');
@SqliteDbv3_column_table_name    := LoadFunction('sqlite3_column_table_name');
@SqliteDbv3_column_table_name16  := LoadFunction('sqlite3_column_table_name16');

@SqliteDbv3_finalize             := LoadFunction('sqlite3_finalize');
@SqliteDbv3_reset                := LoadFunction('sqlite3_reset');
@SqliteDbv3_create_function      := LoadFunction('sqlite3_create_function');
@SqliteDbv3_create_function16    := LoadFunction('sqlite3_create_function16');
@SqliteDbv3_aggregate_count      := LoadFunction('sqlite3_aggregate_count');
@SqliteDbv3_value_blob           := LoadFunction('sqlite3_value_blob');
@SqliteDbv3_value_bytes          := LoadFunction('sqlite3_value_bytes');
@SqliteDbv3_value_double         := LoadFunction('sqlite3_value_double');
@SqliteDbv3_value_int            := LoadFunction('sqlite3_value_int');
@SqliteDbv3_value_int64          := LoadFunction('sqlite3_value_int64');
@SqliteDbv3_value_text           := LoadFunction('sqlite3_value_text');
@SqliteDbv3_value_text16         := LoadFunction('sqlite3_value_text16');
@SqliteDbv3_value_type           := LoadFunction('sqlite3_value_type');
@SqliteDbv3_aggregate_context    := LoadFunction('sqlite3_aggregate_context');
@SqliteDbv3_user_data            := LoadFunction('sqlite3_user_data');
@SqliteDbv3_get_auxdata          := LoadFunction('sqlite3_get_auxdata');
@SqliteDbv3_set_auxdata          := LoadFunction('sqlite3_set_auxdata');
@SqliteDbv3_result_blob          := LoadFunction('sqlite3_result_blob');
@SqliteDbv3_result_double        := LoadFunction('sqlite3_result_double');
@SqliteDbv3_result_error         := LoadFunction('sqlite3_result_error');
@SqliteDbv3_result_int           := LoadFunction('sqlite3_result_int');
@SqliteDbv3_result_int64         := LoadFunction('sqlite3_result_int64');
@SqliteDbv3_result_null          := LoadFunction('sqlite3_result_null');
@SqliteDbv3_result_text          := LoadFunction('sqlite3_result_text');
@SqliteDbv3_result_text16        := LoadFunction('sqlite3_result_text16');
@SqliteDbv3_result_value         := LoadFunction('sqlite3_result_value');
@SqliteDbv3_create_collation     := LoadFunction('sqlite3_create_collation');
@SqliteDbv3_collation_needed     := LoadFunction('sqlite3_collation_needed');
@SqliteDbv3_bind_parameter_count := LoadFunction('sqlite3_bind_parameter_count');
@SqliteDbv3_bind_parameter_name  := LoadFunction('sqlite3_bind_parameter_name');
@SqliteDbv3_bind_parameter_index := LoadFunction('sqlite3_bind_parameter_index');
finally
Result := ErrorList.Count = 0;
If Not Result
   then FDatabase.DatabaseError.RaiseExceptionFmt(Msg3002,[CRLF, ErrorList.Text, FLibraryFile, CRLF], -1, FDatabase, vlLog);
ErrorList.Free;
end;
end;

{ TSqlitPassTransaction }

constructor TSqlitePassTransaction.Create(Owner: TSqlitePassEngine);
begin
inherited Create;
FEngine := Owner;
FLevel := 0;
end;

destructor TSqlitePassTransaction.Destroy;
begin
FEngine := nil;
Inherited Destroy;
end;

procedure TSqlitePassTransaction.Start;
begin
FEngine.ExecSql('Begin;');
Inc(FLevel);
end;

procedure TSqlitePassTransaction.Commit;
begin
FEngine.ExecSql('Commit;');
Dec(FLevel);
end;

procedure TSqlitePassTransaction.CommitAll;
begin
ResetTransactions(True);
end;

procedure TSqlitePassTransaction.Rollback;
begin
FEngine.ExecSql('Rollback;');
Dec(FLevel);
end;

procedure TSqlitePassTransaction.RollbackAll;
begin
ResetTransactions(False);
end;

procedure TSqlitePassTransaction.ResetTransactions(CommitTransaction: Boolean);
begin
While FLevel > 0 do
    Case CommitTransaction of
         True  : Commit;
         False : Rollback;
         end;
end;

procedure TSqlitePassTransaction.StartInternalTransaction;
begin
if FLevel = 0
   then begin
        FInternalTransaction := True;
        Start;
        end;
end;

procedure TSqlitePassTransaction.CommitInternalTransaction;
begin
if FInternalTransaction = True
   then begin
        FInternalTransaction := False;
        Commit;
        end;
end;

procedure TSqlitePassTransaction.RollbackInternalTransaction;
begin
if FInternalTransaction = True
   then begin
        FInternalTransaction := False;
        Rollback;
        end;
end;

procedure TSqlitePassEngine.StartChrono;
begin
 FStartTime:=Integer(GetTickCount);
end;

procedure TSqlitePassEngine.StopChrono;
begin
 FStopTime:=Integer(GetTickCount);
 FElapsedTime := (FStopTime - FStartTime) / 1000;
end;

