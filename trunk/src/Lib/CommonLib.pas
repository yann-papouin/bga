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
 * The Original Code is CommonLib (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit CommonLib;

interface

uses
  JvGnuGetText, shlobj,  TypInfo, ShellAPI, Registry,
  SysUtils, StrUtils, Windows, Messages, Classes, Controls, ExtCtrls, Dialogs,  Graphics,
  Forms, Math, Variants, Varutils;


type

  TSinglePoint = record
    X : single;
    Y : single;
  end;

  TMouseEvent = record
    Flag : boolean;
    Sender : TObject;
    Button: TMouseButton;
    Shift : TShiftState;
    X, Y: Integer;
  end;

  TGLSelection = record
    Enabled : boolean;
    P1 : TPoint;
    P2 : TPoint;
  end;

  TSingleRect = record
    Left : single;
    Right : single;
    Top : single;
    Bottom : single;
  end;


// DLLVERSIONINFO structure

  PDLLVerInfo=^TDLLVersionInfo;
  TDLLVersionInfo=Record
    cbSize,   // Size of the structure, in bytes.
    dwMajorVersion, // Major version of the DLL
    dwMinorVersion, // Minor version of the DLL
    dwBuildNumber, // Build number of the DLL
    dwPlatformID: DWord; // Identifies the platform for which the DLL was built
  end;


  TWmiPingResult =(
    prSuccess                         = 0,
    prBuffer_Too_Small                = 11001,
    prDestination_Net_Unreachable     = 11002,
    prDestination_Host_Unreachable    = 11003,
    prDestination_Protocol_Unreachable= 11004,
    prDestination_Port_Unreachable    = 11005,
    prNo_Resources                    = 11006,
    prBad_Option                      = 11007,
    prHardware_Error                  = 11008,
    prPacket_Too_Big                  = 11009,
    prRequest_Timed_Out               = 11010,
    prBad_Request                     = 11011,
    prBad_Route                       = 11012,
    prTimeToLive_Expired_Transit      = 11013,
    prTimeToLive_Expired_Reassembly   = 11014,
    prParameter_Problem               = 11015,
    prSource_Quench                   = 11016,
    prOption_Too_Big                  = 11017,
    prBad_Destination                 = 11018,
    prNegotiating_IPSEC               = 11032,
    prGeneral_Failure                 = 11050
  );

  TCommande = (
    cmdTitres,
    cmdLignes,
    cmdFin
  );


  function Pythagore(DistX,DistY : single) : single;
  procedure GradHorizontal(Canvas: TCanvas; CanvasRect:TRect; Rect:TRect; FromColor, ToColor:TColor) ;
  procedure GradVertical(Canvas: TCanvas; CanvasRect:TRect; Rect:TRect; FromColor, ToColor:TColor) ;
  procedure Delay(MSecs: Cardinal);

  function ByteToHex(InByte:byte):shortstring;

  procedure VariantToStream(const v: olevariant; Stream: TMemoryStream);
  procedure StreamToVariant(Stream: TMemoryStream; var v: OleVariant);
  procedure BmpToVariant(aBmp: TBitmap; var aVariant:OleVariant);
  procedure VariantToBmp(aValue: OleVariant; var aBmp: TBitmap);

  function FileLastModified(const TheFile: string): TDateTime;

  function GetGenericFileType(Extension: string ): string;

  function GetMyDocuments: string;
  function GetTempDirectory: String;
  function GetApplicationData: string;
  function GetSystemDir: string;
  function GetProgramFiles: string;

  function RemoveAcute(const S: string): string;
  function Mince(PathToMince :String; InSpace :Integer): String;

  function ValidFilename(Filename: String; islong: Boolean = true) : Boolean;
  function ValidDirectoryname(Filename: String; islong: Boolean = true) : Boolean;
  function StripNonConforming(const S: string; const ValidChars: TSysCharSet): string;

  function GetNetUser : Ansistring;


  function Tableau(Commande : TCommande; Champ : TStringList; var TailleChamp : TStringList) : String;

  procedure BuildModulesList(ProcessID: DWORD; var AText : TStringList);

  function SizeToStr(const Size : int64) : string;

  function AreInSameRange(AValue, BValue, R1Min, R1Max, R2Min, R2Max:Integer) : boolean;
  function SameRange(AValue, BValue, RStart, RStop:Integer) : boolean;

  function ExecAndWait(const FileName, Params: string; WindowState: Word): Boolean;
  function Exec(const FileName, Params: string; WindowState: Word; var ProcInfo: TProcessInformation): Boolean;

  function KillWindow(hWindowHandle: HWND) : boolean;  
  function KillProcess(processHandle: THandle) : boolean;
  function GetWindowFromID(ProcessID : Cardinal): THandle;

  function DblQuotedStr(const S: string): string;
  
  function GetDllVersion(DllName: string; var DLLVersionInfo: TDLLVersionInfo): Boolean;

  function ExtractUrlFileName(const AUrl: string): string;

  function DetectWineEnvironment : boolean;

  procedure EnableDecimalSeparatorCompatibility;
  procedure DisableDecimalSeparatorCompatibility;

  procedure ProcessMessages;

  function RandomString(expr: string): string;

implementation

uses
  TLHelp32, DbugIntf;

var
  OriginalDecimalSeparator : char;
  DllGetVersion: function(dvi: PDLLVerInfo): PDLLVerInfo; stdcall;

const

  ShortFileForbiddenChars : set of Char = [';', '=', '+', '<', '>', '|','"', '[', ']', '\', '/', '''']; { for short 8.3 file names }
  LongFileForbiddenChars  : set of Char = ['<', '>', '|', '"', '\', '/', ':', '*', '?']; { for long file names }

  ShortDirForbiddenChars : set of Char = [';', '=', '+', '<', '>', '|','"', '[', ']', '''']; { for short 8.3 file names }
  LongDirForbiddenChars  : set of Char = ['<', '>', '|', '"', ':', '*', '?']; { for long file names }

	WithAccent     : set of Char = ['À','Á','Â','Ã','Ä','Å','à','á','â','ã','ä','å','Ò','Ó','Ô','Õ','Ö','Ø','ò','ó','ô','õ','ö','ø','È','É','Ê','Ë','è','é','ê','ë','Ì','Í','Î','Ï','ì','í','î','ï','Ù','Ú','Û','Ü','ù','ú','û','ü','ÿ','Ñ','ñ','Ç','ç'];
	WithoutAccent  : set of Char = ['A','A','A','A','A','A','a','a','a','a','a','a','O','O','O','O','O','O','o','o','o','o','o','o','E','E','E','E','e','e','e','e','I','I','I','I','i','i','i','i','U','U','U','U','u','u','u','u','y','N','n','C','c'];

function ValidFilename(Filename: String; islong: Boolean = true) : Boolean;
var
  I: integer;
begin
  Result := Filename <> '';
  if islong then
  begin
    for I := 1 to Length(Filename) do
      Result := Result and not (Filename[I] in LongFileForbiddenChars);
  end
  else
  begin
    for I := 1 to Length(Filename) do
      Result := Result and not (Filename[I] in ShortFileForbiddenChars);
  end;
end;


function ValidDirectoryname(Filename: String; islong: Boolean = true) : Boolean;
var
  I: integer;
begin
  Result := Filename <> '';
  if islong then
  begin
    for I := 1 to Length(Filename) do
      Result := Result and not (Filename[I] in LongDirForbiddenChars);
  end
  else
  begin
    for I := 1 to Length(Filename) do
      Result := Result and not (Filename[I] in ShortDirForbiddenChars);
  end;
end;

function StripNonConforming(const S: string; const ValidChars: TSysCharSet): string;
var
  DestI: Integer;
  SourceI: Integer;
begin
  SetLength(Result, Length(S));
  DestI := 0;
  for SourceI := 1 to Length(S) do
    if S[SourceI] in ValidChars then
    begin
      Inc(DestI);
      Result[DestI] := S[SourceI]
    end;
  SetLength(Result, DestI)
end;

function RandomString(expr: string): string;
{
  (c) 2002 Uros Gaber, PowerCom d.o.o.
  expr values that combine (*nix style)
  1: A..Z
  2: a..z
  4: 0..9
  if you want
  (A..Z, a..z) use 3;
  (A..Z, a..z, 0..9) use 7
  (A..Z, 0..9) use 5
  (a..z, 0..9) use 6
  i.e. RandomString('123'); to generate a 3 letters random string...
}
var
  i: Byte;
  s: string;
  v: Byte;
begin
  Randomize;
  SetLength(Result, Length(expr));
  for i:=1 to Length(expr) do
  begin
    s:='';
    try
      v:=StrToInt(Expr[i]);
    except
      v:=0;
    end;
    if (v-4) >= 0 then
    begin
      s:=s+'0123456789';
      dec(v, 4);
    end;
    if (v-2) >= 0 then
    begin
      s:=s+'abcdefghijklmnopqrstuvwxyz';
      dec(v, 2);
    end;
    if (v-1) >= 0 then
      s:=s+'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
    Result[i]:=s[Random(Length(s)-1)+1];
  end;
end;

function ExtractUrlFileName(const AUrl: string): string;
var 
  i: Integer; 
begin 
  i := LastDelimiter('/', AUrl);
  Result := Copy(AUrl, i + 1, Length(AUrl) - (i)); 
end;


function SizeToStr(const Size : int64) : string;
begin
  if Size < $000000000400 then
     result := format('%d %s',[Size,_('bytes')])
  else
  if Size < $000000100000 then
     result := format('%.1f %s',[Size/$000000000400,_('Kb')])
  else
  if Size < $000040000000 then
     result := format('%.2f %s',[Size/$000000100000,_('Mb')])
  else
  if Size < $010000000000 then
     result := format('%.2f %s',[Size/$000040000000,_('Gb')])
  else
     result := format('%.2f %s',[Size/$010000000000,_('Tb')])
end;


procedure ProcessMessages;
begin
  //SendDebug('ProcessMessages');
  Application.ProcessMessages;
end;


function DblQuotedStr(const S: string): string;
  var
    I: Integer;
  begin
    Result := S;
    for I := Length(Result) downto 1 do
      if Result[I] = '"' then Insert('"', Result, I);
    Result := '"' + Result + '"';
  end;


function Exec(const FileName, Params: string; WindowState: Word; var ProcInfo: TProcessInformation): Boolean;
var
  SUInfo: TStartupInfo;
  CmdLine: string;
begin
  { Enclose filename in quotes to take care of
    long filenames with spaces. }
  CmdLine := DblQuotedStr(FileName) + ' ' + Params;
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  with SUInfo do
  begin
    cb := SizeOf(SUInfo);
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := WindowState;
  end;
  Result := CreateProcess(nil, PChar(CmdLine), nil, nil, False,
    CREATE_NEW_CONSOLE or
    NORMAL_PRIORITY_CLASS, nil,
    nil,//PChar(ExtractFilePath(FileName)),
    SUInfo, ProcInfo);
end;

function ExecAndWait(const FileName, Params: string; WindowState: Word): Boolean;
var
  ProcInfo: TProcessInformation;
begin
  if Exec(FileName, Params, WindowState, ProcInfo) then
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);
end;

function KillWindow(hWindowHandle: HWND) : boolean;
var
  hprocessID: INTEGER;
  processHandle: THandle;
  DWResult: DWORD;
begin
  Result := false;
  SendMessageTimeout(hWindowHandle, WM_CLOSE, 0, 0,
    SMTO_ABORTIFHUNG or SMTO_NORMAL, 5000, DWResult);
  if isWindow(hWindowHandle) then
  begin
    { Get the process identifier for the window}
    GetWindowThreadProcessID(hWindowHandle, @hprocessID);
    if hprocessID <> 0 then
    begin
      { Get the process handle }
      processHandle := OpenProcess(PROCESS_TERMINATE or PROCESS_QUERY_INFORMATION, False, hprocessID);
      if processHandle <> 0 then
      begin
        { Terminate the process }
        if TerminateProcess(processHandle, 0) then
          Result := CloseHandle(ProcessHandle);
      end;
    end;
  end;
end;

function GetWindowFromID(ProcessID : Cardinal): THandle;
var TestID  : Cardinal;
    TestHandle : Thandle;
begin
  Result := 0;
  TestHandle := FindWindowEx(GetDesktopWindow, 0, Nil, Nil);
  While TestHandle > 0 do begin
      If GetParent(TestHandle) = 0 Then
        GetWindowThreadProcessId(TestHandle,  @TestID);
          If TestID = ProcessID Then  Begin
            Result := TestHandle;
            Exit;
          end;
      TestHandle := GetWindow(TestHandle, GW_HWNDNEXT)
  end;
end;


function KillProcess(processHandle: THandle) : boolean;
begin
  Result := false;
  if processHandle <> 0 then
  begin
    { Terminate the process }
    if TerminateProcess(processHandle, 0) then
      Result := CloseHandle(ProcessHandle);
  end;
end;

(*
procedure ListFileDir(Path: string; Pattern : string = '*.*'; List : TStringList = nil);
var
  intFound: Integer;
  SearchRec: TSearchRec;
begin
  if List = nil then
    raise Exception.Create('List (TStringList) is nil');

  List.Clear;
  intFound := FindFirst(Path + Pattern, faAnyFile, SearchRec);
  while intFound = 0 do
  begin
    List.Add(Path + SearchRec.Name);
    intFound := FindNext(SearchRec);
  end;
  Sysutils.FindClose(SearchRec);
end;

*)

// Returns the network ID for the current system user. Returns
// null string ('') on error.
function GetNetUser : Ansistring;
var
  dwI : DWord;
begin 
  dwI := MAX_PATH; 
  SetLength (Result, dwI + 1); 
  if WNetGetUser (Nil, PChar (Result), dwI) = NO_ERROR then 
    SetLength (Result, StrLen (PChar (Result))) 
  else 
    SetLength (Result, 0)
end;



function RemoveAcute(const S: string): string;
var
  PResult : PChar;
  PStr    : PChar;
begin
  Result  := S;
  PResult := PChar(Result);
  PStr    := PChar(S);
  while PStr[0] <> #0 do begin
    case PStr[0] of
       #192..#197 : PResult[0] := 'A';
       #199       : PResult[0] := 'C';
       #200..#203 : PResult[0] := 'E';
       #204..#207 : PResult[0] := 'I';
       #209       : PResult[0] := 'N';
       #210..#214 : PResult[0] := 'O';
       #138       : PResult[0] := 'S';
       #217..#220 : PResult[0] := 'U';
       #159,#221  : PResult[0] := 'Y';
       #142       : PResult[0] := 'Z';
       #224..#229 : PResult[0] := 'a';
       #231       : PResult[0] := 'c';
       #232..#235 : PResult[0] := 'e';
       #236..#239 : PResult[0] := 'i';
       #241       : PResult[0] := 'n';
       #242..#246 : PResult[0] := 'o';
       #154       : PResult[0] := 's';
       #249..#252 : PResult[0] := 'u';
       #253,#255  : PResult[0] := 'y';
       #158       : PResult[0] := 'z';
    end;
    inc(PResult);
    inc(PStr);
  end;
end;


function GetGenericFileType(Extension: string ): string;
{ Get file type for an extension }
var
  AInfo: TSHFileInfo;
begin
  SHGetFileInfo( PChar(Extension), FILE_ATTRIBUTE_NORMAL, AInfo, SizeOf( AInfo ),
    SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES );
  Result := AInfo.szTypeName;
end;


function GetTempDirectory: String;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;

function GetMyDocuments: string;
var
   r: Bool;
   path: array[0..Max_Path] of Char;
begin
   r := ShGetSpecialFolderPath(0, path, CSIDL_Personal, False) ;
   if not r then raise Exception.Create('Could not find MyDocuments folder location.') ;
   Result := Path;
end;


function GetApplicationData: string;
var
   r: Bool;
   path: array[0..Max_Path] of Char;
begin
   r := ShGetSpecialFolderPath(0, path, CSIDL_LOCAL_APPDATA, False) ;
   if not r then raise Exception.Create('Could not find ApplicationData folder location.') ;
   Result := Path;
end;


function GetSystemDir: string;
var
   r: Bool;
   path: array[0..Max_Path] of Char;
begin
   r := ShGetSpecialFolderPath(0, path, CSIDL_SYSTEM, False) ;
   if not r then raise Exception.Create('Could not find System folder location.') ;
   Result := Path;
end;

function GetProgramFiles: string;
var
   r: Bool;
   path: array[0..Max_Path] of Char;
begin
   r := ShGetSpecialFolderPath(0, path, CSIDL_PROGRAM_FILES, False) ;
   if not r then raise Exception.Create('Could not find Program files folder location.') ;
   Result := Path;
end;






function FileLastModified(const TheFile: string): TDateTime;
var
  FileH : THandle;
  LocalFT : TFileTime;
  DosFT : DWORD;
  LastAccessedTime : TDateTime;
  FindData : TWin32FindData;
begin
  Result := EncodeDate(1983, 10, 25);
  FileH := FindFirstFile(PChar(TheFile), FindData) ;
  if FileH <> INVALID_HANDLE_VALUE then
  begin
   Windows.FindClose(FileH) ;

    //if (FindData.dwFileAttributes AND FILE_ATTRIBUTE_DIRECTORY) = 0 then
    begin
     FileTimeToLocalFileTime
      (FindData.ftLastWriteTime,LocalFT) ;
     FileTimeToDosDateTime
      (LocalFT,LongRec(DosFT).Hi,LongRec(DosFT).Lo) ;
     LastAccessedTime := FileDateToDateTime(DosFT) ;

     //Result := FormatDateTime('dddd d mmm yyyy', LastAccessedTime) + ' ' + _('at') + ' ' + FormatDateTime('hh:mm', LastAccessedTime);
     Result := LastAccessedTime;
    end;
  end;
end;


procedure VariantToStream (const v : olevariant; Stream : TMemoryStream);
var
  p : pointer;
begin
  Stream.Position := 0;
  Stream.Size := VarArrayHighBound (v, 1) - VarArrayLowBound(v,  1) + 1;
  p := VarArrayLock (v);
  Stream.Write (p^, Stream.Size);
  VarArrayUnlock (v);
  Stream.Position := 0;
end;

procedure StreamToVariant (Stream : TMemoryStream; var v : OleVariant);
var
  p : pointer;
begin
  v := VarArrayCreate ([0, Stream.Size - 1], varByte);
  p := VarArrayLock (v);
  Stream.Position := 0;
  Stream.Read (p^, Stream.Size);
  VarArrayUnlock (v);
end;

procedure BmpToVariant(aBmp : TBitmap; var aVariant:OleVariant);
var
  Stream : TMemoryStream;
begin
  Stream := nil;
  try
     Stream := TMemoryStream.Create;
     aBmp.SaveToStream(Stream);
     StreamToVariant(Stream, aVariant);
  finally
     Stream.Free;
 end;
end;

procedure VariantToBmp(aValue : OleVariant;var aBmp:TBitmap);
var
   Stream : TMemoryStream;
begin
  Stream := nil;
  try
    Stream := TMemoryStream.Create;
    VariantToStream (aValue,Stream);
    aBmp.LoadfromStream(Stream);
  finally
    //VariantClear(aValue);
    Stream.free;
  end;
end;



function ByteToHex(InByte:byte):shortstring;
const Digits:array[0..15] of char='0123456789ABCDEF';
begin
 result:=digits[InByte shr 4]+digits[InByte and $0F];
end;


function Pythagore(DistX,DistY : single) : single;
begin
  DistX := Power(DistX,2);
  DistY := Power(DistY,2);
  result := Power(DistX+DistY,1/2);
end; 


procedure Delay(MSecs: Cardinal);
Var FirstTick,CurrentTick: Cardinal;
    Done : Boolean;
Begin
  Done := FALSE;
  FirstTick := GetTickCount;
  While Not Done do Begin
    Application.ProcessMessages;
    CurrentTick := GetTickCount;
    If Int64(CurrentTick) - Int64(FirstTick) < 0 Then Begin
      If CurrentTick >= (Int64(FirstTick) - High(Cardinal) + MSecs) Then Done := TRUE;
    End Else If CurrentTick - FirstTick >= MSecs Then Done := TRUE;
  End;
End;

function AreInSameRange(AValue, BValue, R1Min, R1Max, R2Min, R2Max:Integer) : boolean;
begin
  if (InRange(AValue, R1Min, R1Max)
  and not InRange(BValue, R1Min, R1Max))
  or (InRange(AValue, R2Min, R2Max)
  and not InRange(BValue, R2Min, R2Max))
  then
    result := false
  else
    result := true;
end;

function SameRange(AValue, BValue, RStart, RStop:Integer) : boolean;
begin
  if (InRange(AValue, RStart, RStop) and InRange(BValue, RStart, RStop))
  then
    result := true
  else
    result := false;
end;



procedure GradHorizontal(Canvas: TCanvas; CanvasRect:TRect; Rect:TRect; FromColor, ToColor:TColor) ;
var
  X:integer;
  dr,dg,db:Extended;
  C1,C2:TColor;
  r1,r2,g1,g2,b1,b2:Byte;
  R,G,B:Byte;
  cnt:integer;
  step:integer;
begin

  C1 := ColorToRGB(FromColor);
  R1 := GetRValue(C1) ;
  G1 := GetGValue(C1) ;
  B1 := GetBValue(C1) ;

  C2 := ColorToRGB(ToColor);
  R2 := GetRValue(C2) ;
  G2 := GetGValue(C2) ;
  B2 := GetBValue(C2) ;

  step := (Rect.Right - Rect.Left);
  //if step = 0 then step := (CanvasRect.Bottom - CanvasRect.Top);
  

  dr := (R2-R1) / step;
  dg := (G2-G1) / step;
  db := (B2-B1) / step;

  cnt := 0;

  for X := Rect.Left to Rect.Right -1  do
  begin
    R := R1+Ceil(dr*cnt) ;
    G := G1+Ceil(dg*cnt) ;
    B := B1+Ceil(db*cnt) ;

    Canvas.Pen.Color := RGB(R,G,B) ;
    Canvas.MoveTo(X,Rect.Top) ;
    Canvas.LineTo(X,Rect.Bottom) ;

    Inc(cnt) ;
  end;


end;


procedure GradVertical(Canvas: TCanvas; CanvasRect:TRect;  Rect:TRect; FromColor, ToColor:TColor) ;
var
  Y:integer;
  dr,dg,db:Extended;
  C1,C2:TColor;
  r1,r2,g1,g2,b1,b2:Byte;
  R,G,B:Byte;
  cnt:integer;
  step:integer;
begin

  C1 := ColorToRGB(FromColor);
  R1 := GetRValue(C1) ;
  G1 := GetGValue(C1) ;
  B1 := GetBValue(C1) ;

  C2 := ColorToRGB(ToColor);
  R2 := GetRValue(C2) ;
  G2 := GetGValue(C2) ;
  B2 := GetBValue(C2) ;

  step := (Rect.Bottom - Rect.Top);

  dr := (R2-R1) / step;
  dg := (G2-G1) / step;
  db := (B2-B1) / step;

  cnt := 0;

  for Y := Rect.Top to Rect.Bottom -1  do
  begin
    R := R1+Ceil(dr*cnt) ;
    G := G1+Ceil(dg*cnt) ;
    B := B1+Ceil(db*cnt) ;

    Canvas.Pen.Color := RGB(R,G,B) ;
    Canvas.MoveTo(Rect.Left,Y) ;
    Canvas.LineTo(Rect.Right,Y) ;

    Inc(cnt) ;
  end;


end;



{------------------------------------------------------------------------------}
{                           Génération du tableau ASCII                        }
{------------------------------------------------------------------------------}
function Tableau(Commande : TCommande; Champ : TStringList; var TailleChamp : TStringList) : String;

   {Centrage du texte}
   function FormatTxt(TailleChamp : Integer; Item : String) : String;

   var
   TailleItem,Deb,Fin : Integer;

   begin
   TailleItem  := Length(Item);

      If TailleItem > TailleChamp then
      begin
      {Si la taille du texte à inséré est supérieur à la taille du champ on
      tronque le texte}
      Result := '|' + Copy(Item,1,TailleChamp);
      end
      else
      begin
      {On détermine le nombre d'espace avant et apres le texte}
      Deb := 0; //(TailleChamp - TailleItem) div 2;    align left
      Fin := TailleChamp - TailleItem - Deb;
      Result := '|' + StringOfChar(' ',Deb) + Item + StringOfChar(' ',Fin);
      end;
   end;

var
Temp : String;
I : Integer;

begin

   Try

      If TailleChamp.Count < Champ.Count then
      begin
      {Si la taille de tous les champs n'est pas définis et bien on prends la taile
      du texte du champ comme taille de champ}

         For I:= TailleChamp.Count to Champ.Count-1 do
         TailleChamp.Add(IntToStr(Length(Champ.Strings[I])));
      end;

   Temp := '';

      For I:=0 to Champ.Count-1 do
      begin

         {Si la taille du champ est nul on prends la taille du texte du champ}
         If Length(TailleChamp.Strings[I]) = 0 then
         TailleChamp.Strings[I] := IntToStr(Length(Champ.Strings[I]));

      Temp := Temp + FormatTxt(StrToInt(TailleChamp.Strings[I]),Champ.Strings[I]);
      end;

   Temp := Temp + '|';

      {Commande =
       cmdTitres : Création de l'entete du tableau
       cmdLignes : Ajout d'une ligne
       cmdFin : Cloture du tableau} 
      If Commande = cmdTitres then
      Result := '   +' + StringOfChar('-',Length(Temp)-2) + '+' + #13#10 + '   ' +
             Temp + #13#10 + '   +' + StringOfChar('-',Length(Temp)-2) + '+'
      else if Commande = cmdLignes then Result := '   ' + Temp
      else Result := '   +' + StringOfChar('-',Length(Temp)-2) + '+';
   except
   Result := 'Erreur à la construction de la ligne.';
   end;
end;




function IntToExtended(I: Integer): Extended;
begin
  Result := I;
end;

function Mince (PathToMince :String; InSpace :Integer): String;
var
  TotalLength, FLength : Integer;
begin
  TotalLength := Length(PathToMince) ;
  if TotalLength > InSpace then
  begin
   FLength := (Inspace Div 2) - 2;
   Result := Copy(PathToMince, 0, fLength div 2)
             + '...'
             + Copy(PathToMince,
                   TotalLength-fLength,
                   TotalLength) ;
  end
  else
    Result := PathToMince;

end;

procedure BuildModulesList(ProcessID: DWORD; var AText : TStringList);
var
  SnapProcHandle: THandle;
  ModuleEntry: TModuleEntry32;
  Next: Boolean;
  Field,FieldSize : TStringList;
begin
{Création des TStringList}
  Field       := TStringList.Create; //Va contenir le texte a inserer dans le tableau
  FieldSize   := TStringList.Create; //Va contenir la taille des champs

  Field.Add('Module');      FieldSize.Add('18');
  Field.Add('MID');         FieldSize.Add('10');
  Field.Add('Base');        FieldSize.Add('10');
  Field.Add('Size');        FieldSize.Add('14');
  Field.Add('Global#');     FieldSize.Add('8');
  Field.Add('Process#');    FieldSize.Add('8');
  Field.Add('Handle #');    FieldSize.Add('12');
  Field.Add('Filename');    FieldSize.Add('50');

  {On ecrit le titre des colonnes}
  AText.Add(Tableau(cmdTitres,Field,FieldSize));

  SnapProcHandle := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, ProcessID);
  if SnapProcHandle <> THandle(-1) then
  begin
    ModuleEntry.dwSize := Sizeof(ModuleEntry);
    Next := Module32First(SnapProcHandle, ModuleEntry);
    while Next do
    begin
      with ModuleEntry do
      begin
        Field.Strings[0] := AnsiLowerCase(szModule);
        Field.Strings[1] := Format('%.8x', [th32ModuleID]);
        Field.Strings[2] := Format('%p', [modBaseAddr]);
        Field.Strings[3] := Format('%.0n', [IntToExtended(modBaseSize)]);
        Field.Strings[4] := Format('%d', [GlblcntUsage]);
        Field.Strings[5] := Format('%d', [ProccntUsage]);
        Field.Strings[6] := Format('%.8x', [hModule]);
        Field.Strings[7] := Mince(szExePath,50);
        AText.Add(Tableau(cmdLignes,Field,FieldSize));
      end;
      Next := Module32Next(SnapProcHandle, ModuleEntry);
    end;
    CloseHandle(SnapProcHandle);
  end;

  {On inscrit une ligne vide pour cloturer le tableau}
  AText.Add(Tableau(cmdFin,Field,FieldSize));

  Field.Free;
  FieldSize.Free;
  
end;



function GetDllVersion(DllName: string; var DLLVersionInfo: TDLLVersionInfo): Boolean;
var
  hInstDll: THandle;
  p: pDLLVerInfo;
begin
  Result := False;
  // Get a handle to the DLL module.
  // das Handle zum DLL Modul ermitteln.
  hInstDll := LoadLibrary(PChar(DllName));
  if (hInstDll = 0) then Exit;
  // Return the address of the specified exported (DLL) function.
  // Adresse der Dll-Funktion ermitteln
  @DllGetVersion := GetProcAddress(hInstDll, 'DllGetVersion');
  // If the handle is not valid, clean up an exit.
  // Wenn das Handle ungültig ist, wird die Funktion verlassen
  if (@DllGetVersion) = nil then
  begin
    FreeLibrary(hInstDll);
    Exit;
  end;

  new(p);
  try
    ZeroMemory(p, SizeOf(p^));
    p^.cbSize := SizeOf(p^);

    // Call the DllGetVersion function
    // Die DllGetVersion() Funktion aufrufen
    DllGetVersion(p);
    DLLVersionInfo.dwMajorVersion := p^.dwMajorVersion;
    DLLVersionInfo.dwMinorVersion := p^.dwMinorVersion;

    @DllGetVersion := nil;
    Result := True;
  finally
    dispose(P);
  end;
 // Free the DLL module.
 // Dll wieder freigeben.
 FreeLibrary(hInstDll);
end;


function DetectWineEnvironment : boolean;
var
  Reg:TRegistry;
begin
  Reg := nil;
  Result := False;
  try
    Reg:= TRegistry.Create(KEY_READ);
    Reg.RootKey := HKEY_CURRENT_USER;
    Result := Reg.KeyExists('SOFTWARE\Wine\');
  finally
    Reg.Free;
  end;
end;


procedure EnableDecimalSeparatorCompatibility;
begin
  DecimalSeparator := '.';
end;

procedure DisableDecimalSeparatorCompatibility;  
begin
  DecimalSeparator := OriginalDecimalSeparator;
end;



initialization
  OriginalDecimalSeparator := DecimalSeparator;

finalization

end.
