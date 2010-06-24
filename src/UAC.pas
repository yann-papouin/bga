unit UAC;

interface

uses
  Forms, Windows, Messages, ShellApi, SysUtils;

  procedure RunAsAdmin(hWnd : HWND; aFile : String; aParameters : String);
  function Perform : boolean;

  procedure BgaFileAssociation;
  function RefreshScreenIcons : Boolean;

const
  BGA_FILE_ASSOCIATION_REQUEST = '{5FF0C89E-9421-483E-9022-D9243B1231AC}';

implementation

uses
  DbugIntf, CommonLib, Registry;

procedure RunAsAdmin(hWnd : HWND; aFile : String; aParameters : String);
var
  Sei : TShellExecuteInfo;
begin
  Fillchar(sei,SizeOf(sei),0);
  sei.cbSize := SizeOf(sei);
  sei.Wnd    := hWnd;
  sei.fMask  := SEE_MASK_FLAG_DDEWAIT or SEE_MASK_FLAG_NO_UI;
  sei.lpfile := PChar(aFile);
  sei.lpVerb := 'runas';
  sei.lpParameters := PChar(aParameters);
  sei.nShow := SW_SHOWNORMAL;
  if not ShellExecuteEx(@sei) then
    RaiseLastOSError;
end;

function Perform : boolean;
begin
  Result := false;
  if ParamStr(1) = BGA_FILE_ASSOCIATION_REQUEST then
  begin
    BgaFileAssociation;
    Result := true;
  end;
end;

procedure BgaFileAssociation;
var
  Reg : TRegistry;
begin
  Reg := TRegistry.Create;

  with Reg do try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKey('.rfa', true) then
    begin
      WriteString('', 'Battlefield.Archive'); // Write RFA Class
      CloseKey;
    end;
    if OpenKey('Battlefield.Archive', true) then
    begin
      WriteString('', 'Battlefield Archive'); // Write RFA description
      CloseKey;
    end;
    if OpenKey('Battlefield.Archive\shell\open\command', true) then
    begin
      WriteString('', DblQuotedStr(Application.ExeName) + ' ' + DblQuotedStr('%1')); // Write RFA association path
      CloseKey;
    end;
    if OpenKey('Applications\BGA.exe\shell\open\command', true) then
    begin
      WriteString('', DblQuotedStr(Application.ExeName) + ' ' + DblQuotedStr('%1')); // Write BGA path
      CloseKey;
    end;
  finally
    Free;
  end;
end;


function RefreshScreenIcons : Boolean;
const
  KEY_TYPE = HKEY_CURRENT_USER;
  KEY_NAME = 'Control Panel\Desktop\WindowMetrics';
  KEY_VALUE = 'Shell Icon Size';
var
  Reg: TRegistry;
  strDataRet, strDataRet2: string;

 procedure BroadcastChanges;
 var
   success: DWORD;
 begin
   SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, SPI_SETNONCLIENTMETRICS, 0, SMTO_ABORTIFHUNG, 10000, success);
 end;

begin
  Result := False;
  Reg := TRegistry.Create;
  try
    Reg.RootKey := KEY_TYPE;
    // 1. open HKEY_CURRENT_USER\Control Panel\Desktop\WindowMetrics
    if Reg.OpenKey(KEY_NAME, False) then
    begin
      // 2. Get the value for that key
      strDataRet := Reg.ReadString(KEY_VALUE);
      Reg.CloseKey;
      if strDataRet <> '' then
      begin
        // 3. Convert sDataRet to a number and subtract 1,
        //    convert back to a string, and write it to the registry
        strDataRet2 := IntToStr(StrToInt(strDataRet) - 1);
        if Reg.OpenKey(KEY_NAME, False) then
        begin
          Reg.WriteString(KEY_VALUE, strDataRet2);
          Reg.CloseKey;
          // 4. because the registry was changed, broadcast
          //    the fact passing SPI_SETNONCLIENTMETRICS,
          //    with a timeout of 10000 milliseconds (10 seconds)
          BroadcastChanges;
          // 5. the desktop will have refreshed with the
          //    new (shrunken) icon size. Now restore things
          //    back to the correct settings by again writing
          //    to the registry and posing another message.
          if Reg.OpenKey(KEY_NAME, False) then
          begin
            Reg.WriteString(KEY_VALUE, strDataRet);
            Reg.CloseKey;
            // 6.  broadcast the change again
            BroadcastChanges;
            Result := True;
          end;
        end;
      end;
    end;
  finally
    Reg.Free;
  end;
end;

end.
