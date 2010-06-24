unit UAC;

interface

uses
  Forms, Windows, ShellApi, SysUtils;

  procedure RunAsAdmin(hWnd : HWND; aFile : String; aParameters : String);
  function Perform : boolean;

  procedure BgaFileAssociation;

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

end.
