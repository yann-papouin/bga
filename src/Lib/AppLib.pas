unit AppLib;

interface

uses
  JvGnuGetText, TypInfo, CommonLib,
  SysUtils, StrUtils, Windows, Messages, Classes, Controls, ExtCtrls, Dialogs, Graphics,
  Forms, Math, Variants, Varutils;

function GetAppTempDirectory: String;
function GetMapTempDirectory: String;

implementation

function GetAppTempDirectory: String;
begin
  Result := IncludeTrailingBackslash(GetTempDirectory);
  Result := Result + IncludeTrailingBackslash('BGA');
end;

function GetMapTempDirectory: String;
begin
  Result := IncludeTrailingBackslash(GetAppTempDirectory);
  Result := Result + IncludeTrailingBackslash('bftemp_'+IntToStr(GetCurrentProcessId));
end;

end.
