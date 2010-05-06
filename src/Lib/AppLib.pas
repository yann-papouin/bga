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
 * The Original Code is AppLib (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

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
