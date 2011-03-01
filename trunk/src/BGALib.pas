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
 * The Original Code is BGALib (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit BGALib;

interface

uses
  SysUtils;

type

  TBgaGetFileByPath = function(Sender: TObject; const VirtualPath : string) : string of object;

  procedure EnableBFDecimal;
  procedure DisableBFDecimal;

implementation

const
  BATTLEFIELD_DECIMAL = '.';

var
  SystemDecimalSeparator : Char;

  procedure EnableBFDecimal;
  begin
    if SystemDecimalSeparator = #0 then
    begin
      SystemDecimalSeparator := DecimalSeparator;
      DecimalSeparator := BATTLEFIELD_DECIMAL;
    end;
  end;

  procedure DisableBFDecimal;
  begin
    if SystemDecimalSeparator <> #0 then
    begin
      DecimalSeparator := SystemDecimalSeparator;
      SystemDecimalSeparator := #0;
    end;
  end;
end.
