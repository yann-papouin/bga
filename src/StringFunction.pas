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
 * The Original Code is StringFunction (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit StringFunction;

interface

uses
  Classes, SysUtils, Math, Variants;

  type
    TGXAlignMode = (
      gamRightmost,
      gamFirstToken
    );

  function Align(Lines: TStrings; AlignToken : string; AlignMode: TGXAlignMode): Boolean;


  function SFRight(substr: string; s: string): string;
  function SFRightRight(substr: string; s: string): string;
  function SFLeft(substr: string; s: string): string;
  function SFLeftFromLast(substr: string; s: string): string;
  function SFCountSubstr(substr: string; s: string): integer;
  function SFNRight(substr: string; s: string;n:integer): string;
  function SFNLeft(substr: string; s: string;n:integer): string;
  function SFLeftNRight(substr: string; s: string;n:integer): string;
  function SFBetween(substr: string; s: string): string;
  function SFBetweenTwo(substrLeft: string; substrRight: string; s: string): string;

  function VariantToString(AVar: OleVariant): string;

var
  TAB_SIZE : byte = 2;
  WHITE_SPACE : integer = 0;

implementation

function ExpandTabsInLine(AText: string; ATabSize: Integer): string;
var
  i: Integer;
  ResultLen, SpaceLen: Integer;
begin
  Result := '';
  ResultLen := 0;

  for i := 1 to Length(AText) do
  begin
    if AText[i] <> #9 then
    begin
      Result := Result + AText[i];
      Inc(ResultLen);
    end
    else
    begin
      SpaceLen := ATabSize - (ResultLen mod ATabSize);
      Result := Result + StringOfChar(' ', SpaceLen);
      Inc(ResultLen, SpaceLen);
    end;
  end;
end;

function Align(Lines: TStrings; AlignToken : string; AlignMode: TGXAlignMode): Boolean;
var
  TabSize: Integer;
  i: Integer;
  FirstIndex, PosIndex: Integer;
  RowLength, MaxRowLength: Integer;
  AlignIndex: Integer;
  Temp: string;
  MaxPos: Integer;
  LineSuffix: string;
begin
  Assert(Assigned(Lines));
  Result := False;

  if Lines.Count < 2 then
    Exit;

  FirstIndex := 0;
  RowLength := 0;
  MaxRowLength := 0;

  TabSize := TAB_SIZE;

  // Decide at what column to align by
  for i := 0 to Lines.Count - 1 do
  begin
    Temp := ExpandTabsInLine(Lines[i], TabSize);

    PosIndex := Pos(AlignToken, Temp);

    if (PosIndex > 0) and (FirstIndex = 0) then
    begin
      FirstIndex := PosIndex;
      // If first line contains token, only align based on that token
      if AlignMode = gamFirstToken then
        Break;
    end;

    if PosIndex > 0 then
      RowLength := Length(TrimRight(Copy(Temp, 1, PosIndex - 1))) + WHITE_SPACE;

    if RowLength > MaxRowLength then
      MaxRowLength := RowLength;
  end;

  // Exit if nothing to align
  if FirstIndex = 0 then
    Exit;

  // Try to align at column of first found otherwise
  // align after the maximum length of a row
  if FirstIndex > MaxRowLength then
    AlignIndex  := FirstIndex - 1
  else
    AlignIndex := MaxRowLength;

  // Perform alignment
  for i := 0 to Lines.Count - 1 do
  begin
    PosIndex := Pos(AlignToken, Lines[i]);

    if PosIndex > 0 then
    begin
      Temp := TrimRight(Copy(Lines[i], 1, PosIndex - 1));
      MaxPos := Max(AlignIndex - Length(ExpandTabsInLine(Temp, TabSize)), WHITE_SPACE);
      LineSuffix := Copy(Lines[i], PosIndex, Length(Lines[i]));
      Lines[i] := Temp + StringOfChar(' ', MaxPos) + LineSuffix;
    end;
  end;

  Result := True;
end;

function SFRight(substr: string; s: string): string;            // Right
begin
  if pos(substr,s)=0 then result:='' else
    result:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
end;

function SFRightRight(substr: string; s: string): string;      // RightLast
{============================================================================}
{ fonction qui renvoie la sous chaine de caractere situee a droite de la sous}
{ chaine substr situee la plus a droite                                      }
{ ex: si substr = '\' et S= 'truc\tr\essai.exe droiteDroite renvoie essai.exe}
{============================================================================}
begin
  Repeat
    S:=SFRight(substr,s);
  until pos(substr,s)=0;
  result:=S;
end;

function SFLeft(substr: string; s: string): string;           // Left
{============================================================================}
{ fonction qui renvoie la sous chaine de caractere situee à gauche de la sous}
{ chaine substr                                                              }
{ ex: si substr = '\' et S= 'truc\tr\essai.exe' gauche renvoie truc          }
{============================================================================}
begin
  result:= copy(s, 1, pos(substr, s)-1);
  //result:= substr+S; //copy(s, 1, pos(substr, s)-1);
end;

function SFLeftFromLast(substr: string; s: string): string;    // LeftLast
{============================================================================}
{ fonction qui renvoie la sous chaine de caractere situee a gauche de la     }
{ derniere sous chaine substr                                                }
{ ex: si substr = '\' et S= 'truc\tr\essai.exe' gauche renvoie truc\tr       }
{============================================================================}
var
 s1:string;
 i:integer;
begin
  s1:='';
  for i:=1 to SFCountSubstr(substr, s)-1 do
  begin
    s1:=s1+SFLeft(substr,s)+substr;
    s:=SFRight(substr,s);
  end;
  s1:=s1+SFLeft(substr,s);
  result:=s1;
end;

function SFCountSubstr(substr: string; s: string): integer;     //Count
{==================================================================================}
{ renvoie le nombre de fois que la sous chaine substr est presente dans la chaine S}
{==================================================================================}
begin
  result:=0;
  while pos(substr,s)<>0 do
  begin
    S:=SFRight(substr,s);
    inc(result);
  end;
end;

function SFNRight(substr: string; s: string;n:integer): string;   //RightN
{==============================================================================}
{ renvoie ce qui est a droite de la n ieme sous chaine substr de la chaine S   }
{==============================================================================}
var i:integer;
begin
  for i:=1 to n do
  begin
    S:=SFRight(substr,s);
  end;
  result:=S;
end;

function SFNLeft(substr: string; s: string;n:integer): string;   //LeftN
{==============================================================================}
{ renvoie ce qui est a gauche de la n ieme sous chaine substr de la chaine S   }
{==============================================================================}
var i:integer;
begin
  for i:=1 to SFCountSubstr(substr,s) - n +1 do
  begin
    S:=SFLeftFromLast(substr,s);
  end;
  result:=S;
end;

function SFLeftNRight(substr: string; s: string;n:integer): string; //Extract
{==============================================================================}
{ renvoie ce qui est a gauche de la droite de la n ieme sous chaine substr     }
{ de la chaine S                                                               }
{ ex : GaucheNDroite('\','c:machin\truc\essai.exe',1) renvoie 'truc'           }
{ Permet d'extraire un a un les elements d'une chaine séparés par un separateur}
{==============================================================================}
var i:integer;
begin
  S:=S+substr;
  for i:=1 to n do
  begin
    S:=copy(s, pos(substr, s)+length(substr), length(s)-pos(substr, s)+length(substr));
  end;
  result:=copy(s, 1, pos(substr, s)-1);
end;

function SFBetween(substr: string; s: string): string;
begin
  result := SFLeftNRight(substr, s, 1);
end;


function SFBetweenTwo(substrLeft: string; substrRight: string; s: string): string;
begin
  Result := SFRight(substrLeft, s);
  Result := SFLeftFromLast(substrRight, Result);
end;


function VariantToString(AVar: OleVariant): string;
var
  i: integer;
  V: olevariant;
begin
  Result := '';
  if VarType(AVar) = (varVariant or varByRef) then
     V := Variant(TVarData(AVar).VPointer^)
  else V := AVar;

  if VarType(V) = (varByte or varArray) then
      try
        for i:=VarArrayLowBound(V,1) to VarArrayHighBound(V,1) do
           Result := Result + Chr(Byte(V[i]));
      except;
      end
    else Result := V;
end;

end.
