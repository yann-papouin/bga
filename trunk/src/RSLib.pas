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
 * The Original Code is RSLib (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit RSLib;

interface

uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Generics.Collections,
   GLState, VectorTypes;

type

  TRsResource = class
  private
  public
    Name : string;
    SubType : string;
    MaterialDiffuse : TVector4f;
    MaterialSpecular : TVector4f;
    MaterialSpecularPower : single;
    Lighting : boolean;
    LightingSpecular : boolean;
    TwoSided : boolean;
    Transparent : boolean;
    DepthWrite : boolean;
	  BlendSrc : TBlendFunction;
	  BlendDest : TBlendFunction;
    AlphaTestRef : single;
    Texture : string;
  end;

  TRsParser = class(TObjectList<TRsResource>)
  private
    FParsed: boolean;
  published
  public
    constructor Create;
    procedure Parse(Data : TStringList);
    property Parsed : boolean read FParsed;
  end;


  (*
subshader "plantsgroup1-large_Material0" "StandardMesh/Default"
{
	lighting false;
	materialDiffuse .7 .7 .7;
	lightingSpecular false;
	twosided true;
	transparent true;
	depthWrite true;
	alphaTestRef 0.4;
	texture "bf1942/levels/DC_LostVillage/Texture/desertplants1";
}
  *)

implementation

{ TRsParser }

uses
  StringFunction, GLColor, CommonLib, BGALib;

constructor TRsParser.Create;
begin
  inherited;
  FParsed := false;
  OwnsObjects := true;
end;

procedure TRsParser.Parse(Data: TStringList);
var
  Line, Pos : Integer;
  Path, BldFun : string;
  Resource : TRsResource;

  SectionSubShader : boolean;
  TitleSubShader : boolean;

  function ParseInlineVectorData(s : string) : TVector4f;
  const
    FloatCharSet = [' ', ',', '.', '0' .. '9'];
  var
    ArrayText : TStringList;
  begin
    EnableBFDecimal;
    ArrayText := TStringList.Create;
    s := StripNonConforming(s, FloatCharSet);
    SFParseDelimited(ArrayText, trim(s), ' ');
    if ArrayText.Count = 3 then
    begin
      Result[0] := StrToFloatDef(ArrayText[0], 0.0);
      Result[1] := StrToFloatDef(ArrayText[1], 0.0);
      Result[2] := StrToFloatDef(ArrayText[2], 0.0);
      Result[3] := 1.0;
    end;
    ArrayText.Free;
    DisableBFDecimal;
  end;

begin
  FParsed := false;
  Clear;

  for Line := 0 to Data.Count - 1 do
  begin

    if not TitleSubShader then
    begin
      Pos := SFUniPos('subshader', Data[Line]);
      if Pos > 0 then
      begin
        TitleSubShader := true;

        Resource := TRsResource.Create;
        Resource.MaterialDiffuse := clrWhite;
        Resource.MaterialSpecular := clrBlack;
        Resource.MaterialSpecularPower := 0.0;
        Resource.Lighting := true;
        Resource.LightingSpecular := false;
        Resource.Transparent := false;
        Resource.DepthWrite := true;
        Add(Resource);
      end;
    end;

    if TitleSubShader then
    begin
      Pos := SFUniPos('{', Data[Line]);
      if Pos > 0 then
      begin
        TitleSubShader := false;
        SectionSubShader := true;
      end;
    end;

    if TitleSubShader then
    begin
      Resource.Name := SFBetween('"', Data[Line]);
    end;

    if SectionSubShader then
    begin
      Pos := SFUniPos('}', Data[Line]);
      if Pos > 0 then
      begin
        SectionSubShader := false;
      end;
    end;

    if SectionSubShader then
    begin

      Pos := SFUniPos('MaterialDiffuse ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.MaterialDiffuse := ParseInlineVectorData(SFRight(' ',Data[Line]));
      end;

      Pos := SFUniPos('MaterialSpecular ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.MaterialSpecular := ParseInlineVectorData(SFRight(' ',Data[Line]));
      end;

      Pos := SFUniPos('MaterialSpecularPower ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.MaterialSpecularPower := StrToFloatDef(SFBetweenTwo(' ', ';', Data[Line]), 1.0);
      end;

      Pos := SFUniPos('Lighting ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.Lighting := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := SFUniPos('LightingSpecular ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.LightingSpecular := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := SFUniPos('TwoSided ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.TwoSided := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := SFUniPos('Transparent ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.Transparent := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := SFUniPos('DepthWrite ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.DepthWrite := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := SFUniPos('BlendSrc ', Data[Line]);
      if Pos > 0 then
      begin
        BldFun := SFBetweenTwo(' ', ';', Data[Line]);

        if SFUniPos('sourceAlpha ', BldFun) > 0 then
          Resource.BlendSrc := bfSrcAlpha
        else
        if SFUniPos('one ', BldFun) > 0 then
          Resource.BlendSrc := bfOne
        else
          Resource.BlendSrc := bfZero;
      end;

      Pos := SFUniPos('BlendDest ', Data[Line]);
      if Pos > 0 then
      begin
        BldFun := SFBetweenTwo(' ', ';', Data[Line]);

        if SFUniPos('sourceAlpha ', BldFun) > 0 then
          Resource.BlendDest := bfSrcAlpha
        else
        if SFUniPos('one ', BldFun) > 0 then
          Resource.BlendDest := bfOne
        else
          Resource.BlendSrc := bfZero;
      end;

      Pos := SFUniPos('AlphaTestRef ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.AlphaTestRef := StrToFloatDef(SFBetweenTwo(' ', ';', Data[Line]), 1.0);
      end;

      Pos := SFUniPos('texture ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.Texture := SFBetween('"', Data[Line]) +'.dds';
      end;

    end;

    if (Count > 0) and (Line = Data.Count-1) then
      FParsed := true;
  end;

end;

end.
