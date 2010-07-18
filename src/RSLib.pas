unit RSLib;

interface

uses
   Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Generics.Collections,
   GLState;

type

  TRsResource = class
  private
  public
    Name : string;
    SubType : string;
    MaterialDiffuse : Array[1..3] of single;
    MaterialSpecular : Array[1..3] of single;
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
  StringFunction;

constructor TRsParser.Create;
begin
  inherited;
  FParsed := false;
end;

procedure TRsParser.Parse(Data: TStringList);
var
  Line, Pos : Integer;
  Path, BldFun : string;
  Resource : TRsResource;

  SectionSubShader : boolean;
  TitleSubShader : boolean;
begin
  FParsed := false;
  Clear;

  for Line := 0 to Data.Count - 1 do
  begin

    if not TitleSubShader then
    begin
      Pos := AnsiPos('subshader', Data[Line]);
      if Pos > 0 then
      begin
        TitleSubShader := true;

        Resource := TRsResource.Create;
        Resource.Transparent := false;
        Resource.DepthWrite := true;
        Add(Resource);
      end;
    end;

    if TitleSubShader then
    begin
      Pos := AnsiPos('{', Data[Line]);
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
      Pos := AnsiPos('}', Data[Line]);
      if Pos > 0 then
      begin
        SectionSubShader := false;
      end;
    end;

    if SectionSubShader then
    begin

      Pos := AnsiPos('MaterialSpecularPower ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.MaterialSpecularPower := StrToFloatDef(SFBetweenTwo(' ', ';', Data[Line]), 1.0);
      end;

      Pos := AnsiPos('lighting ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.Lighting := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := AnsiPos('lightingSpecular ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.LightingSpecular := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := AnsiPos('TwoSided ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.TwoSided := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := AnsiPos('Transparent ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.Transparent := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := AnsiPos('DepthWrite ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.DepthWrite := StrToBoolDef(SFBetweenTwo(' ', ';', Data[Line]), false);
      end;

      Pos := AnsiPos('BlendSrc ', Data[Line]);
      if Pos > 0 then
      begin
        BldFun := SFBetweenTwo(' ', ';', Data[Line]);

        if AnsiPos('sourceAlpha ', BldFun) > 0 then
          Resource.BlendSrc := bfSrcAlpha
        else
        if AnsiPos('one ', BldFun) > 0 then
          Resource.BlendSrc := bfOne
        else
          Resource.BlendSrc := bfZero;
      end;

      Pos := AnsiPos('BlendDest ', Data[Line]);
      if Pos > 0 then
      begin
        BldFun := SFBetweenTwo(' ', ';', Data[Line]);

        if AnsiPos('sourceAlpha ', BldFun) > 0 then
          Resource.BlendDest := bfSrcAlpha
        else
        if AnsiPos('one ', BldFun) > 0 then
          Resource.BlendDest := bfOne
        else
          Resource.BlendSrc := bfZero;
      end;


      Pos := AnsiPos('AlphaTestRef ', Data[Line]);
      if Pos > 0 then
      begin
        Resource.AlphaTestRef := StrToFloatDef(SFBetweenTwo(' ', ';', Data[Line]), 1.0);
      end;

      Pos := AnsiPos('texture ', Data[Line]);
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
