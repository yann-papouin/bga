unit VirtualTreeviewTheme;

interface

uses
  DbugIntf,
  Windows, Graphics, VirtualTrees, SpTBXControls, SpTBXSkins;

type

  TSpTBXVTHeader = class(TObject)
  private
    FOnHeaderDrawQueryElements: TVTHeaderPaintQueryElementsEvent;
    FOnAdvancedHeaderDraw: TVTAdvancedHeaderPaintEvent;
  protected
    procedure HeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
    procedure AdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
  public

  end;

  procedure EnableSkinning(Sender: TVirtualStringTree);

implementation

var
  ASpTBXVTHeader : TSpTBXVTHeader;

function MakeProcedureOfObject(Target : Pointer):TMethod;
begin
 Result.code := Target;
 Result.Data := nil;
end;


procedure TSpTBXVTHeader.HeaderDrawQueryElements(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; var Elements: THeaderPaintElements);
begin
  if not (SkinManager.GetSkinType <= sknWindows) then
    Elements := Elements + [hpeBackground];

  if Assigned(FOnHeaderDrawQueryElements) then
    FOnHeaderDrawQueryElements(Sender, PaintInfo, Elements);
end;

procedure TSpTBXVTHeader.AdvancedHeaderDraw(Sender: TVTHeader; var PaintInfo: THeaderPaintInfo; const Elements: THeaderPaintElements);
begin
  if not (SkinManager.GetSkinType <= sknWindows) then
  begin
    if hpeBackground in Elements then
      SpDrawXPHeader(PaintInfo.TargetCanvas, PaintInfo.PaintRectangle,
        PaintInfo.IsHoverIndex, PaintInfo.IsDownIndex, sknSkin);
  end;

  if Assigned(FOnAdvancedHeaderDraw) then
    FOnAdvancedHeaderDraw(Sender, PaintInfo, Elements);
end;


procedure EnableSkinning(Sender: TVirtualStringTree);
begin
  Sender.Header.Options := Sender.Header.Options + [hoHotTrack, hoOwnerDraw];
 // Sender.TreeOptions.PaintOptions := Sender.TreeOptions.PaintOptions + [toHideFocusRect, toAlwaysHideSelection];

  if Assigned(Sender.OnHeaderDrawQueryElements) then
    ASpTBXVTHeader.FOnHeaderDrawQueryElements := Sender.OnHeaderDrawQueryElements;

  if Assigned(Sender.OnAdvancedHeaderDraw) then
    ASpTBXVTHeader.FOnAdvancedHeaderDraw := Sender.OnAdvancedHeaderDraw;

  Sender.OnHeaderDrawQueryElements := ASpTBXVTHeader.HeaderDrawQueryElements;
  Sender.OnAdvancedHeaderDraw := ASpTBXVTHeader.AdvancedHeaderDraw;
end;


initialization
  ASpTBXVTHeader := TSpTBXVTHeader.Create;
finalization
  ASpTBXVTHeader.Free;
end.
