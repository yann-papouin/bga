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
 * The Original Code is GuiPicView (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiPicView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Contnrs,
  Dialogs, GuiFormCommon, GLScene, GLObjects, GLCoordinates, GLWin32Viewer, GLNodes,
  GLCrossPlatform, BaseClasses, DDSImage, GLSimpleNavigation, GLBitmapFont, GLMultiPolygon,
  GLWindowsFont, GLHUDObjects, GLCadencer, jpeg, tga, pngimage, GraphicEx, StdCtrls, GLGeomObjects,
  VectorTypes, VectorGeometry;

type

  TGLArea = class;

  TDrawMode =
  (
    dmFreeform,
    dmRectangle,
    dmEllipse
  );

  TGLAreaPencil = class(TGLLines)
  private
    FCross : TGLLines;
    FCursorNode: TGLNode;
    FPencilPosition: TVector;
    FScreenPoint: TPoint;
    FDrawMode: TDrawMode;
    //function GetLastNode: TGLNode;
    procedure SetScreenPoint(const Value: TPoint);
  public
    InitialPosition : TVector;
    property ScreenPoint : TPoint read FScreenPoint write SetScreenPoint;
    property PencilPosition : TVector read FPencilPosition; // Position in scene
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsDrawing : Boolean;
    procedure Cancel;
    procedure Undo;
    property CursorNode : TGLNode read FCursorNode;
    property DrawMode : TDrawMode read FDrawMode;
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  end;

  TGLArea = class(TGLPolygon)
  private
    FContour: TGLLines;
  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoProgress(const progressTime: TProgressTimes); override;
    property Contour : TGLLines read FContour;
  end;

  TAreaPod = class(TObjectList)
  private
    function GetItem(AIndex: integer): TGLArea;
    procedure SetItem(AIndex: integer; const Value: TGLArea);
  public
    function Add(AItem: TGLArea): integer;
    property Items[AIndex: integer]: TGLArea read GetItem write SetItem; default;
  end;


  TSinglePoint = record
    X : single;
    Y : single;
  end;


  TPICViewForm = class(TFormCommon)
    GLScene: TGLScene;
    Viewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    Cadencer: TGLCadencer;
    GLZoom: TGLAbsoluteHUDText;
    GLZoomPlane: TGLPlane;
    Background: TGLPlane;
    GLFileInfo: TGLHUDText;
    WindowsBitmapFontBold: TGLWindowsBitmapFont;
    Invariant: TGLDummyCube;
    PicturePlane: TGLPlane;
    Scalable: TGLDummyCube;
    GLLines1: TGLLines;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerDblClick(Sender: TObject);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);
  private
    { Déclarations privées }
    FPencil : TGLAreaPencil;
    FInitPos : TSinglePoint;
    FMousePos : TSinglePoint;
    FCamOffset : TSinglePoint;
    FAutoPanOffset : TSinglePoint;
    FMoving : boolean;
    FIntScale : single;
    FScale : single;
    FTxHeight : integer;
    FTxWidth : integer;
    procedure Reset;
  public
    { Déclarations publiques }
    procedure LoadTexture(Filename: string);
    procedure Preview;
  end;

var
  PICViewForm: TPICViewForm;

implementation

{$R *.dfm}

uses
  Math, Types, GLMaterial, GLColor, Dbugintf;

const
  MIN_SCENE_SCALE = 0.01;
  CAM_SPEED = 1/6;

  idX=0;
  idY=1;
  idZ=2;
  idW=3;

{ TDDSViewForm }

function SimpleRoundTo(const AValue: TVector; const ADigit: TRoundToRange = -2): TVector;
begin
  Result[0] := Math.SimpleRoundTo(AValue[0], ADigit);
  Result[1] := Math.SimpleRoundTo(AValue[1], ADigit);
  Result[2] := Math.SimpleRoundTo(AValue[2], ADigit);
  Result[3] := Math.SimpleRoundTo(AValue[3], ADigit);
end;


procedure TPICViewForm.Preview;
begin
  Reset;
  Show;
  Cadencer.Enabled := true;
end;


procedure TPICViewForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  inherited;
  Cadencer.Enabled := false;
end;

procedure TPICViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FScale := 1;
  FPencil := TGLAreaPencil.Create(Self);
  FPencil.Parent := Scalable;
  FPencil.FCross.Parent := GLScene.Objects;
end;

procedure TPICViewForm.FormDestroy(Sender: TObject);
begin
  FPencil.Free;
  inherited;
end;

procedure TPICViewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  KEY_OFFSET = 100;
  SCALE_OFFSET = 0.1;
begin
  inherited;

  if Key = VK_ESCAPE then
  begin
    FPencil.Cancel;
  end;

  if Key = VK_BACK then
  begin
    FPencil.Undo;
  end;


  if ssCtrl in Shift then
  begin
    if Key = VK_DOWN then
      FIntScale := FIntScale + (SCALE_OFFSET*FIntScale);
    if Key = VK_UP then
      FIntScale := FIntScale - (SCALE_OFFSET*FIntScale);
  end;

  if Key = VK_LEFT then
    FAutoPanOffset.X := FAutoPanOffset.X - KEY_OFFSET;
  if Key = VK_RIGHT then
    FAutoPanOffset.X := FAutoPanOffset.X + KEY_OFFSET;

  if not (ssCtrl in Shift) then
  begin
    if Key = VK_DOWN then
      FAutoPanOffset.Y := FAutoPanOffset.Y - KEY_OFFSET;
    if Key = VK_UP then
      FAutoPanOffset.Y := FAutoPanOffset.Y + KEY_OFFSET;
  end;
end;

procedure TPICViewForm.LoadTexture(Filename: string);
begin
  Cadencer.Enabled := false;

  FCamOffset.X := 0;
  FCamOffset.Y := 0;

  Title := Filename;

  PicturePlane.Material.Texture.Enabled := true;
  PicturePlane.Material.Texture.Image.LoadFromFile(Filename);

  FTxWidth := PicturePlane.Material.Texture.Image.Width;
  FTxHeight := PicturePlane.Material.Texture.Image.Height;

  PicturePlane.Width := FTxWidth;
  PicturePlane.Height := FTxHeight;

  GLFileInfo.Text := Format('%s (%dx%d)',[ExtractFileName(Filename), FTxWidth, FTxHeight]);
end;


procedure TPICViewForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
begin
  inherited;

  case CompareValue(FCamOffset.X, FAutoPanOffset.X, 0.1) of
  LessThanValue   : FCamOffset.X := FCamOffset.X + abs(FCamOffset.X - FAutoPanOffset.X)* CAM_SPEED;
  GreaterThanValue: FCamOffset.X := FCamOffset.X - abs(FCamOffset.X - FAutoPanOffset.X)* CAM_SPEED;
  end;

  case CompareValue(FCamOffset.Y, FAutoPanOffset.Y, 0.1) of
  LessThanValue   : FCamOffset.Y := FCamOffset.Y + abs(FCamOffset.Y - FAutoPanOffset.Y)* CAM_SPEED;
  GreaterThanValue: FCamOffset.Y := FCamOffset.Y - abs(FCamOffset.Y - FAutoPanOffset.Y)* CAM_SPEED;
  end;

  GLCamera.Position.X :=  (-Viewer.Width)/2  + FCamOffset.X;
  GLCamera.Position.Y :=  (-Viewer.Height)/2 + FCamOffset.Y;

  GLFileInfo.Position.X := Viewer.Width/2;
  GLFileInfo.Position.Y := Viewer.Height - 20;

  case CompareValue(FScale, FIntScale, 0.001) of
  EqualsValue     :
  begin
    GLZoomPlane.Visible := false;
    Exit;
  end;
  LessThanValue   : FScale := FScale + abs(FScale - FIntScale)* CAM_SPEED;
  GreaterThanValue: FScale := FScale - abs(FScale - FIntScale)* CAM_SPEED;
  end;

  if FScale < MIN_SCENE_SCALE then
  begin
    FScale := MIN_SCENE_SCALE;
    FIntScale := MIN_SCENE_SCALE;
  end;

  GLZoom.Text := Format('%d%%',[Round(FScale * 100)]);
  GLZoomPlane.Visible := true;
  GLZoomPlane.Position.X := GLCamera.Position.X + Viewer.Width/2;
  GLZoomPlane.Position.Y := GLCamera.Position.Y + Viewer.Height/2;

  Scalable.Scale.X := FScale;
  Scalable.Scale.Y := FScale;
end;


procedure TPICViewForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  InitialPixelPos, FinalPixelPos : TSinglePoint;

  function GetPixelPosFromScale(MousePosition : TSinglePoint; Scale : Single) : TSinglePoint;
  begin
    Result.X := ( MousePosition.X + GLCamera.Position.X)/Scale;
    Result.Y := (-MousePosition.Y + GLCamera.Position.Y + Viewer.Height)/Scale;
  end;

begin
  inherited;
  Handled := True;

  InitialPixelPos:= GetPixelPosFromScale(FMousePos, FScale);
  FIntScale := FIntScale + WheelDelta / (1000/FIntScale);
  FinalPixelPos := GetPixelPosFromScale(FMousePos, FIntScale);

  case CompareValue(FinalPixelPos.X, InitialPixelPos.X, 0.1) of
  LessThanValue   : FAutoPanOffset.X := FCamOffset.X + abs(FinalPixelPos.X - InitialPixelPos.X)*FIntScale;
  GreaterThanValue: FAutoPanOffset.X := FCamOffset.X - abs(FinalPixelPos.X - InitialPixelPos.X)*FIntScale;
  end;

  case CompareValue(FinalPixelPos.Y, InitialPixelPos.Y, 0.1) of
  LessThanValue   : FAutoPanOffset.Y := FCamOffset.Y + abs(FinalPixelPos.Y - InitialPixelPos.Y)*FIntScale;
  GreaterThanValue: FAutoPanOffset.Y := FCamOffset.Y - abs(FinalPixelPos.Y - InitialPixelPos.Y)*FIntScale;
  end;

end;

procedure TPICViewForm.Reset;
begin
  FAutoPanOffset.X := 0;
  FAutoPanOffset.Y := 0;
  FIntScale := 1;
end;



procedure TPICViewForm.ViewerDblClick(Sender: TObject);
begin
  inherited;
  if not FPencil.IsDrawing then
    Reset;
end;



procedure TPICViewForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if FMoving then
    Exit
  else
    FMoving := true;

  FMousePos.X := X;
  FMousePos.Y := Y;

  if ssLeft in Shift then
  begin
    FAutoPanOffset.X := -(X - FInitPos.X);
    FAutoPanOffset.Y :=  (Y - FInitPos.Y);
  end;

  FPencil.MouseMove(Sender, Shift, X, Y);

  FMoving := false;
end;

procedure TPICViewForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FInitPos.X := X + FCamOffset.X;
  FInitPos.Y := Y - FCamOffset.Y;

  Viewer.Cursor := crHandPoint;

  FPencil.MouseDown(Sender, Button, Shift, X, Y);
end;

procedure TPICViewForm.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Viewer.Cursor := crDefault;
end;


{ TAreaPod }

function TAreaPod.Add(AItem: TGLArea): integer;
begin
  Result := inherited Add(AItem);
end;

function TAreaPod.GetItem(AIndex: integer): TGLArea;
begin
  Result := Get(AIndex);
end;

procedure TAreaPod.SetItem(AIndex: integer; const Value: TGLArea);
begin
  Put(AIndex, Value);
end;

{ TGLArea }

constructor TGLArea.Create(AOwner: TComponent);
begin
  inherited;
  //Parts := [ppTop];
  FContour := TGLLines.Create(Self);
  FContour.Parent := Self;
  FContour.AntiAliased := True;
  FContour.LineWidth := 2;
  FContour.NodesAspect := lnaInvisible;
  FContour.SplineMode := lsmLines;
end;

destructor TGLArea.Destroy;
begin
  FContour.Free;
  inherited;
end;

procedure TGLArea.DoProgress(const progressTime: TProgressTimes);
begin
  inherited;

end;

{ TGLAreaPencil }

constructor TGLAreaPencil.Create(AOwner: TComponent);
begin
  inherited;
  FDrawMode := dmRectangle;
  Parent := Self;
  LineColor.Color := clrBlue;
  AntiAliased := True;
  LineWidth := 2;
  SplineMode := lsmLines;

  FCross := TGLLines.Create(Self);
  FCross.AntiAliased := True;
  FCross.LineColor.Color := clrWhite;
  FCross.LineColor.Alpha := 0.5;
  FCross.LinePattern := 255;
  FCross.NodesAspect := lnaInvisible;
  FCross.SplineMode := lsmSegments;
  FCross.Parent := Parent;
  FCross.Nodes.Add;
  FCross.Nodes.Add;
  FCross.Nodes.Add;
  FCross.Nodes.Add;
end;


destructor TGLAreaPencil.Destroy;
begin
  FCross.Free;
  inherited;
end;

(*
function TGLAreaPencil.GetLastNode: TGLNode;
begin
  if Nodes.Count >= 2 then
    Result := Nodes[Nodes.Count-2]
  else
    Result := nil;
end;
*)

function TGLAreaPencil.IsDrawing: Boolean;
begin
  Result := FCursorNode<>nil;
end;


procedure TGLAreaPencil.MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Area : TGLArea;
begin
  if not IsDrawing and (ssCtrl in Shift) then
  begin

    if DrawMode = dmFreeform then
    begin
      FCursorNode := Nodes.Add;
      FCursorNode.AsVector := FPencilPosition;
    end;

    if DrawMode = dmRectangle then
    begin
      FCursorNode := Nodes.Add;
      FCursorNode.AsVector := FPencilPosition;
    end;

    MoveLast;
  end;

  if IsDrawing then
  begin

    if DrawMode = dmFreeform then
    begin
      if ssDouble in Shift then
      begin
        FCursorNode.AsVector := Nodes.First.AsVector;
      end;

      // Check close
      if (Nodes.Count>=2) and VectorEquals(Nodes.First.AsVector, Nodes.Last.AsVector) then
      begin
        Area := TGLArea.Create(Self);
        Area.Nodes.Assign(Nodes);
        Area.Parent := Parent;

        Area.Contour.Nodes.Assign(Nodes);
        Area.Contour.LineWidth := 2;

        Area.Material.BlendingMode := bmTransparency;
        Area.Material.FrontProperties.Emission.RandomColor;
        Area.Material.FrontProperties.Diffuse.Alpha := 0.5;
        Area.Contour.LineColor := Area.Material.FrontProperties.Emission;

        Cancel;
      end
        else // Continue
      begin
        InitialPosition := FCursorNode.AsVector;
        FCursorNode := Nodes.Add;
        FCursorNode.AsVector := FPencilPosition;
      end;
    end;


    if DrawMode = dmRectangle then
    begin

      // Check close
      if (Nodes.Count>=4) then
      begin
        Area := TGLArea.Create(Self);
        Area.Nodes.Assign(Nodes);
        Area.Parent := Parent;

        Area.Contour.Nodes.Assign(Nodes);
        Area.Contour.LineWidth := 2;

        Area.Material.BlendingMode := bmTransparency;
        Area.Material.FrontProperties.Emission.RandomColor;
        Area.Material.FrontProperties.Diffuse.Alpha := 0.5;
        Area.Contour.LineColor := Area.Material.FrontProperties.Emission;

        Cancel;
      end
        else // Continue
      begin
        InitialPosition := FCursorNode.AsVector;
        Nodes.Add;
        FCursorNode := Nodes.Add;
        Nodes.Add;
        Nodes.Add;
        FCursorNode.AsVector := FPencilPosition;
      end;

    end;

  end;

end;

procedure TGLAreaPencil.MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Point : TPoint;
  PosA, PosB, PosC, PosD : TVector;
  Slope : Extended;
  AngleRad : Extended;
  AngleDeg : Extended;
begin

  Point.X := X;
  Point.Y := Y;

  ScreenPoint := Point;

  if IsDrawing then
  begin
    // Get current mouse position as Point A
    PosA := PencilPosition;
    // Get position of the previous point added as Point B
    PosB := InitialPosition;

    if DrawMode = dmFreeform then
    begin

      // Convert 2-Points data to Angles
      AngleRad := ArcTan2(PosA[idY]-PosB[idY], PosA[idX]-PosB[idX]);
      AngleDeg := RadToDeg(AngleRad);

      //SendDebugFmt('Teta(rad) = %.3f,  Teta(deg) = %.3f', [AngleRad, AngleDeg]);

      if not IsNan(AngleDeg) then
      begin
        if ssCtrl in Shift then
          AngleDeg := Math.SimpleRoundTo(AngleDeg, 1)
        else
          AngleDeg := Math.SimpleRoundTo(AngleDeg, 0);

        AngleRad := DegToRad(AngleDeg);
        Slope := Tan(AngleRad);

        if ((Abs(AngleDeg) >= 0) and (Abs(AngleDeg) <= 45)) or (Abs(AngleDeg) >= 145) then
        begin
          // Affect Y value only
          PosA[idY] := Slope*(PosA[idX]-PosB[idX]) + PosB[idY];
        end
          else
        begin
          // Affect X value only
          PosA[idX] := (PosA[idY]-PosB[idY])/Slope+PosB[idX];
        end;
      end;

      CursorNode.AsVector := PosA;
    end;

    if DrawMode = dmRectangle then
    begin
      CursorNode.AsVector := PosA;

      PosC[idX] := PosA[idX];
      PosC[idY] := PosB[idY];

      PosD[idX] := PosB[idX];
      PosD[idY] := PosA[idY];

      Nodes[1].AsVector := PosC;
      Nodes[3].AsVector := PosD;
      Nodes[4].AsVector := PosB;
    end;


  end;

end;

procedure TGLAreaPencil.SetScreenPoint(const Value: TPoint);
var
  UnscalePosition : TVector;
const
  A=0;
  B=1;
  C=2;
  D=3;
begin
  FScreenPoint := Value;

  // Get scene position from ScreenPoint
  FPencilPosition := VectorMake(Scene.CurrentBuffer.ScreenToWorld(FScreenPoint.X, FScreenPoint.Y));
  FPencilPosition := Parent.AbsoluteToLocal(FPencilPosition);
  FPencilPosition := SimpleRoundTo(FPencilPosition, 1);
  UnscalePosition := Parent.LocalToAbsolute(FPencilPosition);

  // Place cross
  FCross.Visible := IsDrawing;

  if FCross.Visible then
  begin
    FCross.Nodes[A].X := UnscalePosition[0];
    FCross.Nodes[B].X := UnscalePosition[0];

    FCross.Nodes[C].Y := UnscalePosition[1];
    FCross.Nodes[D].Y := UnscalePosition[1];

    FCross.Nodes[C].X := -Scene.CurrentBuffer.Width + Scene.CurrentGLCamera.Position.X;
    FCross.Nodes[D].X :=  Scene.CurrentBuffer.Width + Scene.CurrentGLCamera.Position.X;

    FCross.Nodes[A].Y := -Scene.CurrentBuffer.Height + Scene.CurrentGLCamera.Position.Y;
    FCross.Nodes[B].Y :=  Scene.CurrentBuffer.Height + Scene.CurrentGLCamera.Position.Y;
  end;
end;


procedure TGLAreaPencil.Cancel;
begin
  //Finalize drawing by releasing mouse cursor
  FCursorNode := nil;
  Nodes.Clear;
end;



procedure TGLAreaPencil.Undo;
begin
  if IsDrawing and (Nodes.Count > 2) then
  begin
    Nodes.Delete(Nodes.Last.Index);
    FCursorNode := Nodes.Last;
  end;
end;

end.
