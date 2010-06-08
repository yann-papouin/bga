unit GuiPicView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GuiFormCommon, GLScene, GLObjects, GLCoordinates, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, DDS, GLSimpleNavigation, GLBitmapFont,
  GLWindowsFont, GLHUDObjects, GLCadencer, jpeg, tga, pngimage, GraphicEx;

type
  TSinglePoint = record
    X : single;
    Y : single;
  end;


  TPICViewForm = class(TFormCommon)
    GLScene: TGLScene;
    Viewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    GLFilename: TGLHUDText;
    GLResolution: TGLHUDText;
    Cadencer: TGLCadencer;
    Plane: TGLPlane;
    GLZoom: TGLAbsoluteHUDText;
    GLZoomPlane: TGLPlane;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerDblClick(Sender: TObject);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Déclarations privées }
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
  Math, Types;

const
  MIN_SCENE_SCALE = 0.01;
  ZOOM_SPEED = 5;
  PAN_SPEED = 5;

{ TDDSViewForm }

procedure TPICViewForm.Preview;
begin
  Reset;
  Show;
  Cadencer.Enabled := true;
end;


procedure TPICViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Cadencer.Enabled := false;
end;

procedure TPICViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FScale := GLCamera.SceneScale;

end;

procedure TPICViewForm.LoadTexture(Filename: string);
begin
  Cadencer.Enabled := false;

  FCamOffset.X := 0;
  FCamOffset.Y := 0;

  Title := Filename;

  Plane.Material.Texture.Enabled := true;
  Plane.Material.Texture.Image.LoadFromFile(Filename);

  FTxWidth := Plane.Material.Texture.Image.Width;
  FTxHeight := Plane.Material.Texture.Image.Height;

  Plane.Width := FTxWidth;
  Plane.Height := FTxHeight;

  GLFilename.Text := ExtractFileName(Filename);
  GLResolution.Text := Format('%dx%d',[FTxWidth, FTxHeight]);
end;

const
  TEST = 10;
  MAX_AUTOZOOM = 1.0;

procedure TPICViewForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
//var
  //mx, my:single;
  //PixelPosition: TSinglePoint;
begin
  inherited;
(*
  PixelPosition := GetPixelPositionFromPosition(FAutoZoomInfo.MousePosition);
  GLResolution.Text := Format('%.3f, %.3f',[PixelPosition.X,PixelPosition.Y]);
  GLResolution.Text :=  GLResolution.Text + Format('   %.3f, %.3f',[FAutoZoomInfo.PixelPosition.X,FAutoZoomInfo.PixelPosition.Y]);

  if (FAutoZoomInfo.MousePosition.X <> 0) or (FAutoZoomInfo.MousePosition.Y <> 0)  then
  begin

    case CompareValue(PixelPosition.X, FAutoZoomInfo.PixelPosition.X, 0.1) of
    LessThanValue   : FAutoPanOffset.X := FAutoPanOffset.X + abs(PixelPosition.X - FAutoZoomInfo.PixelPosition.X)*FIntScale;
    GreaterThanValue: FAutoPanOffset.X := FAutoPanOffset.X - abs(PixelPosition.X - FAutoZoomInfo.PixelPosition.X)*FIntScale;
    end;

    case CompareValue(PixelPosition.Y, FAutoZoomInfo.PixelPosition.Y, 0.1) of
    LessThanValue   : FAutoPanOffset.Y := FAutoPanOffset.Y + abs(PixelPosition.Y - FAutoZoomInfo.PixelPosition.Y)*FIntScale;
    GreaterThanValue: FAutoPanOffset.Y := FAutoPanOffset.Y - abs(PixelPosition.Y - FAutoZoomInfo.PixelPosition.Y)*FIntScale;
    end;
    ResetAutoZoom;
  end;
*)
  //GLResolution.Text := Format('DIFF: = %.3f, %.3f',[abs(PixelPosition.X - FAutoZoomInfo.PixelPosition.X), abs(PixelPosition.Y - FAutoZoomInfo.PixelPosition.Y)]);

  case CompareValue(FCamOffset.X, FAutoPanOffset.X, 0.1) of
  LessThanValue   : FCamOffset.X := FCamOffset.X + abs(FCamOffset.X - FAutoPanOffset.X)/ PAN_SPEED;
  GreaterThanValue: FCamOffset.X := FCamOffset.X - abs(FCamOffset.X - FAutoPanOffset.X)/ PAN_SPEED;
  end;

  case CompareValue(FCamOffset.Y, FAutoPanOffset.Y, 0.1) of
  LessThanValue   : FCamOffset.Y := FCamOffset.Y + abs(FCamOffset.Y - FAutoPanOffset.Y)/ PAN_SPEED;
  GreaterThanValue: FCamOffset.Y := FCamOffset.Y - abs(FCamOffset.Y - FAutoPanOffset.Y)/ PAN_SPEED;
  end;

  GLCamera.Position.X := -FTxWidth/2 + (FTxWidth - Viewer.Width)/2 + FCamOffset.X;
  GLCamera.Position.Y := -FTxHeight/2 + (FTxHeight - Viewer.Height)/2 + FCamOffset.Y;

(*
  mx :=  FMousePos.X + GLCamera.Position.X;
  my := -FMousePos.Y + GLCamera.Position.Y + Viewer.Height;
  GLResolution.Text := GLResolution.Text + #10#13 + Format('%.3f, %.3f',[mx,my]);

  mx := mx / FScale;
  my := my / FScale;
  GLResolution.Text := GLResolution.Text + Format('    %.3f, %.3f',[mx,my]);
*)

  case CompareValue(FScale, FIntScale, 0.001) of
  EqualsValue     :
  begin
    GLZoomPlane.Visible := false;
    Exit;
  end;
  LessThanValue   : FScale := FScale + abs(FScale - FIntScale)/ ZOOM_SPEED;
  GreaterThanValue: FScale := FScale - abs(FScale - FIntScale)/ ZOOM_SPEED;
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

  Plane.Width := FTxWidth * FScale;
  Plane.Height := FTxHeight * FScale;
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
  Reset;
end;


procedure TPICViewForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  FDragDelta : TSinglePoint;
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
    FDragDelta.X := X - FInitPos.X;
    FDragDelta.Y := Y - FInitPos.Y;

    FAutoPanOffset.X := -FDragDelta.X;
    FAutoPanOffset.Y :=  FDragDelta.Y;
  end;

  FMoving := false;
end;

procedure TPICViewForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  FInitPos.X := X + FCamOffset.X;
  FInitPos.Y := Y - FCamOffset.Y;

  Viewer.Cursor := crHandPoint;
end;

procedure TPICViewForm.ViewerMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Viewer.Cursor := crDefault;
end;

end.
