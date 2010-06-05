unit GuiDDSView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GuiFormCommon, GLScene, GLObjects, GLCoordinates, GLWin32Viewer,
  GLCrossPlatform, BaseClasses, DDS, GLSimpleNavigation, GLBitmapFont,
  GLWindowsFont, GLHUDObjects, GLCadencer;

type
  TDDSViewForm = class(TFormCommon)
    GLScene: TGLScene;
    Viewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLLightSource1: TGLLightSource;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    GLFilename: TGLHUDText;
    GLResolution: TGLHUDText;
    Cadencer: TGLCadencer;
    GLCameraPosition: TGLHUDText;
    MouseWheelDelta: TGLHUDText;
    Plane: TGLPlane;
    Background: TGLPlane;
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CadencerProgress(Sender: TObject; const deltaTime,
      newTime: Double);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ViewerDblClick(Sender: TObject);
  private
    { Déclarations privées }
    FInitPos : TPoint;
    FMousePos : TPoint;
    FCamOffset : TPoint;
    FMoving : boolean;
    FIntScale : single;
    FScale : single;
    FTxHeight : integer;
    FTxWidth : integer;
  public
    { Déclarations publiques }
    procedure LoadTexture(Filename: string);
  end;

var
  DDSViewForm: TDDSViewForm;

implementation

{$R *.dfm}

uses
  Math, Types;

const
  MIN_SCENE_SCALE = 0.001;

{ TDDSViewForm }

procedure TDDSViewForm.FormShow(Sender: TObject);
begin
  inherited;
  FScale := 1;
  FIntScale := FScale;
  Cadencer.Enabled := true;
end;

procedure TDDSViewForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Cadencer.Enabled := false;
end;

procedure TDDSViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  FScale := GLCamera.SceneScale;

  LoadTexture('C:\Users\Yann\Pictures\n1215558181_57804_1999.jpg');
  Show;
end;


procedure TDDSViewForm.CadencerProgress(Sender: TObject; const deltaTime, newTime: Double);
var
  mx, my : single;
begin
  inherited;

  mx :=  FMousePos.X - GLCamera.Position.X - Viewer.Width;
  my := -FMousePos.Y + GLCamera.Position.Y + Viewer.Height;
  GLCameraPosition.Text := Format('%.3f, %.3f',[mx,my]);

  GLCamera.Position.X := -FTxWidth/2 + (FTxWidth - Viewer.Width)/2 + FCamOffset.X;
  GLCamera.Position.Y := -FTxHeight/2 + (FTxHeight - Viewer.Height)/2 + FCamOffset.Y;

  case CompareValue(FScale, FIntScale, 0.005) of
  EqualsValue     : Exit;
  LessThanValue   : FScale := FScale + abs(FScale - FIntScale)/ 20;
  GreaterThanValue: FScale := FScale - abs(FScale - FIntScale)/ 20;
  end;

  if FScale < MIN_SCENE_SCALE then
  begin
    FScale := MIN_SCENE_SCALE;
    FIntScale := MIN_SCENE_SCALE;
  end;

  Plane.Width := FTxWidth * FScale;
  Plane.Height := FTxHeight * FScale;

  //FCamOffset.X := FCamOffset.X - Round(Abs(mx - FCamOffset.X)/20);
  //FCamOffset.Y := FCamOffset.Y - Round(Abs(my - FCamOffset.Y)/20);

  //GLCameraPosition.Text := FloatToStr(FScale);

end;

procedure TDDSViewForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  Handled := True;
  FIntScale := FIntScale + WheelDelta / 1000;
  MouseWheelDelta.Text := FloatToStr(FScale);
end;


procedure TDDSViewForm.LoadTexture(Filename: string);
begin
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

procedure TDDSViewForm.ViewerDblClick(Sender: TObject);
begin
  inherited;
  FCamOffset.X := 0;
  FCamOffset.Y := 0;
end;

procedure TDDSViewForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FInitPos.X := X + FCamOffset.X;
  FInitPos.Y := Y - FCamOffset.Y;
end;

procedure TDDSViewForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  FDragDelta : TPoint;
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

    FCamOffset.X := -FDragDelta.X;
    FCamOffset.Y :=  FDragDelta.Y;
  end;

  FMoving := false;
end;

end.
