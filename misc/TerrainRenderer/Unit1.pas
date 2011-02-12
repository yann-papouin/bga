unit Unit1;

interface

uses
  GLCrossPlatform,
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  GLScene,
  GLObjects,
  GLCadencer,
  ExtCtrls,
  GLWin32Viewer,
  oge2_TerrainRendering,
  oge2_TerrainDataSource,
  oge2_HeightMap,
  GLContext,
  StdCtrls,
  glTerrainRenderer,
  ComCtrls,
  GLCoordinates,
  BaseClasses,
  Jpeg,
  GLMaterial;

const
  MAX = 256;

type
  TForm1 = class(TForm)
    GLScene1: TGLScene;
    glsv: TGLSceneViewer;
    Panel1: TPanel;
    GLCadencer1: TGLCadencer;
    GLCamera: TGLCamera;
    GLDummyCube1: TGLDummyCube;
    Timer1: TTimer;
    cb1: TCheckBox;
    cb2: TCheckBox;
    DCCamera: TGLDummyCube;
    Button1: TButton;
    rg1: TRadioGroup;
    GLDummyCube2: TGLDummyCube;
    tb1: TTrackBar;
    Label1: TLabel;
    GLMaterialLibrary1: TGLMaterialLibrary;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HDSPreparingData(HeightData: TOGEHeightMap);
    procedure Timer1Timer(Sender: TObject);
    procedure cb1Click(Sender: TObject);
    procedure cb2Click(Sender: TObject);
    procedure GLCadencer1Progress(Sender: TObject;
      const deltaTime, newTime: Double);
    procedure glsvMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure tb1Change(Sender: TObject);
  private
    { Private declarations }
  public
    FMap: Array [-MAX .. MAX, -MAX .. MAX] of THeight;
    Terrain: TOGETerrainRendering;
    TerrainData: TOGEHeightDataSource;
    procedure CreateMap;
    { Public declarations }
  end;

var
  Form1: TForm1;
  NewMousePos: TPoint;
  defaultmousepos: TPoint;
  clickdrapasat, ClickApasat: boolean;

implementation

uses
  math,
  GLKeyboard;
{$R *.dfm}

procedure ResetMousePos;
begin
  SetCursorPos(defaultmousepos.X, defaultmousepos.Y);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  vIgnoreOpenglErrors := true;
  CreateMap;
  Terrain := TOGETerrainRendering.CreateAsChild(GLDummyCube1);
  // Terrain.Scale.SetVector(2.5,2.5,2.5);
  // Terrain.Direction.SetVector(0,1,0);
  // Terrain.Up.SetVector(0,0,1);
  TerrainData := TOGEHeightDataSource.Create;
  Terrain.HeightDataSource := TerrainData;
  Terrain.MaterialLibrary := GLMaterialLibrary1;
  Terrain.lodType := tlodNone;
  TerrainData.OnStartPreparingData := HDSPreparingData;
  TerrainData.MarkDirty(-200, -200, 200, 200);
  GLCamera.Pitch(-45);

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  TerrainData.Free;
  Terrain.Free;
end;

procedure TForm1.HDSPreparingData(HeightData: TOGEHeightMap);
var
  ii, jj: Integer;
begin
  with HeightData do
  begin
    MaterialName := 'LibMaterial';
    Allocate;
    for ii := 0 to Size - 1 do
    begin
      for jj := 0 to Size - 1 do
      begin
        SetHeight(ii, jj, FMap[XLeft + ii, Ytop + jj]);
      end;
    end;
    DataState := dsReady;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  caption := Format('Illyrium Terrain Renderer - FPS : %.1f',
    [glsv.buffer.FramesPerSecond]);
  glsv.buffer.ResetPerformanceMonitor;
end;

procedure TForm1.cb1Click(Sender: TObject);
begin
  Terrain.DrawTextured := cb1.checked;
  glsv.SetFocus;
end;

procedure TForm1.cb2Click(Sender: TObject);
begin
  Terrain.DrawWireframe := cb2.checked;
  glsv.SetFocus;
end;

procedure TForm1.CreateMap;
var
  ii, jj: Integer;
begin
  for ii := -MAX to MAX do
  begin
    for jj := -MAX to MAX do
    begin
      FMap[ii, jj] := round(Cos(ii / 10) * 1000);
    end;
  end;
end;

procedure TForm1.GLCadencer1Progress(Sender: TObject;
  const deltaTime, newTime: Double);
var
  speed: single;
begin

  GLDummyCube2.Roll(1);
  if clickdrapasat then
  begin
    if iskeydown(VK_Rbutton) then
    begin
      glsv.Cursor := crnone;
      GetCursorPos(NewMousePos);
      DCCamera.Roll(-(defaultmousepos.X - NewMousePos.X) * 0.25);
      GLCamera.Pitch((defaultmousepos.Y - NewMousePos.Y) * 0.25);
      ResetMousePos;

    end
    else
    begin
      glsv.Cursor := crdefault;
      clickdrapasat := false;
    end;
  end;
  if active = false then
    exit;
  if iskeydown(VK_Shift) then
    speed := 20 * deltaTime
  else
    speed := 5 * deltaTime;
  if iskeydown(vk_up) then
  begin
    DCCamera.Position.AddScaledVector(speed, GLCamera.AbsoluteVectorToTarget);
  end;
  if iskeydown(vk_down) then
  begin
    DCCamera.Position.AddScaledVector(-speed, GLCamera.AbsoluteVectorToTarget);
  end;
  if iskeydown(VK_LEFT) then
    DCCamera.Position.AddScaledVector(speed,
      GLCamera.AbsoluteRightVectorToTarget);
  if iskeydown(VK_RIGHT) then
    DCCamera.Position.AddScaledVector(-speed,
      GLCamera.AbsoluteRightVectorToTarget);
  if iskeydown(VK_PRIOR) then
  begin
    DCCamera.Position.Z := DCCamera.Position.Z + speed;
  end;
  if iskeydown(VK_NEXT) then
  begin
    DCCamera.Position.Z := DCCamera.Position.Z - speed;
  end;
end;

procedure TForm1.glsvMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbright then
  begin
    GetCursorPos(defaultmousepos);
    ResetMousePos;
    clickdrapasat := true;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  case rg1.ItemIndex of
    0:
      Terrain.lodType := tlodNone;
    1:
      Terrain.lodType := tlodSoar;
    2:
      Terrain.lodType := tlodIllyrium;
    3:
      Terrain.lodType := tlodIllyriumVBO;
  end;
  TerrainData.MarkDirty(-200, -200, 200, 200);
  glsv.SetFocus;
end;

procedure TForm1.tb1Change(Sender: TObject);
begin
  Label1.caption := 'LOD Precision : ' + inttostr(tb1.Position);
  Terrain.CLODPrecision := tb1.Position;
  glsv.SetFocus;
end;

end.
