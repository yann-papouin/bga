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
 * The Original Code is GuiSMView (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GuiSMView;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DDSImage, GLWin32Viewer, GLObjects, GLScene, GLGraph, GLCoordinates, GLCrossPlatform,
  BaseClasses, GLRenderContextInfo, GLGizmoEx,
  GLSimpleNavigation, GLVectorFileObjects, ImgList, PngImageList, VirtualTrees, SpTBXDkPanels,
  StdCtrls, ExtCtrls, SpTBXItem, SpTBXControls, GuiFormCommon, BGALib, GLMaterial, GLBitmapFont,
  GLWindowsFont, RSLib, GLSkydome, GLAtmosphere, ActnList, TB2Dock, TB2Toolbar, TB2Item, GLGizmo;

type



  TSMViewForm = class(TFormCommon)
    Scene: TGLScene;
    DummyCube: TGLDummyCube;
    CameraTarget: TGLDummyCube;
    Camera: TGLCamera;
    LightFront: TGLLightSource;
    CamLight: TGLLightSource;
    FreeMesh: TGLFreeForm;
    Grid: TGLXYZGrid;
    MeshList: TVirtualStringTree;
    Splitter: TSpTBXSplitter;
    GLMaterialLibrary: TGLMaterialLibrary;
    WindowsBitmapFont: TGLWindowsBitmapFont;
    LightBack: TGLLightSource;
    GLDirect: TGLDirectOpenGL;
    Actions: TActionList;
    DrawEdges: TAction;
    Images: TPngImageList;
    ToolDock: TSpTBXDock;
    ToolbarViewMode: TSpTBXToolbar;
    DrawVertices: TAction;
    SpTBXItem1: TSpTBXItem;
    DrawWireframe: TAction;
    DrawEdgeVertices: TAction;
    DrawMesh: TAction;
    DrawTextures: TAction;
    SpTBXItem2: TSpTBXItem;
    SpTBXItem3: TSpTBXItem;
    SpTBXItem4: TSpTBXItem;
    SpTBXItem5: TSpTBXItem;
    SpTBXItem6: TSpTBXItem;
    Panel: TPanel;
    SpTBXSeparatorItem1: TSpTBXSeparatorItem;
    SkyDome: TGLSkyDome;
    Viewer: TGLSceneViewer;
    RootTemp: TGLDummyCube;
    ToolbarControlMode: TSpTBXToolbar;
    SpTBXItem7: TSpTBXItem;
    SpTBXItem8: TSpTBXItem;
    SpTBXItem9: TSpTBXItem;
    ControlSelect: TAction;
    ControlMove: TAction;
    ControlScale: TAction;
    ControlRotate: TAction;
    ModeCamera: TAction;
    SpTBXItem11: TSpTBXItem;
    SpTBXSeparatorItem2: TSpTBXSeparatorItem;
    procedure FormCreate(Sender: TObject);
    procedure MeshListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure MeshListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure MeshListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure MeshListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure MeshListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure FormDestroy(Sender: TObject);
    procedure GLDirectRender(Sender: TObject; var rci: TRenderContextInfo);
    procedure DrawTexturesExecute(Sender: TObject);
    procedure DrawWireframeExecute(Sender: TObject);
    procedure DrawMeshExecute(Sender: TObject);
    procedure DrawEdgesExecute(Sender: TObject);
    procedure DrawVerticesExecute(Sender: TObject);
    procedure DrawEdgeVerticesExecute(Sender: TObject);
    procedure ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ControlSelectExecute(Sender: TObject);
    procedure ControlMoveExecute(Sender: TObject);
    procedure ControlRotateExecute(Sender: TObject);
    procedure ControlScaleExecute(Sender: TObject);
    procedure ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ModeCameraExecute(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    { Déclarations privées }
    FPreviousMouse : TPoint;
    FWorldRootNode : PVirtualNode;
    FColRootNode : PVirtualNode;
    FMeshRootNode : PVirtualNode;
    FFilename : string;
    FParser : TRsParser;
    procedure LoadSMData;
  protected
    function FindNodeID(ID : integer) : PVirtualNode;
    function BranchSelected(Sender: TBaseVirtualTree; Node : PVirtualNode) : boolean;
  public
    { Déclarations publiques }
    FDrawBackEdges : boolean;
    FDrawNormals : boolean;
    FDrawEdges : boolean;
    FDrawVertices : boolean;
    FDrawTextures : boolean;
    GetFileByPath: TBgaGetFileByPath;
    Gizmo : TGLGizmoEx;
    procedure LoadMaterials(Filename: string);    // Load textures from .RS file
    procedure LoadStandardMesh(Filename: string); // Load mesh from .SM file
    procedure Preview;
  end;



var
  SMViewForm: TSMViewForm;

implementation

{$R *.dfm}

uses
  DbugIntf, StringFunction, VirtualTreeviewTheme, GLContext, OpenGLAdapter, OpenGLTokens, GLState,
  GLFileSM, AppLib, Resources, Math, VectorTypes, VectorGeometry, GLColor;

type
  pData = ^rData;
  rData = record
    MeshID : integer;
    Text : string[255];
    MeshType : TSMMeshType;
    Mesh : TGLSMMeshObject;
    Img : integer;
  end;


{ TSMViewForm }

procedure TSMViewForm.FormCreate(Sender: TObject);
begin
  inherited;
  EnableSkinning(MeshList);
  FParser := TRsParser.Create;

  (*
  FDrawWireFrame := true;
  FDrawEdges := true;
  FDrawBackEdges := false;
  FDrawVertices := true;
  FDrawNormals := false;
  *)
  DrawMesh.Checked := true;
  DrawTextures.Checked := true;
  Gizmo := TGLGizmoEx.Create(Self);

  Gizmo.LabelFont := WindowsBitmapFont;
  Gizmo.SelectedObj := Grid;
  Gizmo.Viewer := Viewer;
  Gizmo.Enabled := False;

  Gizmo.RootGizmo := DummyCube;
  Gizmo.RootObjects:= Scene.Objects;
  Gizmo.AutoZoomFactor := 10;
  //Gizmo.GizmoTmpRoot := RootTemp;

  Gizmo.ExcludeObjectsList.Add('Grid');
  Gizmo.Visible := False;
end;

procedure TSMViewForm.FormDestroy(Sender: TObject);
begin
  Gizmo.Free;
  FParser.Free;
  inherited;
end;



procedure TSMViewForm.GLDirectRender(Sender: TObject; var rci: TRenderContextInfo);
var
  i, j, k, id :integer;
  vA, vB, vC, vP, vL, vM, vN : TAffineVector;
  FaceGroup : TFGVertexNormalTexIndexList;
  tes : TColorVector;
begin

  with rci.GLStates do
  begin
    Disable(stLighting);
    Enable(stBlend);
    SetBlendFunc(bfSrcAlpha, bfOneMinusSrcAlpha);

    if FDrawEdges then
    begin
      Enable(stLineSmooth);
      LineWidth := 1;
      Enable(stPolygonOffsetFill);
      SetPolygonOffset(1, 2);

      if FDrawBackEdges then
        SetDepthRange(0.0, 0.9);
    end;

    if FDrawVertices then
    begin
      Enable(stPointSmooth);
      PointSize := 4;
    end;
  end;


  for i := 0 to FreeMesh.MeshObjects.Count - 1 do
  begin
    if FreeMesh.MeshObjects[i].Visible then
    begin
      for j := 0 to FreeMesh.MeshObjects[i].FaceGroups.Count - 1 do
      begin
        FaceGroup := FreeMesh.MeshObjects[i].FaceGroups[j] as TFGVertexNormalTexIndexList;

        if FDrawEdges or FDrawVertices then
        begin
          for k := 0 to FaceGroup.VertexIndices.Count - 1 do
          begin
            if k mod 3 = 0 then
            begin
              vA := FreeMesh.MeshObjects[i].Vertices[FaceGroup.VertexIndices[k+0]];
              vB := FreeMesh.MeshObjects[i].Vertices[FaceGroup.VertexIndices[k+1]];
              vC := FreeMesh.MeshObjects[i].Vertices[FaceGroup.VertexIndices[k+2]];

              if FDrawEdges then
              begin
                GL.Color4fv(@clrWhite);
                GL.Begin_(GL_LINE_STRIP);
                GL.Vertex3f(vA[0], vA[1], vA[2]);
                GL.Vertex3f(vB[0], vB[1], vB[2]);
                GL.Vertex3f(vC[0], vC[1], vC[2]);
                GL.End_;
              end;

              if FDrawVertices then
              begin
                GL.Color4fv(@clrBlue);
                GL.Begin_(GL_POINTS);
                GL.Vertex3f(vA[0], vA[1], vA[2]);
                GL.Vertex3f(vB[0], vB[1], vB[2]);
                GL.Vertex3f(vC[0], vC[1], vC[2]);
                GL.End_;
              end;

            end;
          end;
        end;

        if FDrawNormals then
        begin
          GL.Color3f(1, 1, 1);
          for k := 0 to FaceGroup.VertexIndices.Count - 1 do
          begin
            if k mod 3 = 0 then
            begin
              vA := FreeMesh.MeshObjects[i].Vertices[FaceGroup.VertexIndices[k+0]];
              vB := FreeMesh.MeshObjects[i].Vertices[FaceGroup.VertexIndices[k+1]];
              vC := FreeMesh.MeshObjects[i].Vertices[FaceGroup.VertexIndices[k+2]];

              vL := FreeMesh.MeshObjects[i].Normals[FaceGroup.NormalIndices[k+0]];
              vM := FreeMesh.MeshObjects[i].Normals[FaceGroup.NormalIndices[k+1]];
              vN := FreeMesh.MeshObjects[i].Normals[FaceGroup.NormalIndices[k+2]];

              vP[0] := (vA[0] + vB[0] + vC[0]) /3;
              vP[1] := (vA[1] + vB[1] + vC[1]) /3;
              vP[2] := (vA[2] + vB[2] + vC[2]) /3;

              GL.Begin_(GL_POINTS);
              GL.Vertex3f(vP[0], vP[1], vP[2]);
              GL.End_;

            end;
          end;
        end;


      end;
    end;
  end;

end;



procedure TSMViewForm.DrawTexturesExecute(Sender: TObject);
var
  i :integer;
begin
  FDrawTextures := DrawTextures.Checked;
  for i := 0 to GLMaterialLibrary.Materials.Count - 1 do
  begin
    GLMaterialLibrary.Materials[i].Material.Texture.Enabled := FDrawTextures;
  end;
  Scene.NotifyChange(Self);
end;

procedure TSMViewForm.DrawWireframeExecute(Sender: TObject);
begin
  FreeMesh.Visible := not (Sender as TAction).Checked;
  FDrawEdges := (Sender as TAction).Checked;
  //FDrawEdges := false;
  FDrawVertices := false;
  Scene.NotifyChange(Self);
end;

procedure TSMViewForm.DrawMeshExecute(Sender: TObject);
begin
  FreeMesh.Visible := true;
  FDrawEdges := false;
  FDrawVertices := false;
  Scene.NotifyChange(Self);
end;

procedure TSMViewForm.ControlMoveExecute(Sender: TObject);
begin
  Gizmo.Enabled := True;
  Gizmo.OperationMode := gomMove;
end;

procedure TSMViewForm.ControlRotateExecute(Sender: TObject);
begin
  Gizmo.Enabled := True;
  Gizmo.OperationMode := gomRotate;
end;

procedure TSMViewForm.ControlScaleExecute(Sender: TObject);
begin
  Gizmo.Enabled := True;
  Gizmo.OperationMode := gomScale;
end;

procedure TSMViewForm.ControlSelectExecute(Sender: TObject);
begin
  Gizmo.Enabled := True;
  Gizmo.OperationMode := gomSelect;
end;

procedure TSMViewForm.DrawEdgesExecute(Sender: TObject);
begin
  FreeMesh.Visible := true;
  FDrawEdges := (Sender as TAction).Checked;
  FDrawVertices := false;
  Scene.NotifyChange(Self);
end;

procedure TSMViewForm.DrawEdgeVerticesExecute(Sender: TObject);
begin
  FreeMesh.Visible := true;
  FDrawEdges := (Sender as TAction).Checked;
  FDrawVertices := (Sender as TAction).Checked;
  Scene.NotifyChange(Self);
end;

procedure TSMViewForm.DrawVerticesExecute(Sender: TObject);
begin
  FreeMesh.Visible := true;
  FDrawEdges := false;
  FDrawVertices := (Sender as TAction).Checked;
  Scene.NotifyChange(Self);
end;


function TSMViewForm.FindNodeID(ID: integer): PVirtualNode;
var
  ColCounter, MeshCounter : integer;
  Node : PVirtualNode;
  Data : pData;
  i :integer;
begin
  Result := nil;
  Node := MeshList.GetFirst;
  while Node <> nil do
  begin
    Data := MeshList.GetNodeData(Node);
    if (Data.MeshType = mtMesh) and (Data.MeshID = ID) then
    begin
      Result := Node;
      break;
    end;
    Node := MeshList.GetNext(Node);
  end;
end;


procedure TSMViewForm.LoadSMData;
var
  ColCounter, MeshCounter, MatCounter : integer;
  Node, NodeID : PVirtualNode;
  Data : pData;
  i :integer;
  CamDistance : single;
begin
  MeshList.Clear;

  ColCounter := 0;
  MeshCounter := 0;

  FWorldRootNode := MeshList.AddChild(nil);
  Data := MeshList.GetNodeData(FWorldRootNode);
  Data.MeshType := mtNone;
  Data.Text := 'World';
  Data.Img := 1162;

  FColRootNode := MeshList.AddChild(FWorldRootNode);
  Data := MeshList.GetNodeData(FColRootNode);
  Data.MeshType := mtNone;
  Data.Text := 'Collisions';
  Data.Img := 971;

  FMeshRootNode := MeshList.AddChild(FWorldRootNode);
  Data := MeshList.GetNodeData(FMeshRootNode);
  Data.MeshType := mtNone;
  Data.Text := 'Meshes';
  Data.Img := 571;


  for i := 0 to FreeMesh.MeshObjects.Count - 1 do
  begin
    if FreeMesh.MeshObjects[i] is TGLSMColMeshObject then
    begin
      Inc(ColCounter);
      Node := MeshList.AddChild(FColRootNode);
      Data := MeshList.GetNodeData(Node);
      Data.MeshType := mtCollision;
      Data.Text := Format('Collision %d',[ColCounter]);

      Data.Mesh := (FreeMesh.MeshObjects[i] as TGLSMMeshObject);
      Data.Mesh.Visible := false;
      Data.Img := 779;

    end;

    if FreeMesh.MeshObjects[i] is TGLSMMatMeshObject then
    begin
      NodeID := FindNodeID((FreeMesh.MeshObjects[i] as TGLSMMatMeshObject).ParentMeshID);
      if NodeID = nil then
      begin
        MatCounter := 0;
        Inc(MeshCounter);
        NodeID := MeshList.AddChild(FMeshRootNode);
        Data := MeshList.GetNodeData(NodeID);
        Data.MeshType := mtMesh;
        Data.Mesh := nil;
        Data.MeshID := (FreeMesh.MeshObjects[i] as TGLSMMatMeshObject).ParentMeshID;
        Data.Text := Format('Mesh %d',[MeshCounter]);
        Data.Img := 290;
      end;

      Inc(MatCounter);
      Node := MeshList.AddChild(NodeID);
      Data := MeshList.GetNodeData(Node);
      Data.MeshType := mtMat;
      Data.Mesh := (FreeMesh.MeshObjects[i] as TGLSMMeshObject);
      Data.Mesh.Visible := false;
      Data.Text := Format('Mat %d',[MatCounter]);
      Data.Img := 283;
    end;
  end;

  if MeshCounter = 0 then
    MeshList.DeleteNode(FMeshRootNode)
  else
  begin
    MeshList.FullyVisible[FMeshRootNode.FirstChild] := true;
    MeshList.Selected[FMeshRootNode.FirstChild] := true;
    MeshList.FocusedNode := FMeshRootNode.FirstChild;
  end;

  if ColCounter = 0 then
    MeshList.DeleteNode(FColRootNode)
  else
  begin
    MeshList.FullyVisible[FColRootNode.LastChild] := true;

    if MeshList.SelectedCount = 0 then
    begin
      MeshList.Selected[FColRootNode.LastChild] := true;
      MeshList.FocusedNode := FColRootNode.LastChild;
    end;
  end;

  CamDistance := 0;
  CamDistance := Max(CamDistance, FreeMesh.BarycenterAbsolutePosition[0]);
  CamDistance := Max(CamDistance, FreeMesh.BarycenterAbsolutePosition[1]);
  CamDistance := Max(CamDistance, FreeMesh.BarycenterAbsolutePosition[2]);
  CamDistance := Max(CamDistance, FreeMesh.BarycenterAbsolutePosition[3]);

  CamDistance := Max(CamDistance*2, FreeMesh.BoundingSphereRadius);

  Camera.Position.X := CamDistance;
  Camera.Position.Y := CamDistance;
  Camera.Position.Z := CamDistance;

  LightFront.Position.X := Camera.Position.X;
  LightFront.Position.Y := Camera.Position.Y;
  LightFront.Position.Z := Camera.Position.Z;

  LightBack.Position.X := -Camera.Position.X;
  LightBack.Position.Y := -Camera.Position.Y;
  LightBack.Position.Z := -Camera.Position.Z;
end;

procedure TSMViewForm.LoadStandardMesh(Filename: string);
begin
  FreeMesh.MeshObjects.Clear;

  FFilename := Filename;
  FreeMesh.LoadFromFile(Filename);
  Title := Filename;

  LoadSMData;
end;

procedure TSMViewForm.LoadMaterials(Filename: string);
var
  Data : TStringList;
  i : Integer;
  Path: string;
  LibMaterial : TGLLibMaterial;
begin
  FreeMesh.MaterialLibrary.Materials.Clear;
  SendDebug(Filename);
  if FileExists(Filename) then
  try
    Data := TStringList.Create;
    Data.LoadFromFile(Filename);
    FParser.Parse(Data);

    if FParser.Parsed then
    begin
      for i := 0 to FParser.Count - 1 do
      begin
        Path := GetFileByPath(Self, FParser[i].Texture);
        LibMaterial := FreeMesh.MaterialLibrary.AddTextureMaterial(FParser[i].Name, Path);

        if FParser[i].Transparent then
          LibMaterial.Material.BlendingMode := bmTransparency;

        LibMaterial.Material.FrontProperties.Diffuse.Color := FParser[i].MaterialDiffuse;
        LibMaterial.Material.FrontProperties.Specular.Color := FParser[i].MaterialSpecular;
        LibMaterial.Material.FrontProperties.Shininess := EnsureRange(Round(FParser[i].MaterialSpecularPower), 0, 128);
        if not FParser[i].Lighting then
          LibMaterial.Material.MaterialOptions := LibMaterial.Material.MaterialOptions + [moNoLighting];

        LibMaterial.Material.DepthProperties.DepthWrite := FParser[i].DepthWrite;
        //LibMaterial.Material.FrontProperties.Specular.Red
      end;
    end;
  finally
    Data.Free;
  end;
  DrawTexturesExecute(Self);
end;



function TSMViewForm.BranchSelected(Sender: TBaseVirtualTree; Node: PVirtualNode): boolean;
begin
  Result := false;
  repeat
    begin
      if Sender.Selected[Node] or (Sender.FocusedNode = Node) then
      begin
        Result := true;
        Break;
      end;
      Node := Sender.NodeParent[Node];
    end
  until Node = nil;
end;

procedure TSMViewForm.MeshListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data : pData;
begin
  Node := Sender.GetFirst;
  while Node <> nil do
  begin
    Data := Sender.GetNodeData(Node);
    if (Data.MeshType = mtCollision) or (Data.MeshType = mtMat) then
    begin
      Data.Mesh.Visible := BranchSelected(Sender, Node);
    end;
    Node := Sender.GetNext(Node, true);
  end;

  FreeMesh.StructureChanged;
end;

procedure TSMViewForm.MeshListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data : pData;
begin
  Data := Sender.GetNodeData(Node);
  Finalize(Data^);
end;

procedure TSMViewForm.MeshListGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data : pData;
begin
  Data := Sender.GetNodeData(Node);
  ImageIndex := Data.Img;
end;

procedure TSMViewForm.MeshListGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(rData);
end;

procedure TSMViewForm.MeshListGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  Data : pData;
begin
  Data := Sender.GetNodeData(Node);
  CellText := Data.Text;
end;



procedure TSMViewForm.ModeCameraExecute(Sender: TObject);
begin
  Gizmo.Enabled := false;
end;

procedure TSMViewForm.Preview;
begin
  Show;
end;

procedure TSMViewForm.ViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SetFocus;
  if Gizmo.Enabled or (Gizmo.OperationMode = gomSelect) then
    Gizmo.ViewerMouseDown(X, Y);
end;

procedure TSMViewForm.ViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
	Delta : TPoint;
begin
  if Gizmo.Enabled then
    Gizmo.ViewerMouseMove(X,Y)
  else
  begin
    if ssLeft in Shift then
    begin
      Delta.X := FPreviousMouse.X - X;
      Delta.Y := FPreviousMouse.Y - Y;
      Camera.MoveAroundTarget(Delta.Y, Delta.X);
    end;
  end;
  FPreviousMouse.X := X;
  FPreviousMouse.Y := Y;
end;

procedure TSMViewForm.ViewerMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Gizmo.Enabled then
    Gizmo.ViewerMouseUp(X, Y);
end;

procedure TSMViewForm.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if Gizmo.Enabled then
  begin

  end
    else
  begin
    // Note that 1 wheel-step induces a WheelDelta of 120,
    // this code adjusts the distance to target with a 10% per wheel-step ratio
    Camera.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
  end;
end;

end.

(*

procedure TForm1.GLSceneViewer1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	// store mouse coordinates when a button went down
	mdx:=x; mdy:=y;
end;

procedure TForm1.GLSceneViewer1MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
	dx, dy : Integer;
	v : TVector;
begin
	// calculate delta since last move or last mousedown
	dx:=mdx-x; dy:=mdy-y;
	mdx:=x; mdy:=y;
	if ssLeft in Shift then begin
      if ssShift in Shift then begin
         // right button with shift rotates the teapot
         // (rotation happens around camera's axis)
	   	GLCamera1.RotateObject(Teapot1, dy, dx);
      end else begin
   		// right button without shift changes camera angle
	   	// (we're moving around the parent and target dummycube)
		   GLCamera1.MoveAroundTarget(dy, dx)
      end;
	end else if Shift=[ssRight] then begin
		// left button moves our target and parent dummycube
		v:=GLCamera1.ScreenDeltaToVectorXY(dx, -dy,
							0.12*GLCamera1.DistanceToTarget/GLCamera1.FocalLength);
		DummyCube1.Position.Translate(v);
		// notify camera that its position/target has been changed
		GLCamera1.TransformationChanged;
	end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
	// Note that 1 wheel-step induces a WheelDelta of 120,
	// this code adjusts the distance to target with a 10% per wheel-step ratio
	GLCamera1.AdjustDistanceToTarget(Power(1.1, WheelDelta/120));
end;
*)
