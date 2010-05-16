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
  Dialogs, GLWin32Viewer, GLObjects, GLScene, GLGraph, GLCoordinates, GLCrossPlatform, BaseClasses, GLSimpleNavigation, GLVectorFileObjects, ImgList, PngImageList, VirtualTrees, SpTBXDkPanels,
  StdCtrls, ExtCtrls, SpTBXItem, SpTBXControls;

type
  TSMViewForm = class(TForm)
    Navigation: TGLSimpleNavigation;
    Scene: TGLScene;
    DummyCube: TGLDummyCube;
    CameraTarget: TGLDummyCube;
    Camera: TGLCamera;
    Light: TGLLightSource;
    CamLight: TGLLightSource;
    FreeMesh: TGLFreeForm;
    Grid: TGLXYZGrid;
    MeshList: TVirtualStringTree;
    Splitter: TSpTBXSplitter;
    Panel1: TPanel;
    Label1: TSpTBXLabel;
    Label2: TSpTBXLabel;
    procedure FormCreate(Sender: TObject);
    procedure MeshListGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure MeshListGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure MeshListGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure MeshListFocusChanging(Sender: TBaseVirtualTree; OldNode,
      NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
      var Allowed: Boolean);
    procedure MeshListFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure MeshListChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { Déclarations privées }
    FWorldRootNode : PVirtualNode;
    FColRootNode : PVirtualNode;
    FMeshRootNode : PVirtualNode;
    FApplicationTitle : string;
    FFilename : string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure LoadSMData;
  protected
    function FindNodeID(ID : integer) : PVirtualNode;
    function BranchSelected(Sender: TBaseVirtualTree; Node : PVirtualNode) : boolean;
  public
    { Déclarations publiques }
    procedure LoadStandardMesh(Filename: string);
    property Title : string read GetTitle write SetTitle;
  end;


var
  SMViewForm: TSMViewForm;

implementation

{$R *.dfm}

uses
  GLFileSM, AppLib, Resources;

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
  FApplicationTitle := Caption + ' - ' + ApplicationSvnTitle;
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

  if ColCounter = 0 then
    MeshList.DeleteNode(FColRootNode)
  else
  begin
    MeshList.Expanded[FColRootNode] := true;
    MeshList.FocusedNode := FColRootNode.LastChild;
  end;

  if MeshCounter = 0 then
    MeshList.DeleteNode(FMeshRootNode)
  else
  begin
    MeshList.Expanded[FMeshRootNode] := true;
    MeshList.Selected[FMeshRootNode.FirstChild] := true;
  end;

  Camera.Position.X := FreeMesh.BoundingSphereRadius;
  Camera.Position.Y := FreeMesh.BoundingSphereRadius;
  Camera.Position.Z := FreeMesh.BoundingSphereRadius;

end;

procedure TSMViewForm.LoadStandardMesh(Filename: string);
begin
  FFilename := Filename;
  FreeMesh.LoadFromFile(Filename);
  Title := Filename;

  LoadSMData;
end;


function TSMViewForm.BranchSelected(Sender: TBaseVirtualTree; Node: PVirtualNode): boolean;
begin
  Result := false;
  repeat
    begin
      if Sender.Selected[Node] then
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

procedure TSMViewForm.MeshListFocusChanging(Sender: TBaseVirtualTree; OldNode, NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex; var Allowed: Boolean);
var
  OldData, NewData : pData;
  FNeedRecalc : boolean;
begin
Exit;
  FNeedRecalc := false;

  if OldNode <> NewNode then
  begin
    OldData := Sender.GetNodeData(OldNode);
    NewData := Sender.GetNodeData(NewNode);

    if (NewData <> nil) and (NewData.Mesh <> nil) then
    begin
      if not NewData.Mesh.Visible then
      begin
        NewData.Mesh.Visible := true;
        FNeedRecalc := true;
      end;

      if (OldData <> nil) and (OldData.Mesh <> nil) then
        if OldData.Mesh.Visible then
        begin
          OldData.Mesh.Visible := false;
          FNeedRecalc := true;
        end;
    end;
  end;

  if FNeedRecalc then
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


function TSMViewForm.GetTitle: string;
begin
  Result := Caption
end;

procedure TSMViewForm.SetTitle(const Value: string);
begin
  if Value <> EmptyStr then
    Caption := Format('%s - %s',[ExtractFilename(Value), FApplicationTitle])
  else
    Caption := FApplicationTitle;
end;

end.
