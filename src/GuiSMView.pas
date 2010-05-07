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
  Dialogs, GLWin32Viewer, GLObjects, GLScene, GLGraph, GLCoordinates, GLCrossPlatform, BaseClasses, GLSimpleNavigation, GLVectorFileObjects, ImgList, PngImageList, VirtualTrees, SpTBXDkPanels;

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
    ExplorerImg: TPngImageList;
    MeshList: TVirtualStringTree;
    Splitter: TSpTBXSplitter;
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
  private
    { Déclarations privées }
    FColRootNode : PVirtualNode;
    FLodRootNode : PVirtualNode;
    FApplicationTitle : string;
    FFilename : string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    procedure LoadSMData;
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
  GLFileSM, AppLib;

type
  pData = ^rData;
  rData = record
    //
    Text : string[255];
    Mesh : TGLSMMeshObject;
    Img : integer;
  end;


{ TSMViewForm }


procedure TSMViewForm.FormCreate(Sender: TObject);
begin
  FApplicationTitle := Caption + ' - ' + ApplicationSvnTitle;
end;

procedure TSMViewForm.LoadSMData;
var
  ColCounter, LodCounter : integer;
  Node : PVirtualNode;
  Data : pData;
  i :integer;
begin
  MeshList.Clear;

  ColCounter := 0;
  LodCounter := 0;

  FColRootNode := MeshList.AddChild(nil);
  Data := MeshList.GetNodeData(FColRootNode);
  Data.Text := 'Collisions';
  Data.Img := 7;

  FLodRootNode := MeshList.AddChild(nil);
  Data := MeshList.GetNodeData(FLodRootNode);
  Data.Text := 'Lods';
  Data.Img := 5;

  for i := 0 to FreeMesh.MeshObjects.Count - 1 do
  begin
    if FreeMesh.MeshObjects[i] is TGLSMColMeshObject then
    begin
      Inc(ColCounter);
      Node := MeshList.AddChild(FColRootNode);
      Data := MeshList.GetNodeData(Node);
      Data.Text := Format('Collision %d',[ColCounter]);

      Data.Mesh := (FreeMesh.MeshObjects[i] as TGLSMMeshObject);
      Data.Mesh.Visible := false;
      Data.Img := 112;

    end;

    if FreeMesh.MeshObjects[i] is TGLSMLodMeshObject then
    begin
      Inc(LodCounter);
      Node := MeshList.AddChild(FLodRootNode);
      Data := MeshList.GetNodeData(Node);
      Data.Text := Format('Lod %d',[LodCounter]);

      Data.Mesh := (FreeMesh.MeshObjects[i] as TGLSMMeshObject);
      Data.Mesh.Visible := false;
      Data.Img := 111;
    end;
  end;

  if ColCounter = 0 then
    MeshList.DeleteNode(FColRootNode)
  else
  begin
    MeshList.Expanded[FColRootNode] := true;
    MeshList.FocusedNode := FColRootNode.LastChild;
  end;

  if LodCounter = 0 then
    MeshList.DeleteNode(FLodRootNode)
  else
  begin
    MeshList.Expanded[FLodRootNode] := true;
    MeshList.FocusedNode := FLodRootNode.FirstChild;
  end;
end;

procedure TSMViewForm.LoadStandardMesh(Filename: string);
begin
  FFilename := Filename;
  FreeMesh.LoadFromFile(Filename);
  Title := Filename;

  LoadSMData;
end;

procedure TSMViewForm.MeshListFocusChanging(Sender: TBaseVirtualTree; OldNode,
  NewNode: PVirtualNode; OldColumn, NewColumn: TColumnIndex;
  var Allowed: Boolean);
var
  OldData, NewData : pData;
  FNeedRecalc : boolean;
begin
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
