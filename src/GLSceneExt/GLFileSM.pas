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
 * The Original Code is GLFileSM (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit GLFileSM;

interface


uses
  Classes, SysUtils, GLVectorFileObjects, ApplicationFileIO, FileSM, TypesSM;

type
   // TGLSMVectorFile
   //
   {: The SM vector file (Refractor2 standard mesh).<p>
      }
  TGLSMVectorFile = class(TVectorFile)
  public
     { Public Declarations }
     class function Capabilities : TDataFileCapabilities; override;
     procedure LoadFromStream(aStream : TStream); override;
  end;

  TSMMeshType =
  (
    mtNone,
    mtCollision,
    mtMesh,
    mtMat
  );

  TGLSMMeshObject = class (TMeshObject)
  private
    function GetMeshType: TSMMeshType;
  public
    property MeshType : TSMMeshType read GetMeshType;
  end;


  TGLSMColMeshObject = class(TGLSMMeshObject)
  private

  public

  end;

  TGLSMMatMeshObject = class(TGLSMMeshObject)
  private
    FTexturePath: string;
    FParentMeshID: integer;
  public
    property TexturePath : string read FTexturePath;
    property ParentMeshID : integer read FParentMeshID;
  end;

implementation

uses
  Dbugintf, VectorTypes;


// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
{ TGLSMVectorFile }

class function TGLSMVectorFile.Capabilities: TDataFileCapabilities;
begin
   Result:=[dfcRead];
end;

procedure TGLSMVectorFile.LoadFromStream(aStream: TStream);
var
   i, j, k : Integer;
   SMFile : TFileSM;
   ColMesh : TGLSMColMeshObject;
   MatMesh : TGLSMMatMeshObject;
   Mat : TMatrix3f;
begin
  inherited;
  SMFile:=TFileSM.Create;
  SMFile.LoadFromStream(aStream);

  try

    if SMFile.CollMeshCount > 0 then
    begin
      // retrieve ColMesh data
      for i := 0 to SMFile.CollMeshCount - 1 do
      begin
        ColMesh := TGLSMColMeshObject.CreateOwned(Owner.MeshObjects);

        for j:=0 to SMFile.CollMeshes[i].FaceCount-1 do
        begin
          Mat := SMFile.CollVertexFromFaceId(i, j);

          ColMesh.Vertices.Add(Mat[0]);
          ColMesh.Vertices.Add(Mat[1]);
          ColMesh.Vertices.Add(Mat[2]);
        end;

        //SendDebugFmt('Current Mesh.Vertices.Capacity is %d',[ColMesh.Vertices.Capacity]);
        //SendDebugFmt('Current Mesh.TriangleCount is %d',[ColMesh.TriangleCount]);
      end;
    end;

    if SMFile.MeshCount > 0 then
    begin
      // retrieve LodMesh data
      for i := 0 to SMFile.MeshCount - 1 do
      begin
        for j:=0 to SMFile.Meshes[i].MatMeshCount-1 do
        begin
          MatMesh := TGLSMMatMeshObject.CreateOwned(Owner.MeshObjects);
          MatMesh.FTexturePath := SMFile.Meshes[i].MatMeshes[j].Material.Name;
          MatMesh.FParentMeshID := i;

          for k:=0 to SMFile.Meshes[i].MatMeshes[j].MeshData.FaceCount-1 do
          begin
            Mat := SMFile.MeshVertexFromLodFaceId(i, j, k);

            MatMesh.Vertices.Add(Mat[0]);
            MatMesh.Vertices.Add(Mat[1]);
            MatMesh.Vertices.Add(Mat[2]);
          end;

          //SendDebugFmt('Current Mesh.Vertices.Capacity is %d',[LodMesh.Vertices.Capacity]);
          //SendDebugFmt('Current Mesh.TriangleCount is %d',[LodMesh.TriangleCount]);
        end;
      end;
    end;

  finally
    SMFile.Free;
  end;
end;



{ TGLSMMeshObject }

function TGLSMMeshObject.GetMeshType: TSMMeshType;
begin
  if Self is TGLSMColMeshObject then
    result := mtCollision
  else
  if Self is TGLSMMatMeshObject then
    result := mtMat
  else
    result := mtNone
end;

initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

  RegisterVectorFileFormat('sm', 'Battlefield 1942 standardmesh files', TGLSMVectorFile);

end.
