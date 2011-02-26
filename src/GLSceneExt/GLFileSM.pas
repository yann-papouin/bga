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

{$DEFINE DEBUG_GLSM}

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
    FParentMeshID: integer;
  public
    property ParentMeshID : integer read FParentMeshID;
  end;

implementation

uses
  {$IfDef DEBUG_GLSM}
  Dbugintf,
  {$EndIf}
  VectorTypes,
  VectorGeometry,
  GLColor;


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
   id : array[0..2] of Integer;
   SMFile : TFileSM;
   ColMesh : TGLSMColMeshObject;
   MatMesh : TGLSMMatMeshObject;
   TexPoint3 : TTexPoint3;
   MatVert : TMatrix3f;
   MatNorm : TMatrix3f;
   FaceGroup : TFGVertexNormalTexIndexList;
begin

  SMFile:=TFileSM.Create;
  SMFile.LoadFromStream(aStream);

  try

    if SMFile.CollMeshCount > 0 then
    begin
      // retrieve ColMesh data
      for i := 0 to SMFile.CollMeshCount - 1 do
      begin
        ColMesh := TGLSMColMeshObject.CreateOwned(Owner.MeshObjects);
        ColMesh.UseVBO := false;
        ColMesh.Mode := momFaceGroups;
        //ColMesh.Mode := momTriangles;

        FaceGroup:=TFGVertexNormalTexIndexList.CreateOwned(ColMesh.FaceGroups);
        FaceGroup.Mode:=fgmmTriangles;

        if SMFile.CollMeshes[i].FaceCount > 0 then
          for j:=0 to SMFile.CollMeshes[i].FaceCount-1 do
          begin
            MatVert := SMFile.CollVertexFromFaceId(i, j);
            id[0] := ColMesh.Vertices.Add(MatVert[0]);
            id[1] := ColMesh.Vertices.Add(MatVert[1]);
            id[2] := ColMesh.Vertices.Add(MatVert[2]);
            FaceGroup.VertexIndices.Add(id[0], id[1], id[2]);
          end;

        ColMesh.BuildNormals(FaceGroup.VertexIndices, momTriangles);

        {$IfDef DEBUG_GLSM}
          SendDebugFmt('Current Mesh.Vertices.Capacity is %d',[ColMesh.Vertices.Capacity]);
          SendDebugFmt('Current Mesh.TriangleCount is %d',[ColMesh.TriangleCount]);
        {$EndIf}
      end;
    end;

    if SMFile.MeshCount > 0 then
    begin
      // retrieve LodMesh data
      for i := 0 to SMFile.MeshCount - 1 do
      begin
        {$IfDef DEBUG_GLSM}
        SendInteger('MatMeshCount', SMFile.Meshes[i].MatMeshCount);
        {$EndIf}

        if SMFile.Meshes[i].MatMeshCount > 0 then
        for j:=0 to SMFile.Meshes[i].MatMeshCount-1 do
          begin
            MatMesh := TGLSMMatMeshObject.CreateOwned(Owner.MeshObjects);
            MatMesh.UseVBO := false;
            MatMesh.Mode := momFaceGroups;
            MatMesh.FParentMeshID := i;

            FaceGroup:=TFGVertexNormalTexIndexList.CreateOwned(MatMesh.FaceGroups);
            FaceGroup.MaterialName := SMFile.Meshes[i].MatMeshes[j].Material.Name;
            FaceGroup.Mode:=fgmmTriangles;

            if SMFile.Meshes[i].MatMeshes[j].MeshData.FaceCount > 0 then
            for k:=0 to SMFile.Meshes[i].MatMeshes[j].MeshData.FaceCount-1 do
            begin
              MatVert := SMFile.MeshVertexFromMatFaceId(i, j, k);
              id[0] := MatMesh.Vertices.Add(MatVert[0]);
              id[1] := MatMesh.Vertices.Add(MatVert[1]);
              id[2] := MatMesh.Vertices.Add(MatVert[2]);
              FaceGroup.VertexIndices.Add(id[0], id[1], id[2]);

              MatNorm := SMFile.MeshNormaleFromMatFaceId(i, j, k);
              id[0] := MatMesh.Normals.Add(MatNorm[0]);
              id[1] := MatMesh.Normals.Add(MatNorm[1]);
              id[2] := MatMesh.Normals.Add(MatNorm[2]);
              FaceGroup.NormalIndices.Add(id[0], id[1], id[2]);

              TexPoint3 := SMFile.MeshTextureFromMatFaceId(i, j, k);
              id[0] := MatMesh.TexCoords.Add(TexPoint3[0]);
              id[1] := MatMesh.TexCoords.Add(TexPoint3[1]);
              id[2] := MatMesh.TexCoords.Add(TexPoint3[2]);
              FaceGroup.TexCoordIndices.Add(id[0], id[1], id[2]);
            end;

          {$IfDef DEBUG_GLSM}
            SendDebugFmt('VertexIndices =  %d',[FaceGroup.VertexIndices.Count]);
            SendDebugFmt('NormalIndices =  %d',[FaceGroup.NormalIndices.Count]);
            SendDebugFmt('TexCooIndices =  %d',[FaceGroup.TexCoordIndices.Count]);
          {$EndIf}

          {$IfDef DEBUG_GLSM}
            SendDebugFmt('Current Mesh.Vertices.Capacity is %d',[MatMesh.Vertices.Capacity]);
            SendDebugFmt('Current Mesh.TriangleCount is %d',[MatMesh.TriangleCount]);
          {$EndIf}

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
