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
 * The Original Code is FileTM (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit FileTM;

interface

{$DEFINE DEBUG_TM}
{.$DEFINE DEBUG_TM_DETAILS}

uses Classes, TypesTM, VectorTypes, VectorGeometry;

type

  // TFileSM
  //
  TFileSM = class
  private
    FBBoxMesh : TTMBBox;
    FBBoxLeaves : TTMBBox;
    FHeader : Longword;
    FAngleCount : Longword;
    FCollMesh : TTMColMeshData;
    FVisMesh : TTMVertexData; // used only as a storage way
    FBranchMeshCount : Longword;
    FBranchMeshes : array of TTMFaceData;
    FTrunkMeshCount : Longword;
    FTrunkMeshes : array of TTMFaceData;
    FSpriteMeshCount : Longword;
    FSpriteMeshes : array of TTMFaceData;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LoadFromStream(aStream : TStream);

    property Header : Longword read FHeader;
  end;


implementation

uses
  DbugIntf,
  SysUtils, Math;

const
  DWORD_SIZE = 4;
  WORD_SIZE = 2;
  BYTE_SIZE = 1;
  FLOAT_SIZE = 4;

{ TFileSM }

function StringFrom(AStream: TStream): AnsiString;
var
  Size: Cardinal;
begin
  AStream.Read(Size,4);   // Read string length (stored in a 32bits value)
  if Size > 255 then
  begin
    Size := 0;
    Result := EmptyStr;
    //raise Exception.Create('Max AnsiString size is 255');
  end
    else
  begin
    try
      SetLength(Result, Size);
      AStream.Read(Result[1], Size);
    finally

    end;
  end;
end;

constructor TFileSM.Create;
begin
  FHeader := 0;
end;

destructor TFileSM.Destroy;
begin

  inherited;
end;


procedure TFileSM.LoadFromStream(aStream: TStream);
var
  matnum : Longword;
  SizeOfSection : Longword;
  endOffset : int64;

  Unknown1, Unknown2, Index: Longword;
  FaceValues : array of word;
  i,j  : integer;

  TmHeader : TTMHeader;
  ptVertex : PTMVertex;
  ptFace : PTMFace;

  IndiceCount : Longword;
  Indices : array of Integer;

  procedure ReadBoundingBox(var Box: TTMBBox);
  begin
    aStream.Read(Box.P1[0] , FLOAT_SIZE);
    Box.P1[0] := Box.P1[0] * DTM_SCALE;
    aStream.Read(Box.P1[1] , FLOAT_SIZE);
    Box.P1[1] := Box.P1[1] * DTM_SCALE;
    aStream.Read(Box.P1[2] , FLOAT_SIZE);
    Box.P1[2] := Box.P1[2] * DTM_SCALE;

    aStream.Read(Box.P2[0] , FLOAT_SIZE);
    Box.P2[0] := Box.P2[0] * DTM_SCALE;
    aStream.Read(Box.P2[1] , FLOAT_SIZE);
    Box.P2[1] := Box.P2[1] * DTM_SCALE;
    aStream.Read(Box.P2[2] , FLOAT_SIZE);
    Box.P2[2] := Box.P2[2] * DTM_SCALE;
  end;

  procedure ReadTreemeshBlock(var Block : TTMBlock);
  begin
    aStream.Read(Block.IndexStart, DWORD_SIZE);
    aStream.Read(Block.PrimitiveCount, DWORD_SIZE);
    Block.TextureName := StringFrom(aStream);
  end;

begin
  Assert(Assigned(aStream));

  aStream.Position := 0;
  aStream.Read(FHeader , DWORD_SIZE);
  aStream.Position := aStream.Position+(4);

  if FHeader = 3 then
  begin
    /// Angle count -- for billboards
    aStream.Read(FAngleCount , DWORD_SIZE);

    /// Extracting bounding boxes
    ReadBoundingBox(FBBoxMesh);
    ReadBoundingBox(FBBoxLeaves);

    /// Header data of Visible geometry types
    begin
    aStream.Read(TmHeader.BranchCount , DWORD_SIZE);
    SetLength(TmHeader.Branches, TmHeader.BranchCount);
    for i := 0 to TmHeader.BranchCount - 1 do
      ReadTreemeshBlock(TmHeader.Branches[i]);

    aStream.Read(TmHeader.TrunkCount , DWORD_SIZE);
    SetLength(TmHeader.Trunks, TmHeader.TrunkCount);
    for i := 0 to TmHeader.TrunkCount - 1 do
      ReadTreemeshBlock(TmHeader.Trunks[i]);

    aStream.Read(TmHeader.SpriteCount , DWORD_SIZE);
    SetLength(TmHeader.Sprites, TmHeader.SpriteCount);
    for i := 0 to TmHeader.SpriteCount - 1 do
      ReadTreemeshBlock(TmHeader.Sprites[i]);

    aStream.Read(TmHeader.BillboardCount , DWORD_SIZE);
    SetLength(TmHeader.Billboards, TmHeader.BillboardCount);
    for i := 0 to TmHeader.BillboardCount - 1 do
      ReadTreemeshBlock(TmHeader.Billboards[i]);
    end;

    /// Extracting collision mesh data
    aStream.Read(Unknown1 , DWORD_SIZE);
    if Unknown1 = $EB97C2FA then
    begin
      aStream.Read(Unknown2, DWORD_SIZE);

      if Unknown2 = 5 then
      begin
        aStream.Read(FCollMesh.VertexCount , DWORD_SIZE);
        SetLength(FCollMesh.Vertex, FCollMesh.VertexCount);

        for i := 0 to FCollMesh.VertexCount-1 do
        begin
          ptVertex := @(FCollMesh.Vertex[i]);

          aStream.Read(ptVertex.Position[0] , FLOAT_SIZE);
          ptVertex.Position[0] := ptVertex.Position[0] * DTM_SCALE;
          aStream.Read(ptVertex.Position[1] , FLOAT_SIZE);
          ptVertex.Position[1] := ptVertex.Position[1] * DTM_SCALE;
          aStream.Read(ptVertex.Position[2] , FLOAT_SIZE);
          ptVertex.Position[2] := ptVertex.Position[2] * DTM_SCALE;

          aStream.Position := aStream.Position+ 4;

          {$IfDef DEBUG_TM_DETAILS}
          SendDebugFmt('Current vertex is %d/%d (X=%.3f, Y=%.3f, Z=%.3f)',[i+1, FCollMesh.VertexCount, ptVertex.Position[0], ptVertex.Position[1], ptVertex.Position[2]]);
          {$EndIf}
        end;

        aStream.Read(FCollMesh.FaceCount , DWORD_SIZE);
        SetLength(FCollMesh.Faces, FCollMesh.FaceCount);

        for i := 0 to FCollMesh.FaceCount-1 do
        begin
          ptFace := @FCollMesh.Faces[i];

          aStream.Read(ptFace.A, WORD_SIZE);
          aStream.Read(ptFace.B, WORD_SIZE);
          aStream.Read(ptFace.C, WORD_SIZE);

          aStream.Read(ptFace.MaterialID , WORD_SIZE);
          ptFace.MaterialID := ptFace.MaterialID +1;

          if ptFace.MaterialID = 0 then
            ptFace.MaterialID := 10000; // 10000 equals 0

          {$IfDef DEBUG_TM_DETAILS}
          SendDebugFmt('Current face is %d/%d (%d,%d,%d) MatID=[%d]',[i+1, FCollMeshes.FaceCount, ptFace.A, ptFace.B, ptFace.C, ptFace.MaterialID]);
          {$EndIf}
        end;

      end;
    end;

    /// Extracting visible geometry mesh data
    aStream.Read(FVisMesh.VertexCount , DWORD_SIZE);
    SetLength(FVisMesh.Vertex, FVisMesh.VertexCount);
    for i := 0 to FVisMesh.VertexCount-1 do
    begin
      ptVertex := @(FVisMesh.Vertex[i]);

      aStream.Read(ptVertex.Position[0] , FLOAT_SIZE);
      ptVertex.Position[0] := ptVertex.Position[0] * DTM_SCALE;
      aStream.Read(ptVertex.Position[1] , FLOAT_SIZE);
      ptVertex.Position[1] := ptVertex.Position[1] * DTM_SCALE;
      aStream.Read(ptVertex.Position[2] , FLOAT_SIZE);
      ptVertex.Position[2] := ptVertex.Position[2] * DTM_SCALE;

      aStream.Read(ptVertex.Normale[0], FLOAT_SIZE);
      aStream.Read(ptVertex.Normale[1], FLOAT_SIZE);
      aStream.Read(ptVertex.Normale[2], FLOAT_SIZE);

      aStream.Read(ptVertex.Diffuse32, DWORD_SIZE);

      aStream.Read(ptVertex.TextureCoord.S, FLOAT_SIZE);
      aStream.Read(ptVertex.TextureCoord.T, FLOAT_SIZE);
      ptVertex.TextureCoord.T := -ptVertex.TextureCoord.T;

      aStream.Read(ptVertex.SpriteSize.S, FLOAT_SIZE);
      aStream.Read(ptVertex.SpriteSize.T, FLOAT_SIZE);
    end;


    /// Read vertex indices for faces
    aStream.Read(IndiceCount , DWORD_SIZE);
    SetLength(Indices, IndiceCount);
    for i := 0 to IndiceCount-1 do
    begin
      aStream.Read(Indices[i] , WORD_SIZE);
    end;


    /// Faces of Branches
    SetLength(FBranchMeshes, TmHeader.BranchCount);
    for i := 0 to TmHeader.BranchCount - 1 do
    begin
      Index := TmHeader.Branches[i].IndexStart;
      SetLength(FBranchMeshes[i].Faces, TmHeader.Branches[i].PrimitiveCount);
      FBranchMeshes[i].VertexData := @FVisMesh;

      for j := 0 to TmHeader.Branches[i].PrimitiveCount - 1 do
      begin
        ptFace := @FBranchMeshes[i].Faces[j];

        ptFace.A := Indices[j+0];
        ptFace.B := Indices[j+1];
        ptFace.C := Indices[j+2];

        Inc(Index,3);
      end;
    end;


    /// Faces of Trunks
    SetLength(FTrunkMeshes, TmHeader.TrunkCount);
    for i := 0 to TmHeader.TrunkCount - 1 do
    begin
      Index := TmHeader.Trunks[i].IndexStart;
      SetLength(FTrunkMeshes[i].Faces, TmHeader.Trunks[i].PrimitiveCount);
      FTrunkMeshes[i].VertexData := @FVisMesh;

      for j := 0 to TmHeader.Trunks[i].PrimitiveCount - 1 do
      begin
        ptFace := @FTrunkMeshes[i].Faces[j];

        ptFace.A := Indices[j+0];
        ptFace.B := Indices[j+1];
        ptFace.C := Indices[j+2];

        Inc(Index,3);
      end;
    end;


    /// Sprites
    SetLength(FSpriteMeshes, TmHeader.TrunkCount);
    for i := 0 to TmHeader.SpriteCount - 1 do
    begin
      Index := TmHeader.Sprites[i].IndexStart;
      SetLength(FSpriteMeshes[i].Faces, TmHeader.Sprites[i].PrimitiveCount);
      FSpriteMeshes[i].VertexData := @FVisMesh;

      for j := 0 to TmHeader.Sprites[i].PrimitiveCount - 1 do
      begin
        ptFace := @FSpriteMeshes[i].Faces[j];

        ptFace.A := Indices[j+0];
        ptFace.B := Indices[j+1];
        ptFace.C := Indices[j+2];

        Inc(Index,3);
      end;
    end;
  end
    else
  begin
    //Invalid format
  end;

end;


end.
