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
 * The Original Code is TypesTM (http://code.google.com/p/bga)
 *
 * The Initial Developer of the Original Code is
 * Yann Papouin <yann.papouin at @ gmail.com>
 *
 * ***** END LICENSE BLOCK ***** *)

unit TypesTM;

interface

uses Classes, VectorTypes, VectorGeometry;

const
  DTM_SCALE = 10;

type

  PTMBBox = ^TTMBBox;
  TTMBBox = record
    P1 : TVector3f;
    P2 : TVector3f;
  end;

  PTMBlock = ^TTMBlock;
  TTMBlock = record
    IndexStart : Longword;
    PrimitiveCount : Longword;
    TextureName : Ansistring;
  end;

  PTMHeader = ^TTMHeader;
  TTMHeader = record
    BranchCount : Longword;
    Branches : array of TTMBlock;
    TrunkCount : Longword;
    Trunks : array of TTMBlock;
    SpriteCount : Longword;
    Sprites : array of TTMBlock;
    BillboardCount : Longword;
    Billboards : array of TTMBlock;
  end;

  PTMVertex = ^TTMVertex;
  TTMVertex= record
    Position : TVector3f;
    Normale : TVector3f;
    MaterialID : Word;
    Diffuse32 : Longword;
    TextureCoord : TTexPoint;
    SpriteSize : TTexPoint;
  end;

  PTMFace = ^TTMFace;
  TTMFace = record
    A : LongWord;
    B : LongWord;
    C : LongWord;
    MaterialID : Word;
  end;

  PTMVertexData = ^TTMVertexData;
  TTMVertexData = record
    VertexCount : Longword;
    Vertex : array of TTMVertex;
  end;

  PTMFaceData = ^TTMFaceData;
  TTMFaceData = record
    VertexData : PTMVertexData;
    FaceCount : Longword;
    Faces : array of TTMFace;
  end;

  PTMColMeshData = ^TTMColMeshData;
  TTMColMeshData = record
    VertexCount : Longword;
    Vertex : array of TTMVertex;
    FaceCount : Longword;
    Faces : array of TTMFace;
  end;




implementation

end.
