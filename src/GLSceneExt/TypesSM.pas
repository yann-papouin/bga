unit TypesSM;

interface

uses Classes, VectorTypes, VectorGeometry;

type

  PSMBBox = ^TSMBBox;
  TSMBBox = record
    P1 : TVector3f;
    P2 : TVector3f;
  end;

  PSMVertex = ^TSMVertex;
  TSMVertex= record
    Value : TVector3f;
    MaterialID : Word;
  end;

  PSMFace = ^TSMFace;
  TSMFace = record
    A : Word;
    B : Word;
    C : Word;
    MaterialID : Word;
  end;

  PSMMaterial = ^TSMMaterial;
  TSMMaterial = record
    Name : Ansistring;
    RenderType : Longword;
    VertFormat : Longword;  //vertexFormat
    VertStride : Longword;  //VertByteSize
    TexCoord1 : array of TTexPoint;
    TexCoord1Count : Longword;
    TexCoord2 : array of TTexPoint;
    TexCoord2Count : Longword;
    IndexNum : Longword;    //numVertices = numFaces*3
  end;

  PSMMeshData = ^TSMMeshData;
  TSMMeshData = record
    VertexCount : Longword;
    Vertex : array of TSMVertex;
    FaceCount : Longword;
    Faces : array of TSMFace;
    NormaleCount : Longword;
    Normales : array of TSMVertex;
  end;

  PSMLodMesh = ^TSMLodMesh;
  TSMLodMesh = record
    MeshData : TSMMeshData;
    Material : TSMMaterial;
  end;

  PSMMesh = ^TSMMesh;
  TSMMesh = record
    LodMeshCount : Longword;
    LodMeshes : array of TSMLodMesh;
  end;

implementation

end.
