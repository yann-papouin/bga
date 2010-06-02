-- Treemesh Export script
-- Author: Rex Hill
-- Date: 2004
--



fn charIsNumber tmpChar =
(
	local theNumbers = "0123456789"
	if (findString theNumbers tmpChar) != undefined then return true	
	else return false
)




fn renderBillboard objIn dir angles res=
(
	local tmpCnt = 1	
	try tmpCnt = objIn.count
	catch tmpCnt = 0

	local customLights = #()
	if lights.count == 0 then
	(
		customLights[1] = Omnilight pos:[-600,600,0] rgb:(color 255 255 255) shadowColor:(color 0 0 0) multiplier:1 contrast:0 softenDiffuseEdge:0 nearAttenStart:0 nearAttenEnd:40 farAttenStart:80 farAttenEnd:200 decayRadius:40 atmosOpacity:100 atmosColorAmt:100 shadowMultiplier:1
		customLights[2] = Omnilight pos:[600,600,0] rgb:(color 255 255 255) shadowColor:(color 0 0 0) multiplier:1 contrast:0 softenDiffuseEdge:0 nearAttenStart:0 nearAttenEnd:40 farAttenStart:80 farAttenEnd:200 decayRadius:40 atmosOpacity:100 atmosColorAmt:100 shadowMultiplier:1
		customLights[3] = Omnilight pos:[600,-600,0] rgb:(color 255 255 255) shadowColor:(color 0 0 0) multiplier:1 contrast:0 softenDiffuseEdge:0 nearAttenStart:0 nearAttenEnd:40 farAttenStart:80 farAttenEnd:200 decayRadius:40 atmosOpacity:100 atmosColorAmt:100 shadowMultiplier:1
		customLights[4] = Omnilight pos:[-600,-600,0] rgb:(color 255 255 255) shadowColor:(color 0 0 0) multiplier:1 contrast:0 softenDiffuseEdge:0 nearAttenStart:0 nearAttenEnd:40 farAttenStart:80 farAttenEnd:200 decayRadius:40 atmosOpacity:100 atmosColorAmt:100 shadowMultiplier:1
	
	)
	
	geometry.renderable = false
	objIn.renderable = true
	
	local tmpName = "tree"
	if tmpCnt > 0 then tmpName = objIn[1].name
	else tmpName = objIn.name
		
	local obj = objIn
	
	local objHeight = (obj.max.z - obj.min.z)
	local centerHeight = (objHeight * 0.5) + obj.min.z
	local maxWidth = obj.max.x
	if ( abs obj.min.x ) > maxWidth do maxWidth = ( abs obj.min.x )

	local tmpPixAspect = (maxwidth*1.88 / objHeight )
--	format "%\n" tmpPixAspect 
	
	maxwidth *= 2.5	
	local dummyParent = dummy name:"DumbRot"
	local cam = \ 
	Targetcamera fov:45 nearclip:1 farclip:1000 pos:[0,maxwidth , centerHeight] \
	target:(Targetobject transform:(matrix3 [1,0,0] [0,1,0] [0,0,1]	[0,0,centerHeight])) \
	parent:dummyParent

	local rotIncr = 360.0 / angles
--	format "rotationalIncrement: %\n" rotIncr
	targa.setColorDepth 32
	targa.setCompressed false
	targa.setAlphaSplit false
	targa.setPreMultAlpha true
	
	local theBitmap = bitmap (res * angles) res fileName:((getFilenamePath dir) + tmpName + ".tga")
	for i=1 to angles do
	(
		
		local tmpBitmap = render camera:cam pixelaspect:tmpPixAspect outputSize:[res,res] vfb:false
		dummyParent.rotation = eulerAngles 0 0 (i * rotIncr)
		
		for y=0 to (res-1) do
		(
			local tmpPixels = getPixels tmpBitmap [0,y] res
			setPixels theBitmap [(i-1)*res,y] tmpPixels
		)
	)
	geometry.renderable = true
	delete customLights
	
	display theBitmap
	
	delete dummyParent
	delete cam
	return true
)
struct tmBlock
(	indexStart,
	primitiveCnt,
	stringLen,
	textureName
)





--------------------------------------
-- gathers bbox info from an array of nodes
--
fn getBBoxOfArray tmpArray =
(
	local bbox = #([0,0,0], [0,0,0])
	
	for i=1 to tmpArray.count do
	(
		local tmpPoint3 = tmpArray[i].min
	
		for j=1 to 3 do
		(	if tmpPoint3[j] < bbox[1][j] then bbox[1][j] = tmpPoint3[j]
		)
		
		tmpPoint3 = tmpArray[i].max
		for j=1 to 3 do
		(	if tmpPoint3[j] > bbox[2][j] then bbox[2][j] = tmpPoint3[j]
		)
	)
		
	return bbox
)

fn writeBBoxInfo f bboxInfo scale =
(	for i=1 to 2 do
	(	bf_writeFloat f (bboxInfo[i][1]*scale)
		bf_writeFloat f (bboxInfo[i][3]*scale)
		bf_writeFloat f (bboxInfo[i][2]*scale)
	)
)


fn treemeshParseNames tObjSet=
(
	local tmpArray = #()

	for i=1 to 5 do
		tmpArray[i] = #()
	
	for i=1 to tObjSet.count do
	(
		-- make sure this object is ok to export
		if (classof tObjSet[i]) != Editable_mesh then convertToMesh tObjSet[i]	
		else collapseStack tObjSet[i]

		local tmpObjName = lowercase tObjSet[i].name
		
		local tmpName = ""
		for j=1 to tmpObjName .count do
		(	if not (charIsNumber tmpObjName[j]) and tmpObjName[j] != "_" then
				tmpName += tmpObjName[j]
			else exit
		)
	--	format "tmpname: %\n" tmpName
		
		local tmpReturn = undefined
		case tmpName of 
		(
			"col": 			tmpReturn = 1;
			"collision": 	tmpReturn = 1;
			"branch": 		tmpReturn = 2; 
			"trunk": 		tmpReturn = 3; 
			"sprite": 		tmpReturn = 4; 
		--	"billboard": 	tmpReturn = 5;
			default: tmpReturn = undefined;
		)
		if tmpReturn != undefined then
			append tmpArray[tmpReturn] tObjSet[i]
	)

	return tmpArray
)


fn getTMTexture objMat matName:"NONE"=
(
	local texName
	if objMat != undefined then
	(
		try 
		(
			if objMat.count > 0 then
			(
				try texName = objMat[1].maps[2].bitmap.filename
				catch()
			)
		)
		catch
		(
			try texName = objMat.maps[2].bitmap.filename
			catch()
		)
	)
	if texName == undefined then texName = matName
	texName = "texture/" + (getfileNameFile texName)
--	format "texName: %\n" texName

	return texName
)








--------------------------
--
--
fn angularTriangleSort objMesh angle =
(

	local startAngleOffset = 22.5;
	
	angle = angle + startAngleOffset
	if angle > 360 then angle -= 360
	else if angle < 0 then angle += 360
		
	local tmpTransform = (eulerAngles 0 0 angle) as matrix3
	
	local numVerts = objMesh.numVerts
	
	local vertYs = #()
	local vertYs_Sorted = #()	
	
	-- Store transformed y axis of each vert
	for i=1 to numVerts do
	(
		local tmpVert = (getVert objMesh i) * tmpTransform
		vertYs[i] = tmpVert[2]
		vertYs_Sorted[i] = tmpVert[2]
	)

	-- sort list verts based on transformed y axis position
	local vertRanks = #()

	sort vertYs_Sorted
	
	for i=1 to numVerts do
	(	vertRanks[i] = findItem vertYs_Sorted vertYs[i]
	)
	
	local numFaces = objMesh.numFaces
	
	local lowestVertRank = #()	
	
	for i=1 to numFaces do
	(
		local tmpFace = getFace objMesh i
		
		lowestVertRank[i] = vertRanks[ tmpFace[1] ]
	
		if lowestVertRank[i] > vertRanks[ tmpFace[2] ] then
			lowestVertRank[i] = vertRanks[ tmpFace[2] ]
			
		if lowestVertRank[i] > vertRanks[ tmpFace[3] ] then
			lowestVertRank[i] = vertRanks[ tmpFace[3] ]
		
	)
	
	
--	format "lowestVertRank: %\n" lowestVertRank
	
	local SortedTriangles = #()


	for i=1 to numVerts do
	(
		local tmpFacesArray = #()
		for j=1 to numFaces do
		(
			if i == lowestVertRank[j] then 
				append SortedTriangles j
		)
		
	)
	

	--format "sortedTriangles %\n" sortedTriangles
	
	return SortedTriangles
)




--------------------------
--
--
fn exportTM_Col f obj tScale matIDForce=
(
	format "--Stage: exportTM_Col\n"
	if matIDForce == 10000 do matIDForce = 0;
	
	
	local numVerts = obj.numVerts
	local numFaces = obj.numFaces
	
	bf_writeLong f -342375686
	bf_writeLong f 5
	
	local tmpVert, tmpMatID
	bf_writeLong f numVerts	
	for i=1 to numVerts do
	(	tmpVert = (getVert obj i) * tScale
		bf_writeFloat f tmpVert.x
		bf_writeFloat f tmpVert.z
		bf_writeFloat f tmpVert.y

		bf_writeFloat f tmpVert.x
		bf_fseek f -4 #seek_cur

		if matIDForce != -1 then
			tmpMatID = matIDForce 
		else
			tmpMatID = (getFaceMatID obj 1)
			
		bf_writeShort f tmpMatID 
		bf_fseek f 2 #seek_cur

	)
	
	
	bf_writeLong f numFaces
	for i=1 to numFaces do
	(	bf_writeShort f ((getface obj i).x - 1)
		bf_writeShort f ((getface obj i).y - 1)
		bf_writeShort f ((getface obj i).z - 1)

		if matIDForce != -1 then
			bf_writeShort f matIDForce
		else
			bf_writeShort f (getFaceMatID obj i)
	)
	
)

---------------------
--
--
fn WriteSimpleBsp f obj matIDForce=
(
	format "bspWRite startOffset: %\n" (bf_ftell f)
	local numFaces = obj.numfaces
	bf_writeLong f numFaces
	bf_writeLong f 0
	
	bf_writeLong f numFaces
	for i=1 to numFaces do
	(	bf_writeFloat f ((getFaceNormal obj i).x)
		bf_writeFloat f ((getFaceNormal obj i).z)
		bf_writeFloat f ((getFaceNormal obj i).y)
	
		bf_writeLong f 0
		bf_writeLong f ((getface obj i).x - 1)
		bf_writeLong f ((getface obj i).y - 1)
		bf_writeLong f ((getface obj i).z - 1)
	
		if matIDForce != -1 then
			bf_writeLong f matIDForce
		else
			bf_writeLong f (getFaceMatID obj i)
	)
	
	bf_writeString f "SimpleBSP tree method  "
	bf_writeLong f numFaces
	
	for i=0 to (numFaces - 1) do
		bf_writeLong f i 
	
	bf_writeShort f 0 -- just filler
	format "bspWRite EndOffset: %\n" (bf_ftell f)
)


--------------------
-- Exports visible mesh part to open file handle
-- 
fn exportTM_Vis f obj tScale= 
(
	local numFaces = obj.numFaces 
	local theMapCoords = #()
	for i=1 to numFaces do
	(
		local mapFace = meshop.getMapFace obj 1 i
		local visFace = getface obj i
		
		for j=1 to 3 do
			theMapCoords[ visFace[j] ] = mapFace[j]
	)

	local tmpVert
	
	local numVerts = obj.numVerts
	
	for i=1 to numVerts do
	(
		tmpVert = (getVert obj i) * tScale
		bf_writeFloat f tmpVert.x
		bf_writeFloat f tmpVert.z
		bf_writeFloat f tmpVert.y
		
		tmpVert = getNormal obj i
		bf_writeFloat f tmpVert.x
		bf_writeFloat f tmpVert.z
		bf_writeFloat f tmpVert.y
		
		-- local vDiffuse32 = 
		bf_writeLong f -2139062144 -- vertex color / vDiffuse32 
		
		
		
		tmpVert = meshop.getMapVert obj 1 theMapCoords[i]
		--format "%_tmpVert: %: %\n" i theMapCoords[i] tmpVert 
		
		bf_writeFloat f tmpVert.x
		bf_writeFloat f ( -tmpVert.y + 1.0 )
		
		-- 2nd set of texcords not used in branch and trunk meshes.
		bf_writeFloat f 0.0
		bf_writeFloat f 1.0
			
	
	)
)



fn exportTM_SpriteVerts f theSpriteData=
(
	for i=1 to theSpriteData.count do
	(
		local spriteGroup = theSpriteData[i]
		local numVerts = spriteGroup[1].count
		for n=1 to numVerts do
		(
			local tmpVert = spriteGroup[1][n]
			
			bf_writeFloat f tmpVert.x
			bf_writeFloat f tmpVert.y
			bf_writeFloat f tmpVert.z
			
			-- Normal (not used)
			bf_writeFloat f 0
			bf_writeFloat f 0
			bf_writeFloat f 0
			
			bf_writeLong f 0 -- vDiffuse32 (not used)
	
			tmpVert = spriteGroup[3][n]
			bf_writeFloat f tmpVert.x
			bf_writeFloat f tmpVert.y

			tmpVert = spriteGroup[4][n]
			bf_writeFloat f tmpVert.x
			bf_writeFloat f tmpVert.y			
		)
	)
)


fn exportTM_SpriteIndices f theSpriteData=
(
	for i=1 to theSpriteData.count do
	(
		local theIndices = theSpriteData[i][2]
		for n=1 to theIndices.count do
		(
			bf_writeShort f theIndices[n]
		)
	)
)
















fn bfmesh_RemDegenFaces obj= 
( 
	if obj != undefined do
	(
	   local delThese = #() 
	   for i=1 to obj.numfaces do 
	   ( 
	      local tmpFace, doDel = false 
	      tmpFace = getFace obj i 
	      if tmpFace[1] == tmpface[2] then doDel = true 
	      else if tmpFace[1] == tmpFace[3] then doDel = true 
	         else if tmpFace[2] == tmpFace[3] do doDel = true 
	      if doDel do delThese[delThese.count + 1] = i 
	   ) 
	--   format "delThese: %\n" delThese 
	
		-- GMAX FIX (** system exception **)
		if delThese.count > 0 do
		(	Rex_meshop_deleteFaces obj delThese delIsoVerts:true 
	   	)
   )
)



----------------------------------------------
-- Combine Tverts that are equal in U,V coords
fn r_condenseTvNewlist obj newlist chan:1=
(
	
--	format "Newlist: %\n" newList
	
	for i=1 to newlist[1].count do
	(
		if newlist[1][i] != undefined then
		(
			local tmpUVcoords = meshop.getMapVert obj chan newlist[1][i]
			
			for j=(i+1) to newList[1].count do -- scan remaining list for matches
			(	
				if (newlist[1][j] != undefined) then
				(
					local tmp2coords = meshop.getMapVert obj chan newlist[1][j]
					
					if tmpUVcoords.x == tmp2coords.x then
					(
						if tmpUVcoords.y == tmp2coords.y then
						(
							--	format "Condense TVERT\n"
		
							-- move faces to use this tvert [ i ]
							for k=1 to newlist[2][j].count do
							(
								append newlist[2][i] newlist[2][j][k]
							)
							
							-- remove faces from old location
							newlist[2][j] = #()
							
							-- set this vert as ready to remove from list
							newlist[1][j] = undefined
						)
					)
				)
			)
		)
		
	)
	
	-- remove undefined items from list
	local newerList = #( #(), #() )
	local newerList_Cnt = 1
	for i=1 to newlist[1].count do
	(
		if newlist[1][i] != undefined then
		(
			newerList[1][newerList_Cnt] = newList[1][i]
			newerList[2][newerList_Cnt] = newList[2][i]
			newerList_Cnt += 1
		)
	)
	
--	format "NewList: %\n" newerList
	return newerList
)
------------------------------------------------------------
-- For storing texture vert coords with vis Vert positions
fn r_FixTextureVerts obj=
(

	local trueVertNormalsList = #()
	local vertsUsedByList = #()


	for i=1 to obj.numverts do 
	(	vertsUsedByList[i] = #()
		trueVertNormalsList[i] = getnormal obj i
	)
	
	-- Each item consists of: FACE INX, Relative Vertex pos (1 2 or 3)

	for i=1 to obj.numfaces do
	(	local tmpFace = getFace obj i
		for j=1 to 3 do 
			append vertsUsedByList[ tmpFace[j] ] [i , j ]
	)




	local tmpNumVerts = vertsUsedByList.count
	for i=1 to tmpNumVerts do 
	(
		if (vertsUsedByList[ i ].count > 1) then -- more than 1 face sharing this vertex
		(				
			-- Take this  vertsUsedByList[ i ] list and sort into ... 
			-- ... a new array Based on texture vert ID that face uses for that vert
			
			local newList = #( #(), #() )
			
			for j=1 to vertsUsedByList[ i ].count do -- for each face sharing this vertex
			(
				local tmpTverts = meshop.getmapFace obj 1 (vertsUsedByList[ i ][j][1])
				
				local tmpTVertex = tmpTverts[ vertsUsedByList[ i ][j][2] ]
				local tmpFoundId = findItem newList[1] tmpTVertex
				if (tmpFoundId > 0) then
				(
					append newList[2][tmpFoundId] vertsUsedByList[ i ][j] -- [FaceInx, vert # 1-3]
				)
				else
				(
					append newList[1] tmpTVertex
					append newList[2] #( vertsUsedByList[ i ][j] ) -- [FaceInx, vert # 1-3]
				)
			)


			-- Combine Tverts that are equal in U,V coords
			newList = r_condenseTvNewlist obj newlist chan:1


			-- Now do the breaking
			-- example: #(#(3.0), #(#([1,2], [2,1])))
			if newList[1].count > 1 then
			(
			--	format "Vert: %   newList: %\n" i newList

				local tmpFaceB
				local tmpVertCount = obj.numverts - 1
				meshop.setnumverts obj (tmpVertCount + newList[1].count)
				
				for j=2 to newList[1].count do
				(
				--	format "%_Making Copy of vertex: %\n" j i
				--	format "  Assigning these faces to this new vert: %\n" newList[2][j]
					
					setvert obj (tmpVertCount + j) (getVert obj i)

					-- The vertex normal is overridden by 3dsmax
					-- build a normals list array to work around this undesired behavior				
					trueVertNormalsList[tmpVertCount + j] = trueVertNormalsList[i]

					for k=1 to newList[2][j].count do
					(
					--	format " %\n" newList[2][j][k]
						tmpFaceB = getface obj newList[2][j][k][1] 
						tmpFaceB[ newList[2][j][k][2] ] = tmpVertCount + j
						setface obj newList[2][j][k][1] tmpFaceB 
					)


				)
				
			)
			
		)
		
	)
	
	
	--
	

	-- It is important when writing the tverts that a custom getTvert function is written
	-- make sure to get the TVert by face, as the tvert's index in 3dsmax has no correlation
	-- also, there may be more verts than TVerts so face based get will advoid this problem
	return trueVertNormalsList
)

---------------------------
--	Fix texture verts so that each Vertex has one UV texture coord
--    3ds export/import method
fn bfmesh_FixTVerts obj lightmapped chan=
(

--	format "FixTVerts() cnt: %\n" obj.numVerts
	bfmesh_RemDegenFaces obj
	meshop.deleteIsoMapVertsAll obj
	
	local objMesh
	local trueNormals = #()
	
	if (meshop.getMapSupport obj 1) then
	(
		if obj.numFaces != (meshop.getNumMapFaces obj 1) do
		(
			format " numMapFaces != obj.numFaces \n"
			meshop.buildMapFaces obj 1
		)
	)
	else
	(
		meshop.setMapSupport obj 1 true
		meshop.buildMapFaces obj 1
	)

	if lightmapped then
	(
		objMesh = copy obj.mesh
		trueNormals = #()
	)
	else
	(
		with redraw off
		(
			undo off
			(
				objMesh = copy obj.mesh
				trueNormals = r_FixTextureVerts objMesh
			)
		)
	)
	--	format "end # Verts: %\tTextureVerts: %\n" (getNumVerts objMesh) (meshop.getNumMapVerts objMesh chan)

	return #(objMesh, trueNormals)
)
---------------------




---------------------
--
--
fn exportTM filename objs scale matIDForce=
(

	local f = bf_fopen filename "wb"
	if f != undefined then
	(
		format "Saving: %\n" filename
		format " objs: %\n" objs
		
		-- Version
		bf_writeLong f 3
		bf_writeLong f 0
		
		-- for billboards
		local angleCnt = 8
		bf_writeLong f angleCnt
		
		-- write BBox mesh
		local bboxMesh = #(geometry.min, geometry.max) 
		writeBBoxInfo f bboxMesh scale
		
		-- write BBox spriteLeaves
		local bboxSpriteLeaves = getBBoxOfArray objs[4]
		writeBBoxInfo f bboxSpriteLeaves scale
		
		local objMeshes = #()
		local indexStart = 0, totalVerts = 0, objInfo = #()
		
		
		-- branches and trunks
		for i=2 to 3 do
		(
			objInfo[i] = #()
			objMeshes[i] = #()
			
			bf_writeLong f objs[i].count
			for j=1 to objs[i].count do
			(
				local tmpObjMeshA = getMesh objs[i][j] false
				local tmpRet = bfmesh_FixTVerts tmpObjMeshA false 1
				
				objMeshes[i][j] = tmpRet[1]
				
				local primitiveCnt = objMeshes[i][j].numFaces
				
				objInfo[i][j] = #( indexStart, primitiveCnt , totalVerts )
				
				bf_writeLong f indexStart
				bf_writeLong f primitiveCnt -- FaceCount
				format " indexStart: %\n" indexStart
				format " primitiveCnt: %\n" primitiveCnt 
				
				local tmpTextureName = getTMTexture objs[i][j].material
				bf_writeLong f tmpTextureName.count
				writeBinaryString f tmpTextureName
				
				totalVerts += objMeshes[i][j].numVerts
				indexStart += objMeshes[i][j].numFaces * 3 * 8
			)
		)
		
		-- sprites (Grouped by common materials)
		-- format "beforeSprites, totalVerts: %\n" totalVerts
		local theSpriteData = tmGetSpriteData objs[4] stVtx:totalVerts
		(
			objInfo[4] = #()
			
			local spriteMats = theSpriteData[1]
			local spriteGroups = theSpriteData[2]
			
			bf_writeLong f spriteGroups.count
			for j=1 to spriteGroups.count do
			(
				local primitiveCnt = spriteGroups[j][1].count / 2
				format " sprite_primitiveCnt: %\n" primitiveCnt
				
				objInfo[4][j] = #( indexStart, primitiveCnt , totalVerts )
				
				bf_writeLong f indexStart
				bf_writeLong f primitiveCnt -- FaceCount
				
				local tmpTextureName = spriteMats[j]
				bf_writeLong f tmpTextureName.count
				writeBinaryString f tmpTextureName
				
				totalVerts += spriteGroups[j][1].count
				indexStart += spriteGroups[j][2].count
			)
		)		
		
		-- Billboard Count is always 0, not supported
		bf_writeLong f 0

		-- write collision model if it exists
		if objs[1].count > 0 then
		(
			exportTM_Col f objs[1][1] scale matIDForce 
			WriteSimpleBsp f objs[1][1] matIDForce
		)
		else bf_writeLong f 0
		
		-- write visible mesh portion
		(
			-- Write all verticies first
			format " Total Verts: % (@ %)\n" totalVerts (bf_ftell f)
		
			bf_writeLong f totalVerts

			-- 2-branch
			for j=1 to objs[2].count do 
			(
				exportTM_Vis f objMeshes[2][j] Scale
			)
			
			-- 3-trunk
			for j=1 to objs[3].count do 
			(
				exportTM_Vis f objMeshes[3][j] Scale
			)
			
			-- 4-sprite
			exportTM_SpriteVerts f theSpriteData[2]
			
			
			-- write indicies
			format " Total Indicies: %\n" indexStart 
			bf_writeLong f indexStart 
			for i=2 to 3 do
			(
				for j=1 to objs[i].count do
				(
					local tmpAngle = 0
					
					for b=1 to 8 do -- oneForEach billboard angle
					(
						local sortedTris = angularTriangleSort objMeshes[i][j] tmpAngle 
						-- format "sortedTris: %\n" sortedTris 
						
						for k=1 to objMeshes[i][j].numFaces do
						(	
							bf_writeShort f (objInfo[i][j][3] + (getface objMeshes[i][j] sortedTris[k]).z - 1)
							bf_writeShort f (objInfo[i][j][3] + (getface objMeshes[i][j] sortedTris[k]).y - 1)
							bf_writeShort f (objInfo[i][j][3] + (getface objMeshes[i][j] sortedTris[k]).x - 1)
						)
						
						tmpAngle += 45
					)
				)
			)	

			-- write sprite indices
			exportTM_SpriteIndices f theSpriteData[2]
		)
		
		bf_fclose f
		format "Success\n"
	)
	else
	format "ERROR! Could not create file: %\n" filename
)




fn test1a =
(
	ClearListener()

	--local fname = getSaveFileName types:"TreeMesh(*.tm)|*.tm|All|*.*|"
	local fname = "d:\\tmp.tm"
	if fname != undefined then exportTM fname (treemeshParseNames geometry) 0.1 -1
)

fn test1b =
(	
	clearListener()
	delete objects
--	local fname = getOpenFileName types:"TreeMesh(*.tm)|*.tm|All|*.*|"
	local fname = "D:\\tmp.tm" 
	if fname != undefined then importTM fname 10.0 forLM:false
)


/*
(
	test1a()
	test1b()
)
*/