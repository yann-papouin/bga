/* 
   Treemesh Import
   Author: Rex Hill
   Date: 2004
  
Changes:
 - Each object is imported individualy, parented to a single Point Node
 - added Sprite Import/Export Support
 
*/




struct tmBlock
(	indexStart,
	primitiveCnt,
	textureName
)

fn readBBoxInfo f scale=
(
	local tmpBboxInfo = #(0,0,0,0,0,0)
	tmpBboxInfo[1] = bf_ReadFloat f
	tmpBboxInfo[3] = bf_ReadFloat f
	tmpBboxInfo[2] = bf_ReadFloat f
	
	tmpBboxInfo[4] = bf_ReadFloat f
	tmpBboxInfo[6] = bf_ReadFloat f
	tmpBboxInfo[5] = bf_ReadFloat f
	
	return tmpBboxInfo
)


---------------------
-- TreemeshBlock
fn readTmBlock f =
(
	local tmpTmBlock = tmBlock textureName:""
		
	tmpTmBlock.indexStart = (bf_ReadLong f) + 1
	tmpTmBlock.primitiveCnt = bf_ReadLong f 
	
	local tmpStringLen = bf_ReadLong f
	tmpTmBlock.textureName = bf_readString2 f tmpStringLen
	
--	format "  indexStart:   %\t" tmpTmBlock.indexStart
--	format "  primitiveCnt: %\t" tmpTmBlock.primitiveCnt
--	format "  textureName: %\n" tmpTmBlock.textureName
	
	return tmpTmBlock
)

fn bypassBspDataNode f =
(
	bf_fseek f 24 #seek_cur -- m_plane 24
	bf_fseek f ((bf_ReadLong f)*4) #seek_cur -- indices count, and indices
	
	if (bf_ReadByte f) == 1 then -- above node
		bypassBspDataNode f
	if (bf_ReadByte f) == 1 then -- below node
		bypassBspDataNode f
)
fn bypassBspData f =
(
	local totalFaceListCnt, totalBspNodeCnt, totalFaceCnt 
	totalFaceListCnt = bf_ReadLong f
	totalBspNodeCnt = bf_ReadLong f
	totalFaceCnt = bf_ReadLong f
--	format "    BspHeader: %\n" #(totalFaceListCnt, totalBspNodeCnt, totalFaceCnt)
	bf_fseek f (totalFaceCnt * 32) #seek_cur
	
	bypassBspDataNode f
)










fn PointToObj obj pointTo_obj =
(
	if LookAt_Constraint == obj.rotation.controller then return()
	
	local lookatCtrl = LookAt_Constraint()
	obj.rotation.controller = lookatCtrl
	
	lookatCtrl.relative = false
	lookatCtrl.lookat_vector_length = 0.0
	lookatCtrl.target_axis = 1
	lookatCtrl.target_axisFlip = true
	lookatCtrl.upnode_axis = 2
	lookatCtrl.upnode_world = true
	lookatCtrl.stoUp_axis = 2
	lookatCtrl.stoUp_axisFlip = false
	lookatCtrl.appendTarget pointTo_obj 100.0
)

fn setup_spritesInScene pointTo_obj=
(
	if not (isValidNode pointTo_obj) then
		return false
	
	for i=1 to geometry.count do
	(
		if 1 == (findString geometry[i].name "Sprite") then
		(
			PointToObj geometry[i] pointTo_obj
		)
	)
	return true
)



fn importTM fileName scale doCollision:false forLM:false=
(
	--format "Importing: %\n" fileName
	local f = bf_fopen fileName "rb"
	if f == undefined then
	(
		format "Error! Could not open file: %\n" fileName
		return()
	)
	
	
	local header = bf_ReadLong f 
	if header != 3 then
	(
		format "Error! Could not open file: %\n" fileName
		format " Version: % != 3\n" header
		bf_fclose f
		return()
	)
	
	
	bf_fseek f 4 #seek_cur -- subVersion
	
	local angleCnt = bf_ReadLong f
	
	local bboxMesh = readBBoxInfo f scale
	local bboxSpriteLeaves = readBBoxInfo f scale

	--format " -- Header -- \n"
	--format "angleCount:   %\n" angleCnt -- for billboards
	--format "bboxMesh:     %\n" bboxMesh
	--format "bboxLeaves:   %\n" bboxSpriteLeaves

	--format " -- VisGeometry Info -- @%\n" (bf_ftell f)
	local visGeom = #()
	
	-- Header data of Visible geometry types
	(
		visGeom[1] = #()
		local BranchCnt = bf_ReadLong f 
		--format "BranchCnt: %\n" BranchCnt		
		for i=1 to BranchCnt do visGeom[1][i] = readTmBlock f
		
		visGeom[2] = #()
		local TrunkCnt = bf_ReadLong f 
		--format "TrunkCnt: %\n" TrunkCnt 
		for i=1 to TrunkCnt do visGeom[2][i] = readTmBlock f
		
		visGeom[3] = #()
		local SpriteCnt = bf_ReadLong f 
		-- format "SpriteGrpCnt: %\n" SpriteCnt
		for i=1 to SpriteCnt do visGeom[3][i] = readTmBlock f
		
		visGeom[4] = #()
		local BillboardCnt = bf_ReadLong f 
		--format "BillboardCnt: %\n" BillboardCnt
		for i=1 to BillboardCnt do visGeom[3][i] = readTmBlock f
	)
	
	local theRootObj = Point name:(getFileNameFile fileName)
	
	
	--format " -- Collision Mesh -- @%\n" (bf_ftell f)
	
	-- Collision Mesh data
	if (bf_ReadLong f) == -342375686 then
	(	
		if (bf_ReadLong f) == 5 then
		(
			
		
			local numVerts = bf_ReadLong f
			-- format "Collision Mesh, numVerts: %\n" numVerts
			local verts = #()
		--	local vertsMod = #()
			for k=1 to numVerts do
			(
				verts[k] = [0,0,0]
				verts[k].x = bf_ReadFloat f
				verts[k].z = bf_ReadFloat f
				verts[k].y = bf_ReadFloat f
				verts[k] *= scale
				
				bf_fseek f 4 #seek_cur
			)
			
			
			local numFaces = bf_ReadLong f
			-- format "   numColFaces: %\n" numFaces
			local faces = #()
			local facesIds = #()
			for k=1 to numFaces do
			(
				faces[k] = [1,2,3]
				faces[k][1] = (bf_ReadShort f)+1
				faces[k][2] = (bf_ReadShort f)+1
				faces[k][3] = (bf_ReadShort f)+1
				facesIds[k] = bf_ReadShort f -- Material ID
				if facesIds[k] == 0 then facesIds[k] = 10000 -- 10000 = 0
			)
			
			-- createNew Mesh
			if doCollision then
			(
				if not forLM then
				(
					local collisionMeshNode = mesh name:"COL_treemesh" vertices:verts faces:faces materialIDs:facesIds
					collisionMeshNode.parent = theRootObj
				)
			)
			
			-- BSP data
			--format "    bspWStart ReadOffset: %\n" (bf_ftell f)
			byPassBspData f
			--format "    EndBspReadPoint: @ %\n" (bf_ftell f)
			
		)
	)
	
	
	
	format " -- VisGeometry Data -- @%\n" (bf_ftell f)
	-- VisData
	(
		local numVerts = bf_ReadLong f
		format "Vertex_Count: %\n" numVerts
		
		local vPos = #()
		local vNormal= #()
		local vDiffuse32 = #()
		local vTexCoord = #()
		for i=1 to numVerts do
		(
			vpos[i] = [0,0,0]
			vpos[i].x = (bf_ReadFloat f)*scale
			vpos[i].z = (bf_ReadFloat f)*scale
			vpos[i].y = (bf_ReadFloat f)*scale
			
			vNormal[i] = [0,0,0]
			vNormal[i].x = bf_ReadFloat f
			vNormal[i].z = bf_ReadFloat f
			vNormal[i].y = bf_ReadFloat f
			
			vDiffuse32[i] = bf_ReadLong f
		--	format "vDiffuse32: %\n" vDiffuse32[i]
			
			vTexCoord[i] = #([0,0,0],[0,0,0])

			vTexCoord[i][1].x = bf_ReadFloat f
			vTexCoord[i][1].y = -(bf_ReadFloat f)
			
			-- These hold sprite width and height values
			vTexCoord[i][2].x = bf_ReadFloat f
			vTexCoord[i][2].y = bf_ReadFloat f
			
			--format "UVs: %\n" vTexCoord[i]
			
		)
		format " EndVerts: @%\n" (bf_ftell f)
		-- End Vis Verts
		
		
	
		-- Vis faces
		local numVisIndices = bf_ReadLong f
		format "Index_Count: %\n" numVisIndices
		local visIndices = #()	
		for i=1 to numVisIndices do
		(
			visIndices[i] = (bf_ReadShort f ) + 1
		)
		

		-- BRANCHES
		for m=1 to visGeom[1].count do
		(
			local inxPos = visGeom[1][m].indexStart
			local primitiveCnt = visGeom[1][m].primitiveCnt
			
			local faceList = #()
			for i=1 to primitiveCnt do
			(
				faceList[i] = [visIndices[inxPos+2], visIndices[inxPos+1], visIndices[inxPos]]
				inxPos += 3
			)
			
			local MeshNode = mesh name:("Branch_0"+(m as string)) vertices:vpos faces:faceList
			
			if not forLM then
			(	
				meshop.setNumMaps MeshNode 2	
				for i=1 to 1 do 
				(
					meshop.setmapsupport MeshNode i true
					meshop.setNumMapVerts MeshNode i vTexCoord.count
					for n=1 to vTexCoord.count do
					(
						meshop.setMapVert MeshNode i n vTexCoord[n][i]
					)
				
					for n=1 to faceList.count do
						meshop.setMapFace MeshNode i n faceList[n]
				)
			

				local textureName = visGeom[1][m].textureName
				if classof textureName == string then
				(
					local tmpMaterial = StandardMaterial name:"TM_Branch"
					tmpMaterial.maps[2] = BitmapTex filename:( textureName + ".dds" )
					tmpMaterial.twoSided = true
					tmpMaterial.maps[7] = BitmapTex filename:( textureName + ".dds" )
					tmpMaterial.maps[7].monoOutput = 1
					tmpMaterial.maps[7].RGBOutput = 1
					tmpMaterial.showInViewport = true
					MeshNode.material = tmpMaterial
				)
			)
			
			meshop.deleteIsoVerts MeshNode 
			MeshNode.parent = theRootObj
		)
	

	
		
	-- Trunks
		for m=1 to visGeom[2].count do
		(
			local inxPos = visGeom[2][m].indexStart
			local primitiveCnt = visGeom[2][m].primitiveCnt
			
			local faceList = #()
			for i = 1 to primitiveCnt do
			(
				faceList[i] = [visIndices[inxPos+2], visIndices[inxPos+1], visIndices[inxPos]]
				inxPos += 3
			)
			
			local MeshNode = mesh name:("Trunk_0"+(m as string)) vertices:vpos faces:faceList
		
			if not forLM then
			(
				meshop.setNumMaps MeshNode 2	
				for i=1 to 1 do 
				(	
					meshop.setmapsupport MeshNode i true
					meshop.setNumMapVerts MeshNode i vTexCoord.count
					for n=1 to vTexCoord.count do
					(
						meshop.setMapVert MeshNode i n vTexCoord[n][i]
					)
				
					for n=1 to faceList.count do
						meshop.setMapFace MeshNode i n faceList[n]
				)
				
				local textureName = visGeom[2][m].textureName
				if classof textureName == string then
				(
					local tmpMaterial = StandardMaterial name:"TM_Trunk"
					tmpMaterial.maps[2] = BitmapTex filename:( textureName + ".dds" )
					showTextureMap tmpMaterial tmpMaterial.maps[2] true
					tmpMaterial.twoSided = true
					MeshNode.material = tmpMaterial
				)
			)
			
			meshop.deleteIsoVerts MeshNode 
			MeshNode.parent = theRootObj
		)




		
	-- Sprites
		for m=1 to visGeom[3].count do
		(
			
			local inxPos = visGeom[3][m].indexStart
			local primitiveCnt = visGeom[3][m].primitiveCnt
			
			--format "primitiveCnt: %\n" primitiveCnt 
			
			local SpriteCount = primitiveCnt / 2
			
			local tmpMaterial = StandardMaterial name:"TM_Sprite"
			local textureName = visGeom[3][m].textureName
			if classof textureName == string then
			(
				if not forLM then
					tmpMaterial.maps[2] = BitmapTex filename:( textureName + ".dds" )
				
				tmpMaterial.selfIllumination = 100.0
				tmpMaterial.maps[7] = BitmapTex filename:( textureName + ".dds" )
				tmpMaterial.maps[7].monoOutput = 1
				tmpMaterial.maps[7].RGBOutput = 1
				
				if not forLM then
					tmpMaterial.showInViewport = true
			)
			
			format " START INDEX: %\n" inxPos 
			
			local faceList = #()
			format " SpriteCount: %\n" SpriteCount	
			
			local vertStart = visIndices[inxPos]
				
			for i=1 to SpriteCount do
			(
				
				local vStart = visIndices[inxPos]
				
			--	format "   %_: %\n" i inxPos 
			--	format "% % % | " visIndices[inxPos] visIndices[inxPos+1] visIndices[inxPos+2]
			--	format "% % %\n" visIndices[inxPos+2] visIndices[inxPos+3] visIndices[inxPos+4]
				
				local QuadVertIDs = #(vStart,vStart+1,vStart+2,vStart+3)
				
				--format " %_QuadVertIDs : %\n" i QuadVertIDs 
				
				--for j=1 to 4 do
				(
				--	format "   %_pos: %\n" QuadVertIDs[j] vpos[ QuadVertIDs[j] ]
				--	format "   %_uvs: %\n" QuadVertIDs[j] vTexCoord[ QuadVertIDs[j] ]
				)
			
				local texCoordTmp = vTexCoord[ QuadVertIDs[1] ][2]
				local w = texCoordTmp[1] * scale * -1.0
				local h = texCoordTmp[2] * scale
				--format "  w: % h: %\n" w h
				
				local tmpVerts = #([-0.5*w,0,0.5*h],[0.5*w,0,0.5*h],[0.5*w,0,-0.5*h],[-0.5*w,0,-0.5*h])
				
				local tmpFaces = #([3,2,1],[4,3,1])
				local tmpObj = mesh vertices:tmpVerts faces:tmpFaces
				tmpObj.pos = vpos[QuadVertIDs[1]]
				tmpObj.name = "Sprite_" + (m as string) + "_" + (i as string)
				tmpObj.material = tmpMaterial
				
				meshop.setNumMaps tmpObj 2	
				meshop.setmapsupport tmpObj 1 true
				meshop.setNumMapVerts tmpObj 1 4
	
				for n=1 to 4 do
				(	
					local err = 1
					if QuadVertIDs[n] != undefined then
					(
						if vTexCoord[ QuadVertIDs[n] ] != undefined then
						(	
							err = 0
							meshop.setMapVert tmpObj 1 n vTexCoord[ QuadVertIDs[n] ][1]
						)
					)
					
					if err == 1 then
					(
						format "ERROR!!!! QuadVertIDs[%]: %\n" n QuadVertIDs[n]
					)
				)
				
				for n=1 to faceList.count do
					meshop.setMapFace tmpObj 1 n tmpFaces[n]

				
				tmpObj.parent = theRootObj
				
				inxPos+=6

			)
			
			--local MeshNode = mesh name:("Sprite_0"+(m as string)) vertices:vpos faces:faceList
		)
		
		
		bf_fclose f
	)				

	
	bf_fclose f
	
	if false == setup_spritesInScene $Bf_Light then
		setup_spritesInScene $Camera01

	return theRootObj
)


/*
(	clearListener()
	delete objects
--	local fname = getOpenFileName types:"TreeMesh(*.tm)|*.tm|All|*.*|"
	local fname = "D:\\Afri_tree4_M1.tm"
	local fname = "D:\\tmp.tm" 
	if fname != undefined then importTM fname 10.0 forLM:false
)
*/