fn getMaterialsInObjSet objSet =
(
	local theMaterials = #()
	for i=1 to objSet.count do
	(
		if (findItem theMaterials objSet[i].material) == 0 then
			append theMaterials objSet[i].material
	)
	return theMaterials 
)
fn getGroupedObjsByMat objSet theMaterials:undefined=
(
	if theMaterials == undefined then
		theMaterials = getMaterialsInObjSet objSet
	
	local newObjSet = #()
	for i=1 to theMaterials.count do
	(
		newObjSet[i] = #()
	)

	for i=1 to objSet.count do
	(
		local foundID = findItem theMaterials objSet[i].material
		if foundID != 0 then
			append newObjSet[foundID] objSet[i]
	)
	
	return newObjSet
)


fn tmExportSprites objs scale:0.1 stVtx:0=
(


	if objs.count < 1 then return false
	
	local verts_pos = #()
	local verts_uv = #()
	local verts_uv2 = #()
	
	local indices = #()
	
	local inxPos = 1
	local VertID = 0
	
	for i=1 to objs.count do
	(
		-- top left, top right, bot right, bot left,
		append indices (VertID + 0 + stVtx)
		append indices (VertID + 2 + stVtx)
		append indices (VertID + 3 + stVtx)
		
		append indices (VertID + 0 + stVtx)
		append indices (VertID + 1 + stVtx)
		append indices (VertID + 2 + stVtx)

		
		
		local tmpObj= objs[i]
	--	format "%: %\n" i tmpObj
		
		local w = (tmpObj.max.x - tmpObj.min.x) * scale
		local h = (tmpObj.max.z - tmpObj.min.z) * scale
		verts_uv2[VertID+1] = [-w,h]
		verts_uv2[VertID+2] = [w,h]
		verts_uv2[VertID+3] = [w,-h]
		verts_uv2[VertID+4] = [-w,-h]
		
		--format " w: %  h: %\n" w h
		
		-- positional values are all the center of the sprite
		local tmpPos = [tmpObj.pos.x, tmpObj.pos.z, tmpObj.pos.y] * scale
		for n=1 to 4 do
			verts_pos[VertID + n] = tmpPos 
		
		for n=1 to 4 do
		(
			local tmpMapVert = meshop.getMapVert tmpObj 1 n
			verts_uv[VertID + n] = [tmpMapVert.x, -tmpMapVert.y]
			--format "  %\n" verts_uv[VertID + n - 1]
		)
		
		VertID += 4
		inxPos += 6
	)
	
--	format "verts_pos: %\n" verts_pos
--	format "indices: %\n" indices 
--	format "verts_uv: %\n" verts_uv
--	format "verts_uv2: %\n" verts_uv2
	return #(verts_pos, indices, verts_uv, verts_uv2)
)

fn tmGetSpriteData objs scale:0.1 stVtx:0= 
(
	local theMaterials = getMaterialsInObjSet objs
	
	local theTextureNames = #()
	for i=1 to theMaterials.count do
		theTextureNames[i] = getTMTexture theMaterials[i]
	
	
	local theObjsSorted = getGroupedObjsByMat objs theMaterials:theMaterials
	
	
	local spriteData = #()
	local totalVerts = stVtx
	
	for i=1 to theObjsSorted.count do
	(
		spriteData[i] = tmExportSprites theObjsSorted[i] scale:scale stVtx:totalVerts
		totalVerts += spriteData[i][1].count
	)
	
	return #(theTextureNames, spriteData)
)

fn TM_CreateSprite w:10.0 h:10.0 =
(
	local tmpVerts = #([-0.5*w,0,0.5*h], [0.5*w,0,0.5*h], [0.5*w,0,-0.5*h], [-0.5*w,0,-0.5*h])
	
	
	local tmpFaces = #([3,2,1],[4,3,1])
	local tmpObj = mesh vertices:tmpVerts faces:tmpFaces
				-- tmpObj.pos = [0,0,0]
				tmpObj.name = "Sprite_0"
				tmpObj.material = tmpMaterial

	-- don't let the user rotate this item
	setTransformLockFlags tmpObj #{4..6} 
		
	(			
		meshop.setNumMaps tmpObj 2	
		meshop.setmapsupport tmpObj 1 true		
		meshop.setNumMapVerts tmpObj 1 4
		
		local uvs = #([1,0,0], [0,0,0], [0,1,0], [1,1,0])
		for i=1 to 4 do
			meshop.setMapVert tmpObj 1 i uvs[i]

		for i=1 to 2 do
			meshop.setMapFace tmpObj 1 i tmpFaces[i]
	)
	return tmpObj
)

(	
--	clearListener()
	
--	local tmObjs = treemeshParseNames geometry
--	tmGetSpriteData tmObjs[4] stVtx:0
)