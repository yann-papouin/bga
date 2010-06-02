-- Place an object template -> dummy in scene
-- 
-- Rex Hill


global bf_modPaths


/*
fn bf_ve_AddFname fname=
(
	local tmpFnames = bf_odb[2][1].tFileNames
	
	-- add a non-unique filename
	local tmpFindInx = findItem tmpFnames fname
	if tmpFindInx > 0 then return (tmpFindInx - 1)
	
	append tmpFnames fname
	
	return (tmpFnames.count - 1)
)


 
fn bf_ve_SetGenerateFname objRef M =
(
--	format "bf_ve_SetGenerateFname % %\n" objRef M
	
	local tmpObj = bf_odb[2][M].children[objRef]
--	format "tmpObj: %\n" tmpObj
	
	local strType = lowercase bf_odb[2][M].tTypes[tmpObj.type_ref+1]
--	format "strType: %\n" strType
	
	-- for i=1 to bf_odb[2][M].tTypes.count do format "%\n" bf_odb[2][M].tTypes[i]
	
	-- format "strType: %\n" strType
	local strF = case strType of
	(
		"effectbundle": "effects"
		"emitter": "effects"
		"particle": "effects"
		
		"standardmesh": "geometries"
		
		"projectile": "weapons"
		
		"engine": "physics"
		"spring": "physics"
		"wing": "physics"
		"floatingbundle": "physics"
		
		default: "objects"
	)
	
	
	local strNewFname = "objects/_gen/" + strF + ".con"
	
	return tmpObj.fname_ref = bf_ve_AddFname strNewFname
)

*/











-- import node Geometry from .tm files
fn bfImportNodeGeoms_TM theArray scale:10.0 useMat:false=
(
	-- read init.con file from mod
	bf_loadModpaths()
	
	local filesToExtract = #()

	
	for i=1 to theArray.count do
	(	
		append filesToExtract ("TreeMesh/" + theArray[i][2] + ".tm")
	)
	
	
	bf_RFA_extractFnamesMod FilesToExtract "TreeMesh" bf_modPaths
	

	-- Extract the .tm visible model (LOD01)
	-- Replace the geometry with extracted files
	
	local spawnCnt = 0
	
	for i=1 to theArray.count do
	(
		local tmpSMfname = g_TEMP_BF + "TreeMesh/" + theArray[i][2] + ".tm"
		
		--format " --- SMfname: %\n" tmpSMfname
		
		if (getFiles tmpSMfname).count > 0 then
		(				
			local SM_Node = importTM tmpSMfname (0.1*scale) doCollision:false
			
			if classof SM_Node == Array then
			(
				if isValidNode SM_Node[1] then
				(
					local obj = theArray[i][1]
					if isValidNode obj then
					(
						spawnCnt += 1
						
						SM_Node = SM_Node[1]				
						SM_Node.name = obj.name
						SM_Node.transform = obj.transform
						
						local t_userPropBuff = getUserPropBuffer obj
						if t_userPropBuff.count > 1 then setUserPropBuffer SM_Node t_userPropBuff
						
						-- re-assign children to this new node
						local tmpChildren = #()
						for j=1 to obj.children.count do tmpChildren[j] = obj.children[j]
						for j=1 to tmpChildren.count do tmpChildren[j].parent = SM_Node
		
						SM_Node.parent = obj.parent
						
						delete obj
						theArray[i][1] = SM_Node
					)
				)
			)
			---------

		)
	)


	return spawnCnt
)


-- import node Geometry from .sm files
fn bfImportNodeGeoms theArray scale:10.0 useMat:false=
(
	-- read init.con file from mod
	bf_loadModpaths()
	
	local filesToExtract = #()

	
	for i=1 to theArray.count do
	(	
		append filesToExtract ("StandardMesh/" + theArray[i][2] + ".sm")
	)
	
	if useMat == true then
	(
		for i=1 to theArray.count do
		(	
			append filesToExtract ("StandardMesh/" + theArray[i][2] + ".rs")
		)
	)
	
	bf_RFA_extractFnamesMod FilesToExtract "StandardMesh" bf_modPaths
	

	-- Extract the .sm visible model (LOD01)
	-- Replace the geometry with extracted files
	
	local spawnCnt = 0
	
	for i=1 to theArray.count do
	(
		local tmpSMfname = g_TEMP_BF + "standardMesh/" + theArray[i][2] + ".sm"
		
		--format " --- SMfname: %\n" tmpSMfname
		
		if (getFiles tmpSMfname).count > 0 then
		(				
			local SM_Node = readSMVisMesh tmpSMfname (0.1*scale) true
			
			if classof SM_Node == Array then
			(
				if isValidNode SM_Node[1] then
				(
					local obj = theArray[i][1]
					if isValidNode obj then
					(
						spawnCnt += 1
						
						SM_Node = SM_Node[1]				
						SM_Node.name = obj.name
						SM_Node.transform = obj.transform
						
						local t_userPropBuff = getUserPropBuffer obj
						if t_userPropBuff.count > 1 then setUserPropBuffer SM_Node t_userPropBuff
						
						-- re-assign children to this new node
						local tmpChildren = #()
						for j=1 to obj.children.count do tmpChildren[j] = obj.children[j]
						for j=1 to tmpChildren.count do tmpChildren[j].parent = SM_Node
		
						SM_Node.parent = obj.parent
						
						delete obj
						theArray[i][1] = SM_Node
					)
				)
			)
			---------

		)
	)

	return spawnCnt
)




fn bfGetGeomTempFile thisName =
(
	local theProps = bfODB_GetTemplate thisName masterType:1
	for i=1 to theProps.count do
	(
		if (theProps[i][1] as name) == #file then
		(
			return theProps[i][2]
			exit
		)
	) 
	return ""
)


fn bf_ve_ImportGeometry obj theArray s:100.0 andChildren:true useMat:false=
(
	if not isValidNode obj then return()
	
	local smFileName
	local myTemplate = bfODB_GetTemplate obj.Name
	for i=1 to myTemplate.count do
	(
		if myTemplate[i].count > 1 then
		(
			if (lowercase myTemplate[i][1]) == "geometry" then
			(
				local tmpSMfileName = bfGetGeomTempFile myTemplate[i][2]
				if tmpSMfileName.count > 0 then
				(
					if tmpSMfileName[1] == "\\" or tmpSMfileName[1] == "/" then
						tmpSMfileName = substring tmpSMfileName 2 -1
						
					append theArray #(obj, tmpSMfileName)
				)
			)
		)
	)
	
	if andChildren == true then
	(
		for i=1 to obj.children.count do
		(
			bf_ve_ImportGeometry obj.children[i] theArray andChildren:true useMat:useMat
		)
	)
	
)

------------------------------
-- Place an object in the scene
fn bfSpawnObj_Name objectName scale:100.0 useGeom:true useMat:false=
(
	bfODB_spawn objectName
	local thisObjNode = getNodeByName objectName
	if not isValidNOde thisObjNode then return()
	
	if useGeom == true then
	(
		local theArray = #()
		bf_ve_ImportGeometry thisObjNode theArray andChildren:true useMat:useMat
		
		format "theArray: %\n" theArray 
		if (bfImportNodeGeoms theArray scale:100.0 useMat:useMat) == 0 then
		(
		-- TODO: Support treemeshes
/*			bfImportNodeGeoms_TM theArray scale:100.0 useMat:useMat
			if theArray.count > 0 then
			(
				if isValidNode theArray[1][1] then
				(
					delete theArray[1][1]
					theArray[1][1] = getNodeByName objectName
				)
			)
*/
		)
		
		if theArray.count > 0 then thisObjNode = theArray[1][1]
		else thisObjNode = Undefined
	)
	
	return thisObjNode
)







-- bfODB_spawn "AH-6"


--	clearListener()
--	a = bfSpawnObj_Name "Afri_tree1_M1"
	-- bfSpawnObj_Name_TM "oak_tree_m1"
	


-------------------------------------------
/*
fn Test1a testStr=
(
	delete objects
	clearListener()
	bfSpawnObj_Name testStr useMat:true
)


-- Test1a "WILLY"
*/
