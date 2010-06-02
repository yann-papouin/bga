global g_bfodb_index
global g_bfodb_index_vh


--	clearListener()
--	bfODB_LoadMod "D:/games/Battlefield 1942/Mods/Force/init.con" "D:/games/Battlefield 1942/"
/*


loadMaxFile "D:/3dsmax5/scenes/willy.max"
bfODB_LoadMod "D:/games/Battlefield 1942/Mods/Force/init.con" "D:/games/Battlefield 1942/"

bf_ve_DBUpdateFromScene()
bfODB_SaveMod (g_TEMP_BF + "objects.rfa")


*/









fn bfODB_SetTemplate templateName propsStr masterType:0=
(
	bfmdtops.odb_SetTemplateStr templateName masterType propsStr
)
-- bfODB_SetTemplate "Willy" "mass 2500;"
-- bfODB_SetTemplate "Willy" "*;mass 2500;all props for this object"


fn bfODB_GetTemplate templateName masterType:0=
(
	local tmpStr = bfmdtops.odb_GetTemplateStr templateName masterType
	local propsList = filterString tmpStr ";"
	for i=1 to propsList.count do
	(
		local foundID = findString propsList[i] " "
		if foundID != undefined then 
			propsList[i] = #( (subString propsList[i] 1 (foundID-1)), (subString propsList[i] (foundID+1) -1))
		else
		(
			format "ERROR! bfODB_GetTemplate(\"%\") No spaces in property! %\n" templateName propsList[i]
			format " tmpStr: %\n" tmpStr 
			format " tmpStr.count: %\n" tmpStr.count
		)
	)
	return propsList
)

---------------------------------
fn bf_odb_CreateCamera name theParent s:100.0=
(
	local tmpCam = targetCamera name:name transform:(matrix3 [1,0,0] [0,0,1] [0,-1,0] [0,0,0])
	tmpCam.fov = 75
	tmpCam.type = #target
	tmpCam.targetDistance = 0.1
	tmpCam.clipManually = true
	tmpCam.nearClip = 0.01
	
	local myOffset = [0,0,0]
	local tmpTemplate = bfODB_GetTemplate name
	for i=1 to tmpTemplate.count do
	(
		local tmpPropName = lowercase tmpTemplate[i][1]
		if tmpPropName != "setpivotposition" then
		(
			if tmpPropName != "pivotposition" then
				continue
		)
		
		local argArray = filterString tmpTemplate[i][2] "/ "
		if argArray.count > 2 then
		(
			myOffset = [argArray[1] as float, argArray[2] as float, argArray[3] as float]
			myOffset = myOffset * s * 0.1
		)
		
		exit		
	)
	tmpCam.pos = myOffset
	tmpCam.transform = tmpCam.transform * theParent.transform
	tmpCam.parent = theParent
	return tmpCam
)


fn bfTempCreatePoint thisName thisType s:100.0=
(	
	thisType = thisType as name
	
	local tmpWireColor = case thisType of
	(
		#playercontrolobject: #(color 0 0 255, true, true, true)
		#engine: 			#(color 255 0 0, true, true, true)
		#rotationalbundle: 	#(color 164 94 37, true, true, false)
		
		#spring: 			#(color 88 88 225, true, false, false)
		#wing: 				#(color 26 177 148, true, false, false)
		#floatingbundle: 	#(color 26 177 148, true, false, false)
		
		default: 			#(color 100 0 0, true, false, false)
	)
	

	
	local obj = Point Name:thisName
	obj.wireColor = tmpWireColor[1];obj.box = tmpWireColor[2];obj.cross = tmpWireColor[3];obj.centerMarker = tmpWireColor[4];
	
	if tmpWireColor[1] != (color 100 0 0) then
		obj.size = s * 0.1;
	else obj.size = s * 0.05;
	
	if thisType == #Camera then
	(
		bf_odb_CreateCamera thisName obj
	)
	
	return obj
)
---------------------------------



--------------------------
fn bf_ve_isODB_loaded =
(
	if classof g_bfodb_index != Array then 
	(	g_bfodb_index = #()
		return false
	)

	if g_bfodb_index.count == 0 then return false

	return true
)

fn bfODB_BuildIndex =
(
	local tmpFname = g_BFDIR_BIN + "~odb_index.txt"
	bfmdtops.odb_SaveIndex tmpFname
	
	g_bfodb_index = #()
	g_bfodb_index_vh = #()
	
	local objCount = 0
	
	local f = openFile tmpFname
	if f != undefined then
	(
		local tmpLine
		local currentType = 0
		local typePCO = false
		while not (eof f) do
		(
			tmpLine = readLine f
			if (tmpLine.count > 0) then
			(
				if tmpLine[1] == "*" then
				(
					currentType += 1
					g_bfodb_index[currentType] = #(tmpLine)
					if lowercase tmpLine == "*playercontrolobject" then
					(
						typePCO = true
					)
					else typePCO = false
				)
				else
				(
					objCount += 1
					
					if typePCO then
					(
				 		local theseProps = filterString tmpLine ";"
						if (theseProps.count > 1) then
						(						
							local t_icon = ""
							local t_cat = "none"
							local t_mPhysics = false
							for n=2 to theseProps.count do
							(
								if theseProps[n].count > 2 then
								(
									if theseProps[n][1] == "i" then
									(	t_icon = substring theseProps[n] 3 -1
										if t_icon.count > 1 then
											if t_icon[1] == "\"" then
											(	t_icon = substring t_icon 2 (t_icon.count - 1)
											)
									)
									else if theseProps[n][1] == "c" then
										t_cat = substring theseProps[n] 3 -1
									else if theseProps[n][1] == "m" then
									(	
										t_mPhysics = "1" == (substring theseProps[n] 3 -1)
									)
								)
							)
							
							
							append g_bfodb_index_vh #(theseProps[1], t_icon, t_cat, t_mPhysics)
							-- append g_bfodb_index[currentType] theseProps[1]
						)
					)
					else
					(
						append g_bfodb_index[currentType] tmpLine
					)
					
				)
			)
		)
		close f
	)
	
	format "odbCount: %\n" objCount
	
	return objCount
)


fn bfODB_LoadMod strInitCon strGameDir=
(	bfmdtops.odb_LoadMod strInitCon strGameDir
	bfODB_BuildIndex()
)

fn bfODB_spawn objName =
(
	local tmpFname = g_BFDIR_BIN + "~odb_SpawnObj.ms"
	bfmdtops.odb_spawnObject objName tmpFname
	fileIn tmpFname
)

fn bfODB_SaveMod strRfaName =
(
	local strRawFilesDir = copy g_TEMP_BF
	local strRawFilesDir2 = copy g_TEMP_BF	
	
	-- Clean destination
	bf_ClearTmpDir "objects"
	
	local destRfaName = copy strRfaName 
	
	-- Save out .con files
	bfmdtops.odb_SaveMod strRfaName strRawFilesDir

	
	-- Pack and update the objects.rfa for the current mod
	bfmdtops.rfa_packFolder (strRawFilesDir2 + "objects") "objects" destRfaName 1
)







---------------------------------------------
--------- LOD selector operations -----------
---------------------------------------------

fn r_getChildrenAsArray node =
(
	local tmpchildren = #()
	for i=1 to node.children.count do
		append tmpchildren node.children[i]
	return tmpChildren
)


mapped fn r_unHideObjs node=
(
	if isValidNode node then
	(
		unhide node
		local tmpChildren = r_getChildrenAsArray node
		for i=1 to tmpChildren.count do
			r_unHideObjs tmpChildren[i]
	)
)


mapped fn r_hideObjs node=
(
	if isValidNode node then
	(
		hide node
		local tmpChildren = r_getChildrenAsArray node
		for i=1 to tmpChildren.count do
			r_hideObjs tmpChildren[i]
	)
)

fn r_isGrandParent young old =
(
	local tmpParent = young.parent

	while (tmpParent != undefined) do
	(
		if tmpParent == old then return old
		
		tmpParent = tmpParent.parent
	)
	
	return undefined
)




fn bfGetLodSelectorName objName =
(
	local tmpProps = bfODB_GetTemplate objName 
	for j=1 to tmpProps.count do
	(
		if lowercase tmpProps[j][1] == "lodselector" then
		(
			return tmpProps[j][2]
			exit
		)
	)
	return ""
)

---------------------------------------------
----- ODB general operations ----------------
---------------------------------------------



-------------------------
(
--	local timeStart = timeStamp()

--	format "Time: %\n" (timeStamp() - timeStart)
)