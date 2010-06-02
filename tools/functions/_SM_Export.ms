-- .sm file export
-- Author: Rex Hill

fn charIsNumber tmpChar =
(
	local theNumbers = "0123456789"
	if (findString theNumbers tmpChar) != undefined then return true	
	else return false
)



fn floatsEqual a b = (floor ((b * 1000.0) + 0.5)) * 0.001 == (floor ((a * 1000.0) + 0.5)) * 0.001
fn point3Equal a b =
(	for i=1 to 3 do if not (floatsEqual a[i] b[i]) then return false
	return true;
)



fn rexAddLightmap obj=
(
	
	try (meshop.getMapVert obj 1 1) 
	catch
	(
		format "original mesh has no texture coordinates: %\n" obj
		meshop.setNumMaps obj 4 keep:true
		meshop.setMapSupport obj 1 true
		meshop.defaultMapFaces obj 1
	)

	select obj

	-- save the original channel 1 information
	local numMapVerts = meshop.getNumMapVerts obj 1
	
	local OriginalMapVerts = #()
	for i=1 to numMapVerts do
		OriginalMapVerts[i] = meshop.getMapVert obj 1 i
		
	local OriginalMapFaces = #()
	for i=1 to obj.numFaces do
		OriginalMapFaces[i] = meshop.getMapFace obj 1 i
		
	-- generate the flattened coords
	
	max modify mode
	addModifier obj (unwrap_UVW())
--	obj.modifiers[#unwrap_UVW].setMapChannel 3 -- MaxBug: Chan 3 will not flatten
	
	-- make sure no verts are selected
	obj.modifiers[#unwrap_UVW].selectVertices #{} 
	obj.modifiers[#unwrap_UVW].flattenMapNoParams()
	
	local numMapVertsB = meshop.getNumMapVerts obj 1
	
	convertToMesh obj

	-- setup map channel 3
	meshop.setNumMaps obj 4 keep:true
	meshop.setMapSupport obj 3 true
	meshop.defaultMapFaces obj 3
	
	-- copy channel 1 to channel 3	
	meshop.setNumMapVerts obj 3 numMapVertsB
	for i=1 to numMapVertsB do
		meshop.setMapVert obj 3 i (meshop.getMapVert obj 1 i)
		
	for i=1 to obj.numFaces do
		meshop.setMapFace obj 3 i (meshop.getMapFace obj 1 i)
		
	
	-- set channel 1 back to normal	
	meshop.setNumMapVerts obj 1 numMapVerts
	for i=1 to numMapVerts do
		meshop.setMapVert obj 1 i OriginalMapVerts[i]	
	for i=1 to obj.numFaces do
		 meshop.setMapFace obj 1 i OriginalMapFaces[i]

	return true
)

fn cleanArrayofUndef inArray =
(
	local outArray = #()
	
	for i=1 to inArray.count do
		if inArray[i] != undefined then append outArray inArray[i]
		
	return outArray
)



struct bf1942Mesh (
--------------------
-- Finds objects in scene that follow a naming convention
fn parseSceneNames tObjSet=
(
	local tmpArray = #()

	for i=1 to 3 do tmpArray[i] = #()
	
	for i=1 to tObjSet.count do
	(
		-- make sure this object is ok to export
		(	local tmpResult = 1
			if (classof tObjSet[i]) != Editable_mesh then tmpResult = convertToMesh tObjSet[i]	
			else collapseStack tObjSet[i]
			if tmpResult == undefined then
			(	format "Not geometry\n"
				continue
			)
		)
		
		local tmpReturn, tmpName, stringNoName = "", tmpNumber = 0
		local objSteName = lowercase tObjSet[i].name
		if objSteName.count > 0 then
		(
			tmpName = ""
			try (
				tmpName += objSteName[1] 
				tmpName += objSteName[2]
				tmpName += objSteName[3]
			)
			catch ()
			tmpName = lowercase tmpName
		)
		else tmpName "noName"
		

		local extraInfoName = ""
		for j=4 to objSteName.count do
		(
			if (charIsNumber objSteName[j]) then -- add number to numberName
				stringNoName += objSteName[j]
			else
				exit
		)

		
	--	format "tmpName: %\n" tmpName
	--	format "stringNoName: %\n" stringNoName

		if (stringNoName as integer) != undefined then
			tmpNumber = stringNoName as integer
		else
		(	if tmpName[1] != "s" and tmpName[1] != "b" do
				format "ERROR! Unable to getNumber from object name: % [%]\n" objSteName stringNoName
		)

		case tmpName of 
		(
			"lod": tmpReturn = 1;
			"col": tmpReturn = 2;
			"sha": tmpReturn = 3; -- "shadow"
			"bbo": tmpReturn = 4; -- "bbox"
			"bou": tmpReturn = 4; -- "bounds"
			default: tmpReturn = undefined;
		)

		
		if tmpReturn != undefined do
		(
			if tmpReturn == 4 then -- Bounding Box
			(	
				tmpArray[4] = tObjSet[i]
				format "   Using custom bounding box\n"
			)
			else
			(	if tmpNumber != 0 then
				(
					if tmpArray[tmpReturn][ tmpNumber ] == undefined then
						tmpArray[tmpReturn][tmpNumber] = tObjSet[i]
				)
				else if tmpReturn == 3 do tmpArray[tmpReturn][1] = tObjSet[i]
			)
		)
	)
	
	for n=1 to 3 do
		tmpArray[n] = cleanArrayofUndef tmpArray[n]
		

/*
	if tmpArray[1][1] == undefined and geometry.count > 0 then -- no main mesh
	(
		format "Using geometry[1] as main mesh: %\n" geometry[1].name
		convertToMesh geometry[1]
		tmpArray[1][1] = geometry[1]
	)
*/
	
	return tmpArray
),

-------------------
--
--
fn makeSomeLODs sourceArray cnt =
(
--Clearlistener()
--print "-----------------"
	
	format "sourceArray: %   cnt: %\n" sourceArray cnt
	local endArray = #()
	local isTemp = #()
	
	if sourceArray[1] != undefined then
	(	
		-- format "sourceArray[1].numverts: %\n" sourceArray[1].numverts
		endArray[1] = sourceArray[1]
		
		local Stepping = (1.0/cnt as float)
		with redraw off
		(
			for i=2 to cnt do
			(
				-- check that LOD mesh does not already exist
				if sourceArray[i] == undefined then 
				(
					--format "   i: %  endArray: %\n" i endArray
					endArray[i] = copy sourceArray[1]
					isTemp[i] = true; -- so we know to delete this after exporting
					endArray[i].name = "LOD0" + (i as string)

					--format "    aft_endArray: % \n" endArray
					
					local tmpMod = (100.0*(1.0 - i*stepping+stepping*0.5) as float)
					-- format "%_tmpMod: %\n" i tmpMod


					if g_BFisGmax == true then
					(
						BFreduce endArray[i] tmpMod 
					)
					else
					(
						-- format "MultiRes\n"
						max modify Mode
						addModifier endArray[i] (MultiRes())
						select endArray[i]
						endArray[i].modifiers[#MultiRes].Vertex_Percentage = tmpMod
						endArray[i].modifiers[#MultiRes].generate = true -- keeps only channel 1 data
					)
					
					
					if endArray[i].numFaces > 1 then
					(
						collapseStack endArray[i]
					)
					else
					(
						delete endArray[i]
						endArray[i] = undefined
						isTemp[i] = undefined
						format "Error generating LODs, stopped: 0%\n" (i-1)
						return  #(endArray,isTemp)
					)
				)
				else
				(
					endArray[i] = sourceArray[i]
				)
				
			)
		)
	)
	

	format "--MakeSomeLods()--\n"
	for i=1 to endArray.count do
	(
		format "%-%: %\n" i EndArray[i].name EndArray[i].mesh.numverts
	)
	format "------------------\n"


--print "-----------------"	
	return #(endArray,isTemp)
),


--------------------
-- .RS write material/shader file
--
fn writeRS thisFile materialData simpleShader=
(
	local thisNewFile = (getFilenamePath thisFile) + (getFilenameFile thisFile) + ".rs"
	format "Saving: %\n" thisNewFile 
	local f = bf_fopen thisNewFile "w"
	if f != undefined then
	(
		local frs = stringStream ""
		for i=1 to materialData.count do
		(
			rs_writeRSstruct frs materialData[i] simpleShader:simpleShader
		)
		bf_writeString2 f frs
		bf_fclose f
	)
	else format "ERROR! Could not create file: %" thisNewFile
	
),



--------------------
--   Material info gather
--
--
fn getMaterialData obj objmesh prjName lightmapped:false=
(
	--format "getMaterialData()\n"
	local numMats = 0, isMultiSub = false, meshObj = #(), tmpFace, tmpVert
	try 
	(	numMats = obj.material.count
		isMultiSub = true;
	)
	catch 
	(	numMats = 1;
		isMultiSub = false;
	)
	
	
	local tMaterials = #()

	local matIDList = #(), tmpID
	for j=1 to objmesh.numFaces do
	(	if (findItem matIDList (getFaceMatID objmesh j)) == 0 then
			append matIDList (getFaceMatID objmesh j)
	)
	
	sort matIDList
	
	

	
	if not isMultiSub then
	(	
		if matIDList.count > 1 do
		(
			matIDList = #(1)
		)
		local tmpMaterial
		if obj.material != undefined then
			tmpMaterial = obj.material
		else tmpMaterial = standardMaterial name:"blankNull"
		
		tMaterials[1] = rs_MaterialToRsStruct \
				tmpMaterial \
				tmpName:(prjName + "_Material0") lightmapped:lightmapped
				
		return #(tMaterials, true)		
	)
	else
	(	
		local matIDMax = obj.material.materialIDList[obj.material.materialIDList.count]

		for i=1 to matIDList.count do
		(
			local tmpFindID = findItem (obj.material.materialIDList) matIDList[i]
			if tmpFindID == 0 then
			(
				-- wrap the matIDList[i] value
				-- find Remainder
				
				local a_1 = matIDList[i] / matIDMax
				local r_1 = matIDList[i] - (a_1 * matIDMax)
				--format " r_1: %\n" r_1
				
				tmpFindID = findItem (obj.material.materialIDList) r_1
				if tmpFindID != 0 then
				(
					tmpFindID = r_1
				)
				else
				(
					tmpFindID = obj.material.materialIDList[1]
				)
			)
			--format "  tmpFindID: % | %\n" tmpFindID obj.material[3]
			
			tMaterials[i] = rs_MaterialToRsStruct \
				obj.material[ tmpFindID ] \
				tmpName:(prjName + "_Material" + ((matIDList[i] - 1) as string)) \
				lightmapped:lightmapped
				

		)

	)	
	
	
	return #(tMaterials, false)

),




--------------------
-- MAIN		Exports mesh to .sm file format
--
--

fn exportSM thisFile meshNodes tScale lightMapped simpleShader useMatID isLocal vertStride:64=
(
	format "exportSM: %\n" meshNodes

	local lightMapChan = 3
	
	(	local tmpCnt = 0
		for i=1 to 3 do tmpCnt += meshNodes[i].count
	--	format "Array[%]: %\n" tmpCnt meshNodes
		if tmpCnt < 1 do (MessageBox "Error! No meshes set to export\n";return false;)
	)
	
	format "\nSaving: % (isLocal: %)\n" thisFile isLocal
	local dir_tmpSM = g_TEMP_BF + "_SM/"
	MakeDir dir_tmpSM
	
	local f = bf_createFile (dir_tmpSM+"Settings.txt") 
	if f != undefined then
	(
		--local theBBox 
		--if isValidNode meshNodes[4] then theBBox = getTmpBBoxInfo meshNodes[4] isLocal 
		--else theBBox = getTmpBBoxInfo meshNodes[1][1] isLocal 
		
		format "*SETTINGS_GENERAL\n" to:f
		format "setFormat\tstandardmesh\n" to:f
		format "setScale\t1.0\n" to:f -- Scale is already applied to exported .rex files
		format "setStride\t%\n" vertStride to:f
		format "setversion\t 10\n" to:f
		
		if lightMapped == true then
			format "setLightmapped\t%\n" 1 to:f
		else
			format "setLightmapped\t%\n" 0 to:f
		
	
		
		
		
		-- Collision Meshes
		format "\n*MODELS_COLLISION\n" to:f
		for i=1 to meshNodes[2].count do
		(
			local tmpFname = "COL"
			if i < 10 then tmpFname = tmpFname + "0"
			tmpFname = tmpFname + (i as string) + ".rex"

			if true == (RexMesh.export meshNodes[2][i] fname:(dir_tmpSM+tmpFname) forceMatID:useMatID s:tScale isLocal:isLocal) then
				format "%\n" tmpFname to:f
		)

		
		-- Materials
		local allMatStructs = #(#())
		if meshNodes[1].count > 0 then
		(
			if isValidNode meshNodes[1][1] then
			(
				try
				(
					allMatStructs = Bf1942Mesh.getMaterialData meshNodes[1][1] meshNodes[1][1].mesh (getFilenameFile thisFile) lightmapped:lightmapped 
				)
				catch MessageBox ("LOD Object has no material or object is not a mesh:\n"+meshNodes[1][1].name)
				
				bf1942mesh.WriteRS thisFile allMatStructs[1] simpleShader
			)
		)
		
		-- Visible Meshes
		format "\n*MODELS_VISIBLE\n" to:f
		for i=1 to meshNodes[1].count do -- for EACH LOD
		(
			
			local tmpFname = "LOD"
			if i < 10 then tmpFname = tmpFname + "0"
			tmpFname = tmpFname + (i as string) + ".rex"
			
			if not isValidNode meshNodes[1][i] then
			(
				format "ERROR! invalid VisMesh LOD: %\n" meshNodes[1][i]
				continue
			)
			
			format "%_VISIBLE MESH NODE: %\n" i meshNodes[1][i].name
			local meshToExport = meshNodes[1][i]
			local meshIsTmpCopy = false
			
			
			if lightmapped then
			(		
				if not (meshop.getmapsupport meshNodes[1][i] lightMapChan) then
				(
					format " No support for lightmap channel: %\n" lightMapChan
					 
					if not g_BFisGmax then
					(
						meshToExport = copy meshNodes[1][i]
						local tmpRet = rexAddLightmap meshToExport
						format "DEBUG lightmapAdd success?: %\n" tmpRet 
						meshIsTmpCopy = true
					)
					else
						MessageBox "Unable to add lightmap, this version of max does have flatten UVW map\n"
				)
			)
			
			
			local tmpForceMatID = -1
			
			-- Force a model with no material to use 1 matID
			if (meshNodes[1][i].material == undefined) then tmpForceMatID = 1
			else
			(	
				-- Force a model with 1 material to use 1 matID
				if (allMatStructs[2] == true) then tmpForceMatID = 1
			)
			
			if true == (RexMesh.export meshToExport fname:(dir_tmpSM+tmpFname) forceMatID:tmpForceMatID s:tScale isLocal:isLocal) then
				format "%\n" tmpFname to:f
			
			if meshIsTmpCopy == true then
			(
				delete meshToExport
			)
			
		)
		

		
		
 
		-- Shadow Mesh
		format "\n*MODELS_SHADOW\n" to:f
		if meshNodes[3].count > 0 then
		(	
			local tmpFname = "SHADOW.rex"
			if true == (RexMesh.export meshNodes[3][1] fname:(dir_tmpSM+tmpFname) s:tScale isLocal:isLocal) then
				format "%\n" tmpFname to:f
		)
		

		-- Custom BBox Mesh
		format "\n*MODELS_BBOX\n" to:f
		if isValidNode meshNodes[4] then
		(	
			local tmpFname = "bbox.rex"
			if true == (RexMesh.export meshNodes[4] fname:(dir_tmpSM+tmpFname) s:tScale isLocal:isLocal) then
				format "%\n" tmpFname to:f
		)		
		format "\n\n" to:f
		
		bf_close f
		
		
		
		local f2 = stringstream "" 
		local f_out2 = bf_fopen ( g_BFDIR_BIN + "_ConvertSM.bat" ) "w"
		if f_out2 != undefined then
		(	
			format "\"%rexmesh.exe\" " g_BFDIR_BIN to:f2
			--format "\"%rexmesh.exe\" " "D:/code/STANDARDMESH/rexMesh/Release/" to:f2
			format "\"%\" " thisFile to:f2
			format "\"%settings.txt\"\n" dir_tmpSM to:f2		
			bf_writeString2 f_out2 f2
			bf_fclose f_out2
			f2 = Undefined
				
			-- run the .bat
			--format "cmd: %\n" ( "call \"" + g_BFDIR_BIN+ "_ConvertSM.bat\"" )
			--doscommand ( "call \"" + g_BFDIR_BIN+ "_ConvertSM.bat\"" )
			prettyDosBatch ( "call \"" + g_BFDIR_BIN+ "_ConvertSM.bat\"" )
		)
	)
	else
	(
		format "Error! Could not create settings.txt file!\n"
		return false;
	)



	return true
),

fn saveSimple thisFile objSet s:0.1 lightMapped:false simpleShader:true useMatID:-1 isLocal:false=
(
	bf1942mesh.exportSM thisFile (bf1942mesh.parseSceneNames objSet) s lightMapped simpleShader useMatID isLocal
)


)












fn makeAutoLods tmpArrayB numLods=
(
	if tmpArrayB.count < 1 then return #(#(),#())
	
	format "makeAutoLods: %\n" tmpArrayB
	
	-- Prevent crash if user supplies something like this: "LOD01", undefined, "LOD03"
	local totalItems = tmpArrayB.count
	local inx = 1
	
	-- Prevent deleted scene nodes, and compact list
	while inx <= totalItems do
	(	
		if not (isValidNode tmpArrayB[inx]) then 
		(
			deleteItem tmpArrayB inx
			totalItems -= 1
		)
		else inx += 1
	)
	
	local tmpArrayC = #()
	local isTmpArray = #()

	if numLods > tmpArrayB.count then	
	(	
		format "   Generating LODs from existing: % | %\n" tmpArrayB.count tmpArrayB
		local tmpRetArray = bf1942mesh.makeSomeLODs tmpArrayB numLods
		tmpArrayC = tmpRetArray[1]
		isTmpArray = tmpRetArray[2]
		--format "   Generated LODs count: % | % \n" tmpArrayC.count tmpArrayC
		
		-- Prevent deleted scene nodes, and compact list
		totalItems = tmpArrayC.count
		if isTmpArray[tmpArrayC.count] != true then isTmpArray[tmpArrayC.count] = undefined
		
		inx = 1
		while inx <= totalItems do
		(	
			if not (isValidNode tmpArrayC[inx]) then 
			(
				format "    Invalid Item: % | %\n" inx tmpArrayC[inx]
				deleteItem tmpArrayC inx
				deleteItem isTmpArray inx
				totalItems -= 1
			)
			else inx += 1
		)
		
		
	)
	else
	(
		return #(tmpArrayB , #())
	)
	
	return #(tmpArrayC, isTmpArray)	
)






	
fn addBspToThisFile fname arg1=
(
	local makeBspLocation = (g_BFDIR_BIN + "makeBsp.exe")
	if (getFiles makeBspLocation).count != 0 then
	(	local f = bf_fopen (g_BFDIR_BIN + "_batchBsp.bat") "w"
		if f != undefined then
		(
			local tmpDosCmd = ("@\"" + makeBspLocation + "\" " +"\"" + fname +"\" " + arg1 + " >" + "\"" +g_BFDIR_BIN+ "_bspBatch.log\"\n" )
			bf_writeString2 f tmpDosCmd
			bf_fclose f
			
			prettyDosBatch ("call " + "\"" + g_BFDIR_BIN + "_batchBsp.bat\"")
		)
		else messagebox "Error! Could Not create bsp .bat file" title:"Error"
	)
	else
	(	messagebox ("Error! Could not find file necessary for BSP:\n  " + makeBspLocation) title:"Error"
		return false
	)
	return true
)





(
--	clearListener()
--	bf1942mesh.getMaterialData geometry[1] "FRED"
--	bf1942mesh.saveSimple "C:\\test1.sm" geometry lightMapped:false
--	bf1942mesh.getMaterialData geometry[1] "fred"
)
