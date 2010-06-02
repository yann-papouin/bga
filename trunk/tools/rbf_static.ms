-- static object functions/rollouts






-------------------------------------------
-------------------------------
-------------------------------
fn ConvertTga24BfLightmaps8 inputFile =
(
	local f = bf_fopen inputFile "rb"
	if f != undefined then
	(
		local tgaType = bf_readlong f
		if tgaType == 131072 then -- 24bit uncompressed
		(	
			fseek f 8 #seek_cur
			local newFileName = inputFile + ".tmp"
			local f2 = bf_fopen newFileName "wb"
			if f2 != undefined then
			(
				local xres = bf_readshort f
				local yres = bf_readshort f
				fseek f 2 #seek_cur
				
				bf_writeLong f2 65792
				bf_writeLong f2 402718720
				bf_writeLong f2 0
				
				bf_writeshort f2 xres -- xres
				bf_writeshort f2 yres -- yres
				bf_writeshort f2 8 -- bit depth
				
				
				-- write pal
				for i=0 to 255 do
				(	bf_writebyte f2 i
					bf_writebyte f2 i
					bf_writebyte f2 i
				)
				
				tmp1 = 0
				for a=1 to yres do
				(
					for b=1 to xres do
					(
						bf_writebyte f2 (readbyte f)			
						bf_fseek f 2 #seek_cur
					)
				)
				bf_fclose f2
			)
			bf_fclose f
			
			if (deletefile inputFile) then
			(
				renameFile newFileName inputFile 
				return 1
			)
			else
			(	format "Could not delete existing file: %\n" inputFile
				return 0
			)
		)
		else
		(
			if tgaType == 65792 do 
			(	print ("Error: already correct format - " + inputFile)
				return 0
			)
			if tgaType == 655360 do 
			(	print ("Error: Compressed tga header - " + inputFile)
				return 0
			)
	
			-- unknown type
			(	print ("Error: Unknown tga header: " + (tgaType as string) )
				return 0
			)
		)
	)
	return 0
)
fn bf_lm_convertTgaBatch tmpFname:undefined =
(
	if tmpFname == undefined then
		tmpFname = getOpenFileName caption:"Convert .tga files 24bit -> 8bit (Batch, select the first file)" types:"*.tga|*.tga|All|*.*|"
	
	if tmpFname != undefined then
	(
		local tmpFiles = getFiles ((getFilenamePath tmpFname) + "*.tga")
		for i=1 to tmpFiles.count do
		(
			ConvertTga24BfLightmaps8 tmpFiles[i]
		)
	)
)
-------------------------------
-------------------------------
--------------------------------------------------









fn bf_GetDuplicateSceneNode tmpObj=
(
	-- duplicate tmpObj and all of tmpObj's children (keep the same names)
	
	local tmpRetNode = copy TmpObj
	tmpRetNode.name = tmpObj.name
	
	local tmpChildren = r_getChildrenAsArray tmpObj
	
	for i=1 to tmpChildren.count do
	(
		local tmpChild = bf_GetDuplicateSceneNode tmpChildren[i]
		tmpChild.parent = tmpRetNode
	)
	return tmpRetNode
)






fn BF_importStatics fname:undefined scale:10.0=
(
	if fname == undefined then
	(
		fname = getOpenFileName caption:"IMPORT .con file" types:"*.con|*.con|All|*.*|"
		if fname == undefined then return false;
	)
	
	local f = openFile fname 
	if f != undefined then
	(
		local tmpArrayB
		local tmpNode
	
		local ObjT = #(".create" as name)
		ObjT[2] = ".absolutePosition" as name
		ObjT[3] = ".rotation" as name
		ObjT[4] = ".geometry.scale" as name
		-- ObjT[5] = "Object.geometry.color" as name
		
		local posWasSet = false
		
		while not (eof f) do
		(
			local tmpLine = readline f
			
			local foundPos = findString tmpLine "."
			
			if foundPos != undefined then 
				tmpLine = substring tmpLine foundPos  -1
				
			local tmpArray = FilterString tmpLine " \t/"
			
			if tmpArray.count > 1 then
			(	

				local tmp_Arg1 = tmpArray[1] as name


				-- Spawn Object
				if tmp_Arg1 == ObjT[1] then
				(
					if posWasSet == false then
					(
						format "***POS NOT SET! FilePos: % | %\n" (filePos f) tmpNode
						if (filePos f) > 200 then
						(
							close f
							return 1
						)
					)
					posWasSet = false
					
					local tmpNodeDup = getNodeByName tmpArray[2]
					if isValidNode tmpNodeDup then
					(
						tmpNode = bf_GetDuplicateSceneNode tmpNodeDup
						tmpNode.scale = [1,1,1]
					)
					else
					(
						tmpNode = BfSpawnObj_name (lowercase tmpArray[2])
						
						if not (IsValidNode tmpNode) then
						(
							format "ERROR! NODE MISSINGa! %|\n" tmpArray[2]
							tmpNode = Dummy name:tmpArray[2]
						)
					)
					
					if not (IsValidNode tmpNode) then
					(
						format "ERROR! NODE MISSINGb! %|\n" tmpArray[2]
						tmpNode = Dummy name:tmpArray[2]
					)
				)
				else -- properties of spawned object
				(
					if isValidNode tmpNode then
					(
						if tmp_Arg1 != #rem then
						(
							-- SetPos
							if tmp_Arg1 == ObjT[2] then
							(
								posWasSet = true
								tmpNode.pos.x = (tmpArray[2] as float) * Scale
								tmpNode.pos.y = (tmpArray[4] as float) * Scale
								tmpNode.pos.z = (tmpArray[3] as float) * Scale
							)
							else 
							(
								-- SetRot
								if tmp_Arg1 == ObjT[3] then 
								(
									local tmpPos = tmpNode.pos
									tmpNode.pos = [0,0,0]
									
									tmpNode.transform = bf2maxConRot #(tmpArray[2] as float, tmpArray[3] as float, tmpArray[4] as float)
									
									tmpNode.pos = tmpPos
								)
								else
								(
									-- SetScale
									if tmp_Arg1 == ObjT[4] then
									(
										local tmpPoint3 = [tmpArray[2] as float, tmpArray[4] as float, tmpArray[3] as float]
										tmpNode.Scale = tmpPoint3
									)
									else
									(
									--	format "// Unknown prop! %\n" tmp_Arg1
									)
								)
							)
						)
					)
					else		
					(
				--		format "// NOT VALID NODE! line: %\n" tmpArray[2] 
					)
				)
				
	
			)
			
		)
		close f
	)
)


-- Search through this object's children for one that has a name starting with tmpName
fn bf_findChildNameStart tmpObj tmpName =
(
	local tmpChildren = r_getChildrenAsArray tmpObj
	for i=1 to tmpChildren.count do
	(
		local tmpNameChild = lowercase tmpChildren[i].name
		if (findString tmpNameChild tmpName) == 1 then
			return tmpChildren[i]
	)
	return undefined
)








-- configure lightmaps, rename and delete neccessary items (lodselectors etc..)
fn BF_setupLightmaps objs:undefined=
(
	if objs == undefined then objs = objects
	
	
	local objsToRender = #()
	
	for i=1 to objs.count do
	(
		if isValidNode objs[i] then
		if objs[i].parent == undefined then
		(
	
			if objs[i].children.count > 0 then
			(
				-- Find the first lodSelector template
				-- then use the first item in that list
				
				-- look for prefix "lod" in it's children
				local tmpChildLod = bf_findChildNameStart objs[i] "lod"
				if isValidNode tmpChildLod then
				(
					local tmpChildren = r_getChildrenAsArray tmpChildLod
					if tmpChildren.count > 0 then
					(
					--	format "  thisOne: %\n" tmpChildren[ 1 ]
						
						for i=2 to tmpChildren.count do
						(
						--	format "Delete! %\n" tmpChildren[i]
						--	delete tmpChildren[i]
						)
						tmpChildren[1].name = objs[i].name
						append objsToRender #(objs[i], tmpChildren[1])
					)
				)
				else
				(
				)
				
			)
			else
			(
				if (classof objs[i]) == Editable_mesh then
				(
					append objsToRender #(objs[i], objs[i])
				)
			)
		)
	)
	
	format "objsToRender: %\n" objsToRender.count
	
	-- now configure lightmap rendering settings
	for i=1 to objsToRender.count do
	(
		if Classof objsToRender[i][2] == Editable_mesh then
		(
			if (meshop.GetMapSupport objsToRender[i][2] 3) == true then
			(
				local be = LightingMap()
				tOb = objsToRender[i][2].INodeBakeProperties 
				tOb.removeAllBakeElements()
				
				objsToRender[i][2].bakeEnabled = true
				tOb.bakeChannel = 3
				
				tOb.addBakeElement be
			
				tx = "_"
				tx = tx + (((objsToRender[i][1].pos.x * 0.1) as integer) as string) + "-"
				tx = tx + (((objsToRender[i][1].pos.z * 0.1) as integer) as string) + "-"
				tx = tx + (((objsToRender[i][1].pos.y * 0.1) as integer) as string)
				
				format "  %%.tga\n" objsToRender[i][1].name tx
				
				be.elementName = tx
				be.autoSzOn = true
			)
		)
		else
		(
			format " %_not Editable_mesh: %\n" i objsToRender[i][2]
		)
	)
	
)

rollout rbf_lmRender "Lightmap Rendering" width:200 height:160
(
	button btn1 "Import StaticObjects.con" pos:[8,24] width:184 height:24
	button btn2 "Setup Scene Object's lightmaps" pos:[8,56] width:184 height:24
	button btn3 "Render Your lightmaps" pos:[8,88] width:184 height:24
	button btn4 "Convert Your lightmaps (24bit -> 8bit)" pos:[8,120] width:184 height:24
	
	on btn1 pressed do
	(
		BF_importStatics()
	)
	
	
	on btn2 pressed do
	(
		BF_setupLightmaps()
	)
	
	
	on btn3 pressed do
	(
		local tmpText = ""
		tmpText += "Setup your .tga save settings:\n"
		tmpText += "- Render a frame, and save as .tga file (choose 24bit, and pre-mult alpha only!)\n"
		tmpText += "\n"
		tmpText += "Setup render To Texture:\n"
		tmpText += "- Bring up the Render To Texture Dialog\n"
		tmpText += "- Under General Settings, Objects to Bake, Choose All Prepared Objects\n"
		tmpText += "- Uncheck Automatic Unwrap Mapping (on checkbox)\n"
		tmpText += "- Uncheck Create Baked Materials\n"
		tmpText += "- Set Automatic map size: 0.001 scale (adjust as yous see fit)\n"
		tmpText += "- Check Nearest Power of 2\n"
		tmpText += "- Set Min to 32, max to 1024, (or how you see fit)\n"
		tmpText += "- Set your output path\n"
		tmpText += "- Tweak other setings, setup your scene lights, (as you see fit)\n"
		tmpText += "- Hit Render\n"
		MessageBox tmpText title:"Instructions"
	)
	
	
	on btn4 pressed do
	(
		bf_lm_convertTgaBatch()
	)
)

fn bf_Lightmaps =
(
	CreateDialog rbf_lmRender
)

/*
(
	delete objects
	gc()
	clearListener()
	BF_importStatics fname:"D:\\StaticObjects.con"
)
*/
