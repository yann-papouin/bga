-- Don't use persistent global for g_BFtheMeshArray of nodes
-- 3dsmax will crash when you try to load the scene and have specified a bbox back when you saved that scene
global g_BFtheMeshArray


rollout rBF_sm_Export "Mesh Export (.sm)" height:384  -- width:570 
(
/*	-- local uviconsI = (getdir #ui + "/icons/uvtools_24i.bmp")
	local TiconsI = ("tools_24i.bmp")
	local TiconsA = undefined
	local TiconsCount = 26
	--images:#(TiconsI , TiconsA , TiconsCount, 1, 2, 3, 4)
*/

-----------------------------------------------------------------------
-----------------------------------------------------------------------
	local changeRoll
	fn ckbImportPressed =
	(	changeRoll rBF_sm_Import
	)	
	-- Top Tabs
	include "rBFi_topHeader2.ms"
-----------------------------------------------------------------------
-----------------------------------------------------------------------
	label lbl1DUMMY "" pos:[0,340] width:26 height:8
	
	checkbox sm_useNames "Use Object Names For Export" pos:[172,56] width:164 height:16
	button btn_useNamesHelp "?" pos:[338,53] width:20 height:18 tooltip:"Instructions on using naming conventions"
	button btn6SaveAs "EXPORT" pos:[430,313] width:105 height:27 tooltip:"Saves the .sm and .rs files, prompting for the name and location first."
	spinner spn1numLOD "Min # of LODs: " pos:[111,176] width:53 height:16 enabled:true range:[2,6,3] type:#integer scale:1
	checkbox chk2lightMap "Lightmapped" pos:[433,135] width:85 height:16
	checkbox chk7simpleMat "Simple Shader" pos:[433,154] width:88 height:16
	checkbox chkLocal "Local" pos:[433,173] width:56 height:16
	spinner spn2Scale "Scale % " pos:[472,194] width:60 height:16 range:[0.1,10000,100] scale:1
	checkbox chk3matID "Force MatID" pos:[250,241] width:80 height:16
	spinner spn7matID "Material ID " pos:[287,304] width:56 height:16 enabled:false range:[0,65535,45] type:#integer scale:1
	groupBox grp5 "Visible Level of Detail Meshes" pos:[22,89] width:178 height:258
	button btn5Help "?" pos:[173,109] width:20 height:18 tooltip:"Gives instructions on a simple export procedure"
	checkbox chk8bspAuto "BSP Optimization" pos:[250,194] width:104 height:16 checked:true
	groupBox grp3 "Collision Mesh" pos:[215,89] width:186 height:258
	dropDownList ddl_materialIDs "" pos:[236,265] width:152 height:21 enabled:false items:#("Generic Armor")
	button btnRemLod "Remove" pos:[114,208] width:68 height:22 enabled:false tooltip:"Removes the selected LOD mesh entry from the listbox"
	pickButton btnAddLod "Add LOD" pos:[40,208] width:66 height:22 tooltip:"Adds a scene geometry object to the list of LOD meshes"
	listBox lbx_LODS "" pos:[35,240] width:151 height:7
	pickButton btnLowDetail "Low Detail" pos:[241,129] width:112 height:20 tooltip:"Pick a collision mesh used for low detail collision tests. (Terrain geometry collisions)"
	pickButton btnHighDetail "High Detail" pos:[241,159] width:112 height:20 enabled:false tooltip:"Pick a collision mesh used in game for high detail collision tests. (Collision Other objects, bullets etc..)"
	pickButton btnShadow "Shadow Mesh" pos:[428,255] width:81 height:20 tooltip:"Pickbutton designation of a scene geometry to be used in casting real-time 3d shadows onto the terrain. Used for vehicle parts. Note: Player model shadows are generated from the visible model"
	pickButton btnBBox "Custom BBox" pos:[428,228] width:81 height:20 tooltip:"The bounding box is used to determine clipping distances of objects visible mesh as well as physics balancing"
	groupBox grp4 "More Options" pos:[415,111] width:129 height:178
	button btnBBox_X " X" pos:[513,228] width:20 height:20 enabled:false tooltip:"Clear"
	button btnShadow_X " X" pos:[513,255] width:20 height:20 enabled:false tooltip:"Clear"
	button btnLowDetail_X " X" pos:[357,129] width:20 height:20 enabled:false tooltip:"Clear"
	button btnHighDetail_X " X" pos:[357,159] width:20 height:20 enabled:false tooltip:"Clear"
	checkbox chkGenLODS "Auto Generate LODs" pos:[44,150] width:124 height:17 checked:true
	checkbox chkTangents "64byte - BFV Only" pos:[44,125] width:125 height:16 checked:false 

	
--------------------------------
	local chk8bspAuto_checked = true
	local theMeshArray = #( #(), #(), #())
-----------------------------------------------------------------------


------------------------------------
-- (Node selections stored by scene, other settings used for all scenes)
	fn doNodeSelByScene =
	(
		theMeshArray = #( #(), #(), #())
		lbx_LODS.items = #()
		
		-- try (
		if (classof g_BFtheMeshArray == Array) then
		(
			if g_BFtheMeshArray.count > 2 then
			(
				-- main LOD
				if (classof g_BFtheMeshArray[1] == Array) then
				(
					
					for i=1 to g_BFtheMeshArray[1].count do
					(
						if isValidNode g_BFtheMeshArray[1][i] then
							btnAddLod.picked g_BFtheMeshArray[1][i]
					)
				)
				
				-- 
				if (classof g_BFtheMeshArray[2] == Array) then
				(
					if (g_BFtheMeshArray[2].count > 1) then
					(
						if isValidNode g_BFtheMeshArray[2][2] then
							btnHighDetail.picked g_BFtheMeshArray[2][2]
					)
					if (g_BFtheMeshArray[2].count > 0) then
					(
						if isValidNode g_BFtheMeshArray[2][1] then
							btnLowDetail.picked g_BFtheMeshArray[2][1]
					)
				)
				
				try 
				(	if (isValidNode g_BFtheMeshArray[3][1]) then 
						btnShadow.picked g_BFtheMeshArray[3][1]
				)
				catch()
				
				if (isValidNode g_BFtheMeshArray[4]) then 
					btnBBox.picked g_BFtheMeshArray[4]
			)
		)
		-- )
		-- Catch()
	)
----------------------
-- Disable controls when they have no effect 
	on sm_useNames changed state do
	(
		btnAddLod.enabled = not state
		
		lbx_LODS.enabled = not state

		btnLowDetail.enabled = not state	
		btnHighDetail.enabled = not state
			
		btnBBox.enabled = not state
		btnShadow.enabled = not state
		
		
		if (state == true) then
		(
			btnLowDetail_X.enabled = not state
			btnHighDetail_X.enabled = not state
			
			btnBBox_X.enabled = not state
			btnShadow_X.enabled = not state
			
			btnRemLod.enabled = not state
		)
		else
		(
			doNodeSelByScene()
		)

	)	
-----------------------------------------------------------------------	
	on rBF_sm_Export open do
	(
			
		-- INI settings (checkboxes, spinners, etc...)
		local tmpSetting = (getBfSetting "SMscale") as float
		if tmpSetting > 0.009 then
		(	spn2Scale.value = tmpSetting
			
			spn1numLOD.value = (getBfSetting "spn1numLOD") as float
			spn7matID.value = (getBfSetting "spn7matID") as float
			
			sm_useNames.checked = str2bool (getBfSetting "sm_useNames")
			chk2lightMap.checked = str2bool (getBfSetting "chk2lightMap")
			chk7simpleMat.checked = str2bool (getBfSetting "chk7simpleMat")
			chk3matID.checked = str2bool (getBfSetting "chk3matID")
			chk8bspAuto.checked = str2bool (getBfsetting "chk8bspAuto")
			chkGenLODS.checked = str2bool (getBfsetting "chkGenLODS")
			chkTangents.checked = str2bool (getBfsetting "chkTangents")
			chkLocal.checked = str2bool (getBFsetting "chkLocalSM")
			
			spn1numLOD.enabled = chkGenLODS.checked
			spn7matID.enabled = chk3matID.checked
			ddl_materialIDs.enabled = chk3matID.checked 
		)
		
		-- There is no multires in gmax
		
		if g_BFisGmax then
		(
			spn1numLOD.enabled = false
			chkGenLODS.checked = false
			chkGenLODS.enabled = false
			
			-- no auto-unwrap in gmax
			chk2lightMap.enabled = false
			chk2lightMap.checked = false
		)
		
		doNodeSelByScene()

		sm_useNames.changed sm_useNames.checked
	)
--------------------
	on rBF_sm_Export Close do
	(
		setBfSettingB "SMscale" (spn2Scale.value as string)
		setBfSettingB "spn1numLOD" (spn1numLOD.value as string)
		setBfSettingB "spn7matID" (spn7matID.value as string)
			
		setBfSettingB "sm_useNames" (sm_useNames.checked as string)
		setBfSettingB "chk2lightMap" (chk2lightMap.checked as string)
		setBfSettingB "chk7simpleMat" (chk7simpleMat.checked as string)
		setBfSettingB "chk3matID" (chk3matID.checked as string)
		setBfSettingB "chk8bspAuto" (chk8bspAuto.checked as string)
		setBfSettingB "chkGenLODS" (chkGenLODS.checked as string)
		setBfSettingB "chkTangents" (chkTangents.checked as string)
		setBfSettingB "chkLocalSM" (chkLocal.checked as string)
		
		g_BFtheMeshArray = theMeshArray
	)

------------------------------------
	

	on btnAddLod picked obj do
	(
		if classof theMeshArray[1] != array then theMeshArray[1] = #()
		
		append theMeshArray[1] obj
		local tmpItems = lbx_LODS.items
		append tmpItems (obj.name as string)
		lbx_LODS.items = tmpItems
		
		btnRemLod.enabled = true
	)
	
	on btnRemLod pressed do
	(
		local tmpINX = lbx_LODS.selection
		if tmpINX > 0 then
		(
			local tmpItems = lbx_LODS.items
			deleteItem tmpItems tmpINX
			
			if theMeshArray.count > 0 then
			(
				if theMeshArray[1].count >= tmpINX then
					deleteItem theMeshArray[1] tmpINX
			)
			
			lbx_LODS.items = tmpItems
			
			if tmpItems.count == 0 then
				btnRemLod.enabled = false
		)
		
	)
	
---------------------------------------------------	
	on btnHighDetail picked obj do
	(
		theMeshArray[2][2] = obj
		btnHighDetail.text = ("High: " + (obj.name as string))
		btnHighDetail_X.enabled = true
	)
	on btnHighDetail_X pressed do
	(
		if theMeshArray[2][2] != undefined then deleteItem theMeshArray[2] 2
		btnHighDetail.text = "High Detail"
		btnHighDetail_X.enabled = false
	)
		
	on btnLowDetail picked obj do
	(
		theMeshArray[2][1] = obj
		btnLowDetail.text = ("Low: " + (obj.name as string))
		btnLowDetail_X.enabled = true
		
		btnHighDetail.enabled = true
	)
	on btnLowDetail_X pressed do
	(
		if theMeshArray[2][1] != undefined then deleteItem theMeshArray[2] 1
		btnLowDetail.text = "Low Detail"
		btnLowDetail_X.enabled = false
		
		btnHighDetail_X.pressed()
		btnHighDetail.enabled = false
	)
	
---------------------------------------------------
	on btnShadow picked obj do
	(
		theMeshArray[3][1] = obj
		btnShadow.text = ("Shd: " + (obj.name as string))
		btnShadow_X.enabled = true
	)
	on btnShadow_X pressed do
	(
		if theMeshArray[3][1] != undefined then theMeshArray[3] = #()
		btnShadow.text = "Shadow Mesh"
		btnShadow_X.enabled = false
	)
	
		
	on btnBBox picked obj do
	(
		theMeshArray[4] = obj
		btnBBox.text = ("BBox: " + (obj.name as string))
		btnBBox_X.enabled = true
	)
	on btnBBox_X pressed do
	(
		if theMeshArray[4] != undefined then theMeshArray[4] = undefined
		btnBBox.text = "Custom BBox"
		btnBBox_X.enabled = false
	)
	
---------------------------------------------------
	on chkGenLODS changed state do
	(
		spn1numLOD.enabled = state
	)
	on chk3matID changed state do
	(
		spn7matID.enabled = state
		ddl_materialIDs.enabled = state
	)
	
	
	
	fn smSave fname useNames:false=
	(
		if useNames then
		(
			theMeshArray = bf1942mesh.parseSceneNames geometry
		)

		local genLodCount = 1
		if chkGenLODS.checked == true then genLodCount = spn1numLOD.value
		
		local tmpRetLods = makeAutoLods theMeshArray[1] genLodCount
		
		--format " Final LOD count: %\n" tmpRetLods[1].count
		theMeshArray[1] = tmpRetLods[1]
			
		local UseMatID = -1;
		if chk3matID.checked then UseMatID = spn7matID.value;	
		
		local vertStride = 64
		if chkTangents.checked == false then
		(
			if chk2lightMap.checked == true then vertStride = 40
			else vertStride = 32
		)	
		
		bf1942mesh.exportSM fname theMeshArray (spn2Scale.value * 0.001) \
				chk2lightMap.checked chk7simpleMat.checked UseMatID chkLocal.checked vertStride:vertStride
			
		-- delete auto-generated LOD geometry from the scene
		for i=1 to tmpRetLods[2].count do
		(
			if tmpRetLods[2][i] == true then
			(
				if isValidNode tmpRetLods[1][i] then
				(
					delete tmpRetLods[1][i]
				--	format " DEBUG: Deleted LOD%\n" i
				)
			)
		)

				
		if chk8bspAuto.checked then
		(
			addBspToThisFile fname "-a"
		)
	)
	
	on btn6SaveAs pressed do
	(
		-- check for meshes to export
		if (sm_useNames.checked) then
		(
			theMeshArray = bf1942mesh.parseSceneNames geometry
		)
		
		
		--format "btn6SaveAs.pressed() %\n" theMeshArray 
		
		(
			local tmpCount = 0
			for i=1 to 3 do
			(
				if tmpCount == 1 then 
					exit
					
				for j=1 to theMeshArray[i].count do
				(
					if isValidNode theMeshArray[i][j] then
					(
						tmpCount = 1
						exit
					)
				)
			)
			
			if tmpCount < 1 do 
			(
				MessageBox "Error! No meshes set to export\n";
				lbx_LODS.items = #()
				theMeshArray = #( #(), #(), #())
				
				btnShadow_X.pressed()
				btnLowDetail_X.pressed()
				btnHighDetail_X.pressed()
				btnBBox_X.pressed()
					
				return();
			)
		)
	
		local fname = getSaveFileName types:"StandardMesh(*.sm)|*.sm|All|*.*|"
		if fname != undefined then
		(	
			smSave fname useNames:(sm_useNames.checked)
		)
	)

	on btn5Help pressed do
	(	local msgTxt = ""
		msgTxt += "Beginner's instructions:\n"	
		msgTxt += "\n"
		msgTxt += " 1) Press Add LOD\n"
		msgTxt += " 2) Pick a mesh in the scene to be your visible mesh  \n"
		msgTxt += " 3) Press the EXPORT button\n"
		messagebox msgTxt title:"Help with .sm exporting" beep:false
	)
	
	on btn_useNamesHelp pressed do
	(
		local msgTxt = ""
		msgTxt += "Export with naming conventions.     \n"
		msgTxt += "\n"
		msgTxt += "\n"
		msgTxt += "LOD01  = Highest polygon visible mesh \n"
		msgTxt += "LOD02  = Lower polygon visible mesh \n"
		msgTxt += "LOD03  = Even lower polygon visible mesh \n"
		msgTxt += "...        = Least polygons \n"
		msgTxt += "\n"
		msgTxt += "COL01  = Simple Collision mesh \n"
		msgTxt += "COL02  = Complex Collision mesh \n"
		msgTxt += "bbox     = Optional bounding box (also \"bounds\")\n"
		msgTxt += "shadow = Optional Shadow \n"
		msgTxt += "\n"
		messagebox msgTxt title:".sm (Name-Based) exporting help" beep:false
	)

)






