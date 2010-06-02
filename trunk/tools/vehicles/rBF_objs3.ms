-- Indexes
global g_bfodb_index
global g_bfodb_index_vh

global g_BfSceneLODs


-- Rollouts
global rBF_objs
global rbf_selectMod
global rBf_pickGame


global bf_odb_sel
global bf_loadModpaths

global g_PathBfDir
global g_bfCurrentMod

-- Functions
global bf_ve_DBUpdateFromScene
global bfODB_SaveMod



filein (bfmsloc + "vehicles\\bf_RFA.ms")

filein (bfmsloc + "vehicles\\bf_odb_ops.ms")

filein (bfmsloc + "vehicles\\bf_GetMod.ms")


-- need work on these two scripts
filein (bfmsloc + "vehicles\\bf_odb_Spawning.ms")
filein (bfmsloc + "vehicles\\bf_odb_SaveChanges.ms")

filein (bfmsloc + "vehicles\\ext_sceneMatTextures.ms")


-------------------------------------
-------------------------------------
-------------------------------------

rollout rBF_objs "Object Rollout" width:552 height:384
(

-----------------------------------------------------------------------
-----------------------------------------------------------------------
	-- Top Tabs
	include "rBFi_topHeader.ms"
-----------------------------------------------------------------------
-----------------------------------------------------------------------

	
	GroupBox grp1 "Vehicle Import" pos:[25,80] width:320 height:248
	GroupBox grp4 "Update Mod Based On Scene" pos:[360,272] width:168 height:56
	button btn_SaveAll "Save Changes" pos:[379,294] width:129 height:24
	button btn_objSpawn "Import Vehicle" pos:[56,292] width:112 height:22
	checkbox chk_OptTexures "Material and Textures" pos:[199,264] width:125 height:16 checked:true
	spinner spn_OptScale "" pos:[271,287] width:55 height:16 range:[0,1000,100]
	listbox cbx_Obj "" pos:[40,160] width:144 height:9
	dropdownList ddl_SubType "" pos:[40,128] width:144 height:21 enabled:true items:#("All")
	button btn_modSet "Load Mod" pos:[24,48] width:64 height:24
	label label_ModName "" pos:[125,56] width:240 height:16
	label lbl2 "Mod:" pos:[96,56] width:26 height:16
	checkbox chkMobilePhysics "Mobile Physics" pos:[40,106] width:144 height:16 checked:true
	listbox lbxLODa "" pos:[376,120] width:136 height:4
	listbox lbxLODb "" pos:[376,182] width:136 height:4
	GroupBox grp17 "Lodselector Template Display" pos:[360,96] width:168 height:172
	bitmap bmp1 "" pos:[200,128] width:128 height:128
	checkbox chkPreview "Show Preview" pos:[200,106] width:99 height:16 checked:true
	label lbl9 "Scale:" pos:[219,286] width:31 height:16
	button btnLODall "Show All" pos:[446,244] width:62 height:18
	button btnLOD1st "Show First" pos:[380,244] width:62 height:18
	
	
	label lbl4 "" pos:[0,340] width:17 height:8
	

	

-------------------------
-------- Functions ------
-------------------------
	fn setActiveState state:undefined=
	(
		if state == undefined then
		(
			state = bf_ve_isODB_loaded()
		)
		
		btn_SaveAll.enabled = state

		btn_objSpawn.enabled = state
		

		chk_OptTexures.enabled = state
		
		spn_OptScale.enabled = state
		cbx_Obj.enabled = state
		ddl_SubType.enabled = state
		
		chkMobilePhysics.enabled = state

		chkPreview.enabled = state

		
		btnLODall.enabled = state
		btnLOD1st.enabled = state
		
		return state
	)



	
	


	------------------------------------------------
	-- Update the GUI listbox of objects
	--
	fn refresh_objList MobilePhysics:-1 =
	(
		if MobilePhysics == -1 then
			MobilePhysics = chkMobilePhysics.checked
			
		local currentSubType = ddl_SubType.selected
		local bIsAllType = (currentSubType == "ALL")
		
		local objsArray  = #()
		if bIsAllType then
		(
			for i=1 to g_bfodb_index_vh.count do
			(
				if g_bfodb_index_vh[i][4] == true then
				(	if MobilePhysics then append objsArray g_bfodb_index_vh[i][1]

				)
				else
				(	if not MobilePhysics then append objsArray g_bfodb_index_vh[i][1]
				)
			)
		)
		else
		(
			for i=1 to g_bfodb_index_vh.count do
			(		
				if g_bfodb_index_vh[i][3] == currentSubType then
				(
					if g_bfodb_index_vh[i][4] == true then
					(	if MobilePhysics then append objsArray g_bfodb_index_vh[i][1]
					)
					else
					(	if not MobilePhysics then append objsArray g_bfodb_index_vh[i][1]
					)
				)
			)
		)
		
		
		-- Note: Case insensitive compare (Sort() will not work here!)
		--objsArray = sort_stringArray objsArray
		cbx_Obj.items = (sort_stringArray objsArray)
	)
	












	fn extractPreviewImages =
	(
		format "extractPreviewImages()\n"
		
		local tmpFilesToExtract = #()
		local theFilesToExtract = #()
		
		-- blank versions
		for i=1 to g_bfodb_index_vh.count do
		(
			local tmpFname = \
				(getFilenamePath ("menu/texture/" + (g_bfodb_index_vh[i][2] as string))) \
				+ (GetFilenameFile (g_bfodb_index_vh[i][2] as string))
			
			if 0 == findItem tmpFilesToExtract tmpFname then
			(
				append tmpFilesToExtract tmpFname
				--format " %_VehicleIcon: %\n" i tmpFname
			)
		)
	
		
		-- .dds versions
		for i=1 to tmpFilesToExtract.count do
		(
			local tmpFname = tmpFilesToExtract[i] + ".dds"
			append theFilesToExtract tmpFname
		)

		-- .tga versions
		for i=1 to tmpFilesToExtract.count do
		(
			local tmpFname = tmpFilesToExtract[i] + ".tga"
			append theFilesToExtract tmpFname
		)
		

		bf_RFA_extractFnamesMod theFilesToExtract "menu" bf_modPaths
		
		-- Apply alpha transparency info to color channels
		for i=1 to tmpFilesToExtract.count do
		(
			local tmpFname = g_TEMP_BF + tmpFilesToExtract[i] + ".tga"
			if (bfmdtOps.PrepBFicon tmpFname tmpFname) != 0 then
			(
				local tmpFname2 = g_TEMP_BF + tmpFilesToExtract[i] + ".dds"
				if (bfmdtOps.PrepBFicon tmpFname tmpFname2) != 0 then
				(
					--format "NO ICON Conversion: %\n" tmpFname
				)
			)
		)
		
	)



	------------------
	-- Loading of the GUI window, Mod already loaded
	--
	fn Load1B =
	(
		-- Show/Hide controls of the GUI
		setActiveState()
		
		if bf_ve_isODB_loaded() != true then return()
		
		label_ModName.text = g_bfCurrentMod as string


		-- VehicleType (Drop Down List)
		-- 
		(
			local tmpVehicleTypes_all = #("ALL")
			local tmpVehicleTypes = #()
			local t_veType = ""
			for i=1 to g_bfodb_index_vh.count do
			(
				t_veType = g_bfodb_index_vh[i][3]
				
				-- NOTE: VehicleType string is case sensitive!
				if (findItem tmpVehicleTypes t_veType) == 0 then
					append tmpVehicleTypes t_veType
			)
			
			join tmpVehicleTypes_all (sort_stringArray tmpVehicleTypes)		
			
			ddl_SubType.items = tmpVehicleTypes_all
		)
		
		refresh_objList()
	)
	
	
	
	
	
	
	--------------------------
	-- Loading database
	fn Load1 =
	(	
		Load1B()
		
		if chkPreview.checked then
		(
			extractPreviewImages()
			cbx_Obj_selected cbx_Obj.selection
		)
	)


	fn modSet_fn2 =
	(	
		local strModInitCon = g_PathBfDir + "mods\\" + g_bfCurrentMod + "\\init.con"
		if not ( doesThisFileExist strModInitCon ) then
		(
			setActiveState()
			return()
		)				

		setBfSettingB "g_bfCurrentMod" g_bfCurrentMod
		bf_loadModpaths()
		
		label_ModName.text = g_bfCurrentMod as string
		
		
		-- load this mod's ODB
		bfODB_LoadMod strModInitCon g_PathBfDir
		-- format " mod: % %\n" strModInitCon g_PathBfDir
		
		Load1()

		setActiveState()
	)


	-- Vehicle name is selected
	--
	fn cbx_Obj_selected =
	(
		local tmpBfdirT = ((getDir #maxRoot)+ "TEMP_BF\\")

		-- refresh preview picture
		if chkPreview.checked then
		(
			-- clear the image first		
			local bDidFindFile = false

			local itemID = 0
			local obj_name = cbx_obj.selected
			
			for i=1 to g_bfodb_index_vh.count do
			(
				if g_bfodb_index_vh[i][1] == obj_name then
				(
					itemID = i
					exit
				)
			)
			
			if (itemID > 0) then
			(
				local tmpImageName = g_bfodb_index_vh[itemID][2]
				if Classof tmpImageName == String then
				(
					tmpImageName = tmpBfdirT + "menu/texture/" + tmpImageName
					local tmpImageName2 = (getFilenamepath tmpImageName) + (getFilenameFile tmpImageName)
					if (getFiles (tmpImageName2 + ".tga")).count == 0 then -- file doesn't exist
					(
						if (getFiles (tmpImageName2 + ".dds")).count != 0 then
						(
							bmp1.filename = tmpImageName2 + ".dds"
							bDidFindFile = true
						)
					)
					else 
					(	
						bmp1.filename = tmpImageName2 + ".tga"
						bDidFindFile = true
					)
				)
			)
			
	
			
			if bDidFindFile == false then
				bmp1.filename = g_BFDIR_BIN + "NoImage.tga" 
		
		)
		else
			bmp1.bitmap = bitmap 128 128 color:gray
	
	)
	
	
	
	
	
	----------------------
	fn LOD_sublist_refresh_GUI =
	(
		local lbxLODb_items = #()
		
		local inx = lbxLODa.selection
		if inx > 0 then
		(
			append lbxLODb_items "ALL"

			local tmpChildren = r_getChildrenAsArray g_BfSceneLODs[inx][2]
			local dispID = g_BfSceneLODs[inx][3]

			for i=1 to tmpChildren.count do
			(
				append lbxLODb_items tmpChildren[i].name
			)
			
			if dispID <= tmpChildren.count then
			(	
				lbxLODb.selection = dispID + 1
			)
		)
		
		lbxLODb.items = lbxLODb_items
	
	)
	
	fn LOD_refresh_GUI defaultDispID:1=
	(
		g_BfSceneLODs = #()
		
		local lbxLODa_items = #()
		for i=1 to objects.count do
		(
			local tmpLODselName = bfGetLodSelectorName objects[i].name
			if tmpLODselName.count > 0 then
			(	
				-- #( lodSelectorName, Object_Node, DisplayChildID )
				-- DisplayChildID: 0 = ALL, 1 = first, 2 = second, 3 = third, ...

				append g_BfSceneLODs #(tmpLODselName,objects[i], defaultDispID) 
				append lbxLODa_items objects[i].name
			)
		)
		
		lbxLODa.items = lbxLODa_items
		
		LOD_sublist_refresh_GUI()
	)
	
	fn LOD_CheckValid =
	(
		if classof g_BfSceneLODs != Array then
		(
			LOD_refresh_GUI()
			return 1;
		)
		
		for inx=1 to g_BfSceneLODs.count do
		(
			if not isValidNode g_BfSceneLODs[inx][2] then 
			(
				LOD_refresh_GUI()
				return 1;
			)
		) 
		return 0;
	)
	
	
	---------------------
	-- This will leave lods selected the way they are
	--
	fn LOD_refresh_GUI2=
	(	
	
		if LOD_CheckValid() != 0 then
		(
			return()
		)
		
		local sceneLodNodes = #()
		for i=1 to g_BfSceneLODs.count do
		(
			append sceneLodNodes g_BfSceneLODs[i][2]
		)
		
		local currentLodNode = 0
		local lbxLODa_items = #()
		for i=1 to objects.count do
		(
			local tmpLODselName = bfGetLodSelectorName objects[i].name
			if tmpLODselName.count > 0 then
			(	
				-- #( lodSelectorName, Object_Node, DisplayChildID )
				-- DisplayChildID: 0 = ALL, 1 = first, 2 = second, 3 = third, ...
				
				currentLodNode += 1
				if currentLodNode <= sceneLodNodes.count then
				(
					if sceneLodNodes[currentLodNode] == objects[i] then
					(
						if g_BfSceneLODs[currentLodNode][1] == tmpLODselName then
						(	
							append lbxLODa_items objects[i].name					
							continue
						)
					)
				)
			
				
				LOD_refresh_GUI()
				return()
			)
		)
		
		lbxLODa.items = lbxLODa_items
		
		LOD_sublist_refresh_GUI()
	)
	
	
	
	fn LOD_ApplyShowHide =
	(	
		
		undo off
		(
			with redraw off
			(
		
				LOD_CheckValid()
		
				local grandChildHide = #()
				for inx=1 to g_BfSceneLODs.count do
				(
					if inx == 1 then grandChildHide[inx] = false
					else grandChildHide[inx] = true
				)
				
				
				for inx=1 to g_BfSceneLODs.count do
				(		
					if grandChildHide[inx] == true then continue
					
					local thisLodNode = g_BfSceneLODs[inx][2]
					
					local tmpChildren = r_getChildrenAsArray thisLodNode
					local dispID = g_BfSceneLODs[inx][3]
					
					if dispID == 0 then
					(
						r_unHideObjs tmpChildren
							
						for n=inx+1 to g_BfSceneLODs.count do
						(
							if (r_isGrandParent g_BfSceneLODs[n][2] thisLodNode) != undefined then
							(
								grandChildHide[n] = false
							)
						)
		
					)
					else 
					(
						r_hideObjs tmpChildren
						if dispID <= tmpChildren.count then
						(
							r_unHideObjs tmpChildren[dispID]
							
							
							for n=inx+1 to g_BfSceneLODs.count do
							(
								if (r_isGrandParent g_BfSceneLODs[n][2] tmpChildren[dispID]) != undefined then
								(
									grandChildHide[n] = false
								)
							)
						)
					)
					
					
				)
			)
		)
		
	)
	
	----------------------
	
	
	
-------------------------
------- Events ----------
-------------------------
	
	
	
	----------
	
	on btnLODall pressed do
	(
		LOD_refresh_GUI defaultDispID:0
		LOD_ApplyShowHide()
	)
	
	on btnLOD1st pressed do
	(
		LOD_refresh_GUI()
		LOD_ApplyShowHide()
	)
	
	
	on lbxLODa selected arg do
	(	
		LOD_CheckValid()
		LOD_sublist_refresh_GUI()
	)
	
	on lbxLODb selected arg do
	(
		LOD_CheckValid()
		
		local inx = lbxLODa.selection
		if inx > 0 then
		(
			g_BfSceneLODs[inx][3] = arg - 1	
			LOD_sublist_refresh_GUI()
		
			LOD_ApplyShowHide()
		)
	)	
	----------
	
	
	
	
	on btn_SaveAll pressed do
	(
		bf_ve_DBUpdateFromScene()
		bfODB_SaveMod (g_PathBfDir+"mods/"+g_bfCurrentMod+"/archives/objects.rfa")
	)
	
	
	on btn_objSpawn pressed do
	(
		if cbx_obj.selection > 0 then
		(
			format "*Spawning: %\n" cbx_obj.selected
	
			local tmpUseMat = chk_OptTexures.checked
			bfSpawnObj_Name cbx_obj.selected scale:spn_OptScale.value useGeom:true useMat:tmpUseMat
			
			if tmpUseMat == true then
				extr_SceneMat_textures()

			
			btnLOD1st.pressed()
		)
	)


	on cbx_Obj selected arg do
	(
		cbx_Obj_selected()
	)
	on chkPreview changed state do
	(
		if state == true then
		(
			extractPreviewImages()
			
			-- display the selected object's image
			cbx_Obj_selected()
		)
		else
		(
			bmp1.bitmap = bitmap 128 128 color:gray
		)
	)
	
	
	on chkMobilePhysics changed state do
	(
		refresh_objList MobilePhysics:state
	)
	
	
	-----------------------
	-- Vehicle SubType Chosen
	on ddl_SubType selected arg do
	(
		refresh_objList()
		cbx_Obj_selected()
	)
	
	
	-----------------------------
	-- Set the Current Mod
	on btn_modSet pressed do
	(
		createDialog rbf_selectMod
		rbf_selectMod.setExitFn "rBF_objs.modSet_fn2()"
	)
	
	
	
	on rBF_objs close do
	(
		setBfSettingB "SMscale" (spn_OptScale.value as string)
		setBfSettingB "rVe_chk_OptTexures" (chk_OptTexures.checked as string)
		setBfSettingB "rVe_chkPreview" (chkPreview.checked as string)
		setBfSettingB "rVe_chkMobilePhysics" (chkMobilePhysics.checked as string)
		
		setBfSettingB "rVe_ddl_SubType" (ddl_SubType.selected as string)
	)
	
	
	
	on rBF_objs open do
	(	
		local tmpSetting = (getBfSetting "SMscale") as float
		if tmpSetting > 0.009 then
		(	
			spn_OptScale.value = tmpSetting

			chk_OptTexures.checked = str2bool (getBfSetting "rVe_chk_OptTexures")
			chkPreview.checked = str2bool (getBfSetting "rVe_chkPreview")
			chkMobilePhysics.checked = str2bool (getBfSetting "rVe_chkMobilePhysics")
			

		)
		
		if bf_ve_isODB_loaded() == true then
		(
			Load1b()

			LOD_refresh_GUI2()
			
			local lastSubTypeName = getBfSetting "rVe_ddl_SubType"
			if lastSubTypeName.count > 0 then
			(
				local tmpItems = ddl_SubType.items
				local findID = findItem tmpItems lastSubTypeName 
				if findID > 0 then
				(	
					ddl_SubType.selection = findID
					refresh_objList()
				)
			)
			
			cbx_Obj.selection = 1
			cbx_Obj_selected()
		)
		
		setActiveState()
	)
)








-- createDialog rBF_objs

