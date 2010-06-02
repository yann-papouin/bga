-- 


rollout rBF_tm_Export "Tree-Mesh (.tm)" height:384  -- width:570 
(
/*	-- local uviconsI = (getdir #ui + "/icons/uvtools_24i.bmp")
	local TiconsI = ("tools_24i.bmp")
	local TiconsA = undefined
	local TiconsCount = 26
	--images:#(TiconsI , TiconsA , TiconsCount, 1, 2, 3, 4)
*/

-----------------------------------------------------------------------
-----------------------------------------------------------------------
	-- Top Tabs
	include "rBFi_topHeader.ms"
--	ckbImport.visible = false
	
-----------------------------------------------------------------------
-----------------------------------------------------------------------
	GroupBox grp1 "Tree Mesh Import" pos:[367,117] width:160 height:109
	button BtnImport "IMPORT..." pos:[399,186] width:92 height:22 toolTip:"Imports a TreeMesh (.tm) geometry file into 3dsmax"
	spinner spnScaleImport " Scale % " pos:[423,155] width:76 height:16 enabled:true range:[0.01,10000,100] type:#float scale:1
	GroupBox grp10 "Tree Mesh Export" pos:[32,72] width:304 height:271
	button exportTreemeshBtn "Export .tm" pos:[221,302] width:99 height:31 toolTip:"Exports a TreeMesh geometry file"
	button rendTreeBil "Render Billboard" pos:[392,308] width:103 height:22 toolTip:"Renders the selected object to a billboard image. The image must be saved as the same name used for the .tm file."
	button tremmeshHelp "?" pos:[308,84] width:22 height:20 enabled:true toolTip:""
	spinner spn7 "Resolution " pos:[429,286] width:76 height:16 enabled:true range:[0,512,128] type:#integer scale:16
	GroupBox grp11 "Billboard Rendering" pos:[367,252] width:160 height:90
	checkbox chk3matID "Force Material ID" pos:[206,146] width:106 height:17 checked:true
	spinner spn7matID "Material ID:" pos:[253,176] width:55 height:16 enabled:true range:[0,65535,166] type:#integer scale:1
	GroupBox grp12 "Material Effect IDs" pos:[191,121] width:130 height:95
	pickbutton btnCOL "Pick Collision" pos:[56,120] width:95 height:22 enabled:true toolTip:"Pick a collision mesh from scene. Used in hit detection with other objects in game including bullets and vehicles"
	button btnCOL_X "X" pos:[157,120] width:19 height:22 enabled:true toolTip:"Clear"
	pickbutton btnBranch "  Add Branch" pos:[57,145] width:119 height:22 enabled:true toolTip:"Pick geometry to add as a branch (Displayed in a way that works well at simulating leaves)"
	pickbutton btnTrunk "Add Trunk" pos:[57,170] width:119 height:22 enabled:true toolTip:"Pick geometry to add as a trunk (Solid shapes that have no transparency effects)"
	pickbutton btnSprite "Add Sprite" pos:[57,195] width:120 height:22 enabled:true toolTip:"Pick geometry to add a sprite (These have special properties suited to render as several leaves)"
	listbox lbxExport "" pos:[48,234] width:158 height:7 enabled:true
	button btnRemItem "Remove Item" pos:[223,234] width:80 height:22 enabled:true toolTip:"Removes the selected item from the listbox"
	spinner spnExport "Scale % " pos:[245,279] width:76 height:16 enabled:true range:[0.01,10000,100] type:#float scale:1
	button btnCreateSprite "Create Sprite" pos:[199,93] width:89 height:20
	checkbox chkNameBased "Name Based Export" pos:[56,96] width:119 height:18 enabled:true checked:true

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
	local theMeshArray = #( #(), #(), #(), #(), #() )
	local theItemMeshArray = #()
-------------------------------	
	fn toggleNameBased state =
	(
		btnCOL.enabled = state
		btnCOL_X.enabled = state		
		btnBranch.enabled = state
		btnTrunk.enabled = state
		btnSprite.enabled = state
		btnRemItem.enabled = state
	)
	
	on rBF_tm_Export open do
	(
		if (g_BFisGmax) then
		(
			rendTreeBil.enabled = false
			spn7.enabled = false
			grp11.enabled = false
		)
		local tmpSetting = (getBfSetting "tree_spnScaleImport") as float
		if tmpSetting > 0.009 then
		(	spnScaleImport.value = tmpSetting
			
			
			spn7matID.value = (getBfSetting "tree_spnmatID") as float
			spnExport.value = (getBfSetting "tree_spnExport") as float		
			
			spn7.value = (getBfSetting "tree_spn7") as float
			chk3matID.checked = str2bool (getBfSetting "tree_chk3matID")
			
			spn7matID.enabled = chk3matID.checked
			
			chkNameBased.checked = str2bool (getBfSetting "tree_chkNameBased")
		)
		toggleNameBased (not chkNameBased.checked)
	)
	on rBF_tm_Export Close do
	(
		setBfSettingB "tree_spnmatID" (spn7matID.value as string)
		setBfSettingB "tree_spnExport" (spnExport.value as string)
		setBfSettingB "tree_spnScaleImport" (spnScaleImport.value as string)
		setBfSettingB "tree_spn7" (spn7.value as string)
		
		setBfSettingB "tree_chk3matID" (chk3matID.checked as string)
		setBfSettingB "tree_chkNameBased" (chkNameBased.checked as string)
	)
		

	on chkNameBased changed state do
	(
		toggleNameBased (not state)
	)
	
	on btnCreateSprite pressed do
	(
		TM_CreateSprite()
	)

	on btnBranch picked obj do
	(
		append theMeshArray[2] obj
		local tmpArray = lbxExport.items
		append tmpArray ( "B: " + (obj.name as string) )
		lbxExport.items = tmpArray
		append theItemMeshArray #(2, theMeshArray[2].count)
	)
	on btnTrunk picked obj do
	(
		append theMeshArray[3] obj
		local tmpArray = lbxExport.items
		append tmpArray ( "T: " + (obj.name as string) )
		lbxExport.items = tmpArray
		append theItemMeshArray #(3, theMeshArray[3].count)
	)
	on btnSprite picked obj do
	(
		append theMeshArray[4] obj
		local tmpArray = lbxExport.items
		append tmpArray ( "S: " + (obj.name as string) )
		lbxExport.items = tmpArray
		append theItemMeshArray #(4, theMeshArray[4].count)
		
	)
	
	on btnRemItem pressed do
	(
		if lbxExport.selection > 0 then
		(
			local tmpA = theItemMeshArray[lbxExport.selection]
			
			deleteItem theMeshArray[tmpA[1]] tmpA[2]
	
			-- setNew values for theItemMeshArray to reflect changes	
			for i=1 to theItemMeshArray.count do
			(
				if theItemMeshArray[i][1] == tmpA[1] then
				(
					if theItemMeshArray[i][2] > tmpA[2] then
					(
						theItemMeshArray[i][2] -= 1
					)
				)
			)
			deleteItem theItemMeshArray lbxExport.selection
			
			local tmpArray = lbxExport.items
			deleteItem tmpArray lbxExport.selection
			lbxExport.items = tmpArray
		)
	)
	
		
	on chk3matID changed state do
	(
		spn7matID.enabled = state
	)
	
	on rendTreeBil pressed do
	(
		if selection[1] != undefined then
		(
			renderBillboard selection "c:\\test.bmp" 8 (spn7.value)
		)
		else messageBox "Error! Must have an object selected!"
	)

	on btnCOL picked obj do
	(
		theMeshArray[1][1] = obj
		btnCOL.text = ("C: " + (obj.name as string))
		btnCOL_X.enabled = true
	)
	on btnCOL_X pressed do
	(
		theMeshArray[1] = #()
		btnCOL.text = "Pick Collision"
		btnCOL_X.enabled = false
	)
	
	
	on tremmeshHelp pressed do
	(	local msgTxt = ""
		msgTxt += "Beginner's instructions:\n"	
		msgTxt += "\n"
		msgTxt += " 1) Press Add Trunk\n"
		msgTxt += " 2) Pick a mesh in the scene to be your trunk  \n"
		msgTxt += " 3) Press EXPORT .tm\n"
		msgTxt += "\n"
		msgTxt += "Note: Each mesh you add may contain only 1 material \n"
		msgTxt += "\n"
		msgTxt += "Node \"name based export\" works with these names: \n"
		msgTxt += "collision = collision mesh (only one of these)\n"
		msgTxt += "trunk_01 = solid meshes\n"
		msgTxt += "branch_01 = see through meshes\n"
		msgTxt += "sprite_01 = planes that face the camera (create sprite with the supplied button)\n"
		msgTxt += "\n"
		messagebox msgTxt title:"Help with .tm exporting" beep:false
	)
		
		
	on exportTreemeshBtn pressed do
	(	
		local hasSomethingToExport = false
		
		if chkNameBased.checked then
		(
			theMeshArray = treemeshParseNames geometry
		)
		
		for i=1 to theMeshArray.count do
			if theMeshArray[i].count > 0 then hasSomethingToExport = true

		
		if hasSomethingToExport then
		(
			local fname = getSaveFileName types:"TreeMesh(*.tm)|*.tm|All|*.*|"
			if fname != undefined then
			(
				local tmpMatIDval = -1	
				if chk3matID.checked then tmpMatIDval = spn7matID.value
				
				local tmpGeom = #( #(), #(), #(),#(),#())
				for i=1 to theMeshArray.count do
				(	for j=1 to theMeshArray[i].count do
					(	if (isValidNode theMeshArray[i][j]) then
						(
							local tmpObj = copy theMeshArray[i][j]
							append tmpGeom[i] tmpObj
							if ((classof tmpObj) != Editable_mesh) then convertToMesh tmpObj
						)
					)
				)
			
				exportTM fname tmpGeom (spnExport.value * 0.001) tmpMatIDval
				
				for i=1 to tmpGeom.count do
				(	for j=1 to tmpGeom[i].count do
						delete tmpGeom[i][j]
				)
				
			)
		)
		else 
		(
			if chkNameBased.checked then
				Messagebox "There is nothing to export.\nPlease name your meshes according to what you want them to export as.\n(Collision, branch, trunk, or sprite)" title:"Error"
			else
			(
				Messagebox "There is nothing to export.\nPlease press the add trunk/branch and try again." title:"Error"
			)
			
			tremmeshHelp.pressed()
		)
	)
	
	on BtnImport pressed do
	(	
		local fname = getOpenFileName types:"TreeMesh(*.tm)|*.tm|All|*.*|"
		if fname != undefined then 
			importTM fname (spnScaleImport.value * 0.1) doCollision:true
	)


	
)






