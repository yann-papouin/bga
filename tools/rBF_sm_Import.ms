rollout rBF_sm_Import "Mesh Import" height:500  -- width:570 
(
-----------------------------------------------------------------------
-----------------------------------------------------------------------
	local changeRoll
	fn ckbExportPressed =
	(	changeRoll rBF_sm_Export
	
	)	
	-- Top Tabs
	include "rBFi_topHeader2.ms"
	
	label labInvisi "" pos:[60,335]

-----------------------------------------------------------------------
-----------------------------------------------------------------------
	groupBox grp1 "Mesh Import" pos:[349,108] width:172 height:224
	button showInfoButton  "Get polycount of .sm" pos:[78,112] width:123 height:22 tooltip:"Displays information about a .sm file in the listbox below"
	button smImportBtn "IMPORT..." pos:[387,290] width:92 height:22 tooltip:"Imports a StandardMesh (.sm) file with the options you have set."
	checkbox gonnaskip2last "Only Main Vis LOD" pos:[370,129] width:124 height:16 checked:true
	spinner sclitdown " Scale % " pos:[412,269] width:78 height:16 enabled:true range:[0.01,10000,100] type:#float scale:1
	checkbox impOptBBox "BoundingBox" pos:[389,185] width:85 height:16
	checkbox impOptCol "Collision" pos:[389,204] width:85 height:16 checked:true
	checkbox impOptVis "Visible" pos:[389,223] width:85 height:16 checked:true
	checkbox impOptShad "Shadow" pos:[389,243] width:85 height:16 checked:true
	listbox lbxMeshInfo "" pos:[64,149] width:229 height:12
	GroupBox grp3 "Mesh Information" pos:[44,80] width:270 height:252
	button btnClearInfo "Clear" pos:[237,113] width:40 height:22 toolTip:"Clears the contents of the listbox below."
	checkbox chkNoAttach "Detach Materials" pos:[370,145] width:111 height:16
	checkbox chkQuickImport "Re-Weld (Not perfect)" pos:[370,161] width:144 height:16 enabled:true checked:false

-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
	on rBF_sm_Import open do
	(	
		local tmpVal = (getBfSetting "COL_sclitdown") as float
		if tmpVal > 0.02 then 
		(
			sclitdown.value = tmpVal 
			gonnaskip2last.checked = str2bool (getBfSetting "COL_gonnaskip2last")
			impOptBBox.checked = str2bool (getBfSetting "COL_BoundingBox")
			impOptCol.checked = str2bool (getBfSetting "COL_Collision")
			impOptVis.checked = str2bool (getBfSetting "COL_Visible")
			impOptShad.checked = str2bool (getBfSetting "COL_Shadow")
			chkNoAttach.checked = str2bool (getBfSetting "COL_chkNoAttach")
			chkQuickImport.checked = str2bool (getBfSetting "vis_chkQuickImport")
		)
	)
	on rBF_sm_Import Close do
	(
		setBfSettingB "COL_gonnaskip2last" (gonnaskip2last.checked as string)
		setBfSettingB "COL_sclitdown" (sclitdown.value as string)

		setBfSettingB "COL_BoundingBox" (impOptBBox.checked as string)
		setBfSettingB "COL_Collision" (impOptCol.checked as string)
		setBfSettingB "COL_Visible" (impOptVis.checked as string)
		setBfSettingB "COL_Shadow" (impOptShad.checked as string)
		setBfSettingB "COL_chkNoAttach" (chkNoAttach.checked as string)
		setBfSettingB "vis_chkQuickImport" (chkQuickImport.checked as string)
	)


-- IMPORT BUTTON PRESSED
	on smImportBtn pressed do
	(
		local bSomethingChecked = false
		if impOptBBox.checked then bSomethingChecked = true
		if impOptCol.checked then bSomethingChecked = true
		if impOptVis.checked then bSomethingChecked = true
		if impOptShad.checked then bSomethingChecked = true

		-- if nothing checked, then check the Visible model portion for the user
		if not bSomethingChecked then
		(
			impOptVis.checked = true
		)
		
		local DSMscale = sclitdown.value * 0.1
		local fname = getOpenFileName caption:"Import .sm"  types:" .sm|*.sm| .rfa|*.rfa"
		if fname != undefined then
		(
			--
-- Bounding Box
			if impOptBBox.checked then
			(
				local f = bf_fopen fname "rb"
				if f != undefined then
				(
					bf_fseek f 8 #seek_set
					tmp1 = (bf_ReadFloat f)*DSMscale
					tmp2 = (bf_ReadFloat f)*DSMscale
					tmp3 = (bf_ReadFloat f)*DSMscale
					tmp4 = (bf_ReadFloat f)*DSMscale
					tmp5 = (bf_ReadFloat f)*DSMscale
					tmp6 = (bf_ReadFloat f)*DSMscale
					local boundingBox = box length:(tmp6-tmp3) width:(tmp4-tmp1) height:(tmp5-tmp2)
					boundingBox.name = "bbox_" + (lowercase (getFilenameFile fname))
					boundingBox.pos = [((tmp4-tmp1)*0.5+tmp1) , ((tmp6-tmp3)*0.5+tmp3) , tmp2]
					convertToMesh boundingBox
					bf_fclose f
				)
			)
-- Collision
			if impOptCol.checked then
			(
				-- importSMcollision fname gonnaskip2last.checked DSMscale
				importSMcollision fname false DSMscale
			)
--- Visible LODs
			if impOptVis.checked then
			(
				val32 = sclitdown.value
				readSMVisMesh fname DSMscale gonnaskip2last.checked NoAttach:chkNoAttach.checked weldSameNormals:(chkQuickImport.checked)
			)
--- Shadow
			if impOptShad.checked then
			(
				pre_importShadow fname DSMscale
			)
		)
	)
	
	on btnClearInfo pressed do
	(	lbxMeshInfo.items = #()
	)
	on showInfoButton pressed do
	(
		local fname = getOpenFileName caption:"Import .sm" types:" .sm|*.sm|"
		if fname != undefined then
		(
			lbxMeshInfo.items = #()
			lbxMeshInfo.items = getSMpolyCount fname returnText:true
		)
	)


	

	
)






