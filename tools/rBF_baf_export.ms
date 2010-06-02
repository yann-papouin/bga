Persistent Global BAFBatch_Browsetxt

rollout rBF_baf_Export "Animation (.baf)" width:570 height:360
(
-----------------------------------------------------------------------
-----------------------------------------------------------------------
	include "rBFi_topHeader.ms"

-----------------------------------------------------------------------
-----------------------------------------------------------------------

	edittext prefixTxt "Prefix " pos:[156,49] width:132 height:20
	edittext anmName "Name " pos:[154,79] width:159 height:20
	GroupBox grp1 "Animation Sequences" pos:[33,106] width:497 height:240
	button batchBtn "Generate Sequence" pos:[398,295] width:115 height:30 toolTip:"Export .baf file"
	button AddAnm "Add" pos:[51,220] width:68 height:22
	button RemAnm "Remove" pos:[124,219] width:68 height:22 enabled:false
	button ClearBtn "Clear" pos:[51,246] width:142 height:22 enabled:false
	button LoadBtn "Load Sequence" pos:[51,305] width:142 height:22
	button saveBtn "Save Sequence" pos:[51,279] width:142 height:22 enabled:false
	checkbox chkUseActiveSegment "Active Time Segment" pos:[392,122] width:121 height:16
	spinner startFrame "Start " pos:[216,140] width:64 height:16 range:[-100,1000,0] type:#integer scale:1
	spinner endFrame "End " pos:[309,140] width:62 height:16 range:[-100,10000,100] type:#integer scale:1
	listbox listBx "" pos:[209,170] width:166 height:12 enabled:true
	edittext browseTxt "" pos:[396,219] width:109 height:20
	progressBar pb1 "ProgressBar" pos:[407,269] width:96 height:8 color:(color 30 10 190)
	label lbl4 "Progress" pos:[408,253] width:46 height:15
	button BrowseBtn "Browse" pos:[462,200] width:45 height:16
	GroupBox grp2 "Output" pos:[387,171] width:134 height:165
	label lbl6 "Directory" pos:[402,201] width:46 height:17
	button btnExportSingleBAF "Export Single .baf" pos:[400,141] width:98 height:22
	button btnImportBaf "Import .baf" pos:[37,48] width:80 height:21
	checkbox chkIsWeapon "Is Weapon" pos:[300,51] width:80 height:16
	radioButtons radioBoneSet "" pos:[69,130] width:98 height:64 columns:1 labels:#("1p Upper", "3p Upper", "3p Lower", "Selected Bones") default:4
	groupBox grp3 "Sequence" pos:[43,199] width:158 height:135
	spinner spnScale "Scale % " pos:[71,80] width:60 height:16 range:[0.1,10000,100] scale:1
	
	
	fn remInvalidChars tmpStr =
	(
		local validChars = "01234567890_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
		local outStr = ""
		for i=1 to tmpStr.count do
			if (findString validChars tmpStr[i]) != undefined then outStr = outStr + tmpStr[i]
		return outStr 
	)
	fn remDotFromTxt tmpTxt =
	(	local foundInx = (findstring tmpTxt ".")
		if foundInx != undefined then
			return (substring tmpTxt 1 (foundInx - 1))
		else return tmpTxt
	)
	fn bafAnName =
	( 	return ((remInvalidChars (remDotFromTxt anmName.text)) + " (" + (startFrame.value as string) + \
		 "-" + (endFrame.value as string) + ")" )
	)
	
	fn decodeTxtVal itemTxt=
	(
		local tmpTxt = filterString itemTxt "(-)"
		tmpTxt[1] = substring tmpTxt[1] 1 (tmptxt[1].count - 1)
		tmpTxt[2] = tmpTxt[2] as integer
		tmpTxt[3]= tmptxt[3] as integer
		return tmpTxt
	)
	fn refreshStates1 =
	(
		if listbx.items.count > 0 then
		(				
			ClearBtn.enabled = true
			saveBtn.enabled = true
			
			if listbx.selection > 0 then
				RemAnm.enabled = true
		)
		else
		(
			ClearBtn.enabled = false
			saveBtn.enabled = false
			RemAnm.enabled = false
		)
	)
	
-----------------------------------
	on startFrame changed var1 do
	(
		if endframe.value < var1 then
			endframe.value = var1
	)
	on EndFrame changed var1 do
	(
		if var1 < startframe.value then
			startframe.value = var1
	)
-----------------------------------
	on btnImportBaf pressed do
	(
		if objects.count == 0 then
		(
			messagebox "Warning! You should have bone objects in scene\nbefore importing the (.baf) animation file." title:"Warning!"
		)
		
		local fname = getOpenFileName caption:"Import .baf" types:"Animation File .baf|*.baf|All|*.*|"
		if fname != undefined then
		(
			readBafFile fname objects (spnScale.value * 0.1) true isWeapon:chkIsWeapon.checked --(not bafDebugMode.checked)
		)
		
	)
--------------------------------
	on listBx selected tmpArg do
	(	
		local tmpTxta = decodeTxtVal listbx.items[tmpArg]
		
		-- TODO: look for prefix match
		
		anmName.text = tmpTxta[1]
		startFrame.value = tmpTxta[2]
		endframe.value = tmptxta[3]
		
		refreshStates1()
	)
	on chkUseActiveSegment changed state do
	(
		startFrame.enabled = not state
		endFrame.enabled = not state
	)
	
	on btnExportSingleBAF pressed do
	(
		if radioBoneSet.state == 0 then
			radioBoneSet.state = 4
		
		case radioBoneSet.state of
		(
			1: bf_AnimSelectBones #1p_upper
			2: bf_AnimSelectBones #3p_upper
			3: bf_AnimSelectBones #3p_lower
		)
		
		if selection.count > 0 then
		(

				local fname = getSaveFileName caption:"Export .baf" types:"Animation File .baf|*.baf|All|*.*|"
				if fname != undefined then
				(
					local segStart=startframe.value , segEnd = endframe.value 
					if chkUseActiveSegment.checked then
					(	segStart = 0 + animationRange.start
						segEnd = 0 + animationrange.end
					)
					
					local bIsWeap = chkIsWeapon.checked
					if radioBoneSet.state < 4 then bIsWeap = false
	
					WriteBafFile fname selection segStart (segEnd - segStart + 1) (spnScale.value * 0.001) isWeapon:bIsWeap
				)
		)
		else
		(
			case radioBoneSet.state of
			(
				1: messagebox "ERROR! Player Skeleton bones not found (1P Upper)!\nIf you wish to export the selected bones only, please choose the Selected Bones radio button"
				2: messagebox "ERROR! Player Skeleton bones not found (3P Upper)!\nIf you wish to export the selected bones only, please choose the Selected Bones radio button"
				3: messagebox "ERROR! Player Skeleton bones not found (3P Lower)!\nIf you wish to export the selected bones only, please choose the Selected Bones radio button"
				4: messagebox "ERROR! There are no bones selected for export!"
				default: messagebox "ERROR! There are no bones ready for export!"
			)
		)
	)
	
	on addAnm pressed do
	(	
		if anmName.text.count > 0 then
		(
			local tmpArray = listbx.items
			local tmpName = bafanName()
			if (findItem tmparray tmpName) == 0 then
			(	append tmpArray (prefixTxt.text + tmpName)
				listbx.items = tmpArray
				endframe.value += 2
				startFrame.value = endFrame.value - 1
				
			)
		)
		else messagebox "Give the sequence a name first!"
		
		refreshStates1()
	)
	
	on RemAnm pressed do
	(	
		if listbx.selection > 0 then
		(
			local tmpArray = listbx.items
			deleteItem tmpArray listbx.selection
			listbx.items = tmpArray
		)
		refreshStates1()
	)
--------------------------------	
	on saveBtn pressed do
	(	
		refreshStates1()
		
		local fname = getSaveFileName types:"List(*.bax)|*.bax|All|*.*|"
		if fname != undefined then
		(	local f = bf_fopen fname "w"
			if f != undefined then
			(	local f2 = stringStream ""
				local tmpArray = listbx.items
				for i=1 to tmpArray.count do
					format "%\n" tmpArray[i] to:f2
				bf_writeString2 f f2
				bf_fClose f
			)
			else messagebox "ERROR! Could not create file"
		)
		else return false
	)
	
	on clearBtn pressed do
	(	if listbx.items.count > 0 then
		(	if (queryBox "Are you sure you want to clear the list?" title:"Clear list items") then
			(	listbx.items = #()
			)
		)
		refreshStates1()
	)
	
	on loadBtn pressed do
	(
		local fname = getOpenFileName types:"List(*.bax)|*.bax|All|*.*|"
		if fname != undefined then
		(
			local f = memStreamMgr.openFile fname
			if f != undefined then
			(
				local tmpArray = #()
				while true != f.eos() do
				(
					local tmpLine = f.ReadLine()
					append tmpArray tmpLine 
				)
				memStreamMgr.close f
				listbx.items = tmpArray
			)
			else messagebox "ERROR! Could not open file"
		)
		refreshStates1()
	)
--------------------------------
	on BrowseBtn pressed do
	(
		local tmpsavePath = getSavePath "Output Directory"
		if tmpsavePath != undefined then
			browseTxt.text = tmpsavePath
	)	
	
	on batchBtn pressed do
	(
		batchBtn.enabled = false
		local tmparray = listbx.items
		if tmparray.count > 0 then
		(
			if selection.count > 0 then
			(
				if browsetxt.text.count < 3 then
				(	BrowseBtn.pressed()
					if browsetxt.text.count < 3 then
					(	batchBtn.enabled = true
						return()
					)
				)
				
				local browsetxt_text = makeTrailingSlash browsetxt.text
			
				local bIsWeap = chkIsWeapon.checked
				if radioBoneSet.state < 4 then bIsWeap = false
					
				local multpl = 100.0 / tmpArray.count
				for i=1 to tmparray.count do
				(	pb1.value = i*multpl
					
					local tmpTxta = decodeTxtVal listbx.items[i]
					format "%\n" (browsetxt_text + tmpTxta[1] + ".baf")
					
					
					WriteBafFile (browsetxt_text + tmpTxta[1] + ".baf") selection tmpTxta[2] tmptxta[3] (spnScale.value * 0.001) isWeapon:bIsWeap
				)
				pb1.value = 0
			)
			else messagebox "ERROR! Select the bones that are animated and ready to be saved."
		)
		batchBtn.enabled = true
	)
	fn bafOkToClose =
	(
		if listbx.items.count > 0 then
		(	local tmpA = (yesNoCancelBox "Would you like to save the .baf list before closing?" title:"Save before closing?")
			if tmpA == #yes then
			(	return ( saveBtn.pressed() )
			)
			else if tmpA == #cancel then return false
		)
		return true
	)
	on rBF_baf_Export OkToClose do
	(
		bafOkToClose()
	)
	on rBF_baf_Export open do
	(

		local tmpSetting = (getBfSetting "ske_bnScale") as float
		if tmpSetting > 0.009 then
		(	spnScale.value = tmpSetting
		)
	
		local tmpTrue = str2bool (getBfSetting "chkUseActiveSegment")
		chkUseActiveSegment.changed tmpTrue
		chkUseActiveSegment.checked = tmpTrue
		
		chkIsWeapon.checked = str2bool (getBfSetting "chkIsWeapon")
		
		radioBoneSet.state = (getBfSetting "radioBoneSet") as Integer
		
		if radioBoneSet.state == 0 then radioBoneSet.state = 4
		
		if BAFBatch_Browsetxt != undefined do
			browsetxt.text = BAFBatch_Browsetxt
	)
	
	on rBF_baf_Export Close do
	(
		setBfSettingB "chkUseActiveSegment" (chkUseActiveSegment.checked as string)
		setBfSettingB "chkIsWeapon" (chkIsWeapon.checked as string)
		setBfSettingB "radioBoneSet" (radioBoneSet.state as string)
		
		
		setBfSettingB "ske_bnScale" (spnScale.value as string)
		
		BAFBatch_Browsetxt = browsetxt.text
	)


)
