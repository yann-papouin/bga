-----------------------------------------------
-----------------------------------------------
-----------------------------------------------
global g_PathBfDir
global rbf_selectMod
global g_bfCurrentMod
global rBf_pickGame
global rBF_objs

fn bfGetInstallDir2 =
(
	local fname = getOpenFileName caption:"Find Battlefield Game EXE" types:"*.exe|*.exe|All|*.*|"
	if fname != undefined then
	(
		local tmp_PathBfDir = getFilenamePath fname

		
		if (getFiles (tmp_PathBfDir + "*.exe")).count == 0 then
		(
			MessageBox "No .exe files at chosen location!\n"
			return false
		)
		
		g_PathBfDir = tmp_PathBfDir
		setBfSettingB "g_PathBfDir" g_PathBfDir
	)
	else
	(
		return false
	)

	return true
)

rollout rBf_pickGame "Change Game" width:280 height:136
(
	dropDownList ddl1 "Game:" pos:[24,16] width:184 height:5
	button btnOK "OK" pos:[48,88] width:80 height:24
	button BtnCancel "Cancel" pos:[152,88] width:80 height:24
	GroupBox grp1 "" pos:[8,0] width:264 height:128
	button btnBrowse "Browse" pos:[216,32] width:48 height:24
	
	local dir_bfV
	local dir_bfV_inx
	
	local dir_Bf
	local dir_bf_inx
		
	local myTmpText = ""
	fn setExitFn tmpTxt = myTmpText = tmpTxt as string
	
	on rBf_pickGame open do
	(
		dir_bfV_inx = 0
		dir_bf_inx = 0
		
		dir_bfV = (bfmdt_Reg_GetBfVietnamInstall())
		if dir_bfV.count > 0 then dir_bfV = makeTrailingSlash dir_bfV
		dir_bf = makeTrailingSlash (bfmdt_Reg_GetBf1942Install())
		if dir_bf.count > 0 then dir_bf = makeTrailingSlash dir_bf
		
		local tmpItems = #()
		if dir_bfV.count > 0 then
		(
			if (getFiles (dir_bfV + "*.exe")).count > 0 then
			(
				append tmpItems "Battlefield: Vietnam"
				dir_bfV_inx = tmpItems.count
			)
		)
		
		if dir_bf.count > 0 then
		(
			if (getFiles (dir_bf + "*.exe")).count > 0 then
			(
				append tmpItems "Battlefield: 1942"
				dir_bf_inx = tmpItems.count
			)
		)
		
		ddl1.items = tmpItems 
	)
	
	
	fn success_ret dir =
	(
		g_PathBfDir = copy dir
		setBfSettingB "g_PathBfDir" g_PathBfDir
		
		try
		(
			execute myTmpText
		)
		catch()
		
		bf_odb_sel = #("rBf_pickGame", true)
		destroyDialog rBf_pickGame	
	)
	
	on btnOk pressed do
	(
		if ddl1.selection > 0 then
		(
			local tmpDir = ""
			if dir_bfV_inx == ddl1.selection then tmpDir = dir_bfV
			else if dir_bf_inx == ddl1.selection then tmpDir = dir_bf
			
			if tmpDir.count > 0 then
				success_ret tmpDir
		)
	)
	
	on btnBrowse pressed do
	(
		if bfGetInstallDir2() then	
			success_ret g_PathBfDir
	)
	on btnCancel pressed do
	(
		bf_odb_sel = #("rBf_pickGame", false)
		DestroyDialog rbf_pickGame
	)
	
)



fn bfGetInstallDir txtCallBack:""=
(
	g_PathBfDir = makeTrailingSlash (getBfSetting "g_PathBfDir")

	if (getFiles (g_PathBfDir + "*.exe")).count == 0 then
	(
		createDialog rbf_pickGame modal:true
	)
	
	if (getFiles (g_PathBfDir + "*.exe")).count == 0 then	
		return false	
			
	return true
)


global g_bf_ve_doNotAllow_modNames = #("bf1942","xpack1","xpack2","bfvietnam")
rollout rbf_selectMod "Select a Mod" width:376 height:224
(
	listbox lbx1 "Select a mod:" pos:[16,40] width:240 height:10
	label lbl1 "" pos:[8,17] width:360 height:16
	button btnOK "OK" pos:[264,64] width:96 height:24 enabled:false
	button BtnCancel "Cancel" pos:[264,94] width:96 height:24
	button btnChangeGame "Change Game" pos:[264,140] width:96 height:24
	button btnCreateMod "Create Mod" pos:[264,169] width:96 height:24
	button btnRefresh "Refresh List" pos:[96,35] width:81 height:17
	
	local myTmpText = ""
	fn setExitFn tmpTxt = (myTmpText = tmpTxt as string)


	fn onOpenYes =
	(
		btnOK.enabled = false
		
		lbl1.text = g_PathBfDir as string
		
		local tmpArray = #()
		local tmpCount = (g_PathBfDir + "mods\\").count + 1
		local tmpFolders = getDirectories ( g_PathBfDir + "mods\\*" )
		for i=1 to tmpFolders.count do
		(
			local tmpModStr = substring tmpFolders[i] tmpCount (tmpFolders[i].count - tmpCount)
			local okToModThisMod = true
			for j=1 to g_bf_ve_doNotAllow_modNames.count do
			(
				if (matchPattern tmpModStr pattern:g_bf_ve_doNotAllow_modNames[j]) == true then
				(
					okToModThisMod = false
					exit;
				)
			)
			
			if okToModThisMod == true then
				append tmpArray tmpModStr
		)
		
		lbx1.items = tmpArray
		
		if tmpArray.count > 0 then
		(	lbx1.selection = 1
			btnOK.enabled = true
		)
	)
	
	on rbf_selectMod open do
	(
		if ( bfGetInstallDir() ) then
		(
			onOpenYes()
		)
		else
		(
			DestroyDialog rbf_selectMod
		)
	)
	
	on btnRefresh pressed do
	(
		btnOK.enabled = false
		onOpenYes()
	)
	
	on lbx1 selected arg do
	(
		if arg > 0 then
		(
			btnOK.enabled = true
		)
	)
	
	on lbx1 doubleClicked arg do
	(
		btnOK.pressed()
	)
	
	on btnOK pressed do
	(
		
		if lbx1.selection > 0 then
		(
			local tmpArray = lbx1.items
			g_bfCurrentMod = tmpArray[ lbx1.selection ]
		)
		
		bf_odb_sel = #("rbf_selectmod", true)

		try
		(
			execute myTmpText
		)
		catch()
		
		
		DestroyDialog rbf_selectMod
	)
	
	on BtnCancel pressed do
	(	
		-- close dialog
		bf_odb_sel = #("rbf_selectmod", false)
		DestroyDialog rbf_selectMod
	)
	
	on btnChangeGame pressed do
	(	
		createDialog rbf_pickGame modal:true
	
	 --	if (getFiles (g_PathBfDir + "*.exe")).count > 0 then
	--		onOpenYes()

		btnRefresh.pressed()
	)
	on btnCreateMod pressed do
	(
		local tmpBinDir2 = makeTrailingSlash (bfmdt_Reg_GetBfMDTInstall())
		if bf_ShellLaunch (tmpBinDir2 + "ModWizard\\ModWizard.exe") "" < 33 then
			MessageBox "The Mod Development Toolkit must be installed for this tool to work"
		else
		(
			Messagebox "Press OK after you have created your new mod"
			btnRefresh.pressed()
		)
	)
	
)


fn bfGetMod txtCallback:""=
(
	if ( bfGetInstallDir() == true ) then
	(
		g_bfCurrentMod = getBfSetting "g_bfCurrentMod"
				
		if not ( doesThisFileExist (g_PathBfDir + "mods\\" + g_bfCurrentMod + "\\init.con") ) then
		(		
			createDialog rbf_selectMod modal:true
			if doesThisFileExist (g_PathBfDir + "mods\\" + g_bfCurrentMod + "\\init.con") then
			(
				setBfSettingB "g_bfCurrentMod" g_bfCurrentMod
				
				return true
			)

			
			return false
		)		
		else 
			return true
		
		
	)
	return false
)
-----------------------------------------------
-----------------------------------------------
-----------------------------------------------

global bf_modPaths

fn bf_loadModpaths=
(
	bf_modPaths = #()
	
	if g_PathBfDir == undefined then
		return bf_modPaths
		
	if g_bfCurrentMod == undefined then
		return bf_modPaths	
	
	local initConFilePath = g_PathBfDir + "mods\\" + g_bfCurrentMod + "\\init.con"
	--format "init.con : %\n" initConFilePath
	
	local f = bf_ReadFileToBuffer initConFilePath
	if f != undefined then
	(
		while not (eof f) do
		(
			local tmpLine = readline f
			tmpLine = filterString tmpLine " \t"
			if tmpLine.count > 1 then
			(
				if (lowercase tmpLine[1]) == "game.addmodpath" then
				(
					append bf_modPaths tmpLine[2]
				)
			)	
		)
	)
	
	return bf_modPaths
)













-- createDialog rBf_pickGame modal:true