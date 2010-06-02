global bfmsloc = (getDir #scripts) + "\\Battlefield Tools\\"
global loadBfToolset
global bf_ShellLaunch
global bf_setINISetting
global bf_getINISetting

fn makeTrailingSlash inputStr =
(	
	local tmpCount = inputStr.count

	-- TODO: convert all slashes to face one way here
	
	local i = tmpCount
	while (i > 1) do
	(
		if inputStr[i] != "\\" and inputStr[i] != "/" then
			exit;
	
		
		i -= 1;
	)
	
	inputStr = (substring inputStr 1 i) + "\\"
	
	return inputStr
)
fn bfmdt_GetRegKey keyInfo =
(
	copy (bfmdtops.GetRegKey1 keyInfo[1] keyInfo[2] keyInfo[3])
)

fn bfmdt_Reg_GetBf1942Install =
(
	bfmdt_GetRegKey #("HKEY_LOCAL_MACHINE", "SOFTWARE\\EA GAMES\\Battlefield 1942", "GAMEDIR")
)
fn bfmdt_Reg_GetBfVietnamInstall =
(
	bfmdt_GetRegKey #("HKEY_LOCAL_MACHINE", "SOFTWARE\\EA GAMES\\Battlefield Vietnam", "GAMEDIR")
)
fn bfmdt_Reg_GetBfMDTInstall =
(
	local tmpKey = bfmdt_GetRegKey #("HKEY_LOCAL_MACHINE", "SOFTWARE\\EA GAMES\\MDT", "Dir")
	
	if tmpKey.count > 0 then
	(
		tmpKey = (MakeTrailingSlash tmpKey) -- + "Mod Development Tools"
	)
	
	return tmpKey
)

filein (bfmsloc+"rBFi_isGmax.ms")
-- if g_BFisGmax then filein (bfmsloc+"rBF_wrappers.mse")
-- else 
filein (bfmsloc+"rBF_wrappers.ms")


fn loadBfToolset arg =
(	local fi = bfmsloc + "rBfg_MAIN.ms";
	global bfLoadUpInto = arg
	filein fi;
)


fn doesThisFileExist filename =
(
	if classof filename == string then	
		if (getFiles filename).count > 0 then
			return true
	
	return false;
)

global g_bfMDTHelpLocation
fn getbfMDThelp =
(
	g_bfMDTHelpLocation = "http://www.planetbattlefield.com/MDT/tutorials/index.html"

	if bf_ShellLaunch g_bfMDTHelpLocation "" == undefined then
	(
		loadBfToolset 1
		bf_ShellLaunch g_bfMDTHelpLocation ""
	)

)





macroScript BF_Toolset_Mesh
category:"Battlefield: 1942 Editing" 
buttonText:"Model Import/Export"
tooltip:"Launch Bf toolset" 
( 	on execute do loadBfToolset 1
)
macroScript BF_Toolset_Animation
category:"Battlefield: 1942 Editing" 
buttonText:"Animation"
tooltip:"Launch Bf toolset" 
( 	on execute do loadBfToolset 2
)
macroScript BF_Toolset_SkeSkn
category:"Battlefield: 1942 Editing" 
buttonText:"Skeleton and Skin"
tooltip:"Launch Bf toolset" 
( 	on execute do loadBfToolset 3
)
macroScript BF_Toolset_Trees
category:"Battlefield: 1942 Editing" 
buttonText:"Tree Import/Export"
tooltip:"Launch Bf toolset" 
( 	on execute do loadBfToolset 4
)
macroScript BF_Toolset_Vehicles
category:"Battlefield: 1942 Editing" 
buttonText:"Vehicle Import/Export"
tooltip:"Launch Bf toolset" 
( 	on execute do loadBfToolset 5
)

macroScript BF_Help
buttonText:"Help"
category:"Battlefield: 1942 Editing" 
tooltip:"Launch Bf toolset Help" 
( 	on execute do
	(
		getbfMDThelp()
	)
)
macroScript BF_ModWizard
buttonText:"Mod Wizard"
category:"Battlefield: 1942 Editing" 
tooltip:"Launch Bf Mod Wizard" 
( 	on execute do
	( 	
		local tmpSHlLaunch = (makeTrailingSlash (bfmdt_Reg_GetBfMDTInstall())) + "ModWizard\\ModWizard.exe"		
		if bf_ShellLaunch tmpSHlLaunch "" < 33  then
		(	
			loadBfToolset 1
			if bf_ShellLaunch tmpSHlLaunch "" < 33 then
				MessageBox "The Mod Development Toolkit must be installed for this tool to work"
		)
	)
)
macroScript BF_RFA_Tools
buttonText:"RFA Tools"
category:"Battlefield: 1942 Editing" 
tooltip:"Launch Bf Mod RFA Tool" 
( 	on execute do 
	(
		local tmpSHlLaunch = (makeTrailingSlash (bfmdt_Reg_GetBfMDTInstall())) + "RFA Tools\\winRFA.exe"		
		if bf_ShellLaunch tmpSHlLaunch "" < 33 then
		(
			loadBfToolset 1
			if bf_ShellLaunch tmpSHlLaunch "" < 33 then
				MessageBox "The Mod Development Toolkit must be installed for this tool to work"
		)
	)
)

fn bfAddGUIMenu =
(
	local bfOkToCreateMenu = false
	
	if menuMan.registerMenuContext 0xd3e56c62 then 
		bfOkToCreateMenu = true
	else 
	(	
		if (menuMan.findMenu "Battlefield Tools") == undefined then 
			bfOkToCreateMenu = true
	)

	if bfOkToCreateMenu then
	( 
		local mainMenu = menuMan.getMainMenuBar()
		local bfMenu = menuMan.createMenu "Battlefield Tools"
	
		local ItemsTemp = #()
	 	ItemsTemp[1] = menuMan.createActionItem "BF_Toolset_Mesh" "Battlefield: 1942 Editing"
		append ItemsTemp (menuMan.createActionItem "BF_Toolset_Trees" "Battlefield: 1942 Editing")
		
		append ItemsTemp (menuMan.createSeparatorItem())	
		append ItemsTemp (menuMan.createActionItem "BF_Toolset_Animation" "Battlefield: 1942 Editing")
		append ItemsTemp (menuMan.createActionItem "BF_Toolset_SkeSkn" "Battlefield: 1942 Editing")
	
		append ItemsTemp (menuMan.createSeparatorItem())
		append ItemsTemp (menuMan.createActionItem "BF_Toolset_Vehicles" "Battlefield: 1942 Editing")
	
		append ItemsTemp (menuMan.createSeparatorItem())
		append ItemsTemp (menuMan.createActionItem "BF_RFA_Tools" "Battlefield: 1942 Editing")
		append ItemsTemp (menuMan.createActionItem "BF_ModWizard" "Battlefield: 1942 Editing")
		append ItemsTemp (menuMan.createSeparatorItem())
		append ItemsTemp (menuMan.createActionItem "BF_Help" "Battlefield: 1942 Editing")
		
		for i=1 to itemsTemp.count do 
			bfMenu.addItem ItemsTemp[i] -1
		
		local subMenuItem = menuMan.createSubMenuItem "Battlefield Tools" bfMenu
		local subMenuIndex = mainMenu.numItems()
		mainMenu.addItem subMenuItem subMenuIndex
		menuMan.updateMenuBar()
	) 
)


fn bfRemoveGUIMenu =
(
	local tmpMenu = menuMan.findMenu "Battlefield Tools";
	if tmpMenu != undefined then 
	(
		menuMan.unRegisterMenu tmpMenu;
		menuMan.updateMenuBar();
	)
)


fn bfReloadMenu =
(
	bfRemoveGUIMenu()
	bfAddGUIMenu()
)


bfReloadMenu()


