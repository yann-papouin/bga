global g_rBfSDKchild
global g_rBfSDK
global g_rbfSDK_OncePerSession = 0
global rBF_objs
global bfmsloc = (getDir #scripts) + "\\Battlefield Tools\\"

global g_BFDIR_BIN = bfmsloc + "bin\\"
global g_TEMP_BF = ((getDir #maxRoot)+ "TEMP_BF/")
(for i=1 to g_TEMP_BF.count do if g_TEMP_BF[i] == "\\" then g_TEMP_BF[i] = "/"
)


if bfLoadUpInto == undefined then
	global bfLoadUpInto = 1


bfmdtops.makeDir g_TEMP_BF

-- g_BFisGmax
filein (bfmsloc+"rBFi_isGmax.ms")

---------------------------------------------------------
if g_rbfSDK_OncePerSession < 2 then
(
	if g_rbfSDK_OncePerSession == 1 then g_rbfSDK_OncePerSession = 2
	
	-- if g_BFisGmax then filein (bfmsloc+"rBF_wrappers.mse")
	-- else 
	filein (bfmsloc+"rBF_wrappers.ms")
	
	fn bf_fnfileIn arg = filein (bfmsloc+"functions\\"+arg+".ms")
	bf_fnfileIn "prettyDosBatch"
	bf_fnfileIn "z_miscFunctions"
	bf_fnfileIn "rexMesh"
	bf_fnfileIn "rsParser"
	bf_fnfileIn "_SM_import"
	bf_fnfileIn "ske_tools"
	bf_fnfileIn "skn_tools"
	bf_fnfileIn "_SM_Export"
	bf_fnfileIn "baf_animation"
	bf_fnfileIn "TMexportSprite"
	bf_fnfileIn "TreeMeshExport"
	bf_fnfileIn "TreeMeshImport"
	

	
	-----------------------------------------------------------
	global rBF_sm_Export
	global rBF_sm_Import
	global rBF_baf_Export
	global rBF_ske_Export
	global rBF_tm_Export
	--------
	filein (bfmsloc+"rBF_settings.ms")
	filein (bfmsloc+"rBF_sm_Export.ms")
	filein (bfmsloc+"rBF_sm_Import.ms")
	
	filein (bfmsloc+"rBF_baf_Export.ms")
	filein (bfmsloc+"rBF_ske_Export.ms")
	
	filein (bfmsloc+"rBF_tm_Export.ms")
	
	filein (bfmsloc + "vehicles\\rBF_objs3.ms")
	filein (bfmsloc+"rBF_Static.ms")
)

--createDialog rollBf1942sdk style:#(#style_titlebar,#style_sysmenu,#style_minimizebox)
try( CloseRolloutFloater g_rBfSDK) catch()


if g_BFisGmax then
	g_rBfSDK = newRolloutFloater "Battlefield: Mod Development Toolkit - Ver: 2.52" 575 422
else
	g_rBfSDK = newRolloutFloater "Battlefield: Mod Development Toolkit - Ver: 2.52" 575 385

g_rBfSDKchild = rBF_sm_Export
-- addRollout g_rBfSDKchild g_rBfSDK

case bfLoadUpInto of
(
	1: g_rBfSDKchild.ckb3dModels.Changed true
	2: g_rBfSDKchild.ckbAnimation.Changed true
	3: g_rBfSDKchild.ckbSkeSkn.Changed true
	4: g_rBfSDKchild.ckbTrees.Changed true
	5: g_rBfSDKchild.ckbVehicles.Changed true
)



