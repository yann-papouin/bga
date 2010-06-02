-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Top Tabs
checkButton ckb3dModels "3D Models" pos:[15,10] width:66 height:20 checked:true toolTip:"Standard Geometry Import/Export (.sm)"
checkButton ckbAnimation "Animation" pos:[82,10] width:64 height:20 checked:false toolTip:"Animation Data Export/Import  (.baf)"
checkButton ckbSkeSkn "Ske/Skn" pos:[148,10] width:58 height:20 checked:false toolTip:"Skeleton and Skin Import/Export (.ske,.skn)"
checkButton ckbTrees "Trees" pos:[208,10] width:40 height:20 checked:false toolTip:"Tree Geometry Import/Export (.tm)"
checkButton ckbVehicles "Vehicles" pos:[250,10] width:54 height:20 checked:false toolTip:"Vehicle Import/Export (.con)"

groupBox grpMaster "" pos:[9,0] width:300 height:34

bitmap bmp9a "Bitmap" pos:[420,16] width:117 height:69 fileName:"bfsdk_eadiceLogo.bmp"

fn bfChangeChecked =
(
	case bfLoadUpInto of
	(
		1:
		(
		g_rBfSDKchild.ckb3dModels.checked = true
		g_rBfSDKchild.ckbAnimation.checked = false
		g_rBfSDKchild.ckbSkeSkn.checked = false
		g_rBfSDKchild.ckbTrees.checked = false
		g_rBfSDKchild.ckbVehicles.checked = false
		)
		2:
		(
		g_rBfSDKchild.ckb3dModels.checked = false
		g_rBfSDKchild.ckbAnimation.checked = true
		g_rBfSDKchild.ckbSkeSkn.checked = false
		g_rBfSDKchild.ckbTrees.checked = false
		g_rBfSDKchild.ckbVehicles.checked = false
		)
		3:
		(
		g_rBfSDKchild.ckb3dModels.checked = false
		g_rBfSDKchild.ckbAnimation.checked = false
		g_rBfSDKchild.ckbSkeSkn.checked = true
		g_rBfSDKchild.ckbTrees.checked = false
		g_rBfSDKchild.ckbVehicles.checked = false
		)
		4:
		(
		g_rBfSDKchild.ckb3dModels.checked = false
		g_rBfSDKchild.ckbAnimation.checked = false
		g_rBfSDKchild.ckbSkeSkn.checked = false
		g_rBfSDKchild.ckbTrees.checked = true
		g_rBfSDKchild.ckbVehicles.checked = false
		)		
		5:
		(
		g_rBfSDKchild.ckb3dModels.checked = false
		g_rBfSDKchild.ckbAnimation.checked = false
		g_rBfSDKchild.ckbSkeSkn.checked = false
		g_rBfSDKchild.ckbTrees.checked = false
		g_rBfSDKchild.ckbVehicles.checked = true
		)		
	)
)

fn changeRoll thisRoll =
(
/*	if g_rBfSDKchild.name == "rBF_baf_Export" then
	(if not g_rBfSDKchild.bafOkToClose() then return()
	)
*/
	removeRollout g_rBfSDKchild g_rBfSDK
	g_rBfSDKchild = thisRoll
	addRollout g_rBfSDKchild g_rBfSDK
	
	bfChangeChecked()
)

on ckb3dModels changed state do
(	
	if ( state ) then
	(
		bfLoadUpInto = 1
		changeRoll rBF_sm_Export
	)
	else ckb3dModels.checked = true
)

on ckbAnimation changed state do
(	
	if ( state ) then
	(
		bfLoadUpInto = 2
		changeRoll rBF_baf_export
	)
	else ckbAnimation.checked = true
)

on ckbSkeSkn changed state do
(
	if ( state ) then
	(		
		bfLoadUpInto = 3
		changeRoll rBF_ske_Export
	)
	else ckbSkeSkn.checked = true
)

on ckbTrees changed state do
(
	if ( state ) then
	(		
		bfLoadUpInto = 4
		changeRoll rBF_tm_Export
	)
	else ckbTrees.checked = true
)

on ckbVehicles changed state do
(
	if ( state ) then
	(		
		bfLoadUpInto = 5
		changeRoll rBF_objs
	)
	else ckbVehicles.checked = true
)
 
/*
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-- Sub-Tabs
checkButton ckbExport "Export" pos:[21,40] width:40 height:20 checked:true tooltip:"Export (.sm) geometry files"
checkButton ckbImport "Import" pos:[63,40] width:40 height:20 tooltip:"Import (.sm) geometry files"


on ckbExport changed state do
(	
	if ( state ) then
	(
		if ckbExportPressed != undefined then ckbExportPressed()
		
		g_rBfSDKchild.ckbExport.checked = true
		g_rBfSDKchild.ckbImport.checked = false
	)
	else ckbExport.checked = true
)

on ckbImport changed state do
(
	if ( state ) then
	(
		if ckbImportPressed != undefined then ckbImportPressed()	
		
		g_rBfSDKchild.ckbImport.checked = true
		g_rBfSDKchild.ckbExport.checked = false
	)
	else ckbImport.checked = true
)
*/