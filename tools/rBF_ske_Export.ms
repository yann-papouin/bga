rollout rBF_ske_Export "Skeleton (.ske) and Skin (.skn)" height:384  -- width:570 
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
	groupBox grp1 "Skeleton Import" pos:[58,73] width:179 height:147
	checkbox skeMakeBoxBonesChk "As Boxes" pos:[105,111] width:71 height:16
	spinner bnScale "" pos:[149,142] width:61 height:16 range:[0.01,10000,100] type:#float scale:1
	label lbl3 "Bone Size % " pos:[85,143] width:62 height:13
	button skeImpButton "Import (.ske)" pos:[92,175] width:105 height:24
	groupBox grp3 "Skeleton Export" pos:[296,129] width:179 height:90

	button skeExpButton "Export (.ske)" pos:[330,170] width:105 height:24
	button sknExpButton "Export (.skn)" pos:[336,283] width:105 height:24
	groupBox grp4 "Skin Export" pos:[296,250] width:179 height:77
	groupBox grp5 "" pos:[58,250] width:179 height:78
--	groupBox grp5 "Skin Import" pos:[58,250] width:179 height:78
--	button sknImpButton "Import (.skn)" pos:[92,282] width:105 height:24 enabled:false
	CheckBox chkIsWeapon "Is Weapon" pos:[300,100] width:80 height:18
	
	label lbl4 "" pos:[0,340] width:17 height:8
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-------------------------------	
	on rBF_ske_Export open do
	(

		
		local tmpSetting = (getBfSetting "ske_bnScale") as float
		if tmpSetting > 0.009 then
		(	bnScale.value = tmpSetting
			skeMakeBoxBonesChk.checked = str2bool (getBfSetting "skeMakeBoxBonesChk")
			chkIsWeapon.checked = (str2bool (getBfSetting "chkIsWeapon"))
		)
	)
	on rBF_ske_Export Close do
	(
		setBfSettingB "ske_bnScale" (bnScale.value as string)	
		setBfSettingB "skeMakeBoxBonesChk" (skeMakeBoxBonesChk.checked as string)
		setBfSettingB "chkIsWeapon" (chkIsWeapon.checked as string)
		
	)
		



	fn bfAniUserError errorNumber=
	(
		local errStr = "ERROR!! "
		errStr += case errorNumber of
		(	1: "Select something before pressing this button."
			2: "Pick a mesh to apply a .skn file first."
			default: "Unknown Error"
		)
		messagebox errStr title:"Error"; return OK
	)
-----------
--------------------------------------------
	on skeImpButton pressed do
	(
		local fname = getOpenFileName caption:"Import .ske" types:"Skeleton .ske|*.ske|All|*.*|"
		if fname != undefined do
		(
			local TmpArray1 = importBfskeA fname (bnScale.value * 0.1) isWeapon:chkIsWeapon.checked
			
			if not chkIsWeapon.checked then -- weapons are already imported as boxes
			(
				if skeMakeBoxBonesChk.checked then
				(
					local tmpObjs = bfSKE.makeBoxBones TmpArray1 (bnScale.value * 0.1)
					delete TmpArray1
					tmpArray1 = tmpObjs
				)
			)
			
			-- Try to rotate if it is a weapon or player skeleton
			if Classof tmpArray1 == Array then 
			(
				ske_rotatePlayerImport()
			--	ske_rotateWeaponImport tmpArray1[1]
			)

		)
	)
	on skeExpButton pressed do
	(
		if selection.count > 0 then
		(
			fname = getSaveFileName caption:"Export .ske" types:"Skeleton .ske|*.ske|All|*.*|"
			if fname != undefined then
				Write_Ske fname (ske_GetBones_FromScene selection isWeapon:chkIsWeapon.checked) 0.1 isWeapon:chkIsWeapon.checked --(bnscale.value * 0.001)
		)
		else bfAniUserError 1
	)
--------------------------------------------
	on sknExpButton pressed do
	(
		if selection[1] != undefined then
			writeBfsknA selection[1] 0.1 isWeapon:chkIsWeapon.checked --(bnscale.value * 0.001)
		else bfAniUserError 1
	)
	on sknImpButton pressed do
	(
		fname = getOpenFileName caption:"Import .skn" types:"Skin .skn|*.skn|All|*.*|"
		if fname != undefined then
		(
		)
	)
--------------------------------------------

)






