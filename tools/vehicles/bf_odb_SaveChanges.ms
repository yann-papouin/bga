fn r_point3cmp a b t:5=
(
	if around_to a[1] t != around_to b[1] t then return false
	if around_to a[2] t != around_to b[2] t then return false
	if around_to a[3] t != around_to b[3] t then return false
	
	return true
)

fn r_arraycmp a b t:5=
(
	for i=1 to a.count do
		if around_to a[i] t != around_to b[i] t then return false

	return true
)




global g_bf_odb_CleanProps = \
	#("addtemplate","setposition","position","setrotation","rotation","setinputid","startoneffects", \
	"setrandomgeometries","setlodvalues","bindtoskeletonpart","useasbone","setboneoriginoffset")
	
fn bfGetTemplateCleaned objName =
(
	local tmpProps = bfODB_GetTemplate objName 
	local finalProps = #()
	for j=1 to tmpProps.count do
	(
		local tmpPropName = lowercase tmpProps[j][1]
		local findID = (findItem g_bf_odb_CleanProps tmpPropName)	
		if 0 == findID then
		(
			append finalProps tmpProps[j]
		)
	)
	return finalProps 
)



fn bfGetLodSelectorName objName =
(
	local tmpProps = bfODB_GetTemplate objName 
	for j=1 to tmpProps.count do
	(
		if lowercase tmpProps[j][1] == "lodselector" then
		(
			return tmpProps[j][2]
			exit
		)
	)
	return ""
)




-----------------------------------------------------------------------
-- Updates bf_odb #() based on scene object positions and parent/child
-- 
fn bf_ve_DBUpdateFromScene objs:undefined s:0.1=
(

	if bf_ve_isODB_loaded() != true then
	(
		format "Bf_odb not loaded\n"
		return false
	)
	
	if classof objs != Array then objs = objects;

	local objsSet = 0
	for i=1 to objs.count do
	(
		local tmpObj = objs[i]
		-- format "**** %_Object: %\n" i tmpObj.name

		local tmpTemplate = bfGetTemplateCleaned tmpObj.name
		

		-- get Children Info from scene
		for c=1 to tmpObj.children.count do
		(
			local tmpChild = tmpObj.children[c]
			if (lowercase tmpChild.name) == (lowercase tmpObj.name) then continue
			
			-- format "  %\n" tmpChild
			append tmpTemplate #("addTemplate", tmpChild.name)


			-- relative (local) transform coords
			local tmpTransform = tmpChild.transform * (inverse tmpObj.transform)

			-- setPos		
			local tmpPos = tmpTransform.row4 * s
			if (r_point3cmp tmpPos [0,0,0]) != true then
			(
				
				local strPropArg = (tmpPos[1] as string) + "/" + (tmpPos[3] as string) + "/" + (tmpPos[2] as string)
				append tmpTemplate #("setposition", strPropArg )
			)
			
			-- setRot
			local tmpRot = #(tmpTransform.rotation.x, tmpTransform.rotation.y, tmpTransform.rotation.z, tmpTransform.rotation.w)
			if ( r_arraycmp tmpRot #(0,0,0,1)) != true then
			(
				tmpRot = getBfYPR tmpTransform
				local strPropArg = (tmpRot[1] as string) + "/" + (tmpRot[2] as string) + "/" + (tmpRot[3] as string)
			--	format "       rot: %\n" strPropArg 
				append tmpTemplate #("setrotation", strPropArg)
			)				

			-- Additional props
			local tmpPropBuffer = getUserPropBuffer tmpChild
			 
			if (tmpPropBuffer.count > 0) then
			(
				local propsList = filterString tmpPropBuffer ";"
				for n=1 to propsList.count do
				(				
					local foundID = findString propsList[n] " "
					if (foundID > 0) then 	
					(
						local moreStuff = #( (subString propsList[n] 1 (foundID-1)), (subString propsList[n] (foundID+1) -1))
						append tmpTemplate moreStuff
					)
					else MessageBox "ERROR! bfODB_GetTemplate() No spaces in property!"				
				)
			)

		)
		
	
		-- Now set the properties in memory
		local tmpSetString = "*;"
		
		-- Skip the first, it is the object Type
		for n=2 to tmpTemplate.count do
		(
			append tmpSetString (tmpTemplate[n][1] + " " + tmpTemplate[n][2] + ";")
		)
		
		local tmpRet = bfODB_SetTemplate tmpObj.name tmpSetString 
		if (tmpRet > 0) then objsSet += 1

	)

	return objsSet


)



(
--	clearListener()
--	bf_ve_DBUpdateFromScene()

	--bfGetTemplateCleaned "Willy"
	
	--bf_ve_DBUpdateFromScene()
	--bf_ve_DBSave()
)
