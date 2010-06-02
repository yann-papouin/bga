-- These are defined in bf_menu_setup.ms
/*
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
		tmpKey = (MakeTrailingSlash tmpKey) + "Battlefield Mod Development Tools"
	)
	
	return tmpKey
)
*/

global g_lc_upper="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
global g_lc_lower="abcdefghijklmnopqrstuvwxyz"
fn lowerCase instring =
(	
	local outString = ""
	
	if Classof inString == String then
	(
		outstring = "" + instring
		local inx
	   	for i=1 to instring.count do
	   	(  
			inx=findString g_lc_upper instring[i]
			if inx != undefined then
				outstring[i] = g_lc_lower[inx]
	   	)
	 
   )
 
   return outstring
)

fn r_readBinStr f =
(
	local tmpLen = bf_readByte f
	local tmpStr = ""
	for i=1 to tmpLen do
		tmpStr = tmpStr + (bit.intAsChar (bf_readByte f))
	
	return tmpStr
)

-- Might not be used anymore?
fn r_loadSlist f =
(
	local strs = #()
	local strCount = bf_readlong f
	
	for i=1 to strCount do
		strs[i] = r_readBinStr f

	return strs;
)


-- STRING operation
fn sort_stringArray theArray =
(
	-- Note: Duplicate Strings are removed
	
	local LcArray = #()
	local LcArray2 = #()
	
	for i=1 to theArray.count do
	(
		LcArray[i] = lowercase theArray[i]
		LcArray2[i] = lowercase theArray[i]
	)
	
	sort LcArray
	
	local finalArray = #()
	local foundID = 0
	for i=1 to LcArray.count do
	(
		foundID = findItem LcArray2 LcArray[i]
		if (foundID > 0) then
		(	finalArray[i] = theArray[foundID]
		)
		else format "WARNING! sortString() dropped: %\n" LcArray[i]
	)

	return finalArray 
)




fn bfAnim_CleanBoneName tmpName = 
(
	local tmpCnt = tmpName.count
	while tmpCnt > 1 do
	(
		if tmpName[tmpCnt] != " " then exit
		else tmpCnt -= 1
	)
	return (subString tmpName 1 tmpCnt)
)

--------------------------------
-- Mimic Animation data
--
fn rexMimicAnimation theMaster theSlave =
(

	local startFrame = animationRange.start
	local endFrame = animationRange.end
	animate on
	(
		for t=startframe to endFrame do
		(	
			at time t
			(
				theSlave.transform = theMaster.transform
			)
		)
	)
	
)




fn bf_ReadFileToBuffer thisFile =
(
	local f = bf_fopen thisFile "r"
	if f != undefined and f != 0 then
	(
		bf_fseek f 0 #seek_End
		local fiSize = bf_Ftell f
		bf_fseek f 0 #seek_set
		
		local tmpBuff = stringStream ""
		
		local maxTmpBuf = 500
		local amntLeft = fiSize
		local amtToRead = maxTmpBuf 
		while (amntLeft > 0) do
		(
			if (amntLeft < maxTmpBuf) then
				amtToRead = amntLeft 
			
			format "%" (bf_readString2 f amtToRead) to:tmpBuff
			amntLeft -= amtToRead
		)
		bf_fClose f
		
		format "\n" to:tmpBuff
		
		seek tmpBuff 0
		
		return tmpBuff
	)
	
	return Undefined
)


--------------------------------------------
--- weld verts Modifier Stand-in for gmax
fn BFreduce obj t=
(
	if (isValidNode obj) != true then return()
	if Classof obj != Editable_Mesh then return()	
	local a = obj.numverts
	
	if a < 1 then return()
	
	local maxWidth = 0.0
	for i=1 to obj.numverts do
	(
		local tmpVert = getVert obj i
		for j=(i+1) to obj.numverts do
		(
			local tmpDist = (distance (getVert obj j) tmpVert)
			if tmpDist  > maxWidth then
				maxWidth = tmpDist
		)
	)

	local numVerts = obj.numverts
	format "maxDistance: %\n" maxWidth
--	format "numVerts: %\n" numVerts
	
	local weldDist = maxWidth * (100.0 - (t / 4.0 + 75.0)) * 0.01
	format "Weld Threshold Dist: %\n" weldDist

	max modify mode
	select obj
	
	subObjectLevel = 1
	meshop.setWeldThreshold obj weldDist
	obj.selectedVerts=#{1..a}
	meshops.weld obj
	obj.selectedVerts = #{}

)

fn Rex_meshop_deleteFaces obj faceList delIsoVerts:true asMesh:false=
(
	sort faceList
	
	for i=1 to FaceList.count do
	(
		deleteFace obj (faceList[i] - i + 1)
	)
	
	if (delIsoVerts == true) then
		meshop.deleteIsoVerts obj
	
)
fn rexAttach dest source =
(	
	dest = dest + source
)

-- rounds a value to n decimal places
fn Around_to val n = 
(   local mult = 10.0 ^ n
	(floor ((val * mult) + 0.5)) / mult
)



fn copyMapChannel obj fromThis obj2 toThis =
(
	local numMapVerts = meshop.getNumMapVerts obj fromThis
--	format "copyMapChannel(): % % | % % \n" obj fromThis obj2 toThis
	if obj.numFaces == obj2.numFaces then
	(
		meshop.setNumMaps obj2 (toThis+1) keep:true
		meshop.setMapSupport obj2 toThis true
		meshop.setNumMapVerts obj2 toThis numMapVerts
		
		for i=1 to numMapVerts do
			meshop.setMapVert obj2 toThis i (meshop.getMapVert obj fromThis i)
		for i=1 to obj2.numFaces do
			meshop.setMapFace obj2 toThis i (meshop.getMapFace obj fromThis i)
	)
	else format "Error! mapfaces != % %\n" obj.numFaces obj2.numFaces
)


------------------------------------
-- Returns trimesh with expected transform (not affected by pivot misbehaving)
--
fn getMesh obj isLocal=
(	
	local tmA = obj.transform
	if isLocal then obj.Transform = matrix3 1
	

	local tmpMesh = snapShotAsMesh obj

	
	if isLocal then obj.Transform = tmA
	
	return tmpMesh
)










fn bf2maxConRot bfRot =
(
	local tmpRot = (rotateYMatrix -bfRot[3]) * (rotateXmatrix -bfRot[2]) * (rotateZmatrix -bfRot[1])

	return tmpRot
)

fn getBfYPR inputMatrix=
(
	local tmpMatrix = copy inputMatrix
	tmpMatrix.row4 = [0,0,0]
	
	-- Matrix in this form:
	-- row1 = X-axis Vector (Red) - Right
	-- row2 = Y-axis Vector (Green) - Away
	-- row3 = Z-axis Vector (Blue) - Up
	
	local tmpYaw, tmpPitch, tmpRoll
	local yAxis, xAxis
	
	-- Y-axis vector (row2)
	yAxis = tmpMatrix.row2


	-- roll is the twist of the (y-axis)

	-- Goal here is to make yAxis == [0,1,0]
	-- Then you can find out the actual roll value
	
	-- remove "absolute" yaw
	tmpYaw = atan (yAxis.y / yAxis.x) -- tan = opp / adj
	if yAxis.y > 0 then 
	(
		if yAxis.x < 0 then tmpYaw = 90 + tmpYaw
		else tmpYaw = tmpYaw - 90
	)
	else
	(
		if yAxis.x < 0 then tmpYaw = 90 + tmpYaw
		else tmpYaw = tmpYaw - 90
	)
--	format "tmpYaw: %\n" tmpYaw
	tmpMatrix = tmpMatrix * (rotateZmatrix -tmpYaw)
	yAxis = tmpMatrix.row2

	

	-- remove "absolute" pitch (with yaw removed)
	tmpPitch = atan (yAxis.z / yAxis.y)
	if yAxis.y < 0 then
	(
		tmpPitch = 180 + tmpPitch
		if tmpPitch > 180 then tmpPitch = -360 + tmpPitch
	)
--	format "tmpPitch: %\n" tmpPitch
	tmpMatrix = tmpMatrix * (rotateXmatrix (-tmpPitch))

			
	-- you can now factor Roll (twist of y-axis) from the x-axis
	xAxis = tmpMatrix.row1
	tmpRoll = atan (xAxis.z / xAxis.x)
	if xAxis.x > 0 then
	(
		tmpRoll = -tmpRoll
	)
	else
	(
		tmpRoll = 180 - tmpRoll
		if tmpRoll > 180 then tmpRoll = - (360 - tmpRoll)
	)

	return #( -tmpYaw, -tmpPitch, -tmpRoll )
)








fn writeBinaryString f tmpString =
(	for i=1 to tmpString.count do bf_writeByte f (bit.CharAsInt tmpString[i])
)

fn trueCollapse_A tmpObject1 =
(
	tmpName = tmpObject1.name
	convertTo tmpObject1 TriMeshGeometry
	tmpObj = Plane mapCoords:on lengthsegs:1 widthsegs:1 length:1 width:1
	convertTo tmpObj TriMeshGeometry
	attach tmpObj tmpObject1
	meshop.deleteVerts tmpObj #(1,2,3,4)
	meshop.deleteIsoVerts tmpObj
	tmpObj.name = tmpName
	return tmpObj
)

-- For scene object replacement
fn swapObjects objA objB =
(
	if objA != undefined and objB != undefined then
	(
	-- Store lists of children for later use
	local objAChildren = #()
	for i=1 to objA.children.count do append objAChildren objA.children[i]
	local objBChildren = #()
	for i=1 to objB.children.count do append objBChildren objB.children[i]
	
	-- Free children from any parent
	for i=1 to objAChildren.count do objAChildren[i].parent = undefined
	for i=1 to objBChildren.count do objBChildren[i].parent = undefined
	
	-- Swap transforms
	local tmpTransform = copy objA.transform
	objA.transform = copy objB.transform
	objB.transform = copy tmpTransform
	
	-- Re-Assign Children to new parents
	for i=1 to objAChildren.count do objAChildren[i].parent = objB
	for i=1 to objBChildren.count do objBChildren[i].parent = objA
	
	-- Swap parents
	local tmpParent = objA.parent
	objA.parent = objB.parent
	objB.parent = tmpParent
	)
)

----------------------
-- currently does not support precisions other than 15 bit mantissa
fn MakeVariablebitFloat tmpFloat numBitsTotal precis=
(
	local tmpSign = false
	if tmpFloat < 0.0 do
	(	tmpSign = true
		tmpFloat = abs(tmpFloat)
	)

	maximumValue = (pow 2 (numBitsTotal - precis - 1)) 
	expValue = 0
	theFinalValue = 0
	manTisField = 0

	amntLeft = tmpFloat as float
	if abs(tmpFloat) >= maximumValue then theFinalValue = (pow 2 precis) as integer - 1
	else
	(
		for i=1 to precis do
		(
			expValue -= 1
			if amntLeft >= (pow 2 expValue) do
			(
			--	format "%\t% >= %\n" i amntLeft (pow 2 expValue)
				manTisField = bit.set manTisField (precis + 1 - i) true
				amntLeft -= (pow 2 expValue)

			)
		)
		theFinalValue = manTisField 
	)
--	format "manTisField: %\n" manTisField
	return (bit.set theFinalValue numBitsTotal tmpSign)
)



----------------------
--
fn ReadVariablebitFloat tmpInt numBitsTotal precis=
(
	expField = 0
	expFieldSize = (numBitsTotal - 1 - precis)
	

	curPlace = numBitsTotal - 1
	theFinalValue = 0.0
	if expFieldSize > 0 do
	(
		expSubtract = (pow 2 expFieldSize) / 2 - 1
		for i=1 to expFieldSize do
		(
			if (bit.get tmpInt curPlace) do
				expField  = bit.set expField (expFieldSize + 1 - i) true

			curPlace -= 1
		)

		expField = expField - expSubtract
		theFinalValue += (pow 2 expField)
	)

	for i=1 to precis do
	(
		if (bit.get tmpInt curPlace) do
			theFinalValue += (pow 2 (expField - i))

		curPlace -= 1
	)

	if (bit.get tmpInt numBitsTotal) do theFinalValue *= -1
	
	
	
	return theFinalValue
)


------------------
mapped fn unlinkTheseObjects Tobj = Tobj.parent = undefined



------------------------------------------------
fn getFileSize_compatable thisFile =
(
	fi_sizo = 0
	local f = bf_fopen thisFile "rb"
	if f != undefined do
	(	bf_fseek f 0 #seek_end
		fi_sizo = bf_ftell f
		bf_fclose f
	)
	return fi_sizo

)
fn existFile fname = (getfiles fname).count != 0 
------------------------------------------------
fn listFiDir thisFile=
(
	dirPath = getFilenamePath thisFile
	saveListFi = "c:\\"+"tmplist.txt"
	bf_DOSCommand ("dir \""+dirPath+"\" /a-d /b /o:g /s > \""+saveListFi+"\"")
	fa = bf_OpenFile saveListFi
	list0Files = #()
	if fa != undefined do
	(
		i = 0
		while (eof fa) != true do
		(	i+=1
			list0Files[i] = bf_ReadLine fa
		)
	)
	list0Files
)
-----------------------------------
----------Scene OBJECT LISTER------
-----------------------------------
fn listObjectsNamesScene question nothing =
(
	listo = #()
	if geometry.count > 0 then
	(	print "---Object list---"

		listo[1] = geometry[1].name

		for i=2 to geometry.count do
		(	tmpResult = true
				--// test to see if this object is unique to the list
			for j=1 to listo.count do
				if i != j do
					if geometry[i].name == listo[j] do
						tmpResult = false
			if tmpResult == true do
			(	tmpCt = listo.count + 1
				listo[tmpCt] = geometry[i].name
			)
		)
		if question == 1 do
		(	f=bf_fopen nothing "wb"
			bf_writeLong f listo.count
			for i=1 to listo.count do
				bf_writeString f (listo[i] as string)
			bf_fclose f
		)
		for i=1 to listo.count do
			format ((listo[i] as string)+"\n")
	)
	else print "**ERROR: 0 geometry objects in this scene"
	return listo
)

--------------------------------
--- AlignToGround --------------
--------------------------------
fn calcFaceNormal vec3 = normalize ( cross (vec3[1] - vec3[2]) (vec3[2] - vec3[3]) )

fn pointZOnPlane normal testPos = 
(	( - (normal.x*testPos.x + normal.y*testPos.y) / normal.z)
)

fn testAedge edge point =
(	 (normalize (edge[1] - edge[2])) + (normalize (point - edge[1])) 
)

fn isEven thisValue =
(	return (thisValue == ((thisValue * 0.5) as integer) * 2)
)



--------------------------------------------
-----------StandardMesh Polygon Counter ----
--------------------------------------------
fn getSMpolyCount thisFile returnText:false=
(
	local endedAt = 0
	local strOut = #()
	if thisFile != undefined then
	(
		
		append strOut (thisFile as string)
		local f=bf_fopen thisFile "rb"
		try
		(
			SM_HeaderV = bf_ReadLong f
			if SM_HeaderV <= 10 then
			(

				bf_fseek f 28 #seek_cur
				if SM_HeaderV == 10 do
					bf_fseek f 1 #seek_cur
				numTopModels = bf_ReadLong f
				for i=1 to numTopModels do -- hitbox skip
				(	tmpSeek = bf_ReadLong f
					bf_fseek f tmpSeek #seek_cur
				)

				numLods = bf_ReadLong f 
				append strOut ("numLods: " + (numLods as string) )
				nameLn = 0
				for i=1 to numLods do
				(
					numMaterials = (bf_ReadLong f )
					append strOut ( (i as string)+"_numMaterials: " + (numMaterials as string) )
					numbaFaces = 0
					numbaVerts = 0
					VertByteSize = 0

					totalVertBytes = 0
					for i=1 to numMaterials do
					(
						nameLn = bf_ReadLong f
--	print ( "nameLn: " + (nameLn as string) )
						bf_fseek f (nameLn+20) #seek_cur

						VertByteSize = bf_ReadLong f

						tnumbaVerts = bf_ReadLong f
						tnumbaFaces = bf_ReadLong f
--	print ( "\t DEBUG  V: " + (tnumbaVerts as string) + "\tF: " + (tnumbaFaces as string) + "\tBytes: " + ((tnumbaVerts*32 + tnumbaFaces*2) as string ) )
						totalVertBytes	= totalVertBytes + (tnumbaVerts * VertByteSize)

						numbaVerts += tnumbaVerts
						numbaFaces += tnumbaFaces
						bf_fseek f 4 #seek_cur
					)
					append strOut ("  Verts: "+(numbaVerts as string)+"  Faces: "+((numbaFaces/3) as string))
					bf_fseek f (totalVertBytes + numbaFaces*2) #seek_cur -- faces is numba face data values (words)
--	print ( (bf_ftell f) as string)
				)
				endedAt = bf_ftell f
			)
			else append strOut "Error: Unsupported header type"
		)
		catch (messageBox "There was an exception error while reading the file.")

		bf_fclose f
	)
	
	for i=1 to strout.count do
		format "%\n" strOut[i]
		
	if returnText == true then
		return strOut
	else
		return endedAt
)





 

