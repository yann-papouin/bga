-- Project: Battlefield: 1942/Vietnam
-- Group:   Animation Import/Export
--
-- Author: Rex Hill
-- Last Update: FEB 2004
--




--------------------------------------------
-- Selects bone sets for exporting animation
--
fn bf_AnimSelectBones boneSet=
(
	boneSet = boneSet as name
	
	local boneNames = #()
	case boneSet of
	(
		#1p_upper:
		(	boneNames = #("bip01 spine1","bip01 spine2","bip01 spine3","bip01 neck","bip01 head","bip01 l clavicle","bip01 l upperarm","bip01 l forearm","bip01 l hand","bip01 l finger0","bip01 l finger01","bip01 l finger02","bip01 l finger1","bip01 l finger11","bip01 l finger12","bip01 l finger2","bip01 l finger21","bip01 l finger22","bip01 l finger3","bip01 l finger31","bip01 l finger32","bip01 l finger4","bip01 l finger41","bip01 l finger42","bone07","bone08","bone09","wrist bone left","bip01 r clavicle","bip01 r upperarm","bip01 r forearm","bip01 r hand","bip01 r finger0","bip01 r finger01","bip01 r finger02","bip01 r finger1","bip01 r finger11","bip01 r finger12","bip01 r finger2","bip01 r finger21","bip01 r finger22","bip01 r finger3","bip01 r finger31","bip01 r finger32","bip01 r finger4","bip01 r finger41","bip01 r finger42","bone01","bone02","bone03","wrist bone right","bip01 spine")
		)
		#3p_upper:
		(	boneNames = #("bip01 spine","bip01 spine1","bip01 spine2","bip01 spine3","bip01 neck","bip01 head","bip01 l clavicle","bip01 l upperarm","bip01 l forearm","bip01 l hand","bip01 l finger0","bip01 l finger01","bip01 l finger02","bip01 l finger1","bip01 l finger11","bip01 l finger12","bip01 l finger2","bip01 l finger21","bip01 l finger22","bip01 l finger3","bip01 l finger31","bip01 l finger32","bip01 l finger4","bip01 l finger41","bip01 l finger42","bip01 r clavicle","bip01 r upperarm","bip01 r forearm","bip01 r hand","bip01 r finger0","bip01 r finger01","bip01 r finger02","bip01 r finger1","bip01 r finger11","bip01 r finger12","bip01 r finger2","bip01 r finger21","bip01 r finger22","bip01 r finger3","bip01 r finger31","bip01 r finger32","bip01 r finger4","bip01 r finger41","bip01 r finger42")
		)
		#3p_lower:
		(	boneNames = #("bip01","bip01 pelvis","bip01 l thigh","bip01 l calf","bip01 l foot","bip01 l toe0","bip01 r thigh","bip01 r calf","bip01 r foot","bip01 r toe0","spine root")
		)
		default: boneNames = #()
	)
	
	clearSelection()
	for i=1 to boneNames.count do
	(
		local tmpNode = getNodeByName boneNames[i]
		if (isValidNode tmpNode) then
			selectMore tmpNode 
	)
	return boneNames
)

------------------------------
-- Compresses an array of values to a RLE format array, true/false (rle/noRle), followed by numFrames
fn packRleStream strm CloseEqual:undefined=
(
	-- append gTest strm
--	format "packRleStream()\n"
--	format "  strm: %\n" strm
	
	if (Classof CloseEqual) == Integer then
	(	
		for i=1 to strm.count do strm[i] = Around_to strm[i] CloseEqual
	)
	
	local rleArray = #(strm.count,0)
	local maxFrames = 125
	local Writing_NonRle = false, startNonRlePos = 1, runLength = 1
	local tmpEndPos, specialCase = false
	
	local currPos = 1
	while currPos <= rleArray[1] do
	(
	
		if not Writing_NonRle do
		(		
			if (currPos + maxFrames - 1) < rleArray[1] then
				tmpEndPos = currPos + maxFrames - 1
			else
				tmpEndPos = rleArray[1]
		)
		

		for i = (currPos + 1) to tmpEndPos do
		(
			if strm[currPos] == strm[i] then
			(
				if i == tmpEndPos do
				(
					if tmpEndPos == rleArray[1] do specialCase = true
					
					runLength = i - currPos + 1
				)
			)
			else 
			(
				runLength = i - currPos
				exit
			)
		)
		
		if runLength > 2 then -- rle Pack
		(
			if Writing_NonRle do
			(
				Writing_NonRle = false
				rleArray[startNonRlePos] = rleArray.count - startNonRlePos 
			)
			
			rleArray[rleArray.count + 1] = true -- isRLE
			rleArray[2] += 2
			rleArray[rleArray.count + 1] = runLength -- t_numFrames
			rleArray[rleArray.count + 1] = strm[currPos]
			currPos += runLength
			lastHeaderPos = currPos
			runLength = 1
		)
		else -- uncompressed
		(
			if not Writing_NonRle do
			(
				Writing_NonRle = true
				rleArray[rleArray.count + 1] = false -- isNotRLE
				rleArray[2] += 1
				rleArray[rleArray.count + 1] = 1 -- t_numFrames (put dummy placeholder, change later)
				startNonRlePos = rleArray.count
			)
			if currPos == tmpEndPos do
			(
				rleArray[startNonRlePos] = rleArray.count - startNonRlePos + 1
				Writing_NonRle = false
			)
			for j=0 to (runLength-1) do
			(
				rleArray[rleArray.count + 1] = strm[currPos + j]
				rleArray[2] += 1
			)
				
			currPos += runLength
			
			if specialCase do
				rleArray[startNonRlePos] = rleArray.count - startNonRlePos
		)
	)
	local tmpCntA = rleArray.count
	for i=1 to tmpCntA do
	(
		if rleArray[i] == undefined do
		( 	deleteItem rleArray i
			rleArray[2] -= 1
		)
	)

	return rleArray
)




fn baf_weaponTransform_import t =
(
--(matrix3 [t.row1.x, t.row1.y, t.row1.z] [t.row2.x, t.row2.y, t.row2.z] [t.row3.x, t.row3.y, t.row3.z] [-t.row4.x, -t.row4.y, t.row4.z])
(matrix3 [t.row1.x, t.row1.y, t.row1.z] [t.row2.x, t.row2.y, t.row2.z] [t.row3.x, t.row3.y, t.row3.z] [-t.row4.x, -t.row4.y, t.row4.z])

)

---------------
--- for debug purposes
fn saySomeBits theBits=
(	-- Bit order:   MOST <--- significant ---> LEAST 
	CntBits = 0;	outString = "";
	for i=1 to theBits.count do
	(	CntBits += 1;
		if CntBits > 8 do (outString += " "; CntBits = 1;)
		bitFlagtmp = 0;
		if theBits[i] do bitFlagtmp = 1;
		outString += bitFlagtmp as string;
	)
	return outString;
)

fn formatBitArray a =
(	for i=1 to 16 do
	(	if i == 9 then format " "
		if a[i] then format "1"
		else format "0"
	); format "\n"
)

fn bafInt2Bits Valu nCnt=
(	theBits = #()
	for i=1 to nCnt do theBits[i] = bit.get Valu i
	return theBits;
)


fn bafBits2int theBits =
(	a = 0
	for i=1 to theBits.count do a = bit.set a i theBits[i]
	return a;
)

---
---------------------- END debug functions

fn makeQuatBehave inQuat =
(
	if abs(inQuat.x) < 0.00001 do inQuat.x = 0.0
	if abs(inQuat.y) < 0.00001 do inQuat.y = 0.0
	if abs(inQuat.z) < 0.00001 do inQuat.z = 0.0
	if abs(inQuat.w) < 0.00001 do inQuat.w = 0.0
	return inQuat
)


----------------------
---
fn max2BfQuat inQuat =
(	local tmpQuat = quat 0.0 0.0 0.0 1.0
	inQuat = makeQuatBehave inQuat
	-- (quat (-inQuat.x) (-inQuat.z) tmpValA tmpValB)
	
	tmpQuat.x = -inQuat.x
	tmpQuat.z = -inQuat.y
	tmpQuat.y = -inQuat.z
	tmpQuat.w = -inQuat.w
	if inQuat.w == 1 then tmpQuat.w = 1

	return tmpQuat
)


fn bf2maxQuat inQuat =
(	local tmpQuat = quat 0.0 0.0 0.0 1.0

	tmpQuat.x = -inQuat[1]
	tmpQuat.z = -inQuat[2]
	tmpQuat.y = -inQuat[3]
	tmpQuat.w = -inQuat[4]
		
	return tmpQuat
)


----------------------------------------------------
------------- FLOATING POINT  (16-Bit) -------------
----------------------------------------------------
fn get16motorolaBits tmpInt =
(	-- motorola format
	local a = #{}
	for i=1 to 16 do a[i] = bit.get tmpInt (17-i)

	return a
)

fn ReadVariable16bitFloat tmpInt precis=
(
	local flt16_mult = 32767.0 / (pow 2 (15-precis))

	local tmpVal = tmpInt

	if tmpInt > 32767 then
	(	tmpVal = tmpVal - 65535
	)
	
	return (tmpVal / flt16_mult)
)

fn MakeVariable16bitFloat tmpFloat precis=
(
	local flt16_mult = 32767.0 / (pow 2 (15-precis))
	local tmpInt = 0
	
	if tmpFloat < 0 then tmpInt = 65535.0 + flt16_mult * tmpFloat
	else tmpInt = flt16_mult * tmpFloat

	return (tmpInt as integer)
)



-- Gather array of all parents for a node
fn traceFamilyOrigin obj =
(
	local parentList = #()
	local currentObj = obj
	for i=1 to objects.count do
	(
		if currentObj.parent != undefined then
		(
			append parentList currentObj.parent
			currentObj = currentObj.parent
		)
		else exit
	)
	return parentList
)



-----------------------
-- Error box
fn bafFileHadError fi_h errorNumber =
(	if fi_h != undefined do bf_fclose fi_h
	errStr = "ERROR!! "
	errStr += case errorNumber of
	(	1: "Header != 3, incorrect .baf file type"
		2: "Bones not selected in scene that match those in file"
		3: "No Bones to export"
		9: "File could not be opened"
		default: "Unknown Error"
	)
	messagebox errStr; return OK
)

-----------------------
--- IMPORT .baf animation file
fn readBafFile thisFileName objs tScale doApplyBaf isWeapon:true =
(		
	format "-- .Baf File IMPORT (%)--\n" tScale
		
	local f = bf_fopen thisFileName "rb"
	if (f == undefined) do (bafFileHadError f 9; return()) -- file could not Open
		
	if ( (bf_ReadLong f) != 3) do (bafFileHadError f 1; return()) -- header is wrong
		
	objs = sortArrayH objs -- sort the hierarchy correctly
		
	local boneNameArray = #()
	local numBones = bf_ReadShort f 
	format "numBones: %\n" numBones
		
	-- add in a nameSearch matching method, and sort
	for i=1 to numBones do
	(	
		local nameLn = bf_ReadShort f
		boneNameArray[i] = bfAnim_CleanBoneName (lowercase (bf_ReadString f))
		format "  %_: \"%\"\n" i boneNameArray[i]
	)	
	local newObjArray = #()
	local dbgBeenFound = #()
	(	
		for i=1 to objs.count do
		(
			local tmpName = lowercase objs[i].name
			local tmpFoundID = findItem boneNameArray tmpName
			
			if tmpFoundID != 0 then
			(	
				dbgBeenFound[tmpFoundID] = true
				append newObjArray objs[i]
			)
		)
		objs = newObjArray
	)	
	
--	format "newObjArray.count: %\n" newObjArray.count
--	format "dbgBeenFound: %\n" dbgBeenFound
	for i=1 to dbgBeenFound.count do
	(
		if dbgBeenFound[i] != true then
		(
			format " ERROR! Bone not in scene: \"%\"\n" boneNameArray[i]
			append objs (box length:2.5 width:3.0 height:2.25 name:boneNameArray[i])
		)
	)
	
	
	if doApplyBaf then
	(
		if (numBones != objs.count) then -- (bafFileHadError f 2;return())
		(
			format "Scene is missing bones! Creating dummy animations for them\n"
			objs = #()
			for i=1 to numBones do objs[i] = box length:2.5 width:3.0 height:2.25 name:boneNameArray[i]
		)
		else -- order the selected bones to that of the file order
		(	newObjArray = #()
			--format "%\n" boneNameArray 
			--format "%\n" objs
			for i=1 to objs.count do
			(	local foundID = FindItem boneNameArray (bfAnim_CleanBoneName (lowercase objs[i].name))
			--	format "%_scene: % | % \n" i objs[i].name foundID
				if foundID > 0 then
					newObjArray[foundID] = objs[i]
				else (bf_fclose f; return false;)
			)
			objs = newObjArray
		--	format "newObjs: %\n" newObjArray
		)
	)
	
	local numFrames = bf_ReadLong f 
	local filePrecision = bf_ReadByte f 
	format "numFrames: %\t\tPrecision: %\n" numFrames filePrecision

	
	animationRange = interval 0 (numFrames - 1)
	
	format "isWeapon: %\n" isWeapon
	
	
	-- (quick fix) // Overly complicated method for fixing weapons animations
	local weapBaseTransform = undefined
	local weapBaseNode = undefined
	if isWeapon == true then
	(
		if numBones > 0 then
		(
			local myFamilyTree = traceFamilyOrigin objs[1]
			local matchText = "bip01 r hand"
			for i=2 to myFamilyTree.count do
			(
				if (lowercase myFamilyTree[i].name) == matchText then
				(
					weapBaseNode = myFamilyTree[i-1]
					exit
				)
			)
			
			format "  weapBaseNode: %\n" weapBaseNode
			
			if weapBaseNode != undefined then
			(
				weapBaseTransform = weapBaseNode.transform
				animate off
				(
					weapBaseNode.transform = matrix3 1
				)
				format " weapBaseTransform: %\n" weapBaseTransform
			)
		)
	)
	---------------------------------
	

	
	for i=1 to numbones do
	(
		if not doApplyBaf then format "--This bone Starts: %\n" (bf_ftell f)
		
		local numData = bf_ReadShort f 
		
		if not doApplyBaf then format "%_NumData: %\n" i numData

		local bnData = #() -- prepare place to store imported data
		for j=1 to numFrames do bnData[j] = #(0,0,0,0, 0,0,0)	

		for j=1 to 7 do
		(
			local curFrame = 0
			local dataLeft = bf_ReadShort f 
			if not doApplyBaf then format "  (%) Bn: % \tdataLeft: %\n" j i dataLeft
			while dataLeft > 0 do -- data left till next header
			(
				local TmpValu = bf_ReadByte f 
				local tmpBits = bafInt2Bits TmpValu 8
				
				local rleCompression = bit.get TmpValu 8
				deleteItem tmpBits 8
				local tNumFrames = bafBits2int tmpBits
				local tNxtHeader = bf_ReadByte f 
				
				if not doApplyBaf then
				(
					format "\t  rle: %" rleCompression
					format "\t  tNumFrames: %" tNumFrames
					format "\t  tNxtHeader: %\n" tNxtHeader
				)
					
				if rleCompression != true then
				(
					for m=1 to tNumFrames do
					(
						curFrame += 1
						local TmpValu = bf_ReadShort f 
						
						if j < 5 then 
							bnData[curFrame][j] = (ReadVariable16bitFloat TmpValu 15)
						else
							bnData[curFrame][j] = (ReadVariable16bitFloat TmpValu filePrecision)
						
						if not doApplyBaf do
						(						
							tmpBits = bafInt2Bits TmpValu 16;		
							format "\t%\t[V: % | Off: %]" (saySomeBits tmpBits) TmpValu ((bf_ftell f) - 2)
							format "\t%\n" bnData[curFrame][j]
						)
							
					)
					if not doApplyBaf do format "\n"
				)
				else
				(
					local TmpValu = bf_ReadShort f 
					for m=1 to tNumFrames do
					(	curFrame += 1
						if j < 5 then
							bnData[curFrame][j] = (ReadVariable16bitFloat TmpValu 15)
						else
							bnData[curFrame][j] = (ReadVariable16bitFloat TmpValu filePrecision)	
					)
					
					if not doApplyBaf do
					(
						tmpBits = bafInt2Bits TmpValu 16;
						format "\t%\t[V: % | Off: %]\n" (saySomeBits tmpBits) TmpValu ((bf_ftell f) - 2)
						format "\t%\n" bnData[curFrame][j]
					)
				)

				dataLeft -= tNxtHeader;
			)
		)
		
		local bWeapBaseNode = isvalidNode weapBaseNode

		
		-- ANIMATE this bone
		if doApplyBaf then
		(	
			
			with redraw off (undo off 
			(
			
			
				for j=1 to numFrames do
				(
					
					
					at time (j-1)
					(
						
						local tmpPos = [ bnData[j][5], bnData[j][7], bnData[j][6] ] * tScale
					--	format "%: %\n" (j-1) tmpPos
						
						-- format "%\t%" i (quat bnData[j][1] bnData[j][3] bnData[j][2] bnData[j][4])
						if isWeapon != true then
						(
							animate on 
							(
								if objs[i].parent != undefined then
								(
									in coordsys objs[i].parent objs[i].rotation = (bf2maxQuat bnData[j])
									in coordsys objs[i].parent objs[i].pos = tmpPos
								)
								else
								(	
									local tmpRot = bnData[j]
									tmpRot = bf2maxQuat tmpRot 
																
									objs[i].rotation = tmpRot
									objs[i].pos = tmpPos
								)	
							)
						)
						else
						(
							local tmpTransf
							if bWeapBaseNode then
							(
								tmpTransf = weapBaseNode.transform
								animate off
								( 
									weapBaseNode.transform = matrix3 1
								)
							)
							
							local tmpRot = bnData[j]
							tmpRot = bf2maxQuat tmpRot 
							
							tmpRot.x = -tmpRot.x
							tmpRot.y = -tmpRot.y				
							animate on 
							(
								objs[i].rotation = tmpRot
								objs[i].pos = tmpPos
							
								objs[i].transform = baf_weaponTransform_import objs[i].transform
							)
							
							if bWeapBaseNode then
							(
								animate off
								(
									weapBaseNode.transform = tmpTransf 
								)
							)
							
						)
						
						
					)
				)
				
				
			)
			)
			
			
		)
		
	
		
	)

	if isWeapon then
	(	
		if isValidNode weapBaseNode then
		(
			animate off
			(
				weapBaseNode.transform = weapBaseTransform 	
			)
		)
	)

	bf_fclose f
	select objs
	return objs
)





-------------------------------------------
---- Returns Animation data of an objectSet
fn gatherBAFdata1 objs StartFrame numFrames tScale isWeapon:false=
(
	format "GatherBAFdata1: (%)\n" tScale
	local theBnData = #()

	animate off
	(
	
	local tmpQuat,tmpRot,tmpPos
	local curTime = StartFrame


-----------------------------------	// Weapon Quick Fix

	-- Find the bip01 r hand node
	local weapBaseNode = undefined
	if isWeapon == true then
	(
		if objs.count > 0 then
		(
			local myFamilyTree = traceFamilyOrigin objs[1]
			local matchText = "bip01 r hand" as name
			for i=2 to myFamilyTree.count do
			(
				if (myFamilyTree[i].name as name) == matchText then
				(
					weapBaseNode = myFamilyTree[i-1]
					exit
				)
			)
			format "  weapBaseNode: %\n" weapBaseNode
		)
	)
	local bWeapBaseNode = isvalidNode weapBaseNode
------------------------------------

	for i=1 to objs.count do
	(
		if not (progressUpdate ( 50.0 * (i as float) / objs.count )) then
		(
			return false
		)
		
		curTime = StartFrame
		theBnData[i] = #()
		
		for j=1 to 7 do
			theBnData[i][j] = #()
			
		for m=1 to numFrames do
		(
			for j=1 to 7 do
				theBnData[i][j][m] = #(0,0,0,0, 0,0,0)
			
			at time curTime
			(
				local tmpTransformA = undefined
				if isWeapon != true then -- Not WEAPON
				(	
					tmpTransformA = objs[i].transform
					if objs[i].parent != undefined then
					(	
						tmpTransformA = tmpTransformA * (inverse objs[i].parent.transform)
					)
					
					tmpPos = tmpTransformA.pos
					tmpRot = tmpTransformA.rotation
					
	
					tmpRot.x = -tmpRot.x
					tmpRot.y = -tmpRot.y
					tmpRot.z = -tmpRot.z

	
					tmpQuat = max2BfQuat tmpRot
				
				)
				else -- WEAPON
				(
					local tmpTransf

					if bWeapBaseNode then
					(			
						tmpTransf = weapBaseNode.transform
						weapBaseNode.transform = matrix3 1
					)
					
					tmpTransformA = objs[i].transform
					
					tmpTransformA = baf_weaponTransform_import tmpTransformA
					
					tmpPos = tmpTransformA.pos
					tmpRot = tmpTransformA.rotation
					
					tmpRot.z = -tmpRot.z
	
					tmpQuat = max2BfQuat tmpRot
					
					if bWeapBaseNode then
					(
						weapBaseNode.transform = tmpTransf
					)
				)
				
				theBnData[i][1][m] = tmpQuat.x
				theBnData[i][2][m] = tmpQuat.y
				theBnData[i][3][m] = tmpQuat.z
				theBnData[i][4][m] = tmpQuat.w
			
			--	format "%_%: %\n" i m tmpPos
				
				tmpPos *= tScale
				
				
				theBnData[i][5][m] = tmpPos.x
				theBnData[i][6][m] = tmpPos.z
				theBnData[i][7][m] = tmpPos.y
				
			)
			curTime += 1
	 	)
	)
	

	)
	
	return theBnData;
)











fn getBAFprecis theBnData=
(
	local precis = 0
	local maxValue = 0.0
	
	local numFrames = theBnData[1][1].count
	for i=1 to theBnData.count do
	(	for m=1 to numFrames do
		(	for j=5 to 7 do
				if (abs theBnData[i][j][m]) > maxValue then maxValue = (abs theBnData[i][j][m])
		)
	)
	format "maxValue: %\n" maxValue
--	local widthExp
	for i=0 to 15 do
	(
	--	format "%\n" (pow 2 i)
		if maxValue < (pow 2 i) then
		(
			precis = 15 - i
			exit
		)
	)
 
	return precis 
)
-----------------------
--- WRITE .baf animation file
fn WriteBafFile thisFileName objs StartFrame numFrames tScale isWeapon:true =
(


	local timestamp_STRT = timeStamp()
	format "-- .Baf File EXPORT --"
	local f = undefined
	
	if objs.count < 1 do (bafFileHadError f 3;return()) -- nothing to export
	objs = sortArrayH objs -- sort the hierarchy correctly
	
	
	f = bf_fopen thisFileName "wb"
	if (f == undefined) do (bafFileHadError f 9;return()) -- fileNotFound
	
	format "% numFrames: %\n" thisFileName numFrames
	
	bf_writeLong f 3 -- header
	bf_writeShort f (objs.count)
	
	for i=1 to objs.count do
	(--	format "  %_: %\n" i objs[i].name
		bf_writeShort f (objs[i].name.count + 1)
		bf_writeString f objs[i].name
	)
	
	bf_writeLong f numFrames
	
	progressStart "Exporting .baf"
	local timeStart = timeStamp()
	
	
	
	
	local theBnData = gatherBAFdata1 objs StartFrame numFrames tScale isWeapon:isWeapon
	format "Gathering Data -- %\n" (timeStamp() - timeStart)
	if theBnData == false then
	(
		bf_fclose f
		progressEnd()
		return false
	)
	
	local precis = (getBAFprecis theBnData)
	bf_writeByte f precis
	
	timeStart = timeStamp()
	local tmpShort, TmpValu
	local offset2NextHeader, tmpStreamPos = 3
	for i=1 to objs.count do
	(	
		-- format "  update: %\n" ( 50 + 50.0 * (i as float) / objs.count )
		if not (progressUpdate ( 50 + 50.0 * (i as float) / objs.count )) then
		(
			bf_fclose f
			progressEnd()
			return false
		)
		
		local numData = 0 -- total data?
		local TheStreamZ = #()
		for j=1 to 7 do
		(
			TheStreamZ[j] = packRleStream theBnData[i][j] CloseEqual:6
			numData += TheStreamZ[j][2]
		)

		bf_writeShort f numData 
--		format "numData: %\n" numData
		
		for j=1 to 7 do
		(
			local tmpStreamPos = 3
			local tmpStream = TheStreamZ[j]
			-- print (tmpStream as string)
			bf_writeShort f tmpStream[2] -- number of 16-bit data values
				
			while tmpStreamPos <= tmpStream.count do
			(		
				local numFrames = tmpStream[tmpStreamPos+1]
				
				if (classof numFrames) != Integer then
				(
					format "ERROR! WriteBafFile(); numframes ! Integer: %\n" numFrames
					format " On Bone: \"%\"\n" objs[i].name
					format " j: %\n" j
					fclose f
					return()
				)
				local TmpValu = numFrames -- numFrames for this segment of values
				TmpValu = bit.set tmpValu 8 tmpStream[tmpStreamPos] -- rle or not
				bf_writeByte f TmpValu
				
				if tmpStream[tmpStreamPos] then -- isRLE
					offset2NextHeader = 2
				else -- is notRLE compressed
					offset2NextHeader = numFrames + 1 -- +1 is the header itself 16-bit value
		
				bf_writeByte f offset2NextHeader
				
				local tmpStarty = (tmpStreamPos + 2 )
				local tmpEndy = (tmpStreamPos + offset2NextHeader)
				
			--	format "tmpStarty: %\ttmpEndy: %\n" tmpStarty tmpEndy
				for m = tmpStarty to tmpEndy do
				(
					
					if j < 5 then tmpShort = ( MakeVariable16bitFloat tmpStream[m] 15 )
					else tmpShort = ( MakeVariable16bitFloat tmpStream[m] precis ) -- position
					
					bf_writeShort f tmpShort 
				)

				tmpStreamPos += offset2NextHeader + 1
			)
		)
	)
	bf_fclose f
	progressEnd()
	
	format "WriteFile -- %\n" (timeStamp() - timeStart)
	
	format "-- Done -- %\n" (timeStamp() - timestamp_STRT)
)


/*
--------------------
--- TEST Export
if (1 == 0) do
(
	tDebug = 1
	clearListener()
	tmpFileUse1 = "c:\\Tmp.baf"
	tmpFileUse2 = "D:/games/Bf 1942/Mods/SampleMod/animations/TestBox/TestBoxGo.baf"
	WriteBafFile tmpFileUse1 geometry 0 8 0.1
)

--------------------
--- TEST Import
if (1 == 1) do
(
	tDebug = 1
	clearListener()
	tmpFileUse1 = "C:\code\Baf\Release\\flagBlow.baf"
	tmpFileUse2 = "c:\\Bar1918Fire.baf"
	readBafFile tmpFileUse2 selection 10.0 false
)
*/
