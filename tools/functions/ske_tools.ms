-- Skeleton Import/Export (.ske files)
--
-- Last Updated: FEB 2004
-- Author:	Rex Hill

struct bfSKE
(
	fn nameBasedNodeFind thisName thisArray=
	(	
		local tmpCmp = thisName as name
		for i=1 to thisArray.count do
		(
			if tmpCmp == (thisArray[i] as name) 
				then return i
		)
		
		return 0
	),
	
	-- TODO: possible, create a dummy transform object instead of using a box mesh.
	fn makeBoxBones objs scale=
	(	
		scale = scale * 0.1

		local newNodes = #(),ParentNames = #(),NodeNames = #()
		for i=1 to objs.count do
		(	NodeNames[i] = objs[i].name
			newNodes[i] = box wireColor:objs[i].wirecolor length:(0.25*scale) width:(0.5*scale) height:(0.25*scale) name:NodeNames[i] transform:objs[i].transform	
			if objs[i].parent != undefined then
				ParentNames[i] = objs[i].parent.name
		)
	
		for i=1 to objs.count do
		(	if parentNames[i] != undefined do
			(
				local tmpp = (bfSKE.nameBasedNodeFind ParentNames[i] nodeNames)
				if tmpp > 0 then
					newNodes[i].parent = newNodes[tmpp]
			)
		)

		return newNodes
	)
)




-- Reset an object's transform without affecting its children
fn setTransform_NoAffectChildren obj newTransform=
(
	local childTransforms = #()
	for i=1 to obj.children.count do
		childTransforms[i] = obj.children[i].transform
		
	obj.transform = newTransform
	
	for i=1 to obj.children.count do
		obj.children[i].transform = childTransforms[i]
)

-- Give away nodeA's Children to nodeB
fn rexReAssignChildren nodeOldParent nodeNewParent =
(
	local tmpChildren = #()
	for j=1 to nodeOldParent.children.count do
		tmpChildren[j] = nodeOldParent.children[j]
		
	for j=1 to tmpChildren.count do
		tmpChildren[j].parent = nodeNewParent
)

-- Rotates a player skeleton so it is facing upright when viewed
fn ske_rotatePlayerImport =
(
	local bipper = $Bip01
	if isValidNode bipper then
	(	if bipper.parent == undefined then
		(
			local a = dummy name:"dummyROT"
			bipper.parent = a
			a.transform = (matrix3 [1,0,0] [0,0,-1] [0,1,0] [0,0,0]) 
		)
	)
)
-- Rotates a weapon so its base model has a normal transform matrix when viewed
fn ske_rotateWeaponImport bone1=
(
	if isValidNode bone1 then
	if (lowercase bone1.name) == "bip01 r hand" then
	(	if not (isValidNode bone1.parent) then
		(	if (isValidNode bone1.children[1]) then
			(	local a = dummy name:"dummyROT"
				bone1.parent = a
				a.transform = inverse bone1.children[1].transform
				return true
			)
		)
	)
)


fn ske_weaponTransform3TEST t =
(
	local t2 = copy t
	local tmpPos = t2.row4
	t2.rotation = quat -t.rotation.x -t.rotation.y t.rotation.z t.rotation.w
	t2.row4 = [ -tmpPos.x, -tmpPos.y, tmpPos.z ]
	return t2
)

--------------------------------
-- changes Axis alignment, rotate matrix <-> transform matrix
fn SwapMatrixType tmpMatrix =
(
	inputMatrix = copy tmpMatrix
	outputMatrix = (matrix3 0)
	
	outputMatrix.row1.x = inputMatrix.row1.x
	outputMatrix.row1.y = inputMatrix.row2.x
	outputMatrix.row1.z = inputMatrix.row3.x
	
	outputMatrix.row2.x = inputMatrix.row1.y
	outputMatrix.row2.y = inputMatrix.row2.y
	outputMatrix.row2.z = inputMatrix.row3.y
	
	outputMatrix.row3.x = inputMatrix.row1.z
	outputMatrix.row3.y = inputMatrix.row2.z
	outputMatrix.row3.z = inputMatrix.row3.z
	
	outputMatrix.row4 = inputMatrix.row4
	
	return outputMatrix
)

-------------------------------
fn SwapMatrixAxis tmpMatrix =
(
	inMatx = copy tmpMatrix
	outputMatrix = (matrix3 0)
	outputMatrix.row1 = [inMatx.row1.x, inMatx.row1.z, inMatx.row1.y]
	outputMatrix.row2 = [inMatx.row3.x, inMatx.row3.z, inMatx.row3.y]
	outputMatrix.row3 = [inMatx.row2.x, inMatx.row2.z, inMatx.row2.y]
	outputMatrix.row4 = [inMatx.row4.x, inMatx.row4.z, inMatx.row4.y]
	return outputMatrix
)


--------------------------
-----
fn viewMatrixSke tMatrx =
(
	try tMatrx = tMatrx.transform
	catch()
	tMatrx = SwapMatrixType tMatrx
	scalea = 0.1
	format "%\t%\t%\t%\n%\t%\t%\t%\n%\t%\t%\t%\n" \
		tMatrx.row1.x tMatrx.row1.z tMatrx.row1.y (tMatrx.row4.x*scalea) \
		tMatrx.row3.x tMatrx.row3.z tMatrx.row3.y (tMatrx.row4.z*scalea) \
		tMatrx.row2.x tMatrx.row2.z tMatrx.row2.y (tMatrx.row4.y*scalea)
)


----------------------------
------ creates bone structure from a zBonStruc array
fn boneStructCreate BoneLista scale isWeapon:false =
(
	scale = scale * 0.01
	
	if BoneLista.count > 0 then
	(	
		if (BoneLista[1].name as name) == #bip01 then isWeapon = false
	)
	
	local boneNode = #()
	local mAbsolute = #() -- non-relative absolute transform matrix
	local b_start_pos, b_end_pos
	
	local nodeHasKids = #() -- if that node as any children
	
	for i=1 to BoneLista.count do 
		nodeHasKids[i] = false
	

	-- store non-relative transform matricies
	for i=1 to BoneLista.count do
	(	
		if ( BoneLista[i].p == 0 ) then
			mAbsolute[i] = BoneLista[i].t
		else
		(
			mAbsolute[i] = BoneLista[i].t * mAbsolute[ BoneLista[i].p ]
			if (nodeHasKids[ BoneLista[i].p ] == false) do -- store first of kin
				nodeHasKids[ BoneLista[i].p ] = i
		)	
	)

	for i=1 to BoneLista.count do
	(

		if ( BoneLista[i].p != 0 ) then
		(
			b_start_pos = mAbsolute[i].row4

			if (nodeHasKids[i] != false) then
			(
				-- flaw, should find first child instead of next bone
				b_end_pos = mAbsolute[i+1].row4 
				
				theZUpAxis = mAbsolute[i].row3
				boneNode[i] = BoneSys.createBone b_start_pos b_end_pos theZUpAxis
				boneNode[i].transform = mAbsolute[i]
			)
			else
			(
				b_end_pos = b_start_pos
				theZUpAxis = mAbsolute[i].row3
				boneNode[i] = BoneSys.createBone b_start_pos b_end_pos theZUpAxis
				boneNode[i].transform = mAbsolute[i]
				boneNode[i].length = 1.0 * scale
				boneNode[i].wireColor = (color 20 0 150)
			)
			
			boneNode[i].parent = boneNode[ BoneLista[i].p ]
		)
		else
		(
			b_start_pos = mAbsolute[i].row4
			b_end_pos = b_start_pos
			
			-- extend root bone to reach first of kin
			if (nodeHasKids[i] != false) do
				b_end_pos = mAbsolute[ nodeHasKids[i] ].row4

			theZUpAxis = mAbsolute[i].row3
			boneNode[i] = BoneSys.createBone b_start_pos b_end_pos theZUpAxis
			
			boneNode[i].length = 1.0 * scale
			boneNode[i].transform = mAbsolute[i]
						
			boneNode[i].wireColor = (color 150 0 35)
		)

		boneNode[i].name = BoneLista[i].name
		boneNode[i].width = scale
		boneNode[i].height = scale
		
	)

	return boneNode
)


-----------------------
----  IMPORT FILE bone struct
fn getBoneData_1 filH numBonez isWeapon:false =
(
	local scale = 10.0

	struct zBoneStruct ( name, p, t );
	
	local BL = #()
	local nameLn, tmpName, hBoneFlag
	
	for i=1 to numBonez do
	(
		nameLn = bf_ReadShort filH
		tmpName = bfAnim_CleanBoneName (bf_ReadString filH)
		hBoneFlag = (bf_readShort filH) + 1
		if hBoneFlag > 65534 then hBoneFlag = 0

		BL[i] = zBoneStruct tmpName hBoneFlag (matrix3 0)

-- note: In .ske file, the axies are vertical down colums
--   x = right;  y = up;  z = away;
-- x-axis x | y-axis x | z-axis x
-- x-axis y | y-axis y | z-axis y
-- x-axis z | y-axis z | z-axis z

	---------
		BL[i].t.row1.x = (bf_ReadFloat filH)
		BL[i].t.row1.z = (bf_ReadFloat filH)
		BL[i].t.row1.y = (bf_ReadFloat filH)

		BL[i].t.row4.x = (bf_ReadFloat filH)*scale 
	---------	
		BL[i].t.row3.x = (bf_ReadFloat filH)
		BL[i].t.row3.z = (bf_ReadFloat filH)
		BL[i].t.row3.y = (bf_ReadFloat filH)

		BL[i].t.row4.z = (bf_ReadFloat filH)*scale
	---------		
		BL[i].t.row2.x = (bf_ReadFloat filH)
		BL[i].t.row2.z = (bf_ReadFloat filH)
		BL[i].t.row2.y = (bf_ReadFloat filH)

		BL[i].t.row4.y = (bf_ReadFloat filH)*scale
	---------	
		
		-- convert the vertically aligned axies to horizontal transform axies
		BL[i].t = SwapMatrixType BL[i].t
		
	--	if i == 1 and (lowercase tmpName) == "bip01" then isWeapon = false
		if isWeapon then 
		(
			BL[i].t = ske_weaponTransform3TEST BL[i].t
		)
		
		-- format "%_HERE: %\n" i tmpName
		-- print (BL[i].t as string)
	)
	return BL
)

-------------------------
--- 1) Read a .ske file into a zBoneStruct array
--- 2) Create bones from that array
fn importBfskeA thisFileName S isWeapon:false=
(
	format "Import File: %\n" thisFileName
	local f = bf_fopen thisFileName "rb"
	if f != undefined then
	(
		local header = bf_ReadLong f
		if header == 1 then
		(
			local numBones = bf_ReadLong f
			format " NumBones: %\n" numBones
			
			local existingRHand = getNodeByName "Bip01 R Hand"
			format " existingRHand: %\n" existingRHand
			format " IsWeapon: %\n" isWeapon
			
			local bonelist = getBoneData_1 f numBones isWeapon:isWeapon
			
			bf_fclose f

			local tmplist = boneStructCreate bonelist S
	
			
			if isWeapon then
			(
				if isValidNode tmplist[1] then
				(
					-- make it into boxes, since it is a weapon
					(
						local tmpObjs = bfSKE.makeBoxBones tmplist S
						delete tmplist
						tmplist = tmpObjs
					)
					
					-- make Hand conform
					(
						tmplist[1].transform = matrix3 [-1,0,0] [0,-1,0] [0,0,1] [0,0,0]
						setTransform_NoAffectChildren tmplist[1] (matrix3 1)
					)
					
					-- Attach myself to Bip01 R Hand if in scene already
					if isValidNode existingRHand then
					(
						tmplist[1].transform = existingRHand.transform
						
						rexReAssignChildren tmplist[1] existingRHand 
							
						delete tmplist[1]
						tmpList[1] = existingRHand
							
						select tmplist 
					)
					
				)
				else format "Error not a valid node: %\n" tmplist[1]
				
			) -- END if isWeapon
	
			return tmplist
		)
		else format "ERROR! Non-supported header: %\n" header
	)
	else format "ERROR! Could not open: %\n" thisFileName
	
	return #()
)

---------------------
--------(.ske) Skeleton Export Functions------------

fn StupidArrayFind findNode thisArray =
(	
	for n=1 to thisArray.count do 
		if findNode == thisArray[n] then return n
			
	return 0
)


fn sortNodeArrayAlphabetical thisArray =
(

		local namesLC = #()
		local namesLC_sort = #()
		for i=1 to thisArray.count do
		(
			namesLC[i] = lowercase thisArray[i].name
			namesLC_sort[i] = lowercase thisArray[i].name
		)
		sort namesLC_sort
		
		local finalArray = #()
		for i=1 to thisArray.count do
		(
			local findInx = findItem namesLC namesLC_sort[i]
			if findInx != 0 then
			(
				finalArray[i] = thisArray[findInx]
			)
		)
	
		return finalArray
		
)

------------------
-- Hierarchical sorting of nodes for skeleton export
fn sortArrayH thisArray =
(
	if thisArray.count > 1 then
	(

		--------------------------------------	
		-- For tank treads, sort alphabetical
		--
		local hasParentNode = false
		for i=1 to thisArray.count do
		(
			if thisArray[i].parent != undefined then
			(
				hasParentNode = true
				exit
			)
		)
		
		if hasParentNode == false then
		(
			local namesLC = #()
			local namesLC_sort = #()
			for i=1 to thisArray.count do
			(
				namesLC[i] = lowercase thisArray[i].name
				namesLC_sort[i] = lowercase thisArray[i].name
			)
			sort namesLC_sort
			
			local finalArray = #()
			for i=1 to thisArray.count do
			(
				local findInx = findItem namesLC namesLC_sort[i]
				if findInx != 0 then
				(
					finalArray[i] = thisArray[findInx]
				)
			)
		
			return finalArray
		)
		--------------------------------------		
		
		
		local endArray = #()
		local tmpVal
		
		for i=1 to thisArray.count do
		(		
			tmpVal = StupidArrayFind thisArray[i] objects
			if tmpVal != 0 then endArray[tmpVal] = thisArray[i]
			else format "ERROR!! sortArrayH() COULD NOT FIND MATCHING NODE in geometry: %\n" thisArray[i]
		)
		
		-- remove any nodes left undefined by other objects in scene geometry heirarchy
		local tmpEndArray = #()
		for i=1 to endArray.count do
		(	
			if endArray[i] != undefined then tmpEndArray[tmpEndArray.count + 1] = endArray[i]
		)
	
		return tmpEndArray
	)
	else
		return thisArray
)






------------------
---
fn ske_GetBones_FromScene objs isWeapon:false =
(
	-- Sort the objs #() array based on heirarchy
	-- roots, then children, next generation, etc..
	
	if objs.count > 1 do objs = sortArrayH objs
	
	local BL = #()
	struct zBoneStruct ( name, p, t )
	local parentID, transform_Matrix
	
	local bone_1_Transform
	
	for i=1 to objs.count do
	(
	
	
		-- make Hand conform
		if i == 1 then
		(
			if isWeapon then
			(
				-- backup original transform
				bone_1_Transform = objs[i].transform
				
				-- Temporarily set the transform
				objs[i].transform = (matrix3 1)
				setTransform_NoAffectChildren objs[i] (matrix3 [-1,0,0] [0,-1,0] [0,0,1] [0,0,0])
			)
		)
		
		
		
		if objs[i].parent != undefined then -- has a parent
		(
			parentID = StupidArrayFind objs[i].parent objs
			transform_Matrix = objs[i].transform * (inverse objs[i].parent.transform)	
		)
		else -- is a root, has no parent
		( 	parentID = 0
			transform_Matrix = objs[i].transform
		)
		
		
		
		if isWeapon then
		(
			transform_Matrix = ske_weaponTransform3TEST transform_Matrix	
		)	
		
		
		BL[i] = zBoneStruct objs[i].name parentID transform_Matrix
	)
	
	-- restore Hand conform
	if isWeapon then
	(
		if objs.count > 0 then
		(	
			setTransform_NoAffectChildren objs[1] (matrix3 1)
			objs[1].transform = bone_1_Transform 
		)
	)
			
	return BL
)

--------------------
--- Writes the .ske file from structured data: name, parent, transform
fn Write_Ske thisFile BoneList S isWeapon:false =
(
	if (classof BoneList) != Array then
	(
		format "ERROR! (classof BoneList) != Array: %\n" BoneList 
		return()
	)
	
	local f = bf_fopen thisFile "wb"
	if f != undefined then
	(	
		format "Writing: %\n" thisFile
		
		bf_writeLong f 1 -- header
		bf_writeLong f Bonelist.count
		
		for i=1 to Bonelist.count do
		(
			format "%_: %\n" i Bonelist[i].t
			
			bf_writeShort f (BoneList[i].name.count + 1) 
			bf_writeString f BoneList[i].name
			bf_WriteShort f (BoneList[i].p - 1)
			
			-- convert to vertical Axis style transform matrix
			BoneList[i].t = SwapMatrixType BoneList[i].t 

			bf_writeFloat f BoneList[i].t.row1.x
			bf_writeFloat f BoneList[i].t.row1.z
			bf_writeFloat f BoneList[i].t.row1.y
			bf_writeFloat f (BoneList[i].t.row4.x * S)

			bf_writeFloat f BoneList[i].t.row3.x
			bf_writeFloat f BoneList[i].t.row3.z
			bf_writeFloat f BoneList[i].t.row3.y
			bf_writeFloat f (BoneList[i].t.row4.z * S)

			bf_writeFloat f BoneList[i].t.row2.x
			bf_writeFloat f BoneList[i].t.row2.z
			bf_writeFloat f BoneList[i].t.row2.y
			bf_writeFloat f (BoneList[i].t.row4.y * S)
		)
		
		bf_fclose f
	)
	else format "ERROR creating: %\n" thisFile
)


/*
(
	ClearListener()
	a = importBfskeA "C:\\_bf1942\\animations\\Bar1918.ske" 100.0
)
*/