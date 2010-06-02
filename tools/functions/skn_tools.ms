------------------------------------
--- Physique Modifier (.skn export)
fn writeBfsknA_Physique thisFileName sknMesh DsknScale =
(
	if (physiqueOps as string) != "<Interface:physiqueOps>" do
		messagebox ("ERROR!!!!\nMissing physique ScriptInterface plugin named: IPhysique.gup\n\n" + \
			"It should be included in the .zip file and a folder named: /3Dmax_plugins/\n\n" + \
			"Copy IPhysique.gup to your 3dsmax plugins directory as: \\3dsmax5\\Plugins\\IPhysique.gup\nThen exit and re-launch 3dsmax for it to take effect.")
	
	print "Exporting Physique -> .skn"
	local tmpWeight,tmpBoneNode
	
	physiqueOps.setInitialPose sknMesh true
 
	numVerts = physiqueOps.getVertexCount sknMesh
	UsedBonesArray = physiqueOps.getBones sknMesh
	vtxList = #()
	vtxListWieghtCnt = #()
	for i=1 to numVerts do
	(
		WeightCnt = physiqueOps.getVertexBoneCount sknMesh i
		-- print ((i as string) + "_WeightCnt: "+(WeightCnt as string))
		if WeightCnt > 0 do
		(
			vtxList[vtxList.count + 1] = i
			vtxListWieghtCnt[vtxList.count] = WeightCnt 
		)
	)
	numSknVerts = vtxList.count
	-- print ("numSknVerts: " + (numSknVerts as string))
	
	f=bf_fopen thisFileName "wb"
	if (f != undefined) do
	(
		bf_writeLong f 1 -- header
		bf_writeLong f numSknVerts
		for i=1 to numSknVerts do
		(
			tmpVtxPos = getVert sknMesh vtxList[i]
			bf_writeFloat f (tmpVtxPos.x * DsknScale)
			bf_writeFloat f (tmpVtxPos.z * DsknScale)
			bf_writeFloat f (tmpVtxPos.y * DsknScale)
			
			bf_writeByte f vtxListWieghtCnt[i] 
			
			for j=1 to vtxListWieghtCnt[i] do
			(
				tmpBoneNode = physiqueOps.getVertexBone sknMesh vtxList[i] j
				tmpBoneId = StupidArrayFind tmpBoneNode UsedBonesArray
				bf_writeShort f (tmpBoneId-1) 
				
				tmpWeight = physiqueOps.getVertexWeight sknMesh vtxList[i] j
				bf_writeFloat f tmpWeight
	
				tmpRelPos = physiqueOps.getVertexOffset sknMesh i j			
				tmpRelPos = tmpRelPos * DsknScale
				bf_writeFloat f tmpRelPos.x
				bf_writeFloat f tmpRelPos.z
				bf_writeFloat f tmpRelPos.y
			)
		)
		
		bf_fclose f
	)

	physiqueOps.setInitialPose sknMesh false
	
	for i=1 to UsedBonesArray.count do
		print UsedBonesArray[i].name as string
	-- print UsedBonesArray as string
)



-----------------
--- TEST
if (1 == 0) do
(	tDebug = 1
	clearListener()
	obja = selection[1]
	if obja != undefined do
		writeBfsknA_Physique "c:\\tmp1.skn" obja 0.1
)




-----------------------------------
--- Skin Modifier (.skn export)
fn writeBfsknA_skin thisFileName sknMesh sknModifier DsknScale =
(
	local tmpBoneId, tmpWeight
	local numVerts = skinOps.GetNumberVertices sknModifier
	local numBones = skinOps.GetNumberBones sknModifier
	
	local numUsedBones = numBones
	
	local isEP = false
	if classof sknMesh == PolyMeshObject or classof sknMesh == Editable_Poly then
	(
		isEP = true
	)
	
	local vtxList = #()
	local vtxListWieghtCnt = #()
	for i=1 to numVerts do
	(
		local WeightCnt = skinOps.GetVertexWeightCount sknModifier i
	--	print ((i as string) + "_WeightCnt: "+(WeightCnt as string))
		if WeightCnt > 0 do
		(
			append vtxList i
			vtxListWieghtCnt[vtxList.count] = WeightCnt 
		)
	)
	numSknVerts = vtxList.count
	print ("numSknVerts: " + (numSknVerts as string))
	
	local tmpVtxPos
	
	local f = bf_fopen thisFileName "wb"
	if f != undefined then
	(
		bf_writeLong f 1 -- header
		bf_writeLong f numSknVerts
		for i=1 to numSknVerts do
		(
		--	print ( (i as string) + "_ cnt: " + (vtxListWieghtCnt[i] as string) )
			if isEP then
				tmpVtxPos = polyop.getVert sknMesh vtxList[i]
			else
				tmpVtxPos = getVert sknMesh vtxList[i]
			
			bf_writeFloat f (tmpVtxPos.x * DsknScale)
			bf_writeFloat f (tmpVtxPos.z * DsknScale)
			bf_writeFloat f (tmpVtxPos.y * DsknScale)
			
			bf_writeByte f vtxListWieghtCnt[i] 
			for j=1 to vtxListWieghtCnt[i] do
			(
				local tmpBoneId = SkinOps.GetVertexWeightBoneID sknModifier vtxList[i] j
				bf_writeShort f (tmpBoneId-1) 
				
				local tmpWeight = SkinOps.GetVertexWeight sknModifier vtxList[i] j
				bf_writeFloat f tmpWeight
				
				local tmpBone = getNodeByName (SkinOps.GetBoneName sknModifier tmpBoneId 1)
				
				local tmpRelPos = tmpVtxPos * (inverse tmpBone.transform)
				-- print (tmpRelPos as string)
				
				tmpRelPos = tmpRelPos * DsknScale
				bf_writeFloat f tmpRelPos.x
				bf_writeFloat f tmpRelPos.z
				bf_writeFloat f tmpRelPos.y
			)
		)
		
		local tmpName
		bf_writeShort f numUsedBones 
		for i=1 to numUsedBones do
		(
			tmpName = SkinOps.GetBoneName sknModifier i 1
			bf_writeShort f (tmpName.count+1) 
			bf_writeString f tmpName
			
			format "%\n" tmpName
		)
		 
		bf_fclose f
	)
)


------------------------------------
--- .SKN EXPORT
fn writeBfsknA sknMesh DsknScale =
(
	local sknModifier
	if (sknMesh == undefined) then 
	(	
		format "ERROR: (sknMesh == undefined)\n"
		return()
	)
	
	hasSkinMod = false;
	hasPhysiqueMod = false;
	try 
	(	sknModifier = sknMesh.skin
		hasSkinMod = true;
	)
	catch()
	
	if g_BFisGmax then
	(
		if not hasSkinMod then
		(
			MessageBox "ERROR!!!\n\nNo Skin modifer could be found on the selected object"
			return()
		)
	)
	else
	(
		if (hasSkinMod != true) do
		(	try
			(	sknModifier = sknMesh.physique
				hasPhysiqueMod = true;
			)
			catch
			(	MessageBox "ERROR!!!\n\nNo Physique or Skin modifer could be found on the selected object"
				return()
			)
		)
	)
	
	
	local fname = getSaveFileName caption:"Export .skn" types:"Skin .skn|*.skn|All|*.*|"
	if fname != undefined do
	(	
		print ("Saving: " + fname)
		
		max modify mode
		
		if hasSkinMod then
			writeBfsknA_skin fname sknMesh sknModifier DsknScale
		else
		(	if hasPhysiqueMod do
				writeBfsknA_Physique fname sknMesh DsknScale
		)
	)
)
