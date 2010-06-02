fn importSMcollision thisaFile skiplast extraScale =
(
	if thisaFile != undefined do
	(
		local fi_name = lowercase (getFileNameFile thisaFile)
		
		local objectName = getFilenameFile thisaFile
		format "Reading: % | %\n" thisaFile extraScale
		local f=bf_fopen thisaFile "rb"
	--	try
		(
			local DSMscale = extraScale

			SM_HeaderV = bf_ReadLong f
			if SM_HeaderV == 10 or SM_HeaderV == 9 then
			(
				bf_fseek f 28 #seek_cur

				if SM_HeaderV == 10 do
					bf_fseek f 1 #seek_cur

				local numTopModels = bf_ReadLong f
				if numTopModels <= 0 then
				(
					format "No collision models found in this file\n"
					bf_fclose f
					return()
				)
				else format "NumCollisionModels: %\n" numTopModels
	
				local SizeOfSection = 0
				local endOffset = 0
	
				if skiplast do
				(
					for i=1 to (numTopModels-1) do
					(
						SizeOfSection = (bf_ReadLong f )
						endOffset = ( bf_ftell f )+SizeOfSection
						bf_fseek f endOffset #seek_set
					)
					numTopModels = 1
				)
				
	
				for i=1 to numTopModels do
				(		
					qVerts = #()
					VertMod = #()
		
					qFaces = #()
					FaceMod = #()
				
					SizeOfSection = bf_ReadLong f
					endOffset = (bf_ftell f)+SizeOfSection
				
				
						-- these 8 bytes , a header
					bf_fseek f 8 #seek_cur
						
					numVerts = (bf_ReadLong f )
					for j=1 to numVerts do
					(	
						vert1 = (bf_ReadFloat f)*DSMscale
						vert2 = (bf_ReadFloat f)*DSMscale
						vert3 = (bf_ReadFloat f)*DSMscale
						VertMod[j] = (bf_ReadShort f ) -- MatID
						bf_fseek f 2 #seek_cur -- Stuff of vert1 Repeated
						qVerts[j] = (point3 vert1 vert3 vert2)
					)

					numFaces = bf_ReadLong f
					for j=1 to numFaces do
					(	
						FaceA = (bf_ReadShort f )+1
						FaceB = (bf_ReadShort f )+1 
						FaceC = (bf_ReadShort f )+1
						FaceMod[j] = (bf_ReadShort f ) -- MatID
							if FaceMod[j] == 0 do FaceMod[j] = 10000 -- 10000 = 0
						qFaces[j] = (point3 FaceA FaceB FaceC)
					)
					bf_fseek f endOffset #seek_set

					CollisionMesh = mesh Vertices:qVerts faces:qFaces materialIDS:FaceMod
					
					CollisionMesh.name = "COL0" + (i as string) + "_" + fi_name
					
				
				)
				
			)
			else format  "ERROR: Unsupported header type\n"
		)
	--	catch (Print "ERROR when reading")
		bf_fclose f
	)
)


fn alignMeshIdsToMat meshNode MatNames=
(
	local matEdNames = #()
	for i=1 to meshNode.material.count do
		matEdNames[i] = meshNode.material[i].name

--	format "alignMeshIdsToMat: %\n%\n" meshNode MatNames
--	format "%\n" matEdNames
	local newMatIds = #()
	for i=1 to meshNode.material.count do
		newMatIds[i] = findItem MatNames matEdNames[i]
--	format "newIDS: %\n" newMatIDs
	for i=1 to meshNode.numFaces do
	(
		local tmpMatID = getFaceMatID meshNode i
		local inxFound = findItem newMatIds tmpMatID
		if inxFound > 0 then setFaceMatID meshNode i inxFound
	)

)

---------------------------------------
---------------------------------------
fn readSMVisMesh thisFile Dascale justTopLod noAttach:false weldSameNormals:false=
(
	local f = bf_fopen thisFile "rb"
	if f == undefined then
	(	 
		format "ERROR: Could not open File %\n" thisFile
		return undefined
	)

	local fi_name = lowercase (getFileNameFile thisFile)
	
	local SM_HeaderV = bf_ReadLong f
	local finalMesh = #()
	local b_parts = #()
	
	undo off
	with redraw off
	if SM_HeaderV <= 10 then
	(
		local matName= #()
		local theObjectMaterial 
		
		bf_fseek f 28 #seek_cur
		if SM_HeaderV == 10 do
			bf_fseek f 1 #seek_cur
		local numTopModels = bf_ReadLong f
		local tmpSeek
		for i=1 to numTopModels do -- collision mesh skip
		(	
			tmpSeek = bf_ReadLong f
			bf_fseek f tmpSeek #seek_cur
		)
	
		local numLodMeshes = bf_ReadLong f 
		--format "numLodMeshes: %\t offset: %\n" numLodMeshes ( bf_ftell f)
		
		local numLodMeshesExport = numLodMeshes
		if justTopLod == true do numLodMeshesExport = 1
		
		
		for a=1 to numLodMeshesExport do
		( 
			local numMaterials = bf_ReadLong f
			--format " %_numMaterials: %\n" a numMaterials
			
			if a == 1 then 
				theObjectMaterial = multiSubMaterial numSubs:numMaterials name:fi_name

			local renderType = #()
			local vertexFormat = #()
			VertByteSize = #()
			numVerts = #()
			numFaces = #()
			matName[a] = #()
			for i=1 to numMaterials do
			(	
				local nameLn = bf_ReadLong f
				matName[a][i] = bf_readString2 f nameLn			
					
				bf_fseek f 12 #seek_cur
				renderType[i] = bf_ReadLong f
				vertexFormat[i] = bf_ReadLong f
				VertByteSize[i] = bf_ReadLong f
				numVerts[i] = bf_ReadLong f
				numFaces[i] = bf_ReadLong f
				
				--format "   %_MaterialName: %\n" i matName[a][i]
				--format "     renderType: %\n" renderType[i]
				--format "     VertByteSize: %\n" VertByteSize[i]
				--format "     vertexFormat: %\n" vertexFormat[i]		
				--format "     numVerts: %\n" numVerts[i]
				--format "     numIndicies: %\n" numFaces[i]
				bf_fseek f 4 #seek_cur
			)

			b = #()
			for n=1 to numMaterials do
			(
				if (vertexFormat[n] != 1041) then -- reg
				(	if (vertexFormat[n] != 9233) then -- lightmap
					(	format "ERROR! vertexFormat: %\n" vertexFormat
						bf_fclose f; return undefined;
					)
				)
				if (renderType[n] != 4) and (renderType[n] != 5) then
				(	format "ERROR! renderType: %\n" renderType[n]
					bf_fclose f; return 1;
				)
				if (VertByteSize[n] != 32) then
				(	if (VertByteSize[n] != 40) then
					(	if (VertByteSize[n] != 64) then
						(
							format "ERROR! VertByteSize: %\n" VertByteSize[n]
							bf_fclose f; return 1;
						)
					)
				)
				
			--	format " Before Verts!: %\n" (bf_ftell f)
				
				realVerts = #()
				Vnormal = #()
				Tverts = #()
				lightmapVerts = #()
				for i=1 to numVerts[n] do
				(
					realVerts[i] = [0,0,0]
					realVerts[i].x = (bf_ReadFloat f)*Dascale
					realVerts[i].z = (bf_ReadFloat f)*Dascale
					realVerts[i].y = (bf_ReadFloat f)*Dascale
					
					Vnormal[i] = [0,0,0]
					Vnormal[i].x = bf_ReadFloat f
					Vnormal[i].z = bf_ReadFloat f
					Vnormal[i].y = bf_ReadFloat f
					--format " %_pos: %, %, %\n" i realVerts[i].x realVerts[i].z realVerts[i].y
					--format "   normal: %_: %, %, %\n" i Vnormal[i].x Vnormal[i].z Vnormal[i].y
					--if ( (normalize Vnormal[i]) != Vnormal[i]) then
					--	format "   Error! normal: %_: %, %, %\n" i Vnormal[i].x Vnormal[i].z Vnormal[i].y

					
					Tverts[i] = [0,0,0]
					Tverts[i].x = bf_ReadFloat f
					Tverts[i].y = 1.0 - (bf_ReadFloat f)

					if (VertByteSize[n] == 40) then
					(						
						lightmapVerts[i] = [0,0,0]
						lightmapVerts[i].x = bf_ReadFloat f
						lightmapVerts[i].y = 1.0 - (bf_ReadFloat f)
					)
				)
				
				if (VertByteSize[n] == 64) then
				(
					for i=1 to numVerts[n] do
					(
						lightmapVerts[i] = [0,0,0]
						lightmapVerts[i].x = bf_ReadFloat f
						lightmapVerts[i].y = 1.0 - (bf_ReadFloat f)
						
						bf_fseek f 24 #seek_cur
					)
				)
				

				
				--bf_fclose f; return 1;
				
				local numRealFaces = numFaces[n]
				if renderType[n] == 4 then -- triangleList
					numRealFaces = numFaces[n] / 3
				else
				(	if renderType[n] == 5 then -- triangleStrip
						numRealFaces = numFaces[n] - 2
				)
				
				--format "  %_numRealFaces: %\n" n numRealFaces
				

				
				local faceValues = #()
				for i=1 to numFaces[n] do -- numFace values
					faceValues[i] = (bf_ReadShort f) + 1
				
				local facelist = #([1,2,3])
				if numRealFaces > 0 then
				(
					facelist = #( [faceValues[1],faceValues[2],faceValues[3]] )
					if renderType[n] == 5 then
					(--	format "TriangleStrip\n"
						for i=2 to numRealFaces do
						(
							facelist[i] = [1,2,3] -- placeholder point3
							
							facelist[i].x = facelist[i - 1].y
							facelist[i].y = facelist[i - 1].z
							facelist[i].z = faceValues[i + 2]
							-- format " %_: %\n" i facelist[i]
						)
						for i=1 to numRealFaces by 2 do
							facelist[i] = [facelist[i].z,facelist[i].y,facelist[i].x]
					)
					else
					(--	format "TriangleList\n"
						local thisFace = 1
						for i=1 to numRealFaces do
						(
							facelist[i] = [ faceValues[thisFace+2], faceValues[thisFace+1], faceValues[thisFace+0] ]				
							thisFace += 3
						)
					)
					
					
					tmpMatIDs = #()
					for i=1 to numRealFaces do
						tmpMatIDs[i] = n 
					

				)


			-------------------------
				local faceList2 = #()
				if weldSameNormals == true and numRealFaces > 0 then
				(
					--format "WeldSameNormals!\n"
					
					-- build a verts used by table
					local vertsUsedBy = #()
					
					for i=1 to numVerts[n] do
						vertsUsedBy[i] = #()
					
					local tmpFace
					for i=1 to numRealFaces do
					(
						tmpFace = facelist[i]
						append vertsUsedBy[ tmpFace[1] ] [i,1]
						append vertsUsedBy[ tmpFace[2] ] [i,2]
						append vertsUsedBy[ tmpFace[3] ] [i,3]
					)
					
					
					
					local tmpPosVert
					local tmpPt2
					
					local VertIsIsolated = #()
					local facesToWeldAfter = #()
					for i=1 to numVerts[n] do
					(
						tmpPosVert = realVerts[i]
						for j=(i+1) to numVerts[n] do
						(
							if VertIsIsolated[j] != true then
							(
								if tmpPosVert == realVerts[j] then
								(
									if Vnormal[i] == Vnormal[j] then
									(
										if Tverts[i] == Tverts[j] then
										(
											if VertByteSize[n] > 39 then
											(
												if lightmapVerts[i] != lightmapVerts[j] then continue
												else append facesToWeldAfter [i, j]
											)
											
					
											-- format "WELD! % %\n" i j
											-- tell faces to stop using this other vert
											for k=1 to vertsUsedBy[j].count do
											(
												tmpPt2 = vertsUsedBy[j][k]
									
												facelist[ tmpPt2[1] ][ tmpPt2[2] ] = i
											)
											-- This vert is no longer used
											VertIsIsolated[j] = true;
										
										--	format "  weld Now %\n" j
										)
										else
										(
										--	format "  weld after\n"
											append facesToWeldAfter [i, j]
										)
										
									)
								--	else format "Diff n: % %| % %\n" i j Vnormal[i] Vnormal[j]

								)
								--else format "Diff n: % %| % %\n" i j tmpPosVert realVerts[j]

							)
						
						)
					)
					
					
					
					( -- facelist2 is used for the visual verts, textures use faceList
						for i=1 to numRealFaces do
						(
							facelist2[i] = copy facelist[i]
						)
						
						local tmpB, tmpPt2
						local VertIsIsolated = #()
						for i=1 to facesToWeldAfter.count do
						(
							tmpB = facesToWeldAfter[i]
							if VertIsIsolated[ tmpB[2] ] != true then
							(
								for k=1 to vertsUsedBy[ tmpB[2] ].count do
								(
									tmpPt2 = vertsUsedBy[ tmpB[2] ][k]
									facelist2[ tmpPt2[1] ][ tmpPt2[2] ] = tmpB[1]
								)
								VertIsIsolated[ tmpB[1] ] = true
							)
						)
					)
				)
				



		-------------------------
				
				
				if numRealFaces > 0 then
				(
					if faceList2.count > 0 then
						b[n] = mesh name:("LOD0" + (a as string) + "_" + fi_name + "-" + (n as string)) vertices:realVerts faces:facelist2 materialIDS:tmpMatIDs
					else
						b[n] = mesh name:("LOD0" + (a as string) + "_" + fi_name + "-" + (n as string)) vertices:realVerts faces:facelist materialIDS:tmpMatIDs
					
					for i=1 to b[n].numFaces do setFaceSmoothGroup b[n] i 1
				

					if VertByteSize[n] > 39 then meshop.setNumMaps b[n] 4
					else meshop.setNumMaps b[n] 2
					
					meshop.setmapsupport b[n] 1 true
					meshop.setNumMapVerts b[n] 1 tverts.count
	
	
					
					for i=1 to facelist.count do
						setTVface b[n] i facelist[i]
		
					for i=1 to tverts.count do
							meshop.setMapVert b[n] 1 i tverts[i]
							
					if VertByteSize[n] > 39 then
					(
						meshop.setmapsupport b[n] 3 true
						meshop.setNumMapVerts b[n] 3 lightmapVerts.count
						
						for i=1 to facelist.count do
							meshop.setMapFace b[n] 3 i facelist[i]
							
						for i=1 to lightmapVerts.count do
							meshop.setMapVert b[n] 3 i lightmapVerts[i]
					)
					
				
				
				)
				else
					b[n] = mesh name:("LOD0" + (a as string) + "_" + fi_name + "-" + (n as string)) vertices:#([0,0,0],[0,0,0],[0,0,0]) faces:faceList

				
				
				meshop.deleteIsoVerts b[n]
			)

			
			
			if (not noAttach) then
			(
				for i=2 to b.count do attach b[1] b[i]
			)
			
			if b.count > 0 then
			(
				-- b[1].name = "LOD0" + (a as string) + "_" + fi_name
				finalMesh[a] = b[1]
				b_parts[a] = b
			)
		)
		
		-- format "FilePos: %\n" (bf_ftell f)
		
		bf_fclose f
		
		-- Prevent adding more and more of the same material to scene 10mb or so each
		for i=1 to (sceneMaterials.count-1) do
		(
			if sceneMaterials[i].name == theObjectMaterial.name then
				deleteItem sceneMaterials i
		)
		--if includeMatChk then
		(
			local rsFilename = ((getFilenamePath thisFile) + (getFilenameFile thisFile) + ".rs")
			if (existFile rsFilename) then
			(
				local tmpMaterial = rsToMaxMaterial (getFilenameFile thisFile) (rsReadFile rsFilename)		
				for i=1 to finalMesh.count do
				(
					finalMesh[i].material = tmpMaterial
					alignMeshIdsToMat finalMesh[i] matName[i]
					
					if noAttach then
					(
						for j=1 to b_parts[i].count do
						(
							if isValidNode b_parts[i][j] then
							(
								b_parts[i][j].material = tmpMaterial
							--	alignMeshIdsToMat finalMesh[i] matName[i]
							)
						)
					)

				)
					
				medit.PutMtlToMtlEditor tmpMaterial 1
			)
			-- else format "ERROR, does not exist: %\n" rsFilename
			
		)
		

			
		return finalMesh
	)
	else format  "ERROR: Unsupported file type\n"

	bf_fclose f

	
	return undefined
)

fn importSMSimple thisFile scale:10.0=
(
	readSMVisMesh thisFile scale false
)
--------------------------

-- ce_cwallb1_m1
--
/*
(	tDebug = 1
	delete geometry
	fname = "d:/B17_Fus_m1.sm" -- "d:\\zero_fus_m1.sm"
	clearListener()
	if fname != undefined then
		readSMVisMesh fname 10.0 true false true
)
*/

---------- SHADOW IMPORT -----------
fn importShadow thisFile offset_1 Scaleo =
(
	local f = bf_fopen thisFile "rb"
	if f != undefined then
	(
		bf_fseek f offset_1 #seek_set
		
		bf_fseek f 16 #seek_cur -- data from last mesh
		
		local shadowName = bf_ReadString f
	--	print ((( bf_ftell f) as string)+"_ShadowName: "+shadowName)
	
		shadowName = "shadow_" + (lowercase (getFilenameFile thisFile))
		if (bf_ReadLong f) == 0 then
		(
			bf_fseek f 19 #seek_cur
			numVerts = bf_ReadLong f 
			numFaces = (bf_ReadLong f ) / 3
		--	print ( "Verts: "+(numverts as string)+"\tFaces: "+(numFaces as string) )
			
			theVerts = #()
			bf_fseek f 4 #seek_cur
			for i=1 to numVerts do
			(
				theVerts[i] = [0,0,0]
				theVerts[i].x = (bf_ReadFloat f)*Scaleo
				theVerts[i].z = (bf_ReadFloat f)*Scaleo
				theVerts[i].y = (bf_ReadFloat f)*Scaleo
				bf_fseek f 20 #seek_cur
			)
			
			theFaces = #()
			for i=1 to numFaces do
			(
				theFaces[i] = [1,2,3]
				theFaces[i].x = (bf_ReadShort f)+1
				theFaces[i].y = (bf_ReadShort f)+1
				theFaces[i].z = (bf_ReadShort f)+1
			)
			
			theMesh = mesh name:shadowName vertices:theVerts faces:theFaces
		)
		else print "ERROR! invalid shadow data"
		
		bf_fclose f
	)
	else print "Error: could not open file"
)
------------------------
fn pre_importShadow thisFile Scaleo  =
(
	local offsetSeekTo = getSMpolyCount thisFile returnText:false
	if offsetSeekTo != 0 then
	(
		local fiSize = (getFileSize_compatable thisFile )
		if offsetSeekTo < (fiSize - 64) then
		(
			with redraw off
				importShadow thisFile offsetSeekTo Scaleo
		)
		else print "NoShadow"
	)
	else print "NoShadow"
)


/*
(
	delete objects
	clearListener()
	local startTime = timestamp()
	readSMVisMesh "c:\\teapot.sm" 10.0 true weldSameNormals:true
	format "EndTime: %\n" (timestamp() - startTime)
)
*/