---------------------------------
-- REX 3d-mesh file format (.rex)
-- Author: Rex Hill
--

struct RexMesh 
(
--------------------
	fn GetVer = return 15,
	fn GetFType = return "rex(*.rex)|*.rex|All|*.*|",
	
	fn writePoint3 f pnt3 = (writeFloat f pnt3.x; writeFloat f pnt3.y; writeFloat f pnt3.z;),
	fn writePoint3i f pnt3 = (writeLong f pnt3.x; writeLong f pnt3.y; writeLong f pnt3.z;),
	
	fn readPoint3 f = ( return [readFloat f, readFloat f, readFloat f] ),
	fn readPoint3i f = ( return [readLong f #unsigned, readLong f #unsigned, readLong f #unsigned] ),

--------------------
-- Import ----------
--------------------

	fn import fName:undefined s:10.0=
	(	
		if fName == undefined then fName = getOpenFileName types:( RexMesh.GetFType() )
		if fName == undefined then return();
		
		format "Importing file: %\n" fName
		local f = fopen fName "rb"
		if f != undefined then
		(
		undo off
		(
	
			local header = ReadLong f
			if header == (RexMesh.GetVer()) then
			(
				local theVerts = #()
				local numVerts = readlong f #unsigned
				for i=1 to numVerts do
					theVerts[i] = (RexMesh.readPoint3 f) * s
				
				local theVertNormals = #()
				local numVertNormals = readlong f #unsigned
				for i=1 to numVertNormals do
					theVertNormals[i] = RexMesh.readPoint3 f
				
				local theFaces = #()
				local numFaces = readLong f #unsigned			
				for i=1 to numFaces do
				(
					theFaces[i] = RexMesh.readPoint3i f
					theFaces[i] += 1
				)
		
				format "Header: %\tNumVerts: %\tNumFaces: %\n" header numVerts numFaces
				
				local theFacesMatID = #()
				local numMatIds = readLong f #unsigned
				for i=1 to numMatIds do	
					theFacesMatID[i] = readLong f #unsigned

				
				local tmpMeshNode = mesh vertices:theVerts faces:theFaces materialIDs:theFacesMatID
				
				local numSmoothGroups = readLong f #unsigned
				local tmpSmoothGroupInt = 0
				for i=1 to numSmoothGroups do
				(			
					tmpSmoothGroupInt = readLong f #unsigned
					setFaceSmoothGroup tmpMeshNode i tmpSmoothGroupInt
				)
				
				-- Texture Info Here
				
				local numMapChannels = readlong f #unsigned
				meshop.setNumMaps tmpMeshNode (numMapChannels+1)
				
				format "numMapChannels: %\n" numMapChannels
				for n=1 to numMapChannels do
				(	
					
		
					local theMapVerts = #()
					local numMapVerts = readlong f #unsigned
					format " numMapVerts: %\n" numMapVerts
					for i=1 to numMapVerts do
						theMapVerts[i] = RexMesh.readPoint3 f
		
		
					local theMapFaces = #()
					local numMapFaces = readlong f #unsigned
					format "   numMapFaces: %\n" numMapFaces
					for i=1 to numMapFaces do
					(
						theMapFaces[i] = RexMesh.readPoint3i f
						theMapFaces[i] += 1
					)
			
					if numMapVerts > 0 then
					(
						meshop.setNumMapFaces tmpMeshNode n numMapFaces
						meshop.setNumMapVerts tmpMeshNode n numMapVerts
	
						for i=1 to numMapVerts do
							meshop.setMapVert tmpMeshNode n i theMapVerts[i]
							
						for i=1 to numMapFaces do
							meshop.setMapFace tmpMeshNode n i theMapFaces[i]
					)
					
				)
	
				fclose f
				return tmpMeshNode;
			)
			else messagebox ("ERROR! Unknown header type:\n" +(header as string))
			
			fclose f
		)
		)
		--else messagebox ("ERROR! Could not open:\n" +(fName as string))
		
		return undefined
	),
	
	
--------------------
-- EXPORT ----------
--------------------
	-- materialStruct: u32 NumTextures, u16 textureNameLn, char[] textureName (null terminated)
	-- getTextureName
	fn export objA fname:undefined forceMatID:-1 s:0.1 isLocal:false=
	(
		local obj = undefined
		
		-- check that the geometry is valid before asking for fileName
		if not (isValidNode objA) then return false;
		try
		(
			local oldTransform = objA.transform
			if isLocal == true then objA.transform = matrix3 1
			obj = snapShotAsMesh objA
			if isLocal == true then objA.transform = oldTransform
		)
		catch 
		(	return false
		)
		
		if fName == undefined then
		(
			fName = getSaveFileName types:(RexMesh.GetFType())
		)
		if fName == undefined then return false;
		

		
		format "Saving: %\n" fName
		local f = fopen fName "wb"
		if f != undefined then
		(
			undo off
			(
				writeLong f (RexMesh.GetVer()) -- header
				
		
				-- Verts Pos
				writeLong f (obj.numVerts) #unsigned
				for i=1 to obj.numVerts do
					RexMesh.writePoint3 f ((getVert obj i) * s)
				
	
				-- Vert Normals
				writeLong f 0
				
				-- Commented out normals:
				--  3dsmax gives invalid vertex normals!
				--
				-- writeLong f (obj.numVerts) #unsigned
				-- for i=1 to obj.numVerts do
				--	RexMesh.writePoint3 f (getNormal obj i)
							
	
				-- Faces
				writeLong f (obj.numFaces) #unsigned
				for i=1 to obj.numFaces do 
					RexMesh.writePoint3i f ((getFace obj i) - 1)

			
			
				-- MatIDs
				writeLong f (obj.numFaces) #unsigned
				
				if (forceMatID == -1) then
				(
					for i=1 to obj.numFaces do
						writeLong f (getFaceMatID obj i)	
				)
				else
				(
					for i=1 to obj.numFaces do
						writeLong f forceMatID	
				)
				
				-- SmoothingGroups
				writeLong f (obj.numFaces) #unsigned
				for i=1 to obj.numFaces do
					writeLong f (getFaceSmoothGroup obj i)
			
			
				
				-- Texture information
				local tmpNumMapChannels = meshop.getNumMaps obj
			--	format " tmpNumMapChannels: % (ftell %)\n" tmpNumMapChannels (bf_ftell f)
				writeLong f tmpNumMapChannels #unsigned
				for n=1 to tmpNumMapChannels  do
				(
					if (meshop.GetMapSupport obj n) then
					(
						
						local tmpNumMapVerts = meshop.getNumMapVerts obj n
						writeLong f tmpNumMapVerts
						for i=1 to tmpNumMapVerts do
						(

							RexMesh.writePoint3 f (meshop.getMapVert obj n i)
						)
							
						local tmpNumMapFaces = meshop.getNumMapFaces obj n
						writeLong f tmpNumMapFaces 
						for i=1 to tmpNumMapFaces do
						(
							RexMesh.writePoint3i f ((meshop.getMapFace obj n i) - 1)
						)

					)
					else 
					(
						writeLong f 0; writeLong f 0;
					)
				)

				
			
				fclose f
				
			--	if (isValidNode tmpMeshNode) then delete tmpMeshNode
			)
			return true
		)
		else messageBox ("ERROR! Could not create:\n" +(fName as string))
		
		return false
	)
	
-- end Struct
)


--------------------
-- TEST
/*
(
	ClearListener()
	try delete geometry[2]
	catch()
	
	local timeStart = timeStamp()
	 
	-- RexMesh.Export geometry[1] fname:"c:\\4v_5t_2f.rex"
	local tmpMesh = RexMesh.Import fname:"c:\\output.rex"

	tmpMesh.material = geometry[1].material
	
	format "EndTime: %\n" (timeStamp() - timeStart)
 
)
*/