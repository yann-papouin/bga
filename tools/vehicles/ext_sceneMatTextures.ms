-- Go through the scene materials and extract textures

global g_tmpBFTextureNameList

fn extr_TextureNameList =
(
	if g_tmpBFTextureNameList.count > 0 then
	(
		local tmpFilesToExtract = #()
		local theFilesToExtract = #()
		
		-- blank versions
		for i=1 to g_tmpBFTextureNameList.count do
		(
			local tmpFname = \
				(getFilenamePath (g_tmpBFTextureNameList[i] as string)) \
				+ (getFilenameFile (g_tmpBFTextureNameList[i] as string))
			
			tmpFname = getRsFilename2 tmpFname
			
			if 0 == findItem tmpFilesToExtract tmpFname then
			(
				append tmpFilesToExtract tmpFname
			)
		)
		
		-- .dds versions
		for i=1 to tmpFilesToExtract.count do
		(
			local tmpFname = tmpFilesToExtract[i] + ".dds"
			append theFilesToExtract tmpFname
		)

		-- .tga versions
		for i=1 to tmpFilesToExtract.count do
		(
			local tmpFname = tmpFilesToExtract[i] + ".tga"
			append theFilesToExtract tmpFname
		)
			
		bf_RFA_extractFnamesMod theFilesToExtract "texture" bf_modPaths destPath:g_TEMP_BF
	)
)

fn extr_SceneMat_textures =
(
	g_tmpBFTextureNameList = #()

	for i=1 to geometry.count do
	(
		local tmpMat = geometry[i].material
		if tmpMat != undefined then
		(
		--	format "%_Material: % | %\n" i tmpMat.name (classof tmpMat)
			
			local tmpClass = classof tmpMat
			
			-- BF Materials Only
			if tmpClass == Multimaterial then
			(
				for j=1 to tmpMat.count do
				(
					if classof tmpMat[j] == BfRsShader then
						append g_tmpBFTextureNameList tmpMat[j].texture
				)
			)
		)	
	)
	
	extr_TextureNameList()
	
--	format "g_tmpBFTextureNameList: %\n" g_tmpBFTextureNameList.count
)


fn convert_dds_to_TGA fname=
(
	local tmpOutputName = (getFilenamePath fname) + (getFileNameFile fname) + ".tga"
	bfmdtOps.conv_DDS_TGA tmpOutputName fname
)



