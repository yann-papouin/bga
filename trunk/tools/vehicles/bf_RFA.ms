-------------------
-- Pack RFA FILES
-------------------
--


fn bf_RFA_packFiles sourceDir subPath rfaFName method:1=
(
	sourceDir = makeTrailingSlash sourceDir
	
	-- remove extra trailing slash
	if sourceDir[sourceDir.count] == "/" or sourceDir[sourceDir.count] == "\\" then
		sourceDir = substring sourceDir 1 (sourceDir.count - 1)
		
	bfmdtops.rfa_packFolder sourceDir subPath rfaFName method
)


fn bf_createFoldersFromFnames fnames =
(
	local tmpFullPaths = #()
	
	if fnames.count < 1 then
		return false
	
	tmpFullPaths[1] = getFilenamePath fnames[1]
	
	for i=2 to fnames.count do
	(
		local tmpFnamePath = getFilenamePath fnames[i]
		for j=1 to tmpFullPaths.count do
		(
			local tmpVal = findString tmpFullPaths[j] tmpFnamePath
				
			-- already a substring of another
			if tmpVal == 1 then exit

			if tmpVal == undefined then
			(	
				local tmpVal2 = findString tmpFnamePath tmpFullPaths[j]

				-- the stored value is a substring of this one
				if tmpVal2 == 1 then
				(
					tmpFullPaths[j] = tmpFnamePath
					exit
				)

				if tmpVal2 == undefined then
				(
					append tmpFullPaths tmpFnamePath
					exit
				)
			)
		)
	)
	
	local tmpBinDir = g_BFDIR_BIN
	local bfTempDir = g_TEMP_BF
	
	if tmpFullPaths.count > 0 then
	(
		local f = bf_fopen (bfTempDir + "createDirs.bat") "w"
		if f != undefined then
		(
			for i=1 to tmpFullPaths.count do
			(
				bf_writeString2 f ("mkdir \"" + tmpFullPaths[i] + "\"\n")
			)
			bf_fclose f
			
			prettyDosBatch ("call \"" + bfTempDir + "createDirs.bat\"")
		)
	)
)

fn bf_ClearTmpDir subDir=
(
	local tmpBinDir = g_BFDIR_BIN
	local bfTempDir = g_TEMP_BF
	
	local f = bf_fopen (bfTempDir + "tmpClr.bat") "w"
	if f != undefined then
	(
		local f2 = stringStream ""
		format "rd \"%%\" /s /q\n" bfTempDir subDir to:f2
		bf_writeString2 f f2
		bf_fclose f
		
		prettyDosBatch ("call \"" + bfTempDir + "tmpClr.bat\"")
	)
)









--------------------
-- UnPack RFA FILES
---------------------


--------------------------------------
-- Description: Extracts an array of filenames from a .rfa archive
--
-- return value: an array of filesnames that could not be extracted
--   it is a list of files that do not exist at the extract path
--
--
fn bf_RFA_extractFiles fname rfaFName destPath:undefined=
(

	
	-- input error checking
	if classof fname != array then return #()

	-- setup paths
	local tmpBinDir = g_BFDIR_BIN
	if destPath == undefined then destPath = copy g_TEMP_BF
	
	-- FileList
	local f = bf_fopen ( tmpBinDir + "_rfaTmplist.txt" ) "w"
	if f != undefined then
	(
		for i=1 to fname.count do
			bf_writeString2 f (fname[i] + "\n")
				
		bf_fclose f
		
	
		-- .bat to use filelist
		local f2 = stringstream "" 
		local f_out2 = bf_fopen ( tmpBinDir + "_rfaUnpack.bat" ) "w"
		if f_out2 != undefined then
		(	
			format "\"%rfaUnpack.exe\" " tmpBinDir to:f2
			format "\"%\" " rfaFName to:f2
			
			-- workaround for bug in windows; |"c:\temp\"| is interpreted as: |C:\temp"|, should be: |C:\temp\|
			format "\"%\" \"-l%_rfaTmplist.txt\"" destPath tmpBinDir to:f2
			-- format "\npause" to:f2
			
			bf_writeString2 f_out2 f2
			bf_fclose f_out2
			f2 = Undefined
			

			
			-- run the .bat
			prettyDosBatch ( "call \"" + tmpBinDir + "_rfaUnpack.bat\"" )
			(
				local aFilesErrored = #()
				for i=1 to fname.count do
				(
					local tmpFnameFind = destPath + (fname[i] as string)
					if (getFiles tmpFnameFind).count == 0 then -- file doesn't exist
					(
						append aFilesErrored fname[i]
					)
				)
				return aFilesErrored
			)
			
			/*
			
			local tmpRet = bf_dosCommand ( "\"" + tmpBinDir + "_rfaUnpack.bat\"" )
			if (tmpRet == 0) then
			(
				local aFilesErrored = #()
				for i=1 to fname.count do
				(
					local tmpFnameFind = destPath + (fname[i] as string)
					if (getFiles tmpFnameFind).count == 0 then -- file doesn't exist
					(
						append aFilesErrored fname[i]
					)
				)
				return aFilesErrored
				
			)
			else format "Error! Program quit with non-zero return value: %\n" tmpRet
			*/
			
			
			
		)
	)
	
	return #()
)

fn bf_RFA_extractFnamesMod fnames archiveName modPaths destPath:undefined=
(
	--format "ExtractFnamesMod %\n" modPaths 
	
	if classof modPaths != array then
	(
		bf_loadModpaths()
		
		if classof modPaths != array then
		(	
			format "ERROR! modpaths != Array\n"
			return()
		)
	)
		
	local FilesToExtract = #()
	for i=1 to fnames.count do
	(
		if 0 == findItem FilesToExtract fnames[i] then
			append FilesToExtract fnames[i]
	)
	
	for i=1 to FilesToExtract.count do
	(
		for j=1 to FilesToExtract[i].count do
			if FilesToExtract[i][j] == "\\" then FilesToExtract[i][j] = "/"

	)
--	format "  extract files\n"
	-- extract the files
	for i=1 to modPaths.count do
	(
		local tmpPatchRfas = getFiles (g_PathBfDir + modPaths[i] + "Archives\\" + archiveName + "*.rfa")
	--	format "  %: %\n" modPaths[i] tmpPatchRfas.count
		
		local tmpArrayNotExtracted = #()
		
		local n = tmpPatchRfas.count
		while (n > 0) do
		(
			for j=1 to tmpPatchRfas[n].count do
				if tmpPatchRfas[n][j] == "/" then tmpPatchRfas[n][j] = "\\"
			
			--format "    - %\n" tmpPatchRfas[n]
			
			tmpArrayNotExtracted = bf_RFA_extractFiles FilesToExtract tmpPatchRfas[n] destPath:destPath
			--format "      NotExtracted: %\n" tmpArrayNotExtracted
			
			if tmpArrayNotExtracted.count == 0 then exit
			
			n -= 1
		)
	)

)




--------------------

/*
(
	clearListener()
	local tmpSourceDir = "D:\\3dsmax5\\TEMP_BF\\objects\\"
	local rfaFName = "D:\\objects.rfa"
	bf_RFA_packFiles tmpSourceDir "objects" rfaFName
)
*/


/*
(
	clearListener()
	bf_RFA_extractFnamesMod #("standardmesh/wreck_zero_m1.sm") "standardmesh" bf_modPaths
)
*/