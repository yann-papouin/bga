-- .RS file functions for bf1942 and bfVietnam
-- 
-- Author:			Rex Hill
-- Last updated:	Feb, 2004





fn trimLeadingWhiteSpace tmpStrr =
(
	local outString = ""
	local stopPos = 1
	for i=1 to tmpStrr.count do
	(
		if (tmpStrr[i] == "\n") then continue
		if (tmpStrr[i] == "\t") then continue
		if (tmpStrr[i] == " ") then continue
		
		stopPos = i; exit
	)
	
	for i=StopPos to tmpStrr.count do outString = outString + tmpStrr[i]

	return (copy outString)
)
fn getRidOfQuotes thisStr =
(
	local outString = ""
	for i=1 to thisStr.count do
	(
		if thisStr[i] != "\"" then outString = outString + thisStr[i]
	)

	return (copy outString)
)








-- import/ READ .rs
fn getRsFilename fnamePath =
(

	return (g_TEMP_BF + (getFilenamePath fnamePath) + (getFilenameFile fnamePath) + ".dds")
	

)

-- export/ WRITE .rs
fn getRsFilename2 fnamePath =
(
	local strEnd
	
	if undefined != findstring fnamePath g_TEMP_BF then strEnd = substring fnamePath (g_TEMP_BF.count+1) -1
	else strEnd = "texture/" + (getFilenameFile fnamePath)
	
	strEnd = (getFilenamePath strEnd) + (getFilenameFile strEnd)

	return strEnd
)




struct bfRsMaterial 
(
	name,
	type,
	lighting = true,
	lightingSpecular,
	materialspecular,
	materialspecularpower,
	materialDiffuse,
	transparent,
	twosided,
	envmap,
	depthwrite,
	alphatestref,
	texturefade,
	blendsrc,
	blenddest,
	texture,
	texture2,
-- BfV specific
--
	p = #()
	
)

fn rsCountNumMats f =
(
	seek f 0
	local cnt = -1
	while not (eof f) do
	(
		skipToString f "{"
		cnt += 1
	)
	seek f 0
	return cnt
)
fn rsSetColor tmpValues =
(
	for i=2 to 4 do
	(
		if (tmpValues[i] as float) != undefined then tmpValues[i] = (tmpValues[i] as float) * 255.0
		else tmpValues[i] = 0.0
	)
	return (color tmpValues[2] tmpValues[3] tmpValues[4])
)












----------------------------------------
-- Convert Standard material -> bfRsMaterial struct
fn rs_Standard2struct tmpName tmpMat lightmapped:false=
(
	local rsMat = bfRsMaterial name:(tmpName as string)

	local one255 = 1.0 / 255.0
	
	rsMat.lighting = not lightmapped
	

		
	rsMat.materialDiffuse = color tmpmat.diffuse.r tmpmat.diffuse.g tmpmat.diffuse.b
	
	
	try
	(
		if not lightmapped then
		(
			if (tmpMat.shaderType != 6) then -- Filter out Strauss because it doesn't have a specular level
			(	if (tmpmat.specularlevel > 0.0) then 
				(	rsMat.lightingSpecular = true
					rsMat.materialspecular = color tmpmat.specular.r tmpmat.specular.g tmpmat.specular.b
					rsMat.materialspecularpower = tmpmat.specularlevel
				)
			)
		)
	)
	catch ()
	
	
	
	try
	(	rsMat.twosided = tmpmat.twosided
	)
	catch()
	
	
	
	try
	(
		if tmpmat.opacity < 100.0 then rsMat.transparent = true
		if tmpmat.soften == 1.0 then rsMat.envmap = true
	)
	catch ()

	local tmpfName = "notexture"
	try tmpfName = (getFilenameFile tmpmat.maps[2].filename)
	catch()
	rsMat.texture = tmpfName
	
	return rsMat 
)


----------------------------------------
-- Convert BfRsShader material -> bfRsMaterial struct
fn rs_bfRS2struct tmpName tmpMat lightmapped:false=
(
	local rsMat = bfRsMaterial name:tmpName
	
	rsMat.lighting = tmpmat.lighting
	if lightmapped then rsMat.lighting = false
	rsMat.materialspecular = tmpmat.materialspecular 
	rsMat.materialspecularpower = tmpmat.materialspecularpower 
	rsMat.materialDiffuse = tmpmat.materialDiffuse 
	rsMat.transparent = tmpmat.transparent 
	rsMat.twosided = tmpmat.twosided 
	rsMat.envmap = tmpmat.envmap 
	rsMat.depthwrite = tmpmat.depthwrite 
	rsMat.alphatestref = tmpmat.alphatestref 
	rsMat.texturefade = tmpmat.texturefade 
	rsMat.blendsrc = tmpmat.blendsrc 
	rsMat.blenddest = tmpmat.blenddest
	rsMat.texture = tmpmat.texture 
	rsMat.texture2 = tmpmat.texture2
	
	try
	(
		for i=1 to tmpMat.p.count do
			rsMat.p[i] = tmpmat.p[i]
	)
	catch
	(
		format "Error! Catch() in rs_bfRS2struct()\n"
	)
	if tmpmat.materialspecularpower > 0.0 then rsMat.lightingSpecular = true
	else rsMat.lightingSpecular = false
	
	return rsMat 
)

----------------------------------------
-- Convert bfRsMaterial struct -> BfRsShader material
fn rs_struct2bfRS tmpName tmpMat=
(
	local rsMat = BfRsShader name:tmpMat.name
	
	if tmpmat.lighting != undefined then				rsMat.lighting = tmpmat.lighting
	if tmpmat.materialspecular != undefined then		rsMat.materialspecular = tmpmat.materialspecular 
	if tmpmat.materialspecularpower != undefined then	rsMat.materialspecularpower = tmpmat.materialspecularpower 
	if tmpmat.materialDiffuse != undefined then			rsMat.materialDiffuse = tmpmat.materialDiffuse 
	if tmpmat.transparent != undefined then				rsMat.transparent = tmpmat.transparent 
	if tmpmat.twosided != undefined then				rsMat.twosided = tmpmat.twosided 
	if tmpmat.envmap != undefined then 					rsMat.envmap = tmpmat.envmap 
	if tmpmat.depthwrite != undefined then 				rsMat.depthwrite = tmpmat.depthwrite 
	if tmpmat.alphatestref != undefined then 			rsMat.alphatestref = tmpmat.alphatestref 
	if tmpmat.texturefade != undefined then 			rsMat.texturefade = tmpmat.texturefade 
	
	if tmpmat.blendsrc != undefined then 				rsMat.blendsrc = tmpmat.blendsrc 
	if tmpmat.blenddest!= undefined then 				rsMat.blenddest = tmpmat.blenddest

	if tmpMat.texture != undefined then
	(
		rsMat.texture = tmpmat.texture
		rsMat.delegate.maps[2] = BitmapTex filename:rsMat.texture
		showTextureMap rsMat on
	)
	
	if tmpMat.texture2 != undefined then
	(
		rsMat.texture2 = tmpmat.texture2
	)
	
	try
	(
		if classof tmpMat.p == Array then
		(
			for i=1 to tmpMat.p.count do
			(
				rsMat.p[i] = tmpMat.p[i]
			)
		)
	)
	catch
	(
		format "Error! Catch() in rs_struct2bfRS()\n"
	)
	
	return rsMat 
)


----------------------------------------
-- Write the bfRsMaterial Struct -> disk file (individual subshader)
fn rs_writeRSstruct f rsMat simpleshader:false=
(
	if ClassOf rsMat != bfRsMaterial then 
	(
		format "ERROR! rs_writeRSstruct(), rsmat == %!\n" bfRsMaterial
		return()
	)

	
	format "subshader \"%\" \"StandardMesh/Default\"\n{\n" rsMat.name to:f
	format "\tlighting %;\n" (rsMat.lighting == true) to:f
	
	if simpleShader then
	(	
		format "\tmaterialDiffuse % % %;\n" 1 1 1 to:f
		format "\tlightingSpecular %;\n" "false" to:f
	)
	else
	(
		local one255 = 1.0 / 255.0
	
		if (classof rsMat.materialDiffuse) != Color then
			rsMat.materialDiffuse = Color 255 255 255
			
		if rsMat.materialspecularpower != undefined then
		(	if rsMat.materialspecularpower > 0 then 
			(	format "\tlightingSpecular %;\n" "true" to:f
				format "\tmaterialDiffuse % % %;\n" (rsMat.materialDiffuse.r*one255) (rsMat.materialDiffuse.g*one255) (rsMat.materialDiffuse.b*one255) to:f
				format "\tmaterialSpecular % % %;\n" (rsMat.materialspecular.r*one255) (rsMat.materialspecular.g*one255) (rsMat.materialspecular.b*one255) to:f
				format "\tmaterialSpecularPower %;\n" rsMat.materialspecularpower to:f
			)
			else 
			(	format "\tlightingSpecular %;\n" "false" to:f
				format "\tmaterialDiffuse % % %;\n" (rsMat.materialDiffuse.r*one255) (rsMat.materialDiffuse.g*one255) (rsMat.materialDiffuse.b*one255) to:f
			)
		)
		else 
		(
			format "\tlightingSpecular %;\n" "false" to:f
			format "\tmaterialDiffuse % % %;\n" (rsMat.materialDiffuse.r*one255) (rsMat.materialDiffuse.g*one255) (rsMat.materialDiffuse.b*one255) to:f
		)
	
		 
		
		if rsMat.transparent == true then format "\ttransparent %;\n" rsMat.transparent to:f		

		if rsMat.twosided == true then format "\ttwosided %;\n" rsMat.twosided to:f
		if rsMat.envMap == true then format "\tenvmap %;\n" rsMat.envMap to:f
		if rsMat.texturefade == true  then format "\ttexturefade %;\n" rsMat.texturefade to:f
		
		if classof rsMat.blendsrc == Integer then
			if rsMat.blendsrc > 1 then format "\tblendSrc %;\n" "sourceAlpha" to:f

		if classof rsMat.blenddest == Integer then
			if rsMat.blenddest > 1 then
			(	case rsMat.blenddest of
				(
				2: format "\tblendDest %;\n" "invsourceAlpha" to:f
				3: format "\tblendDest %;\n" "one" to:f
				default: format "ERROR!, rsWriteSubShader() rsMat.blendDest = %\n" rsMat.blendDest to:listener
				)
			)
			
		if rsMat.depthwrite == true then format "\tdepthwrite %;\n" rsMat.depthwrite to:f
		if rsMat.alphatestref != undefined then
			if rsMat.alphatestref > 0.0 then format "\talphaTestRef %;\n" rsMat.alphatestref to:f
			
		if Classof rsMat.p == Array then
		(
			for i=1 to rsMat.p.count do
				format "\t%;\n" rsMat.p[i] to:f
		)
	)
	
	
	format "\ttexture \"%\";\n" (getRSfilename2 (rsMat.texture as string)) to:f
	
	if rsMat.texture2 != undefined then
	(
		if rsMat.texture2.count > 0 then
			if rsMat.texture2 != "None" then
				format "\tnormalmap \"%\";\n" (getRSfilename2 (rsMat.texture2 as string)) to:f
	)
	
	format "}\n\n" to:f
)

----------------------------------------
-- Convert either type of material -> bfRsMaterial struct
fn rs_MaterialToRsStruct tmpMat tmpName:"theTmpName" lightmapped:false=
(
	
	local rsMat = undefined
	case (classof tmpMat) of
	(
		BfRsShader: 
		(
			rsMat = rs_bfRS2struct tmpName tmpMat lightmapped:lightmapped
		)
		Standardmaterial: 
		(
			rsMat = rs_Standard2struct tmpName tmpMat lightmapped:lightmapped
		)
		default:
		(	
			format "ERROR! rs_MaterialToRsStruct(), materialType not supported: %\n" (classof tmpMat)
			rsMat = bfRsMaterial name:tmpName
		)
	)
	return rsMat
)
---------------------------------------------------------------------------



-- rs_MaterialToRsStruct $.material[1] tmpName:"tmpName"












-------------------------------------------
-- Reads a .rs disk file -> bfRsMaterial struct
fn rsReadSubShader f =
(
	local tmpMat = bfRsMaterial name:"noName"
	if not (eof f) then
	(
		tmpMat.name = readValue f
		tmpMat.type = readValue f

		skipToString f "{"
		while not (eof f) do
		(
		--  format "[%] " (filePos f)
			local tmpLineA = (readDelimitedString f ";")	
			if (findString tmpLineA "}") != undefined then exit
			
			--format "  a) [%]\n" tmpLineA
			local tmpLine = trimLeadingWhiteSpace tmpLineA 
			--format "  b) [%]\n" tmpLine
			
			local tmpValues = filterString tmpLine " "
			tmpValues[1] = lowerCase tmpValues[1]
			
			local tmpTrue = false
			if tmpValues[2][1] == "t" or tmpValues[2][1] == "T" then tmpTrue = true
			
		--	format "%\n" tmpValues
			
			case tmpValues[1] of
			(
				"lighting": if tmpTrue then tmpMat.lighting = true;
				"lightingspecular": if tmpTrue then tmpMat.lightingSpecular = true;
				"transparent": if tmpTrue then tmpMat.transparent = true;
				"envmap": if tmpTrue then tmpMat.envmap = true;
				"twosided": if tmpTrue then tmpMat.twosided = true;
				"depthwrite": if tmpTrue then tmpMat.depthwrite = true;
				"texturefade": if tmpTrue then tmpMat.texturefade= true;
				
				"alphatestref": if ((tmpValues[2] as float) != undefined) then tmpMat.alphatestref = (tmpValues[2] as float);
				"materialspecularpower": if ((tmpValues[2] as float) != undefined) then tmpMat.materialspecularpower= (tmpValues[2] as float);
				
				"materialdiffuse": tmpMat.materialDiffuse = rsSetColor tmpValues
				"materialspecular": tmpMat.materialspecular = rsSetColor tmpValues
				
				"texture": tmpMat.texture = getRsFilename (getRidOfQuotes tmpValues[2])
				"normalmap": tmpMat.texture2 = getRsFilename (getRidOfQuotes tmpValues[2])
				
				"blendsrc": tmpMat.blendsrc = 2
				"blenddest": 
				(	
					tmpMat.blenddest = 2
					if (lowercase tmpValues[2]) == "one" then tmpMat.blenddest = 3
				)
				
				default: 
				(
					local tmpValues_STR = ""
					for i=1 to tmpValues.count do 
					(
						tmpValues_STR = tmpValues_STR + (tmpValues[i] as string)
						if i != tmpValues.count do
							tmpValues_STR = tmpValues_STR + " ";
					)
					
					try
						append tmpMat.p tmpValues_STR
					catch
						format "Could not append cust .rs value to struct (tmpMat.p)\n"
						
					-- format "ERROR! unknown Type: %\n" tmpValues
				)
			)
		)
	)
	return tmpMat
)



-------------------------------------------
-- Convert array of bfRsMaterial structs -> multiSub Material
fn rsToMaxMaterial materialName rsMaterials =
(
	local matCnt = 0
	try matCnt = rsMaterials.count
	catch
	(
		format "Error rsToMaxMaterial(): rsMaterials = undefined\n"
		matCnt = 0
	)
	
	if matCnt == 0 then
	(
		return (multiSubMaterial numSubs:1 name:materialName)
	)
	
	-- format "rsMaterials: %\n" rsMaterials
	local outMat = multiSubMaterial numSubs:matCnt name:materialName
	for i=1 to rsMaterials.count do
	(
		outMat[i] = rs_struct2bfRS rsMaterials[i].name rsMaterials[i]
	)
	return outMat
)





-------------------------------------------
-- Read .rs file from disk -> bfRsMaterial structs
fn rsReadFile thisFile =
(
	local f = bf_ReadFileToBuffer thisFile
	if f != undefined then
	(
		local numMats = rsCountNumMats f
	--	format "numMats: %\n" numMats
		local rsMaterials = #()
		for i=1 to numMats do
		(
			seek f 0
			for n=1 to i do skipToString f "subshader "
	
			rsMaterials[i] = rsReadSubShader f
		--	format "\trsMaterials[%]: %\n" i rsMaterials[i] --////////////
		)
		return rsMaterials
	)
	return undefined
)


(
--	clearLIstener()
--	local tmpObj = $LOD01_CE_amobuil
--	rs_Standard2struct "FRED" tmpObj.material[1]
)
