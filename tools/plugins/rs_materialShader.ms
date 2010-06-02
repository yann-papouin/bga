-- ReactorShader (.rs) material
--
-- Author: Rex Hill
-- Date:   Feb 2004
--



/*
-- TODO
-- delegate transparency effect, Specular level, 2-Sided
*/


plugin material BfRsShader
	name:"Battlefield_RS"
	classID:#(0x56922d4d, 0xb25a98b3)
	extends:Standard replaceUI:true version:4
(
	parameters main rollout:params
	(
		lighting type:#boolean default:true ui:lighting
		materialspecular type:#color default:white ui:materialspecular
		materialspecularpower type:#float default:0.0 ui:materialspecularpower 
		materialDiffuse type:#color default:white ui:materialDiffuse 
		
		texture type:#string default:"" --ui:textureBtn
		showTexture1 type:#boolean default:true ui:ckbShow1
		texture2 type:#string default:"" --ui:texture2Btn
		showTexture2 type:#boolean default:false ui:ckbShow2
		
		transparent type:#boolean ui:transparent
		twosided type:#boolean ui:twosided
		envMap type:#boolean ui:envMap
		
		depthwrite type:#boolean ui:depthwrite 
		alphatestref type:#float ui:alphatestref 
		texturefade type:#boolean ui:texturefade 
		blendsrc type:#Integer ui:blendsrc 
		blenddest type:#Integer ui:blenddest 
		p type:#StringTab tabSizeVariable:true 
		
		on materialDiffuse set val do delegate.diffuse = val
		-- on materialspecular set val do delegate.SpecularColor = val
		-- on materialspecularpower set val do delegate.specularLevel = val
		-- on twosided set val do delegate.twosided = val
	)
	
	rollout params "Shader Parameters" width:326 height:411
	(
		colorPicker materialspecular "Specular: " pos:[35,48] width:96 height:22 color:(color 255 255 255) title:"Choose a color"
		colorPicker materialDiffuse "Diffuse: " pos:[44,24] width:87 height:22 color:(color 255 255 255)
		checkbox lighting "Lighting" pos:[192,24] width:61 height:18 checked:true
		
		checkbox transparent "Transparent " pos:[192,40] width:82 height:18
		checkbox twoSided "Twosided" pos:[192,56] width:69 height:18
		checkbox envMap "Environment Map" pos:[192,72] width:104 height:18
		checkbox depthwrite "DepthWrite" pos:[192,191] width:72 height:18
		spinner materialspecularpower "SpecPower: " pos:[68,80] width:73 height:16 range:[0,100,0]
		spinner alphatestref "AlphaTestRef: " pos:[82,179] width:58 height:16 enabled:true range:[0,1,0] scale:0.1
		checkbox texturefade "TextureFade" pos:[192,176] width:80 height:18
		dropdownList blendsrc "BlendSrc:" pos:[40,211] width:124 height:40 items:#("   ", "sourceAlpha") selection:1
		dropdownList blenddest "BlendDest:" pos:[168,211] width:128 height:40 items:#("   ", "invsourceAlpha", "one")
		label lbl1 "Bitmap:" pos:[24,114] width:40 height:16
		GroupBox grp1 "" pos:[184,8] width:120 height:96
		GroupBox grp2 "" pos:[16,8] width:135 height:96
		GroupBox grp3 "" pos:[16,163] width:290 height:96
		label lbl342 "Additional Properties:" pos:[24,267] width:99 height:13
		edittext edit1 "" pos:[24,287] width:260 height:100
		
		button textureBtn "None" pos:[109,111] width:158 height:20
		button texture2Btn "None" pos:[109,140] width:158 height:20
		label lbl7 "Normal Map:" pos:[23,142] width:63 height:16
		checkButton ckbShow1 "Show" pos:[269,111] width:36 height:20
		checkButton ckbShow2 "Show" pos:[269,140] width:36 height:20
		button btnX1 "X" pos:[92,111] width:15 height:20
		button btnX2 "X" pos:[92,140] width:15 height:20


		on params open do
		(		
			try
			(
				if texture.count == 0 then texture = "None"
				if texture2.count == 0 then texture2 = "None"
			)
			catch
			(
				texture = "None"
				texture2 = "None"
			)
				
			textureBtn.caption = getFilenameFile (texture as string)
			texture2Btn.caption = getFilenameFile (texture2 as string)
			
			try
			(
				local tmpText = ""
				for i=1 to p.count do
				(
					tmpText = tmpText + (p[i] as string) + ";\n"
				)
				edit1.text = tmpText 
			)
			catch( format "Catch() BfRsShader, on params open do\n")
			
			ckbShow2.checked = not ckbShow1.checked
			
			try
			(
				delegate.mapEnables[2] = true
				showTextureMap this on
			)
			catch()
		)
		
		fn showProperTexMap =
		(
			delegate.mapEnables[2] = true
			
			if ckbShow1.checked == true then delegate.maps[2] = BitmapTexture filename:texture
			else delegate.maps[2] = BitmapTexture filename:texture2
			
			showTextureMap this on
		)

	
		on edit1 entered arg do
		(
			p = #()
			
			local tmpLine = ""
			for i = 1 to arg.count do
			(
				local tmpVal = bit.charAsInt arg[i]
				if tmpVal < 30 or tmpVal == 59 then -- 0x0A or ";"
				(
					if tmpLine.count > 2 then
					(
						append p tmpLine
						tmpLine = ""
					)
				)
				else tmpLine = tmpLine + arg[i]
			)
		)
		
		on ckbShow1 changed state do
		(
			ckbShow2.checked = not state
			showProperTexMap()
		)
		on ckbShow2 changed state do
		(
			ckbShow1.checked = not state
			showProperTexMap()
		)
		
		on btnX1 pressed do
		(
			texture = "None"
			textureBtn.caption = "None"
			showProperTexMap()
		)
		on btnX2 pressed do
		(
			texture2 = "None"
			texture2Btn.caption = "None"
			showProperTexMap()
		)
		on textureBtn pressed do
		(
			local tmpBitmap = selectBitmap caption:"Select a bitmap (Color Map)"
			if tmpBitmap != undefined then
			(
				texture = tmpBitmap.fileName
				textureBtn.caption = getFilenameFile texture
				
				showProperTexMap()
			)
		)
		on texture2Btn pressed do
		(
			local tmpBitmap = selectBitmap caption:"Select a bitmap (Normal Map)"
			if tmpBitmap != undefined then
			(
				texture2 = tmpBitmap.fileName
				texture2Btn.caption = getFilenameFile texture2
				
				showProperTexMap()
			)
		)
	
	)
	

)