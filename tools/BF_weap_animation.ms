-- Simplified weapon animation setups


/*
1) Import from ODB (weapon template)
	Setup us-soldier skeleton
	Use "Bip 01 R Hand" to align .ske
	
2) Import weapon skeleton mesh parts and replace the bones


*/


fn bfImportWeap thisWeapName =
(

	-- TODO: Check that ODB is loaded
	

	-- WeaponName (.ske) root
	--    LOD template
	--        Complex (.ske)
	--			bone parts
	--		  Simple (one piece, main)
	
	
	-- Main Object's template
	--
	local tmpTemplateREF = bfGetTemplateRefByTemplateName thisWeapName DEF_BF_OT
	format " %: %\n" thisWeapName tmpTemplateREF
	if tmpTemplateREF == undefined then
	(
		format "ObjectTemplate not found: %\n" thisWeapName
		return()
	)
	
	local tmpWeapOT = bfGetTemplateByRef tmpTemplateREF DEF_BF_OT
	
	local propREF_addTemplate = bfGetPropRefByPropName "addtemplate" DEF_BF_OT
	propREF_addTemplate = propREF_addTemplate - 1
	
	
	-- find the LOD selector template and grab the first child object
	-- get this child object's .ske (the complex)
	local tmpLodOT
	for i=1 to tmpWeapOT.children.count do
	(
	--	format "%\n" tmpWeapOT.children[i]
		if propREF_addTemplate == tmpWeapOT.children[i].name_ref then
		(
			local tmpLodOT_name = tmpWeapOT.children[i].args
			tmpLODOT = \
				bfGetTemplateByRef \
				(bfGetTemplateRefByTemplateName tmpLODOT_name DEF_BF_OT) DEF_BF_OT
			
			exit
		)
	)
--	format "tmpLodOT_ref: %\n" tmpLodOT
	
	-- Get the first child of the LOD selector
	--  This is the COMPLEX template (one you see close up)
	--
	local tmpComplexOT
	for i=1 to tmpLodOT.children.count do
	(
	--	format "%\n" tmpLodOT.children[i]
		if propREF_addTemplate == tmpLodOT.children[i].name_ref then
		(
			local tmpComplexOT_name = tmpLodOT.children[i].args
			tmpComplexOT = \
				bfGetTemplateByRef \
				(bfGetTemplateRefByTemplateName tmpComplexOT_name DEF_BF_OT) DEF_BF_OT
			exit
		)
	)
	format "tmpComplexOT: %\n" tmpComplexOT	
	

	-- Find the .ske filename
	--
	local propREF_createSkeleton = bfGetPropRefByPropName "createskeleton" DEF_BF_OT
	propREF_createSkeleton = propREF_createSkeleton - 1
	
	local strSKE_a
	local bone_geomNames = #()
	for i=1 to tmpComplexOT.children.count do
	(
	--	format "%\n" tmpWeapOT.children[i]
		if propREF_createSkeleton == tmpComplexOT.children[i].name_ref then
		(
			strSKE_a = tmpComplexOT.children[i].args
		)
	)
	
	format "strSKE_a: \"%%\"\n" g_TEMP_BF strSKE_a
	
	-- Import the .ske into the scene, store the bones into an array
	
	-- TODO: extract this .ske from the mod to g_TEMP_BF
	
	local tmpWeapBones = importBfskeA (g_TEMP_BF + strSKE_a) 100.0
	
	-- import geometry parts into scene, replace bones required
	local tmpWeapNode = bfSpawnObj_name thisWeapName
	

	
	-- Find weapon template with same name
	--   Spawn this weapon into the scene and attach parts to ske
	
	-- list animation states?....

		
)


/*
(
	delete objects
	ClearListener()
	bfImportWeap "Colt"
)
*/
-- Tasks
/*
-- revisit - .baf animation import fix it up
-- double check .baf animation weapon export

-- Create method of duplicating an existing weapon to a
--   new copy of that weapon

-- Create method to view/manage all the animation states
--  Include tweaking values

*/