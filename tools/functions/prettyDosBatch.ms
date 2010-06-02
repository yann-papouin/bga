-- Methods for:
--   running a dos .bat file without a window showing
--   pause script execution until .bat script has completed

global g_pdb_maxIters = 100 -- 20 secs with 0.2 secs between each check

global g_dlg_pdbStatus
global g_tmpperido = (getDir #scripts) + "\\tmpperido" 

rollout g_dlg_pdbStatus "Progress" width:136 height:72
(
	progressBar pb1 "ProgressBar" pos:[8,41] width:120 height:8
	label lbl1 "Please Wait..." pos:[34,18] width:80 height:16
)


fn pdb_PROC_End =
(
	DestroyDialog g_dlg_pdbStatus
)

fn pdb_PROC_Start =
(
	-- prevent multiple copies of this dialog
	pdb_PROC_End()
	
	createDialog g_dlg_pdbStatus
)

fn pdb_PROC_Update =
(
	local tmpVal = g_dlg_pdbStatus.pb1.value + 25
	if tmpVal > 100 then tmpVal = 0
	
	g_dlg_pdbStatus.pb1.value = tmpVal
)



-----------------------------------------
-- this is the wait function
fn pdb_status fname iter_cnt waitTime =
(
	-- check if this fname exists
	-- format " %, " iter_cnt
	
	if (getFiles fname).count > 0 then -- .bat file complete
	(

		DeleteFile ( g_tmpperido + ".tmp" )
		pdb_PROC_End()
	)
	else
	(
		-- prevent never ending loop in-case something goes wrong
		if iter_cnt > g_pdb_maxIters then
		(
			DeleteFile ( g_tmpperido + ".tmp" )
		)
		else
		(
			iter_cnt += 1
			pdb_PROC_Update()
			sleep waitTime
			pdb_status fname iter_cnt waitTime
		)
	)
	
	
)


fn prettyDosBatch cmd =
(
	local f = bf_fopen ( g_tmpperido + ".bat" ) "w"
	if f != undefined then
	(
		bf_writeString2 f cmd
		bf_writeString2 f "\necho done > \""
		bf_writeString2 f g_tmpperido
		bf_writeString2 f ".tmp\"\n"
		bf_fclose f
		
		DeleteFile ( g_tmpperido + ".tmp" )
		if (getFiles ( g_tmpperido + ".tmp" ) ).count == 0 then
		(
			-- todo use plugin ver of this (no window)
			bfmdtOps.runDosBatch ( g_tmpperido + ".bat" )
			
			pdb_PROC_Start()
			
			pdb_status ( g_tmpperido + ".tmp" ) 0 0.1
		)
		
	)
)



/*
(
	clearListener()
	prettyDosBatch "call D:\\3dsmax5\\scripts\\test1.bat"
)
*/