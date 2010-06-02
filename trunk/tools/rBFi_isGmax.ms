fn bf_testForGmax =
(

	
	
	-- if max5 or higher
	try 
	( 	if maxOps.productAppID == #max then return false 
		else if maxOps.productAppID == #gmax then return true
	)
	catch()
	
	if (GetDir #maxroot)[4] == "g" then return true
	if (GetDir #maxroot)[4] == "3" then return false
	
	if (MaxVersion())[1] > 5000 then return false
	
	return true	
)

global g_BFisGmax = bf_testForGmax()