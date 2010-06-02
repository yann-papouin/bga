-- these values are default for rollouts to use


global g_PathBfDir;
global g_bfCurrentMod;

fn makeTrailingSlash inputStr =
(	
	local tmpCount = inputStr.count

	-- TODO: convert all slashes to face one way here
	
	local i = tmpCount
	while (i > 1) do
	(
		if inputStr[i] != "\\" and inputStr[i] != "/" then
			exit;
	
		
		i -= 1;
	)
	
	inputStr = (substring inputStr 1 i) + "\\"
	
	return inputStr
)



fn str2bool testN = (if testN[1] == "t" then true;else false)


global g_BF_INIFILE = (bfmsloc+"_BF_MaxTools.ini")

mapped fn setBfSetting thisText = bf_setINISetting g_BF_INIFILE "BFToolsConfig" thisText ( (execute thisText) as string)
fn setBfSettingB thisText thisValue = bf_setINISetting g_BF_INIFILE "BFToolsConfig" thisText (thisValue as string)
fn getBfSetting tmpStr = (bf_getINISetting g_BF_INIFILE "BFToolsConfig" tmpStr)


------------------------

fn doespathExistYa pathTocheck=
(	(getdirectories (makeTrailingSlash pathTocheck)).count != 0 )





fn doesThisFileExist filename =
(
	if classof filename == string then	
		if (getFiles filename).count > 0 then
			return true
	
	return false;
)

