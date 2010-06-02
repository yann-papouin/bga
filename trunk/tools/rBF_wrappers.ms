-------------------
-- Misc. wrappers
-- FEb 2004


-------------------------
-- BinStream functions --
-------------------------

fn bf_fopen filename mode =
(	local f = fopen filename mode
--	if f == 0 then f == undefined
	return f
)
fn bf_fclose f=
(	fclose f
)
fn bf_fseek f seekInt seekParam =
(
	fseek f seekInt seekParam
)
fn bf_ftell f=
(	ftell f
)

fn bf_WriteByte f tmpVal=
(
	writeByte f tmpVal
)
fn bf_WriteShort f tmpVal =
(
	writeShort f tmpVal
)

fn bf_writeLong f tmpVal=
(
	writeLong f tmpVal
)
fn bf_WriteFloat f tmpVal=
(
	writeFloat f tmpVal
)
fn bf_WriteString f tmpStr=
(
	writeString f tmpStr
)
fn bf_WriteString2 f tmpStr=
(
	if tmpStr == undefined then return false
	
	local tmpStr2 = tmpStr as string
	local strLen = tmpStr2.count
	for i=1 to strLen do
	(
		writeByte f (bit.CharAsInt tmpStr2[i])
	)
)
------------------
fn bf_readByte f =
(
	ReadByte f #unsigned
)
fn bf_readShort f =
(
	ReadShort f #unsigned
)

fn bf_readLong f =
(
	ReadLong f #unsigned
)
fn bf_readFloat f =
(
	ReadFloat f
)
fn bf_readString f =
(
	ReadString f
)


fn bf_readString2 f strLen=
(
	local tmpStr = ""
	local tmpByte
	for i=1 to strLen do
	(
		tmpByte = readByte f #unsigned
		if tmpByte == undefined then exit
		tmpStr = tmpStr + (bit.IntAsChar tmpByte)
	)
	return tmpStr
)


--------------------------
-- FileStream (text)    --
--------------------------
-- Note: these are not used anymore, 
--   use stringStreams and memStreams

fn bf_CreateFile fname = 
(	createFile fname
)
fn bf_OpenFile fname modeStr:undefined =
(	if modeStr == undefined then OpenFile fname 
	else OpenFile fname mode:modeStr
)
fn bf_close f =
(	close f
)
fn bf_Seek f tmpOffset=
(	Seek f tmpOffset
)
fn bf_FilePos f =
(	filePos f
)
fn bf_eof f =
(	eof f
)


fn bf_readLine f=
(	readLine f
)
fn bf_readDelimitedString f tmpStr=
(	readDelimitedString f tmpStr
)
fn bf_SkipToString f tmpStr=
(	SkipToSTring f tmpStr
)
fn bf_readValue f =
(	readValue f
)


--------------------------
-- Misc other functions --
--------------------------
fn bf_DosCommand cmdStr =
(	prettyDosBatch cmdStr
)
fn bf_ShellLaunch fnameStr paramStr =
(	bfmdtops.shellLaunch fnameStr paramStr 
)

fn bf_getINISetting str_Fname str_Section str_Key =
(
	bfmdtops.getINISetting str_Fname str_Key
)
fn bf_setINISetting str_Fname str_Section str_Key str_KeyValue=
(
	bfmdtops.setINISetting str_Fname str_Key str_KeyValue
)













