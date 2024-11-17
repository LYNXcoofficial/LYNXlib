--LYNXlib v0.1.1 by RockRancher24 & Overo3

--TABLE OF CONTENTS
--Ln. x: Documentation
--Ln. x: Meta
--Ln. x: Autorun
--Ln. x: LYNX Miscellaneous Library (LYNXmisc)
--Ln. x: ZipLib
--Ln. x: LYNX CryptoLib
--Ln. x: CHTP
--Ln. x: Closing


--DOCUMENTATION
--META:
--	version: String containing version of LYNXlib.
--	versionCheck(minVer, maxVer): Returns true if current version is greater than minVer and less than maxVer.
--	lynxColor: Number containing colors.lightBlue for whatever reason.
--LYNXMISC:
--	twoColorPalette(mode: string, enableWhite: bool): Alters the terminal's color palette to a more restricted one for a "retro" feel. Added in v0.1.
--		mode: The palette to use.
--			RED, GREEN, BLUE, GRAYSCALE, YELLOW, CYAN, MAGENTA, 
--			ORANGE, LIME, MINT, CORNFLOWER, PURPLE, FUCHSIA,
--			RED-CYAN, GREEN-MAGENTA, BLUE-YELLOW
--		enableWhite: Controls whether or not white can also be used, along with mode

--	inRange(a: number, x: number, y: number): Returns true if a is between x and y (inclusive). Added in v0.1.0.

--	textBox(text: string, x1: number, y1: number, x2: number, y2: number, xmode: string, ymode: string, bC, tC]): Creates a text box. Added in v0.1.
--		text: The text for the textbox.
--		x1, y1: The top-left corner of the textbox.
--		x2, y2: The bottom-right corner of the textbox.
--		xmode: No desc. Currently only accepts "LEFT"
--		ymode: No desc. Currently only accepts "TOP"

--	readFile(dir: string): Returns the contents of a file, along with its LOSC-UHS header (if one exists). Added in v0.1.
--		dir: The path to the file (absolute or relative)

--	writeFile(dir: string, contents: string, header: table): A file-writing function that adds the header automatically. Added in v0.1.
--		dir: The path to the file (absolute or relative)
--		contents: File contents
--		header: The LOSC-UHS header of the file

--	hsv2rgb(h: number, s: number, v: number): Converts an HSV color to an RGB color. Added in v0.1.
--		h: The hue, in degrees
--		s: The saturation, scaled from 0 (fully pale) to 255 (fully saturated)
--		v: The value or brightness of the color, scaled from 0 (black) to 255 (fully bright)

--	recursiveList(path: string): Returns the path to every file inside a directory. Added in v0.1.
--		path: The path to the folder (absolute or relative)

--	round(num, digits, mode): Rounds a number to a certain number of post-decimal digits. Added in v0.1.
--		mode: The method of rounding to use. Defaults to nearest.
--			nearest, ceil, floor

-- ZIPLIB:
--	compress(inp: string, method: string): Compress a string. Added in v0.1.
--		inp: The data to be compressed.
--		method: The method to use for compression.
--			lzw, best, ratio

--	decompress(inp: string, method: string): Decompresses a string. Added in v0.1.
--		inp: The data to be decompressed.
--		method: The method to use for decompression.
--			lzw, best, ratio

--LYNX CRYPTOLIB:
--  rsaEncrypt(msg: string, key: table, bits: number, byteSize: number): Added in v0.1.1a.
--	msg: String to be encrypted.
--	key: Table deserealized & generated from https://pastebin.com/udGZapmD.
--	bits: Number of bits max.
--	byteSize: Number of bits in a byte (normally 8).

--  rsaDecrypt(msg: string, key: table, bits: number, byteSize: number): Added in v0.1.1a.
--	msg: String to be decrypted.
--	key: Table deserealized & generated from https://pastebin.com/udGZapmD.
--	bits: Number of bits max.
--	byteSize: Number of bits in a byte (normally 8).

--CHTP:
--	send(body: table, target: number, header: string, cipher: string, key: table, lNetwork: bool): Sends a table packet to an IP. Added in v0.1.
--		body: The table to send.
--		target: The IP to send the packet to.
--		header: The header of the packet.

--	awaitPacket(header: string, source: number, key: table, lNetwork: bool): No desc. Added in v0.1.
--		header: See send().
--		source: The IP to listen for packets from. Leave nil to listen to all IPs.
--		key: The key to decrypt the packet with.
--		lNetwork: Whether or not to listen on the wired network.
--		channel: What channel to listen on.


local ll = { }
ll.meta = { }
do
ll.meta.version = "0.1.1"
settings.define("lynx.lynxlibPath", {
    value = shell.getRunningProgram()
})
local function inRange(a, x,y)
    if a >= x and a <= y  then return true
    elseif a <= x and a >= y then return true
    else return false end
end
function ll.meta.versionCheck(minVer, maxVer)
	local vTable = {}
	local minVTable = {}
	local maxVTable = {}
	for str in string.gmatch(version,"[0-9]%.?") do
		if #str == 2 then table.insert(vTable,#vTable+1,tonumber(string.sub(str,1,1))) else table.insert(vTable,#vTable+1,tonumber(str)) end
	end
	for str in string.gmatch(minVer,"[0-9]%.?") do
		if #str == 2 then table.insert(minVTable,#minVTable+1,tonumber(string.sub(str,1,1))) else table.insert(minVTable,#minVTable+1,tonumber(str)) end
	end
	for str in string.gmatch(maxVer,"[0-9]%.?") do
		if #str == 2 then table.insert(maxVTable,#maxVTable+1,tonumber(string.sub(str,1,1))) else table.insert(maxVTable,#maxVTable+1,tonumber(str)) end
	end
    if maxVer ~= nil and minVer ~= nil then
		if vTable[1] > minVTable[1] or vTable[1] == minVTable[1] and vTable[2] > minVTable[2] or vTable[1] == minVTable[1] and vTable[2] == minVTable[2] and vTable[3] >= minVTable[3] then
			if vTable[1] < maxVTable[1] or vTable[1] == maxVTable[1] and vTable[2] < maxVTable[2] or vTable[1] == maxVTable[1] and vTable[2] == maxVTable[2] and vTable[3] <= maxVTable[3] then
				return true
			end
		end
		return false
	else
		return nil
	end
end
ll.meta.lynxColor = colors.blue
end


--AUTORUN
if not settings.get("startup_util.dir_path") then settings.set("startup_util.dir_path", "/startup/")
else
    if not fs.exists(fs.combine(settings.get("startup_util.dir_path"), "llstartup.lua")) then
        local arf = fs.open(settings.get("startup_util.dir_path").."/llstartup.lua", "w")
        local autorun = [[
math.randomseed(os.epoch())\n
if settings.get(\"chtp.thisIP\") == nil then settings.define(\"chtp.thisIP\", {\n
	description = \"This computer's IP address.\",\n
	default = math.random(1, 2^32),\n
})\n
	settings.set(\"chtp.thisIP\", math.random(1, 2^32))\n
	settings.save()\n
end"
	]]
	arf.write(autorun)
	arf.close()
    end
end


--LYNXMISC
ll.lynxmisc = { }
ll.lm = ll.lynxmisc
do

function ll.lynxmisc.inRange(a, x,y)
    if a >= x and a <= y  then return true
    elseif a <= x and a >= y then return true
    else return false end
end

function ll.lynxmisc.textBox(text, x1,y1, x2,y2, xmode,ymode, bC,tC)
    local startTC = term.getTextColor()
    local startBGC = term.getBackgroundColor()

    local xp,yp
    local tx,ty = term.getSize()
    if not text then text = "[UNDEFINED]" end
    if not x1 then x1 = 1 end
    if not y1 then y1 = 1 end
    if not xmode then xmode = "LEFT" end
    if not ymode then ymode = "TOP" end
    if not x2 and xmode == "LEFT" then x2 = x1 + #text end
    if not y2 and ymode == "TOP" then y2 = y1 end
    if not x2 then x2 = x1 end
    if not y2 then y2 = y1 end
    if not tC then tC = startTC end
    --bC handled later, no need to deal with it here
    
    if xmode == "LEFT" then xp = x1
    elseif xmode == "RIGHT" then xp = x2 - #text
    elseif xmode == "CENTER" then xp = x1 + math.floor((x2-x1 + 1) / 2) - math.floor(#text / 2) end
    
    if ymode == "TOP" then yp = y1
    elseif ymode == "BOTTOM" then yp = y2
    elseif ymode == "CENTER" then yp = y1 + math.floor((y2-y1 + 1) / 2) end

    if bC then paintutils.drawFilledBox(x1,y1, x2,y2, bC) end
    term.setTextColor(tC)
    term.setCursorPos(xp,yp)
    write(text)
    term.setBackgroundColor(startBGC)
    term.setTextColor(startTC)
end

function ll.lynxmisc.readFile(dir)
    local absDir = shell.resolve(dir)
    local file = fs.open(absDir, "r")
    local c = file.readAll()
    file.close()
    if c:sub(5,5) == "|" then
        local f = c:gsub("%-%-%[%[|.-|%]%]", "")
        local rh
        local h = textutils.unserialize(rh:gsub("\\n","\n"))
        return f, h
    else return c end
end

function ll.lynxmisc.writeFile(dir, contents, header)
    local oldHeader
    local _
    if fs.exists(dir) then
        _, oldHeader = readFile(dir)
    end
    local file = fs.open(dir, "w")
    file.writeLine("--[[|"..string.gsub(textutils.serialize(header), "\n","\\n").."|]]")
    file.write(contents)
    file.close()
    return oldHeader or true
end

function ll.lynxmisc.getHeader(dir)
    local _, h = readFile(dir)
    return h
end

function ll.lynxmisc.hsv2rgb(hue,saturation,value)
    saturation = saturation/255
    value = value/255
    local c = value*saturation
    local huePrime = hue/60
    local x = c * (1-math.abs(huePrime%2 - 1))
    local m = value-c
    local R,G,B
    if huePrime < 1 then R,G,B = c,x,0
    elseif huePrime < 2 then R,G,B = x,c,0
    elseif huePrime < 3 then R,G,B = 0,c,x
    elseif huePrime < 4 then R,G,B = 0,x,c
    elseif huePrime < 5 then R,G,B = x,0,c
    elseif huePrime < 6 then R,G,B = c,0,x
    end
    local r,g,b = R+m,G+m,B+m
    return r,g,b
end

function ll.lynxmisc.contextMenu(x,y, options, title, bgColor, titleColor)
    local longestOption = #title
    for i = 1, #options do
        if #options[i] > longestOption then longestOption = #options[i] end
    end
    paintutils.drawFilledBox(x,y, x+longestOption,y+#options, bgColor)
    term.setCursorPos(x,y)
    textBox(title, { x1 = x, y1 = y, x2 = x1+longestOption, xMode = "CENTER", textColor = titleColor })
    for i = 1, #options do
        if type(options[i]) == "string" then textBox(options[i], { x1 = x, y1 = y+i })
        elseif type(options[i]) == "table" then textBox(options[i].text or options[i][1], { x1 = x, y1 = y+i, textColor = options[i].textColor, backgroundColor = options[i].backgroundColor })
        elseif type(options[i]) == "number" then textBox(options[i], { x1 = x, y1 = y+i }) end
    end
end

function ll.lynxmisc.recursiveList(dir)
    local function sepList(path)
        local t = fs.list(path)
        local tF, tD = { }, { }
        for i = 1, #t do
            t[i] = fs.combine(path, t[i])
            if fs.isDir(t[i]) then table.insert(tD, t[i])
            else table.insert(tF, t[i]) end
        end
        return tF, tD
    end
    local totalFiles = { }

    local function recurseDir(currentDir)
        local files, dirs = sepList(currentDir)
        
        for _, file in ipairs(files) do
            table.insert(totalFiles, file)
        end
        
        for _, subDir in ipairs(dirs) do
            recurseDir(subDir)
        end
    end
    recurseDir(dir)
    table.sort(totalFiles)
    return totalFiles
end

function ll.lynxmisc.round(num, digits, mode)
    if not digits then digits = 0 end
    local multNum = num * 10^digits
    if type(mode) == "string" and mode:lower() == "floor" then return math.floor(multNum) / 10^digits
    elseif type(mode) == "string" and mode:lower() == "ceil" then return math.ceil(multNum) / 10^digits
    else return math.floor(multNum+0.5) / 10^digits end
end

function ll.lynxmisc.scale(num, units, digits, roundMode, prefix)
    local dNum
    if not digits then digits = 2 end
    local dTable = { { "", 0 }, { "k", 10^3 }, { "m", 10^6 }, { "b", 10^9 }, { "t", 10^12 }, { "qa", 10^15 }, { "qi", 10^18 }, { "sx", 10^21 }, { "sp", 10^24 }, { "oc", 10^27 }, { "no", 10^30 }, { "dc", 10^33 }, { "ud", 10^36 }, { "dd", 10^39 }, { "td", 10^42 } }
    if not units then units = settings.get("lynx.defaultUnits") or dTable end
    if units == "scientific" then
        dNum = num / 10^math.floor(math.log10(num)).."*10^"..math.floor(math.log10(num))
    elseif type(units) == "table" then
        if not digits then digits = 2 end
        local maxUnit = 1
        for i = 1, #units-1 do
            if type(units[maxUnit+1]) == "number" then
                if math.abs(num) > units[maxUnit+1].value or units[maxUnit+1][2] then maxUnit = i end
            end
        end

        if not prefix then dNum = round(num/(units[maxUnit].value or units[maxUnit][2]), digits, roundMode)..(units[maxUnit].name or units[maxUnit][1])
        else dNum = (units[maxUnit].name or units[maxUnit][1])..round((num/units[maxUnit].value or units[maxUnit][2]), digits, roundMode) end
    end
    return dNum
end
-- ends the do block
end


--ZIPLIB

ll.ziplib = { }
ll.zl = ll.ziplib
do
local char = string.char
local type = type
local select = select
local sub = string.sub
local tconcat = table.concat

local basedictcompress = {}
local basedictdecompress = {}
for i = 0, 255 do
    local ic, iic = char(i), char(i, 0)
    basedictcompress[ic] = iic
    basedictdecompress[iic] = ic
end

local function dictAddA(str, dict, a, b)
    if a >= 256 then
        a, b = 0, b+1
        if b >= 256 then
            dict = {}
            b = 1
        end
    end
    dict[str] = char(a,b)
    a = a+1
    return dict, a, b
end

local function lzw_compress(input)
    if type(input) ~= "string" then
        return nil, "string expected, got "..type(input)
    end
    local len = #input
    if len <= 1 then
        return "u"..input
    end

    local dict = {}
    local a, b = 0, 1

    local result = {"c"}
    local resultlen = 1
    local n = 2
    local word = ""
    for i = 1, len do
        local c = sub(input, i, i)
        local wc = word..c
        if not (basedictcompress[wc] or dict[wc]) then
            local write = basedictcompress[word] or dict[word]
            if not write then
                return nil, "algorithm error, could not fetch word"
            end
            result[n] = write
            resultlen = resultlen + #write
            n = n+1
            if  len <= resultlen then
                return "u"..input
            end
            dict, a, b = dictAddA(wc, dict, a, b)
            word = c
        else
            word = wc
        end
    end
    result[n] = basedictcompress[word] or dict[word]
    resultlen = resultlen+#result[n]
    n = n+1
    if  len <= resultlen then
        return "u"..input
    end
    return tconcat(result)
end

local function dictAddB(str, dict, a, b)
    if a >= 256 then
        a, b = 0, b+1
        if b >= 256 then
            dict = {}
            b = 1
        end
    end
    dict[char(a,b)] = str
    a = a+1
    return dict, a, b
end

local function lzw_decompress(input)
    if type(input) ~= "string" then
        return nil, "string expected, got "..type(input)
    end

    if #input < 1 then
        return nil, "invalid input - not a compressed string"
    end

    local control = sub(input, 1, 1)
    if control == "u" then
        return sub(input, 2)
    elseif control ~= "c" then
        return nil, "invalid input - not a compressed string"
    end
    input = sub(input, 2)
    local len = #input

    if len < 2 then
        return nil, "invalid input - not a compressed string"
    end

    local dict = {}
    local a, b = 0, 1

    local result = {}
    local n = 1
    local last = sub(input, 1, 2)
    result[n] = basedictdecompress[last] or dict[last]
    n = n+1
    for i = 3, len, 2 do
        local code = sub(input, i, i+1)
        local lastStr = basedictdecompress[last] or dict[last]
        if not lastStr then
            return nil, "could not find last from dict. Invalid input?"
        end
        local toAdd = basedictdecompress[code] or dict[code]
        if toAdd then
            result[n] = toAdd
            n = n+1
            dict, a, b = dictAddB(lastStr..sub(toAdd, 1, 1), dict, a, b)
        else
            local tmp = lastStr..sub(lastStr, 1, 1)
            result[n] = tmp
            n = n+1
            dict, a, b = dictAddB(tmp, dict, a, b)
        end
        last = code
    end
    return tconcat(result)
end

local bestRatio = "lzw"
local best = "lzw"

function ll.ziplib.compress(inp, method)
    if not method then method = "best" end
    if method:lower() == "lzw" then return lzw_compress(inp)
    elseif method:lower() == "best" then compress(inp, best)
    elseif method:lower() == "ratio" then compress(inp, bestRatio)
    end
end

function ll.ziplib.decompress(inp, method)
    if not method then method = "best" end
    if method:lower() == "lzw" then return lzw_decompress(inp)
    elseif method:lower() == "best" then decompress(inp, best)
    elseif method:lower() == "ratio" then decompress(inp, bestRatio)
    end
end
-- ends the do block
end

--CHTP
ll.chtp = { }
do
local gModem
local lModem
local psd = { }
local incMessage
local modems = { peripheral.find("modem") }
for i in ipairs(modems) do
    local curPer = modems[i]
    if curPer then
        if curPer.isWireless then
            if curPer.isWireless() == true then gModem = curPer
            elseif curPer.isWireless() == false then lModem = curPer end
        end
    end
end
if not lModem then lModem = gModem
elseif not gModem then gModem = { transmit = lModem.transmit, close = lModem.close, closeAll = lModem.closeAll, isOpen = lModem.isOpen, open = lModem.open, isWireless = lModem.isWireless } end
local function timeout() os.sleep(settings.get("chtp.packetTimeout") or 5) end
local function packetRec()
    repeat
        _, _, _, _, incMessage = os.pullEvent("modem_message")
        if not incMessage then incMessage = { } end
        local cont = false
        if incMessage[1] ~= settings.get("chtp.thisIP") then cont = true
        elseif incMessage[2] ~= psd.from and psd.from then cont = true
        elseif incMessage[3] ~= psd.header and psd.header then cont = true end
    until not cont
end
function ll.chtp.send(packet, target, header, cipher, key, lNetwork, channel)
    if not gModem then lNetwork = true end
    if lNetwork and not lModem then return nil, "No modem" end
    if not channel then channel = 127 end
    sPacket = lynxcryptolib.encrypt(textutils.serialize(packet), key, cipher)
    if not lNetwork then gModem.transmit(channel, channel, { target, settings.get("chtp.thisIP"), header, sPacket, cipher })
    else lModem.transmit(channel, channel, { target, settings.get("chtp.thisIP"), header, sPacket, cipher }) end
end
function ll.chtp.awaitPacket(header, from, key, lNetwork, channel)
    if not gModem then lNetwork = true end
    if lNetwork and not lModem then return nil, "No modem" end
    psd.header = header
    psd.from = from
    local incPacket
    incMessage = { }
    gModem.closeAll()
    lModem.closeAll()
    if not channel then channel = 127 end
    if not lNetwork then gModem.open(channel)
    else lModem.open(channel) end
    parallel.waitForAny(timeout, packetRec)
    if type(incMessage) == "table" then if incMessage[4] then incPacket = decrypt(incMessage[4], key, incMessage[5]) end
    else return nil, "Timeout" end
    if type(incPacket) ~= "string" then return nil end
    --Packet/nil, header/error code, return address, encryption method
    return textutils.unserialize(incPacket), incMessage[3], incMessage[2], incMessage[5]
end
end


--CLOSING
return ll
