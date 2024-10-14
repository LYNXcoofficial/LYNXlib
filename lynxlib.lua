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

--  aesEncrypt(data: string, key: string, iv: number): Added in v0.1.
--	data: String to be encrypted.
--	key: String to be used as a key.
--	iv: A number used as initalizaiton. Must be the same when enc/dec.

--  aesDecrypt(data: string, key: string, iv: number): Added in v0.1.
--	data: String to be encrypted.
--	key: String to be used as a key.
--	iv: A number used as initalization. Must be the same when enc/dec.

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


--LYNX CRYPTOLIB
ll.cryptolib = { }
ll.cl = ll.cryptolib
ll.lynxcryptolib = ll.cryptolib
do
local function fl(x)
	if x < 0 then
		return math.ceil(x) + 0 -- make -0 go away
	else
		return math.floor(x)
	end
end

local function cmod(a, b)
	local x = a % b
	if a < 0 and x > 0 then
		x = x - b
	end
	return x
end


local radix = 2^24 -- maybe up to 2^26 is safe?
local radix_sqrt = fl(math.sqrt(radix))

local bigintmt -- forward decl

local function alloc()
	local bi = {}
	setmetatable(bi, bigintmt)
	bi.comps = {}
	bi.sign = 1;
	return bi
end

local function clone(a)
	local bi = alloc()
	bi.sign = a.sign
	local c = bi.comps
	local ac = a.comps
	for i = 1, #ac do
		c[i] = ac[i]
	end
	return bi
end

local function normalize(bi, notrunc)
	local c = bi.comps
	local v
	-- borrow for negative components
	for i = 1, #c - 1 do
		v = c[i]
		if v < 0 then
			c[i+1] = c[i+1] + fl(v / radix) - 1
			v = cmod(v, radix)
			if v ~= 0 then
				c[i] = v + radix
			else
				c[i] = v
				c[i+1] = c[i+1] + 1
			end
		end
	end
	-- is top component negative?
	if c[#c] < 0 then
		-- switch the sign and fix components
		bi.sign = -bi.sign
		for i = 1, #c - 1 do
			v = c[i]
			c[i] = radix - v
			c[i+1] = c[i+1] + 1
		end
		c[#c] = -c[#c]
	end
	-- carry for components larger than radix
	for i = 1, #c do
		v = c[i]
		if v > radix then
			c[i+1] = (c[i+1] or 0) + fl(v / radix)
			c[i] = cmod(v, radix)
		end
	end
	-- trim off leading zeros
	if not notrunc then
		for i = #c, 2, -1 do
			if c[i] == 0 then
				c[i] = nil
			else
				break
			end
		end
	end
	-- check for -0
	if #c == 1 and c[1] == 0 and bi.sign == -1 then
		bi.sign = 1
	end
end

local function negate(a)
	local bi = clone(a)
	bi.sign = -bi.sign
	return bi
end

local function compare(a, b)
	local ac, bc = a.comps, b.comps
	local as, bs = a.sign, b.sign
	if ac == bc then
		return 0
	elseif as > bs then
		return 1
	elseif as < bs then
		return -1
	elseif #ac > #bc then
		return as
	elseif #ac < #bc then
		return -as
	end
	for i = #ac, 1, -1 do
		if ac[i] > bc[i] then
			return as
		elseif ac[i] < bc[i] then
			return -as
		end
	end
	return 0
end

local function lt(a, b)
	return compare(a, b) < 0
end

local function eq(a, b)
	return compare(a, b) == 0
end

local function le(a, b)
	return compare(a, b) <= 0
end

local function addint(a, n)
	local bi = clone(a)
	if bi.sign == 1 then
		bi.comps[1] = bi.comps[1] + n
	else
		bi.comps[1] = bi.comps[1] - n
	end
	normalize(bi)
	return bi
end

local function add(a, b)
	if type(a) == "number" then
		return addint(b, a)
	elseif type(b) == "number" then
		return addint(a, b)
	end
	local bi = clone(a)
	local sign = bi.sign == b.sign
	local c = bi.comps
	for i = #c + 1, #b.comps do
		c[i] = 0
	end
	local bc = b.comps
	for i = 1, #bc do
		local v = bc[i]
		if sign then
			c[i] = c[i] + v
		else
			c[i] = c[i] - v
		end
	end
	normalize(bi)
	return bi
end

local function sub(a, b)
	if type(b) == "number" then
		return addint(a, -b)
	elseif type(a) == "number" then
		a = bigint(a)
	end
	return add(a, negate(b))
end

local function mulint(a, b)
	local bi = clone(a)
	if b < 0 then
		b = -b
		bi.sign = -bi.sign
	end
	local bc = bi.comps
	for i = 1, #bc do
		bc[i] = bc[i] * b
	end
	normalize(bi)
	return bi
end

local function multiply(a, b)
	local bi = alloc()
	local c = bi.comps
	local ac, bc = a.comps, b.comps
	for i = 1, #ac + #bc do
		c[i] = 0
	end
	for i = 1, #ac do
		for j = 1, #bc do
			c[i+j-1] = c[i+j-1] + ac[i] * bc[j]
		end
		-- keep the zeroes
		normalize(bi, true)
	end
	normalize(bi)
	if bi ~= bigint(0) then
		bi.sign = a.sign * b.sign
	end
	return bi
end

local function kmul(a, b)
	local ac, bc = a.comps, b.comps
	local an, bn = #a.comps, #b.comps
	local bi, bj, bk, bl = alloc(), alloc(), alloc(), alloc()
	local ic, jc, kc, lc = bi.comps, bj.comps, bk.comps, bl.comps

	local n = fl((math.max(an, bn) + 1) / 2)
	for i = 1, n do
		ic[i] = (i + n <= an) and ac[i+n] or 0
		jc[i] = (i <= an) and ac[i] or 0
		kc[i] = (i + n <= bn) and bc[i+n] or 0
		lc[i] = (i <= bn) and bc[i] or 0
	end
	normalize(bi)
	normalize(bj)
	normalize(bk)
	normalize(bl)
	local ik = bi * bk
	local jl = bj * bl
	local mid = (bi + bj) * (bk + bl) - ik - jl
	local mc = mid.comps
	local ikc = ik.comps
	local jlc = jl.comps
	for i = 1, #ikc + n*2 do -- fill it up
		jlc[i] = jlc[i] or 0
	end
	for i = 1, #mc do
		jlc[i+n] = jlc[i+n] + mc[i]
	end
	for i = 1, #ikc do
		jlc[i+n*2] = jlc[i+n*2] + ikc[i]
	end
	jl.sign = a.sign * b.sign
	normalize(jl)
	return jl
end

local kthresh = 12

local function mul(a, b)
	if type(a) == "number" then
		return mulint(b, a)
	elseif type(b) == "number" then
		return mulint(a, b)
	end
	if #a.comps < kthresh or #b.comps < kthresh then
		return multiply(a, b)
	end
	return kmul(a, b)
end

local function divint(numer, denom)
	local bi = clone(numer)
	if denom < 0 then
		denom = -denom
		bi.sign = -bi.sign
	end
	local r = 0
	local c = bi.comps
	for i = #c, 1, -1 do
		r = r * radix + c[i]
		c[i] = fl(r / denom)
		r = cmod(r, denom)
	end
	normalize(bi)
	return bi
end

local function multi_divide(numer, denom)
	local n = #denom.comps
	local approx = divint(numer, denom.comps[n])
	for i = n, #approx.comps do
		approx.comps[i - n + 1] = approx.comps[i]
	end
	for i = #approx.comps, #approx.comps - n + 2, -1 do
		approx.comps[i] = nil
	end
	local rem = approx * denom - numer
	if rem < denom then
		quotient = approx
	else
		quotient = approx - multi_divide(rem, denom)
	end
	return quotient
end

local function multi_divide_wrap(numer, denom)
	-- we use a successive approximation method, but it doesn't work
	-- if the high order component is too small.  adjust if needed.
	if denom.comps[#denom.comps] < radix_sqrt then
		numer = mulint(numer, radix_sqrt)
		denom = mulint(denom, radix_sqrt)
	end
	return multi_divide(numer, denom)
end

local function div(numer, denom)
	if type(denom) == "number" then
		if denom == 0 then
			error("divide by 0", 2)
		end
		return divint(numer, denom)
	elseif type(numer) == "number" then
		numer = bigint(numer)
	end
	-- check signs and trivial cases
	local sign = 1
	local cmp = compare(denom, bigint(0))
	if cmp == 0 then
		error("divide by 0", 2)
	elseif cmp == -1 then
		sign = -sign
		denom = negate(denom)
	end
	cmp = compare(numer, bigint(0))
	if cmp == 0 then
		return bigint(0)
	elseif cmp == -1 then
		sign = -sign
		numer = negate(numer)
	end
	cmp = compare(numer, denom)
	if cmp == -1 then
		return bigint(0)
	elseif cmp == 0 then
		return bigint(sign)
	end
	local bi
	-- if small enough, do it the easy way
	if #denom.comps == 1 then
		bi = divint(numer, denom.comps[1])
	else
		bi = multi_divide_wrap(numer, denom)
	end
	if sign == -1 then
		bi = negate(bi)
	end
	return bi
end

local function intrem(bi, m)
	if m < 0 then
		m = -m
	end
	local rad_r = 1
	local r = 0
	local bc = bi.comps
	for i = 1, #bc do
		local v = bc[i]
		r = cmod(r + v * rad_r, m)
		rad_r = cmod(rad_r * radix, m)
	end
	if bi.sign < 1 then
		r = -r
	end
	return r
end

local function intmod(bi, m)
	local r = intrem(bi, m)
	if r < 0 then
		r = r + m
	end
	return r
end

local function rem(bi, m)
	if type(m) == "number" then
		return bigint(intrem(bi, m))
	elseif type(bi) == "number" then
		bi = bigint(bi)
	end

	return bi - ((bi / m) * m)
end

local function mod(a, m)
	local bi = rem(a, m)
	if bi.sign == -1 then
		bi = bi + m
	end
	return bi
end

local printscale = 10000000
local printscalefmt = string.format("%%.%dd", math.log10(printscale))
local function makestr(bi, s)
	if bi >= bigint(printscale) then
		makestr(divint(bi, printscale), s)
	end
	table.insert(s, string.format(printscalefmt, intmod(bi, printscale)))
end

local function biginttostring(bi)
	local s = {}
	if bi < bigint(0) then
		bi = negate(bi)
		table.insert(s, "-")
	end
	makestr(bi, s)
	s = table.concat(s):gsub("^0*", "")
	if s == "" then s = "0" end
	return s
end

local function biginttonumber(bi)
	return tonumber(biginttostring(bi))
end

bigintmt = {
	__add = add,
	__sub = sub,
	__mul = mul,
	__div = div,
	__mod = mod,
	__unm = negate,
	__eq = eq,
	__lt = lt,
	__le = le,
	__tostring = biginttostring,
}

local cache = {}
local ncache = 0

function bigint(n)
	if cache[n] then
		return cache[n]
	end
	local bi
	if type(n) == "string" then
		local digits = { n:byte(1, -1) }
		for i = 1, #digits do
			digits[i] = string.char(digits[i])
		end
		local start = 1
		local sign = 1
		if digits[i] == '-' then
			sign = -1
			start = 2
		end
		bi = bigint(0)
		for i = start, #digits do
			bi = addint(mulint(bi, 10), tonumber(digits[i]))
		end
		bi = mulint(bi, sign)
	else
		bi = alloc()
		bi.comps[1] = n
		normalize(bi)
	end
	if ncache > 100 then
		cache = {}
		ncache = 0
	end
	cache[n] = bi
	ncache = ncache + 1
	return bi
end

local powersTwo = {
bigint("2"),
bigint("4"),
bigint("8"),
bigint("16"),
bigint("32"),
bigint("64"),
bigint("128"),
bigint("256"),
bigint("512"),
bigint("1024"),
bigint("2048"),
bigint("4096"),
bigint("8192"),
bigint("16384"),
bigint("32768"),
bigint("65536"),
bigint("131072"),
bigint("262144"),
bigint("524288"),
bigint("1048576"),
bigint("2097152"),
bigint("4194304"),
bigint("8388608"),
bigint("16777216"),
bigint("33554432"),
bigint("67108864"),
bigint("134217728"),
bigint("268435456"),
bigint("536870912"),
bigint("1073741824"),
bigint("2147483648"),
bigint("4294967296"),
bigint("8589934592"),
bigint("17179869184"),
bigint("34359738368"),
bigint("68719476736"),
bigint("137438953472"),
bigint("274877906944"),
bigint("549755813888"),
bigint("1099511627776"),
bigint("2199023255552"),
bigint("4398046511104"),
bigint("8796093022208"),
bigint("17592186044416"),
bigint("35184372088832"),
bigint("70368744177664"),
bigint("140737488355328"),
bigint("281474976710656"),
bigint("562949953421312"),
bigint("1125899906842624"),
bigint("2251799813685248"),
bigint("4503599627370496"),
bigint("9007199254740992"),
bigint("18014398509481984"),
bigint("36028797018963968"),
bigint("72057594037927936"),
bigint("144115188075855872"),
bigint("288230376151711744"),
bigint("576460752303423488"),
bigint("1152921504606846976"),
bigint("2305843009213693952"),
bigint("4611686018427387904"),
bigint("9223372036854775808"),
bigint("18446744073709551616"),
bigint("36893488147419103232"),
bigint("73786976294838206464"),
bigint("147573952589676412928"),
bigint("295147905179352825856"),
bigint("590295810358705651712"),
bigint("1180591620717411303424"),
bigint("2361183241434822606848"),
bigint("4722366482869645213696"),
bigint("9444732965739290427392"),
bigint("18889465931478580854784"),
bigint("37778931862957161709568"),
bigint("75557863725914323419136"),
bigint("151115727451828646838272"),
bigint("302231454903657293676544"),
bigint("604462909807314587353088"),
bigint("1208925819614629174706176"),
bigint("2417851639229258349412352"),
bigint("4835703278458516698824704"),
bigint("9671406556917033397649408"),
bigint("19342813113834066795298816"),
bigint("38685626227668133590597632"),
bigint("77371252455336267181195264"),
bigint("154742504910672534362390528"),
bigint("309485009821345068724781056"),
bigint("618970019642690137449562112"),
bigint("1237940039285380274899124224"),
bigint("2475880078570760549798248448"),
bigint("4951760157141521099596496896"),
bigint("9903520314283042199192993792"),
bigint("19807040628566084398385987584"),
bigint("39614081257132168796771975168"),
bigint("79228162514264337593543950336"),
bigint("158456325028528675187087900672"),
bigint("316912650057057350374175801344"),
bigint("633825300114114700748351602688"),
bigint("1267650600228229401496703205376"),
bigint("2535301200456458802993406410752"),
bigint("5070602400912917605986812821504"),
bigint("10141204801825835211973625643008"),
bigint("20282409603651670423947251286016"),
bigint("40564819207303340847894502572032"),
bigint("81129638414606681695789005144064"),
bigint("162259276829213363391578010288128"),
bigint("324518553658426726783156020576256"),
bigint("649037107316853453566312041152512"),
bigint("1298074214633706907132624082305024"),
bigint("2596148429267413814265248164610048"),
bigint("5192296858534827628530496329220096"),
bigint("10384593717069655257060992658440192"),
bigint("20769187434139310514121985316880384"),
bigint("41538374868278621028243970633760768"),
bigint("83076749736557242056487941267521536"),
bigint("166153499473114484112975882535043072"),
bigint("332306998946228968225951765070086144"),
bigint("664613997892457936451903530140172288"),
bigint("1329227995784915872903807060280344576"),
bigint("2658455991569831745807614120560689152"),
bigint("5316911983139663491615228241121378304"),
bigint("10633823966279326983230456482242756608"),
bigint("21267647932558653966460912964485513216"),
bigint("42535295865117307932921825928971026432"),
bigint("85070591730234615865843651857942052864"),
bigint("170141183460469231731687303715884105728"),
bigint("340282366920938463463374607431768211456"),
bigint("680564733841876926926749214863536422912"),
bigint("1361129467683753853853498429727072845824"),
bigint("2722258935367507707706996859454145691648"),
bigint("5444517870735015415413993718908291383296"),
bigint("10889035741470030830827987437816582766592"),
bigint("21778071482940061661655974875633165533184"),
bigint("43556142965880123323311949751266331066368"),
bigint("87112285931760246646623899502532662132736"),
bigint("174224571863520493293247799005065324265472"),
bigint("348449143727040986586495598010130648530944"),
bigint("696898287454081973172991196020261297061888"),
bigint("1393796574908163946345982392040522594123776"),
bigint("2787593149816327892691964784081045188247552"),
bigint("5575186299632655785383929568162090376495104"),
bigint("11150372599265311570767859136324180752990208"),
bigint("22300745198530623141535718272648361505980416"),
bigint("44601490397061246283071436545296723011960832"),
bigint("89202980794122492566142873090593446023921664"),
bigint("178405961588244985132285746181186892047843328"),
bigint("356811923176489970264571492362373784095686656"),
bigint("713623846352979940529142984724747568191373312"),
bigint("1427247692705959881058285969449495136382746624"),
bigint("2854495385411919762116571938898990272765493248"),
bigint("5708990770823839524233143877797980545530986496"),
bigint("11417981541647679048466287755595961091061972992"),
bigint("22835963083295358096932575511191922182123945984"),
bigint("45671926166590716193865151022383844364247891968"),
bigint("91343852333181432387730302044767688728495783936"),
bigint("182687704666362864775460604089535377456991567872"),
bigint("365375409332725729550921208179070754913983135744"),
bigint("730750818665451459101842416358141509827966271488"),
bigint("1461501637330902918203684832716283019655932542976"),
bigint("2923003274661805836407369665432566039311865085952"),
bigint("5846006549323611672814739330865132078623730171904"),
bigint("11692013098647223345629478661730264157247460343808"),
bigint("23384026197294446691258957323460528314494920687616"),
bigint("46768052394588893382517914646921056628989841375232"),
bigint("93536104789177786765035829293842113257979682750464"),
bigint("187072209578355573530071658587684226515959365500928"),
bigint("374144419156711147060143317175368453031918731001856"),
bigint("748288838313422294120286634350736906063837462003712"),
bigint("1496577676626844588240573268701473812127674924007424"),
bigint("2993155353253689176481146537402947624255349848014848"),
bigint("5986310706507378352962293074805895248510699696029696"),
bigint("11972621413014756705924586149611790497021399392059392"),
bigint("23945242826029513411849172299223580994042798784118784"),
bigint("47890485652059026823698344598447161988085597568237568"),
bigint("95780971304118053647396689196894323976171195136475136"),
bigint("191561942608236107294793378393788647952342390272950272"),
bigint("383123885216472214589586756787577295904684780545900544"),
bigint("766247770432944429179173513575154591809369561091801088"),
bigint("1532495540865888858358347027150309183618739122183602176"),
bigint("3064991081731777716716694054300618367237478244367204352"),
bigint("6129982163463555433433388108601236734474956488734408704"),
bigint("12259964326927110866866776217202473468949912977468817408"),
bigint("24519928653854221733733552434404946937899825954937634816"),
bigint("49039857307708443467467104868809893875799651909875269632"),
bigint("98079714615416886934934209737619787751599303819750539264"),
bigint("196159429230833773869868419475239575503198607639501078528"),
bigint("392318858461667547739736838950479151006397215279002157056"),
bigint("784637716923335095479473677900958302012794430558004314112"),
bigint("1569275433846670190958947355801916604025588861116008628224"),
bigint("3138550867693340381917894711603833208051177722232017256448"),
bigint("6277101735386680763835789423207666416102355444464034512896"),
bigint("12554203470773361527671578846415332832204710888928069025792"),
bigint("25108406941546723055343157692830665664409421777856138051584"),
bigint("50216813883093446110686315385661331328818843555712276103168"),
bigint("100433627766186892221372630771322662657637687111424552206336"),
bigint("200867255532373784442745261542645325315275374222849104412672"),
bigint("401734511064747568885490523085290650630550748445698208825344"),
bigint("803469022129495137770981046170581301261101496891396417650688"),
bigint("1606938044258990275541962092341162602522202993782792835301376"),
bigint("3213876088517980551083924184682325205044405987565585670602752"),
bigint("6427752177035961102167848369364650410088811975131171341205504"),
bigint("12855504354071922204335696738729300820177623950262342682411008"),
bigint("25711008708143844408671393477458601640355247900524685364822016"),
bigint("51422017416287688817342786954917203280710495801049370729644032"),
bigint("102844034832575377634685573909834406561420991602098741459288064"),
bigint("205688069665150755269371147819668813122841983204197482918576128"),
bigint("411376139330301510538742295639337626245683966408394965837152256"),
bigint("822752278660603021077484591278675252491367932816789931674304512"),
bigint("1645504557321206042154969182557350504982735865633579863348609024"),
bigint("3291009114642412084309938365114701009965471731267159726697218048"),
bigint("6582018229284824168619876730229402019930943462534319453394436096"),
bigint("13164036458569648337239753460458804039861886925068638906788872192"),
bigint("26328072917139296674479506920917608079723773850137277813577744384"),
bigint("52656145834278593348959013841835216159447547700274555627155488768"),
bigint("105312291668557186697918027683670432318895095400549111254310977536"),
bigint("210624583337114373395836055367340864637790190801098222508621955072"),
bigint("421249166674228746791672110734681729275580381602196445017243910144"),
bigint("842498333348457493583344221469363458551160763204392890034487820288"),
bigint("1684996666696914987166688442938726917102321526408785780068975640576"),
bigint("3369993333393829974333376885877453834204643052817571560137951281152"),
bigint("6739986666787659948666753771754907668409286105635143120275902562304"),
bigint("13479973333575319897333507543509815336818572211270286240551805124608"),
bigint("26959946667150639794667015087019630673637144422540572481103610249216"),
bigint("53919893334301279589334030174039261347274288845081144962207220498432"),
bigint("107839786668602559178668060348078522694548577690162289924414440996864"),
bigint("215679573337205118357336120696157045389097155380324579848828881993728"),
bigint("431359146674410236714672241392314090778194310760649159697657763987456"),
bigint("862718293348820473429344482784628181556388621521298319395315527974912"),
bigint("1725436586697640946858688965569256363112777243042596638790631055949824"),
bigint("3450873173395281893717377931138512726225554486085193277581262111899648"),
bigint("6901746346790563787434755862277025452451108972170386555162524223799296"),
bigint("13803492693581127574869511724554050904902217944340773110325048447598592"),
bigint("27606985387162255149739023449108101809804435888681546220650096895197184"),
bigint("55213970774324510299478046898216203619608871777363092441300193790394368"),
bigint("110427941548649020598956093796432407239217743554726184882600387580788736"),
bigint("220855883097298041197912187592864814478435487109452369765200775161577472"),
bigint("441711766194596082395824375185729628956870974218904739530401550323154944"),
bigint("883423532389192164791648750371459257913741948437809479060803100646309888"),
bigint("1766847064778384329583297500742918515827483896875618958121606201292619776"),
bigint("3533694129556768659166595001485837031654967793751237916243212402585239552"),
bigint("7067388259113537318333190002971674063309935587502475832486424805170479104"),
bigint("14134776518227074636666380005943348126619871175004951664972849610340958208"),
bigint("28269553036454149273332760011886696253239742350009903329945699220681916416"),
bigint("56539106072908298546665520023773392506479484700019806659891398441363832832"),
bigint("113078212145816597093331040047546785012958969400039613319782796882727665664"),
bigint("226156424291633194186662080095093570025917938800079226639565593765455331328"),
bigint("452312848583266388373324160190187140051835877600158453279131187530910662656"),
bigint("904625697166532776746648320380374280103671755200316906558262375061821325312"),
bigint("1809251394333065553493296640760748560207343510400633813116524750123642650624"),
bigint("3618502788666131106986593281521497120414687020801267626233049500247285301248"),
bigint("7237005577332262213973186563042994240829374041602535252466099000494570602496"),
bigint("14474011154664524427946373126085988481658748083205070504932198000989141204992"),
bigint("28948022309329048855892746252171976963317496166410141009864396001978282409984"),
bigint("57896044618658097711785492504343953926634992332820282019728792003956564819968"),
bigint("115792089237316195423570985008687907853269984665640564039457584007913129639936"),
}

powersTwo[0] = bigint("1")

local bigZero = bigint(0)
local bigOne = bigint(1)

local function numberToBytes(num, bits, byteSize)
	if bits > #powersTwo then
		error("Too many bits. Must be <= " .. #powersTwo .. ".")
	end

	num = bigint(num)

	local resultBits = {}
	resultBits[1] = {}
	for i = bits - 1, 0, -1 do
		local expVal = powersTwo[i]
		local resultant = num - expVal
		if expVal <= resultant then
			-- Invalid data!
			return nil
		end

		if resultant < bigZero then
			-- A zero bit
			if #(resultBits[#resultBits]) >= byteSize then
				table.insert(resultBits, {0})
			else
				table.insert(resultBits[#resultBits], 0)
			end
		else
			-- A one bit
			num = resultant
			if #(resultBits[#resultBits]) >= byteSize then
				table.insert(resultBits, {1})
			else
				table.insert(resultBits[#resultBits], 1)
			end
		end

		if num == bigint(0) then
			break
		end
	end

	local results = {}
	for _, binarySeq in pairs(resultBits) do
		local thisResult = 0
		for k, bin in pairs(binarySeq) do
			if bin == 1 then
				thisResult = thisResult + 2^(byteSize - k)
			end
		end
		table.insert(results, thisResult)
	end

	return results
end

local function bytesToNumber(bytes, bits, byteSize)
	if bits > #powersTwo then
		error("Too many bits. Must be <= " .. #powersTwo .. ".")
	end

	if #bytes > bits/byteSize then
		error("Too many bytes to store into the number of bits available. Must be <= " ..
			bits/byteSize .. ".")
	end

	local binary = {}
	for _, byte in pairs(bytes) do
		for i = byteSize - 1, 0, -1 do
			if byte - (2 ^ i) < 0 then
				table.insert(binary, 0)
			else
				table.insert(binary, 1)
				byte = byte - (2 ^ i)
			end
		end
	end

	local num = bigint(0)
	for i = 1, #binary do
		if binary[i] == 1 then
			num = num + powersTwo[bits - i]
		end
	end

	return tostring(num)
end

local function encodeBigNumbers(numbers)
	for k, v in pairs(numbers) do
		numbers[k] = tostring(v)
	end
	return numbers
end

local function stringToBytes(str)
	local result = {}
	for i = 1, #str do
		table.insert(result, string.byte(str, i))
	end
	return result
end

local function bytesToString(bytes)
	local str = ""
	for _, v in pairs(bytes) do
		str = str .. string.char(v)
	end
	return str
end

local function modexp(base, exponent, modulus)
	local r = 1

	while true do
		if exponent % 2 == bigOne then
			r = r * base % modulus
		end
		exponent = exponent / 2

		if exponent == bigZero then
			break
		end
		base = base * base % modulus
	end

	return r
end

local function crypt(key, number)
	local exp
	if key.public then
		exp = bigint(key.public)
	else
		exp = bigint(key.private)
	end

	return tostring(modexp(bigint(number), exp, bigint(key.shared)))
end

function ll.cryptolib.rsaEncrypt(msg, key, bits, byteSize)
	local res = bytesToNumber(stringToBytes(msg), bits, byteSize)
	return crypt(key, res)
end

function ll.cryptolib.rsaDecrypt(msg, key, bits, byteSize)
	local decrypted = crypt(privateKey, encrypted)
	return bytesToString(numberToBytes(decrypted, bits, byteSize))
end

local aes = {}

local sbox = {
[0]=0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76,
0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0,
0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15,
0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75,
0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84,
0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF,
0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8,
0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2,
0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73,
0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB,
0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79,
0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08,
0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A,
0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E,
0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF,
0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16}

local inv_sbox = {
[0]=0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB,
0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB,
0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E,
0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25,
0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92,
0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84,
0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06,
0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B,
0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73,
0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E,
0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B,
0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4,
0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F,
0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF,
0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61,
0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D}

local Rcon = {
[0]=0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a,
0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39,
0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a,
0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8,
0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef,
0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc,
0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b,
0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3,
0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94,
0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20,
0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63, 0xc6, 0x97, 0x35,
0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd, 0x61, 0xc2, 0x9f,
0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d, 0x01, 0x02, 0x04,
0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36, 0x6c, 0xd8, 0xab, 0x4d, 0x9a, 0x2f, 0x5e, 0xbc, 0x63,
0xc6, 0x97, 0x35, 0x6a, 0xd4, 0xb3, 0x7d, 0xfa, 0xef, 0xc5, 0x91, 0x39, 0x72, 0xe4, 0xd3, 0xbd,
0x61, 0xc2, 0x9f, 0x25, 0x4a, 0x94, 0x33, 0x66, 0xcc, 0x83, 0x1d, 0x3a, 0x74, 0xe8, 0xcb, 0x8d}

-- Finite-field multiplication lookup tables:

local mul_2 = {
[0]=0x00,0x02,0x04,0x06,0x08,0x0a,0x0c,0x0e,0x10,0x12,0x14,0x16,0x18,0x1a,0x1c,0x1e,
0x20,0x22,0x24,0x26,0x28,0x2a,0x2c,0x2e,0x30,0x32,0x34,0x36,0x38,0x3a,0x3c,0x3e,
0x40,0x42,0x44,0x46,0x48,0x4a,0x4c,0x4e,0x50,0x52,0x54,0x56,0x58,0x5a,0x5c,0x5e,
0x60,0x62,0x64,0x66,0x68,0x6a,0x6c,0x6e,0x70,0x72,0x74,0x76,0x78,0x7a,0x7c,0x7e,
0x80,0x82,0x84,0x86,0x88,0x8a,0x8c,0x8e,0x90,0x92,0x94,0x96,0x98,0x9a,0x9c,0x9e,
0xa0,0xa2,0xa4,0xa6,0xa8,0xaa,0xac,0xae,0xb0,0xb2,0xb4,0xb6,0xb8,0xba,0xbc,0xbe,
0xc0,0xc2,0xc4,0xc6,0xc8,0xca,0xcc,0xce,0xd0,0xd2,0xd4,0xd6,0xd8,0xda,0xdc,0xde,
0xe0,0xe2,0xe4,0xe6,0xe8,0xea,0xec,0xee,0xf0,0xf2,0xf4,0xf6,0xf8,0xfa,0xfc,0xfe,
0x1b,0x19,0x1f,0x1d,0x13,0x11,0x17,0x15,0x0b,0x09,0x0f,0x0d,0x03,0x01,0x07,0x05,
0x3b,0x39,0x3f,0x3d,0x33,0x31,0x37,0x35,0x2b,0x29,0x2f,0x2d,0x23,0x21,0x27,0x25,
0x5b,0x59,0x5f,0x5d,0x53,0x51,0x57,0x55,0x4b,0x49,0x4f,0x4d,0x43,0x41,0x47,0x45,
0x7b,0x79,0x7f,0x7d,0x73,0x71,0x77,0x75,0x6b,0x69,0x6f,0x6d,0x63,0x61,0x67,0x65,
0x9b,0x99,0x9f,0x9d,0x93,0x91,0x97,0x95,0x8b,0x89,0x8f,0x8d,0x83,0x81,0x87,0x85,
0xbb,0xb9,0xbf,0xbd,0xb3,0xb1,0xb7,0xb5,0xab,0xa9,0xaf,0xad,0xa3,0xa1,0xa7,0xa5,
0xdb,0xd9,0xdf,0xdd,0xd3,0xd1,0xd7,0xd5,0xcb,0xc9,0xcf,0xcd,0xc3,0xc1,0xc7,0xc5,
0xfb,0xf9,0xff,0xfd,0xf3,0xf1,0xf7,0xf5,0xeb,0xe9,0xef,0xed,0xe3,0xe1,0xe7,0xe5,
}

local mul_3 = {
[0]=0x00,0x03,0x06,0x05,0x0c,0x0f,0x0a,0x09,0x18,0x1b,0x1e,0x1d,0x14,0x17,0x12,0x11,
0x30,0x33,0x36,0x35,0x3c,0x3f,0x3a,0x39,0x28,0x2b,0x2e,0x2d,0x24,0x27,0x22,0x21,
0x60,0x63,0x66,0x65,0x6c,0x6f,0x6a,0x69,0x78,0x7b,0x7e,0x7d,0x74,0x77,0x72,0x71,
0x50,0x53,0x56,0x55,0x5c,0x5f,0x5a,0x59,0x48,0x4b,0x4e,0x4d,0x44,0x47,0x42,0x41,
0xc0,0xc3,0xc6,0xc5,0xcc,0xcf,0xca,0xc9,0xd8,0xdb,0xde,0xdd,0xd4,0xd7,0xd2,0xd1,
0xf0,0xf3,0xf6,0xf5,0xfc,0xff,0xfa,0xf9,0xe8,0xeb,0xee,0xed,0xe4,0xe7,0xe2,0xe1,
0xa0,0xa3,0xa6,0xa5,0xac,0xaf,0xaa,0xa9,0xb8,0xbb,0xbe,0xbd,0xb4,0xb7,0xb2,0xb1,
0x90,0x93,0x96,0x95,0x9c,0x9f,0x9a,0x99,0x88,0x8b,0x8e,0x8d,0x84,0x87,0x82,0x81,
0x9b,0x98,0x9d,0x9e,0x97,0x94,0x91,0x92,0x83,0x80,0x85,0x86,0x8f,0x8c,0x89,0x8a,
0xab,0xa8,0xad,0xae,0xa7,0xa4,0xa1,0xa2,0xb3,0xb0,0xb5,0xb6,0xbf,0xbc,0xb9,0xba,
0xfb,0xf8,0xfd,0xfe,0xf7,0xf4,0xf1,0xf2,0xe3,0xe0,0xe5,0xe6,0xef,0xec,0xe9,0xea,
0xcb,0xc8,0xcd,0xce,0xc7,0xc4,0xc1,0xc2,0xd3,0xd0,0xd5,0xd6,0xdf,0xdc,0xd9,0xda,
0x5b,0x58,0x5d,0x5e,0x57,0x54,0x51,0x52,0x43,0x40,0x45,0x46,0x4f,0x4c,0x49,0x4a,
0x6b,0x68,0x6d,0x6e,0x67,0x64,0x61,0x62,0x73,0x70,0x75,0x76,0x7f,0x7c,0x79,0x7a,
0x3b,0x38,0x3d,0x3e,0x37,0x34,0x31,0x32,0x23,0x20,0x25,0x26,0x2f,0x2c,0x29,0x2a,
0x0b,0x08,0x0d,0x0e,0x07,0x04,0x01,0x02,0x13,0x10,0x15,0x16,0x1f,0x1c,0x19,0x1a,
}

local mul_9 = {
[0]=0x00,0x09,0x12,0x1b,0x24,0x2d,0x36,0x3f,0x48,0x41,0x5a,0x53,0x6c,0x65,0x7e,0x77,
0x90,0x99,0x82,0x8b,0xb4,0xbd,0xa6,0xaf,0xd8,0xd1,0xca,0xc3,0xfc,0xf5,0xee,0xe7,
0x3b,0x32,0x29,0x20,0x1f,0x16,0x0d,0x04,0x73,0x7a,0x61,0x68,0x57,0x5e,0x45,0x4c,
0xab,0xa2,0xb9,0xb0,0x8f,0x86,0x9d,0x94,0xe3,0xea,0xf1,0xf8,0xc7,0xce,0xd5,0xdc,
0x76,0x7f,0x64,0x6d,0x52,0x5b,0x40,0x49,0x3e,0x37,0x2c,0x25,0x1a,0x13,0x08,0x01,
0xe6,0xef,0xf4,0xfd,0xc2,0xcb,0xd0,0xd9,0xae,0xa7,0xbc,0xb5,0x8a,0x83,0x98,0x91,
0x4d,0x44,0x5f,0x56,0x69,0x60,0x7b,0x72,0x05,0x0c,0x17,0x1e,0x21,0x28,0x33,0x3a,
0xdd,0xd4,0xcf,0xc6,0xf9,0xf0,0xeb,0xe2,0x95,0x9c,0x87,0x8e,0xb1,0xb8,0xa3,0xaa,
0xec,0xe5,0xfe,0xf7,0xc8,0xc1,0xda,0xd3,0xa4,0xad,0xb6,0xbf,0x80,0x89,0x92,0x9b,
0x7c,0x75,0x6e,0x67,0x58,0x51,0x4a,0x43,0x34,0x3d,0x26,0x2f,0x10,0x19,0x02,0x0b,
0xd7,0xde,0xc5,0xcc,0xf3,0xfa,0xe1,0xe8,0x9f,0x96,0x8d,0x84,0xbb,0xb2,0xa9,0xa0,
0x47,0x4e,0x55,0x5c,0x63,0x6a,0x71,0x78,0x0f,0x06,0x1d,0x14,0x2b,0x22,0x39,0x30,
0x9a,0x93,0x88,0x81,0xbe,0xb7,0xac,0xa5,0xd2,0xdb,0xc0,0xc9,0xf6,0xff,0xe4,0xed,
0x0a,0x03,0x18,0x11,0x2e,0x27,0x3c,0x35,0x42,0x4b,0x50,0x59,0x66,0x6f,0x74,0x7d,
0xa1,0xa8,0xb3,0xba,0x85,0x8c,0x97,0x9e,0xe9,0xe0,0xfb,0xf2,0xcd,0xc4,0xdf,0xd6,
0x31,0x38,0x23,0x2a,0x15,0x1c,0x07,0x0e,0x79,0x70,0x6b,0x62,0x5d,0x54,0x4f,0x46,
}

local mul_11 = {
[0]=0x00,0x0b,0x16,0x1d,0x2c,0x27,0x3a,0x31,0x58,0x53,0x4e,0x45,0x74,0x7f,0x62,0x69,
0xb0,0xbb,0xa6,0xad,0x9c,0x97,0x8a,0x81,0xe8,0xe3,0xfe,0xf5,0xc4,0xcf,0xd2,0xd9,
0x7b,0x70,0x6d,0x66,0x57,0x5c,0x41,0x4a,0x23,0x28,0x35,0x3e,0x0f,0x04,0x19,0x12,
0xcb,0xc0,0xdd,0xd6,0xe7,0xec,0xf1,0xfa,0x93,0x98,0x85,0x8e,0xbf,0xb4,0xa9,0xa2,
0xf6,0xfd,0xe0,0xeb,0xda,0xd1,0xcc,0xc7,0xae,0xa5,0xb8,0xb3,0x82,0x89,0x94,0x9f,
0x46,0x4d,0x50,0x5b,0x6a,0x61,0x7c,0x77,0x1e,0x15,0x08,0x03,0x32,0x39,0x24,0x2f,
0x8d,0x86,0x9b,0x90,0xa1,0xaa,0xb7,0xbc,0xd5,0xde,0xc3,0xc8,0xf9,0xf2,0xef,0xe4,
0x3d,0x36,0x2b,0x20,0x11,0x1a,0x07,0x0c,0x65,0x6e,0x73,0x78,0x49,0x42,0x5f,0x54,
0xf7,0xfc,0xe1,0xea,0xdb,0xd0,0xcd,0xc6,0xaf,0xa4,0xb9,0xb2,0x83,0x88,0x95,0x9e,
0x47,0x4c,0x51,0x5a,0x6b,0x60,0x7d,0x76,0x1f,0x14,0x09,0x02,0x33,0x38,0x25,0x2e,
0x8c,0x87,0x9a,0x91,0xa0,0xab,0xb6,0xbd,0xd4,0xdf,0xc2,0xc9,0xf8,0xf3,0xee,0xe5,
0x3c,0x37,0x2a,0x21,0x10,0x1b,0x06,0x0d,0x64,0x6f,0x72,0x79,0x48,0x43,0x5e,0x55,
0x01,0x0a,0x17,0x1c,0x2d,0x26,0x3b,0x30,0x59,0x52,0x4f,0x44,0x75,0x7e,0x63,0x68,
0xb1,0xba,0xa7,0xac,0x9d,0x96,0x8b,0x80,0xe9,0xe2,0xff,0xf4,0xc5,0xce,0xd3,0xd8,
0x7a,0x71,0x6c,0x67,0x56,0x5d,0x40,0x4b,0x22,0x29,0x34,0x3f,0x0e,0x05,0x18,0x13,
0xca,0xc1,0xdc,0xd7,0xe6,0xed,0xf0,0xfb,0x92,0x99,0x84,0x8f,0xbe,0xb5,0xa8,0xa3,
}

local mul_13 = {
[0]=0x00,0x0d,0x1a,0x17,0x34,0x39,0x2e,0x23,0x68,0x65,0x72,0x7f,0x5c,0x51,0x46,0x4b,
0xd0,0xdd,0xca,0xc7,0xe4,0xe9,0xfe,0xf3,0xb8,0xb5,0xa2,0xaf,0x8c,0x81,0x96,0x9b,
0xbb,0xb6,0xa1,0xac,0x8f,0x82,0x95,0x98,0xd3,0xde,0xc9,0xc4,0xe7,0xea,0xfd,0xf0,
0x6b,0x66,0x71,0x7c,0x5f,0x52,0x45,0x48,0x03,0x0e,0x19,0x14,0x37,0x3a,0x2d,0x20,
0x6d,0x60,0x77,0x7a,0x59,0x54,0x43,0x4e,0x05,0x08,0x1f,0x12,0x31,0x3c,0x2b,0x26,
0xbd,0xb0,0xa7,0xaa,0x89,0x84,0x93,0x9e,0xd5,0xd8,0xcf,0xc2,0xe1,0xec,0xfb,0xf6,
0xd6,0xdb,0xcc,0xc1,0xe2,0xef,0xf8,0xf5,0xbe,0xb3,0xa4,0xa9,0x8a,0x87,0x90,0x9d,
0x06,0x0b,0x1c,0x11,0x32,0x3f,0x28,0x25,0x6e,0x63,0x74,0x79,0x5a,0x57,0x40,0x4d,
0xda,0xd7,0xc0,0xcd,0xee,0xe3,0xf4,0xf9,0xb2,0xbf,0xa8,0xa5,0x86,0x8b,0x9c,0x91,
0x0a,0x07,0x10,0x1d,0x3e,0x33,0x24,0x29,0x62,0x6f,0x78,0x75,0x56,0x5b,0x4c,0x41,
0x61,0x6c,0x7b,0x76,0x55,0x58,0x4f,0x42,0x09,0x04,0x13,0x1e,0x3d,0x30,0x27,0x2a,
0xb1,0xbc,0xab,0xa6,0x85,0x88,0x9f,0x92,0xd9,0xd4,0xc3,0xce,0xed,0xe0,0xf7,0xfa,
0xb7,0xba,0xad,0xa0,0x83,0x8e,0x99,0x94,0xdf,0xd2,0xc5,0xc8,0xeb,0xe6,0xf1,0xfc,
0x67,0x6a,0x7d,0x70,0x53,0x5e,0x49,0x44,0x0f,0x02,0x15,0x18,0x3b,0x36,0x21,0x2c,
0x0c,0x01,0x16,0x1b,0x38,0x35,0x22,0x2f,0x64,0x69,0x7e,0x73,0x50,0x5d,0x4a,0x47,
0xdc,0xd1,0xc6,0xcb,0xe8,0xe5,0xf2,0xff,0xb4,0xb9,0xae,0xa3,0x80,0x8d,0x9a,0x97,
}

local mul_14 = {
[0]=0x00,0x0e,0x1c,0x12,0x38,0x36,0x24,0x2a,0x70,0x7e,0x6c,0x62,0x48,0x46,0x54,0x5a,
0xe0,0xee,0xfc,0xf2,0xd8,0xd6,0xc4,0xca,0x90,0x9e,0x8c,0x82,0xa8,0xa6,0xb4,0xba,
0xdb,0xd5,0xc7,0xc9,0xe3,0xed,0xff,0xf1,0xab,0xa5,0xb7,0xb9,0x93,0x9d,0x8f,0x81,
0x3b,0x35,0x27,0x29,0x03,0x0d,0x1f,0x11,0x4b,0x45,0x57,0x59,0x73,0x7d,0x6f,0x61,
0xad,0xa3,0xb1,0xbf,0x95,0x9b,0x89,0x87,0xdd,0xd3,0xc1,0xcf,0xe5,0xeb,0xf9,0xf7,
0x4d,0x43,0x51,0x5f,0x75,0x7b,0x69,0x67,0x3d,0x33,0x21,0x2f,0x05,0x0b,0x19,0x17,
0x76,0x78,0x6a,0x64,0x4e,0x40,0x52,0x5c,0x06,0x08,0x1a,0x14,0x3e,0x30,0x22,0x2c,
0x96,0x98,0x8a,0x84,0xae,0xa0,0xb2,0xbc,0xe6,0xe8,0xfa,0xf4,0xde,0xd0,0xc2,0xcc,
0x41,0x4f,0x5d,0x53,0x79,0x77,0x65,0x6b,0x31,0x3f,0x2d,0x23,0x09,0x07,0x15,0x1b,
0xa1,0xaf,0xbd,0xb3,0x99,0x97,0x85,0x8b,0xd1,0xdf,0xcd,0xc3,0xe9,0xe7,0xf5,0xfb,
0x9a,0x94,0x86,0x88,0xa2,0xac,0xbe,0xb0,0xea,0xe4,0xf6,0xf8,0xd2,0xdc,0xce,0xc0,
0x7a,0x74,0x66,0x68,0x42,0x4c,0x5e,0x50,0x0a,0x04,0x16,0x18,0x32,0x3c,0x2e,0x20,
0xec,0xe2,0xf0,0xfe,0xd4,0xda,0xc8,0xc6,0x9c,0x92,0x80,0x8e,0xa4,0xaa,0xb8,0xb6,
0x0c,0x02,0x10,0x1e,0x34,0x3a,0x28,0x26,0x7c,0x72,0x60,0x6e,0x44,0x4a,0x58,0x56,
0x37,0x39,0x2b,0x25,0x0f,0x01,0x13,0x1d,0x47,0x49,0x5b,0x55,0x7f,0x71,0x63,0x6d,
0xd7,0xd9,0xcb,0xc5,0xef,0xe1,0xf3,0xfd,0xa7,0xa9,0xbb,0xb5,0x9f,0x91,0x83,0x8d,
}

local bxor = bit.bxor
local insert = table.insert

local function copy(input)
	local c = {}
	for i, v in pairs(input) do
		c[i] = v
	end
	return c
end

local function subBytes(input, invert)
	for i=1, #input do
		if not (sbox[input[i]] and inv_sbox[input[i]]) then
			error("subBytes: input["..i.."] > 0xFF")
		end
		if invert then
			input[i] = inv_sbox[input[i]]
		else
			input[i] = sbox[input[i]]
		end
	end
	return input
end

local function shiftRows(input)
	local copy = {}
	-- Row 1: No change
	copy[1] = input[1]
	copy[2] = input[2]
	copy[3] = input[3]
	copy[4] = input[4]
	-- Row 2: Offset 1
	copy[5] = input[6]
	copy[6] = input[7]
	copy[7] = input[8]
	copy[8] = input[5]
	-- Row 3: Offset 2
	copy[9] = input[11]
	copy[10] = input[12]
	copy[11] = input[9]
	copy[12] = input[10]
	-- Row 4: Offset 3
	copy[13] = input[16]
	copy[14] = input[13]
	copy[15] = input[14]
	copy[16] = input[15]
	return copy
end

local function invShiftRows(input)
	local copy = {}
	-- Row 1: No change
	copy[1] = input[1]
	copy[2] = input[2]
	copy[3] = input[3]
	copy[4] = input[4]
	-- Row 2: Offset 1
	copy[5] = input[8]
	copy[6] = input[5]
	copy[7] = input[6]
	copy[8] = input[7]
	-- Row 3: Offset 2
	copy[9] = input[11]
	copy[10] = input[12]
	copy[11] = input[9]
	copy[12] = input[10]
	-- Row 4: Offset 3
	copy[13] = input[14]
	copy[14] = input[15]
	copy[15] = input[16]
	copy[16] = input[13]
	return copy
end

local function finite_field_mul(a,b) -- Multiply two numbers in GF(256), assuming that polynomials are 8 bits wide
	local product = 0
	local mulA, mulB = a,b
	for i=1, 8 do
		--print("FFMul: MulA: "..mulA.." MulB: "..mulB)
		if mulA == 0 or mulB == 0 then
			break
		end
		if bit.band(1, mulB) > 0 then
			product = bxor(product, mulA)
		end
		mulB = bit.brshift(mulB, 1)
		local carry = bit.band(0x80, mulA)
		mulA = bit.band(0xFF, bit.blshift(mulA, 1))
		if carry > 0 then
			mulA = bxor( mulA, 0x1B )
		end
	end
	return product
end

local function mixColumn(column)
	local output = {}
	--print("MixColumn: #column: "..#column)
	output[1] = bxor( mul_2[column[1]], bxor( mul_3[column[2]], bxor( column[3], column[4] ) ) )
	output[2] = bxor( column[1], bxor( mul_2[column[2]], bxor( mul_3[column[3]], column[4] ) ) )
	output[3] = bxor( column[1], bxor( column[2], bxor( mul_2[column[3]], mul_3[column[4]] ) ) )
	output[4] = bxor( mul_3[column[1]], bxor( column[2], bxor( column[3], mul_2[column[4]] ) ) )
	return output
end

local function invMixColumn(column)
	local output = {}
	--print("InvMixColumn: #column: "..#column)
	output[1] = bxor( mul_14[column[1]], bxor( mul_11[column[2]], bxor( mul_13[column[3]], mul_9[column[4]] ) ) )
	output[2] = bxor( mul_9[column[1]], bxor( mul_14[column[2]], bxor( mul_11[column[3]], mul_13[column[4]] ) ) )
	output[3] = bxor( mul_13[column[1]], bxor( mul_9[column[2]], bxor( mul_14[column[3]], mul_11[column[4]] ) ) )
	output[4] = bxor( mul_11[column[1]], bxor( mul_13[column[2]], bxor( mul_9[column[3]], mul_14[column[4]] ) ) )
	return output
end

local function mixColumns(input, invert)
	--print("MixColumns: #input: "..#input)
	-- Ooops. I mixed the ROWS instead of the COLUMNS on accident.
	local output = {}
	--[[
	local c1 = { input[1], input[2], input[3], input[4] }
	local c2 = { input[5], input[6], input[7], input[8] }
	local c3 = { input[9], input[10], input[11], input[12] }
	local c4 = { input[13], input[14], input[15], input[16] }
	]]
	local c1 = { input[1], input[5], input[9], input[13] }
	local c2 = { input[2], input[6], input[10], input[14] }
	local c3 = { input[3], input[7], input[11], input[15] }
	local c4 = { input[4], input[8], input[12], input[16] }
	if invert then
		c1 = invMixColumn(c1)
		c2 = invMixColumn(c2)
		c3 = invMixColumn(c3)
		c4 = invMixColumn(c4)
	else
		c1 = mixColumn(c1)
		c2 = mixColumn(c2)
		c3 = mixColumn(c3)
		c4 = mixColumn(c4)
	end
	--[[
	output[1] = c1[1]
	output[2] = c1[2]
	output[3] = c1[3]
	output[4] = c1[4]

	output[5] = c2[1]
	output[6] = c2[2]
	output[7] = c2[3]
	output[8] = c2[4]

	output[9] = c3[1]
	output[10] = c3[2]
	output[11] = c3[3]
	output[12] = c3[4]

	output[13] = c4[1]
	output[14] = c4[2]
	output[15] = c4[3]
	output[16] = c4[4]
	]]

	output[1] = c1[1]
	output[5] = c1[2]
	output[9] = c1[3]
	output[13] = c1[4]

	output[2] = c2[1]
	output[6] = c2[2]
	output[10] = c2[3]
	output[14] = c2[4]

	output[3] = c3[1]
	output[7] = c3[2]
	output[11] = c3[3]
	output[15] = c3[4]

	output[4] = c4[1]
	output[8] = c4[2]
	output[12] = c4[3]
	output[16] = c4[4]

	return output
end

local function addRoundKey(input, exp_key, round)
	local output = {}
	for i=1, 16 do
		assert(input[i], "input["..i.."]=nil!")
		assert(exp_key[ ((round-1)*16)+i ], "round_key["..(((round-1)*16)+i).."]=nil!")
		output[i] = bxor( input[i], exp_key[ ((round-1)*16)+i ] )
	end
	return output
end

local function key_schedule(enc_key)
	local function core(in1, in2, in3, in4, i)
		local s1 = in2
		local s2 = in3
		local s3 = in4
		local s4 = in1
		s1 = bxor(sbox[s1], Rcon[i])
		s2 = sbox[s2]
		s3 = sbox[s3]
		s4 = sbox[s4]
		return s1, s2, s3, s4
	end

	local n, b, key_type = 0, 0, 0

	-- Len | n | b |
	-- 128 |16 |176|
	-- 192 |24 |208|
	-- 256 |32 |240|

	-- Determine keysize:

	if #enc_key < 16 then
		error("Encryption key is too small; key size must be more than 16 bytes.")
	elseif #enc_key >= 16 and #enc_key < 24 then
		n = 16
		b = 176
		--key_type = 1
	elseif #enc_key >= 24 and #enc_key < 32 then
		n = 24
		b = 208
		--key_type = 2
	else
		n = 32
		b = 240
		--key_type = 3
	end

	local exp_key = {}
	local rcon_iter = 1
	for i=1, n do
		exp_key[i] = enc_key[i]
	end
	while #exp_key < b do
		local t1 = exp_key[#exp_key]
		local t2 = exp_key[#exp_key-1]
		local t3 = exp_key[#exp_key-2]
		local t4 = exp_key[#exp_key-3]
		t1, t2, t3, t4 = core(t1, t2, t3, t4, rcon_iter)
		rcon_iter = rcon_iter+1
		t1 = bxor(t1, exp_key[#exp_key-(n-1)])
		t2 = bxor(t2, exp_key[#exp_key-(n-2)])
		t3 = bxor(t3, exp_key[#exp_key-(n-3)])
		t4 = bxor(t4, exp_key[#exp_key-(n-4)])
		insert(exp_key, t1)
		insert(exp_key, t2)
		insert(exp_key, t3)
		insert(exp_key, t4)
		for i=1, 3 do
			t1 = bxor(exp_key[#exp_key], exp_key[#exp_key-(n-1)])
			t2 = bxor(exp_key[#exp_key-1], exp_key[#exp_key-(n-2)])
			t3 = bxor(exp_key[#exp_key-2], exp_key[#exp_key-(n-3)])
			t4 = bxor(exp_key[#exp_key-3], exp_key[#exp_key-(n-4)])
			insert(exp_key, t1)
			insert(exp_key, t2)
			insert(exp_key, t3)
			insert(exp_key, t4)
		end
		if key_type == 3 then -- If we're processing a 256 bit key...
			-- Take the previous 4 bytes of the expanded key, run them through the sbox,
			-- then XOR them with the previous n bytes of the expanded key, then output them
			-- as the next 4 bytes of expanded key.
			t1 = bxor(sbox[exp_key[#exp_key]], exp_key[#exp_key-(n-1)])
			t2 = bxor(sbox[exp_key[#exp_key-1]], exp_key[#exp_key-(n-2)])
			t3 = bxor(sbox[exp_key[#exp_key-2]], exp_key[#exp_key-(n-3)])
			t4 = bxor(sbox[exp_key[#exp_key-3]], exp_key[#exp_key-(n-4)])
			insert(exp_key, t1)
			insert(exp_key, t2)
			insert(exp_key, t3)
			insert(exp_key, t4)
		end
		if key_type == 2 or key_type == 3 then -- If we're processing a 192-bit or 256-bit key..
			local i = 2
			if key_type == 3 then
				i = 3
			end
			for j=1, i do
				t1 = bxor(exp_key[#exp_key], exp_key[#exp_key-(n-1)])
				t2 = bxor(exp_key[#exp_key-1], exp_key[#exp_key-(n-2)])
				t3 = bxor(exp_key[#exp_key-2], exp_key[#exp_key-(n-3)])
				t4 = bxor(exp_key[#exp_key-3], exp_key[#exp_key-(n-4)])
				insert(exp_key, t1)
				insert(exp_key, t2)
				insert(exp_key, t3)
				insert(exp_key, t4)
			end
		end
	end
	return exp_key
end

-- Transform a string of bytes into 16 byte blocks, adding padding to ensure that each block contains 16 bytes.
-- For example:
-- "\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF\xFF" (contains 28 0xFF bytes)
-- Is transformed into this:
-- {0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}, {0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0,0,0,0} (16 0xFF bytes, followed by 12 0xFF bytes and 4 0x00 bytes for padding)

local function breakIntoBlocks(data)
	if type(data) ~= "string" then
		error("breakIntoBlocks: data is not a string", 2)
	end
	while (#data % 16) ~= 0 do
		data = data.."\0"
	end
	local blocks = {}
	local blockNum = 1
	local output = {}
	for i=1, #data, 16 do
		blocks[blockNum] = {}
		for j=1, 16 do
			blocks[blockNum][j] = string.byte(data, ((blockNum-1)*16)+j, ((blockNum-1)*16)+j)
		end
		blockNum = blockNum+1
	end
	return blocks
end

-- Transform a string into a series of blocks.

-- For example, to get a key from a string:
-- local key = strToBlocks(keyStr)
-- key = key[1]

local function strToBlocks(str)
	local rawBytestream = {}
	local blocks = {}
	for i=1, #str do
		rawBytestream[i] = string.byte(str, i, i)
	end
	for i=1, math.ceil(#rawBytestream / 16) do
		blocks[i] = {}
		for j=1, 16 do
			blocks[i][j] = rawBytestream[ ((i-1)*16)+j ] or 0
		end
	end
	return blocks
end

-- Encrypt / Decrypt individual blocks:

local function encrypt_block(data, key)
	local exp_key = key_schedule(key)
	local state = data
	local nr = 0

	if #exp_key == 176 then -- Key type 1 (128-bits)
		nr = 10
	elseif #exp_key == 208 then -- Key type 2 (192-bits)
		nr = 12
	elseif #exp_key == 240 then -- Key type 3 (256-bits)
		nr = 14
	else
		error("encrypt_block: Unknown key size?", 2)
	end

	-- Inital round:
	state = addRoundKey(state, exp_key, 1)

	-- Repeat (Nr-1) times:
	for round_num = 2, nr-1 do
		state = subBytes(state)
		state = shiftRows(state)
		state = mixColumns(state)
		state = addRoundKey(state, exp_key, round_num)
	end

	-- Final round (No mixColumns()):
	state = subBytes(state)
	state = shiftRows(state)
	state = addRoundKey(state, exp_key, nr)
	return state
end

local function decrypt_block(data, key)
	local exp_key = key_schedule(key)
	local state = data
	local nr = 0

	if #exp_key == 176 then -- Key type 1 (128-bits)
		nr = 10
	elseif #exp_key == 208 then -- Key type 2 (192-bits)
		nr = 12
	elseif #exp_key == 240 then -- Key type 3 (256-bits)
		nr = 14
	else
		error("decrypt_block: Unknown key size?", 2)
	end

	-- Inital round:
	state = addRoundKey(state, exp_key, nr)

	-- Repeat (Nr-1) times:
	for round_num = nr-1, 2, -1 do
		state = invShiftRows(state)
		state = subBytes(state, true)
		state = addRoundKey(state, exp_key, round_num)
		state = mixColumns(state, true)
	end

	-- Final round (No mixColumns()):
	state = invShiftRows(state)
	state = subBytes(state, true)
	state = addRoundKey(state, exp_key, 1)
	return state
end

local function encrypt_block_customExpKey(data, exp_key--[[, key_type]]) -- Encrypt blocks, but using a precalculated expanded key instead of performing the key expansion on every step like with the normal encrypt_block(2) call
	local state = data
	local nr = 0
	if #exp_key == 176 then -- Key type 1 (128-bits)
		nr = 10
	elseif #exp_key == 208 then -- Key type 2 (192-bits)
		nr = 12
	elseif #exp_key == 240 then -- Key type 3 (256-bits)
		nr = 14
	else
		error("encrypt_block: Unknown key size?", 2)
	end

	-- Inital round:
	state = addRoundKey(state, exp_key, 1)

	-- Repeat (Nr-1) times:
	for round_num = 2, nr-1 do
		state = subBytes(state)
		state = shiftRows(state)
		state = mixColumns(state)
		state = addRoundKey(state, exp_key, round_num)
	end

	-- Final round (No mixColumns()):
	state = subBytes(state)
	state = shiftRows(state)
	state = addRoundKey(state, exp_key, nr)
	return state
end

local function decrypt_block_customExpKey(data, exp_key--[[, key_type]])
	local state = data
	local nr = 0
	if #exp_key == 176 then -- Key type 1 (128-bits)
		nr = 10
	elseif #exp_key == 208 then -- Key type 2 (192-bits)
		nr = 12
	elseif #exp_key == 240 then -- Key type 3 (256-bits)
		nr = 14
	else
		error("decrypt_block: Unknown key size?", 2)
	end

	-- Inital round:
	state = addRoundKey(state, exp_key, nr)

	-- Repeat (Nr-1) times:
	for round_num = nr-1, 2, -1 do
		state = invShiftRows(state)
		state = subBytes(state, true)
		state = addRoundKey(state, exp_key, round_num)
		state = mixColumns(state, true)
	end

	-- Final round (No mixColumns()):
	state = invShiftRows(state)
	state = subBytes(state, true)
	state = addRoundKey(state, exp_key, 1)
	return state
end

-- Encrypt / Decrypt bytestreams (tables of bytes):



-- CBC (cipher-block chaining) mode:

local function encrypt_bytestream(data, key, init_vector)
	local blocks = { init_vector }
	local outputBytestream = {}
	local exp_key = key_schedule(key)
	if not init_vector then
		error("encrypt_bytestream: No initalization vector was passed.", 2)
	end
	for i=1, #data do
		if data[i] == nil or data[i] >= 256 then
			if type(data[i]) == "number" then
				error("encrypt_bytestream: Invalid data at i="..i.." data[i]="..data[i], 2)
			else
				error("encrypt_bytestream: Invalid data at i="..i.." data[i]="..type(data[i]), 2)
			end
		end
	end
	local s = os.clock()
	for i=1, math.ceil(#data/16) do
		local block = {}
		if not blocks[i] then
			error("encrypt_bytestream: blocks["..i.."] is nil! Input size: "..#data, 2)
		end
		for j=1, 16 do
			block[j] = data[((i-1)*16)+j] or 0
			block[j] = bxor(block[j], blocks[i][j]) -- XOR this block with the previous one
		end
		--print("#bytes: "..#block)
		block = encrypt_block_customExpKey(block, exp_key)
		table.insert(blocks, block)
		for j=1, 16 do
			insert(outputBytestream, block[j])
		end
        if os.clock() - s >= 2.5 then
            os.queueEvent("")
            os.pullEvent("")
            s = os.clock()
        end
	end
	return outputBytestream
end

local function decrypt_bytestream(data, key, init_vector)
	local blocks = { init_vector }
	local outputBytestream = {}
	local exp_key = key_schedule(key)
	if not init_vector then
		error("decrypt_bytestream: No initalization vector was passed.", 2)
	end
	local s = os.clock()
	for i=1, math.ceil(#data/16) do
		local block = {}
		if not blocks[i] then
			error("decrypt_bytestream: blocks["..i.."] is nil! Input size: "..#data, 2)
		end
		for j=1, 16 do
			block[j] = data[((i-1)*16)+j] or 0
		end
		table.insert(blocks, block)
		local dec_block = decrypt_block_customExpKey(block, exp_key)
		for j=1, 16 do
			dec_block[j] = bxor(dec_block[j], blocks[i][j]) -- We use XOR on the plaintext, not the ciphertext
			table.insert(outputBytestream, dec_block[j])
		end
        if os.clock() - s >= 2.5 then
            os.queueEvent("")
            os.pullEvent("")
            s = os.clock()
        end
	end
	-- Remove padding:
	for i=#outputBytestream, #outputBytestream-15, -1 do
		if outputBytestream[i] ~= 0 then
			break
		else
			outputBytestream[i] = nil
		end
	end
	return outputBytestream
end

-- Encrypt / Decrypt strings:

function ll.cryptolib.aesEncrypt(data, key, iv)
	local byteStream = {}
	for i=1, #data do
		table.insert(byteStream, string.byte(data, i, i))
	end
	local output_bytestream = {}
	if iv then
		output_bytestream = encrypt_bytestream(byteStream, key, iv)
	else
		output_bytestream = encrypt_bytestream_ecb(byteStream, key)
	end
	local output = ""
	for i=1, #output_bytestream do
		output = output..string.char(output_bytestream[i])
	end
	return output
end

function ll.cryptolib.aesDecrypt(data, key, iv)
	local byteStream = {}
	for i=1, #data do
		table.insert(byteStream, string.byte(data, i, i))
	end
	local output_bytestream = {}
	if iv then
		output_bytestream = decrypt_bytestream(byteStream, key, iv)
	else
		output_bytestream = decrypt_bytestream_ecb(byteStream, key)
	end
	local output = ""
	for i=1, #output_bytestream do
		output = output..string.char(output_bytestream[i])
	end
	return output
end

local function davies_meyer(data, h0)
	local last_h = h0
    for dm_iter=1, 16 do
        for i=1, math.ceil(#data/16) do
            local block = {}
            for j=1, 16 do
                block[j] = data[((i-1)*16)+j] or 0
            end
            local block = encrypt_block(last_h, block)
            for j=1, 16 do
                block[j] = bxor(block[j], last_h[j]) -- XOR h[i-1] with h[i].
            end
            last_h = block
            os.queueEvent("")
            os.pullEvent("")
        end
    end
	return last_h
end

local function increment_ctr(blk)
	local cpy = {}
	for i=1, 16 do
		cpy[i] = blk[i] or 0
	end
	cpy[1] = cpy[1] + incAmt
	for i=2, 16 do
		if cpy[i-1] <= 255 then
			break
		end
		local carry = cpy[i-1] - 255
		cpy[i] = cpy[i]+carry
	end
	return cpy
end

local function counter_mode_context = {
	key = {},
	ctr = {},
	stream_cache = {}, -- Use "leftover" bytes from generate() here.
	set_key = function(self, key)
		if type(key) == "string" then
			if #key < 16 then
				error("set_key: Key length ("..#key..") must be at least 16 characters!", 2)
			end
			for i=1, 16 do
				self.key[i] = string.byte(key, i, i)
			end
		elseif type(key) == "table" then
			if #key < 16 then
				error("set_key: Key length ("..#key..") must be at least 16 bytes!", 2)
			end
			for i=1, 16 do
				if type(key[i]) ~= "number" or key[i] > 255 or key[i] < 0 then
					if type(key[i]) == "nil" then
						error("set_key: Value key["..i.."] is invalid: nil", 2)
					else
						error("set_key: Value key["..i.."] is invalid: "..key[i], 2)
					end
				end
				self.key[i] = key[i]
			end
		else
			error("set_key: Key type is not supported: "..type(key), 2)
		end
	end,
	set_ctr = function(self, ctr)
		if type(ctr) == "string" then
			if #ctr < 16 then
				error("set_ctr: Counter length ("..#ctr..") must be at least 16 characters!", 2)
			end
			for i=1, 16 do
				self.ctr[i] = string.byte(ctr, i, i)
			end
		elseif type(ctr) == "table" then
			if #ctr < 16 then
				error("set_ctr: Counter length ("..#ctr..") must be at least 16 bytes!", 2)
			end
			for i=1, 16 do
				if type(ctr[i]) ~= "number" or ctr[i] > 255 or ctr[i] < 0 then
					if type(ctr[i]) == "nil" then
						error("set_ctr: Value ctr["..i.."] is invalid: nil", 2)
					else
						error("set_ctr: Value ctr["..i.."] is invalid: "..ctr[i], 2)
					end
				end
				self.ctr[i] = ctr[i]
			end
		elseif type(ctr) == "number" then
			local b1 = bit.band( ctr, 0xFF )
			local b2 = bit.band( bit.brshift(bit.band( ctr, 0xFF00 ), 8), 0xFF )
			local b3 = bit.band( bit.brshift(bit.band( ctr, 0xFF0000 ), 16), 0xFF )
			local b4 = bit.band( bit.brshift(bit.band( ctr, 0xFF000000 ), 24), 0xFF )
			self.ctr = {}
			for i=1, 16 do
				self.ctr[i] = 0
			end
			self.ctr[1] = b1
			self.ctr[2] = b2
			self.ctr[3] = b3
			self.ctr[4] = b4
		else
			error("set_ctr: Counter type is not supported: "..type(ctr), 2)
		end
	end,
	generate = function(self, bytes)
		local genBytes = {}
		if #self.stream_cache >= bytes then
			for i=1, bytes do
				table.insert(genBytes, table.remove(self.stream_cache))
			end
		else
			for i=1, #self.stream_cache do
				table.insert(genBytes, table.remove(self.stream_cache))
			end
			local blocksToGenerate = math.ceil((bytes - #genBytes) / 16)
			for i=1, blocksToGenerate-1 do
				self.ctr = increment_ctr(self.ctr)
				local block = encrypt_block(self.ctr, self.key)
				for i=1, 16 do
					table.insert(genBytes, block[i])
				end
			end
			self.ctr = increment_ctr(self.ctr)
			local block = encrypt_block(self.ctr, self.key)
			for i=1, (bytes - #genBytes) do
				table.insert(genBytes, table.remove(block))
			end
			for i=1, #block do
				table.insert(self.stream_cache, table.remove(block))
			end
		end
		return genBytes
	end,
}

local function new_ctrMode(key, iv)
	local context = {
		stream_cache = {},
		key = {},
		iv = {},
		__index = counter_mode_context,
	}
	setmetatable(context, context)
	context:set_key(key)
	context:set_ctr(iv)
	return context
end
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
