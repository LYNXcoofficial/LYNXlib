--LYNXlib v5 by RockRancher24 & Overo3
local version = 5
settings.define("lynx.lynxlibPath", {
    value = shell.getRunningProgram()
})
local function versionCheck(minVer, maxVer)
    if minVer < 4 then minVer = 4 end
    if maxVer ~= nil then
		if maxVer < 4 then error("No. We do not speak of the pre-v4 days.") end
        if version > maxVer or version < minVer then error("ERROR: Program requires between version v"..minVer.." to v"..maxVer..". You have v"..version.." installed.") end
	elseif version < minVer then error("ERROR: Program requires version v"..minVer.." or greater. You have v"..version.." installed.") end
    return version
end
local lynxColor = colors.blue
local meta = { version = version, versionCheck = versionCheck, lynxColor = lynxColor }

--TABLE OF CONTENTS
--Ln. 27: Documentation
--Ln. 104: Autorun
--Ln. 116: LYNX Miscellaneous Library (LYNXmisc)
--Ln. 322: ZipLib
--Ln. 409: LYNX CryptoLib
--Ln. 440: CHTP
--Ln. 496: Closing


--DOCUMENTATION
--LYNXMISC:
--	twoColorPalette(mode: string, enableWhite: bool): Alters the terminal's color palette to a more restricted one for a "retro" feel. Added in v1.
--		mode: The palette to use.
--			âREDâ, âGREENâ, âBLUEâ, âGRAYSCALEâ, âYELLOWâ, âCYANâ, âMAGENTAâ, 
--			âORANGEâ, âLIMEâ, âMINTâ, âCORNFLOWERâ, âPURPLEâ, âFUCHSIAâ,
--			âRED-CYANâ, âGREEN-MAGENTAâ, âBLUE-YELLOWâ
--		enableWhite: Controls whether or not white can also be used, along with mode

--	inRange(a: number, x: number, y: number): Returns true if a is between x and y (inclusive). Added in v2.

--	textBox(text: string, x1: number, y1: number, x2: number, y2: number, xmode: string, ymode: string, bC, tC]): Creates a textbox. Added in v2.
--      text: The text for the textbox.
--      x1, y1: The top-left corner of the textbox.
--      x2, y2: The bottom-right corner of the textbox.
--      xmode: No desc. Currently only accepts "LEFT"
--      ymode: No desc. Currently only accepts "TOP"

--  readFile(dir: string): Returns the contents of a file, along with its LOSC-UHS header (if one exists). Added in v5.
--      dir: The path to the file (absolute or relative)

--  writeFile(dir: string, contents: string, header: table): A file-writing function that adds the header automatically. Added in v5.
--      dir: The path to the file (absolute or relative)
--      contents: File contents
--      header: The LOSC-UHS header of the file

--  hsv2rgb(h: number, s: number, v: number): Converts an HSV color to an RGB color. Added in v5.
--      h: The hue, in degrees
--      s: The saturation, scaled from 0 (fully pale) to 255 (fully saturated)
--      v: The value or brightness of the color, scaled from 0 (black) to 255 (fully bright)

--  recursiveList(path: string): Returns the path to every file inside a directory. Added in v5.
--      path: The path to the folder (absolute or relative)

--  round(num, digits, mode): Rounds a number to a certain number of post-decimal digits. Added in v5.
--      mode: The method of rounding to use. Defaults to nearest.
--          nearest, ceil, floor

--LYNX CRYPTOLIB:
--	encrypt(inp: string, key: string, cipher: string): Encrypts a string using a key. Added in v1.
--		inp: The string to encrypt
--		key: The key to encrypt with
--		cipher: The cipher to use.
--			"best" or "latest": Current best cipher implemented.
--			"otp": One-time pad cipher.

--	decrypt(inp: string, key: string, cipher: string): Decrypts a string using a key. Added in v1.
--		inp: The string to decrypt
--		key: The key to decrypt with
--		cipher: The cipher to use.
--			"best" or "latest": Current best cipher implemented.
--			"otp": One-time pad cipher.

--	generatePublicKey(privateKey: number): Generates a 256-bit public key for a private key. Added in v1.
--		privateKey: The private key to generate a public key for. Leave nil for your own private key.

--	combineKeys(publicKey: number, privateKey: number): Combines your private key with a target's public key to make a 256-bit session key. Added in v1.
--		publicKey: The public key to combine with the private key
--		privateKey: The private key to combine with the public key. Leave nil for your own private key.

--	string currentBestAlgo: The current best implemented cipher. Added in v1.


--CHTP:
--	send(body: table, target: number, header: string, cipher: string, key: number, lNetwork: bool, channel: number): Sends a table packet to an IP. Added in v1.
--		body: The table to send.
--		target: The IP to send the packet to.
--		header: The header of the packet.

--	awaitPacket(header: string, source: number, key: number, lNetwork: bool, channel: number): No desc. Added in v1.
--		header: See send().
--		source: The IP to listen for packets from. Leave nil to listen to all IPs.
--		key: The key to decrypt the packet with.
--		lNetwork: Whether or not to listen on the wired network.
--		channel: What channel to listen on.


--AUTORUN
if not settings.get("startup_util.dir_path") then settings.set("startup_util.dir_path", "/startup/")
else
    if not fs.exists(fs.combine(settings.get("startup_util.dir_path"), "llstartup.lua")) then
        local arf = fs.open(settings.get("startup_util.dir_path").."/llstartup.lua", "w")
        local autorun = "math.randomseed(os.epoch())\nif settings.get(\"chtp.thisIP\") == nil then settings.define(\"chtp.thisIP\", {\n    description = \"This computer's IP address.\",\n    default = math.random(1, 2^32),\n})\nsettings.set(\"chtp.thisIP\", math.random(1, 2^32))\nsettings.save()\nend"
        arf.write(autorun)
        arf.close()
    end
end


--LYNXMISC
local function twoColorPalette(mode, enableWhite)
    for j = 1, 15 do
        i = 2 ^ j
        local r,g,b = term.getPaletteColor(i)
        local x = math.max(r,g,b)
        local y = math.max(r,g)
        local c = math.max(g,b)
        local m = math.max(b,r)
        local rgb = {r,g,b}
        --Primary colors
        if mode == "RED" then rgb = {x,0,0}
        elseif mode == "GREEN" then rgb = {0,x,0}
        elseif mode == "BLUE" then rgb = {0,0,x}
        elseif mode == "GRAYSCALE" then rgb = {x,x,x}
        elseif mode == "GREYSCALE" then rgb = {x,x,x}
        --Secondary colors
        elseif mode == "YELLOW" then rgb = {x,x,0}
        elseif mode == "CYAN" then rgb = {0,x,x}
        elseif mode == "MAGENTA" then rgb = {x,0,x}
        --Tertiary colors
        elseif mode == "ORANGE" then rgb = {x,x/2,0}
        elseif mode == "LIME" then rgb = {x/2,x,0}
        elseif mode == "MINT" then rgb = {0,x,x/2}
        elseif mode == "CORNFLOWER" then rgb = {0,x/2,x}
        elseif mode == "PURPLE" then rgb = {x/2,0,x}
        elseif mode == "FUCHSIA" then rgb = {x,0,x/2} end
        if r == g and g == b and enableWhite then rgb = {x,x,x} end
        --Primary-secondary color combos
        if mode == "RED-CYAN" then term.setPaletteColor(i, r,c,c)
        elseif mode == "GREEN-MAGENTA" then term.setPaletteColor(i, m,g,m)
        elseif mode == "BLUE-YELLOW" then term.setPaletteColor(i, y,y,b)
        else term.setPaletteColor(i, rgb[1], rgb[2], rgb[3]) end
    end
end

local function inRange(a, x,y)
    if a >= x and a <= y then return true
    else return false end
end
 
local function textBox(text, x1,y1, x2,y2, xmode,ymode, bC,tC)
    local startTC = term.getTextColor()
    local startBGC = term.getBackgroundColor()
    if type(text) == "table" then
        local sText = text
        text = x1
        x1 = sText
    end

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

local function readFile(dir)
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

local function writeFile(dir, contents, header)
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

local function getHeader(dir)
    local _, h = readFile(dir)
    return h
end

local function hsv2rgb(hue,saturation,value)
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

local function contextMenu(x,y, options, title, bgColor, titleColor)
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

local function recursiveList(dir)
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

local function round(num, digits, mode)
    if not digits then digits = 0 end
    local multNum = num * 10^digits
    if type(mode) == "string" and mode:lower() == "floor" then return math.floor(multNum) / 10^digits
    elseif type(mode) == "string" and mode:lower() == "ceil" then return math.ceil(multNum) / 10^digits
    else return math.floor(multNum+0.5) / 10^digits end
end

local function scale(num, units, digits, roundMode, prefix)
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

local lynxmisc = { twoColorPalette = twoColorPalette, inRange = inRange, textBox = textBox, readFile = readFile, writeFile = writeFile, getHeader = getHeader, hsv2rgb = hsv2rgb, recursiveList = recursiveList, round = round, scale = scale }

--ZIPLIB
local function lz77_compress(data)
    local compressed = {}
    local window_size = 256
    local lookahead_size = 16
    local data_len = #data

    local i = 1
    while i <= data_len do
        local max_match_len = 0
        local best_offset = 0
        
        for j = math.max(1, i - window_size), i - 1 do
            local match_len = 0
            while match_len < lookahead_size and data:sub(j + match_len, j + match_len) == data:sub(i + match_len, i + match_len) do
                match_len = match_len + 1
            end
            
            if match_len > max_match_len then
                max_match_len = match_len
                best_offset = i - j
            end
        end
        
        if max_match_len > 1 then
            table.insert(compressed, string.format("<%d,%d>", best_offset, max_match_len))
            i = i + max_match_len
        else
            table.insert(compressed, data:sub(i, i))
            i = i + 1
        end
    end

    return table.concat(compressed)
end

local function lz77_decompress(compressed)
    local decompressed = {}
    local i = 1
    local len = #compressed
    
    while i <= len do
        local char = compressed:sub(i, i)
        
        if char == "<" then
            local j = i + 1
            local offset, length = compressed:match("(%d+),(%d+)", j)
            offset = tonumber(offset)
            length = tonumber(length)
            i = j + #tostring(offset) + #tostring(length) + 1
            local start_pos = #decompressed - offset + 1
            for k = start_pos, start_pos + length - 1 do
                table.insert(decompressed, decompressed[k])
            end
        elseif char == ">" then
            i = i + 1
        else
            table.insert(decompressed, char)
            i = i + 1
        end
    end
    
    return table.concat(decompressed)
end

local bestRatio = "lz77"
local best = "lz77"

local function compress(inp, method)
    if not method then method = "best" end
    if method:lower() == "lz77" then return lz77_compress(inp)
    elseif method:lower() == "best" then compress(inp, best)
    elseif method:lower() == "ratio" then compress(inp, bestRatio)
    end
end

local function decompress(inp, method)
    if not method then method = "best" end
    if method:lower() == "lz77" then return lz77_decompress(inp)
    elseif method:lower() == "best" then decompress(inp, best)
    elseif method:lower() == "ratio" then decompress(inp, bestRatio)
    end
end

local ziplib = { compress = compress, decompress = decompress }


--LYNX CRYPTOLIB
local function encryptStringOTP(inp, key)
    local outp = ""
    for i = 1, #inp do outp = outp..string.char(bit32.bxor(inp:byte(i),key:byte(i % #key))) end
    return outp
end
local function decryptStringOTP(inp, key) encryptStringOTP(inp, key) end

local function encryptString(inp, key, cipher)
    if cipher == nil or cipher == "" then return inp
    elseif cipher == "best" or cipher == "latest" then return encryptStringOTP(inp, key)
    elseif cipher == "otp" then return encryptStringOTP(inp, key) end
end
local function decryptString(inp, key, cipher)
    if cipher == nil or cipher == "" then return inp
    elseif cipher:lower() == "best" or cipher:lower() == "latest" then return decryptStringOTP(inp, key)
    elseif cipher:lower() == "otp" then return decryptStringOTP(inp, key)
    else return inp end
end
local function generatePublicKey(prKey) return (prKey ^ (256)) % 2 ^ settings.get("lynxcryptolib.keySize") end
local function combineKeys(pubKey, prKey) if prKey == nil then prKey = settings.get("chtp.privateKey") end
    local outp = ""
    for i = 1, settings.get("lynxcryptolib.keySize") / 8 do
        outp = outp..string.char(prKey:byte((i + 1) % (settings.get("lynxcryptolib.keySize") / 8) - 1) ^ pubKey:byte((i + 1) % (settings.get("lynxcryptolib.keySize") / 8) - 1) % 2 ^ settings.get("lynxcryptolib.keySize"))
    end
    return outp
end
local currentBestAlgo = "otp"
local lynxcryptolib = { encrypt = encryptString, decrypt = decryptString, generatePublicKey = generatePublicKey, combineKeys = combineKeys, currentBestAlgo = currentBestAlgo }


--CHTP
local gModem
local lModem
local psd = { }
local incMessage
local perSideList = {"top","bottom","left","right","front","back"}
for i = 1, 6 do
    local curPer = peripheral.wrap(perSideList[i])
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
local function send(packet, target, header, cipher, key, lNetwork, channel)
    if not gModem then lNetwork = true end
    if lNetwork and not lModem then return nil, "No modem" end
    if not channel then channel = 127 end
    sPacket = lynxcryptolib.encrypt(textutils.serialize(packet), key, cipher)
    if not lNetwork then gModem.transmit(channel, channel, { target, settings.get("chtp.thisIP"), header, sPacket, cipher })
    else lModem.transmit(channel, channel, { target, settings.get("chtp.thisIP"), header, sPacket, cipher }) end
end
local function awaitPacket(header, from, key, lNetwork, channel)
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

--CLOSING
local chtp = { send = send, awaitPacket = awaitPacket }

return { meta = meta, misc = lynxmisc, lm = lynxmisc, ziplib = ziplib, zl = ziplib, cl = lynxcryptolib, cryptolib = lynxcryptolib, lynxmisc = lynxmisc, lynxcryptolib = lynxcryptolib, lynxCryptoLib = lynxcryptolib, chtp = chtp }
