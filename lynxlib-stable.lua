--|.lua
--|
--LynxLib v4 by RockRancher24
local version = 4
local function versionCheck(minVer, maxVer)
	if maxVer ~= nil then
			if version > maxVer or version < minVer then error("ERROR: Program requires between version v"..minVer.." to v"..maxVer..". You have v"..version.." installed.") end
	elseif version < minVer then error("ERROR: Program requires version v"..minVer.." or greater. You have v"..version.." installed.") end
end
local meta = { version = version, versionCheck = versionCheck }

--TABLE OF CONTENTS
--Ln. 14: Documentation
--Ln. 78: Autorun
--Ln. 107: Lynx Miscellaneous Library (LynxMisc)
--Ln. 172: Lynx CryptoLib
--Ln. 202: CHTP
--Ln. 250: Closing


--DOCUMENTATION
--LYNXMISC:
--	twoColorPalette(String mode, Bool enableWhite): Alters the terminal's color palette to a more restricted one for a "retro" feel. Added in v1.
--		mode: The palette to use.
--			“RED”, “GREEN”, “BLUE”, “GRAYSCALE”, “YELLOW”, “CYAN”, “MAGENTA”, 
--			“ORANGE”, “LIME”, “MINT”, “CORNFLOWER”, “PURPLE”, “FUCHSIA”,
--			“RED-CYAN”, “GREEN-MAGENTA”, “BLUE-YELLOW”
--		enableWhite: Controls whether or not white can also be used, along with mode

--	inRange(a, x,y): Returns true if a is between x and y (inclusive). Added in v2.

--	textBox(text, x1,y1, x2,y2, xmode,ymode, bC,tC): No desc. Added in v2


--LYNX CRYPTOLIB:
--	encrypt(String inp, String key, String cipher): Encrypts a string using a key. Added in v1.
--		inp: The string to encrypt
--		key: The key to encrypt with
--		cipher: The cipher to use.
--			"best" or "latest": Current best cipher implemented.
--			"otp": One-time pad cipher.

--	decrypt(String inp, String key, String cipher): Decrypts a string using a key. Added in v1.
--		inp: The string to decrypt
--		key: The key to decrypt with
--		cipher: The cipher to use.
--			"best" or "latest": Current best cipher implemented.
--			"otp": One-time pad cipher.

--	generatePublicKey(Num privateKey): Generates a 256-bit public key for a private key. Added in v1.
--		privateKey: The private key to generate a public key for. Leave nil for your own private key.

--	combineKeys(Num publicKey, Num privateKey): Combines your private key with a target's public key to make a 256-bit session key. Added in v1.
--		publicKey: The public key to combine with the private key
--		privateKey: The private key to combine with the public key. Leave nil for your own private key.

--	String currentBestAlgo: The current best implemented cipher. Added in v1.


--CHTP:
--	send(Table body, Num target, String header, String cipher, Num key, Bool lNetwork, Num channel): Sends a table packet to an IP. Added in v1.
--		body: The table to send.
--		target: The IP to send the packet to.
--		header: The header of the packet.
--			"chtp.file_request": Requests a file or group of files in the form of strings containing their contents from a machine (if you are in the whitelist for the files, if one exists). Format: {filename, filename... }
--			"chtp.file": A file or group of files in the form of strings containing their contents. Format: {file, file... }
--			"chtp.public_key": Requests that your public key be added to the target's session table under your IP. Format: {publicKey}
--			"chtp.session_request": Exactly the same as public_key but requests the target respond with a public_key of their own so that you can add them to your own session table.
--			[add more here later]

--	awaitPacket(String header, Num source, Num key, Bool lNetwork, Num channel): No desc. Added in v1.
--		header: See send().
--		source: The IP to listen for packets from. Leave nil to listen to all IPs.
--		key: The key to decrypt the packet with.
--		lNetwork: Whether or not to listen on the wired network.
--		channel: What channel to listen on.

-- acquireSession(Num target): No desc. Added in v1.

--AUTORUN
if not fs.exists("/startup/llautorun.lua") then
    local arf = fs.open("/startup/llautorun.lua", "w")
    local autorun = "
    if not settings.get("lynxCryptoLib.keySize") then\n 
    settings.define("lynxCryptoLib.keySize", {\n
        description = "The size of the keys this computer generates.",\n
        default = 256,\n
        type = "number",\n
    })\n
    if settings.get("chtp.privateKey") then settings.undefine("chtp.privateKey") end\n
    settings.define("chtp.privateKey", {\n
        description = "This computer's private key. Resets on startup.",\n
        default = math.random(1, 2 ^ settings.get("lynxCryptoLib.keySize"),\n
        type = "number",\n
    })\n
    end\n
    if settings.get("chtp.dynamicIP") == true then settings.undefine("chtp.thisIP") end\n
    if not settings.get("chtp.thisIP") then\n
        settings.define("chtp.thisIP", {\n
        description = "This computer's IP adress.",\n
        default = math.random(1, 2 ^ 32),\n
    })
    "
    arf.write(autorun)
    arf.close()
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
        --Primary color combos
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
    local xp,yp
    if not x2 then x2 = x1 end
    if not y2 then y2 = y1 end
    if not xmode then xmode = "LEFT" end
    if not ymode then ymode = "TOP" end
    
    if xmode == "LEFT" then xp = x1
    elseif xmode == "RIGHT" then xp = x2 - #text
    elseif xmode == "CENTER" then xp = x1 + math.floor((x2 - x1 + 1) / 2) - math.floor(#text / 2) end
    
    if ymode == "TOP" then yp = y1
    elseif ymode == "BOTTOM" then yp = y2
    elseif ymode == "CENTER" then yp = y1 + math.floor((y2 - y1 + 1) / 2) end
    
    if not tC then tC = term.getTextColor() end
    if bC then paintutils.drawFilledBox(x1,y1, x2,y2, bC) end
    term.setTextColor(tC)
    term.setCursorPos(xp,yp)
    term.write(text)
end
local lynxmisc = { twoColorPalette = twoColorPalette, inRange = inRange, textBox = textBox }


--LYNX CRYPTOLIB
--One-time pad cipher
local function encryptStringOTP(inp, key)
    local outp = ""
    for i = 1, #inp do outp = outp..string.char(inp:byte(i) ~ key:byte(i % #key)) end
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
    elseif cipher == "best" or cipher == "latest" then return decryptStringOTP(inp, key)
    elseif cipher == "otp" then return decryptStringOTP(inp, key) end
end
local function generatePublicKey(prKey) return (prKey ^ (256)) % 2 ^ settings.get("lynxCryptoLib.keySize") end
local function combineKeys(prKey, pubKey) if prKey == nil then prKey = settings.get("chtp.privateKey") end
    local outp = ""
    for i = 1, settings.get("lynxCryptoLib.keySize") / 8 do
                           
    return (prKey ^ pubKey) % 2 ^ settings.get("lynxCryptoLib.keySize")
end
local currentBestAlgo = "otp"
local lynxCryptoLib = { encrypt = encryptString, decrypt = decryptString, generatePublicKey = generatePublicKey, combineKeys = combineKeys, currentBestAlgo = currentBestAlgo }


--CHTP
local gModem
local lModem
local psd
local incPacket
for i = 1, 6 do
    local curPer = peripheral.wrap({"top","bottom","left","right","front","back"}[i])
    if curPer ~= nil then
        if curPer.isWireless == true then gModem = curPer
        elseif curPer.isWireless == false then lModem = curPer end
    end
end
local function timeout() os.sleep(settings.get("chtp.packetTimeout")) end
local function packetRec()
    local incMessage = nil
    while incMessage[3] ~= psd["header"] or incMessage[2] ~= psd["from"] or incMessage[1] ~= settings.get("chtp.thisIP") do
        _, _, _, _, incMessage = os.pullEvent("modem_message") end
    incPacket = incMessage[4]
end
local function send(packet, target, header, cipher, key, lNetwork, channel)
    if channel == nil then channel = 127 end
    sPacket = lynxCryptoLib.encrypt(textutils.serialize(packet), cipher, key)
    if lNetwork false then gModem.transmit(channel, channel, {target, settings.get("chtp.thisIP"), header, sPacket, cipher})
    else lModem.transmit(channel, channel, {target, settings.get("chtp.thisIP"), header, sPacket, cipher}) end
end
local function awaitPacket(header, from, key, lNetwork, channel)
    if lNetwork == nil then lNetwork = "global" end
    psd["header"] = header
    psd["from"] = from
    incPacket = nil
    gModem.closeAll()
    lModem.closeAll()
    if lNetwork == false then gModem.open(channel)
    else lModem.open(channel) end
    parallel.waitForAny(timeout, packetRec)
    if incPacket then if incMessage[4] ~= nil then decrypt(incPacket, key, incMessage[4]) end end
    --Packet, header, return address, encryption method
    incPacket = textutils.unserialize(incPacket)
    return incPacket, incMessage[3], incMessage[2], incMessage[4]
end
local function acquireSession(target)
    send({settings.get("chtp.publicKey")}, target, "session_request")
    local tPub = awaitPacket("public_key", target)
    return lynxCryptoLib.combineKeys(tPub)
end

local chtp = { send = send, awaitPacket = awaitPacket, acquireSession = acquireSession }

return { meta = meta, lynxmisc = lynxmisc, lynxcryptolib = lynxcryptolib, chtp = chtp }
