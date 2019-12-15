local module = {}

function module.numToBinary(int, width)
	local width = width or 8
	local binValues = {128,64,32,16,8,4,2,1}
	if width > 8 then
		for i = 8, width - 1 do
			table.insert(binValues, 1, 2^i)
		end
	end
	assert(int < 2^width, string.format("int value out of range for %s bit width", width))
	local value = ""
	for _,v in pairs(binValues) do
		if int >= v then
			value = value.."1"
			int = int - v
		else
			value = value.."0"
		end
	end
	return value
end

function module.binaryToNum(str)
	local len = str:len()
	-- Generate binary values for the width given
	local binValues = {128,64,32,16,8,4,2,1}
	if len > 8 then
		for i = 8, len - 1 do
			table.insert(binValues, 1, 2^i)
		end
	else
		for i = 1,8 - len do
			table.remove(binValues, 1)
		end
	end

	local int = 0
	for i,v in pairs(binValues) do
		if str:sub(i,i) == "1" then
			int = int + v
		end
	end
	return int
end

local base16Values = {"0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"}
function module.binaryToHex(str, prepend0x)
	local prepend0x = prepend0x == nil and true or prepend0x
	local hex = ""
	for i = 1, str:len(), 4 do
		hex = hex .. base16Values[module.binaryToNum(str:sub(i,i+3))+1]
	end

	if prepend0x then
		return "0x"..hex
	else
		return hex
	end
end

local base64Values = {
	"A","B","C","D","E","F","G","H",
	"I","J","K","L","M","N","O","P",
	"Q","R","S","T","U","V","W","X",
	"Y","Z","a","b","c","d","e","f",
	"g","h","i","j","k","l","m","n",
	"o","p","q","r","s","t","u","v",
	"w","x","y","z","0","1","2","3",
	"4","5","6","7","8","9","+","/"
}
function module.base64Encode(str)
	-- Following RFC 2045
	-- Get the char code representation in denary
	local numArray = {}
	for i = 1, str:len() do
		numArray[i] = str:byte(i)
	end
	-- Convert to bytes and concate into one long binary string
	-- for the next stage of processing
	local blob = ""
	for i,v in pairs(numArray) do
		blob = blob .. module.numToBinary(v)
	end
	-- Take 24 bits at a time from the blob string
	-- These blocks will be treated as 4 groups of 6 bits each
	-- (24/6 = 4)
	local bitArray = {}
	for i = 1, blob:len(), 24 do
		bitArray[#bitArray+1] = blob:sub(i,i+23)
	end
	-- This next section breaks the 24 bits into 6 bits and 
	-- indexes the base64 chart to produce the output
	local base64 = ""
	for _,v in pairs(bitArray) do
		for i = 1,24,6 do
			local chunk = v:sub(i, i+5)
			-- Empty chunks will be marked nil so they can be padded
			if chunk == "" then 
				chunk = nil 
			end
			-- When chunk length is not 6 bits long it is extended with zero bits
			if chunk and chunk:len() < 6 and chunk:len() > 0 then
				chunk = chunk .. string.rep("0",6 - chunk:len())
			end
			base64 = base64..(
				chunk and base64Values[module.binaryToNum(chunk)+1]
				or
				"="
			)
		end
	end
  -- Add linebreaks every 76 characters in line with RFC 2045
  for i = 76, base64:len(), 76 do
    base64 = base64:sub(1, i) .. "\n" .. base64:sub(i+1,base64:len())
  end

	return base64
end

-- In order to index Base64 values the table is inverted (e.g from 1 = A to A = 1)
local base64IndexValues = {}
for i,v in pairs(base64Values) do
	base64IndexValues[v] = i
end
function module.base64Decode(base64)
	assert(type(base64) == "string", "Base64 must be quotable for decoding")
	-- Convert quotable base64 back to binary string
	local blob = ""
	for i = 1, base64:len() do
    	local char = base64:sub(i,i)
    	if base64IndexValues[char] then
      		-- Num to binary returns 8 bits, need 6.
      		blob = blob .. module.numToBinary(base64IndexValues[char]-1):sub(3,8)
		end
  	end
  	-- Convert bytes to integers
  	local numArray = {}
  	for i = 1, blob:len(), 8 do
    	numArray[#numArray+1] = module.binaryToNum(blob:sub(i, i+7))
  	end
  	-- Convert integers to characters
  	local str = ""
  	for i,v in pairs(numArray) do
    	str = str .. string.char(v)
	end
	  
  	return str
end

-- Define internal sha2 functions
function ch(x, y, z)
	return (x & y) ~ (~x & z)
end
function maj(x, y, z)
	return (x & y) ~ (x & z) ~ (y & z)
end
function bsig0(x)
	return bit32.rrotate(x, 2) ~ bit32.rrotate(x, 13) ~ bit32.rrotate(x, 22)
end
function bsig1(x)
	return bit32.rrotate(x, 6) ~ bit32.rrotate(x, 11) ~ bit32.rrotate(x, 25)
end
function ssig0(x)
	return bit32.rrotate(x, 7) ~ bit32.rrotate(x, 18) ~ bit32.rshift(x, 3)
end
function ssig1(x)
	return bit32.rrotate(x, 17) ~ bit32.rrotate(x, 19) ~ bit32.rshift(x, 10)
end
-- Define constants K[1..64]
local K = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
	0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
	0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
	0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
	0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
	0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
	0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
	0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
	0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
}
function module.sha256(str)
	-- Generate bit length and string
	local L = 0
	local bitString = ""
	for i = 1,str:len() do
		L = L + 8
		bitString = bitString .. module.numToBinary(str:byte(i))
	end
	-- If str == "" then hash for 0 bit length (this satisfies a test vector)
	bitString = bitString == "" and "1"..string.rep("0",511) or bitString

	-- Pad bitString so the length is a multiple of 512
	if bitString:len() % 512 ~= 0 then
		bitString = bitString .. "1" 
		.. string.rep("0", (448 - ((L + 1) % 512)) % 512)
		.. module.numToBinary(L, 64)
	end

	-- Split bitString into blocks of 512 bits
	local blocks = {}
	for i = 1, bitString:len(), 512 do
		blocks[#blocks + 1] = bitString:sub(i, i+511)
	end

	-- Initialise hash values H[1..8]
	local H = {
		0x6a09e667,
		0xbb67ae85,
		0x3c6ef372,
		0xa54ff53a,
		0x510e527f,
		0x9b05688c,
		0x1f83d9ab,
		0x5be0cd19
	}

	-- Main hash loop
	for i = 1, #blocks do
		-- Initialise message schedule W[1..64] for block
		local W = {}
		for t = 0, 15 do
			W[t+1] = module.binaryToNum(blocks[i]:sub((t*32)+1,((t*32)+1)+31))
		end
		for t = 17, 64 do
			W[t] = (ssig1(W[t-2]) + W[t-7] + ssig0(W[t-15]) + W[t-16]) % 2^32
		end

		-- Define working variables
		local a = H[1]
		local b = H[2]
		local d = H[4]
		local c = H[3]
		local e = H[5]
		local f = H[6]
		local g = H[7]
		local h = H[8]

		for t = 1, 64 do
			local T1 = (h + bsig1(e) + ch(e, f, g) + K[t] + W[t]) % 2^32
			local T2 = (bsig0(a) + maj(a, b, c)) % 2^32
			h = g
			g = f
			f = e
			e = (d + T1) % 2^32
			d = c
			c = b
			b = a
			a = (T1 + T2) % 2^32
		end

		-- Add working variables to the hash value table
		H[1] = (a + H[1]) % 2^32
		H[2] = (b + H[2]) % 2^32
		H[3] = (c + H[3]) % 2^32
		H[4] = (d + H[4]) % 2^32
		H[5] = (e + H[5]) % 2^32
		H[6] = (f + H[6]) % 2^32
		H[7] = (g + H[7]) % 2^32
		H[8] = (h + H[8]) % 2^32
	end

	-- Produce result as hex string
	local result = ""
	for v = 1, 8 do
		result = result .. module.binaryToHex(module.numToBinary(H[v], 32), false)
	end

	return result
end

return module
