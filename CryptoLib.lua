local module = {}

-- every value up to 64 bits worth
local binValues = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648, 4294967296, 8589934592, 17179869184, 34359738368, 68719476736, 137438953472, 274877906944, 549755813888, 1099511627776, 2199023255552, 4398046511104, 8796093022208, 17592186044416, 35184372088832, 70368744177664, 1.4073748835533e+14, 2.8147497671066e+14, 5.6294995342131e+14, 1.1258999068426e+15, 2.2517998136852e+15, 4.5035996273705e+15, 9.007199254741e+15, 1.8014398509482e+16, 3.6028797018964e+16, 7.2057594037928e+16, 1.4411518807586e+17, 2.8823037615171e+17, 5.7646075230342e+17, 1.1529215046068e+18, 2.3058430092137e+18, 4.6116860184274e+18, 9.2233720368548e+18}
local commonBinary = {"00000000", "00000001", "00000010", "00000011", "00000100", "00000101", "00000110", "00000111", "00001000", "00001001", "00001010", "00001011", "00001100", "00001101", "00001110", "00001111", "00010000", "00010001", "00010010", "00010011", "00010100", "00010101", "00010110", "00010111", "00011000", "00011001", "00011010", "00011011", "00011100", "00011101", "00011110", "00011111", "00100000", "00100001", "00100010", "00100011", "00100100", "00100101", "00100110", "00100111", "00101000", "00101001", "00101010", "00101011", "00101100", "00101101", "00101110", "00101111", "00110000", "00110001", "00110010", "00110011", "00110100", "00110101", "00110110", "00110111", "00111000", "00111001", "00111010", "00111011", "00111100", "00111101", "00111110", "00111111", "01000000", "01000001", "01000010", "01000011", "01000100", "01000101", "01000110", "01000111", "01001000", "01001001", "01001010", "01001011", "01001100", "01001101", "01001110", "01001111", "01010000", "01010001", "01010010", "01010011", "01010100", "01010101", "01010110", "01010111", "01011000", "01011001", "01011010", "01011011", "01011100", "01011101", "01011110", "01011111", "01100000", "01100001", "01100010", "01100011", "01100100", "01100101", "01100110", "01100111", "01101000", "01101001", "01101010", "01101011", "01101100", "01101101", "01101110", "01101111", "01110000", "01110001", "01110010", "01110011", "01110100", "01110101", "01110110", "01110111", "01111000", "01111001", "01111010", "01111011", "01111100", "01111101", "01111110", "01111111", "10000000", "10000001", "10000010", "10000011", "10000100", "10000101", "10000110", "10000111", "10001000", "10001001", "10001010", "10001011", "10001100", "10001101", "10001110", "10001111", "10010000", "10010001", "10010010", "10010011", "10010100", "10010101", "10010110", "10010111", "10011000", "10011001", "10011010", "10011011", "10011100", "10011101", "10011110", "10011111", "10100000", "10100001", "10100010", "10100011", "10100100", "10100101", "10100110", "10100111", "10101000", "10101001", "10101010", "10101011", "10101100", "10101101", "10101110", "10101111", "10110000", "10110001", "10110010", "10110011", "10110100", "10110101", "10110110", "10110111", "10111000", "10111001", "10111010", "10111011", "10111100", "10111101", "10111110", "10111111", "11000000", "11000001", "11000010", "11000011", "11000100", "11000101", "11000110", "11000111", "11001000", "11001001", "11001010", "11001011", "11001100", "11001101", "11001110", "11001111", "11010000", "11010001", "11010010", "11010011", "11010100", "11010101", "11010110", "11010111", "11011000", "11011001", "11011010", "11011011", "11011100", "11011101", "11011110", "11011111", "11100000", "11100001", "11100010", "11100011", "11100100", "11100101", "11100110", "11100111", "11101000", "11101001", "11101010", "11101011", "11101100", "11101101", "11101110", "11101111", "11110000", "11110001", "11110010", "11110011", "11110100", "11110101", "11110110", "11110111", "11111000", "11111001", "11111010", "11111011", "11111100", "11111101", "11111110", "11111111"}
function module.numToBinary(int, width)
	local width = width or 8
	if width > 64 then
		for i = 64, width - 1 do
			binValues[#binValues+1] = 2^i
		end
	end
	--assert(int < 2^width, string.format("int value out of range for %s bit width", width))
	local value = ""
	for i = width, 1, -1 do
		local v = binValues[i]
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
	if len > 64 then
		for i = 64, len - 1 do
			binValues[#binValues+1] = 2^i
		end
	end
	local int = 0
	for i = 1, len do
		if str:sub(i, i) == "1" then
			int = int + binValues[len - (i-1)]
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
	return bit32.bxor(bit32.band(x,y),bit32.band(bit32.bnot(x),z))
end
function maj(x, y, z)
	return bit32.bxor(bit32.bxor(bit32.band(x,y),bit32.band(x,z)),bit32.band(y,z))
end
function bsig0(x)
	return bit32.bxor(bit32.bxor(bit32.rrotate(x, 2),bit32.rrotate(x, 13)),bit32.rrotate(x, 22))
end
function bsig1(x)
	return bit32.bxor(bit32.bxor(bit32.rrotate(x, 6),bit32.rrotate(x, 11)),bit32.rrotate(x, 25))
end
function ssig0(x)
	return bit32.bxor(bit32.bxor(bit32.rrotate(x, 7),bit32.rrotate(x, 18)),bit32.rshift(x, 3))
end
function ssig1(x)
	return bit32.bxor(bit32.bxor(bit32.rrotate(x, 17),bit32.rrotate(x, 19)),bit32.rshift(x, 10))
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
	local len = str:len()
	local L = len * 8
	local bitString = ""
	for i = 1,len do
		bitString = bitString .. module.numToBinary(str:byte(i))
	end
	-- If str == "" then hash for 0 bit length (this satisfies a test vector)
	bitString = bitString == "" and "1"..string.rep("0",511) or bitString

	-- Pad bitString so the length is a multiple of 512
	if bitString:len() % 512 ~= 0 then
		local pad = (448 - ((L + 1) % 512)) % 512
		bitString = bitString .. "1" 
		.. string.rep("0", pad)
		.. module.numToBinary(L, 64)
		L = L + pad + 65
	end

	-- Split bitString into blocks of 512 bits
	local blocks = {}
	for i = 1, L, 512 do
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
			W[t+1] = module.binaryToNum(blocks[i]:sub((t*32)+1,(t*32)+32))
		end
		for t = 17, 64 do
			W[t] = (ssig1(W[t-2]) + W[t-7] + ssig0(W[t-15]) + W[t-16]) % 4294967296
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
			local T1 = (h + bsig1(e) + ch(e, f, g) + K[t] + W[t]) % 4294967296
			local T2 = (bsig0(a) + maj(a, b, c)) % 4294967296
			h = g
			g = f
			f = e
			e = (d + T1) % 4294967296
			d = c
			c = b
			b = a
			a = (T1 + T2) % 4294967296
		end

		-- Add working variables to the hash value table
		H[1] = (a + H[1]) % 4294967296
		H[2] = (b + H[2]) % 4294967296
		H[3] = (c + H[3]) % 4294967296
		H[4] = (d + H[4]) % 4294967296
		H[5] = (e + H[5]) % 4294967296
		H[6] = (f + H[6]) % 4294967296
		H[7] = (g + H[7]) % 4294967296
		H[8] = (h + H[8]) % 4294967296
	end

	-- Produce result as hex string
	local result = ""
	for v = 1, 8 do
		result = result .. module.binaryToHex(module.numToBinary(H[v], 32), false)
	end

	return result
end

function module.sha1(str)
	-- Initial variables
	local h0 = 0x67452301
	local h1 = 0xEFCDAB89
	local h2 = 0x98BADCFE
	local h3 = 0x10325476
	local h4 = 0xC3D2E1F0
	-- Message length in bits
	local len = str:len()
	local ml = len * 8
	local bitString = ""
	for i = 1, len do
		bitString = bitString .. commonBinary[str:byte(i)+1]
	end
	-- If str == "" then hash for 0 bit length (this satisfies a test vector)
	if bitString == "" then
		bitString = "1"..string.rep("0",511)
		ml = 512
	end
	
	-- Pad bitString so the length is a multiple of 512
	if ml % 512 ~= 0 then
		local pad = (448 - ((ml + 1) % 512)) % 512
		bitString = bitString .. "1" 
		.. string.rep("0", pad)
		.. module.numToBinary(ml, 64)
		ml = ml + pad + 65
	end
	
	-- Split bitString into blocks of 512 bits
	local blocks = {}
	for i = 1, ml, 512 do
		blocks[#blocks + 1] = bitString:sub(i, i+511)
	end

	for i = 1,#blocks do
		-- Break blocks into 16 32bit words
		local W = {}
		for t = 0, 15 do
			W[t+1] = module.binaryToNum(blocks[i]:sub((t*32)+1,(t*32)+32)) 
		end
		-- Extend message schedule
		for t = 17, 80 do
			W[t] = bit32.lrotate(bit32.bxor(bit32.bxor(bit32.bxor(W[t-3], W[t-8]), W[t-14]), W[t-16]), 1)
		end
		
		local a = h0
		local b = h1
		local c = h2
		local d = h3
		local e = h4
		
		for i = 1, 80 do
			local f
			local k
			
			if i <= 20 then
				f = bit32.bor(bit32.band(b, c), bit32.band(bit32.bnot(b), d))
				k = 0x5A827999
			elseif i <= 40 then
				f = bit32.bxor(bit32.bxor(b, c), d)
				k = 0x6ED9EBA1
			elseif i <= 60 then
				f = bit32.bor(bit32.bor(bit32.band(b, c), bit32.band(b, d)), bit32.band(c, d)) 
				k = 0x8F1BBCDC
			elseif i <= 80 then
				f = bit32.bxor(bit32.bxor(b, c), d)
				k = 0xCA62C1D6
			end
			
			local T1 = (bit32.lrotate(a, 5) + f + e + k + W[i]) % 4294967296
			e = d
			d = c
	        c = bit32.lrotate(b, 30)
	        b = a
	        a = T1			
		end
		
		h0 = (h0 + a) % 4294967296
		h1 = (h1 + b) % 4294967296
		h2 = (h2 + c) % 4294967296
		h3 = (h3 + d) % 4294967296
		h4 = (h4 + e) % 4294967296
	end
	
	return 
		module.binaryToHex(module.numToBinary(h0, 32), false)..
		module.binaryToHex(module.numToBinary(h1, 32), false)..
		module.binaryToHex(module.numToBinary(h2, 32), false)..
		module.binaryToHex(module.numToBinary(h3, 32), false)..
		module.binaryToHex(module.numToBinary(h4, 32), false)
end

return module
