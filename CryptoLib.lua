local module = {}

local AND = bit32.band
local NOT = bit32.bnot
local OR = bit32.bor
local XOR = bit32.bxor
local ROL = bit32.lrotate
local SHL = bit32.lshift
local ROR = bit32.rrotate
local SHR = bit32.rshift
-- Every value up to 64 bits worth
local binValues = {1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 33554432, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648, 4294967296, 8589934592, 17179869184, 34359738368, 68719476736, 137438953472, 274877906944, 549755813888, 1099511627776, 2199023255552, 4398046511104, 8796093022208, 17592186044416, 35184372088832, 70368744177664, 1.4073748835533e+14, 2.8147497671066e+14, 5.6294995342131e+14, 1.1258999068426e+15, 2.2517998136852e+15, 4.5035996273705e+15, 9.007199254741e+15, 1.8014398509482e+16, 3.6028797018964e+16, 7.2057594037928e+16, 1.4411518807586e+17, 2.8823037615171e+17, 5.7646075230342e+17, 1.1529215046068e+18, 2.3058430092137e+18, 4.6116860184274e+18, 9.2233720368548e+18}
local binValuesLength = 64

-- Binary values from 0..255
local commonBinary = {"00000000", "00000001", "00000010", "00000011", "00000100", "00000101", "00000110", "00000111", "00001000", "00001001", "00001010", "00001011", "00001100", "00001101", "00001110", "00001111", "00010000", "00010001", "00010010", "00010011", "00010100", "00010101", "00010110", "00010111", "00011000", "00011001", "00011010", "00011011", "00011100", "00011101", "00011110", "00011111", "00100000", "00100001", "00100010", "00100011", "00100100", "00100101", "00100110", "00100111", "00101000", "00101001", "00101010", "00101011", "00101100", "00101101", "00101110", "00101111", "00110000", "00110001", "00110010", "00110011", "00110100", "00110101", "00110110", "00110111", "00111000", "00111001", "00111010", "00111011", "00111100", "00111101", "00111110", "00111111", "01000000", "01000001", "01000010", "01000011", "01000100", "01000101", "01000110", "01000111", "01001000", "01001001", "01001010", "01001011", "01001100", "01001101", "01001110", "01001111", "01010000", "01010001", "01010010", "01010011", "01010100", "01010101", "01010110", "01010111", "01011000", "01011001", "01011010", "01011011", "01011100", "01011101", "01011110", "01011111", "01100000", "01100001", "01100010", "01100011", "01100100", "01100101", "01100110", "01100111", "01101000", "01101001", "01101010", "01101011", "01101100", "01101101", "01101110", "01101111", "01110000", "01110001", "01110010", "01110011", "01110100", "01110101", "01110110", "01110111", "01111000", "01111001", "01111010", "01111011", "01111100", "01111101", "01111110", "01111111", "10000000", "10000001", "10000010", "10000011", "10000100", "10000101", "10000110", "10000111", "10001000", "10001001", "10001010", "10001011", "10001100", "10001101", "10001110", "10001111", "10010000", "10010001", "10010010", "10010011", "10010100", "10010101", "10010110", "10010111", "10011000", "10011001", "10011010", "10011011", "10011100", "10011101", "10011110", "10011111", "10100000", "10100001", "10100010", "10100011", "10100100", "10100101", "10100110", "10100111", "10101000", "10101001", "10101010", "10101011", "10101100", "10101101", "10101110", "10101111", "10110000", "10110001", "10110010", "10110011", "10110100", "10110101", "10110110", "10110111", "10111000", "10111001", "10111010", "10111011", "10111100", "10111101", "10111110", "10111111", "11000000", "11000001", "11000010", "11000011", "11000100", "11000101", "11000110", "11000111", "11001000", "11001001", "11001010", "11001011", "11001100", "11001101", "11001110", "11001111", "11010000", "11010001", "11010010", "11010011", "11010100", "11010101", "11010110", "11010111", "11011000", "11011001", "11011010", "11011011", "11011100", "11011101", "11011110", "11011111", "11100000", "11100001", "11100010", "11100011", "11100100", "11100101", "11100110", "11100111", "11101000", "11101001", "11101010", "11101011", "11101100", "11101101", "11101110", "11101111", "11110000", "11110001", "11110010", "11110011", "11110100", "11110101", "11110110", "11110111", "11111000", "11111001", "11111010", "11111011", "11111100", "11111101", "11111110", "11111111"}

local TwoPower = setmetatable({}, {
	__index = function(self, Index)
		local Value = 2^Index
		self[Index] = Value
		return Value
	end;
})

function module.numToBinary(int, width)
	local width = width or 8
	if width > 64 then
		for i = 64, width - 1 do
			binValuesLength = binValuesLength + 1
			binValues[binValuesLength] = TwoPower[i]
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
	local len = #str
	-- Generate binary values for the width given
	if len > 64 then
		for i = 64, len - 1 do
			binValuesLength = binValuesLength + 1
			binValues[binValuesLength] = TwoPower[i]
		end
	end
	local int = 0
	for i = 1, len do
		if string.sub(str, i, i) == "1" then
			int = int + binValues[len - (i-1)]
		end
	end
	return int
end

local module_numToBinary = module.numToBinary
local module_binaryToNum = module.binaryToNum

local base16Values = {"0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"}
function module.binaryToHex(str, prepend0x)
	local prepend0x = prepend0x == nil and true or prepend0x
	local hex = ""
	for i = 1, #str, 4 do
		hex = hex .. base16Values[module_binaryToNum(string.sub(str,i,i+3))+1]
	end

	if prepend0x then
		return "0x"..hex
	else
		return hex
	end
end

local Alphabet = {}
local Indexes = {}

-- A-Z
for Index = 65, 90 do
	table.insert(Alphabet, Index)
end

-- a-z
for Index = 97, 122 do
	table.insert(Alphabet, Index)
end

-- 0-9
for Index = 48, 57 do
	table.insert(Alphabet, Index)
end

table.insert(Alphabet, 43) -- +
table.insert(Alphabet, 47) -- /

for Index, Character in ipairs(Alphabet) do
	Indexes[Character] = Index
end

function module.base64Encode(str)
	local Output = {}
	local Length = 0

	for Index = 1, #str, 3 do
		local C1, C2, C3 = string.byte(str, Index, Index + 2)

		local A = SHR(C1, 2)
		local B = SHL(AND(C1, 3), 4) + SHR(C2 or 0, 4)
		local C = SHL(AND(C2 or 0, 15), 2) + SHR(C3 or 0, 6)
		local D = AND(C3 or 0, 63)

		Length = Length + 1
		Output[Length] = Alphabet[A + 1]

		Length = Length + 1
		Output[Length] = Alphabet[B + 1]

		Length = Length + 1
		Output[Length] = C2 and Alphabet[C + 1] or 61

		Length = Length + 1
		Output[Length] = C3 and Alphabet[D + 1] or 61
	end

	local NewOutput = {}
	local NewLength = 0
	local IndexAdd4096Sub1

	for Index = 1, Length, 4096 do
		NewLength = NewLength + 1
		IndexAdd4096Sub1 = Index + 4096 - 1

		NewOutput[NewLength] = string.char(table.unpack(
			Output,
			Index,
			IndexAdd4096Sub1 > Length and Length or IndexAdd4096Sub1
		))
	end

	local Base64 = table.concat(NewOutput)
	local Base64Length = #Base64

	for Index = 76, Base64Length, 76 do
		Base64 = string.sub(Base64, Index, Index) .. "\n" .. string.sub(Base64, Index + 1, Base64Length)
	end

	return Base64
end

--module.base64Decode = FasterBase64.Decode
function module.base64Decode(base64)
	assert(type(base64) == "string", "Base64 must be quotable for decoding")
	base64 = string.gsub(base64, "\n", "")

	local Output = {}
	local Length = 0

	for Index = 1, #base64, 4 do
		local C1, C2, C3, C4 = string.byte(base64, Index, Index + 3)

		local I1 = Indexes[C1] - 1
		local I2 = Indexes[C2] - 1
		local I3 = (Indexes[C3] or 1) - 1
		local I4 = (Indexes[C4] or 1) - 1

		local A = SHL(I1, 2) + SHR(I2, 4)
		local B = SHL(AND(I2, 15), 4) + SHR(I3, 2)
		local C = SHL(AND(I3, 3), 6) + I4

		Length = Length + 1
		Output[Length] = A

		if C3 ~= 61 then
			Length = Length + 1
			Output[Length] = B
		end

		if C4 ~= 61 then
			Length = Length + 1
			Output[Length] = C
		end
	end

	local NewOutput = {}
	local NewLength = 0
	local IndexAdd4096Sub1

	for Index = 1, Length, 4096 do
		NewLength = NewLength + 1
		IndexAdd4096Sub1 = Index + 4096 - 1

		NewOutput[NewLength] = string.char(table.unpack(
			Output,
			Index,
			IndexAdd4096Sub1 > Length and Length or IndexAdd4096Sub1
		))
	end

	return table.concat(NewOutput)
end

-- Define internal sha2 functions
local function ch(x, y, z)
	return XOR(AND(x,y),AND(NOT(x),z))
end
local function maj(x, y, z)
	return XOR(XOR(AND(x,y),AND(x,z)),AND(y,z))
end
local function bsig0(x)
	return XOR(XOR(ROR(x, 2),ROR(x, 13)),ROR(x, 22))
end
local function bsig1(x)
	return XOR(XOR(ROR(x, 6),ROR(x, 11)),ROR(x, 25))
end
local function ssig0(x)
	return XOR(XOR(ROR(x, 7),ROR(x, 18)),SHR(x, 3))
end
local function ssig1(x)
	return XOR(XOR(ROR(x, 17),ROR(x, 19)),SHR(x, 10))
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
	local len = #str
	local L = len * 8
	local bitArray = {}
	local bitArrayLength = len

	for i = 1, len do
		bitArray[i] = commonBinary[string.byte(str,i)+1]
	end

	-- If str == "" then hash for 0 bit length (this satisfies a test vector)
	if len == 0 then
		bitArray[1] = "10000000"
		for i = 2,64 do
			bitArray[i] = "00000000"
		end
		bitArrayLength = 64
		L = 512
	end
	
	-- Pad bitString so the length is a multiple of 512
	if L % 512 ~= 0 then
		local pad = (448 - ((L + 1) % 512)) % 512
		bitArrayLength = bitArrayLength + 1
		bitArray[bitArrayLength] = "10000000"
		for i = 1, (pad-7)/8 do
			bitArrayLength = bitArrayLength + 1
			bitArray[bitArrayLength] = "00000000"
		end
		-- add message length to end
		len = module_numToBinary(L, 64)
		for i = 1, 64, 8 do
			bitArrayLength = bitArrayLength + 1
			bitArray[bitArrayLength] = string.sub(len,i,i+7)
		end
	end
	
	-- Split bitString into blocks of 512 bits
	local blocks = {}
	local blocksLength = 0
	for i = 1, bitArrayLength, 64 do
		local words = table.create(16)
		local wordsLength = 0

		for w = i, i+63, 4 do
			wordsLength = wordsLength + 1
			words[wordsLength] = module_binaryToNum(table.concat(bitArray, "", w, w+3))
		end

		blocksLength = blocksLength + 1
		blocks[blocksLength] = words
	end
		
	-- Initialise hash values H[1..8]
	local H = table.create(8)
	H[1] = 0x6a09e667
	H[2] = 0xbb67ae85
	H[3] = 0x3c6ef372
	H[4] = 0xa54ff53a
	H[5] = 0x510e527f
	H[6] = 0x9b05688c
	H[7] = 0x1f83d9ab
	H[8] = 0x5be0cd19

	-- Main hash loop
	for i, W in ipairs(blocks) do
		-- Initialise message schedule W[1..64] for block
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
		result = result .. string.format("%x", H[v])
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
	
	local len = #str
	-- Message length in bits
	local ml = len * 8
	local bitArray = {}
	local bitArrayLength = len

	for i = 1, len do
		bitArray[i] = commonBinary[string.byte(str,i)+1]
	end
	
	-- If str == "" then hash for 0 bit length (this satisfies a test vector)
	if len == 0 then
		bitArray[1] = "10000000"
		for i = 2,64 do
			bitArray[i] = "00000000"
		end
		bitArrayLength = 64
		ml = 512
	end
	
	-- Pad bitString so the length is a multiple of 512
	if ml % 512 ~= 0 then
		local pad = (448 - ((ml + 1) % 512)) % 512
		bitArrayLength = bitArrayLength + 1
		bitArray[bitArrayLength] = "10000000"
		for i = 1, (pad-7)/8 do
			bitArrayLength = bitArrayLength + 1
			bitArray[bitArrayLength] = "00000000"
		end
		-- add message length to end
		len = module_numToBinary(ml, 64)
		for i = 1, 64, 8 do
			bitArrayLength = bitArrayLength + 1
			bitArray[bitArrayLength] = string.sub(len,i,i+7)
		end
	end
	
	-- Split bitString into blocks of 512 bits containing 16 32 bit words
	local blocks = {}
	local blocksLength = 0
	for i = 1, bitArrayLength, 64 do
		local words = table.create(16)
		local wordsLength = 0

		for w = i, i+63, 4 do
			wordsLength = wordsLength + 1
			words[wordsLength] = module_binaryToNum(table.concat(bitArray, "", w, w+3))
		end

		blocksLength = blocksLength + 1
		blocks[blocksLength] = words
	end
	
	for i, W in ipairs(blocks) do
		-- Break blocks into 16 32bit words
		-- Extend message schedule
		for t = 17, 80 do
			W[t] = ROL(XOR(XOR(XOR(W[t-3], W[t-8]), W[t-14]), W[t-16]), 1)
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
				f = OR(AND(b, c), AND(NOT(b), d))
				k = 0x5A827999
			elseif i <= 40 then
				f = XOR(XOR(b, c), d)
				k = 0x6ED9EBA1
			elseif i <= 60 then
				f = OR(OR(AND(b, c), AND(b, d)), AND(c, d)) 
				k = 0x8F1BBCDC
			elseif i <= 80 then
				f = XOR(XOR(b, c), d)
				k = 0xCA62C1D6
			end
			
			local T1 = (ROL(a, 5) + f + e + k + W[i]) % 4294967296
			e = d
			d = c
	        c = ROL(b, 30)
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
		string.format("%x", h0)..
		string.format("%x", h1)..
		string.format("%x", h2)..
		string.format("%x", h3)..
		string.format("%x", h4)
end

return module
