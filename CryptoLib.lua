local module = {}

function module.numToBinary(int)
	local binValues = {128,64,32,16,8,4,2,1}
	assert(int < 255, "numToBinary only produces 8 bit wide bytes")
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
	assert(len <= 8, "binaryToNum allows bit width of 8 or less only")
	-- Generate binary values for the width given
	local binValues = {128,64,32,16,8,4,2,1}
	for i = 1,8 - len do
		table.remove(binValues, 1)
	end
	
	local int = 0
	for i,v in pairs(binValues) do
		if str:sub(i,i) == "1" then
			int = int + v
		end
	end
	return int
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
	
	return base64
end

return module
