-- module Prelude

local exports = {}

exports.arrayMap = function(f)
	return function(as)

		local ret = {}

		for i, v in ipairs(as) do

			ret[i] = f(v)
		end

		return ret
	end
end


exports.arrayBind = function(as)

	return function(k)

		local ret = {}
		local index = 1

		for i, v in ipairs(as) do

			for j, w in ipairs(k(v)) do
				ret[index] = w
				index = index + 1
			end
		end

		return ret
	end
end
	

exports.concatString = function(s1)
	return function(s2)
		return s1 .. s2
	end
end


exports.concatArray = function(as)
	return function(bs)

		local ret = {}

		for i, v in ipairs(as) do
			table.insert(ret, v)
		end

		for i, v in ipairs(bs) do
			table.insert(ret, v)
		end

		return ret
	end
end


exports.intAdd = function(x)
	return function(y)
		return x + y
	end
end


exports.intMul = function(x)
	return function(y)
		return x * y
	end
end


exports.numAdd = exports.intAdd

exports.numMul = exports.intMul


exports.numSub = function(x)
	return function(y)
		return x - y
	end
end


exports.intSub = exports.numSub


exports.numDiv = function(x)
	return function(y)
		return x / y
	end
end


exports.intDiv = function(x)
	return function(y)
		return math.modf(x / y)
	end
end


exports.intMod = function(x)
	return function(y)
		return x % y
	end
end


exports.refEq = function(x)
	return function(y)
		return x == y
	end
end


exports.refIneq = function(x)
	return function(y)
		return x ~= y
	end
end


exports.eqArrayImpl = function(comp)
	return function(as)
		return function (bs)

			if (#as ~= #bs) then
				return false
			end

			for i, v in pairs(as) do
				if v ~= bs[i] then
					return false
				end
			end

			return true
		end
	end
end


exports.ordArrayImpl = function(comp)
	return function(as)
		return function(bs)
			for i, v in ipairs(as) do
				local res = comp(v, bs[i])
				if (res ~= 0) then
					return res
				end
			end

			local alen = #as
			local blen = #bs

			if (alen > blen) then
				return -1
			elseif (blen == alen) then
				return 0
			else -- blen > alen
				return 1
			end
		end
	end
end


exports.unsafeCompareImpl = function(lt)
	return function(eq)
		return function(gt)
			return function(x)
				return function(y)

					if x > y then return gt end

					if x == y then return eq end

					return lt
				end
			end
		end
	end
end


-- Using 50 bit to be safe, but probably a wider range is supported.
exports.topInt = 2^50
exports.bottomInt = -2^50

exports.topChar = "\255"
exports.bottomChar = "\0"


exports.boolOr = function(b1)
	return function(b2)
		return b1 or b2
	end
end

exports.boolAnd = function(b1)
	return function(b2)
		return b1 and b2
	end
end

exports.boolNot = function(b) return not b end

exports.showIntImpl = tostring

exports.showNumberImpl = tostring

exports.showCharImpl = function(x) return "'" .. x .. "'" end

exports.showStringImpl = function(x) return "\"" .. x .. "\"" end

exports.showArrayImpl = function(show)
	return function(arr)
		local collect = {}
		local first = true

		table.insert(collect, "[")
		
		for i, v in ipairs(arr) do
			if not first then
				table.insert(collect, ",")
			else
				first = false
			end
			table.insert(collect, show(v))
		end

		table.insert(collect, "]")

		return table.concat(collect)
	end
end

return exports
