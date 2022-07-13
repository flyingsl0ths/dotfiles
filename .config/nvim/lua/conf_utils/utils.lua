return {
	split_at = function(pattern, xs)
		local acc = {}
		for word in xs:gmatch(pattern) do
			table.insert(acc, word)
		end
		return acc
	end,

	run_cmd = function(cmd)
		local fd = io.popen(cmd)

		if fd == nil then
			return
		end

		local output = fd:read("*all")

		return output
	end
}
