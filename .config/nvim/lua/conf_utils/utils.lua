return {
	split_at = function(xs, pattern)
		local acc = {}

		for token in string.gmatch(xs, pattern) do
			table.insert(acc, token)
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
