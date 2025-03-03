local key = require "awful.key"

return {
	make_keybinding = function(binding)
		return key(binding.modifiers, binding.key, binding.command,
			binding.description)
	end,

	run_command = function(command, default_output)
		default_output = default_output or "N/A"

		local fd = io.popen(command)

		if not fd then
			return default_output
		end

		local output = fd:read("*all")
		fd:close()

		return output
	end
}
