local key = require "awful.key"

return {
	make_keybinding = function(binding)
		return key(binding.modifiers, binding.key, binding.command,
			binding.description)
	end
}
