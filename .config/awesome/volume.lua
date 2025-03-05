local wibox = require("wibox")
local utils = require("utils")

local function update_volume()
	local volume = utils.run_command("pamixer --get-volume-human")

	local volume_foreground = volume == "N/A" and "#DDB6F2" or "#F28FAD"

	volume = string.format(" <span foreground='%s'> </span> %s", volume_foreground, volume)

	return volume
end

return setmetatable({}, {
	__call = function()
		local volume_widget = wibox.widget {
			markup = update_volume(),
			widget = wibox.widget.textbox,
			font = "JetBrainsMono Nerd Font, Medium"
		}

		local volume_stat_timer = timer({ timeout = 0.5 })

		volume_stat_timer:connect_signal("timeout",
			function() volume_widget:set_markup(update_volume()) end)

		volume_stat_timer:start()

		return volume_widget
	end
})
