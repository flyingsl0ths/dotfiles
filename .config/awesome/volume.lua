local wibox = require("wibox")

local function update_volume()
	local fd = io.popen("pamixer --get-volume-human")
	local volume = fd:read("*all")
	fd:close()

	volume = string.format(" <span foreground=\"#FAE3B0\"></span> %s", volume)

	return volume
end

return setmetatable({}, { __call = function()
	local volume_widget = wibox.widget {
		markup = update_volume(),
		widget = wibox.widget.textbox,
		font = "JetBrainsMonoMedium Nerd Font"
	}

	local volume_stat_timer = timer({ timeout = 0.5 })

	volume_stat_timer:connect_signal("timeout",
		function() volume_widget:set_markup(update_volume()) end)

	volume_stat_timer:start()

	return volume_widget
end })
