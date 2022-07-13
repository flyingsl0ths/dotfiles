local wibox = require("wibox")
local awful = require("awful")
local home_dir = os.getenv("HOME")

local function update_ram_stats()
	local fd = io.popen(string.format("%s/.config/polybar/scripts/check-ram-usage.sh", home_dir))
	local ram = fd:read("*all")
	fd:close()

	ram = string.format(" <span foreground=\"#FAE3B0\"></span> %s", ram)

	return ram
end

return setmetatable({}, { __call = function()
	local ram_widget = wibox.widget {
		markup = update_ram_stats(),
		widget = wibox.widget.textbox,
		font = "JetBrainsMonoMedium Nerd Font"
	}

	local ram_stat_timer = timer({ timeout = 0.5 })

	ram_stat_timer:connect_signal("timeout",
		function() ram_widget:set_markup(update_ram_stats()) end)

	ram_stat_timer:start()

	return ram_widget
end })
