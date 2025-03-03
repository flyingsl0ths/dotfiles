local wibox = require("wibox")
local utils = require("utils")

local discharging_foreground = "#FAE3B0"
local charging_foreground = "#ABE9B3"
local discharging_icon = ""
local charging_icon = ""

local function update_battery_stats()
	local status = utils.run_command("cat /sys/class/power_supply/BAT0/status")
	local power = utils.run_command("cat /sys/class/power_supply/BAT0/capacity")

	local is_charging = status == "Charging" or status == "Full"
	local foreground = is_charging and charging_foreground or discharging_foreground
	local icon = is_charging and charging_icon or discharging_icon

	local status_fmt = string.format(" <span foreground='%s'>%s </span> %s",
		foreground,
		icon, power)

	return status_fmt
end

return setmetatable({}, {
	__call = function()
		local battery_widget = wibox.widget {
			markup = update_battery_stats(),
			widget = wibox.widget.textbox,
			font = "JetBrainsMono Nerd Font, Medium"
		}

		local battery_stat_timer = timer({ timeout = 1.0 })

		battery_stat_timer:connect_signal("timeout",
			function() battery_widget:set_markup(update_battery_stats()) end)

		battery_stat_timer:start()

		return battery_widget
	end
})
