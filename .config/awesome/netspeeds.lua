local wibox = require("wibox")
local vicious = require("vicious")


return setmetatable({}, {
	__call = function()
		local speeds = wibox.widget.textbox()

		speeds.point = function(geo, _) return { x = geo.width, y = 10 } end

		return vicious.register(speeds, vicious.widgets.net,
			" <span foreground=\"#F28FAD\" >^</span> ${wlan0 up_kb} KB/s <span foreground=\"#ABE9B3\">v</span> ${wlan0 down_kb} KB/s")
	end
})
