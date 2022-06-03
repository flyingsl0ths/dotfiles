local wibox = require("wibox")
local vicious = require("vicious")

function date_widget()
	local clock = wibox.widget.textbox()

	clock.point = function(geo, _) return { x = geo.width + 215, y = 5 } end

	return vicious.register(clock, vicious.widgets.date, " %b %d, %Y %a %I:%M%p ")
end
