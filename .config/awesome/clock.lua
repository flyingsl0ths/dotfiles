local wibox = require("wibox")
local vicious = require("vicious")

function my_clock()
    local clock = wibox.widget.textbox()

    clock.point = function(geo, _) return {x = geo.width + 225, y = 5} end

    return vicious.register(clock, vicious.widgets.date,
                            " %b %d, %Y %a %I:%M%p ")
end
