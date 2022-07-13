local wibox = require("wibox")
local vicious = require("vicious")


return setmetatable({}, { __call = function()
    return vicious.register(wibox.widget.textbox(), vicious.widgets.net,
        " <span foreground=\"#F28FAD\" >^</span> ${wlp1s0 up_kb} KB/s <span foreground=\"#ABE9B3\">v</span> ${wlp1s0 down_kb} KB/s")
end
})
