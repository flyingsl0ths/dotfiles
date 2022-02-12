local wibox = require("wibox")
local awful = require("awful")

function update_volume()
  local fd = io.popen("pamixer --get-volume-human")
  local volume = fd:read("*all")
  fd:close()

  volume = string.format(" <span foreground=\"#FAE3B0\"></span> %s", volume)

  return volume
end

volume_widget = wibox.widget {
  markup = update_volume(),
  widget = wibox.widget.textbox,
  font = "JetBrainsMonoMedium Nerd Font"
}

mytimer = timer({timeout = 0.5})
mytimer:connect_signal("timeout",
                       function() volume_widget:set_markup(update_volume()) end)
mytimer:start()
