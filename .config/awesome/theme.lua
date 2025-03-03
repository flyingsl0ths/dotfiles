local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local icon_directory = os.getenv("HOME") .. "/.config/awesome/icons/"

local theme = {
	font = "JetBrainsMono Nerd Font, Medium 10",
	bg_normal = "#1E1E2E",
	bg_focus = "#96CDFB",
	bg_urgent = "#F28FAD",
	bg_minimize = "#575268",
	bg_systray = "#1E1E2E",
	fg_normal = "#C3BAC6",
	fg_focus = "#302D41",
	fg_urgent = "#D9E0EE",
	fg_minimize = "#D9E0EE",
	useless_gap = dpi(8),
	border_width = dpi(4),
	border_normal = "#302D41",
	border_focus = "#6E6C7E",
	border_marked = "#F28FAD",
	layout_max = icon_directory .. "max.png",
	layout_floating = icon_directory .. "floating.png",
	layout_tile = icon_directory .. "tile.png",
	layout_spiral = icon_directory .. "spiral.png",
	layout_dwindle = icon_directory .. "dwindle.png",
	wallpaper = "~/.local/share/wallpaper/wallpaper",
	icon_theme = "/usr/share/icons/Papirus-Dark"
}

return theme
