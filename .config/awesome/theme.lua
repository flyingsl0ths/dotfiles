local xresources = require("beautiful.xresources")
local dpi = xresources.apply_dpi

local gfs = require("gears.filesystem")
local themes_path = gfs.get_themes_dir()

local theme = {}

theme.font = "JetBrainsMonoMedium Nerd Font"

theme.bg_normal = "#302D41"
theme.bg_focus = "#96CDFB"
theme.bg_urgent = "#F28FAD"
theme.bg_minimize = "#6E6C7E"
theme.bg_systray = theme.bg_normal

theme.fg_normal = "#C3BAC6"
theme.fg_focus = "#302D41"
theme.fg_urgent = "#D9E0EE"
theme.fg_minimize = "#D9E0EE"

theme.useless_gap = dpi(8)
theme.border_width = dpi(4)
theme.border_normal = "#302D41"
theme.border_focus = "#6E6C7E"
theme.border_marked = "#F28FAD"

local theme_assets = require("beautiful.theme_assets")
local taglist_square_size = dpi(7)

theme.taglist_squares_sel = theme_assets.taglist_squares_sel(
                                taglist_square_size, theme.fg_normal)
theme.taglist_squares_unsel = theme_assets.taglist_squares_unsel(
                                  taglist_square_size, theme.fg_normal)

local icon_directory = os.getenv("HOME") .. "/.config/awesome/icons/"
theme.layout_max = icon_directory .. "max.png"
theme.layout_floating = icon_directory .. "floating.png"
theme.layout_tile = icon_directory .. "tile.png"
theme.layout_spiral = icon_directory .. "spiral.png"
theme.layout_dwindle = icon_directory .. "dwindle.png"

theme.wallpaper = "/home/flyingsloths/.local/share/wallpaper/wallpaper"

theme.icon_theme = "/usr/share/icons/ePapirus-Dark"

return theme
