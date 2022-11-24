local db = require('dashboard')

db.custom_header = {
	"               ",
	"               ",
	"               ",
	"⠀⠀⠀⣶⣶⣶⣶⡆⠀⠀⠀⠀⠀⠀ ",
	"⠀⠀⠀⠛⠛⢻⣿⣿⡀⠀⠀⠀⠀⠀⠀",
	"⠀⠀⠀⠀⠀⢀⣿⣿⣷⠀⠀⠀⠀⠀⠀",
	"⠀⠀⠀⠀⢀⣾⣿⣿⣿⣇⠀⠀⠀⠀⠀",
	"⠀⠀⠀⢠⣿⣿⡟⢹⣿⣿⡆⠀⠀⠀⠀",
	"⠀⠀⣰⣿⣿⠏⠀⠀⢻⣿⣿⡄⠀⠀⠀",
	"⠀⣴⣿⡿⠃⠀⠀⠀⠈⢿⣿⣷⣤⣤⡆",
	"⠾⠿⠿⠁⠀⠀⠀⠀⠀⠘⣿⣿⡿⠿⠛",
	"               ",
	"               ",
	"               "
}

local function add_padding(to, n)
	local padded = to

	n = n - #to

	for _ = 1, n do
		padded = padded .. ' '
	end

	return padded
end

local PADDING = 25

db.custom_center = {
	{
		icon = ' ',
		desc = add_padding("Find file", PADDING + 1),
		action = "Telescope find_files",
		shortcut = "SPC ff"
	},
	{
		icon = ' ',
		desc = add_padding("Recents", PADDING + 1),
		command = "Telescope oldfiles",
		shortcut = "SPC of"
	},
	{
		icon = ' ',
		desc = add_padding("Grep", PADDING + 1),
		command = "Telescope live_grep",
		shortcut = "SPC lg"
	},
	{
		icon = '洛',
		desc = add_padding("New File", PADDING),
		command = "DashboardNewFile",
		shortcut = "SPC dnf"
	},
	{
		icon = ' ',
		desc = add_padding("Bookmarks", PADDING),
		command = "Telescope marks",
		shortcut = "SPC djm"
	},
	{
		icon = ' ',
		desc = add_padding("Load Last Session", PADDING),
		command = "SessionLoad",
		shortcut = "SPC dsl"
	},
}

db.custom_footer = { "" }
