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

db.custom_center = {
	{
		icon = '  ',
		desc = "Find File                          ",
		action = "Telescope find_files",
		shortcut = "SPC ff"
	},
	{
		icon = '  ',
		desc = "Recents                            ",
		command = "Telescope oldfiles",
		shortcut = "SPC of"
	},
	{
		icon = ' ',
		desc = " Grep                               ",
		command = "Telescope live_grep",
		shortcut = "SPC lg"
	},
	{
		icon = '洛 ',
		desc = "New File                           ",
		command = "DashboardNewFile",
		shortcut = "SPC dnf"
	},
	{
		icon = '  ',
		desc = "Bookmarks                          ",
		command = "Telescope marks",
		shortcut = "SPC djm"
	},
	{
		icon = '  ',
		desc = "Load Last Session                  ",
		command = "SessionLoad",
		shortcut = "SPC dsl"
	},
}

db.custom_footer = { "" }
