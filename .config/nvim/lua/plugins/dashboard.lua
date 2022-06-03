local vim_globals = vim.g

vim_globals.dashboard_disable_at_vimenter = 0
vim_globals.dashboard_disable_statusline = 1
vim_globals.dashboard_default_executive = "telescope"
vim_globals.dashboard_custom_header = {
	'⠀⠀⠀⣶⣶⣶⣶⡆⠀⠀⠀⠀⠀⠀ ',
	'⠀⠀⠀⠛⠛⢻⣿⣿⡀⠀⠀⠀⠀⠀⠀',
	'⠀⠀⠀⠀⠀⢀⣿⣿⣷⠀⠀⠀⠀⠀⠀',
	'⠀⠀⠀⠀⢀⣾⣿⣿⣿⣇⠀⠀⠀⠀⠀',
	'⠀⠀⠀⢠⣿⣿⡟⢹⣿⣿⡆⠀⠀⠀⠀',
	'⠀⠀⣰⣿⣿⠏⠀⠀⢻⣿⣿⡄⠀⠀⠀',
	'⠀⣴⣿⡿⠃⠀⠀⠀⠈⢿⣿⣷⣤⣤⡆',
	'⠾⠿⠿⠁⠀⠀⠀⠀⠀⠘⣿⣿⡿⠿⠛'
}

vim_globals.dashboard_custom_section = {
	a = {
		description = { "  Find File                  SPC ff" },
		command = "Telescope find_files"
	},
	b = {
		description = { "  Recents                    SPC of" },
		command = "Telescope oldfiles"
	},
	c = {
		description = { "  Grep                       SPC lg" },
		command = "Telescope live_grep"
	},
	d = {
		description = { "洛 New File                   SPC dnf" },
		command = "DashboardNewFile"
	},
	e = {
		description = { "  Bookmarks                  SPC djm" },
		command = "Telescope marks"
	},
	f = {
		description = { "  Load Last Session          SPC dsl" },
		command = "SessionLoad"
	}
}

vim_globals.dashboard_custom_footer = { "" }
