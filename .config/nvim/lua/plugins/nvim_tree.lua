local nvimtree = require "nvim-tree"

vim.g.termguicolors = true
nvimtree.setup {
	diagnostics = {
		enable = false,
		icons = { hint = "", info = "", warning = "", error = "" }
	},
	filters = { dotfiles = false },
	disable_netrw = true,
	hijack_netrw = true,
	open_on_tab = false,
	hijack_cursor = true,
	update_cwd = true,
	update_focused_file = { enable = true, update_cwd = false },
	view = { side = "left", width = 25 },
	renderer = {
		icons = {
			glyphs = {

				default = "",
				symlink = "",
				git = {
					deleted = "",
					ignored = "◌",
					renamed = "➜",
					staged = "✓",
					unmerged = "",
					unstaged = "✗",
					untracked = "★"
				},
				folder = {
					default = "",
					empty = "",
					empty_open = "",
					open = "",
					symlink = "",
					symlink_open = ""
				}
			},
		},
	}
}
