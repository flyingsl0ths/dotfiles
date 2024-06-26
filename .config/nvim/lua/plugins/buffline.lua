local bufferline = require "bufferline"
local mocha = require("catppuccin.palettes").get_palette "mocha"

bufferline.setup {
	options = {
		buffer_close_icon = " ",
		modified_icon = "",
		close_icon = "",
		show_close_icon = true,
		left_trunc_marker = "",
		right_trunc_marker = "",
		close_command = "bdelete! %d", -- can be a string | function, see "Mouse actions"
		max_name_length = 14,
		max_prefix_length = 13,
		tab_size = 20,
		show_tab_indicators = true,
		show_buffer_close_icons = true,
		separator_style = "slope",
		always_show_bufferline = false,
	},
	highlights = require("catppuccin.groups.integrations.bufferline").get {
		styles = { "italic", "bold" },
		custom = {
			all = {
				fill = { bg = "#000000" },
			},
			mocha = {
				background = { fg = mocha.text },
			},
		},
	},
}
