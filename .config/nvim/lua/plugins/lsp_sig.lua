local lspsignature = require "lsp_signature"

lspsignature.setup {
	bind = true,
	doc_lines = 0,
	floating_window = false,
	fix_pos = true,
	hint_enable = true,
	hint_prefix = " ",
	hint_scheme = "String",
	hi_parameter = "Search",
	max_height = 22,
	handler_opts = {
		border = "shadow" -- double, single, shadow, none
	},
}
