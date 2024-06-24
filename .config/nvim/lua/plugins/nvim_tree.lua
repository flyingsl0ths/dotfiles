local nvimtree = require "nvim-tree"
local tree_view = require("nvim-tree.view")

local HEIGHT_RATIO = 0.8 -- You can change this
local WIDTH_RATIO = 0.5  -- You can change this too

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
	view = {
		float = {
			enable = true,
			open_win_config = function()
				local screen_w = vim.opt.columns:get()
				local screen_h = vim.opt.lines:get() - vim.opt.cmdheight:get()
				local window_w = screen_w * WIDTH_RATIO
				local window_h = screen_h * HEIGHT_RATIO
				local window_w_int = math.floor(window_w)
				local window_h_int = math.floor(window_h)
				local center_x = (screen_w - window_w) / 2
				local center_y = ((vim.opt.lines:get() - window_h) / 2)
				    - vim.opt.cmdheight:get()
				return {
					border = 'rounded',
					relative = 'editor',
					row = center_y,
					col = center_x,
					width = window_w_int,
					height = window_h_int,
				}
			end,
		},
		width = function()
			return math.floor(vim.opt.columns:get() * WIDTH_RATIO)
		end,
	},
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


vim.api.nvim_create_augroup("NvimTreeResize", {
	clear = true,
})

vim.api.nvim_create_autocmd({ "VimResized" }, {
	group = "NvimTreeResize",
	callback = function()
		if tree_view.is_visible() then
			tree_view.close()
			nvimtree.open()
		end
	end
})
