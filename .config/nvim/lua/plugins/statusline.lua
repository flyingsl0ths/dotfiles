local statusline = require "feline"

local colors = vim.g.colorscheme == "nord" and ({
	bg = "#2E3440",
	black = "#3B4252",
	yellow = "#EBCB8B",
	cyan = "#88C0D0",
	oceanblue = "#81A1C1",
	green = "#A3BE8C",
	orange = "#D08770",
	violet = "#B48EAD",
	magenta = "#BF616A",
	white = "#ECEFF4",
	fg = "#ECEFF4",
	skyblue = "#8FBCBB",
	red = "#BF616A",
}) or ({
	bg = "#303446",
	black = "#292C3C",
	yellow = "#F2D5CF",
	cyan = "#81C8BE",
	oceanblue = "#8CAAEE",
	green = "#A6D189",
	orange = "#EF9F76",
	violet = "#CA9EE6",
	magenta = "#E78284",
	white = "#C6D0F5",
	fg = "#C6D0F5",
	skyblue = "#99D1DB",
	red = "#E78284",
})

local vi_mode_colors = {
	NORMAL = colors.green,
	INSERT = colors.red,
	VISUAL = colors.magenta,
	OP = colors.green,
	BLOCK = colors.blue,
	REPLACE = colors.violet,
	["V-REPLACE"] = colors.violet,
	ENTER = colors.cyan,
	MORE = colors.cyan,
	SELECT = colors.orange,
	COMMAND = colors.green,
	SHELL = colors.green,
	TERM = colors.green,
	NONE = colors.yellow
}

local components = { active = {}, inactive = {} }

local total_active_components = 2
for _ = 1, total_active_components do table.insert(components.active, {}) end

local total_inactive_components = 2
for _ = 1, total_inactive_components do table.insert(components.inactive, {}) end

local LEFT_SIDE = 1
local RIGHT_SIDE = 2

local vi_mode_utils = require "feline.providers.vi_mode"

table.insert(components.active[LEFT_SIDE], {
	provider = function() return "  " .. vi_mode_utils.get_vim_mode() end,
	hl = function()
		return {
			name = vi_mode_utils.get_mode_highlight_name(),
			fg = colors.bg,
			bg = vi_mode_utils.get_mode_color(),
			style = "bold"
		}
	end,
	right_sep = "block"
})

table.insert(components.active[LEFT_SIDE], {
	provider = {
		name = "file_info",
		opts = { colored_icon = false, type = "relative" }
	},
	hl = function()
		return { fg = colors.bg, bg = colors.oceanblue }
	end,
	left_sep = "block",
	right_sep = "right_rounded"
})

local lsp = require "feline.providers.lsp"

table.insert(components.active[LEFT_SIDE], {
	provider = function()
		return "  " .. lsp.get_diagnostics_count(vim.diagnostic.severity.ERROR)
	end,
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.ERROR)
	end,
	hl = { fg = colors.red }
})

table.insert(components.active[LEFT_SIDE], {
	-- provider = "diagnostic_warnings",
	provider = function()
		return "  " .. lsp.get_diagnostics_count(vim.diagnostic.WARN)
	end,
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.WARN)
	end,
	hl = { fg = colors.yellow }
})

table.insert(components.active[LEFT_SIDE], {
	-- provider = "diagnostic_info",
	provider = function()
		return "  " .. lsp.get_diagnostics_count(vim.diagnostic.INFO)
	end,
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.INFO)
	end,
	hl = { fg = colors.oceanblue }
})

table.insert(components.active[LEFT_SIDE], {
	-- provider = "diagnostic_hints",
	provider = function()
		return "  " .. lsp.get_diagnostics_count(vim.diagnostic.HINT)
	end,
	enabled = function()
		return lsp.diagnostics_exist(vim.diagnostic.severity.HINT)
	end,
	hl = { fg = colors.cyan }
})

table.insert(components.active[RIGHT_SIDE], {
	provider = "lsp_client_names",
	icon = " ",

	hl = { fg = colors.yellow },
	right_sep = " "
})

table.insert(components.active[RIGHT_SIDE], {
	provider = "git_branch",

	hl = { fg = colors.violet, style = "bold" }
})

table.insert(components.active[RIGHT_SIDE],
	{ provider = "git_diff_added", hl = { fg = colors.green } })

table.insert(components.active[RIGHT_SIDE],
	{ provider = "git_diff_changed", hl = { fg = colors.orange } })

table.insert(components.active[RIGHT_SIDE],
	{ provider = "git_diff_removed", hl = { fg = colors.red } })

table.insert(components.active[RIGHT_SIDE], {
	provider = "position",
	left_sep = " ",
	right_sep = " ",
	hl = { fg = colors.cyan, style = "bold" }
})

table.insert(components.active[RIGHT_SIDE],
	{ provider = "line_percentage", hl = { style = "bold" } })
table.insert(components.active[RIGHT_SIDE], {
	provider = "scroll_bar",
	left_sep = " ",
	hl = { fg = colors.oceanblue, style = "bold" }

})

local function file_osinfo()
	local os = vim.bo.fileformat:upper()
	local icon
	if os == "UNIX" then
		icon = " "
	elseif os == "MAC" then
		icon = " "
	else
		icon = " "
	end
	return icon .. os
end

table.insert(components.active[RIGHT_SIDE], {
	provider = file_osinfo,
	left_sep = " ",
	hl = { fg = colors.violet, style = "bold" }
})

table.insert(components.inactive[LEFT_SIDE], components.active[LEFT_SIDE][2])

statusline.setup {
	theme = { bg = colors.bg, fg = colors.fg },
	components = components,
	vi_mode_colors = vi_mode_colors,
	force_inactive = {
		filetypes = { "packer", "NvimTree" },
		buftypes = { "terminal" },
		bufnames = {}
	}
}
