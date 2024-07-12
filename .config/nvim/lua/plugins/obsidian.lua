local obsidian = require "obsidian"

obsidian.setup {
	-- A list of workspace names, paths, and configuration overrides.
	-- If you use the Obsidian app, the 'path' of a workspace should generally be
	-- your vault root (where the `.obsidian` folder is located).
	-- When obsidian.nvim is loaded by your plugin manager, it will automatically set
	-- the workspace to the first workspace in the list whose `path` is a parent of the
	-- current markdown file being edited.
	workspaces = {
		{
			name = "personal",
			path = "~/vaults/personal",
		},
		{
			name = "work",
			path = "~/vaults/code",
		},
	},

	log_level = vim.log.levels.INFO,

	-- Completion of wiki links, local markdown links, and tags
	completion = {
		nvim_cmp = true,
		min_chars = 2
	},

	-- Use defaults
	mappings = {},
}
