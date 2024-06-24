local ts_config = require "nvim-treesitter.configs"

ts_config.setup({
	build = ":TSUpdate",
	lazy = true,
	ensure_installed = {
		"bash",
		"c",
		"cpp",
		"css",
		"dockerfile",
		"gitignore",
		"go",
		"groovy",
		"haskell",
		"html",
		"java",
		"javascript",
		"json",
		"kotlin",
		"lua",
		"markdown",
		"markdown_inline",
		"ocaml",
		"python",
		"query",
		"rust",
		"swift",
		"tsx",
		"typescript",
		"vim",
		"yaml",
	},
	event = { "BufReadPre", "BufNewFile" },
	highlight = { enable = true, use_languagetree = true },
	indent = { enable = true },
	autotag = {
		enable = true,
	},
	incremental_selection = {
		enable = true,
		keymaps = {
			init_selection = "<C-space>",
			node_incremental = "<C-space>",
			scope_incremental = false,
			node_decremental = "<bs>",
		}
	},
	context_commentstring = {
		enable = true,
		enable_autocmd = false,
	},

	textobjects = {
		select = {
			enable = true,
			lookahead = true,

			keymaps = {
				-- You can use the capture groups defined in textobjects.scm
				["a="] = { query = "@assignment.outer", desc = "Select outer part of an assignment" },
				["i="] = { query = "@assignment.inner", desc = "Select inner part of an assignment" },
				["l="] = { query = "@assignment.lhs", desc = "Select left hand side of an assignment" },
				["r="] = { query = "@assignment.rhs", desc = "Select right hand side of an assignment" },

				-- works for javascript/typescript files (custom capture I created in after/queries/ecma/textobjects.scm)
				["a:"] = { query = "@property.outer", desc = "Select outer part of an object property" },
				["i:"] = { query = "@property.inner", desc = "Select inner part of an object property" },
				["l:"] = { query = "@property.lhs", desc = "Select left part of an object property" },
				["r:"] = { query = "@property.rhs", desc = "Select right part of an object property" },

				["ap"] = { query = "@parameter.outer", desc = "Select outer part of a parameter/argument" },
				["ip"] = { query = "@parameter.inner", desc = "Select inner part of a parameter/argument" },

				["ai"] = { query = "@conditional.outer", desc = "Select outer part of a conditional" },
				["ii"] = { query = "@conditional.inner", desc = "Select inner part of a conditional" },

				["al"] = { query = "@loop.outer", desc = "Select outer part of a loop" },
				["il"] = { query = "@loop.inner", desc = "Select inner part of a loop" },

				["ac"] = { query = "@call.outer", desc = "Select outer part of a function call" },
				["ic"] = { query = "@call.inner", desc = "Select inner part of a function call" },

				["af"] = {
					query = "@function.outer",
					desc =
					"Select outer part of a method/function definition"
				},
				["if"] = {
					query = "@function.inner",
					desc =
					"Select inner part of a method/function definition"
				},

				["as"] = { query = "@class.outer", desc = "Select outer part of a class" },
				["is"] = { query = "@class.inner", desc = "Select inner part of a class" },
			},
		},

		swap = {
			enable = true,
			swap_next = {
				["<leader>np"] = "@parameter.inner",
				["<leader>nf"] = "@function.outer"
			},
			swap_previous = {
				["<leader>pp"] = "@parameter.inner",
				["<leader>pf"] = "@function.outer"
			}
		}
	},

})
