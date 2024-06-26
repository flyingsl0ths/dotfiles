local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not vim.loop.fs_stat(lazypath) then
	vim.fn.system({
		"git",
		"clone",
		"--filter=blob:none",
		"https://github.com/folke/lazy.nvim.git",
		"--branch=stable", -- latest stable release
		lazypath,
	})
end

vim.opt.rtp:prepend(lazypath)

local plugins = {
	"wbthomason/packer.nvim",

	{
		"NvChad/nvterm",
		config = function()
			require("nvterm").setup()
		end,
	},


	{ "catppuccin/nvim",   name = "catppuccin", priority = 1000 },

	{
		'akinsho/bufferline.nvim',
		version = "*",
		dependencies = 'nvim-tree/nvim-web-devicons',
		after = "catppuccin",
	},

	{
		'kyazdani42/nvim-tree.lua',
		dependencies = {
			'kyazdani42/nvim-web-devicons' -- optional, for file icon
		}
	},

	{
		'feline-nvim/feline.nvim',
		dependencies = {
			"lewis6991/gitsigns.nvim",
			"kyazdani42/nvim-web-devicons",
			config = function() require("gitsigns").setup() end
		}
	},

	{
		'goolord/alpha-nvim',
		config = function()
			require 'alpha'.setup(require 'alpha.themes.dashboard'.config)
		end
	},

	"neovim/nvim-lspconfig",
	"ray-x/lsp_signature.nvim",

	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"windwp/nvim-ts-autotag",
		}
	},

	"rafamadriz/friendly-snippets",
	"L3MON4D3/LuaSnip",
	"hrsh7th/nvim-cmp",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	"saadparwaiz1/cmp_luasnip",
	"hrsh7th/cmp-nvim-lua",

	"jvgrootveld/telescope-zoxide",

	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/popup.nvim",
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-media-files.nvim",
			"nvim-telescope/telescope-live-grep-args.nvim",
		}
	},

	"udalov/kotlin-vim",

	"purescript-contrib/purescript-vim",

	{
		'saecki/crates.nvim',
		tag = 'v0.3.0',
		dependencies = { 'nvim-lua/plenary.nvim' },
		config = function()
			require('crates').setup()
		end,
	},

	{ "folke/neodev.nvim", opts = {} },
	"nvim-neotest/nvim-nio",
	"mfussenegger/nvim-dap",
	"rcarriga/nvim-dap-ui",
	"nvim-telescope/telescope-dap.nvim",
	"mfussenegger/nvim-dap-python",
	"jbyuki/one-small-step-for-vimkind",

	{
		"simrat39/symbols-outline.nvim",
		config = function()
			require("symbols-outline").setup()
		end
	},

	"szw/vim-maximizer",
	"norcalli/nvim-colorizer.lua",

	{
		'numToStr/Comment.nvim',
		config = function() require('Comment').setup() end
	},

	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		config = function()
			require("todo-comments").setup {}
		end
	},

	"gpanders/editorconfig.nvim",
	"mhartington/formatter.nvim",
	"ggandor/lightspeed.nvim",
	"andymass/vim-matchup",
	{
		"fedepujol/move.nvim",
		config = function()
			require("move").setup()
		end
	},
	"LnL7/vim-nix",
	"vmchale/dhall-vim",
	"github/copilot.vim"
}

local opts = {}

require("lazy").setup(plugins, opts)
