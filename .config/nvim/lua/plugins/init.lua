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

	"nordtheme/vim",


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


	"hrsh7th/nvim-cmp",
	"hrsh7th/cmp-nvim-lsp",
	"hrsh7th/cmp-buffer",
	"hrsh7th/cmp-path",
	"hrsh7th/cmp-calc",
	"hrsh7th/cmp-nvim-lua",
	"hrsh7th/cmp-emoji",

	{
		"L3MON4D3/LuaSnip",
		-- follow latest release.
		version = "v2.*", -- Replace <CurrentMajor> by the latest released major (first number of latest release)
		-- install jsregexp (optional!).
		build = "make install_jsregexp",
		dependencies = { "rafamadriz/friendly-snippets" }
	},

	"amarakon/nvim-cmp-buffer-lines",

	"saadparwaiz1/cmp_luasnip",

	{
		'saecki/crates.nvim',
		event = { "BufRead Cargo.toml" },
		config = function()
			require('crates').setup()
		end,
	},

	{
		"David-Kunz/cmp-npm",
		event = { "BufRead package.json" },
		dependencies = { 'nvim-lua/plenary.nvim' },
		ft = "json",
		config = function()
			require('cmp-npm').setup({})
		end
	},

	"jvgrootveld/telescope-zoxide",
	"chrisgrieser/cmp-nerdfont",

	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/popup.nvim",
			"nvim-lua/plenary.nvim",
			"nvim-telescope/telescope-live-grep-args.nvim",
		}
	},

	"udalov/kotlin-vim",

	"tikhomirov/vim-glsl",

	"purescript-contrib/purescript-vim",

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
	"github/copilot.vim",
	{
		'MeanderingProgrammer/render-markdown.nvim',
		-- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.nvim' }, -- if you use the mini.nvim suite
		-- dependencies = { 'nvim-treesitter/nvim-treesitter', 'echasnovski/mini.icons' }, -- if you use standalone mini plugins
		dependencies = { 'nvim-treesitter/nvim-treesitter', 'nvim-tree/nvim-web-devicons' }, -- if you prefer nvim-web-devicons
	},

}

local opts = {}

require("lazy").setup(plugins, opts)
