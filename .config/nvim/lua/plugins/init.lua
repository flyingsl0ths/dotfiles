-- Bootstrap lazy.nvim
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out,                            "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

-- Make sure to setup `mapleader` and `maplocalleader` before
-- loading lazy.nvim so that mappings are correct.
-- This is also a good place to setup other settings (vim.opt)
vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

local plugins = {
	"wbthomason/packer.nvim",

	{
		"NvChad/nvterm", opts = {}
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
			opts = {}
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
		opts = {}
	},

	{
		"David-Kunz/cmp-npm",
		event = { "BufRead package.json" },
		dependencies = { 'nvim-lua/plenary.nvim' },
		ft = "json",
		opts = {}
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

	{
		"rcarriga/nvim-dap-ui",
		opts = {}
	},

	"nvim-telescope/telescope-dap.nvim",
	"mfussenegger/nvim-dap-python",
	"jbyuki/one-small-step-for-vimkind",

	{
		"simrat39/symbols-outline.nvim",
		opts = {}
	},

	"szw/vim-maximizer",
	"norcalli/nvim-colorizer.lua",

	{
		'numToStr/Comment.nvim',
		opts = {}
	},

	{
		"folke/todo-comments.nvim",
		dependencies = { "nvim-lua/plenary.nvim" },
		opts = {}
	},

	"gpanders/editorconfig.nvim",
	"mhartington/formatter.nvim",
	"ggandor/lightspeed.nvim",
	"andymass/vim-matchup",
	{
		"fedepujol/move.nvim",
		opts = {}
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

require("lazy").setup(plugins)
