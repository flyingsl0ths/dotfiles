local present, packer = pcall(require, "packer")

if not present then return end

packer.startup(function()
	use "wbthomason/packer.nvim"
	use "Nvchad/extensions"

	use({ "catppuccin/nvim", as = "catppuccin" })

	use {
		'kyazdani42/nvim-tree.lua',
		requires = {
			'kyazdani42/nvim-web-devicons' -- optional, for file icon
		}
	}

	use {
		'akinsho/bufferline.nvim',
		tag = "*",
		requires = 'kyazdani42/nvim-web-devicons'
	}


	use {
		'feline-nvim/feline.nvim',
		requires = {
			{
				"lewis6991/gitsigns.nvim",
				config = function() require("gitsigns").setup() end
			}, { "kyazdani42/nvim-web-devicons" }
		}
	}

	use {
		'goolord/alpha-nvim',
		config = function()
			require 'alpha'.setup(require 'alpha.themes.dashboard'.config)
		end
	}

	use "neovim/nvim-lspconfig"
	use "ray-x/lsp_signature.nvim"
	use {
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	}

	use "rafamadriz/friendly-snippets"
	use "L3MON4D3/LuaSnip"
	use "hrsh7th/nvim-cmp"
	use "hrsh7th/cmp-nvim-lsp"
	use { "hrsh7th/cmp-buffer", after = "cmp-nvim-lsp" }
	use { "hrsh7th/cmp-path", after = "cmp-buffer" }
	use "saadparwaiz1/cmp_luasnip"
	use { "hrsh7th/cmp-nvim-lua", after = "cmp_luasnip" }

	use {
		"nvim-telescope/telescope.nvim",
		requires = {
			{ "nvim-lua/popup.nvim" },
			{ "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope-media-files.nvim" },
			{ "nvim-telescope/telescope-live-grep-args.nvim" },
		}
	}

	use "udalov/kotlin-vim"

	use "mfussenegger/nvim-dap"
	use "rcarriga/nvim-dap-ui"
	use "nvim-telescope/telescope-dap.nvim"
	use "mfussenegger/nvim-dap-python"
	use "jbyuki/one-small-step-for-vimkind"
	use {
		"simrat39/symbols-outline.nvim",
		config = function()
			require("symbols-outline").setup()
		end
	}

	use "szw/vim-maximizer"
	use "norcalli/nvim-colorizer.lua"
	use {
		'numToStr/Comment.nvim',
		config = function() require('Comment').setup() end
	}

	use {
		"folke/todo-comments.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function()
			require("todo-comments").setup {}
		end
	}

	use "gpanders/editorconfig.nvim"
	use "mhartington/formatter.nvim"
	use "ggandor/lightspeed.nvim"
	use "andymass/vim-matchup"
	use 'fedepujol/move.nvim'
	use "LnL7/vim-nix"

	use "elkowar/yuck.vim"
end)
