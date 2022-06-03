local present, packer = pcall(require, "packer")

if not present then return end

packer.startup(function()
	use "wbthomason/packer.nvim"
	use "Nvchad/extensions"
	use "gpanders/editorconfig.nvim"

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

	use "glepnir/dashboard-nvim"
	use {
		'feline-nvim/feline.nvim',
		requires = {
			{
				"lewis6991/gitsigns.nvim",
				config = function() require("gitsigns").setup() end
			}, { "kyazdani42/nvim-web-devicons" }
		}
	}

	use "neovim/nvim-lspconfig"
	use "ray-x/lsp_signature.nvim"
	use {
		"nvim-treesitter/nvim-treesitter",
		run = ":TSUpdate",
	}

	use "simrat39/symbols-outline.nvim"
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
			{ "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
			{ "nvim-lua/popup.nvim" }, { "nvim-lua/plenary.nvim" },
			{ "nvim-telescope/telescope-media-files.nvim" }
		}
	}

	use 'mfussenegger/nvim-jdtls'
	use "szw/vim-maximizer"
	use "norcalli/nvim-colorizer.lua"
	use {
		'numToStr/Comment.nvim',
		config = function() require('Comment').setup() end
	}

	use {
		"AmeerTaweel/todo.nvim",
		requires = "nvim-lua/plenary.nvim",
		config = function() require("todo").setup {} end
	}

	use "mhartington/formatter.nvim"
	-- use "ggandor/lightspeed.nvim"
	use "andymass/vim-matchup"
	use "lukas-reineke/indent-blankline.nvim"
	use "udalov/kotlin-vim"
end)
