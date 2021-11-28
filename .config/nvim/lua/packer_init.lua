local present, packer = pcall(require, "packer")

if not present then return end

packer.startup(function()
    use "wbthomason/packer.nvim"

    use 'shaunsingh/nord.nvim'
    use {'kyazdani42/nvim-tree.lua', requires = 'kyazdani42/nvim-web-devicons'}
    use "glepnir/dashboard-nvim"
    use {
        'famiu/feline.nvim',
        requires = {
            {
                'lewis6991/gitsigns.nvim',
                config = function() require('gitsigns').setup() end
            }
        }
    }

    use "neovim/nvim-lspconfig"
    use "ray-x/lsp_signature.nvim"
    use "williamboman/nvim-lsp-installer"
    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate',
        config = function() require'nvim-tree'.setup {} end
    }

    use {
        'w0rp/ale',
        ft = {'sh', 'zsh', 'racket'},
        cmd = 'ALEEnable',
        config = 'vim.cmd[[ALEEnable]]'
    }

    use {"rafamadriz/friendly-snippets", event = "InsertEnter"}
    use 'L3MON4D3/LuaSnip'
    use 'hrsh7th/nvim-cmp'
    use 'hrsh7th/cmp-nvim-lsp'
    use {"hrsh7th/cmp-buffer", after = "cmp-nvim-lsp"}
    use {"hrsh7th/cmp-path", after = "cmp-buffer"}
    use {"hrsh7th/cmp-nvim-lua", after = "cmp_luasnip"}
    use 'saadparwaiz1/cmp_luasnip'

    use {
        'nvim-telescope/telescope.nvim',
        requires = {
            {'nvim-telescope/telescope-fzf-native.nvim', run = 'make'},
            {'nvim-lua/popup.nvim'}, {'nvim-lua/plenary.nvim'},
            {'nvim-telescope/telescope-media-files.nvim'}
        }
    }

    use 'norcalli/nvim-colorizer.lua'
    use "terrortylor/nvim-comment"
    use 'lukas-reineke/format.nvim'
end)
