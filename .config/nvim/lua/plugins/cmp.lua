-- Set completeopt to have a better completion experience
vim.g.completeopt = "menu,menuone,noselect,noinsert"

-- Don't show the dumb matching stuff.
vim.opt.shortmess:append "c"

require("luasnip/loaders/from_vscode").load()

-- nvim-cmp setup
local cmp = require 'cmp'
local lspkind_icons = require("plugins.lspkind_icons")
local luasnip = require('luasnip')

cmp.setup {
    snippet = {expand = function(args) luasnip.lsp_expand(args.body) end},

    formatting = {
        format = function(entry, vim_item)
            -- load lspkind icons
            vim_item.kind = string.format("%s %s",
                                          lspkind_icons.icons[vim_item.kind],
                                          vim_item.kind)

            vim_item.menu = ({
                nvim_lsp = "[lsp]",
                path = "[path]",
                buffer = "[buf]",
                luasnip = "[luasnip]",
                nvim_lua = "[api]"
            })[entry.source.name]

            return vim_item
        end
    },

    mapping = {
        ['<C-p>'] = cmp.mapping.select_prev_item(),
        ['<C-n>'] = cmp.mapping.select_next_item(),
        ['<C-d>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.close(),
        ['<CR>'] = cmp.mapping.confirm {
            -- Accept currently selected item. If none selected, `select` first item.
            -- Set `select` to `false` to only confirm explicitly selected items.
            behavior = cmp.ConfirmBehavior.Insert,
            select = true
        },
        ['<Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_next_item()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end,
        ['<S-Tab>'] = function(fallback)
            if cmp.visible() then
                cmp.select_prev_item()
            elseif luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end
    },

    sources = {
        {name = 'nvim_lsp'}, {name = "path"}, {name = "buffer"},
        {name = 'luasnip'}, {name = "nvim_lua"}
    },

    experimental = {native_menu = true, ghost_text = true}
}
