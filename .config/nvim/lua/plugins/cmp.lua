-- Set completeopt to have a better completion experience
vim.g.completeopt = "menu,menuone,noselect,noinsert"

-- Don't show the dumb matching stuff.
vim.opt.shortmess:append "c"


local cmp = require 'cmp'
local lspkind_icons = require("plugins.lspkind_icons")
local luasnip = require('luasnip')

require("luasnip.loaders.from_vscode").lazy_load()
require('cmp-npm').setup({})


cmp.setup {
	view = {
		entries = "native" -- can be "custom", "wildmenu" or "native"
	},

	snippet = { expand = function(args) luasnip.lsp_expand(args.body) end },

	formatting = {
		format = function(entry, vim_item)
			-- load lspkind icons
			vim_item.kind = string.format("%s %s", lspkind_icons.icons[vim_item.kind],
				vim_item.kind)

			-- Source
			vim_item.menu = ({
				buffer = "[Buffer]",
				path = "[Path]",
				nvim_lsp = "[LSP]",
				luasnip = "[LuaSnip]",
				nvim_lua = "[Lua]",
				latex_symbols = "[LaTeX]",
				npm = "[npm]",
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
		{ name = 'luasnip' },
		{ name = 'nvim_lsp' },
		{ name = "nvim_lua" },
		{ name = "buffer" },
		{ name = "buffer-lines" },
		{ name = "crates" },
		{ name = "crates" },
		{ name = 'npm',         keyword_length = 4 },
		{ name = "path" },
		{ name = "calc" },
		{ name = "emoji" },
		{ name = "nerdfont" }
	}

}
