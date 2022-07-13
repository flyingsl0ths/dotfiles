-- Individual Server Configs
local lspconfig = require 'lspconfig'

-- replace the default lsp diagnostic symbols
local function lsp_symbol(name, icon)
	vim.fn.sign_define("LspDiagnosticsSign" .. name,
		{ text = icon, numhl = "LspDiagnosticsDefault" .. name })
end

lsp_symbol("Error", "")
lsp_symbol("Information", "")
lsp_symbol("Hint", "")
lsp_symbol("Warning", "")

vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" })

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "single" })


local function on_attach(_, bufnr)
	local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

	local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

	-- Enable completion triggered by <c-x><c-o>
	buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")

	-- Mappings.
	local opts = { noremap = true, silent = true }

	-- See `:help vim.lsp.*` for documentation on any of the below functions
	buf_set_keymap("n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)

	buf_set_keymap("n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)

	buf_set_keymap("n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)

	buf_set_keymap("n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", opts)

	buf_set_keymap("n", "gk", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)

	buf_set_keymap("n", "<space>wa",
		"<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>", opts)

	buf_set_keymap("n", "<space>wr",
		"<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>", opts)

	buf_set_keymap("n", "<space>wl",
		"<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>",
		opts)

	buf_set_keymap("n", "<space>D", "<cmd>lua vim.lsp.buf.type_definition()<CR>",
		opts)

	buf_set_keymap("n", "<space>rn", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)

	buf_set_keymap("n", "<space>ca", "<cmd>lua vim.lsp.buf.code_action()<CR>",
		opts)

	buf_set_keymap("n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", opts)

	buf_set_keymap("n", "ge",
		"<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)

	buf_set_keymap("n", "[d", "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>", opts)

	buf_set_keymap("n", "]d", "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>", opts)

	buf_set_keymap("n", "<space>q",
		"<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", opts)

	buf_set_keymap("n", "<space>fm", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)

	buf_set_keymap("v", "<space>ca",
		"<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
end

local capabilities = vim.lsp.protocol.make_client_capabilities()

capabilities.textDocument.completion.completionItem.snippetSupport = true

lspconfig.emmet_ls.setup {
	capabilities = capabilities,

	filetypes = {
		'html', 'css', 'scss', 'javascript', 'javascriptreact', 'typescript',
		'typescriptreact', 'haml', 'xml', 'xsl', 'pug', 'slim', 'sass',
		'stylus', 'less', 'sss'
	},
	root_dir = function(fname) return vim.loop.cwd() end,
}

capabilities.textDocument.completion.completionItem.documentationFormat = {
	"markdown", "plaintext"
}

capabilities.textDocument.completion.completionItem.snippetSupport = true
capabilities.textDocument.completion.completionItem.preselectSupport = true
capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
capabilities.textDocument.completion.completionItem.deprecatedSupport = true
capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
capabilities.textDocument.completion.completionItem.tagSupport = {
	valueSet = { 1 }
}
capabilities.textDocument.completion.completionItem.resolveSupport = {
	properties = { "documentation", "detail", "additionalTextEdits" }
}

capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

local servers = {
	"bashls",
	"clangd",
	"cmake",
	"cssls",
	"groovyls",
	"hls",
	"html",
	"cssls",
	"jdtls",
	"jsonls",
	"kotlin_language_server",
	"pyright",
	"rust_analyzer",
	"sumneko_lua",
	"taplo",
	"tsserver",
}

local function configure_hls(opts)
	opts.settings = {
		haskell = {
			formattingProvider = "fourmolu",
			plugin = {
				hlint = {
					diagnosticsOn = true
				}
			}
		}
	}
end

local function configure_rust_lsp(opts)
	opts.settings = {
		["rust-analyzer"] = {
			assist = {
				importGranularity = "module",
				importPrefix = "self",
			},
			cargo = {
				loadOutDirsFromCheck = true
			},
			procMacro = {
				enable = true
			},
		}
	}

end

for _, lsp in pairs(servers) do
	local server = lspconfig[lsp]
	local opts = {
		on_attach    = on_attach,
		capabilities = capabilities
	}

	if server.name == "hls" then
		configure_hls(opts)
	elseif server.name == "rust_analyzer" then
		configure_rust_lsp(opts)

	elseif server.name == "jdtls" then
		opts.cmd = {
			"jdtls"
		}
	end

	server.setup(opts)
end
