local lspconfig = require "lspconfig"


vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, { border = "single" })

vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, { border = "single" })

local signs = {
	ERROR = '',
	WARN = '',
	HINT = '',
	INFO = '',
}

for _, type in ipairs({ "Error", "Warn", "Info", "Hint" }) do
	local hl = "DiagnosticSign" .. type
	local hl2 = "Diagnostic" .. type
	vim.fn.sign_define(hl, { text = signs[type:upper()], texthl = hl2, numhl = hl2 })
end

vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
	vim.lsp.diagnostic.on_publish_diagnostics,
	{
		virtual_text = false,
		signs = true,
		update_in_insert = false,
		underline = true,
	}
)

local capabilities = vim.lsp.protocol.make_client_capabilities()

capabilities.textDocument.completion.completionItem.snippetSupport = true

lspconfig.emmet_ls.setup {
	capabilities = capabilities,

	root_dir = function() return vim.loop.cwd() end,
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

capabilities = require('cmp_nvim_lsp').default_capabilities(capabilities)

local lsp_utils = require "conf_utils.lsp"

local servers = lsp_utils.servers

for _, lsp in pairs(servers) do
	local server = lspconfig[lsp]

	local opts = {
		on_attach    = lsp_utils.on_attach,
		capabilities = capabilities
	}


	if server.name == "clangd" then
		lsp_utils.configure_clangd(opts)
	elseif server.name == "denols" then
		lsp_utils.configure_denols(opts)
	elseif server.name == "ts_ls" then
		lsp_utils.configure_ts_ls(opts)
	elseif server.name == "hls" then
		lsp_utils.configure_hls(opts)
	elseif server.name == "purescriptls" then
		lsp_utils.configure_purescriptls(opts)
	elseif server.name == "tailwindcss" then
		lsp_utils.configure_tailwindcss(server, opts)
	elseif server.name == "sourcekit" then
		lsp_utils.configure_sourcekit(opts)
	end

	server.setup(opts)
end
