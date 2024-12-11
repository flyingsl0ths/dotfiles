-- Individual Server Configs
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

	filetypes = {
		'html', 'css', 'scss', 'javascript', 'javascriptreact', 'typescript',
		'typescriptreact', 'haml', 'xml', 'xsl', 'pug', 'slim', 'sass',
		'stylus', 'less', 'sss'
	},
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

local servers = {
	"bashls",
	"clangd",
	"cmake",
	"csharp_ls",
	"cssls",
	"denols",
	"docker_compose_language_service",
	"dockerls",
	"eslint",
	"gopls",
	"groovyls",
	"hls",
	"html",
	"jdtls",
	"jsonls",
	"kotlin_language_server",
	"lua_ls",
	"ocamllsp",
	"purescriptls",
	"pylsp",
	"ruff",
	-- "rust_analyzer",
	"sourcekit",
	"tailwindcss",
	"ts_ls",
	"zls",
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

local function configure_jdtls(opts)
	opts.cmd = { "jdtls" }
	opts.root_dir = function(fname)
		return require 'lspconfig'.util.root_pattern(
			'build.xml', -- Ant
			'pom.xml', -- Maven
			'settings.gradle', -- Gradle
			'settings.gradle.kts', -- Gradle
			-- Multi-module projects
			'build.gradle',
			'build.gradle.kts',
			".git"
		)(fname) or vim.fn.getcwd()
	end
end

local lsp_utils = require "conf_utils.lsp_utils"

for _, lsp in pairs(servers) do
	local server = lspconfig[lsp]
	local opts = {
		on_attach    = lsp_utils.on_attach,
		capabilities = capabilities
	}

	if server.name == "hls" then
		configure_hls(opts)
	elseif server.name == "jdtls" then
		configure_jdtls(opts)
	elseif server.name == "denols" then
		opts.root_dir = function(fname)
			return require 'lspconfig'.util.root_pattern(
				"deno.json", "deno.jsonc"
			)(fname)
		end
	elseif server.name == "tsserver" then
		opts.root_dir = function(fname)
			return require 'lspconfig'.util.root_pattern(
				"package.json", "tsconfig.json", "jsconfig.json"
			)(fname)
		end
	elseif server.name == "sourcekit" then
		opts.filetypes = { "swift", "objective-c", "objective-cpp" }
	end

	server.setup(opts)
end
