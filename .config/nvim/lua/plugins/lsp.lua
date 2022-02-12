-- replace the default lsp diagnostic symbols
local function lspSymbol(name, icon)
    vim.fn.sign_define("LspDiagnosticsSign" .. name,
                       {text = icon, numhl = "LspDiagnosticsDefault" .. name})
end

lspSymbol("Error", "")
lspSymbol("Information", "")
lspSymbol("Hint", "")
lspSymbol("Warning", "")

vim.lsp.handlers["textDocument/hover"] =
    vim.lsp.with(vim.lsp.handlers.hover, {border = "single"})

vim.lsp.handlers["textDocument/signatureHelp"] =
    vim.lsp.with(vim.lsp.handlers.signature_help, {border = "single"})

-- Individual Server Configs
local lspconfig = require 'lspconfig'

lspconfig.racket_langserver.setup {}
