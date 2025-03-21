return {
	on_attach = function(_, bufnr)
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

		buf_set_keymap("n", "<space>fm", "<cmd>lua vim.lsp.buf.format { async = true }<CR>", opts)

		buf_set_keymap("v", "<space>ca",
			"<cmd>lua vim.lsp.buf.range_code_action()<CR>", opts)
	end,

	servers = {
		"bashls",
		"biome",
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
		"rust_analyzer",
		"sourcekit",
		"tailwindcss",
		"ts_ls",
		"zls",
	},
	configure_clangd = function(opts)
		opts.cmd = { "clangd", "--clang-tidy" }
	end,

	configure_tailwindcss = function(server, opts)
		opts.filetypes = server.config_def.default_config.filetypes

		local filetypes = {}

		for _, filetype in ipairs(opts.filetypes) do
			if not (filetype == "javascript" or filetype == "typescript") then
				table.insert(filetypes, filetype)
			end
		end

		opts.filetypes = filetypes
	end,

	configure_sourcekit = function(opts)
		opts.filetypes = { "swift", "objc", "objcpp" }
	end,

	configure_purescriptls = function(opts)
		opts.settings = { purescript = { formatter = "purs-tidy" } }
	end,

	configure_hls = function(opts)
		opts.settings = {
			haskell = {
				formattingProvider = "stylish-haskell",
				plugin = {
					hlint = {
						diagnosticsOn = true
					}
				}
			}
		}
	end,
}
