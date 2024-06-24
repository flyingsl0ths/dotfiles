local prettier =
{
	function()
		return {
			exe = "prettier",
			args = {
				"--stdin-filepath",
				vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
			},
			stdin = true
		}
	end
}

local shfmt = {
	-- Shell Script Formatter
	function() return { exe = "shfmt", args = { "-i", 2 }, stdin = true } end
}

require('formatter').setup({
	filetype = {
		zsh = shfmt,
		sh = shfmt,
		dockerfile = shfmt,

		html = prettier,
		xhtml = prettier,
		scss = prettier,
		css = prettier,
		markdown = prettier,
		json = prettier,
		javascriptreact = prettier,
		typescriptreact = prettier,
		yaml = prettier,
		javascript = prettier,
		typescript = prettier,

		cabal = {
			-- Shell Script Formatter
			function()
				return {
					exe = "cabal-fmt",
					args = {
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
					},
					stdin = true
				}
			end
		},

		swift = {
			function()
				return {
					exe = "swift-format",
					args = {
						"format",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
					},
					stdin = true
				}
			end
		},

		cmake = {
			function()
				return {
					exe = "cmake-format",
					args = {
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
					},
					stdin = true
				}
			end
		},

		-- python = {
		-- 	function()
		-- 		return {
		-- 			exe = "autopep8",
		-- 			args = {
		-- 				vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
		-- 			},
		-- 			stdin = true
		-- 		}
		-- 	end
		-- },
		nix = {
			function()
				return {
					exe = "nixpkgs-fmt",
					stdin = true,
				}
			end
		},
		-- haskell = {
		-- 	function()
		-- 		return {
		-- 			exe = "fourmolu",
		-- 			args = {
		-- 				"-m",
		-- 				"stdout",
		-- 				vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
		-- 			},
		-- 			stdin = true,
		-- 		}
		-- 	end
		-- },
	}
})

vim.api.nvim_exec([[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.lua,Dockerfile,*.sh,*.html,*.cmake,*.xhtml,*.css,*scss,*.swift,*.md,*.yaml,*.js,*.ts,*.jsx,*.tsx,*.json,*.nix,*.cabal FormatWrite
augroup END
]], true)
