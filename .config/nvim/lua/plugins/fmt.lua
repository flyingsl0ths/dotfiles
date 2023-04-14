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
		python = {
			-- Configuration for psf/black
			function()
				return {
					exe = "black",
					-- this should be available on your $PATH
					args = {
						"--stdin-filename",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
					},
				}
			end
		},
		nix = {
			function()
				return {
					exe = "nixpkgs-fmt",
					stdin = true,
				}
			end
		},
		haskell = {
			function()
				return {
					exe = "fourmolu",
					args = {
						"-m",
						"stdout",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
					},
					stdin = true,
				}
			end
		},
	}
})

vim.api.nvim_exec([[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.lua,*.sh,*.html,*.xhtml,*.css,*scss,*.md,*.yaml,*.js,*.ts,*.jsx,*.tsx,*.json,*.py,*.nix,*.cabal,*.hs FormatWrite
augroup END
]], true)
