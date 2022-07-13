require('formatter').setup({
	filetype = {
		zsh = {
			-- Shell Script Formatter
			function() return { exe = "shfmt", args = { "-i", 2 }, stdin = true } end
		},

		sh = {
			-- Shell Script Formatter
			function() return { exe = "shfmt", args = { "-i", 2 }, stdin = true } end
		},

		html = {
			-- prettier
			function()
				return {
					exe = "prettier",
					args = {
						"--stdin-filepath",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)), '--single-quote'
					},
					stdin = true
				}
			end
		},

		xhtml = {
			-- prettier
			function()
				return {
					exe = "prettier",
					args = {
						"--stdin-filepath",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)), '--single-quote'
					},
					stdin = true
				}
			end
		},

		scss = {
			-- prettier
			function()
				return {
					exe = "prettier",
					args = {
						"--stdin-filepath",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)), '--single-quote'
					},
					stdin = true
				}
			end
		},

		css = {
			-- prettier
			function()
				return {
					exe = "prettier",
					args = {
						"--stdin-filepath",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)), '--single-quote'
					},
					stdin = true
				}
			end
		},

		markdown = {
			-- prettier
			function()
				return {
					exe = "prettier",
					args = {
						"--stdin-filepath",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)), '--single-quote'
					},
					stdin = true
				}
			end
		},

		yaml = {
			-- prettier
			function()
				return {
					exe = "prettier",
					args = {
						"--stdin-filepath",
						vim.fn.fnameescape(vim.api.nvim_buf_get_name(0)), '--single-quote'
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
					args = { '-' },
					stdin = true
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
		}
	}
})

vim.api.nvim_exec([[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.lua,*.sh,*.html,*.xhtml,*.css,*scss,*.md,*.yaml,*.py,*.nix FormatWrite
augroup END
]], true)
