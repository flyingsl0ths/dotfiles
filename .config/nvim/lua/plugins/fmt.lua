local shfmt = {
	-- Shell Script Formatter
	function() return { exe = "shfmt", args = { "-i", 2 }, stdin = true } end
}

require('formatter').setup({
	filetype = {
		zsh = shfmt,
		sh = shfmt,
		dockerfile = shfmt,

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
  autocmd BufWritePost *.lua,Dockerfile,*.sh,*.cmake,*.swift,*.md,*.nix,*.cabal FormatWrite
augroup END
]], true)
