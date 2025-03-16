-- Don't show any numbers inside terminals
-- vim.cmd " au TermOpen term://* setlocal nonumber norelativenumber | setfiletype terminal "

vim.api.nvim_create_autocmd("TermOpen", {
	pattern = { "term://*" },
	callback = function()
		vim.cmd(":setlocal nonumber norelativenumber")
		vim.bo.filetype = "terminal"
	end,
})

vim.cmd " au BufNewFile,BufRead *.ejs set filetype=html "

vim.api.nvim_create_autocmd({ "BufNewFile", "BufRead" }, {
	pattern = { "*.ejs" },
	callback = function()
		vim.bo.filetype = "html"
	end,
})

vim.api.nvim_create_autocmd({ "BufNew", "BufRead" }, {
	pattern = { "*.cabal" },
	callback = function()
		vim.bo.filetype = "cabal"
	end,
})
