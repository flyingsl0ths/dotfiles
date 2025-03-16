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

vim.api.nvim_create_autocmd("BufWritePost", {
	pattern = { "*.cabal" },
	group = vim.api.nvim_create_augroup("FormatAutogroup", { clear = true }),
	desc = "Auto format cabal files",
	callback = function()
		local buffer_name = vim.api.nvim_buf_get_name(0)
		vim.cmd(":silent write !cabal-fmt -i " .. buffer_name)
	end,
})

vim.api.nvim_create_autocmd("BufWritePost", {
	pattern = { "*.nix" },
	group = vim.api.nvim_create_augroup("FormatAutogroup", { clear = true }),
	desc = "Auto format cabal files",
	callback = function()
		local buffer_name = vim.api.nvim_buf_get_name(0)
		vim.cmd(":silent write !nixfmt " .. buffer_name)
	end,
})

vim.api.nvim_create_autocmd("BufWritePost", {
	pattern = { "*.sh", "Dockerfile" },
	group = vim.api.nvim_create_augroup("FormatAutogroup", { clear = true }),
	callback = function()
		local buffer_name = vim.api.nvim_buf_get_name(0)
		vim.cmd(":silent write !shfmt -i " .. buffer_name)
	end,
})
