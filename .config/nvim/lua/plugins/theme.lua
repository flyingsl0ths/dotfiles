-- local vim_globals = vim.g
--
-- vim_globals.nord_contrast = true
-- vim_globals.nord_borders = true
-- vim_globals.nord_disable_background = true
-- vim_globals.nord_italic = true
-- vim_globals.nord_enable_sidebar_background = true
--
-- require('nord').set()
local catppuccin = require("catppuccin")

catppuccin.setup({ transparent_background = false })
vim.cmd [[colorscheme catppuccin]]

-- local base16 = require 'base16'
-- base16(base16.themes["nord"], true)
