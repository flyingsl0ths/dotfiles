local map = vim.api.nvim_set_keymap

vim.g.mapleader = " "

local enable_noremap = { noremap = true }
local default_options = { noremap = true, silent = true, }
local silent = { silent = true }

-- map() argument format: Mode Keymap Maps-To Options --

-- Reminders to avoid used arrow keys
map("n", "<Left>", ":echo \"Use h\"<CR>", enable_noremap)
map("n", "<Right>", ":echo \"Use l\"<CR>", enable_noremap)
map("n", "<Right>", ":echo \"Use l\"<CR>", enable_noremap)
map("n", "<Up>", ":echo \"Use k\"<CR>", enable_noremap)
map("n", "<Down>", ":echo \"Use j\"<CR>", enable_noremap)

-- Move.nvim
-- Normal-mode commands
vim.keymap.set('n', '<A-j>', ':MoveLine(1)<CR>', default_options)
vim.keymap.set('n', '<A-k>', ':MoveLine(-1)<CR>', default_options)
vim.keymap.set('n', '<A-h>', ':MoveHChar(-1)<CR>', default_options)
vim.keymap.set('n', '<A-l>', ':MoveHChar(1)<CR>', default_options)

-- Visual-mode commands
vim.keymap.set('v', '<A-j>', ':MoveBlock(1)<CR>', default_options)
vim.keymap.set('v', '<A-k>', ':MoveBlock(-1)<CR>', default_options)
vim.keymap.set('v', '<A-h>', ':MoveHBlock(-1)<CR>', default_options)
vim.keymap.set('v', '<A-l>', ':MoveHBlock(1)<CR>', default_options)

-- Pane switching
map("n", "<C-J>", "<C-w><C-j>", enable_noremap)
map("n", "<C-K>", "<C-w><C-k>", enable_noremap)
map("n", "<C-L>", "<C-w><C-l>", enable_noremap)
map("n", "<C-H>", "<C-w><C-h>", enable_noremap)

-- Pane resizing
map("n", "<C-d>", ":vertical resize +5<CR>", default_options)
map("n", "<C-a>", ":vertical resize -5<CR>", default_options)
map("n", "<C-x>", ":res +5<CR>", default_options)
map("n", "<C-s>", ":res -5<CR>", default_options)

-- Buffer movement
map("n", "<A-b>", "<C-w>r", enable_noremap)
map("n", "<A-,>", "<C-w>r", enable_noremap)
map("n", "<A-m>", "<C-w>H", enable_noremap)
map("n", "<A-n>", "<C-w>J", enable_noremap)

-- Writing files
map("n", "<C-p>", ":w<CR>", enable_noremap)
map("n", "<S-w>", ":wa<CR>", default_options)

map("n", "<C-q>", ":qa<CR>", default_options)

-- Tab switching
map("", "<C-n>", ":tabnew<space>", {})
map("", "<leader>x", ":tabclose<CR>", silent)
map("", "<S-t>", ":tabn<CR>", silent)
map("", "<S-p>", ":tabp<CR>", silent)

-- Open splits prompt
map("n", "<leader>;", ":vsplit<CR>", default_options)
map("n", "<leader>'", ":split<CR>", default_options)

-- Clears highlighted searches
map("", "<leader><CR>", ":noh<CR>", silent)

-- Toggle line numbers
map("n", "<leader>n", ":set nu! <CR>", default_options)

-- Toggle spellchecker
map("", "<leader>s", ":setlocal spell!<CR>", silent)

-- Copy/Paste
map("v", "<C-q>", "\"+y<CR>", default_options)
map("n", "<C-w>", "\"+p<CR>", default_options)

-- Terminal commands
-- escape terminal mode
map("t", "jk", "<C-\\><C-n>", {})

-- hide a term from within terminal mode
map("t", "JK", "<C-\\><C-n> :lua require('core.utils').close_buffer() <CR>", {})

-- this opens on top of an existing vert/hori term
map("n", "<leader>h",
	":execute 15 .. 'new +terminal' | let b:term_type = 'hori' | startinsert <CR>",
	default_options)

map("n", "<leader>v",
	":execute 'vnew +terminal' | let b:term_type = 'vert' | startinsert <CR>",
	default_options)

map("n", "<leader>w",
	":execute 'terminal' | let b:term_type = 'wind' | startinsert <CR>",
	default_options)

-- Buffer-Line
map('n', '<C-t>', ':BufferLineCycleNext<CR>', default_options)
map('n', '<S-TAB>', ':BufferLineCyclePrev<CR>', default_options)
map('n', '<S-x>', ':bdelete<CR>', default_options)

-- Telescope
map("n", "<leader>t", ":Telescope<CR>", default_options)
map("n", "<leader>bb", ":Telescope buffers <CR>", default_options)
map("n", "<leader>ff", ":Telescope find_files <CR>", default_options)
map("n", "<leader>fh", ":Telescope find_files hidden=true <CR>", default_options)
map("n", "<leader>fw", ":lua require('telescope').extensions.live_grep_args.live_grep_args()<CR>", default_options)
map("n", "<leader>gc", ":Telescope git_commits <CR>", default_options)
map("n", "<leader>st", ":Telescope git_status <CR>", default_options)
map("n", "<leader>ht", ":Telescope help_tags <CR>", default_options)
map("n", "<leader>lg", ":Telescope live_grep <CR>", default_options)
map("n", "<leader>of", ":Telescope oldfiles <CR>", default_options)
map("n", "<leader>bm", ":Telescope marks <CR>", default_options)
map("n", "<leader>md", ":Telescope media_files <CR>", default_options)
map("n", "<leader>ls", ":Telescope lsp_document_symbols <CR>", default_options)
map("n", "<leader>es", ":EslintFixAll <CR>", default_options)
map("n", "<leader>cs", ":Telescope commands <CR>", default_options)
map("n", "<leader>td", ":TodoTelescope <CR>", default_options)
map("n", "<leader>kk", ":Telescope keymaps <CR>", default_options)
map('n', '<leader>fr', ":Telescope lsp_references <CR>", default_options)
map('n', '<leader>ch', ":Telescope command_history <CR>", default_options)
map("n", "<leader>W", ":Telescope terms <CR>", default_options)
map("n", "<leader>bf", ":Telescope current_buffer_fuzzy_find <CR>", default_options)
map("n", "<leader>uc", ":Gitsigns preview_hunk <CR>", default_options)
map('n', '<C-e>', "<cmd>lua vim.diagnostic.open_float()<CR>", default_options)
map('n', '<leader>ld', "<cmd>:Telescope diagnostics<CR>", default_options)
map("n", "<leader>cd", ":Telescope zoxide list <CR>", default_options)

-- Dap
map("n", "<F4>", ":lua require('osv').run_this()<CR>", default_options)
map("n", "<leader>dc", ":lua require('dap').continue()<CR>", default_options)
map("n", "<leader>ds", ":lua require('dap').step_over()<CR>", default_options)
map("n", "<leader>di", ":lua require('dap').step_into()<CR>", default_options)
map("n", "<leader>do", ":lua require('dap').step_out()<CR>", default_options)
map("n", "<leader>b", ":lua require('dap').toggle_breakpoint()<CR>", default_options)
map("n", "<leader>B", ":lua require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>", default_options)
map("n", "<leader>lp", ":lua require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>",
	default_options)
map("n", "<leader>dt", ":lua require('dap').terminate()<CR>", default_options)

map("n", "<leader>du", ":lua require('dapui').toggle()<CR>", default_options)

map("n", "<leader>d", ":NvimTreeToggle<CR>", { noremap = true, silent = true, })

map("n", "<leader>ss", ":SymbolsOutline<CR>", default_options)
map("n", "<leader>sr", ":source %<CR>", default_options)
map("n", "<leader>lz", ":Lazy <CR>", default_options)
