vim.g.mapleader = " " -- Set leader key

-- Common options
local default_options = { noremap = true, silent = true }
local silent_options = { silent = true }

-- Avoid arrow keys
vim.keymap.set("n", "<Left>", ":echo 'Use h'<CR>", default_options)
vim.keymap.set("n", "<Right>", ":echo 'Use l'<CR>", default_options)
vim.keymap.set("n", "<Up>", ":echo 'Use k'<CR>", default_options)
vim.keymap.set("n", "<Down>", ":echo 'Use j'<CR>", default_options)

-- Cursor
vim.keymap.set("n", "<leader>hc", "*<CR>", silent_options)
vim.keymap.set("n", "<leader><CR>", ":noh<CR>", silent_options)
vim.keymap.set("n", "<leader>sc", '"syiw/<C-r>s<CR>', default_options)

-- Substitution
vim.api.nvim_set_keymap('x', '<leader>sr', '"sy:\'<,\'>s/<C-r>s//g<Left><Left>',
	{ noremap = true, silent = true })

vim.keymap.set("v", "<leader>r", '"sy:\'<,\'>s/<C-r>s//<Left>', { noremap = true })
vim.keymap.set("v", "<leader>gr", '"sy:%s/<C-r>s//<Left>', { noremap = true })

-- Selection
vim.keymap.set("v", "<S-f>", '"sy/<C-r>s<CR>', { noremap = true })

-- Move.nvim
vim.keymap.set("n", "<A-j>", ":MoveLine(1)<CR>", default_options)
vim.keymap.set("n", "<A-k>", ":MoveLine(-1)<CR>", default_options)
vim.keymap.set("n", "<A-h>", ":MoveHChar(-1)<CR>", default_options)
vim.keymap.set("n", "<A-l>", ":MoveHChar(1)<CR>", default_options)
vim.keymap.set("v", "<A-j>", ":MoveBlock(1)<CR>", default_options)
vim.keymap.set("v", "<A-k>", ":MoveBlock(-1)<CR>", default_options)
vim.keymap.set("v", "<A-h>", ":MoveHBlock(-1)<CR>", default_options)
vim.keymap.set("v", "<A-l>", ":MoveHBlock(1)<CR>", default_options)

-- Pane switching
vim.keymap.set("n", "<C-J>", "<C-w>j", default_options)
vim.keymap.set("n", "<C-K>", "<C-w>k", default_options)
vim.keymap.set("n", "<C-L>", "<C-w>l", default_options)
vim.keymap.set("n", "<C-H>", "<C-w>h", default_options)

-- Pane resizing
vim.keymap.set("n", "<C-d>", ":vertical resize +5<CR>", default_options)
vim.keymap.set("n", "<C-a>", ":vertical resize -5<CR>", default_options)
vim.keymap.set("n", "<C-x>", ":resize +5<CR>", default_options)
vim.keymap.set("n", "<C-s>", ":resize -5<CR>", default_options)

-- Writing files
vim.keymap.set("n", "<C-p>", ":w<CR>", default_options)
vim.keymap.set("n", "<S-w>", ":wa<CR>", default_options)

-- Closing files and tabs
vim.keymap.set("n", "<C-q>", ":qa<CR>", default_options)
vim.keymap.set("n", "<leader>x", ":tabclose<CR>", default_options)

-- Tab navigation
vim.keymap.set("n", "<C-n>", ":tabnew<space>", {})
vim.keymap.set("n", "<S-t>", ":tabn<CR>", default_options)
vim.keymap.set("n", "<S-p>", ":tabp<CR>", default_options)

-- Splits
vim.keymap.set("n", "<leader>;", ":vsplit<CR>", default_options)
vim.keymap.set("n", "<leader>'", ":split<CR>", default_options)

-- Toggle line numbers
vim.keymap.set("n", "<leader>n", ":set nu!<CR>", default_options)

-- Toggle spellchecker
vim.keymap.set("n", "<leader>sp", ":setlocal spell!<CR>", default_options)

-- Copy/Paste
vim.keymap.set({ "n", "v" }, "<A-q>", '"+y', default_options)
vim.keymap.set("n", "<A-w>", '"+p', default_options)


-- Terminal commands
vim.keymap.set("t", "jk", "<C-\\><C-n>", default_options)
vim.keymap.set("t", "JK", "<C-\\><C-n> :lua require('core.utils').close_buffer()<CR>", default_options)
vim.keymap.set("n", "<leader>h", ":execute 15 .. 'new +terminal' | let b:term_type = 'hori' | startinsert<CR>",
	default_options)
vim.keymap.set("n", "<leader>v", ":execute 'vnew +terminal' | let b:term_type = 'vert' | startinsert<CR>",
	default_options)
vim.keymap.set("n", "<leader>w", ":execute 'terminal' | let b:term_type = 'wind' | startinsert<CR>", default_options)

-- BufferLine
vim.keymap.set("n", "<C-t>", ":BufferLineCycleNext<CR>", default_options)
vim.keymap.set("n", "<S-TAB>", ":BufferLineCyclePrev<CR>", default_options)
vim.keymap.set("n", "<S-x>", ":bdelete<CR>", default_options)

-- Telescope
vim.keymap.set("n", "<leader>t", ":Telescope<CR>", default_options)
vim.keymap.set("n", "<leader>ff", ":Telescope find_files<CR>", default_options)
vim.keymap.set("n", "<leader>lg", ":Telescope live_grep<CR>", default_options)
vim.keymap.set("n", "<leader>ht", ":Telescope help_tags<CR>", default_options)
vim.keymap.set("n", "<leader>b", ":Telescope buffers<CR>", default_options)
vim.keymap.set("n", "<leader>kk", ":Telescope keymaps<CR>", default_options)
vim.keymap.set("n", "<leader>sh", ":Telescope search_history<CR>", default_options)
vim.keymap.set("n", "<leader>zl", ":Telescope zoxide list<CR>", default_options)

-- Telescope:Git
vim.keymap.set("n", "<leader>cs", ":Telescope git_commits<CR>", default_options)
vim.keymap.set("n", "<leader>br", ":Telescope git_branches<CR>", default_options)

-- Telescope:Lsp
vim.keymap.set("n", "<C-e>", "<cmd>lua vim.diagnostic.open_float()<CR>", default_options)
vim.keymap.set("n", "<leader>ld", ":Telescope diagnostics<CR>", default_options)
vim.keymap.set("n", "<leader>ls", ":Telescope treesitter<CR>", default_options)

-- Debugging (DAP)
vim.keymap.set("n", "<F4>", ":lua require('osv').run_this()<CR>", default_options)
vim.keymap.set("n", "<leader>dc", ":lua require('dap').continue()<CR>", default_options)
vim.keymap.set("n", "<leader>dt", ":lua require('dap').terminate()<CR>", default_options)

-- Plugin shortcuts
vim.keymap.set("n", "<leader>lz", ":Lazy<CR>", default_options)
vim.keymap.set("n", "<leader>d", ":NvimTreeToggle<CR>", default_options)
vim.keymap.set("n", "<leader>sr", ":source %<CR>", default_options)
vim.keymap.set("n", "<leader>ss", ":SymbolsOutline<CR>", default_options)
vim.keymap.set("n", "<leader>cu", ":source $MYVIMRC<CR>", default_options)

-- Etc
vim.keymap.set("n", "<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })
