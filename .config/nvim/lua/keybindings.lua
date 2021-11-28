local map = vim.api.nvim_set_keymap

vim.g.mapleader = " "

local enable_noremap = {noremap = true}
local default_options = {noremap = true, silent = true}

-- Mode  Keymap Mapps to Options 

-- Reminders to avoid used arrow keys
map("n", "<Left>", ":echo \"Use h\"<CR>", enable_noremap)
map("n", "<Right>", ":echo \"Use l\"<CR>", enable_noremap)
map("n", "<Right>", ":echo \"Use l\"<CR>", enable_noremap)
map("n", "<Up>", ":echo \"Use k\"<CR>", enable_noremap)
map("n", "<Down>", ":echo \"Use j\"<CR>", enable_noremap)

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
map("n", "<S-Left>", "<C-w>r", enable_noremap)
map("n", "<S-Right>", "<C-w>r", enable_noremap)
map("n", "<S-Up>", "<C-w>H", enable_noremap)
map("n", "<S-Down>", "<C-w>J", enable_noremap)

-- Buffer switching
map("n", "<Tab>", ":bnext<CR>", enable_noremap)
map("n", "<S-Tab>", ":bprevious<CR>", enable_noremap)

-- Writing files
map("n", "<C-w>", ":w<CR>", enable_noremap)

-- Tab switching
map("", "<C-t>", ":tabnew<space>", {})
map("", "<leader>x", ":tabclose<CR>", {})
map("", "<S-t>", ":tabn<CR>", {})
map("", "<S-p>", ":tabp<CR>", {})

-- Open splits prompt
map("n", "<leader>-", ":vsplit<space>", enable_noremap)
map("n", "<leader>|", ":split<space>", enable_noremap)

-- Clears highlighted searches
map("", "<SPACE><CR>", ":noh<CR>", {})

-- Toggle line numbers
map("n", "<leader>n", ":set nu! <CR>", default_options)

-- Terminal commands
-- escape terminal mode
map("t", "jk", "<C-\\><C-n>", {})

-- pick a hidden terminal
map("n", "<leader>W", ":Telescope terms <CR>", default_options)

-- Open terminals
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

-- Dashboard bindings
map("n", "<leader>d", ":Dashboard <CR>", default_options)
map("n", "<leader>dj", ":DashboardJumpMarks <CR>", default_options)
map("n", "<leader>dn", ":DashboardNewFile <CR>", default_options)
map("n", "<leader>ds", ":SessionSave <CR>", default_options)
map("n", "<leader>dl", ":SessionLoad <CR>", default_options)

-- Telescope bindings
map("n", "<leader>t", ":Telescope<CR>", default_options)
map("n", "<leader>b", ":Telescope buffers <CR>", default_options)
map("n", "<leader>ff", ":Telescope find_files <CR>", default_options)
map("n", "<leader>fh", ":Telescope find_files hidden=true <CR>", default_options)
map("n", "<leader>gc", ":Telescope git_commits <CR>", default_options)
map("n", "<leader>st", ":Telescope git_status <CR>", default_options)
map("n", "<leader>ht", ":Telescope help_tags <CR>", default_options)
map("n", "<leader>lg", ":Telescope live_grep <CR>", default_options)
map("n", "<leader>of", ":Telescope oldfiles <CR>", default_options)
map("n", "<leader>bm", ":Telescope marks <CR>", default_options)
map("n", "<leader>ld", ":Telescope lsp_document_diagnostics <CR>",
    default_options)

-- NvimTree bindings
map("n", "<C-e>", ":NvimTreeToggle<CR>", enable_noremap)
map("n", "<C-f>", ":NvimTreeFocus <CR>", enable_noremap)

-- Nvim-Commment bindings
map("n", "<leader>/", ":CommentToggle <CR>", enable_noremap)
