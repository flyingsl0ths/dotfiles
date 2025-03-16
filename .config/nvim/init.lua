-- SETTINGS
require "core.autocmds"
require "core.preferences"
require "mappings.keybindings"
--

-- PLUGIN INIT
require "plugins"
--

-- "GUI"
require "plugins.theme"
require "plugins.buffline"
require "plugins.statusline"
--

--- EDITOR
--   DEBUG
require "plugins.dap"
require "plugins.dapui"
--

require "plugins.colorizr"

--   LSP
require "plugins.icons"
require "plugins.cmp"
require "plugins.lsp_sig"
require "plugins.lsp"
--

--   LSP-TOOLS
require "plugins.treesitter"
require "plugins.tag_bar"
--

--   TOOLS
require "plugins.nvim_tree"
--
---

-- GUI-TOOLS
require "plugins.tlscope"
--
