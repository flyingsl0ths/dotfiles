local present, nvimtree = pcall(require, "nvim-tree")

if not present then
    local vim_globals = vim.g

    vim.o.termguicolors = true

    vim_globals.nvim_tree_add_trailing = 0 -- append a trailing slash to folder names
    vim_globals.nvim_tree_highlight_opened_files = 0
    vim_globals.nvim_tree_indent_markers = 1
    vim_globals.nvim_tree_quit_on_open = 0 -- closes tree when file's opened
    vim_globals.nvim_tree_root_folder_modifier = table.concat {
        ":t:gs?$?/..", string.rep(" ", 1000), "?:gs?^??"
    }
    vim_globals.nvim_tree_window_picker_exclude = {
        filetype = {'notify', 'packer', 'qf'},
        buftype = {'terminal'}
    }

    --
    vim_globals.nvim_tree_show_icons = {
        folders = 1,
        -- folder_arrows= 1
        files = 1
    }

    vim_globals.nvim_tree_icons = {
        default = "",
        symlink = "",
        git = {
            deleted = "",
            ignored = "◌",
            renamed = "➜",
            staged = "✓",
            unmerged = "",
            unstaged = "✗",
            untracked = "★"
        },
        folder = {
            -- disable indent_markers option to get arrows working or if you want both arrows and indent then just add the arrow icons in front            ofthe default and opened folders below!
            -- arrow_open = "",
            -- arrow_closed = "",
            default = "",
            empty = "", -- 
            empty_open = "",
            open = "",
            symlink = "",
            symlink_open = ""
        }
    }

    nvimtree.setup {
        nvim_tree_gitignore = 0,
        nvim_tree_ignore = {".git", "node_modules", ".cache"},
        diagnostics = {
            enable = false,
            icons = {hint = "", info = "", warning = "", error = ""}
        },
        filters = {dotfiles = false},
        disable_netrw = true,
        hijack_netrw = true,
        ignore_ft_on_setup = {"dashboard"},
        auto_close = false,
        open_on_tab = false,
        hijack_cursor = true,
        update_cwd = true,
        update_focused_file = {enable = true, update_cwd = false},
        view = {allow_resize = true, side = "left", width = 25}
    }
end
