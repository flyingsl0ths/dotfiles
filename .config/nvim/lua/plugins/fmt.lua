require('formatter').setup({
  filetype = {
    lua = {
      -- lua-format
      function()
        return {
          exe = "lua-format",
          args = {
            "--indent-width", 2, "-i",
            vim.fn.fnameescape(vim.api.nvim_buf_get_name(0))
          },
          stdin = false
        }
      end
    },

    zsh = {
      -- Shell Script Formatter
      function() return {exe = "shfmt", args = {"-i", 2}, stdin = true} end
    },

    sh = {
      -- Shell Script Formatter
      function() return {exe = "shfmt", args = {"-i", 2}, stdin = true} end
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
          args = {'-'},
          stdin = true
        }
      end
    }
  }
})

vim.api.nvim_exec([[
augroup FormatAutogroup
  autocmd!
  autocmd BufWritePost *.lua,*.sh,*.html,*.css,*.md,*.yaml,*.py FormatWrite
augroup END
]], true)
