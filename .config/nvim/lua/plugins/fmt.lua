require"format".setup {
    lua = {
        {
            cmd = {
                function(file)
                    return string.format("lua-format -i %s", file)
                end
            }
        }
    },

    sh = {
        {cmd = {function(file)
            return string.format("shfmt -i 4 %s", file)
        end}}
    },

    markdown = {
        {cmd = {"prettier -w"}}, {
            cmd = {"black"},
            start_pattern = "^```python$",
            end_pattern = "^```$",
            target = "current"
        }
    },

    html = {{cmd = {"prettier -w"}}},

    css = {{cmd = {"prettier -w"}}}
}

vim.cmd [[
    augroup Format
        autocmd!
        autocmd BufWritePost * FormatWrite
    augroup END
]]
