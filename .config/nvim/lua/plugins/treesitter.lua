local present, ts_config = pcall(require, "nvim-treesitter.configs")

if not present then
    ts_config.setup {
        ensure_installed = {"lua"},
        highlight = {enable = true, use_languagetree = true}
    }
end
