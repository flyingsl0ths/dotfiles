local present, icons = pcall(require, "nvim-web-devicons")

if present then
	icons.setup {
		default = true,
		override = {
			html = {
				icon = "",
				color = "#D08770",
				cterm_color = "173",
				name = "html"
			},

			css = { icon = "", color = "#5E81AC", cterm_color = "67", name = "css" },

			md = {
				icon = "",
				color = "#D8DEE9",
				cterm_color = "188",
				name = "markdown"
			},

			py = {
				icon = "",
				color = "#EBCB8B",
				cterm_color = "222",
				name = "python"
			}
		}
	}
end
