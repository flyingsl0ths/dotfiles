local autopairs_present, cmp_autopairs = pcall(require,
	"nvim-autopairs.completion.cmp")

local cmp_present, cmp = pcall(require, "cmp")

if autopairs_present and cmp_present then
	cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())

	cmp_autopairs.lisp[#cmp_autopairs.lisp + 1] = "racket"
end
