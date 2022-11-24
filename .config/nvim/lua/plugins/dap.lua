local dap = require "dap"
local utils = require "conf_utils.utils"

local dap_args_env_var = "NVIM_DAP_ARGS"
local function get_dap_args()
	local args = os.getenv(dap_args_env_var)
	return args and utils.split_at("%a+", args) or {}
end

dap.adapters.lldb = {
	type = 'executable',
	command = 'lldb-vscode',
	name = 'lldb'
}

dap.configurations.cpp = {
	{
		name = 'Launch',
		type = 'lldb',
		request = 'launch',
		program = function()
			return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
		end,
		cwd = '${workspaceFolder}',
		stopOnEntry = false,
		args = function()
			return get_dap_args()
		end
	},
}

dap.configurations.c = dap.configurations.cpp
dap.configurations.rust = dap.configurations.cpp

dap.configurations.lua = {
	{
		type = 'nlua',
		request = 'attach',
		name = "Attach to running Neovim instance",
	}
}

dap.adapters.nlua = function(callback, config)
	callback({ type = 'server', host = config.host or "127.0.0.1", port = config.port or 8086 })
end

local dap_py = require("dap-python")
dap_py.setup(utils.run_cmd("which python"))
dap_py.test_runner = "pytest"
