local dap = require "dap"
local utils = require "conf_utils.utils"

local dap_args_env_var = "NVIM_DAP_ARGS"
local function get_dap_args()
	local args = os.getenv(dap_args_env_var)
	return args and utils.split_at("%a+", args) or {}
end

dap.adapters.lldb = {
	type = 'executable',
	command = '/usr/bin/lldb-vscode',
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

dap.configurations.lua = {
	{
		type = 'nlua',
		request = 'attach',
		name = "Attach to running Neovim instance",
		host = function()
			local value = vim.fn.input('Host [127.0.0.1]: ')
			if value ~= "" then
				return value
			end
			return '127.0.0.1'
		end,
		port = function()
			local val = tonumber(vim.fn.input('Port: '))
			assert(val, "Please provide a port number")
			return val
		end,
	}
}

dap.adapters.nlua = function(callback, config)
	callback({ type = 'server', host = config.host, port = config.port })
end

local dap_py = require("dap-python")
dap_py.setup(utils.run_cmd("which python"))
dap_py.test_runner = "pytest"
