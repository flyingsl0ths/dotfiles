local dap = require "dap"
local utils = require "conf_utils.utils"

local dap_args_env_var = "NVIM_DAP_ARGS"
local function get_dap_args()
	local arg = os.getenv(dap_args_env_var)
	local args = arg and utils.split_at(arg, "[^;]+") or {}
	return args
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

dap.adapters.delve = {
	type = 'server',
	port = '${port}',
	executable = {
		command = 'dlv',
		args = { 'dap', '-l', '127.0.0.1:${port}' },
		-- add this if on windows, otherwise server won't open successfully
		-- detached = false
	}
}

-- https://github.com/go-delve/delve/blob/master/Documentation/usage/dlv_dap.md
dap.configurations.go = {
	{
		type = "delve",
		name = "Debug",
		request = "launch",
		program = "${file}"
	},
	{
		type = "delve",
		name = "Debug test", -- configuration for debugging test files
		request = "launch",
		mode = "test",
		program = "${file}"
	},
	-- works with go.mod packages and sub packages
	{
		type = "delve",
		name = "Debug test (go.mod)",
		request = "launch",
		mode = "test",
		program = "./${relativeFileDirname}"
	}
}

local dap_py = require("dap-python")
dap_py.setup(utils.run_cmd("which python"))
dap_py.test_runner = "pytest"
