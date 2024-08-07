local wezterm = require 'wezterm'

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.color_scheme = "nord"

config.font =
    wezterm.font('JetBrainsMono Nerd Font')

config.font_size = 19.0
config.warn_about_missing_glyphs = false

config.keys = {
  {
    key = 'q',
    mods = 'CTRL|ALT|SHIFT',
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'w',
    mods = 'CTRL|ALT|SHIFT',
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'W',
    mods = 'ALT|SHIFT',
    action = wezterm.action.ActivatePaneDirection("Up")
  },
  {
    key = 'S',
    mods = 'ALT|SHIFT',
    action = wezterm.action.ActivatePaneDirection("Down")
  },
  {
    key = 'A',
    mods = 'ALT|SHIFT',
    action = wezterm.action.ActivatePaneDirection("Left")
  },
  {
    key = 'D',
    mods = 'ALT|SHIFT',
    action = wezterm.action.ActivatePaneDirection("Right")
  },
  { key = "q", mods = "ALT|SHIFT", action = wezterm.action { RotatePanes = "CounterClockwise" } },
  { key = "e", mods = "ALT|SHIFT", action = wezterm.action { RotatePanes = "Clockwise" } },
  {
    key = 'X',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.CloseCurrentTab { confirm = true }
  },
  {
    key = "X", mods = "ALT|SHIFT", action = wezterm.action { CloseCurrentPane = { confirm = true } }
  },
  {
    key = "]",
    mods = "CTRL",
    action = wezterm.action.ActivateTabRelative(1)
  },
  {
    key = "[",
    mods = "CTRL",
    action = wezterm.action.ActivateTabRelative(-1)
  },
  {
    key = "a",
    mods = "CTRL|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Left", 1 }
  },
  {
    key = "d",
    mods = "CTRL|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Right", 1 }
  },
  {
    key = "w",
    mods = "CTRL|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Up", 1 }
  },
  {
    key = "s",
    mods = "CTRL|SHIFT",
    action = wezterm.action.AdjustPaneSize { "Down", 1 }
  }
}


return config
