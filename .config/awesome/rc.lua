-- If LuaRocks is installed, make sure that packages installed through it are
-- found (e.g. lgi). If LuaRocks is not installed, do nothing.
pcall(require, "luarocks.loader")
require("theme")

-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")

-- Widget and layout library
local wibox = require("wibox")

-- Theme handling library
local beautiful = require("beautiful")

-- Notification library
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup")

-- Enable hotkeys help widget for VIM and other apps
-- when client with a matching name is opened:
require("awful.hotkeys_popup.keys")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
  naughty.notify({
    preset = naughty.config.presets.critical,
    title = "Oops, there were errors during startup!",
    text = awesome.startup_errors
  })
end

-- Handle runtime errors after startup
do
  local in_error = false
  awesome.connect_signal("debug::error", function(err)
    -- Make sure we don't go into an endless error loop
    if in_error then return end
    in_error = true

    naughty.notify({
      preset = naughty.config.presets.critical,
      title = "Oops, an error happened!",
      text = tostring(err)
    })
    in_error = false
  end)
end
-- }}}

-- {{{ Variable definitions
-- Themes define colours, icons, font and wallpapers.
local home_dir = os.getenv("HOME")
beautiful.init(home_dir .. "/.config/awesome/theme.lua")

-- This is used later as the default terminal and editor to run.
local terminal = "alacritty"
local browser = "google-chrome-stable"

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
local modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
awful.layout.layouts = {
  awful.layout.suit.max,
  awful.layout.suit.tile,
  awful.layout.suit.floating,
  awful.layout.suit.spiral,
  awful.layout.suit.spiral.dwindle
}

-- {{{ Wibar
-- Create a wibox for each screen and add it
local taglist_buttons = gears.table.join(
  awful.button({}, 1, function(t) t:view_only() end),

  awful.button({ modkey }, 1, function(t)
    if client.focus then client.focus:move_to_tag(t) end
  end),

  awful.button({}, 3, awful.tag.viewtoggle),

  awful.button({ modkey }, 3, function(t)
    if client.focus then client.focus:toggle_tag(t) end
  end),

  awful.button({}, 4, function(t) awful.tag.viewnext(t.screen) end),

  awful.button({}, 5, function(t)
    awful.tag.viewprev(t.screen)
  end))

local function set_wallpaper(s)
  -- Wallpaper
  if beautiful.wallpaper then
    local wallpaper = beautiful.wallpaper
    -- If wallpaper is a function, call it with the screen
    if type(wallpaper) == "function" then wallpaper = wallpaper(s) end
    gears.wallpaper.maximized(wallpaper, s, true)
  end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)

awful.screen.connect_for_each_screen(function(s)
  -- Wallpaper
  set_wallpaper(s)

  -- Each screen has its own tag table.
  awful.tag({ "  ", "  ", "  ", "  ", "  ", "  ", "  " }, s,
    awful.layout.layouts[1])

  -- Create a taglist widget
  s.mytaglist = awful.widget.taglist {
    screen = s,
    filter = awful.widget.taglist.filter.all,
    buttons = taglist_buttons,
  }

  -- Create an imagebox widget which will contain an icon indicating which layout we're using.
  -- We need one layoutbox per screen.
  s.mylayoutbox = awful.widget.layoutbox(s)

  s.mylayoutbox:buttons(gears.table.join(
    awful.button({}, 1,
      function() awful.layout.inc(1) end),
    awful.button({}, 3,
      function() awful.layout.inc(-1) end),
    awful.button({}, 4,
      function() awful.layout.inc(1) end),
    awful.button({}, 5,
      function() awful.layout.inc(-1) end)))

  -- Create the wibox
  s.mywibox = awful.wibar({
    position = "top",
    screen = s,
    width = s.geometry.width,
    -- shape = function(cr, width, height)
    --   gears.shape.rounded_rect(cr, width, height, 6)
    -- end
  })

  -- Add widgets to the wibox
  s.mywibox:setup {
    layout = wibox.layout.align.horizontal,
    { -- Left widgets
      s.mytaglist,
      layout = wibox.layout.fixed.horizontal
    },

    {
      -- middle widgets
      (require "clock")(),
      layout = wibox.layout.manual
    },

    { -- Right widgets
      wibox.widget.systray(),
      (require "battery")(),
      (require "volume")(),
      wibox.widget { markup = " ", widget = wibox.widget.textbox() },
      s.mylayoutbox,
      layout = wibox.layout.fixed.horizontal
    }
  }
end)
-- }}}

-- Utils
local utils = require "utils"

-- {{{ Key bindings
globalkeys = gears.table.join(
  utils.make_keybinding({
    modifiers = { modkey },
    key = "s",
    command = hotkeys_popup.show_help,
    description = { description = "Show help", group = "awesome" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "Left",
    command = awful.tag.viewprev,
    description = { description = "View previous", group = "tag" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "Right",
    command = awful.tag.viewnext,
    description = { description = "View next", group = "tag" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "Escape",
    command = awful.tag.history.restore,
    description = { description = "go back", group = "tag" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "j",
    command = function() awful.client.focus.byidx(1) end,
    description = { description = "Focus next by index", group = "client" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "k",
    command = function() awful.client.focus.byidx(-1) end,
    description = { description = "Focus previous by index", group = "client" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "j",
    command = function() awful.client.swap.byidx(1) end,
    description = { description = "Swap with next client by index", group = "client" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "k",
    command = function() awful.client.swap.byidx(-1) end,
    description = { description = "Swap with previous client by index", group = "client" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "j",
    command = function() awful.screen.focus_relative(1) end,
    description = { description = "Focus the next screen", group = "screen" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "k",
    command = function() awful.screen.focus_relative(-1) end,
    description = { description = "Focus the previous screen", group = "screen" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "u",
    command = awful.client.urgent.jumpto,
    description = { description = "Jump to an urgent client", group = "client" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "Tab",
    command = function()
      awful.client.focus.history.previous()
      if client.focus then client.focus:raise() end
    end,
    description = { description = "Tab through previously focused windows", group = "client" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "Return",
    command = function() awful.spawn(terminal) end,
    description = { description = "Open a terminal", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "r",
    command = awesome.restart,
    description = { description = "Restart WM", group = "awesome" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "q",
    command = awesome.quit,
    description = { description = "Quit WM", group = "awesome" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "l",
    command = function() awful.tag.incmwfact(0.05) end,
    description = { description = "Increase master width factor", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "h",
    command = function() awful.tag.incmwfact(-0.05) end,
    description = { description = "Decrease master width factor", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "h",
    command = function() awful.tag.incnmaster(1, nil, true) end,
    description = { description = "Increase master clients number", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "l",
    command = function() awful.tag.incnmaster(-1, nil, true) end,
    description = { description = "Increase master clients number", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "h",
    command = function() awful.tag.incncol(1, nil, true) end,
    description = { description = "Increase window columns number", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "l",
    command = function() awful.tag.incncol(-1, nil, true) end,
    description = { description = "Decrease window columns number", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "space",
    command = function() awful.layout.inc(1) end,
    description = { description = "Selects the next layout", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "space",
    command = function() awful.layout.inc(-1) end,
    description = { description = "Selects the previous layout", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "m",
    command = function()
      awful.layout.set(awful.layout.suit.max)
    end,
    description = { description = "Sets the \"max\" layout", group = "layout" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "n",
    command = function()
      local c = awful.client.restore()
      -- Focus restored client
      if c then
        c:emit_signal("request::activate", "key.unminimize", { raise = true })
      end
    end,
    description = { description = "Restore minimized", group = "client" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "r",
    command = function()
      awful.spawn("dmenu_run -p 'λ' -m 0")
    end,
    description = { description = "Run cmd prompt", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "p",
    command = function()
      awful.spawn(string.format("%s/%s", home_dir,
        ".config/rofi/launchers/ribbon/launcher.sh ribbon_bottom"))
    end,
    description = { description = "Run app launcher", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "t",
    command = function()
      awful.spawn("xfce4-taskmanager")
    end,
    description = { description = "task manager", group = "apps" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "e",
    command = function()
      awful.spawn(string.format("%s/%s", home_dir,
        ".config/rofi/applets/android/editors.sh"))
    end,
    description = { description = "Selects a file editor", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "p",
    command = function()
      awful.spawn(string.format("%s/%s", home_dir,
        ".config/rofi/applets/android/powermenu.sh"))
    end,
    description = { description = "Opens the power menu", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "y",
    command = function()
      awful.spawn(string.format("%s/%s", home_dir, ".local/bin/game_mode yuzu"))
    end,
    description = { description = "Yuzu emulator", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "f",
    command = function()
      awful.spawn("pcmanfm")
    end,
    description = { description = "File manager", group = "apps" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "g",
    command = function()
      awful.spawn("gammy")
    end,
    description = { description = "Gammy", group = "apps" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "g",
    command = function() awful.spawn("gimp") end,
    description = { description = "Gimp", group = "apps" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "w",
    command = function() awful.spawn(browser) end,
    description = { description = "Web browser", group = "apps" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "w",
    command = function() awful.spawn("librewolf --private-window") end,
    description = { description = "Private web browser", group = "apps" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "F3",
    command = function() awful.spawn("brightnessctl set 5%+") end,
    description = { description = "Raise screen brightness", group = "utils" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "F2",
    command = function() awful.spawn("brightnessctl set 5%-") end,
    description = { description = "Lower screen brightness", group = "utils" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "F8",
    command = function() awful.spawn("pamixer -i 5") end,
    description = { description = "Raise volume", group = "utils" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "F7",
    command = function() awful.spawn("pamixer -d 5") end,
    description = { description = "Lower volume", group = "utils" }
  }),

  utils.make_keybinding({
    modifiers = { modkey },
    key = "F6",
    command = function() awful.spawn("pamixer -t") end,
    description = { description = "Toggle mute", group = "utils" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "n",
    command = function() awful.spawn("networkmanager_dmenu") end,
    description = { description = "Open network panel", group = "utils" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "Return",
    command = function() awful.spawn(terminal .. " --class scratchpad") end,
    description = { description = "Open a dropdown terminal", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Shift" },
    key = "p",
    command = function() awful.spawn(terminal .. " --class pyscratchpad -e " .. home_dir .. "/.local/bin/bpython") end,
    description = { description = "Open a python scratchpad", group = "launcher" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "y",
    command = function() awful.spawn(terminal .. " --class ytmscratchpad -e ytfzf -t -m -l") end,
    description = { description = "Open youtube music", group = "scratchpads" }
  }),

  utils.make_keybinding({
    modifiers = { modkey, "Control" },
    key = "f",
    command = function() awful.spawn(terminal .. " --class fmscratchpad -e ranger") end,
    description = { description = "Open the terminal file manager", group = "scratchpads" }
  })
)

clientkeys = gears.table.join(
  awful.key({ modkey }, "f", function(c)
    c.fullscreen = not c.fullscreen
    c:raise()
  end, { description = "toggle fullscreen", group = "client" }),

  awful.key({ modkey }, "q", function(c) c:kill() end,
    {
      description = "close",
      group = "client"
    }),

  awful.key({ modkey, "Control" }, "space", awful.client.floating.toggle,
    { description = "toggle floating", group = "client" }),

  awful.key({ modkey, "Control" }, "Return",
    function(c)
      c:swap(awful.client.getmaster())
    end, { description = "move to master", group = "client" }),

  awful.key({ modkey }, "o",
    function(c) c:move_to_screen() end, {
      description = "move to screen",
      group = "client"
    }),

  awful.key({ modkey }, "t", function(c) c.ontop = not c.ontop end,
    { description = "toggle keep on top", group = "client" }),

  awful.key({ modkey }, "n", function(c)
    -- The client currently has the input focus, so it cannot be
    -- minimized, since minimized clients can't have the focus.
    c.minimized = true
  end, { description = "minimize", group = "client" }),

  awful.key({ modkey }, "m", function(c)
    c.maximized = not c.maximized
    c:raise()
  end, { description = "(un)maximize", group = "client" }),

  awful.key({ modkey, "Control" }, "m", function(c)
    c.maximized_vertical = not c.maximized_vertical
    c:raise()
  end, { description = "(un)maximize vertically", group = "client" }),

  awful.key({ modkey, "Shift" }, "m", function(c)
    c.maximized_horizontal = not c.maximized_horizontal
    c:raise()
  end, { description = "(un)maximize horizontally", group = "client" }))

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it work on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
  globalkeys = gears.table.join(globalkeys, -- View tag only.
    awful.key({ modkey }, "#" .. i + 9, function()
      local screen = awful.screen.focused()
      local tag = screen.tags[i]
      if tag then tag:view_only() end
    end, { description = "view tag #" .. i, group = "tag" }),

    -- Toggle tag display.
    awful.key({ modkey, "Control" }, "#" .. i + 9,
      function()
        local screen = awful.screen.focused()
        local tag = screen.tags[i]
        if tag then awful.tag.viewtoggle(tag) end
      end, { description = "toggle tag #" .. i, group = "tag" }),

    -- Move client to tag.
    awful.key({ modkey, "Shift" }, "#" .. i + 9,
      function()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then client.focus:move_to_tag(tag) end
        end
      end, { description = "move focused client to tag #" .. i, group = "tag" }),

    -- Toggle tag on focused client.
    awful.key({ modkey, "Control", "Shift" },
      "#" .. i + 9, function()
        if client.focus then
          local tag = client.focus.screen.tags[i]
          if tag then client.focus:toggle_tag(tag) end
        end
      end, { description = "toggle focused client on tag #" .. i, group = "tag" }))
end

clientbuttons = gears.table.join(
  awful.button({}, 1, function(c)
    c:emit_signal("request::activate", "mouse_click", { raise = true })
  end),

  awful.button({ modkey }, 1, function(c)
    c:emit_signal("request::activate", "mouse_click", { raise = true })
    awful.mouse.client.move(c)
  end),

  awful.button({ modkey, "Shift" }, 1, function(c)
    c:emit_signal("request::activate", "mouse_click", { raise = true })
    awful.mouse.client.resize(c)
  end))

-- Set keys
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
  -- All clients will match this rule.
  {
    rule = {},
    properties = {
      border_width = beautiful.border_width,
      border_color = beautiful.border_normal,
      focus = awful.client.focus.filter,
      raise = true,
      keys = clientkeys,
      buttons = clientbuttons,
      screen = awful.screen.preferred,
      placement = awful.placement.no_overlap + awful.placement.no_offscreen
    }
  }, -- Floating clients.
  {
    rule_any = { role = { "AlarmWindow", "ConfigManager", "pop-up" } },
    properties = { floating = true }
  }, {
  rule_any = { type = { "normal", "dialog" } },
  properties = { titlebars_enabled = false }
}, {
  rule_any = {
    class = { "scratchpad", "pyscratchpad",
      "ytmscratchpad", "fmscratchpad", "OpenGL Tutorial",
    },

    instance = {
      "gammy", "jetbrains-studio",
      "jetbrains-idea-ce"
    }
  },
  properties = { floating = true }
}
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function(c)
  -- Set the windows at the slave,
  -- i.e. put it at the end of others instead of setting it master.
  -- if not awesome.startup then awful.client.setslave(c) end

  if awesome.startup and not c.size_hints.user_position and
      not c.size_hints.program_position then
    -- Prevent clients from being unreachable after screen count changes.
    awful.placement.no_offscreen(c)
  end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
  c:emit_signal("request::activate", "mouse_enter", { raise = false })
end)

client.connect_signal("focus",
  function(c) c.border_color = beautiful.border_focus end)

client.connect_signal("unfocus",
  function(c) c.border_color = beautiful.border_normal end)

client.connect_signal("property::geometry", function(c)
  c.shape = function(cr, width, height)
    gears.shape.rounded_rect(cr, width, height, 16)
  end
end)
-- }}}

local autostart = {
  "picom -b --corner-radius 0",
  "xsettingsd &",
  "xrdb merge $HOME/Xresources",
  "xautolock -time 10 -locker $HOME/.local/bin/i3lock_color -detectsleep &",
  "gammy &",
  "lxqt-policykit-agent &",
}

for _, cmd in ipairs(autostart) do
  awful.spawn.with_shell(cmd)
end
