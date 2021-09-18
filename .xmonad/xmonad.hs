import           XMonad

import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )

import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                )

import           XMonad.ManageHook              ( className )

import           XMonad.Hooks.ManageHelpers     ( doCenterFloat
                                                , isDialog
                                                )

import           XMonad.Hooks.SetWMName         ( setWMName )

import           XMonad.Layout.Spacing          ( Border(Border)
                                                , spacingRaw
                                                )

import           XMonad.Layout.ThreeColumns     ( ThreeCol(ThreeColMid) )

import           XMonad.Layout.Tabbed           ( activeBorderColor
                                                , activeColor
                                                , activeTextColor
                                                , decoHeight
                                                , fontName
                                                , inactiveBorderColor
                                                , inactiveColor
                                                , inactiveTextColor
                                                , shrinkText
                                                , tabbed
                                                , urgentBorderColor
                                                , urgentColor
                                                , urgentTextColor
                                                )

import           XMonad.Util.EZConfig           ( additionalKeysP )

import           XMonad.Util.Ungrab             ( unGrab )

myManageHook :: ManageHook

myTerminal = "kitty"

myWorkspaces = ["I", "II", "III", "IV", "V", "VI"]

myTabConfig = def
  { activeColor         = "#5e81ac"
  , inactiveColor       = "#4c566a"
  , urgentColor         = "#bf616a"
  , activeBorderColor   = "#5e81ac"
  , inactiveBorderColor = "#4c566a"
  , urgentBorderColor   = "#bf616a"
  , activeTextColor     = "#eceff4"
  , inactiveTextColor   = "#2e3440"
  , urgentTextColor     = "#eceff4"
  , fontName            = "xft:JetBrainsMono Nerd Font:size=10:antialias=true"
  , decoHeight          = 15
  }

myLayout = avoidStruts
  (Full ||| tabbed shrinkText myTabConfig ||| spacingRaw
    True
    (Border 0 10 10 10)
    True
    (Border 10 10 10 10)
    True
    (tiled ||| Mirror tiled ||| ThreeColMid nmaster delta ratio)
  )
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1      -- Default number of windows in the master pane
  ratio   = 1 / 2    -- Default proportion of screen occupied by master pane
  delta   = 3 / 100  -- Percent of screen to increment by when resizing panes

colorNormalBorder = "#eceff4"
colorFocusedBorder = "#5e81ac"

myKeys =
  [ ("M-w"       , spawn "librewolf")
  , ("M-y"       , spawn "typora")
  , ("M-f"       , spawn "thunar")
  , ("M-v"       , spawn "vscodium")
  , ("M-g"       , spawn "gimp")
  , ("M-c"       , spawn "lxqt-sudo corectrl")
  , ("M-z"       , spawn "zathura")
  , ("M-p"       , spawn "pamac-manager")
  , ("M-<Return>", spawn myTerminal)
  , ("M-r", spawn "~/.config/rofi/launchers/dmenu.sh")
  , ("M-S-p", spawn "~/.config/rofi/launchers/ribbon.sh")
  , ("M-S-a", spawn "~/.bin/game_mode android-studio")
  , ("M-S-s", unGrab *> spawn "xfce4-screenshooter")
  , ("M-S-t"     , spawn "xfce4-taskmanager")
  , ("M-S-f", spawn (myTerminal ++ " zsh -i -c 'ranger'"))
  , ("M-S-b", spawn (myTerminal ++ " zsh -i -c 'br'"))
  , ("M-S-l"     , spawn "xdg-screensaver lock")
  , ("M-S-n"     , spawn "vscodium; zathura")
  , ("M-S-w"     , spawn "librewolf --private-window")
  , ("M-<F3>"    , spawn "lux -a 5%")
  , ("M-<F2>"    , spawn "lux -s 5%")
  , ("M-<F8>"    , spawn "amixer sset Master 5%+")
  , ("M-<F7>"    , spawn "amixer sset Master 5%-")
  , ("M-<F6>"    , spawn "amixer sset Master toggle")
  , ("M-S-r", spawn "xmonad --recompile ; xmonad --restart")
  , ("M-q"       , kill)
  ]

myStartUpHook = setWMName "LG3D" >> spawn "~/.xmonad/startup.sh"

myManageHook =
  composeAll [className =? "Gimp" --> doFloat, isDialog --> doFloat]

myConfig = def { modMask            = mod4Mask
               , workspaces         = myWorkspaces
               , layoutHook         = myLayout
               , normalBorderColor  = colorNormalBorder
               , focusedBorderColor = colorFocusedBorder
               , borderWidth        = 0
               , startupHook        = myStartUpHook
               , handleEventHook    = fullscreenEventHook
               , manageHook         = myManageHook
               }


main = xmonad $ ewmh $ docks $ myConfig `additionalKeysP` myKeys
