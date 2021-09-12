import           XMonad

import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , fullscreenEventHook
                                                )

import           XMonad.Hooks.ManageDocks       ( avoidStruts
                                                , docks
                                                )

import           XMonad.Hooks.SetWMName         ( setWMName )

import           XMonad.Layout.Spacing          ( Border(Border)
                                                , spacingRaw
                                                )

import           XMonad.Layout.ThreeColumns     ( ThreeCol(ThreeColMid) )

import           XMonad.Util.EZConfig           ( additionalKeysP )

import           XMonad.Util.Ungrab             ( unGrab )

main :: IO ()

myTerminal = "kitty"

myWorkspaces = ["I", "II", "III", "IV", "V", "VI"]

myLayout = spacingRaw
  True
  (Border 0 10 10 10)
  True
  (Border 10 10 10 10)
  True
  (avoidStruts
    (Full ||| tiled ||| Mirror tiled ||| ThreeColMid nmaster delta ratio)
  )
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1      -- Default number of windows in the master pane
  ratio   = 1 / 2    -- Default proportion of screen occupied by master pane
  delta   = 3 / 100  -- Percent of screen to increment by when resizing panes

-- Theme
colorNormalBorder = "#4c566a"
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
  , ("M-r", spawn "rofi -theme '.config/rofi/rofi/dmenu.rasi' -show run")
  , ("M-S-a", spawn "~/.bin/game_mode android-studio")
  , ("M-S-s", unGrab *> spawn "xfce4-screenshooter")
  , ("M-S-t"     , spawn "xfce4-taskmanager")
  , ("M-S-f", spawn (myTerminal ++ " zsh -i -c 'ranger'"))
  , ("M-S-l"     , spawn "xdg-screensaver lock")
  , ("M-S-w"     , spawn "librewolf --private-window")
  , ("M-<F3>"    , spawn "lux -a 5%")
  , ("M-<F2>"    , spawn "lux -s 5%")
  , ("M-<F8>"    , spawn "amixer sset Master 5%+")
  , ("M-<F7>"    , spawn "amixer sset Master 5%-")
  , ("M-<F6>"    , spawn "amixer sset Master toggle")
  , ("M-S-r"     , spawn "xmonad --restart")
  , ("M-q"       , kill)
  ]

myStartUpHook = setWMName "LG3D" >> spawn "~/.xmonad/startup.sh"

myConfig = def { modMask            = mod4Mask
               , workspaces         = myWorkspaces
               , layoutHook         = myLayout
               , normalBorderColor  = colorNormalBorder
               , focusedBorderColor = colorFocusedBorder
               , borderWidth        = 0
               , startupHook        = myStartUpHook
               , handleEventHook    = fullscreenEventHook
               }


main = xmonad $ ewmh $ docks $ myConfig `additionalKeysP` myKeys
