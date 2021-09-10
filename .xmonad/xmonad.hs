import           XMonad
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.SetWMName
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns
import           XMonad.Util.EZConfig
import           XMonad.Util.Ungrab

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
    (tiled ||| Mirror tiled ||| Full ||| ThreeColMid nmaster delta ratio)
  )
 where
  tiled   = Tall nmaster delta ratio
  nmaster = 1      -- Default number of windows in the master pane
  ratio   = 1 / 2    -- Default proportion of screen occupied by master pane
  delta   = 3 / 100  -- Percent of screen to increment by when resizing panes

-- Theme
colorNormalBorder = "#4c566a"
colorFocusedBorder = "#88c0d0"

myKeys =
  [ ("M-S-s", unGrab *> spawn "xfce4-screenshooter")
  , ("M-w"       , spawn "librewolf")
  , ("M-f"       , spawn "thunar")
  , ("M-S-f", spawn (myTerminal ++ " zsh -i -c 'ranger'"))
  , ("M-v"       , spawn "vscodium")
  , ("M-g"       , spawn "gimp")
  , ("M-c"       , spawn "lxqt-sudo corectrl")
  , ("M-S-t"     , spawn "xfce4-taskmanager")
  , ("M-p"       , spawn "pamac-manager")
  , ("M-<Return>", spawn myTerminal)
  , ("M-r", spawn "rofi -theme '.config/rofi/rofi/dmenu.rasi' -show run")
  , ("M-S-l"     , spawn "xdg-screensaver lock")
  , ("M-<F3>"    , spawn "lux -a 5%")
  , ("M-<F2>"    , spawn "lux -s 5%")
  , ("M-S-r"     , spawn "xmonad --restart")
  , ("M-q"       , kill)
  ]

myStartUpHook = setWMName "LG3D" >> spawn "~/.xmonad/startup.sh"

myEventHook = ewmhDesktopsEventHook

myConfig = def { modMask            = mod4Mask
               , workspaces         = myWorkspaces
               , layoutHook         = myLayout
               , normalBorderColor  = colorNormalBorder
               , focusedBorderColor = colorFocusedBorder
               , borderWidth        = 5
               , startupHook        = myStartUpHook
               , handleEventHook    = fullscreenEventHook
               }


main = xmonad $ ewmh $ docks $ myConfig `additionalKeysP` myKeys
