import qualified Data.Map as Map
  ( fromList,
  )
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.WithAll (killAll)
import XMonad.Core (XConfig)
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
    fullscreenEventHook,
  )
import XMonad.Hooks.ManageDocks
  ( avoidStruts,
    docks,
  )
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat,
    isDialog,
  )
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.Hidden
  ( hiddenWindows,
    hideWindow,
    popOldestHiddenWindow,
  )
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing
  ( Border (Border),
    spacingRaw,
  )
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
  ( Theme,
    activeBorderColor,
    activeColor,
    activeTextColor,
    decoHeight,
    fontName,
    inactiveBorderColor,
    inactiveColor,
    inactiveTextColor,
    shrinkText,
    tabbed,
    urgentBorderColor,
    urgentColor,
    urgentTextColor,
  )
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.Layout.WindowArranger
import XMonad.ManageHook (className)
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Ungrab (unGrab)

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:size=10:antialias=true"

myTerminal :: String
myTerminal = "kitty"

-- Used for commands that depend on binaries found in $PATH specified by .zshrc
zshTerminalCommandPrefix :: String
zshTerminalCommandPrefix = myTerminal ++ " zsh -i -c "

gamemodeCommandPrefix :: String
gamemodeCommandPrefix = "~/.local/bin/game_mode "

codeEditors :: [String]
codeEditors =
  [ zshTerminalCommandPrefix ++ "'vim'",
    gamemodeCommandPrefix ++ "android-studio",
    "vscodium",
    "typora",
    "drracket",
    "idea"
  ]

myWorkspaces :: [String]
myWorkspaces = ["I", "II", "III", "IV", "V", "VI"]

myTabConfig :: Theme
myTabConfig =
  def
    { activeColor = "#5e81ac",
      inactiveColor = "#4c566a",
      urgentColor = "#bf616a",
      activeBorderColor = "#5e81ac",
      inactiveBorderColor = "#4c566a",
      urgentBorderColor = "#bf616a",
      activeTextColor = "#eceff4",
      inactiveTextColor = "#eceff4",
      urgentTextColor = "#eceff4",
      fontName = myFont,
      decoHeight = 15
    }

myLayout =
  mouseResize $
    windowArrange $
      hiddenWindows
        ( avoidStruts
            ( spacingRaw
                True
                (Border 0 10 10 10)
                True
                (Border 10 10 10 10)
                True
                ( tiled
                    ||| Mirror tiled
                    ||| ThreeColMid nmaster delta ratio
                    ||| spiralWithDir East CW (6 / 7)
                )
                ||| Full
                ||| tabbed shrinkText myTabConfig
            )
        )
  where
    tiled = Tall nmaster delta ratio
    nmaster = 1 -- Default number of windows in the master pane
    ratio = 1 / 2 -- Default proportion of screen occupied by master pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

colorNormalBorder :: String
colorNormalBorder = "#eceff4"

colorFocusedBorder :: String
colorFocusedBorder = "#5e81ac"

myNavigation :: TwoD a (Maybe a)
myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap =
      Map.fromList
        [ ((0, xK_Escape), cancel),
          ((0, xK_Return), select),
          ((0, xK_slash), substringSearch myNavigation),
          ((0, xK_Left), move (-1, 0) >> myNavigation),
          ((0, xK_Right), move (1, 0) >> myNavigation),
          ((0, xK_Down), move (0, 1) >> myNavigation),
          ((0, xK_Up), move (0, -1) >> myNavigation),
          ((0, xK_h), move (-1, 0) >> myNavigation),
          ((0, xK_l), move (1, 0) >> myNavigation),
          ((0, xK_j), move (0, 1) >> myNavigation),
          ((0, xK_k), move (0, -1) >> myNavigation),
          ((0, xK_space), setPos (0, 0) >> myNavigation)
        ]
    -- The navigation handler ignores unknown key symbols
    navDefaultHandler = const myNavigation

gridSelectionConfig :: GSConfig Window
gridSelectionConfig =
  def
    { gs_cellheight = 150,
      gs_cellwidth = 150,
      gs_navigate = myNavigation,
      gs_font = myFont,
      gs_colorizer = fromClassName
    }

editorGridSelectionConfig :: GSConfig String
editorGridSelectionConfig =
  def
    { gs_cellheight = gs_cellheight gridSelectionConfig,
      gs_cellwidth = gs_cellwidth gridSelectionConfig,
      gs_font = gs_font gridSelectionConfig
    }

myKeys :: [(String, X ())]
myKeys =
  [ -- Programs --
    ("M-w", spawn "firedragon"),
    ("M-S-w", spawn "firedragon --private-window"),
    ("M-y", spawn "typora"),
    ("M-f", spawn "thunar"),
    ("M-g", spawn "gammy"),
    ("M-v", spawn "vscodium"),
    ("M-u", spawn "corectrl"),
    ("M-z", spawn "zathura"),
    ("M-m", spawn "pamac-manager"),
    ("M-S-p", spawn (gamemodeCommandPrefix ++ "gimp")),
    ("M-S-s", spawn (gamemodeCommandPrefix ++ "android-studio")),
    --------------
    -- Dropdowns --
    ( "M-S-m",
      spawn "tdrop -w 450 -h 450 -y 0 gnome-characters"
    ),
    ( "M-S-f",
      spawn "tdrop -w 1000 -h 800 -x 450 alacritty -e zsh -i -c ranger"
    ),
    ("M-S-t", spawn "tdrop -w 1920 -h 800 -y 0 alacritty -e htop"),
    ( "M-S-y",
      spawn "tdrop -w 1000 -h 800 -x 450 alacritty -e zsh -i -c ytm"
    ),
    ---------------
    -- Groups --
    ("M-S-n", spawn "vscodium; zathura"),
    ------------
    -- Tools --
    ("M-<Return>", spawn myTerminal),
    ("M-S-a", spawn "~/.config/rofi/launchers/ribbon.sh"),
    ("M-r", spawn "~/.config/rofi/launchers/dmenu.sh"),
    ("M-S-c", unGrab *> spawn "xfce4-screenshooter"),
    ("M-c", spawn "xdg-screensaver lock"),
    ("M-S-g", goToSelected gridSelectionConfig),
    ("M-S-e", spawnSelected editorGridSelectionConfig codeEditors),
    ("M-S-r", restart "xmonad" True),
    ("M-<F3>", spawn "brightnessctl -q s 5%+"),
    ("M-<F2>", spawn "brightnessctl -q s 5%-"),
    ("M-<F8>", spawn "pamixer -i 5"),
    ("M-<F7>", spawn "pamixer -d 5"),
    ("M-<F6>", spawn "pamixer -m"),
    -----------
    -- WM Actions --
    ("M-/", withFocused hideWindow),
    ("M-\\", popOldestHiddenWindow),
    ("M-S-l", sendMessage FirstLayout),
    ("M-q", kill), -- Kill active window
    ("M-x", killAll) -- Kill all windows on current workspace
    ----------------
  ]

myStartUpHook :: X ()
myStartUpHook = setWMName "LG3D" >> spawn "~/.xmonad/startup.sh"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doCenterFloat,
      className =? "Alacritty" --> doCenterFloat,
      className =? "gammy" --> doCenterFloat,
      isDialog --> doCenterFloat
    ]

myConfig =
  def
    { modMask = mod4Mask,
      workspaces = myWorkspaces,
      layoutHook = myLayout,
      normalBorderColor = colorNormalBorder,
      focusedBorderColor = colorFocusedBorder,
      borderWidth = 0,
      startupHook = myStartUpHook,
      handleEventHook = fullscreenEventHook,
      manageHook = myManageHook
    }

main :: IO ()
main =
  xmonad $
    ewmh $
      docks $
        myConfig `additionalKeysP` myKeys
