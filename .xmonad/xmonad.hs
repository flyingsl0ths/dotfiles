import qualified Data.Map as Mp
  ( Map,
    fromList,
    union,
  )

import System.Exit(exitSuccess)

import LayoutUtils (rawSpacing')

import XMonad

import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseGestures (Direction2D (..), mouseGesture)
import XMonad.Actions.WithAll (killAll)

import XMonad.Core (XConfig)

import XMonad.ManageHook (className)
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

import XMonad.Layout.Accordion (Accordion (..))
import XMonad.Layout.Hidden
  ( hiddenWindows,
    hideWindow,
    popOldestHiddenWindow,
  )
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing,
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

import qualified XMonad.StackSet as W

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
  hiddenWindows $
    avoidStruts $
      Full
        ||| tabbed shrinkText myTabConfig
        ||| resizeableLayouts
  where
    -- Default number of windows in the master pane
    nmaster = 1 :: Int

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2 :: Rational

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100 :: Rational

    tiled = Tall nmaster delta ratio

    tallAccordion = renamed [Replace "verticalAccordion"] Accordion

    wideAccordion = renamed [Replace "horizontalAccordion"] $ Mirror Accordion

    resizeableLayouts =
      rawSpacing'
        10
        ( tiled
            ||| Mirror tiled
            ||| tallAccordion
            ||| wideAccordion
            ||| ThreeColMid nmaster delta ratio
            ||| spiralWithDir East CW (6 / 7)
        )

colorNormalBorder :: String
colorNormalBorder = "#eceff4"

colorFocusedBorder :: String
colorFocusedBorder = "#5e81ac"

myGridNavigation :: TwoD a (Maybe a)
myGridNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where
    navKeyMap =
      Mp.fromList
        [ ((0, xK_Escape), cancel),
          ((0, xK_Return), select),
          ((0, xK_slash), substringSearch myGridNavigation),
          ((0, xK_Left), move (-1, 0) >> myGridNavigation),
          ((0, xK_Right), move (1, 0) >> myGridNavigation),
          ((0, xK_Down), move (0, 1) >> myGridNavigation),
          ((0, xK_Up), move (0, -1) >> myGridNavigation),
          ((0, xK_h), move (-1, 0) >> myGridNavigation),
          ((0, xK_l), move (1, 0) >> myGridNavigation),
          ((0, xK_j), move (0, 1) >> myGridNavigation),
          ((0, xK_k), move (0, -1) >> myGridNavigation),
          ((0, xK_space), setPos (0, 0) >> myGridNavigation)
        ]
    -- The navigation handler ignores unknown key symbols
    navDefaultHandler = const myGridNavigation

myColorizer :: Window -> Bool -> X (String, String)
myColorizer =
  colorRangeFromClassName
    black -- lowest inactive bg
    (0x5E, 0x81, 0xAC) -- highest inactive bg
    black -- active bg
    white -- inactive fg
    white -- active fg
  where
    -- Based off nord color palette
    -- #2E3440
    black = (0x2E, 0x34, 0x40)
    -- #D8DEE9
    white = (0xD8, 0xDE, 0xE9)

gridSelectionConfig :: GSConfig Window
gridSelectionConfig =
  def
    { gs_cellheight = 150,
      gs_cellwidth = 150,
      gs_navigate = myGridNavigation,
      gs_font = myFont,
      gs_colorizer = myColorizer
    }

editorGridSelectionConfig :: GSConfig String
editorGridSelectionConfig =
  def
    { gs_cellheight = gs_cellheight gridSelectionConfig,
      gs_cellwidth = gs_cellwidth gridSelectionConfig,
      gs_font = gs_font gridSelectionConfig
    }

myKeys :: XConfig Layout -> Mp.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} =
  Mp.fromList $
    -- Program -----
    [ ((modMask, xK_w), spawn "firedragon"),
      ((modMask .|. shiftMask, xK_w), spawn "firedragon --private-window"),
      ((modMask, xK_y), spawn "typora"),
      ((modMask, xK_f), spawn "thunar"),
      ((modMask .|. shiftMask, xK_g), spawn "gammy"),
      ((modMask, xK_v), spawn "vscodium"),
      ((modMask, xK_c), spawn "corectrl"),
      ((modMask, xK_z), spawn "zathura"),
      ((modMask, xK_p), spawn "pamac-manager"),
      ((modMask, xK_g), spawn (gamemodeCommandPrefix ++ "gimp")),
      ((modMask, xK_a), spawn (gamemodeCommandPrefix ++ "android-studio")),
      ((modMask .|. shiftMask, xK_t), spawn "xfce4-taskmanager")
    ]
      ++ [
           -----------------
           --- Dropdowns ---
           ((modMask .|. shiftMask, xK_m), spawn "tdrop -w 800 -h 525 -y 0 gnome-characters"),
           ((modMask .|. shiftMask, xK_f), spawn "tdrop -w 1000 -h 800 -x 450 alacritty -e zsh -i -c ranger"),
           ((modMask .|. shiftMask, xK_y), spawn "tdrop -w 1000 -h 800 -x 450 alacritty -e zsh -i -c ytm"),
           -----------------
           ---- Groups -----
           ((modMask .|. controlMask, xK_c), spawn "vscodium; zathura"),
           ((modMask .|. shiftMask, xK_c), spawn (myTerminal ++ " &") >> spawn "zathura")
           -----------------
         ]
      ++ [
           -- Tools ---
           ((modMask .|. shiftMask, xK_a), spawn "~/.config/rofi/launchers/ribbon.sh"),
           ((modMask, xK_r), spawn "~/.config/rofi/launchers/dmenu.sh"),
           ((modMask, xK_s), unGrab *> spawn "xfce4-screenshooter"),
           ((modMask .|. controlMask, xK_l), spawn "xdg-screensaver lock"),
           ((modMask, xK_F3), spawn "brightnessctl -q s 5%+"),
           ((modMask, xK_F2), spawn "brightnessctl -q s 5%-"),
           ((modMask, xK_F8), spawn "pamixer -i 5"),
           ((modMask, xK_F7), spawn "pamixer -d 5"),
           ((modMask, xK_F6), spawn "pamixer -m")
           ------------
         ]
      ++ [
           -- Addtional WM Actions --
           ((modMask .|. controlMask, xK_g), goToSelected gridSelectionConfig),
           ((modMask .|. shiftMask, xK_e), spawnSelected editorGridSelectionConfig codeEditors),
           ((modMask, xK_backslash), withFocused hideWindow),
           ((modMask .|. shiftMask, xK_backslash), popOldestHiddenWindow),
           -- Kill active window
           ((modMask, xK_q), kill),
           -- Kill all windows on current workspace
           ((modMask, xK_x), killAll)
           --------------------------
         ]
      ++
      -- WM Actions ----
      [ ((modMask, xK_Return), spawn $ XMonad.terminal conf),
        -- %! Rotate through the available layout algorithms
        ((modMask, xK_space), sendMessage NextLayout),
        -- %!  Reset the layouts on the current workspace to default
        ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
        -- %! Resize viewed windows to the correct size
        ((modMask, xK_n), refresh),
        -- move focus up or down the window stack
        ---- %! Move focus to the next window
        ((modMask, xK_Tab), windows W.focusDown),
        -- %! Move focus to the previous window
        ((modMask .|. shiftMask, xK_Tab), windows W.focusUp),
        -- %! Move focus to the next window
        ((modMask, xK_j), windows W.focusDown),
        -- %! Move focus to the previous window
        ((modMask, xK_k), windows W.focusUp),
        -- %! Move focus to the master window
        ((modMask, xK_m), windows W.focusMaster),
        -- modifying the window order
        ---- %! Swap the focused window and the master window
        ((modMask .|. shiftMask, xK_Return), windows W.swapMaster),
        -- %! Swap the focused window with the next window
        ((modMask .|. shiftMask, xK_j), windows W.swapDown),
        -- %! Swap the focused window with the previous window
        ((modMask .|. shiftMask, xK_k), windows W.swapUp),
        -- resizing the master/slave ratio
        --
        ---- %! Shrink the master area
        ((modMask, xK_h), sendMessage Shrink),
        -- %! Expand the master area
        ((modMask, xK_l), sendMessage Expand),
        -- floating layer support
        --
        -- %! Push window back into tiling
        ((modMask, xK_t), withFocused $ windows . W.sink),
        -- increase or decrease number of windows in the master area
        --
        -- %! Increment the number of windows in the master area
        ((modMask, xK_comma), sendMessage (IncMasterN 1)),
        -- %! Deincrement the number of windows in the master area
        ((modMask, xK_period), sendMessage (IncMasterN (-1))),
        -- quit, or restart
        -- %! Quit xmonad
        ((modMask .|. shiftMask, xK_q), io exitSuccess),
        -- %! Restart xmonad
        ( (modMask .|. shiftMask, xK_r),
          spawn
            "/home/flyingsl0ths/.local/bin/xmonad --recompile && /home/flyingsl0ths/.local/bin/xmonad --restart"
        )
      ]
      ++
      -- mod-[1..9] %! Switch to workspace N
      -- mod-shift-[1..9] %! Move client to workspace N
      [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]

-----------------

myMouse :: XConfig l -> [((KeyMask, Button), Window -> X ())]
myMouse XConfig {XMonad.modMask = modMask} =
  [ -- Enables resizing a window from any corner
    ((modMask, button3), \w -> focus w >> Flex.mouseResizeWindow w)
  ]

newMouseBindings :: XConfig Layout -> Mp.Map (ButtonMask, Button) (Window -> X ())
newMouseBindings xc = Mp.union (mouseBindings def xc) (Mp.fromList (myMouse xc))

myStartUpHook :: X ()
myStartUpHook = setWMName "LG3D" >> spawn "~/.xmonad/startup.sh"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      className =? "Alacritty" --> doCenterFloat,
      className =? "gammy" --> doFloat,
      className =? "Org.gnome.Characters" --> doFloat,
      isDialog --> doFloat
    ]

myConfig =
  def
    { modMask = mod4Mask,
      terminal = myTerminal,
      workspaces = myWorkspaces,
      layoutHook = myLayout,
      normalBorderColor = colorNormalBorder,
      focusedBorderColor = colorFocusedBorder,
      borderWidth = 0,
      keys = myKeys,
      mouseBindings = newMouseBindings,
      startupHook = myStartUpHook,
      handleEventHook = fullscreenEventHook,
      manageHook = myManageHook
    }

main :: IO ()
main =
  xmonad $
    ewmh $
      docks myConfig
