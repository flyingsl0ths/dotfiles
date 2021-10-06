import qualified Data.Map as M
  ( Map,
    fromList,
    union,
  )
import LayoutUtils (rawSpacing')
import SpawnUtils (gamemodeCommand, makeNamedScratchPad, spawnSelected', zshTerminalCommand)
import System.Exit (exitSuccess)
import XMonad
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.GridSelect
  ( GSConfig (..),
    TwoD (..),
    cancel,
    colorRangeFromClassName,
    goToSelected,
    makeXEventhandler,
    move,
    select,
    setPos,
    shadowWithKeymap,
    substringSearch,
  )
import XMonad.Actions.MouseGestures (Direction2D (..), mouseGesture)
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
import XMonad.Layout.Accordion (Accordion (..))
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Hidden
  ( hiddenWindows,
    hideWindow,
    popOldestHiddenWindow,
  )
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.Renamed (Rename (Replace), renamed)
import XMonad.Layout.Simplest (Simplest)
import XMonad.Layout.Spacing
  ( Border (Border),
    Spacing,
    spacingRaw,
  )
import XMonad.Layout.Spiral
  ( Direction (East),
    Rotation (CW),
    SpiralWithDir,
    spiralWithDir,
  )
import XMonad.Layout.Tabbed
  ( TabbedDecoration,
    Theme,
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
import XMonad.ManageHook (className, (<+>))
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), NamedScratchpads, customFloating, namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.Ungrab (unGrab)

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:size=10:antialias=true"

myTerminal :: String
myTerminal = "kitty"

codeEditors :: [(String, String)]
codeEditors =
  [ ("Vim", zshTerminalCommand myTerminal "'vim'"),
    ("Android Studio", gamemodeCommand "android-studio"),
    ("VSCode", "vscodium"),
    ("Typora", "typora"),
    ("DrRacket", "drracket"),
    ("IntelliJ", "idea")
  ]

myWorkspaces :: [String]
myWorkspaces = ["I", "II", "III", "IV", "V", "VI"]

colorNormalBorder :: String
colorNormalBorder = "#4c566a"

colorFocusedBorder :: String
colorFocusedBorder = "#5e81ac"

tiledLayout :: Tall a
tiledLayout = Tall nmaster delta ratio
  where
    -- Default number of windows in the master pane
    nmaster = 1 :: Int

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2 :: Rational

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100 :: Rational

tallAccordionLayout :: ModifiedLayout Rename Accordion a
tallAccordionLayout = renamed [Replace "verticalAccordion"] Accordion

wideAccordionLayout :: ModifiedLayout Rename (Mirror Accordion) a
wideAccordionLayout = renamed [Replace "horizontalAccordion"] $ Mirror Accordion

threeColumnLayout :: ThreeCol a
threeColumnLayout = ThreeColMid nmaster delta ratio
  where
    -- Default number of windows in the master pane
    nmaster = 1 :: Int

    -- Default proportion of screen occupied by master pane
    ratio = 1 / 2 :: Rational

    -- Percent of screen to increment by when resizing panes
    delta = 3 / 100 :: Rational

spiralLayout :: SpiralWithDir a
spiralLayout = spiralWithDir cardinalDirection spiralDirection windowSizeRation
  where
    cardinalDirection = East :: Direction
    spiralDirection = CW :: XMonad.Layout.Spiral.Rotation
    windowSizeRation = (6 / 7) :: Rational

tabbedLayout :: ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest Window
tabbedLayout = tabbed shrinkText myTabConfig
  where
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
        ||| tabbedLayout
        ||| resizeableLayouts
  where
    resizeableLayouts =
      rawSpacing'
        10
        ( tiledLayout
            ||| Mirror tiledLayout
            ||| tallAccordionLayout
            ||| wideAccordionLayout
            ||| threeColumnLayout
            ||| spiralLayout
        )

windowGridSelectionConfig :: GSConfig Window
windowGridSelectionConfig =
  def
    { gs_cellheight = 150,
      gs_cellwidth = 150,
      gs_navigate = myGridNavigation,
      gs_font = myFont,
      gs_colorizer = myColorizer
    }
  where
    myColorizer :: Window -> Bool -> X (String, String)
    myColorizer =
      colorRangeFromClassName
        polar -- lowest inactive bg
        frost -- highest inactive bg
        polar -- active bg
        snow -- inactive fg
        snow -- active fg
      where
        -- Based off nord color palette
        frost = (0x5E, 0x81, 0xAC)
        polar = (0x2E, 0x34, 0x40)
        snow = (0xD8, 0xDE, 0xE9)

    myGridNavigation :: TwoD a (Maybe a)
    myGridNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
      where
        navKeyMap =
          M.fromList
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

myScratchPads :: NamedScratchpads
myScratchPads =
  [ makeNamedScratchPad "terminal" "scratchpad" "alacritty --class scratchpad" overwrittenByConfig,
    makeNamedScratchPad "terminalfm" "fmscratchpad" "alacritty --class fmscratchpad -e zsh -i -c ranger" overwrittenByConfig,
    makeNamedScratchPad "yt-music" "ytmscratchpad" "alacritty --class ytmscratchpad -e ytfzf -m -t -l" overwrittenByConfig,
    makeNamedScratchPad "bpy" "pyscratchpad" "alacritty --class ytmscratchpad -e /home/flyingsl0ths/.local/bin/bpython" overwrittenByConfig,
    makeNamedScratchPad "emoji-picker" "Org.gnome.Characters" "gnome-characters" overwrittenByConfig
  ]
  where
    overwrittenByConfig = W.RationalRect 0 0 0 0

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} =
  M.fromList $
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
      ((modMask, xK_g), spawn (gamemodeCommand "gimp")),
      ((modMask, xK_a), spawn (gamemodeCommand "android-studio")),
      ((modMask .|. shiftMask, xK_t), spawn "xfce4-taskmanager")
    ]
      ++ [
           -----------------
           -- Scratchpads --
           -----------------
           ((modMask .|. shiftMask, xK_l), namedScratchpadAction myScratchPads "terminal"),
           ((modMask .|. shiftMask, xK_f), namedScratchpadAction myScratchPads "terminalfm"),
           ((modMask .|. shiftMask, xK_y), namedScratchpadAction myScratchPads "yt-music"),
           ((modMask .|. shiftMask, xK_e), namedScratchpadAction myScratchPads "emoji-picker"),
           ((modMask .|. shiftMask, xK_p), namedScratchpadAction myScratchPads "bpy"),
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
           ((modMask .|. controlMask, xK_w), goToSelected windowGridSelectionConfig),
           ((modMask .|. controlMask, xK_e), spawnSelected' codeEditors),
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
          spawn "xmonad --recompile && xmonad --restart"
        )
      ]
      ++
      -- mod-[1..9] %! Switch to workspace N
      -- mod-shift-[1..9] %! Move client to workspace N
      [ ((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      -- mod-{[,],|} %! Switch to physical/Xinerama screens 1, 2, or 3
      -- mod-shift-{[,],|} %! Move client to screen 1, 2, or 3
      [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_bracketleft, xK_bracketright, xK_bar] [0 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
      ]

-----------------
--
myMouse :: XConfig l -> [((KeyMask, Button), Window -> X ())]
myMouse XConfig {XMonad.modMask = modMask} =
  [ -- Enables resizing a window from any corner
    ((modMask .|. shiftMask, button1), \w -> focus w >> Flex.mouseResizeWindow w)
  ]

newMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
newMouseBindings xc = M.union (mouseBindings def xc) (M.fromList (myMouse xc))

myStartUpHook :: X ()
myStartUpHook = setWMName "LG3D" >> spawn "~/.xmonad/startup.sh"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ className =? "Gimp" --> doFloat,
      className =? "Org.gnome.Characters" --> doFloat,
      className =? "gammy" --> doCenterFloat,
      className =? "Alacritty" --> doCenterFloat,
      isDialog --> doCenterFloat
    ]
    <+> namedScratchpadManageHook myScratchPads

myConfig =
  def
    { modMask = mod4Mask,
      terminal = myTerminal,
      workspaces = myWorkspaces,
      layoutHook = myLayout,
      normalBorderColor = colorNormalBorder,
      focusedBorderColor = colorFocusedBorder,
      borderWidth = 3,
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
