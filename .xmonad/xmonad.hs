import qualified Data.Map as M
  ( Map,
    fromList,
    union,
  )
import LayoutUtils (rawSpacing')
import SpawnUtils (gamemodeCommand, makeNamedScratchPad, spawnSelected', zshTerminalCommand)
import System.Exit (exitSuccess)
import XMonad
  ( Button,
    ButtonMask,
    ChangeLayout (NextLayout),
    Default (def),
    Full (Full),
    IncMasterN (IncMasterN),
    KeyMask,
    KeySym,
    Layout,
    ManageHook,
    Mirror (..),
    Resize (Expand, Shrink),
    Tall (Tall),
    Window,
    X,
    XConfig
      ( XConfig,
        borderWidth,
        focusedBorderColor,
        handleEventHook,
        keys,
        layoutHook,
        manageHook,
        modMask,
        mouseBindings,
        normalBorderColor,
        startupHook,
        terminal,
        workspaces
      ),
    button1,
    className,
    composeAll,
    controlMask,
    doFloat,
    focus,
    io,
    kill,
    mod4Mask,
    refresh,
    screenWorkspace,
    sendMessage,
    setLayout,
    shiftMask,
    spawn,
    whenJust,
    windows,
    withFocused,
    xK_1,
    xK_9,
    xK_Down,
    xK_Escape,
    xK_F2,
    xK_F3,
    xK_F6,
    xK_F7,
    xK_F8,
    xK_Left,
    xK_Return,
    xK_Right,
    xK_Tab,
    xK_Up,
    xK_a,
    xK_backslash,
    xK_bar,
    xK_bracketleft,
    xK_bracketright,
    xK_c,
    xK_comma,
    xK_e,
    xK_f,
    xK_g,
    xK_h,
    xK_j,
    xK_k,
    xK_l,
    xK_m,
    xK_n,
    xK_p,
    xK_period,
    xK_q,
    xK_r,
    xK_s,
    xK_slash,
    xK_space,
    xK_t,
    xK_v,
    xK_w,
    xK_x,
    xK_y,
    xK_z,
    xmonad,
    (-->),
    (.|.),
    (<+>),
    (=?),
    (|||),
  )
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.MouseGestures (Direction2D (..), mouseGesture)
import XMonad.Actions.WithAll (killAll)
import XMonad.Core (XConfig)
import XMonad.Hooks.EwmhDesktops
  ( ewmh,
  )
import XMonad.Hooks.ManageDocks
  ( avoidStruts,
    docks,
  )
import XMonad.Hooks.ManageHelpers
  ( doCenterFloat,
    isDialog,
  )
import XMonad.Layout.Accordion (Accordion (..))
import XMonad.Layout.Decoration (Decoration, DefaultShrinker)
import XMonad.Layout.Hidden
  ( hiddenWindows,
    hideWindow,
    popOldestHiddenWindow,
  )
import XMonad.Layout.LayoutModifier (ModifiedLayout (..))
import XMonad.Layout.NoBorders (noBorders)
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
import XMonad.Layout.ThreeColumns (ThreeCol (ThreeColMid))
import XMonad.ManageHook (className, (<+>), (=?))
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), NamedScratchpads, customFloating, namedScratchpadAction, namedScratchpadManageHook)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Ungrab (unGrab)

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:size=10:antialias=true"

myTerminal :: String
myTerminal = "alacritty"

myScratchPads :: NamedScratchpads
myScratchPads =
  [ makeNamedScratchPad "terminal" "scratchpad" "alacritty --class scratchpad" floatingTerminalSize,
    makeNamedScratchPad "terminalfm" "fmscratchpad" "alacritty --class fmscratchpad -e zsh -i -c ranger" floatingTerminalSize,
    makeNamedScratchPad "yt-music" "ytmscratchpad" "alacritty --class ytmscratchpad -e ytfzf -m -t -l" floatingTerminalSize,
    makeNamedScratchPad "bpy" "pyscratchpad" "alacritty --class pyscratchpad -e /home/flyingsl0ths/.local/bin/bpython" floatingTerminalSize,
    makeNamedScratchPad "emoji-picker" "org.gnome.Characters" "gnome-characters" floatingWindowSize
  ]
  where
    floatingTerminalSize = W.RationalRect (1 / 6) (1 / 6) (2 / 3) (2 / 3)
    floatingWindowSize = W.RationalRect 0 0 (1 / 3) (1 / 2)

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

myLayout =
  hiddenWindows $
    avoidStruts $
      noBorders Full
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

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} =
  M.fromList $
    ---- Programs ----
    [ ((modMask, xK_w), spawn "librewolf"),
      ((modMask .|. shiftMask, xK_w), spawn "librewolf --private-window"),
      ((modMask .|. shiftMask, xK_g), spawn "gammy"),
      ((modMask, xK_f), spawn "pcmanfm"),
      ((modMask, xK_v), spawn "vscodium"),
      ((modMask, xK_c), spawn "corectrl"),
      ((modMask, xK_z), spawn "zathura"),
      ((modMask, xK_p), spawn "typora"),
      ((modMask, xK_y), spawn (gamemodeCommand "yuzu")),
      ((modMask, xK_g), spawn (gamemodeCommand "gimp")),
      ((modMask, xK_a), spawn (gamemodeCommand "android-studio")),
      ((modMask .|. shiftMask, xK_y), spawn (gamemodeCommand "yuzu")),
      ((modMask .|. shiftMask, xK_t), spawn "xfce4-taskmanager")
    ]
      ++ [
           -----------------
           -- Scratchpads --
           -----------------
           ((modMask .|. controlMask, xK_Return), namedScratchpadAction myScratchPads "terminal"),
           ((modMask .|. controlMask, xK_f), namedScratchpadAction myScratchPads "terminalfm"),
           ((modMask .|. controlMask, xK_y), namedScratchpadAction myScratchPads "yt-music"),
           ((modMask .|. controlMask, xK_m), namedScratchpadAction myScratchPads "emoji-picker"),
           ((modMask .|. controlMask, xK_p), namedScratchpadAction myScratchPads "bpy"),
           ---- Groups -----
           ((modMask .|. controlMask, xK_c), spawn "vscodium; zathura"),
           ((modMask .|. shiftMask, xK_c), spawn (myTerminal ++ " &") >> spawn "zathura")
           -----------------
         ]
      ++ [
           -- Tools ---
           ((modMask, xK_r), spawn dmenu),
           ((modMask, xK_e), spawn "~/.local/bin/editors"),
           ((modMask, xK_s), unGrab *> spawn "xfce4-screenshooter"),
           ((modMask .|. shiftMask, xK_n), spawn "networkmanager_dmenu"),
           ((modMask .|. shiftMask, xK_p), spawn "~/.config/rofi/applets/android/powermenu.sh"),
           ((modMask .|. shiftMask, xK_a), spawn "~/.config/rofi/launchers/ribbon/launcher.sh"),
           ((modMask .|. controlMask, xK_l), spawn "xdg-screensaver lock"),
           ((modMask, xK_F3), spawn "brightnessctl -q s 5%+"),
           ((modMask, xK_F2), spawn "brightnessctl -q s 5%-"),
           ((modMask, xK_F8), spawn "pamixer -i 5"),
           ((modMask, xK_F7), spawn "pamixer -d 5"),
           ((modMask, xK_F6), spawn "pamixer -t")
           ------------
         ]
      ++ [
           -- Addtional WM Actions --
           ((modMask, xK_backslash), withFocused hideWindow),
           ((modMask .|. shiftMask, xK_backslash), popOldestHiddenWindow),
           -- Kill active window
           ((modMask, xK_q), kill),
           -- Kill all windows on current workspace
           ((modMask .|. shiftMask, xK_x), killAll)
           --------------------------
         ]
      ++
      -- WM Actions ----
      [ ((modMask, xK_Return), spawn $ XMonad.terminal conf),
        -- %! Rotate through the available layout algorithms
        ((modMask, xK_Tab), sendMessage NextLayout),
        -- %!  Reset the layouts on the current workspace to default
        ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf),
        -- %! Resize viewed windows to the correct size
        ((modMask, xK_n), refresh),
        -- move focus up or down the window stack
        ---- %! Move focus to the next window
        ((modMask, xK_space), windows W.focusDown),
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
  where
    dmenu =
      "dmenu_run -p 'λ' -m 0 -fn 'JetBrainsMonoMedium Nerd Font Mono:size=13' "
        ++ "-nb '#2E3440' -nf '#D8DEE9' -sb '#81A1C1' -sf '#E5E9F0'"

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
      manageHook = myManageHook
    }
  where
    myWorkspaces :: [String]
    myWorkspaces = ["I", "II", "III", "IV", "V", "VI"]

    colorNormalBorder :: String
    colorNormalBorder = "#2E3440"

    colorFocusedBorder :: String
    colorFocusedBorder = "#5E81AC"

    myMouse :: XConfig l -> [((KeyMask, Button), Window -> X ())]
    myMouse XConfig {XMonad.modMask = modMask} =
      [ -- Enables resizing a window from any corner
        ((modMask .|. shiftMask, button1), \w -> focus w >> Flex.mouseResizeWindow w)
      ]

    newMouseBindings :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
    newMouseBindings xc = M.union (mouseBindings def xc) (M.fromList (myMouse xc))

    myStartUpHook :: X ()
    myStartUpHook =
      spawnOnce "/usr/lib/xfce4/notifyd/xfce4-notifyd &"
        >> spawnOnce "/usr/lib/xfce-polkit/xfce-polkit &"
        >> spawnOnce "/usr/bin/xautolock -time 10 -locker ~/.local/bin/i3lock-fancy-rapid -detectsleep &"
        >> spawnOnce "~/.config/polybar/launch.sh 'xmonad'"
        >> spawnOnce "(nohup picom &)"
        >> spawnOnce "xinput set-prop 12 316 1"
        >> spawnOnce "gammy &"
        >> spawnOnce "xwallpaper --output eDP --stretch ~/.local/share/wallpaper/oregairu.png"
        >> spawnOnce "xsettingsd &"

    myManageHook :: ManageHook
    myManageHook =
      composeAll
        [ className =? "Gimp" --> doFloat,
          className =? "screengrab" --> doFloat,
          className =? "gammy" --> doCenterFloat,
          className =? "jetbrains-studio" --> doCenterFloat,
          isDialog --> doCenterFloat
        ]
        <+> namedScratchpadManageHook myScratchPads

main :: IO ()
main =
  xmonad $
    ewmh $
      docks myConfig
