module SpawnUtils (gamemodeCommand, zshTerminalCommand, spawnSelected', makeNamedScratchPad) where

import XMonad (X, def, spawn, whenJust)
import XMonad.Actions.GridSelect (GSConfig (..), gridselect)
import XMonad.ManageHook (appName, (=?))
import XMonad.StackSet (RationalRect (..))
import XMonad.Util.NamedScratchpad (NamedScratchpad (NS), customFloating, namedScratchpadAction, namedScratchpadManageHook)

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:size=10:antialias=true"

zshTerminalCommand :: String -> String -> String
zshTerminalCommand terminalName command = terminalName ++ " zsh -i -c " ++ command

gamemodeCommand :: String -> String
gamemodeCommand command = "~/.local/bin/game_mode " ++ command

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' programs = gridselect conf programs >>= flip whenJust spawn
  where
    conf =
      def
        { gs_cellheight = 40,
          gs_cellwidth = 200,
          gs_cellpadding = 6,
          gs_originFractX = 0.5,
          gs_originFractY = 0.5,
          gs_font = myFont
        }

makeNamedScratchPad :: String -> String -> String -> RationalRect -> NamedScratchpad
makeNamedScratchPad scratchpadName windowClassName command windowDimensions =
  NS scratchpadName spawnScratchpad findScratchpad manageScratchpad
  where
    spawnScratchpad = command
    findScratchpad = appName =? windowClassName
    manageScratchpad = customFloating windowDimensions
