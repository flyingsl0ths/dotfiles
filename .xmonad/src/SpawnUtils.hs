module SpawnUtils (gamemodeCommand, zshTerminalCommand, spawnSelected') where

import XMonad (X, def, spawn, whenJust)
import XMonad.Actions.GridSelect (GSConfig (..), gridselect)

myFont :: String
myFont = "xft:JetBrainsMono Nerd Font:size=10:antialias=true"

zshTerminalCommand :: String -> String -> String
zshTerminalCommand terminalName commandName = terminalName ++ " zsh -i -c " ++ commandName

gamemodeCommand :: String -> String
gamemodeCommand commandName = "~/.local/bin/game_mode " ++ commandName

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
