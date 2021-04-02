-- Dependencies: picom, xmobar

import XMonad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.SpawnOnce
import XMonad.Config.Desktop
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

import System.IO

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops -- required for compositor

-- Actions
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WithAll (sinkAll, killAll)

-- Font not currently used
myFont = "xfg:JetBrainsMono Nerd Font:regular:pixelsize=12"
myModMask = mod1Mask -- set mod key to alt
myTerminal = "alacritty"
myBorderWidth = 3
myNormalBorderColor = "#292d3e"
myFocusedBorderColor = "#bbc5ff"

main = do
    xmproc <- spawnPipe "xmobar"

    xmonad $ ewmh desktopConfig
        { terminal   = myTerminal
        , borderWidth = myBorderWidth
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , startupHook = myStartupHook
        , manageHook = manageDocks <+> manageHook defaultConfig
        , layoutHook = avoidStruts  $  layoutHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = mod1Mask
        } `additionalKeysP` myKeys

---AUTOSTART
myStartupHook = do
  spawnOnce "picom&"
  spawnOnce "stalonetray&"
  spawnOnce "xset r rate 200 25"
  spawnOnce "feh --bg-scale ~/dotfiles/bg/cityscape.jpg"
  spawnOnce "syndaemon -i 0.1 -t -K -R -d" -- disable mouse input for 100 ms on keypress
  spawnOnce "dropbox start"
  setWMName "XMonad"

---KEYBINDINGS
myKeys =
  -- Xmonad
  [ ("M-C-r", spawn "xmonad --compile")         -- Recompiles xmonad
  , ("M-S-r", spawn "xmonad --restart")         -- Restarts xmonad
  , ("M-S-q", io exitSuccess)                   -- Quits xmonad

  -- Windows
  , ("M-S-c", kill1)                            -- Kill currently focused client
  , ("M-S-a", killAll)                          -- Kill all windows in current workspace

  -- Floating windows
  , ("M-<Delete>", withFocused $ windows . W.sink)  -- Push floating window back to tile.
  , ("M-S-<Delete>", sinkAll)                  -- Push ALL floating windows back to tile.

  -- Windows navigation
  , ("M-m", windows W.focusMaster)             -- Move focus to the master window
  , ("M-j", windows W.focusDown)               -- Move focus to the next window
  , ("M-k", windows W.focusUp)                 -- Move focus to the prev window
  , ("M-S-m", windows W.swapMaster)            -- Swap the focused window and the master window
  , ("M-S-j", windows W.swapDown)              -- Swap the focused window with the next window
  , ("M-S-k", windows W.swapUp)                -- Swap the focused window with the prev window
  , ("M-<Backspace>", promote)                 -- Moves focused window to master, all others maintain order
  , ("M1-S-<Tab>", rotSlavesDown)              -- Rotate all windows except master and keep focus in place
  , ("M1-C-<Tab>", rotAllDown)                 -- Rotate all the windows in the current stack
  , ("M-S-s", windows copyToAll)
  , ("M-C-s", killAllOtherCopies)

  -- Layouts
  , ("M-C-<Space>", sendMessage NextLayout)

  -- Dmenu
  , ("M-<Space>", spawn "dmenu_run")

  -- Brightness
  , ("<XF86MonBrightnessUp>", spawn "lux -a 10%")
  , ("<XF86MonBrightnessDown>", spawn "lux -s 10%")

  -- Volume
  , ("<XF86AudioMute>", spawn "amixer set Speaker toggle && amixer set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
  ]

