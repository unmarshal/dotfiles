-- Dependencies: picom, mobar

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
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, swapNextScreen)

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
  spawnOnce "xset r rate 200 25"
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

  -- Workspaces
  , ("M-.", nextScreen)
  , ("M-,", prevScreen)

  -- Layouts
  , ("M-C-<Space>", sendMessage NextLayout)
  -- -- , ("M-S-=", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts) -- Toggles noborder/full
  -- -- , ("M-S-f", sendMessage (T.Toggle "float"))
  -- , ("M-S-x", sendMessage $ Toggle REFLECTX)
  -- , ("M-S-y", sendMessage $ Toggle REFLECTY)
  -- , ("M-S-m", sendMessage $ Toggle MIRROR)
  -- , ("M-<KP_Multiply>", sendMessage (IncMasterN 1))   -- Increase number of clients in the master pane
  -- , ("M-<KP_Divide>", sendMessage (IncMasterN (-1)))  -- Decrease number of clients in the master pane
  -- , ("M-S-<KP_Multiply>", increaseLimit)              -- Increase number of windows that can be shown
  -- , ("M-S-<KP_Divide>", decreaseLimit)                -- Decrease number of windows that can be shown

  -- Dmenu
  , ("M-<Space>", spawn "dmenu_run") ]

