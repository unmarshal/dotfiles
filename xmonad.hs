-- Dependencies: picom, xmobar

import XMonad
import XMonad.Config.Desktop
import Data.Monoid
import Data.Maybe (isJust)
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import Text.Printf
import qualified XMonad.StackSet as W

-- Utilities
import XMonad.Util.Loggers
import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, defaultPP, wrap, pad, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.ManageDocks (avoidStruts, docksStartupHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)
import XMonad.Hooks.Place (placeHook, withGaps, smart)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops   -- required for xcomposite in obs to work

-- Actions
import XMonad.Actions.Minimize (minimizeWindow)
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import XMonad.Actions.WithAll (sinkAll, killAll)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen, shiftNextScreen, shiftPrevScreen, swapNextScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.DynamicWorkspaces (addWorkspacePrompt, removeEmptyWorkspace)
import XMonad.Actions.MouseResize
import qualified XMonad.Actions.ConstrainedResize as Sqr

-- Layouts modifiers
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.Spacing (spacing)
import XMonad.Layout.NoBorders
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.OneBig
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ResizableTile
import XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import XMonad.Layout.IM (withIM, Property(Role))

-- Prompts
import XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))

myFont = "xfg:JetBrainsMono Nerd Font:regular:pixelsize=12"
myModMask = mod4Mask
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
        , layoutHook = myLayoutHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
        , modMask = myModMask
        } `additionalKeysP` myKeys

---AUTOSTART
myStartupHook = do
  spawnOnce "picom&"
  spawnOnce "stalonetray&"
  spawnOnce "xset r rate 200 25"
  spawnOnce "feh --bg-scale ~/dotfiles/bg/cityscape.jpg"
  --spawnOnce "syndaemon -i 0.1 -t -K -R -d" -- disable mouse input for 100 ms on keypress
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
  , ("M4-l", spawn "slock /usr/sbin/s2ram")

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
  , ("M-C-<Down>", sendMessage NextLayout)
  , ("M-C-<Up>", sendMessage FirstLayout)
  , ("M-S-f", sendMessage ToggleStruts) -- Toggles struts

  -- Dmenu
  , ("M-<Space>", spawn $ printf "dmenu_run -fn '%s'" myFont)

  -- Brightness
  , ("<XF86MonBrightnessUp>", spawn "lux -a 10%")
  , ("<XF86MonBrightnessDown>", spawn "lux -s 10%")

  -- Volume
  , ("<XF86AudioMute>", spawn "amixer set Speaker toggle && amixer set Master toggle")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")

  -- Screenshot
  , ("<Print>", spawn "scrot -d 2")

  -- Grid Select
  , (("M-S-o"), spawnSelected'
    [ ("Brave", "brave-browser")
    , ("Signal", "signal-desktop")
    , ("Pavucontrol", "pavucontrol")
    , ("Spotify", "spotify")
    ])

  , ("M-S-g", goToSelected $ mygridConfig myColorizer)
  , ("M-S-b", bringSelected $ mygridConfig myColorizer)
  ]

---GRID SELECT
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
                  (0x31,0x2e,0x39) -- lowest inactive bg
                  (0x31,0x2e,0x39) -- highest inactive bg
                  (0x61,0x57,0x72) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0xff,0xff,0xff) -- active fg

-- gridSelect menu layout
mygridConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight   = 30
    , gs_cellwidth    = 200
    , gs_cellpadding  = 8
    , gs_originFractX = 0.5
    , gs_originFractY = 0.5
    , gs_font         = myFont
    }

spawnSelected' :: [(String, String)] -> X ()
spawnSelected' lst = gridselect conf lst >>= flip whenJust spawn
    where conf = defaultGSConfig

myLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
  mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout
  where
    myDefaultLayout = tall ||| grid ||| threeCol ||| threeRow ||| oneBig ||| noBorders monocle ||| space ||| floats

tall       = renamed [Replace "Tall"] $ limitWindows 12 $ spacing 6 $ ResizableTall 1 (3/100) (1/2) []
grid       = renamed [Replace "Grid"] $ limitWindows 12 $ spacing 6 $ mkToggle (single MIRROR) $ Grid (16/10)
threeCol   = renamed [Replace "3Col"] $ limitWindows 3  $ ThreeCol 1 (3/100) (1/2)
threeRow   = renamed [Replace "3Row"] $ limitWindows 3  $ Mirror $ mkToggle (single MIRROR) zoomRow
oneBig     = renamed [Replace "OneBig"] $ limitWindows 6  $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (5/9) (8/12)
monocle    = renamed [Replace "Full"]  $ limitWindows 20 $ Full
space      = renamed [Replace "Space"] $ limitWindows 4  $ spacing 12 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
floats     = renamed [Replace "Floats"] $ limitWindows 20 $ simplestFloat
