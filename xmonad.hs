{-
  xmonad config file.

  Written by Andy Holt.

  Borrows heavily from:
  + David Brewer (https://github.com/davidbrewer/xmonad-ubuntu-conf)
  + John Goerzen (http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration)
-}

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Grid
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.Fullscreen
import XMonad.Util.Run
import XMonad.Util.EZConfig
import XMonad.Actions.Plane
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
import System.IO

{-
  Xmonad config variables
-}

myModMask            = mod4Mask     -- rebind mod to the Windows key
myFocusedBorderColor = "#ff0000"    -- focused border to red
myNormalBorderColor  = "#ccccc"     -- inactive border dark grey
myBorderWidth        = 1            -- width of border is 1 pixel
myTerminal           = "terminator" -- use terminator as default terminal

{-
  Xmobar configuration variables. Control the appearance of text which xmonad
  sends to xmobar via the dynamiclog hook. Main xmobar config is in xmobarrc
  file.
-}

myTitleColor     = "#eeeeee"  -- color of window title light grey
myTitleLength    = 80         -- truncate window title to this length
myCurrentWSColor = "#e6744c"  -- color of active workspace
myVisibleWSColor = "#c185a7"  -- color of inactive workspace
myUrgentWSColor  = "#cc0000"  -- color of workspace with 'urgent' window
myCurrentWSLeft  = "["        -- wrap current workspace with these
myCurrentWSRight = "]"
myVisibleWSRight = "("        -- wrap inactive workspace with these
myVisibleWSLeft  = ")"
myUrgentWSLeft   = "{"        -- wrap urgent workspace with these
myUrgentWSRight  = "}"

{-
  Workspace configuration.

  Set names of workspaces etc.
-}

myWorkspaces =
  [
    "7:Chat",   "8",        "9",
    "4:Music",  "5:Emacs",  "6",
    "1:Term",   "2:Mail",   "3:Web",
    "0:VM",     "Extr1",    "Extr2"
  ]

startupWorkspace = "1:Term"  -- this is where to start after launch

{-
  Layout configuration. In this section, we identify which xmonad layouts to
  use.
  List of default layouts are applied to every window.
  Special layouts are applied to specific layouts.

  Note: all layouts are wrapped in "avoidStruts" - this makes the layouts avoid
  the status bar at the top of the screen. Behaviour can be toggled with
  keybinding.
-}

-- Define the group of default layouts, in the order they appear.
-- "smartBorders" modifier makes the borders only appear if there is more than
-- one visible window.
defaultLayouts = smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on left and remaining
  -- windows tile on the RHS. By default, each area takes up half of screen, but
  -- can be resized using h and l keys.
  ResizableTall 1 (3/100) (1/2) []

  -- Mirrored variation of ResizableTall - large master window on top, remaining
  -- windows tile at bottom of the screen. Resized in same way.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [] )

  -- Full layout makes every window full screen.
  ||| noBorders Full

  -- Grid layout tried to equally distribute windows in the availiable space,
  -- increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid

  -- ThreeColMid layout puts the large master window in the center of the
  -- screen. As configured, by default it takes up 3/4 of the availiable space.
  ||| ThreeColMid 1 (3/100) (3/4)

  -- Circle layout places the master window in the centre of the screen.
  -- Remaining windows appear in a circle around it
  ||| Circle))

myKeyBindings =
  [
    ((myModMask, xK_f), sendMessage ToggleStruts)
  , ((myModMask, xK_a), sendMessage MirrorShrink)
  , ((myModMask, xK_z), sendMessage MirrorShrink)
  , ((myModMask, xK_s), spawn "synapse")
--  , ((myModMask, xK_u), focusUrgent)
  , ((0, 0x1008ff12), spawn "amixer -D pulse set Master toggle")
  , ((0, 0x1008ff11), spawn "amixer -q set Master 5%-")
  , ((0, 0x1008ff13), spawn "amixer -q set Master 5%+")
  , ((mod4Mask, xK_w), spawn "gnome-screensaver-command -l")
  , ((mod4Mask, xK_c), spawn "emacsclient -c -e '(org-capture)'") 
    ]

{-
  Management hooks. Enforce certain behaviours to certain programs or
  windows.
-}

myManagementHooks :: [ManageHook]
myManagementHooks = [
    resource =? "synapse" --> doIgnore
  , className =? "Gimp"   --> doFloat
  ]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/adh/.xmobarrc"
  xmonad $ defaultConfig
    { focusedBorderColor = myFocusedBorderColor
    , normalBorderColor = myNormalBorderColor
    , borderWidth = myBorderWidth
    , terminal = myTerminal
    , modMask = myModMask
    , workspaces = myWorkspaces
    , manageHook = manageHook defaultConfig
                   <+> composeAll myManagementHooks
                   <+> manageDocks
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP $ xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
            , ppCurrent = xmobarColor myCurrentWSColor ""
                          . wrap myCurrentWSLeft myCurrentWSRight
            , ppVisible = xmobarColor myVisibleWSColor ""
                          . wrap myVisibleWSLeft myVisibleWSRight
            , ppUrgent = xmobarColor myUrgentWSColor ""
                          . wrap myUrgentWSLeft myUrgentWSRight
            }
    , startupHook = do
        spawn "~/.xmonad/startup-hook"
    }
      `additionalKeys` myKeyBindings
