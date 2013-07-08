{-
  xmonad config file.

  Written by Andy Holt.

  Borrows heavily from:
  + David Brewer (https://github.com/davidbrewer/xmonad-ubuntu-conf)
  + John Goerzen (http://www.haskell.org/haskellwiki/Xmonad/Config_archive/John_Goerzen's_Configuration)
-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
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

myManageHook = composeAll
  [ className =? "Gimp"      --> doFloat
  , className =? "Vncviewer" --> doFloat
  ]

myWorkspaces = ["1:mail","2:web","3:music","4:emacs","5","6","7","8","9"]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/adh/.xmobarrc"
  xmonad $ defaultConfig
    { focusedBorderColor = myFocusedBorderColor
    , normalBorderColor = myNormalBorderColor
    , borderWidth = myBorderWidth
    , terminal = myTerminal
    , modMask = myModMask
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
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
    } `additionalKeys`
    [ -- lock screen
        ((mod4Mask, xK_L), spawn "gnome-screensaver-command -l")
      , ((mod4Mask, xK_c), spawn "emacsclient -c -e '(org-capture)'") 
    ]
