import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myManageHook = composeAll
  [ className =? "Gimp"      --> doFloat
  , className =? "Vncviewer" --> doFloat
  ]

myTerminal = "terminator"

myWorkspaces = ["1:mail","2:web","3:music","4:emacs","5","6","7","8","9"]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/adh/.xmobarrc"
  xmonad $ defaultConfig
    { manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = avoidStruts  $  layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "green" "" . shorten 50
            }
    , modMask = mod4Mask     -- rebind Mod to the Windows key
    , workspaces = myWorkspaces
    , startupHook = do
        spawn "~/.xmonad/startup-hook"
    } `additionalKeys`
    [ -- lock screen
        ((mod4Mask, xK_L), spawn "gnome-screensaver-command -l")
      , ((mod4Mask, xK_c), spawn "emacsclient -c -e '(org-capture)'") 
    ]
