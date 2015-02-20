import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

myMask = modMask defaultConfig

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , logHook = dynamicLogWithPP xmobarPP
                  { ppOutput = hPutStrLn xmproc
                  , ppTitle = xmobarColor "green" "" . shorten 50
                  }
    , startupHook = spawn "xscreensaver -no-splash"
    } `additionalKeys`
    [ ((myMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    ]

