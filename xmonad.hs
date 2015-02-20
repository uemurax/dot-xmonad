import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)

myMask = modMask defaultConfig

main = do
  xmobar <- spawnPipe "xmobar"
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , manageHook = manageDocks <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ layoutHook defaultConfig
    , startupHook = spawn "xscreensaver -no-splash"
    } `additionalKeys`
    [ ((myMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    ]

