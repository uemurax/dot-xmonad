import XMonad
import XMonad.Util.EZConfig(additionalKeys)

myMask = modMask defaultConfig

main = do
  xmonad $ defaultConfig
    { terminal = "urxvt"
    , startupHook = spawn "xscreensaver -no-splash"
    } `additionalKeys`
    [ ((myMask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
    ]

