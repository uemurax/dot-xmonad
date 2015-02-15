import XMonad

main = do
xmonad $ defaultConfig
  { terminal = "urxvt"
  , startupHook = spawn "xscreensaver -no-splash"
  }

