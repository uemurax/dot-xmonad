.xmonad
=======

Configurations for [xmonad](http://xmonad.org).

Install
-------

    $ git clone https://github.com/uemurax/dot-xmonad ~/.xmonad

Put, for example, the following lines in `~/.xmonad/xmonad.hs`.

    import XMonad
    import XMonad.Config.MyConfig
    
    main = xmonad myConfig

Then execute:

    $ xmonad --recompile

Features
--------

### Hit a Hint ###

We can use *hints*,
inspired by [Vimperator](http://www.vimperator.org/),
to focus a window.
Try `M-f`.

Copyright and License
---------------------

Copyright (c) 2015 - 2020 Taichi Uemura <t.uemura00@gmail.com>

License: MIT
