.xmonad
=======

Configurations for [xmonad](http://xmonad.org).

Install
-------

    $ git clone https://github.com/uemurax/dot-xmonad ~/.xmonad

Put, for example, the following lines in `~/.xmonad/xmonad.hs`.

    import XMonad
    import XMonad.Config.MyConfig ( mkMyConfig )
    
    main = xmonad $ mkMyConfig def

Then execute:

    $ xmonad --recompile

Features
--------

### Hit a Hint ###

We can use *hints*,
inspired by [Vimperator](http://www.vimperator.org/),
to focus a window.
Try `M-f`.

(Optional) Dependencies
-----------------------

*   `xcompmgr` and `transset`
:   to make windows transparent.
    Add `xcompmgr &` in your `~/.xinitrc`.
*   `imagemagick`
:   to take a screenshot

Copyright and License
---------------------

Copyright (c) 2015 - 2017 Taichi Uemura <t.uemura00@gmail.com>

License: MIT
