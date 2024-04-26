# guile-hello

[![GNU Guile 3.0](https://github.com/padawanphysicist/guile-hello/actions/workflows/guile3.0.yml/badge.svg)](https://github.com/padawanphysicist/guile-hello/actions/workflows/guile3.0.yml)

`guile-hello` is a toy module for Guile, to use as a project template.

# Installation

If you are cloning the repository make sure you run the `bootstrap` script
first:

    $ ./bootstrap

Then, run the typical sequence:

    $ ./configure --prefix=<guile-prefix>
    $ make
    $ sudo make install

Where `<guile-prefix>` should preferably be the same as your system Guile
installation directory (e.g. /usr).

If everything installed successfully you should be up and running:

    $ guile
    scheme@(guile-user)> (use-modules (hello))
    scheme@(guile-user)> (say)
    "Hello World"

It might be that you installed guile-hello somewhere differently than your
system's Guile. If so, you need to indicate Guile where to find guile-hello, for
example:

    $ GUILE_LOAD_PATH=/usr/local/share/guile/site guile

# Usage

guile-hello provides two procedure: `say` and `info`. `say` displays the message
`Hello World!` and `info` shows the directory containing the source code of the
package. It was included to show how to use the substitution rule of automake.

You can use the procedures by loading the module:

    scheme@(guile-user)> (use-modules (hello))

# License

Copyright (C) 2024 Victor Santos <victor_santos@fisica.ufc.br>

guile-hello is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

guile-hello is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with guile-hello. If not, see https://www.gnu.org/licenses/.
