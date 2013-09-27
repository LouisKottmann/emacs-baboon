# Louis "Baboon" Kottmann's EMACS configuration

## Installation

First you need to install EMACS:\n
    - [Debian/LMDE](http://emacs.naquadah.org/) (pick "stable")\n
    - Ubuntu/Mint: ?

Then you need to install [Prelude](https://github.com/bbatsov/prelude)

Also, SBCL is required for [SLIME](http://www.cliki.net/SLIME%20Features) to work properly. On Debian just do `sudo apt-get install sbcl`.

Finally, fetch this repo into `~/.emacs.d/personal/`

The external libraries it depends on are linked as git submodules when applicable, so do a quick `git submodule update` and you're set. Slime does not have a git repo, it uses cvs, check out [this page](http://www.cliki.net/SLIME-HOWTO) to update it.

## Usage

Run `emacs --daemon` once (may be done at startup like in a systemd service)

Then use `nohup emacsclient -c &` to enter a session.
