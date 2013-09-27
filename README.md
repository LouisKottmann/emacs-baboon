# Louis "Baboon" Kottmann's EMACS configuration

## Installation

First you need to install EMACS:
* [Debian/LMDE](http://emacs.naquadah.org/) (pick "stable")
* Ubuntu/Mint: ?

Then you need to install [Prelude](https://github.com/bbatsov/prelude)

Also, SBCL is required for [SLIME](http://www.cliki.net/SLIME%20Features) to work properly. On Debian just do `sudo apt-get install sbcl`.

Finally, fetch this repo into `~/.emacs.d/personal/`

The external libraries it depends on are linked as git submodules, so do a quick `git submodule update` and you're set.

## Usage

### The easy way

Clone [my bash repo](https://bitbucket.org/louis_kottmann/bash) with yummy aliases and environment variables then use the `em` command

### The usual way

Run `emacs --daemon` once (may be done at startup like in a systemd service)

Then use `nohup emacsclient -c &` to enter a session.

or, a bit better (but hard to remember):
run `emacsclient -c -n -a "" -F "((fullscreen . maximized))"`
it will start the daemon if necessary and run itself daemonized
