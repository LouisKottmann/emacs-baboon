# Louis "Baboon" Kottmann's EMACS configuration

## Installation

1) Install EMACS:

* [Debian/LMDE](http://emacs.naquadah.org/) (pick "stable")
* [Ubuntu/Min]( https://launchpad.net/~cassou/+archive/emacs)

.

    add-apt-repository ppa:cassou/emacs
    apt-get update
    apt-get install emacs-snapshot-el emacs-snapshot-gtk emacs-snapshot

2) Install [Prelude](https://github.com/bbatsov/prelude)

3) SBCL is required for [SLIME](http://www.cliki.net/SLIME%20Features) to work properly. On Debian just do `sudo apt-get install sbcl`.

4) Fetch this repo into `~/.emacs.d/personal/`

5) Start EMACS

## Usage

### The easy way

Clone [my bash repo](https://bitbucket.org/louis_kottmann/bash) with yummy aliases and environment variables then use the `em` command.

Please also note that for emails you need to have a `~/.authinfo` file containing gmail access (see [the emacswiki section](http://www.emacswiki.org/emacs/GnusGmail#toc1)).

### The usual way

Run `emacs --daemon` once (may be done at startup like in a systemd service)

Then use `nohup emacsclient -c &` to enter a session.

or, a bit better (but hard to remember):
run `emacsclient -c -n -a "" -F "((fullscreen . maximized))"`
it will start the daemon if necessary and run itself daemonized
