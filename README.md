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

5) Get the git submodules

    git submodule init
    git submodule update

6) Start EMACS and get the following EMACS packages (M-x package-install):

* solarized
* slime
* ecb
* w3
* haml
* tabbar
* auto-complete
* powerline

7) Restart EMACS

8) You're set

## Usage

### The easy way

Clone [my bash repo](https://bitbucket.org/louis_kottmann/bash) with yummy aliases and environment variables then use the `em` command

### The usual way

Run `emacs --daemon` once (may be done at startup like in a systemd service)

Then use `nohup emacsclient -c &` to enter a session.

or, a bit better (but hard to remember):
run `emacsclient -c -n -a "" -F "((fullscreen . maximized))"`
it will start the daemon if necessary and run itself daemonized
