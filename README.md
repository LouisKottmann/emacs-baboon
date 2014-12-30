# Louis "Baboon" Kottmann's EMACS configuration

Slowly modified prelude by a monkey.

## Installation

See this script for relevant 
[EMACS](https://github.com/LouisKottmann/baboon-bash/blob/master/Scripts/baboon-install-linux.sh#L106) 
parts

## Usage

### The easy way

Clone [my bash repo](https://github.com/LouisKottmann/baboon-bash) with yummy aliases and environment variables then use the `emc` command (`em` to load a file into an existing instance).

Please also note that for emails you need to have a `~/.authinfo` file containing gmail access (see [the emacswiki section](http://www.emacswiki.org/emacs/GnusGmail#toc1)).

### The usual way

Run `emacs --daemon` once (may be done at startup like in a systemd service)

Then use `nohup emacsclient -c &` to enter a session.

or, a bit better (but hard to remember):
run `emacsclient -c -n -a "" -F "((fullscreen . maximized))"`
it will start the daemon if necessary and run itself daemonized
