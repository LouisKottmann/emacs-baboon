;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Color theme
(load-theme 'solarized-light t)

;; Powerline support
(powerline-default-theme)

;; Disable scrollbars
(scroll-bar-mode -1)

;; ECB
;;(require 'ecb)
;;(ecb-activate)

;; Auto complete
(add-to-list 'load-path "~/.emacs.d/personal/auto-complete")
; Load the default configuration
(require 'auto-complete-config)
; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/.emacs.d/personal/auto-complete/dict")
;; Performance workarounds
(ac-flyspell-workaround)
(ac-linum-workaround)
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 1 characters of a word
(setq ac-auto-start 1)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; ERC
(require 'erc)
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)
(setq erc-autojoin-channels-alist '(("freenode.net" "#emacs")))
(erc :server "irc.freenode.net" :port "6667" :nick "baboon")
;;(setq erc-autojoin-channels-alist '(("wyplay.net" "#webapp" "#g7")))
;;(erc :server "irc.wyplay.net" :port "6667" :nick "louis")
;;(setq erc-autojoin-channels-alist '(("irc-ssl.sackheads.org" "McCoy")))
;;(erc :server "irc-ssl.sackheads.org" :port "6669" :nick "fatalbaboon")

;; HAML
(require 'haml-mode)

;; Soft wrap (words are not split at the end of a line)
(global-visual-line-mode 1)
(global-rainbow-delimiters-mode 1)
