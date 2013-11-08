;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Color theme
(load-theme 'solarized-light t)

;; Powerline support
(powerline-default-theme)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Auto complete
(add-to-list 'load-path "~/.emacs.d/personal/auto-complete")
(require 'auto-complete-config)
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
(setq erc-autojoin-channels-alist '((".*freenode.net"
                                     "#emacs")
                                    (".*wyplay.net"
                                     "#g7" "#webapp")
                                    (".*sackheads.org"
                                     "#mccoy")))
(erc :server "irc.freenode.net" :port "6667" :nick "baboon")
;; (erc-ssl :server "irc.wyplay.net" :port "6667" :nick "louis")

;; HAML
(require 'haml-mode)

;; Soft wrap (words are not split at the end of a line)
(global-visual-line-mode 1)

;; Colors parenthesis pairs
(global-rainbow-delimiters-mode 1)

;; Twittering-mode
;; The library autoloads on (twit) but that's counter-intuitive
(autoload 'twittering-mode "twittering-mode")
(setq twittering-username "louiskottmann")
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)

;; TODO: ecb-show/hide-windows keymaps
;; TODO: tabbar grouping
;; TODO: customize powerline
;;       -> hide minor modes in powerline
