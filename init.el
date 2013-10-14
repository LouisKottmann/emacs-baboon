;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Color theme
(load-theme 'solarized-dark t)

;; ERC
(require 'erc)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs"")
        ("oftc.net" "#bitlbee")))
(erc :server "irc.freenode.net" :port "6667" :nick "baboon")
(erc :server "irc-ssl.sackheads.org" :port "6669" :nick "fatalbaboon")
;;(setq erc-autojoin-channels-alist '(("irc-ssl.sackheads.org" "#McCoy")))
(erc :server "irc.wyplay.net" :port "6667" :nick "louis")
;;(setq erc-autojoin-channels-alist '(("irc.wyplay.net" "#webapp" "#g7")))
;;(setq erc-kill-buffer-on-part t)
;;(setq erc-kill-queries-on-quit t)
;;(setq erc-kill-server-buffer-on-quit t)

;; Slime support
;; (add-to-list 'load-path "~/.emacs.d/personal/slime/")
;; (require 'slime)
;; (add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))
;; Optionally, specify the lisp program you are using. Default is "lisp"
;;(setq inferior-lisp-program "yourlisp")

;; Powerline support
(add-to-list 'load-path "~/.emacs.d/personal/powerline/")
(require 'powerline)
(powerline-default-theme)

;; Disable scrollbars
(scroll-bar-mode -1)

;; ECB
(ecb-activate)
(ecb-toggle-layout)
(ecb-toggle-layout)


;; Auto complete
(add-to-list 'load-path "~/.emacs.d/personal/auto-complete")
; Load the default configuration
(require 'auto-complete-config)
; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/.emacs.d/personal/auto-complete/dict")
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 1)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/personal/dict")
;; (require 'auto-complete-config)
;; (ac-config-default)
