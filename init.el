;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Color theme
(load-theme 'solarized-dark t)

;; Powerline support
(add-to-list 'load-path "~/.emacs.d/personal/powerline/")
(require 'powerline)
(powerline-default-theme)

;; Disable scrollbars
(scroll-bar-mode -1)

;; ECB
(ecb-activate)

;; Auto complete
(add-to-list 'load-path "~/.emacs.d/personal/auto-complete")
; Load the default configuration
(require 'auto-complete-config)
; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/.emacs.d/personal/auto-complete/dict")
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 1 characters of a word
(setq ac-auto-start 1)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;; HAML
(require 'haml-mode)
