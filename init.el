;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Color theme
(load-theme 'solarized-dark t)

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
