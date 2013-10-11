;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Color theme
(load-theme 'solarized-light t)

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
