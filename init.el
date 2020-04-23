;;; init.el --- Entrypoint of Emacs config

;; (setq user-emacs-directory "~/.emacs.d/")
;; better way:
(let (file-name-handler-alist)
  ;; Ensure emacs is running out of this file's directory
  (setq user-emacs-directory (file-name-directory load-file-name)))

;; Always load newest byte code
(setq load-prefer-newer t)

;; User Info
(setq user-full-name "Louis Kottmann")
(setq user-mail-address "louis.kottmann@baboon.io")

;; Increase GC threshold to 50MB
(setq gc-cons-threshold 50000000)

;; Amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Install use-package if necessary
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives (append package-archives
                               '(("melpa" . "https://melpa.org/packages/")
                                 ("gnu" . "https://elpa.gnu.org/packages/"))))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))
(require 'bind-key)                ;; if you use any :bind variant

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package use-package-chords
  :config (key-chord-mode 1))

;; Set the path variable
(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

;;
(defvar baboon-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Baboon config.")
(defvar baboon-savefile-dir (expand-file-name "savefile" baboon-dir)
  "This folder stores all the automatically generated save/history-files.")

(unless (file-exists-p baboon-savefile-dir)
  (make-directory baboon-savefile-dir))

(setq custom-file (expand-file-name "custom.el" baboon-savefile-dir))

(use-package auto-package-update
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-prompt-before-update t
         auto-package-update-interval 180
         auto-package-update-last-update-day-path (expand-file-name "last-package-update-day" baboon-savefile-dir))
   (auto-package-update-maybe))

;; Essential libraries
(use-package dash)
(use-package dash-functional)

(use-package general
  :init
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-local #'general-setq-local)
  (defalias 'gsetq-default #'general-setq-default))

;;
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 150)
(setq recentf-save-file (expand-file-name "recentf" baboon-savefile-dir))
;; disable recentf-cleanup on Emacs start, because it can cause
;; problems with remote files
(setq recentf-auto-cleanup 'never)

(defun baboon-recentf-mode-startup ()
  (message "init recentf")
  (recentf-mode 1)
  (recentf-load-list)
  (add-hook 'find-file-hook 'recentf-save-list))
(add-hook 'emacs-startup-hook 'baboon-recentf-mode-startup)

;; Load baboon's config
(dolist (x (file-expand-wildcards "~/.emacs.d/init-*.el"))
  (load-file x))

;; We don't load custom-file, we just use it to get the syntax and then copy to the correct use-package definition
;; (when (file-exists-p custom-file)
;;   (load custom-file))
