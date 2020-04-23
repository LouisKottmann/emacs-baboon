;;; init-20-interface.el --- Interface customizations

;; don't use tabs to indent
(setq-default indent-tabs-mode nil)
;; but maintain correct appearance
(setq-default tab-width 8)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(use-package super-save
  :demand t
  :config
  (super-save-mode +1))

;; savehist keeps track of some history
(use-package savehist
  :demand t
  :init
  (gsetq savehist-additional-variables '(search ring regexp-search-ring) ;; search entries
        savehist-autosave-interval 60                                    ;; save every minute
        savehist-file (expand-file-name "savehist" baboon-savefile-dir)) ;; keep the home clean
  :config
  (savehist-mode +1))

;; flyspell-mode does spell-checking on the fly as you type
(use-package flyspell
  :demand t
  :init
  (gsetq ispell-program-name "aspell" ; use aspell instead of ispell
         ispell-extra-args '("--sug-mode=ultra"))
  (flyspell-mode +1))

;;
(use-package ido
  :demand t
  :init
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10
        ido-save-directory-list-file (expand-file-name "ido.hist" baboon-savefile-dir)
        ido-default-file-method 'selected-window
        ido-auto-merge-work-directories-length -1)
  (ido-mode +1))

(use-package ido-completing-read+
  :demand t
  :init
  (ido-ubiquitous-mode +1))

(use-package flx-ido
  :demand t
  :config
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil))

(use-package ido-grid-mode
  :demand t
  :init
  (ido-grid-mode 1))


;; C-h f, while Smex is active, runs describe-function on the currently selected command.
;; M-. jumps to the definition of the selected command.
;; C-h w shows the key bindings for the selected command.
;; Show command that desserve a binding: `smex-show-unbound-commands'
(use-package smex
  :demand t
  :custom
  (smex-history-length 35)
  :init
  (setq smex-save-file (expand-file-name ".smex-items" baboon-savefile-dir))
  (smex-initialize)
  :bind (("M-X" . smex-major-mode-commands)
         ("M-x" . smex)))

(use-package all-the-icons
  :init (gsetq inhibit-compacting-font-caches t)
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf" all-the-icons-octicon          "gear"     :face all-the-icons-dyellow :v-adjust 0.0))
  (add-to-list 'all-the-icons-icon-alist
               '("ssh_config" all-the-icons-alltheicon "terminal" :face all-the-icons-lsilver))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.ru$" all-the-icons-alltheicon        "ruby-alt" :face all-the-icons-red)))

(use-package neotree
  :after all-the-icons
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :chords ("FF" . neotree-find)
  :custom
  (neo-auto-indent-point t)
  (neo-banner-message "")
  (neo-force-change-root t)
  (neo-mode-line-type 'none)
  (neo-persist-show nil)
  (neo-show-header nil)
  (neo-theme (if (display-graphic-p) 'icons 'nerd))
  (neo-vc-integration '(face))
  (neo-window-width 30)
  :custom-face
  (neo-dir-link-face ((t (:inherit font-lock-function-name-face))))
  (neo-file-link-face ((t (:inherit font-lock-reference-face))))
  (neo-header-face ((t (:inherit font-lock-type-face :weight bold))))
  (neo-vc-up-to-date-face ((t (:foreground "snow4")))))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(when (fboundp 'fringe-mode)
  (fringe-mode '(8 . 0)))

(setq-default indicate-buffer-boundaries 'left)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " Baboon - " (:eval (if (buffer-file-name)
                                                   (abbreviate-file-name (buffer-file-name))
                                                 "%b"))))

;; highlight the current line
(global-hl-line-mode +1)

(use-package undo-tree)

(use-package volatile-highlights
  :config
  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree))

(use-package anzu
  :config (global-anzu-mode))

(use-package ag
  :custom
  (ag-arguments
   (quote
    ("--smart-case" "--nogroup" "--column" "--ignore=TAGS" "--ignore=*.min.js" "--ignore=*.instrumented.js" "--")))
  (ag-highlight-search t t)
  (ag-reuse-window t))

(use-package helm
  :demand t
  :init
  ;; Changes the helm prefix key
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  ;;
  (gsetq helm-split-window-in-side-p           t
         helm-move-to-line-cycle-in-source     t
         helm-ff-search-library-in-sexp        t
         helm-scroll-amount                    8
         helm-M-x-fuzzy-match                  t
         helm-ff-file-name-history-use-recentf t
         helm-buffers-fuzzy-matching           t
         helm-recentf-fuzzy-match              t
         helm-imenu-fuzzy-match                t
         helm-locate-fuzzy-match               t
         helm-apropos-fuzzy-match              t
         helm-lisp-fuzzy-completion            t
         helm-allow-mouse                      t
         helm-follow-mode-persistent           t
         helm-ff-lynx-style-map                t
         helm-bookmark-show-location           t)
  :bind (("s-b" . helm-mini)
         ("s-r" . helm-recentf))
  :config
  (require 'helm-files)
  (require 'helm-config)) ; Necessary for helm-mode

(use-package helm-ag
  :bind ("s-g" . helm-do-ag-project-root)
  :custom
  (helm-ag-fuzzy-match t)
  (helm-ag-insert-at-point 'symbol t)
  (helm-ag-use-agignore t)
  (helm-ag-use-temp-buffer t))

;; (use-package helm-swoop)

(use-package projectile
  :after helm
  :config
  (setq projectile-cache-file (expand-file-name  "projectile.cache" baboon-savefile-dir))
  (projectile-mode t)
  :init
  (gsetq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" baboon-savefile-dir)))

(use-package helm-projectile
  :after helm
  :commands (helm-projectile-find-file
             helm-projectile-recentf
             helm-projectile-switch-project
             helm-projectile-switch-to-buffer)
  :init
  (helm-projectile-on)
  (gsetq projectile-completion-system 'helm)
  :bind ("s-f" . helm-projectile-find-file))

(use-package move-text
  :bind (("M-S-<up>" . move-text-up)
         ("M-S-<down>" . move-text-down)))

;; Sometimes you end up with cursors outside of your view.
;; You can scroll the screen to center on each cursor with C-v and M-v
;; or you can press C-' to hide all lines without a cursor, press C-' again to unhide.
(use-package multiple-cursors
  :init
  (setq mc/list-file (expand-file-name "mc-lists.el" baboon-savefile-dir))
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-c C-<" . mc/mark-all-in-region)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  ;;
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  :custom
  (sp-autodelete-closing-pair nil)
  (sp-autodelete-opening-pair nil)
  (sp-autodelete-pair nil)
  (sp-autodelete-wrap nil)
  (sp-autoescape-string-quote nil)
  (sp-autoinsert-pair nil)
  (sp-autoskip-closing-pair nil)
  (sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:background "#268BD2" :foreground "white")))))

(use-package rainbow-mode
  :hook (((after-init
           text-mode
           org-mode
           css-mode
           html-mode
           prog-mode) . rainbow-mode)))

(use-package rainbow-delimiters
  :after smartparens
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-blocks)

;; whitespace-mode
(use-package whitespace
  :init
  (setq whitespace-action '(auto-cleanup)
        whitespace-line-column 120 ;; make characters after column 100 purple
        whitespace-style '(face tabs empty trailing lines-tail))
  :config
  (global-whitespace-mode +1)
  :hook
  ;; makefile requires tab to work
  (makefile-mode . (lambda () (whitespace-mode 0))))

(use-package visual-regexp
  :config
  (defalias 'replace-regexp 'vr/replace)
  :bind
   ;; if you use multiple-cursors, this is for you:
   ("C-c m" . vr/mc-mark))

(use-package auto-highlight-symbol
  :custom
  (global-auto-highlight-symbol-mode t))

(use-package diff-hl
  :demand t
  :config
  (global-diff-hl-mode)
  :hook
  (dired-mode . diff-hl-dired-mode)
  :custom-face
  (diff-hl-change ((t (:background "deep sky blue" :foreground "deep sky blue"))))
  (diff-hl-insert ((t (:background "lime green" :foreground "lime green"))))
  (diff-hl-delete ((t (:background "#a33c35" :foreground "#a33c35")))))

(use-package easy-kill
  :bind
  (([remap kill-ring-save] . easy-kill)
   ([remap mark-sexp]      . easy-mark)))

(use-package ansi-color
  :defer t
  :preface
  (defun colorise-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook ((compilation-filter . colorise-compilation-buffer)))

(use-package helpful
  :bind (("C-c C-d" . helpful-at-point))
  :init (general-define-key
         :prefix "C-h"
         "f" 'helpful-callable
         "v" 'helpful-variable
         "k" 'helpful-key
         "F" 'helpful-function
         "C" 'helpful-command))

(use-package ace-window
  :bind
  (("s-:" . ace-window)
   ("s-/" . ace-swap-window)))

(use-package flycheck
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook (after-init . global-flycheck-mode))

(use-package vlf
  :config
  (require 'vlf-setup))

(use-package link-hint
  :config
  (defalias 'browse-url-hint 'link-hint-open-link)
  (defalias 'browse-u-hint 'link-hint-open-link))

(use-package magit
  :bind ("s-m" . magit-status)
  :config (gsetq magit-diff-refine-hunk 'all))

(use-package git-timemachine)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-center-content t
        dashboard-show-shortcuts t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-set-navigator t
        dashboard-startup-banner 'logo
        dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          ;; (agenda . 5)
                          ;; (registers . 5)
                          (projects . 5))))

(use-package iedit)

(use-package undo-tree
  :init
  (gsetq undo-tree-visualizer-timestamps t
         undo-tree-visualizer-diff t)
  :chords ("UU" . undo-tree-visualize)
  :config
  (global-undo-tree-mode 1))

(use-package browse-kill-ring
  :chords ("YY" . browse-kill-ring))

(use-package which-key
  :config
  (which-key-mode))

(defun baboon-random-buffer ()
  (interactive)
  (switch-to-buffer
   (eval-expression
    (quote
     (concat "sdf "
             (format "%s" (random 5000))))
    nil)))
