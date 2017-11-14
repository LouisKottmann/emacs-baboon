(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(ac-auto-start 1)
 '(ac-ignore-case (quote smart))
 '(ag-arguments
   (quote
    ("--smart-case" "--nogroup" "--column" "--ignore=TAGS" "--ignore=*.min.js" "--ignore=*.instrumented.js" "--")))
 '(ag-highlight-search t t)
 '(ag-reuse-window t)
 '(ahs-idle-interval 0.5)
 '(ahs-modes
   (quote
    (actionscript-mode apache-mode bat-generic-mode c++-mode c-mode csharp-mode css-mode dos-mode emacs-lisp-mode html-mode ini-generic-mode java-mode javascript-mode js-mode lisp-interaction-mode lua-mode latex-mode makefile-mode makefile-gmake-mode markdown-mode moccur-edit-mode nxml-mode nxhtml-mode outline-mode perl-mode cperl-mode php-mode python-mode rc-generic-mode reg-generic-mode ruby-mode sgml-mode sh-mode squirrel-mode text-mode tcl-mode visual-basic-mode js2-mode web-mode lisp-mode)))
 '(company-idle-delay 0.5)
 '(company-quickhelp-delay 0)
 '(company-quickhelp-mode t)
 '(custom-safe-themes
   (quote
    ("764e3a6472a3a4821d929cdbd786e759fab6ef6c2081884fca45f1e1e3077d1d" "cf205b711e61963020e2d1561e87cdbe7727679b58af25dcabfe5073572b16f0" "8d6fb24169d94df45422617a1dfabf15ca42a97d594d28b3584dc6db711e0e0b" "08efabe5a8f3827508634a3ceed33fa06b9daeef9c70a24218b70494acdf7855" "90edd91338ebfdfcd52ecd4025f1c7f731aced4c9c49ed28cfbebb3a3654840b" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ecb-layout-name "left13")
 '(ecb-new-ecb-frame nil)
 '(ecb-options-version "2.40")
 '(ecb-other-window-behavior (quote all))
 '(ecb-source-path
   (quote
    (("/" "/")
     ("/home/louis/.emacs.d/personal" "emacs")
     ("/home/louis/JS/webapp" "webapp")
     ("/home/louis/JS/swag" "swag")
     ("/home/baboon/Ruby/afpric-salon2013" "afpric")
     ("/home/louis/JS/ext_test_app" "external_test_app")
     ("/home/louis/JS/testapp" "testapp")
     (#("/home/louis/Ruby/hut" 0 20
        (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"))
      "hut"))))
 '(ecb-windows-width 0.15)
 '(emms-mode-line-mode-line-function nil)
 '(emms-stream-repeat-p t)
 '(erc-autojoin-mode t)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp spelling track truncate)))
 '(erc-pcomplete-mode nil)
 '(erc-port 6667)
 '(erc-reuse-buffers nil)
 '(ggtags-global-ignore-case t)
 '(global-auto-highlight-symbol-mode t)
 '(global-mark-ring-max 64)
 '(gnus-select-method
   (quote
    (nnimap "gmail"
            (nnimap-address "imap.gmail.com")
            (nnimap-server-port 993)
            (nnimap-stream ssl))))
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "fr")
 '(google-translate-enable-ido-completion t)
 '(guide-key/guide-key-sequence (quote ("C-c" "C-x" "C-h" "s-m")))
 '(guide-key/highlight-command-regexp "baboon\\|prelude")
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
 '(helm-ag-fuzzy-match t)
 '(helm-ag-insert-at-point (quote \'word))
 '(ibuffer-default-sorting-mode (quote major-mode))
 '(ibuffer-expert t)
 '(ibuffer-formats
   (quote
    ((mark modified read-only " "
           (name 22 22 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename))))
 '(indent-tabs-mode nil)
 '(initial-major-mode (quote ruby-mode))
 '(initial-scratch-message nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(magit-use-overlays t)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(neo-auto-indent-point t)
 '(neo-banner-message "")
 '(neo-force-change-root t)
 '(neo-mode-line-type (quote none))
 '(neo-persist-show nil)
 '(neo-show-header nil)
 '(neo-theme (quote nerd))
 '(neo-vc-integration (quote (face)))
 '(neo-window-width 30)
 '(nginx-indent-level 2)
 '(nxml-child-indent 4)
 '(package-selected-packages
   (quote
    (sql-indent helm-swoop vlf company-ansible ssh-config-mode ansible-doc jinja2-mode yaml-mode scss-mode yari ruby-tools rainbow-delimiters guru-mode key-chord smex ido-ubiquitous flx-ido zygospore web-mode w3 volatile-highlights vline visual-regexp undo-tree twittering-mode tabbar systemd soundcloud solarized-theme smooth-scrolling smartscan smartrep smartparens slime-company robe rbenv rainbow-mode projectile-rails powerline operate-on-number neotree multiple-cursors moz move-text markdown-mode magit json-mode js2-mode ido-grid-mode highlight-numbers hideshowvis helm-ag haml-mode hackernews guide-key grizzl google-translate gitignore-mode gitconfig-mode git-timemachine gist ggtags flycheck fancy-narrow ewmctrl erc-image elisp-slime-nav easy-kill dockerfile-mode discover-my-major diff-hl company-shell company-quickhelp company-inf-ruby company-anaconda coffee-mode change-inner calfw browse-kill-ring auto-highlight-symbol anzu ag ace-window ace-jump-mode ace-jump-buffer)))
 '(paradox-date-format "%d-%m-%Y")
 '(paradox-display-download-count t)
 '(paradox-github-token t)
 '(paradox-lines-per-entry 2)
 '(pe/omit-regex "^\\.\\|^#\\|~$\\|\\.pyc\\|\\.pyo")
 '(powerline-default-separator (quote arrow))
 '(prelude-guru nil)
 '(recentf-max-saved-items 250)
 '(send-mail-function nil)
 '(sh-indentation 2)
 '(smex-history-length 25)
 '(smooth-scroll/vscroll-step-size 3)
 '(smtpmail-auth-credentials
   (quote
    (("smtp.gmail.com" 587 "louis.kottmann@gmail.com" nil))))
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-starttls-credentials (quote (("smtp.gmail.com" 587 nil nil))))
 '(sp-autodelete-closing-pair nil)
 '(sp-autodelete-opening-pair nil)
 '(sp-autodelete-pair nil)
 '(sp-autodelete-wrap nil)
 '(sp-autoescape-string-quote nil)
 '(sp-autoinsert-pair nil)
 '(sp-autoskip-closing-pair nil)
 '(sp-show-pair-from-inside t)
 '(tabbar-background-color "#EEE8D5")
 '(tabbar-buffer-home-button (quote (("") "")))
 '(tabbar-cycle-scope (quote tabs))
 '(tabbar-mode t nil (tabbar))
 '(tabbar-scroll-left-button (quote (("") "")))
 '(tabbar-scroll-right-button (quote (("") "")))
 '(tabbar-separator (quote (0.5)))
 '(tabbar-use-images nil)
 '(user-mail-address "louis.kottmann@gmail.com")
 '(whitespace-line-column 90))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-foreground ((t (:inherit font-lock-constant-face :background "#fdf6e3" :foreground "#268BD2" :inverse-video nil :weight bold))))
 '(ag-match-face ((t (:inherit baboon-main-color))))
 '(fringe ((t (:background "#fdf6e3"))))
 '(git-timemachine-minibuffer-detail-face ((t (:foreground "firebrick"))))
 '(hackernews-link-face ((t (:inherit font-lock-function-name-face))))
 '(highlight-numbers-number ((t (:inherit baboon-main-color))))
 '(neo-dir-link-face ((t (:inherit font-lock-function-name-face))))
 '(neo-file-link-face ((t (:inherit font-lock-reference-face))))
 '(neo-header-face ((t (:inherit font-lock-type-face :weight bold))))
 '(neo-vc-up-to-date-face ((t (:foreground "snow4"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#CB8E06" :foreground "#4F4F4F"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#268BD2" :foreground "#FDF6E3"))))
 '(sp-show-pair-match-face ((t (:background "#268BD2" :foreground "white"))))
 '(tabbar-button ((t (:background "#5A768B" :foreground "#FDF6E3"))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#EEE8D5" :foreground "#EEE8D5" :height 0.8))))
 '(tabbar-selected ((t (:background "#5A768B" :foreground "#FDF6E3" :box (:line-width -1 :style pressed-button)))))
 '(tabbar-separator ((t (:inherit tabbar-default :height 0.1))))
 '(tabbar-unselected ((t (:background "#FDF6E3" :foreground "#5A768B" :box (:line-width -1 :style released-button)))))
 '(vline ((t (:background "#EEE8D5")))))
