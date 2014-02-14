(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-show-menu t)
 '(ac-auto-start 1)
 '(ac-ignore-case (quote smart))
 '(custom-safe-themes
   (quote
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(ecb-layout-name "left13")
 '(ecb-new-ecb-frame nil)
 '(ecb-options-version "2.40")
 '(ecb-other-window-behavior (quote only-edit))
 '(ecb-source-path
   (quote
    (("/" "/")
     ("/home/louis/.emacs.d/personal" "emacs")
     ("/home/louis/JS/webapp" "webapp")
     ("/home/louis/JS/swag" "swag")
     ("/home/baboon/Ruby/afpric-salon2013" "afpric")
     ("/home/louis/JS/ext_test_app" "external_test_app")
     ("/home/louis/JS/testapp" "testapp"))))
 '(ecb-windows-width 0.15)
 '(emms-mode-line-mode-line-function nil)
 '(emms-stream-repeat-p t)
 '(erc-autojoin-mode t)
 '(erc-hide-list (quote ("JOIN" "PART" "QUIT")))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands notifications readonly ring stamp spelling track truncate)))
 '(erc-port 6667)
 '(erc-reuse-buffers nil)
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
 '(guide-key/guide-key-sequence (quote ("C-c" "C-x" "C-h")))
 '(guide-key/highlight-command-regexp "baboon\\|prelude")
 '(guide-key/popup-window-position (quote bottom))
 '(guide-key/recursive-key-sequence-flag t)
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
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-include-node-externs t)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(nxml-child-indent 4)
 '(powerline-default-separator (quote slant))
 '(send-mail-function nil)
 '(smex-history-length 25)
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
 '(tabbar-use-images nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ag-match-face ((t (:inherit lazy-highlight))))
 '(powerline-active1 ((t (:inherit mode-line :background "#CB8E06" :foreground "#4F4F4F"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#268BD2" :foreground "#FDF6E3"))))
 '(sp-show-pair-match-face ((t (:inherit lazy-highlight))))
 '(tabbar-button ((t (:background "#5A768B" :foreground "#FDF6E3"))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#EEE8D5" :foreground "#EEE8D5" :height 0.8))))
 '(tabbar-selected ((t (:background "#5A768B" :foreground "#FDF6E3" :box (:line-width -1 :style pressed-button)))))
 '(tabbar-separator ((t (:inherit tabbar-default :height 0.1))))
 '(tabbar-unselected ((t (:background "#FDF6E3" :foreground "#5A768B" :box (:line-width -1 :style released-button)))))
 '(vline ((t (:background "#EEE8D5"))) t))
