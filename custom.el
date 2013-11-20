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
     ("/home/louis/JS/swag" "swag"))
     (#("/home/baboon/Ruby/afpric-salon2013" 0 34
        (help-echo "Mouse-2 toggles maximizing, mouse-3 displays a popup-menu"))
      "afpric")))
 '(ecb-windows-width 0.15)
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
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mode-line-format
   (quote
    ("%e"
     (:eval
      (let*
          ((active
            (powerline-selected-window-active))
           (mode-line
            (if active
                (quote mode-line)
              (quote mode-line-inactive)))
           (face1
            (if active
                (quote powerline-active1)
              (quote powerline-inactive1)))
           (face2
            (if active
                (quote powerline-active2)
              (quote powerline-inactive2)))
           (separator-left
            (intern
             (format "powerline-%s-%s" powerline-default-separator
                     (car powerline-default-separator-dir))))
           (separator-right
            (intern
             (format "powerline-%s-%s" powerline-default-separator
                     (cdr powerline-default-separator-dir))))
           (lhs
            (list
             (powerline-raw "%*" nil
                            (quote l))
             (powerline-buffer-size nil
                                    (quote l))
             (powerline-raw mode-line-mule-info nil
                            (quote l))
             (powerline-buffer-id nil
                                  (quote l))
             (when
                 (and
                  (boundp
                   (quote which-func-mode))
                  which-func-mode)
               (powerline-raw which-func-format nil
                              (quote l)))
             (powerline-raw " ")
             (funcall separator-left mode-line face1)
             (when
                 (boundp
                  (quote erc-modified-channels-object))
               (powerline-raw erc-modified-channels-object face1
                              (quote l)))
             (powerline-major-mode face1
                                   (quote l))
             (powerline-process face1)
             (powerline-narrow face1
                               (quote l))
             (powerline-raw " " face1)
             (funcall separator-left face1 face2)
             (powerline-vc face2
                           (quote r))))
           (rhs
            (list
             (powerline-raw global-mode-string face2
                            (quote r))
             (funcall separator-right face2 face1)
             (powerline-raw "%4l" face1
                            (quote l))
             (powerline-raw ":" face1
                            (quote l))
             (powerline-raw "%3c" face1
                            (quote r))
             (funcall separator-right face1 mode-line)
             (powerline-raw " ")
             (powerline-raw "%6p" nil
                            (quote r))
             (powerline-hud face2 face1))))
        (concat
         (powerline-render lhs)
         (powerline-fill face2
                         (powerline-width rhs))
         (powerline-render rhs)))))))
 '(nxml-child-indent 4)
 '(powerline-default-separator (quote slant))
 '(send-mail-function nil)
 '(smtpmail-auth-credentials
   (quote
    (("smtp.gmail.com" 587 "louis.kottmann@gmail.com" nil))))
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-starttls-credentials (quote (("smtp.gmail.com" 587 nil nil))))
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
 '(default ((t (:background "nil"))))
 '(powerline-active1 ((t (:inherit mode-line :background "#CB8E06" :foreground "#4F4F4F"))))
 '(powerline-active2 ((t (:inherit mode-line :background "#268BD2" :foreground "#FDF6E3"))))
 '(tabbar-button ((t (:background "#5A768B" :foreground "#FDF6E3"))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#EEE8D5" :foreground "#EEE8D5" :height 0.8))))
 '(tabbar-selected ((t (:background "#5A768B" :foreground "#FDF6E3" :box (:line-width -1 :style pressed-button)))))
 '(tabbar-separator ((t (:inherit tabbar-default :height 0.1))))
 '(tabbar-unselected ((t (:background "#FDF6E3" :foreground "#5A768B" :box (:line-width -1 :style released-button))))))
