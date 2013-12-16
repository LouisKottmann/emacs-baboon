;;; init.el Emacs configuration file for Louis "Baboon" Kottmann
(require 'cl)

;; Install missing packages
(require 'package)
(defvar baboon-packages
  '(w3 solarized-theme haml-mode
       ecb tabbar powerline auto-complete
       twittering-mode emms hackernews
       rinari markdown-mode web-mode)
  "Packages any decent baboon would use.")

(package-initialize)
(message "%s" "The jungle god is looking for newer packages..")
(package-refresh-contents)
(message "%s" " done.")
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 baboon-packages)

;; Color theme
(load-theme 'solarized-light t)

;; Powerline support
;;;###autoload
(defun powerline-baboon-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-raw mode-line-mule-info nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
;;                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(powerline-baboon-theme)

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
                                    ))
;; (erc :server "irc.freenode.net" :port "6667" :nick "baboon")

;; HAML
(require 'haml-mode)

;; Soft wrap (words are not split at the end of a line)
(global-visual-line-mode 1)

;; Colors parenthesis pairs
(global-rainbow-delimiters-mode 1)

;; Twittering-mode
;; The library autoloads on (twit) but that's counter-intuitive
(autoload 'twittering-mode "twittering-mode"
  "loads twittering-mode, just like twit"
  t nil)
(setq twittering-username "louiskottmann")
(setq twittering-use-master-password t)
(setq twittering-icon-mode t)

;; Web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-engines-alist
      '(("javascript" . "\\.ejs\\'")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Emacs Multimedia System
(require 'emms-setup)
(emms-devel)
(emms-default-players)
(setq emms-player-mpg321-parameters '("-o" "alsa"))
(setq emms-source-file-default-directory "/media/trinasse/partage/Musique/")
(add-hook 'emms-player-started-hook 'emms-show)

(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl") ;; Replace "sbcl" with the path to your implementation

;; TODO: tabbar grouping
;; TODO: interactive function to align the space on the right after align-regexp

;; Custom ELISP
;; should assign this to an interactive command
(defun align-regexp-lefty(beg end align-on)
  "Same as align-regexp except the spaces are on the left."
  (interactive "*r \nMAlign on: ")
  (align-regexp beg end (concat "\\(\\s-*\\)" align-on))
  (save-restriction
    (narrow-to-region beg end)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp
              (concat "^\\(\\ *\\)\\([^[:space:]]*\\)\\ \\(\\ *\\)"
                      align-on
                      "\\ *\\(.+\\)$")
              nil t)
        (replace-match (concat "\\1\\3\\2"
                               align-on
                               " \\4")
                       nil nil)))))

;; Keybindings
(global-set-key (kbd "s-<right>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<left>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)


;;;init.el ends here
