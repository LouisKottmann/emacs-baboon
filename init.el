;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Install missing packages
(require 'package)
(defvar baboon-packages
  '(w3 solarized-theme haml-mode
       ecb tabbar powerline auto-complete
       twittering-mode emms hackernews
       rinari markdown-mode ;; web-mode
       smartscan vline google-translate)
  "Packages any decent baboon would use.")

(condition-case nil
    (progn
      (package-initialize)
      (message "%s" "The jungle god is looking for newer packages..")
      (package-refresh-contents)
      (message "%s" " done.")
      (mapc
       (lambda (package)
         (or (package-installed-p package)
             (package-install package)))
       baboon-packages))
  (error
   (message "%s" "God failed.. do you have internet access?")))

(require 'google-translate)

;; Color theme
(load-theme 'solarized-light t)

;; French keyboard support (^ etc)
(load-library "iso-transl")

;; <- Powerline support
(defvar baboon-mode-line-buffer-count nil)
(make-variable-buffer-local 'baboon-mode-line-buffer-count)

;;;###autoload
(defun powerline-baboon-theme ()
  "Setup the default mode-line."
  (interactive)
  (setq-default
   mode-line-format
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
                        (when (and (boundp 'which-func-mode) which-func-mode)
                          (powerline-raw which-func-format nil 'l))
                        (powerline-raw " ")
                        (funcall separator-left mode-line face1)
                        (when (boundp 'erc-modified-channels-object)
                          (powerline-raw erc-modified-channels-object face1 'l))
                        (powerline-major-mode face1 'l)
                        (powerline-process face1)
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
                        (powerline-raw baboon-mode-line-buffer-count nil 'r)
                        (powerline-raw " "))))
        (concat (powerline-render lhs)
                (powerline-fill face2 (powerline-width rhs))
                (powerline-render rhs)))))))

(defun baboon-mode-line-count-lines ()
  (setq baboon-mode-line-buffer-count
        (if line-number-mode
            (int-to-string (+ 1 (count-lines (point-min) (point-max))))
          "?")))

(add-hook 'find-file-hook 'baboon-mode-line-count-lines)
(add-hook 'after-save-hook 'baboon-mode-line-count-lines)
(add-hook 'after-revert-hook 'baboon-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'baboon-mode-line-count-lines)

(powerline-baboon-theme)
;; <- Powerline support

;; Disable scrollbars
(scroll-bar-mode -1)

;; Hide cursor when typing
(setq make-pointer-invisible t)

;; Allow to search for next (M-n)/previous (M-p) occurence of word at point
(global-smartscan-mode 1) ;; (M-') replaces occurences of word at point

;; Auto complete
(require 'auto-complete-config)
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
(setq twittering-use-icon-storage t)

;; Html-mode
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . html-mode))

;; Web-mode
;; (require 'web-mode)
;; (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (setq web-mode-engines-alist
;;       '(("javascript" . "\\.ejs\\'")))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Tern - JS jumps & completions - http://ternjs.net/doc/manual.html#emacs
(add-to-list 'load-path "/home/louis/GitRepos/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; Emacs Multimedia System
(require 'emms-setup)
(emms-devel)
(emms-default-players)
(setq emms-player-mpg321-parameters '("-o" "alsa"))
(setq emms-source-file-default-directory "/media/trinasse/partage/Musique/")
(add-hook 'emms-player-started-hook 'emms-show)

;; SLIME
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
(require 'slime)

;; Magit
;; Fix diff colors
(eval-after-load "magit"
  '(dolist (additional-options '("-c" "color.diff=false"))
    (add-to-list 'magit-git-standard-options additional-options t)))

;; Custom ELISP

(defun align-regexp-lefty(beg end align-on)
  "Aligns hashes-like structures around their key-value separator.
\(same as align-regexp except the spaces are on the left\)"
  (interactive "*r \nMAlign on: ")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (align-regexp (point-min) (point-max) (concat "\\(\\s-*\\)" align-on))
      (goto-char (point-min))
      (while (search-forward-regexp
              (concat "^\\(\\ *\\)\\([^[:space:]]*\\)\\ \\(\\ *\\)"
                      align-on
                      "\\ *\\(.+\\)$")
              nil t)
        (replace-match (concat "\\1\\3\\2" align-on " \\4")
                       nil nil)))))

(defun baboon-rm-multi-whitespace(beg end)
  "Does blablablah magic -> ?? -> profit!"
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (search-forward-regexp " \\{2,\\}" nil t)
        (replace-match " " nil nil))
      (goto-char (point-min))
      (indent-region beg end))))

;; -> baboon-dedicated-window-modeline
(defvar baboon-dedicated-window-mode-line-string "⚓"
  "A string added to the modeline when the window is dedicated")

(defun baboon-remove-dedicated-window-mode-line-string ()
  (setq mode-line-format
        (remove baboon-dedicated-window-mode-line-string mode-line-format)))

(defun baboon-add-dedicated-window-mode-line-string ()
  (setq mode-line-format
        (append `(,baboon-dedicated-window-mode-line-string) mode-line-format)))

(defadvice set-window-dedicated-p (after baboon-dedicated-window-mode-line activate)
  "Adds `baboon-dedicated-window-mode-line-string`
to the mode-line of windows that are dedicated"
  (baboon-remove-dedicated-window-mode-line-string)
  (when (window-dedicated-p window)
        (baboon-add-dedicated-window-mode-line-string)))

(defun baboon-dedicate-window ()
  "(toggler) Make the currently selected window irreplacable"
  (interactive)
  (set-window-dedicated-p
   (selected-window)
   (not
    (window-dedicated-p (selected-window)))))
;; <- baboon-dedicated-window-modeline

;; Prelude remapping
(add-hook 'prelude-mode-hook
          (lambda ()
            (define-key prelude-mode-map (kbd "M-o") 'other-window)))

;; Baboon Keybindings
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)
(global-set-key (kbd "C-c . l a") 'ecb-activate)
(global-set-key (kbd "C-x M-o") 'other-frame)
(global-set-key (kbd "C-c C-q") 'slime-close-all-parens-in-sexp)

;;;init.el ends here
