;;; init.el Emacs configuration file for Louis "Baboon" Kottmann

;; Install missing packages
(require 'google-translate)

;; Color theme
;;; Check 'seti theme
(load-theme 'solarized-light t)

;; custom faces
(defface baboon-main-color '((t (:foreground  "#182F31"))) "Background color of my logo")

;; French keyboard support (^ etc)
(load-library "iso-transl")

;; Fonts
(set-face-attribute 'default nil :height 110)

;; <- Powerline support
(defvar baboon-mode-line-buffer-count nil)
(make-variable-buffer-local 'baboon-mode-line-buffer-count)

(defun baboon-mode-line-count-lines ()
  (setq baboon-mode-line-buffer-count
        (if line-number-mode
            (int-to-string (count-lines (point-min) (point-max)))
          "?")))

(add-hook 'find-file-hook 'baboon-mode-line-count-lines)
(add-hook 'after-save-hook 'baboon-mode-line-count-lines)
(add-hook 'after-revert-hook 'baboon-mode-line-count-lines)
(add-hook 'dired-after-readin-hook 'baboon-mode-line-count-lines)

(defun baboon-current-buffer-changed ()
  (baboon-mode-line-count-lines))

(add-hook 'window-configuration-change-hook 'baboon-current-buffer-changed)
(add-hook 'focus-in-hook 'baboon-current-buffer-changed)
(add-hook 'focus-out-hook 'baboon-current-buffer-changed)

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

(powerline-baboon-theme)
;; <- Powerline support

(require 'smooth-scrolling)

;; Disable scrollbars
(scroll-bar-mode -1)

;; Hide cursor when typing
(setq make-pointer-invisible t)

;; Allow to search for next (M-n)/previous (M-p) occurence of word at point
(global-smartscan-mode 1) ;; (M-') replaces occurences of word at point

;; ERC
(require 'erc)
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)
(setq erc-nick "baboon")
(setq erc-autojoin-channels-alist
      '(
        (".*freenode.net" "#emacs")
        ))
(add-hook
 'after-init-hook
 (lambda ()
   (setq erc-fill-column
         (max 90 (- (/ (frame-width) 2) 10)))))
(add-hook 'erc-mode-hook 'linum-mode)
(add-hook 'erc-mode-hook 'visual-line-mode)
;; (erc :server "irc.freenode.net" :port "6667" :nick "baboon")

(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

;; HAML
(require 'haml-mode)

;; Colors parenthesis pairs
(rainbow-delimiters-mode 1)

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

;; js2-mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; json-mode
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; shell-script-mode
(add-to-list 'auto-mode-alist '("\\.bash_aliases\\'" . shell-script-mode))

;; nginx-mode
(add-to-list 'auto-mode-alist '("\\.com\\'" . nginx-mode))

;; Emacs Multimedia System
(require 'emms-setup)
(emms-devel)
(setq emms-source-file-default-directory "/media/trinasse/partage/Musique/")
(add-hook 'emms-player-started-hook 'emms-show)
(emms-mode-line-disable)
;; MPD config
(require 'emms-player-mpd)
(setq emms-player-mpd-server-name "localhost")
(setq emms-player-mpd-server-port "6600")
(add-to-list 'emms-info-functions 'emms-info-mpd)
(add-to-list 'emms-player-list 'emms-player-mpd) ;; Top of players list
(condition-case nil
    (emms-player-mpd-connect)
  (error "Failed to connect to MPD, moving on.."))

;; SLIME
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
(require 'slime)

;; Webjump
(eval-after-load "webjump"
  '(setq
    webjump-sites
    '(("DuckDuckGo" .
       [simple-query "duckduckgo.com" "duckduckgo.com/?q=" ""])
      ("Google" .
       [simple-query "www.google.com" "www.google.com/search?q=" ""])
      ("Wikipedia" .
       [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""])
      ("Github" .
       [simple-query "github.com" "github.com/search?q=" ""])
      ("Youtube" .
       [simple-query "youtube.com" "http://www.youtube.com/results?search_query=" ""])
      ("SoundCloud" .
       [simple-query "soundcloud.com" "https://soundcloud.com/search?q=" "" ])
      ("Emacs Wiki" .
       [simple-query "www.emacswiki.org" "www.emacswiki.org/cgi-bin/wiki/" ""])
      )))

;; guide-key
(guide-key-mode 1)

;; ag (the silver searcher)
(setq ag-highlight-search t)
(setq ag-reuse-buffers 't)

 ;; ace-jump-mode
(setq ace-jump-mode-move-keys
      (nconc (loop for i from ?a to ?z collect i)
             (loop for i from ?A to ?Z collect i)
             (loop for i from ?0 to ?9 collect i)
             '(?, ?? ?\; ?. ?: ?/ ?! ?§)
             '(?& ?é ?\" ?' ?( ?- ?è ?_ ?ç ?à ?) ?=)
             '(?ù ?% ?* ?µ)
             '(?^ ?$ ?£)
             '(?€)))

;; ido
(setq ido-use-faces t)
(require 'ido-vertical-mode)
(ido-vertical-mode 1)

;; fringe
(fringe-mode '(8 . 0))
(setq-default indicate-buffer-boundaries 'left)

;; ibuffer
(require 'ibuffer)
(add-to-list
 'ibuffer-formats
 '(mark modified read-only " "
        (name 28 28 :left :elide)
        " "
        (mode 16 16 :right :elide)
        " "
        (size 7 7 :left :elide)
        " "
        filename-and-process))

;; anaconda
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'eldoc-mode)
(setq company-backends (delete 'company-ropemacs company-backends))
(add-to-list 'company-backends 'company-anaconda)

;; popwin
;; (popwin-mode -1)
;; (setq popwin:popup-window-position 'bottom)
;; (setq popwin:popup-window-width 80)
;; (setq popwin:popup-window-height 20)
;; (push '(direx:direx-mode :position left :width 25 :stick true)
;;       popwin:special-display-config)
;; (push '(help-mode :position right)
;;       popwin:special-display-config)
;; (push '(occur-mode :position bottom :height 18 :stick true)
;;       popwin:special-display-config)

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<down>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-<up>") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-words-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-symbols-like-this)

;; rbenv
(require 'rbenv)
(rbenv-use-global)

;; Robe (use C-c C-l to load a ruby file)
(require 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)
(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rbenv-use-corresponding))
(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

;; Rails stuff
(add-hook 'projectile-mode-hook 'projectile-rails-mode)

;; fancy-narrow (grays out narrowed out instead of hiding it completely - C-x n d/w)
(fancy-narrow-mode 1)

;; Company-mode
(define-key company-active-map (kbd "\C-n") 'company-select-next)
(define-key company-active-map (kbd "\C-p") 'company-select-previous)
(define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
(global-set-key (kbd "<C-tab>") 'company-complete)
(push 'company-robe company-backends)
(push 'company-inf-ruby company-backends)
(slime-setup '(slime-company))

;; change-innner (kills by default, C-u to copy instead)
(require 'change-inner)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-S-i") 'change-outer)

;; discover-my-major
(global-set-key (kbd "C-h C-m") 'discover-my-major)

;; WDired (writable dired, C-x C-q in a dired buffer to activate)
(setq wdired-allow-to-change-permissions t)
(setq wdired-confirm-overwrite t)

;; highlight-numbers-mode
(require 'highlight-numbers)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; helm-ag
(defun projectile-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

;; mozrepl goodies
(defun baboon-refresh-firefox ()
  (interactive)
  (comint-send-string (inferior-moz-process)
                      "setTimeout(BrowserReload(), \"1000\");"))

(defun baboon-start-auto-refresh-firefox ()
  (interactive)
  (add-hook 'after-save-hook
            'baboon-refresh-firefox
            'append 'local)) ; buffer-local
(defun baboon-stop-auto-refresh-firefox ()
  (interactive)
  (remove-hook 'after-save-hook
               'baboon-refresh-firefox
               'local))
;; too many refreshes with these:
;; (add-hook 'html-mode-hook 'auto-reload-firefox-on-after-save-hook)
;; (add-hook 'css-mode-hook 'auto-reload-firefox-on-after-save-hook)
;; (add-hook 'haml-mode-hook 'auto-reload-firefox-on-after-save-hook)

;; Aliasing default commands to enhance them
(defalias 'replace-regexp 'vr/replace) ;; visual-regexp
(defalias 'isearch-forward 'isearch-forward-regexp)
(defalias 'isearch-backward 'isearch-backward-regexp)

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
  "Removes extra whitespaces from region"
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

;; Flusher
(defun baboon-flush-lines (&optional move-to-top-p)
  "Flushes all lines after the point, + the current one"
  (interactive "P")
  (when (not (equal move-to-top-p nil))
    (beginning-of-buffer))
  (flush-lines "" nil nil t))

;; Find and open this file
(defun baboon-find-emacs-init-file ()
  (interactive)
  (find-file "~/.emacs.d/personal/init.el"))

(defun baboon-find-shell-init-file ()
  (interactive)
  (find-file (expand-file-name ".bash_aliases" (getenv "HOME"))))

;; -> half screen scrolling
(defvar where-to
  (make-hash-table :test 'equal))

(puthash 'down -2 where-to)
(puthash 'up 2 where-to)

(defun half-scroll (where)
  (move-to-window-line nil)
  (recenter (gethash where where-to))
  (move-to-window-line nil))

(defun half-scroll-down ()
  (interactive)
  (half-scroll 'down))

(defun half-scroll-up ()
  (interactive)
  (half-scroll 'up))
;; <- half screen scrolling

;; Open randomly named buffer
(defvar phonetically-spaced-words
  '() "")

(defun baboon-random-buffer ()
  (interactive)
  (switch-to-buffer
   (eval-expression (quote
                     (concat "sdf "
                             (format "%s"
                                     (random 5000)))) nil)))

;; Open file on lilith
(defun baboon-lilith-tramp ()
  (interactive)
  (find-file "/ssh:baboon@baboon.io#10522:/home/baboon"))

;; Comment current line/region
(defun baboon-comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

;; Smartparens remapping
(add-hook
 'smartparens-mode-hook
 (lambda ()
   (define-key smartparens-mode-map (kbd "M-r") 'nil)))

;; Flyspell remapping
(define-key flyspell-mode-map (kbd "C-,") nil)

;; Hideshowvis: Shows +/- in the fringe for foldable regions
(hideshowvis-symbols)

(add-hook
 'prog-mode-hook
 'hideshowvis-minor-mode)

;; Baboon Keybindings
(global-set-key (kbd "s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-<down>") 'shrink-window)
(global-set-key (kbd "s-<up>") 'enlarge-window)
(global-set-key (kbd "C-c . l a") 'ecb-activate)
(global-set-key (kbd "C-c . l d") 'ecb-deactivate)
(global-set-key (kbd "C-x M-o") 'other-frame)
(global-set-key (kbd "C-c C-q") 'slime-close-all-parens-in-sexp)
(global-set-key (kbd "C-c w") 'webjump)
(global-set-key (kbd "C-c F") 'baboon-flush-lines)
(global-set-key (kbd "C-c E") 'baboon-find-emacs-init-file)
(global-set-key (kbd "s-;") 'ace-jump-word-mode)
(global-set-key (kbd "s-.") 'ace-jump-line-mode)
(global-set-key (kbd "C-h C-P") 'describe-package)
(global-set-key (kbd "M-D") 'sp-kill-symbol)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)
(global-set-key (kbd "C-S-v") 'half-scroll-up)
(global-set-key (kbd "M-S-v") 'half-scroll-down)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(global-set-key (kbd "C-<enter>") 'prelude-smart-open-line)
(global-set-key (kbd "C-S-<enter>") 'prelude-smart-open-line-above)
(global-set-key (kbd "s-F") 'baboon-refresh-firefox)
(global-set-key (kbd "M-;") 'baboon-comment-line-or-region)
(key-chord-define-global "KK" 'kill-this-buffer)

;;;init.el ends here
