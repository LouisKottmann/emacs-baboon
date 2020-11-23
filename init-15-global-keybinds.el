;;; init-15-global-keybinds.el --- Global Keybindings

;; A lot of these come from emacs-prelude by bbatsov
;; I renamed them with a baboon- to find them more easily

(windmove-default-keybindings)

(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-o") 'other-window)
(key-chord-define-global "KK" 'kill-this-buffer)

(use-package crux
  :bind (("C-S-<return>" . crux-smart-open-line-above)
         ("S-<return>" . crux-smart-open-line)
         ("s-k" . crux-kill-whole-line)
         ("C-c d" . crux-duplicate-current-line-or-region))
  :chords ("JJ" . crux-switch-to-previous-buffer)
  :init (global-set-key [remap move-beginning-of-line]
                        'crux-move-beginning-of-line))


;;
(defun baboon-flush-lines (&optional move-to-top-p)
  "Flushes all lines after the point, + the current one"
  (interactive "P")
  (when (not (equal move-to-top-p nil))
    (beginning-of-buffer))
  (flush-lines "" nil nil t))

(global-set-key (kbd "C-c F") 'baboon-flush-lines)

;;
(defun baboon-copy-file-path ()
  "Show the full path file name in the minibuffer and copies it to the clipboard."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

(global-set-key (kbd "s-c") 'baboon-copy-file-path)

;;
(defun baboon-copy-file-name ()
  "Show the file name in the minibuffer and copies it to the clipboard."
  (interactive)
  (let ((filename (message (file-name-nondirectory (buffer-file-name)))))
    (message filename)
    (kill-new filename)))

(global-set-key (kbd "C-s-c") 'baboon-copy-file-name)

;;
(defun baboon-open-nautilus ()
  "Starts nautilus in the current directory"
  (interactive)
  (let* ((filename (file-truename buffer-file-name))
         (scmd (concat "nautilus" " '" filename "'")))
    (shell-command scmd)))

(global-set-key (kbd "s-C") 'baboon-open-nautilus)

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

(global-set-key (kbd "M-;") 'baboon-comment-line-or-region)

;;
(defun baboon-find-emacs-init-file ()
  (interactive)
  (find-file (expand-file-name "init.el" baboon-dir)))

(global-set-key (kbd "C-c E") 'baboon-find-emacs-init-file)
