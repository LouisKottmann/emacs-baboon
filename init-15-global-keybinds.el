;;; init-15-global-keybinds.el --- Global Keybindings

(windmove-default-keybindings)

(global-set-key (kbd "M-SPC") 'cycle-spacing)
(global-set-key (kbd "M-o") 'other-window)

;;
(defun baboon-flush-lines (&optional move-to-top-p)
  "Flushes all lines after the point, + the current one"
  (interactive "P")
  (when (not (equal move-to-top-p nil))
    (beginning-of-buffer))
  (flush-lines "" nil nil t))

(global-set-key (kbd "C-c F") 'baboon-flush-lines)

;;
(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "C-S-<return>") 'prelude-smart-open-line-above)

;;
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.

With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(global-set-key (kbd "S-<return>") 'prelude-smart-open-line)

;;
(defun prelude-top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

;;
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key [remap move-beginning-of-line]
                'prelude-move-beginning-of-line)

;;
(defun prelude-kill-whole-line (&optional arg)
  "A simple wrapper around command `kill-whole-line' that respects indentation.
Passes ARG to command `kill-whole-line' when provided."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation))

(global-set-key (kbd "s-k") 'prelude-kill-whole-line)

;;
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

(defun prelude-duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (pcase-let* ((origin (point))
               (`(,beg . ,end) (prelude-get-positions-of-line-or-region))
               (region (buffer-substring-no-properties beg end)))
    (-dotimes arg
      (lambda (n)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point))))
    (goto-char (+ origin (* (length region) arg) arg))))

(global-set-key (kbd "C-c d") 'prelude-duplicate-current-line-or-region)

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

;;
(key-chord-define-global "KK" 'kill-this-buffer)

;;
(defun prelude-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(key-chord-define-global "JJ" 'prelude-switch-to-previous-buffer)
