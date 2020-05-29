;;; init-10-face.el --- Customize the look of emacs

;; Splash Screen
(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'markdown-mode)

;; Theme
(use-package solarized-theme
  :init
  (load-theme 'solarized-light t))

;; French keyboard support (^ etc)
(load-library "iso-transl")

;; Font
(set-face-attribute 'default nil
                    :family "UbuntuMono"
                    :height 95)

;; Colors
(defface baboon-main-color
  '((t (:foreground  "#182F31")))
  "Background color of my logo")

;; Line counter in modeline
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
(add-hook 'window-configuration-change-hook 'baboon-mode-line-count-lines)
(add-hook 'focus-in-hook 'baboon-mode-line-count-lines)
(add-hook 'focus-out-hook 'baboon-mode-line-count-lines)


;; Powerline support
(use-package powerline
  :demand t
  :custom
  (powerline-default-separator (quote arrow))
  :custom-face
  (powerline-active1 ((t (:inherit mode-line :background "#CB8E06" :foreground "#4F4F4F"))))
  (powerline-active2 ((t (:inherit mode-line :background "#268BD2" :foreground "#FDF6E3"))))
  :preface
  (defun baboon-modeline-save-status-icon ()
    "Doc string"
    (let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0 :face all-the-icons-blue)
              ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0 :face all-the-icons-blue)
              ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1 :face all-the-icons-blue)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))
      (propertize (apply (cadr result) (cddr result))
                  'face `(:family ,(funcall (car result))))))
  :init
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw " ")
                                     (baboon-modeline-save-status-icon)
                                     (powerline-buffer-size nil 'l)
                                     ;; (powerline-raw mode-line-mule-info nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     ;; (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (and (boundp 'multiple-cursors-mode) multiple-cursors-mode)
                                       (powerline-raw mc/mode-line face2))
                                     ))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (powerline-buffer-id face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw baboon-mode-line-buffer-count nil 'r)
                                     (powerline-raw "  "))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))


(defvar baboon-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defun baboon-cleanup-buffer-or-region ()
  "Cleanup a region if selected, otherwise the whole buffer."
  (interactive)
  (call-interactively 'untabify)
  (unless (member major-mode baboon-indent-sensitive-modes)
    (call-interactively 'indent-region))
  (whitespace-cleanup))
